;;; macher-agent-orchestration.el --- Orchestration -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive orchestration commands for Macher Agent.  This file provides
;; functions to coordinate and execute tasks.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'generator)
(require 'gptel)
(require 'macher)
(require 'macher-agent-core)
(require 'macher-agent-vfs-client)
(require 'macher-agent-presets)
(require 'macher-agent-macher-bridge)
(require 'macher-agent-gptel-bridge)

(defvar macher-agent-active-subagents nil
  "Store active sub-agents and their locked directories as an alist.

Each entry is a cell mapping sub-agent buffer instances or identifiers
to their designated sandbox directory paths.

Return an association list mapping sub-agent buffers to sandbox paths, or nil.

Side effects: Global variable storing active sub-agent mappings.")

(defvar macher-agent-submit-task-result-tool nil
  "Store the tool object for task result submission.

Holds the gptel tool object that sub-agents call to submit their final result.

Return the submit task result tool object, or nil.

Side effects: None.")

(defvar macher-agent--task-registry (make-hash-table :test 'equal)
  "Global registry mapping Task IDs to their originating buffer names.
Used to route completed artifacts back to the correct agent's inbox.")

(put 'macher-agent--current-task-id 'permanent-local t)

(defun macher-agent--generate-uuid ()
  "Generate a unique identifier string."
  (if (fboundp 'org-id-uuid)
      (org-id-uuid)
    (format "task-%04x%04x-%04x-%04x"
            (random #xffff) (random #xffff) (random #xffff) (random #xffff))))

(defvar macher-agent-a2a-pipeline-functions
  '(macher-agent-a2a-pipe--resolve-target
    macher-agent-a2a-pipe--route-instructions
    macher-agent-a2a-pipe--bind-closure
    macher-agent-a2a-pipe--transmit)
  "Chained reducer pipeline to construct and execute an A2A sub-agent.")

(defun macher-agent-a2a-dispatch (send-message-payloads final-callback)
  "Dispatch A2A SEND-MESSAGE-PAYLOADS using a reducer pipeline.
Passes each payload through `macher-agent-a2a-pipeline-functions' and calls
FINAL-CALLBACK upon completion."
  (let* ((total (length send-message-payloads))
         (actual-parent-fsm (or (bound-and-true-p macher-agent--active-fsm)
                                (ignore-errors (macher-agent--get-fsm-latest))))
         (actual-parent-buf (or (when actual-parent-fsm
                                  (ignore-errors (plist-get (gptel-fsm-info actual-parent-fsm) :buffer)))
                                (current-buffer)))
         (shared-state (list :results (make-hash-table :test 'equal)
                             :total total
                             :final-callback final-callback
                             :parent-buf actual-parent-buf
                             :parent-fsm actual-parent-fsm
                             :original-payloads send-message-payloads)))
    (if (= total 0)
        (when final-callback (funcall final-callback nil))
      (dolist (msg send-message-payloads)
        (let ((initial-state (list :a2a-msg msg
                                   :shared-state shared-state
                                   :child-buf nil)))
          (seq-reduce (lambda (state pipe-fn)
                        (funcall pipe-fn state))
                      macher-agent-a2a-pipeline-functions
                      initial-state))))))

(defun macher-agent-a2a-pipe--resolve-target (state)
  "Pipeline step 1: Resolve existing buffer or spawn a new one."
  (let* ((msg (plist-get state :a2a-msg))
         (meta (plist-get msg :metadata))
         (buf-name (plist-get meta :buffer_name))
         (presets (plist-get meta :presets))
         (shared (plist-get state :shared-state))
         (parent-buf (plist-get shared :parent-buf))
         (parent-ctx (when (and parent-buf (buffer-live-p parent-buf))
                       (buffer-local-value 'macher-agent--persistent-context parent-buf)))
         (existing-buf (when buf-name (get-buffer buf-name))))
    (if (and existing-buf (buffer-live-p existing-buf))
        (progn
          (with-current-buffer existing-buf
            (setq-local macher-agent--is-subagent t)
            (setq-local macher-agent--suppress-patch (plist-get meta :suppress_patch))
            (setq-local macher-agent--current-task-id (plist-get msg :task-id))
            (when (plist-get meta :background)
              (setq-local macher-agent--ready-to-reap nil))
            (when parent-ctx
              (let ((existing-ctx (bound-and-true-p macher-agent--persistent-context)))
                (if (null existing-ctx)
                    (setq-local macher-agent--persistent-context
                                (if (fboundp 'macher-agent--clone-context)
                                    (macher-agent--clone-context parent-ctx)
                                  parent-ctx))
                  (when (not (eq existing-ctx parent-ctx))
                    (if (fboundp 'macher-agent--merge-contexts)
                        (let ((merged (macher-agent--merge-contexts existing-ctx parent-ctx)))
                          (when merged
                            (setq-local macher-agent--persistent-context merged)))
                      (setq-local macher-agent--persistent-context
                                  (if (fboundp 'macher-agent--clone-context)
                                      (macher-agent--clone-context parent-ctx)
                                    parent-ctx)))))))
            (unless (plist-get meta :background)
              (when (fboundp 'macher-agent-ui-show)
                (macher-agent-ui-show existing-buf))))
          (plist-put state :child-buf existing-buf))
      (if (and presets (stringp buf-name) (not (string-empty-p buf-name)))
          (condition-case _err
              (let ((child-buf (macher-agent-add-subagent buf-name presets parent-buf nil parent-ctx)))
                (unless (plist-get meta :background)
                  (when (fboundp 'macher-agent-ui-show)
                    (macher-agent-ui-show child-buf)))
                (with-current-buffer child-buf
                  (setq-local macher-agent--suppress-patch (plist-get meta :suppress_patch))
                  (setq-local macher-agent--current-task-id (plist-get msg :task-id))
                  (when (plist-get meta :background)
                    (setq-local macher-agent--ready-to-reap nil)))
                (plist-put state :child-buf child-buf))
            (error
             (plist-put state :error-payload
                        (list :status 'error
                              :error (format "ERROR: Sub-agent buffer '%s' not found." buf-name)
                              :buffer-name buf-name
                              :task-id (plist-get msg :task-id)))))
        (plist-put state :error-payload
                   (list :status 'error
                         :error (format "ERROR: Sub-agent buffer '%s' not found." buf-name)
                         :buffer-name buf-name
                         :task-id (plist-get msg :task-id)))))))

(defun macher-agent-a2a-pipe--route-instructions (state)
  "Pipeline step 2: Route instructions to the buffer or intercept for wakeup."
  (if (plist-get state :error-payload)
      state
    (let* ((child-buf (plist-get state :child-buf))
           (msg-payload (plist-get (plist-get state :a2a-msg) :message))
           (instructions (if (listp msg-payload) (plist-get msg-payload :instructions) msg-payload))
           (wake-cb (and child-buf (buffer-live-p child-buf)
                         (gethash (buffer-name child-buf) macher-agent--pending-callbacks))))
      (if wake-cb
          (progn
            (remhash (buffer-name child-buf) macher-agent--pending-callbacks)
            (plist-put state :wake-cb wake-cb)
            (plist-put state :wake-msg instructions))
        (when (and child-buf (buffer-live-p child-buf))
          (with-current-buffer child-buf
            (goto-char (point-max))
            (insert (or instructions "") "\n\n"))))
      state)))

(defun macher-agent-a2a-pipe--bind-closure (state)
  "Pipeline step 4: Bind the lexical callback for STATE."
  (let* ((msg (plist-get state :a2a-msg))
         (meta (plist-get msg :metadata))
         (task-id (plist-get msg :task-id))
         (suppress-patch (plist-get meta :suppress_patch))
         (shared (plist-get state :shared-state))
         (results (plist-get shared :results))
         (parent-buf (plist-get shared :parent-buf))
         (parent-fsm (plist-get shared :parent-fsm))
         (total (plist-get shared :total))
         (final-callback (plist-get shared :final-callback))
         (original-payloads (plist-get shared :original-payloads))
         (err-payload (plist-get state :error-payload))
         (wake-cb (plist-get state :wake-cb))) ; <- Extract wake-cb from state

    (if err-payload
        (progn
          (puthash task-id err-payload results)
          (when-let* (((= (hash-table-count results) total))
                      ((not (gethash '*completed* results)))
                      (ordered-results
                       (mapcar (lambda (p) (gethash (plist-get p :task-id) results))
                               original-payloads)))
            (puthash '*completed* t results)
            (when final-callback
              (if (and parent-buf (buffer-live-p parent-buf))
                  (with-current-buffer parent-buf
                    (let ((macher-agent--active-fsm parent-fsm)
                          (gptel--fsm-last parent-fsm))
                      (funcall final-callback (vconcat ordered-results))))
                (funcall final-callback (vconcat ordered-results))))))
      (let ((child-buf (plist-get state :child-buf)))
        (when (and child-buf (buffer-live-p child-buf))
          (with-current-buffer child-buf
            (when (plist-get meta :background)
              (setq-local macher-agent--ready-to-reap nil))

            (unless wake-cb
              (let ((a2a-cb
                     (lambda (artifact-payload)
                       (let
                           ((child-ctx
                             (when (buffer-live-p child-buf)
                               (buffer-local-value 'macher-agent--persistent-context child-buf))))
                         (macher-agent--push-context-to-parent child-ctx parent-buf))

                       (puthash task-id artifact-payload results)

                       (when-let* (((= (hash-table-count results) total))
                                   ((not (gethash '*completed* results)))
                                   (ordered-results
                                    (mapcar (lambda (p) (gethash (plist-get p :task-id) results))
                                            original-payloads)))
                         (puthash '*completed* t results)
                         (when final-callback
                           (with-current-buffer parent-buf
                             (let ((macher-agent--active-fsm parent-fsm)
                                   (gptel--fsm-last parent-fsm))
                               (funcall final-callback (vconcat ordered-results)))))))))
                (macher-agent--push-parent parent-buf nil a2a-cb task-id suppress-patch)
                (plist-put state :a2a-cb a2a-cb)))))))
    state))

(defun macher-agent--push-context-to-parent (child-ctx parent-buf)
  "Merge CHILD-CTX into PARENT-BUF within the strict scope
of the parent.  This guarantees mutation hooks and skill
initialisations update the parent's registry."
  (when (and child-ctx (buffer-live-p parent-buf))
    (with-current-buffer parent-buf
      (let ((parent-ctx (or (bound-and-true-p macher-agent--persistent-context)
                            (ignore-errors (macher-agent-resolve-context)))))
        (when (and parent-ctx (not (eq parent-ctx child-ctx)))
          (let ((merged-ctx (macher-agent--merge-contexts parent-ctx child-ctx)))
            (when merged-ctx
              (setq macher-agent--persistent-context merged-ctx))))))))

(defun macher-agent-a2a-pipe--transmit (state)
  "Pipeline step 5: Transmit the FSM or wake the suspended agent."
  (unless (plist-get state :error-payload)
    (let ((wake-cb (plist-get state :wake-cb))
          (wake-msg (plist-get state :wake-msg))
          (child-buf (plist-get state :child-buf))
          (msg (plist-get state :a2a-msg)))
      (if wake-cb
          (funcall wake-cb wake-msg)
        (when (and child-buf (buffer-live-p child-buf))
          (with-current-buffer child-buf
            (let* ((task-id (or (plist-get msg :task-id)
                                (bound-and-true-p macher-agent--current-task-id)))
                   (a2a-cb (or (plist-get state :a2a-cb)
                               (bound-and-true-p macher-agent--a2a-callback)))
                   (target-name (buffer-name child-buf))
                   (callbacks
                    (list :success-cb
                          (lambda (res)
                            (when (functionp a2a-cb)
                              (funcall a2a-cb (list :status 'success
                                                    :data res
                                                    :task-id task-id
                                                    :buffer-name target-name))))
                          :on-success
                          (lambda (res)
                            (when (functionp a2a-cb)
                              (funcall a2a-cb (list :status 'success
                                                    :data res
                                                    :task-id task-id
                                                    :buffer-name target-name))))
                          :error-cb
                          (lambda (err)
                            (when (functionp a2a-cb)
                              (funcall a2a-cb (list :status 'error
                                                    :error err
                                                    :task-id task-id
                                                    :buffer-name target-name))))
                          :on-error
                          (lambda (err)
                            (when (functionp a2a-cb)
                              (funcall a2a-cb (list :status 'error
                                                    :error err
                                                    :task-id task-id
                                                    :buffer-name target-name)))))))
              (if (fboundp 'macher-agent-gptel-transmit)
                  (let ((ctx (when (fboundp 'make-macher-agent-task-context)
                               (make-macher-agent-task-context :target-buffer (current-buffer)))))
                    (if ctx (macher-agent-gptel-transmit ctx callbacks) (gptel-send)))
                (gptel-send)))))))
    state))

(cl-defstruct macher-agent-task-context
  "Represent a task execution context structure.

WORKSPACE is the target workspace instance or path.
TARGET-BUFFER is the target buffer for task execution.
SKILL-SYM is the active skill or preset symbol.
SYSTEM-MESSAGE is the system prompt message string.

Return a new `macher-agent-task-context' struct instance.

Side effects: None."
  workspace
  target-buffer
  skill-sym
  system-message)

(defun macher-agent--resolve-buffer-name (name)
  "Resolve buffer string or buffer object NAME to a buffer name string.

NAME is a buffer object, string buffer name, or file path.

Return the resolved buffer name string, or NAME if string and unmapped.

Side effects: None."
  (cond
   ((bufferp name) (buffer-name name))
   ((stringp name)
    (or (when-let* ((buf (get-buffer name)))
          (buffer-name buf))
        (when-let* ((buf (get-file-buffer name)))
          (buffer-name buf))
        (when-let* ((buf (get-file-buffer (expand-file-name name))))
          (buffer-name buf))
        name))
   (t name)))

(defun macher-agent-subagent-p (&optional buffer)
  "Check whether BUFFER is a sub-agent buffer.

BUFFER is the optional target buffer, defaulting to the current buffer.

Return non-nil if BUFFER is a sub-agent, otherwise nil.

Side effects: None."
  (with-current-buffer (or buffer (current-buffer))
    (bound-and-true-p macher-agent--is-subagent)))

(defun macher-agent-ready-to-reap-p (&optional buffer)
  "Check whether BUFFER is ready to be reaped.

BUFFER is the optional target buffer, defaulting to the current buffer.

Return non-nil if BUFFER is ready to be reaped, otherwise nil.

Side effects: None."
  (with-current-buffer (or buffer (current-buffer))
    (bound-and-true-p macher-agent--ready-to-reap)))

(defun macher-agent--reap-buffer (buf)
  "Reap sub-agent BUF by aborting gptel operations and killing it.

BUF is the sub-agent buffer to reap.

Return nil.

Side effects: Aborts pending gptel operations and kills BUF."
  (when (and (buffer-live-p buf)
             (with-current-buffer buf
               (and (macher-agent-subagent-p)
                    (macher-agent-ready-to-reap-p))))
    (with-current-buffer buf
      (set-buffer-modified-p nil)
      (gptel-abort buf)
      (let ((kill-buffer-query-functions nil)
            (kill-buffer-hook nil))
        (kill-buffer buf)))))

(defun macher-normalise-preset-name (preset)
  "Normalise PRESET name by stripping leading @ symbols.

PRESET is the preset identifier (string, symbol, or list).

Return the normalised preset symbol, or nil.

Side effects: None."
  (let ((str (cond ((stringp preset) preset)
                   ((symbolp preset) (symbol-name preset))
                   ((and (listp preset) (stringp (car preset))) (car preset))
                   (t (format "%s" preset)))))
    (intern (replace-regexp-in-string "^@+" "" str))))

(defun macher-agent--compose-merge-system-prompt (current-sys sym sys-spec)
  "Merge SYS-SPEC into CURRENT-SYS for skill SYM.

CURRENT-SYS is the existing system prompt (string, list, or nil).
SYM is the preset symbol.
SYS-SPEC is the system prompt specification (string or modifier).

Return the updated system prompt value.

Side effects: None."
  (if (stringp sys-spec)
      (let ((cur (if (listp current-sys)
                     (string-join current-sys "\n---\n")
                   (or current-sys ""))))
        (concat cur
                (if (string-empty-p cur) "" "\n---\n")
                (format "### Skill: %s\n%s\n" sym sys-spec)))
    (gptel--modify-value current-sys sys-spec)))

(defun macher-agent--resolve-single-tool-object (tool)
  "Resolve TOOL to a `gptel-tool' structure.

If TOOL is already a `gptel-tool' structure (checked via `gptel-tool-p`),
pass it through intact.
If TOOL is a string, symbol, or abstract list, use the existing registry
lookup to convert it to a `gptel-tool' object before assignment.

Return the resolved `gptel-tool' object, or TOOL if unresolved.

Side effects: None."
  (cond
   ((and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
    tool)
   ((and (consp tool) (keywordp (car tool)))
    (apply #'gptel-make-tool tool))
   ((symbolp tool)
    (or (when (and (boundp tool)
                   (fboundp 'gptel-tool-p)
                   (gptel-tool-p (symbol-value tool)))
          (symbol-value tool))
        (ignore-errors (gptel-get-tool (symbol-name tool)))
        (ignore-errors
          (gptel-get-tool (replace-regexp-in-string "-" "_" (symbol-name tool))))
        (when (fboundp 'macher-agent-resolve-tool)
          (let ((res (ignore-errors (macher-agent-resolve-tool tool nil))))
            (if (and (fboundp 'gptel-tool-p) (gptel-tool-p res))
                res
              nil)))
        (when (fboundp 'macher-agent-resolve-to-struct)
          (ignore-errors (macher-agent-resolve-to-struct tool)))
        tool))
   ((stringp tool)
    (or (ignore-errors (gptel-get-tool tool))
        (ignore-errors
          (gptel-get-tool (replace-regexp-in-string "-" "_" tool)))
        (ignore-errors
          (gptel-get-tool (replace-regexp-in-string "_" "-" tool)))
        (when (fboundp 'macher-agent-resolve-tool)
          (let ((res (ignore-errors (macher-agent-resolve-tool tool nil))))
            (if (and (fboundp 'gptel-tool-p) (gptel-tool-p res))
                res
              nil)))
        (when (fboundp 'macher-agent-resolve-to-struct)
          (ignore-errors (macher-agent-resolve-to-struct tool)))
        tool))
   ((listp tool)
    (or (ignore-errors (gptel-get-tool tool))
        (when (fboundp 'macher-agent-resolve-to-struct)
          (ignore-errors (macher-agent-resolve-to-struct tool)))
        (when (fboundp 'macher-agent-resolve-tool)
          (let ((res (ignore-errors (macher-agent-resolve-tool tool nil))))
            (if (and (fboundp 'gptel-tool-p) (gptel-tool-p res))
                res
              nil)))
        tool))
   (t tool)))

(defun macher-agent--compose-merge-tools (current-tools tools-spec)
  "Merge TOOLS-SPEC into CURRENT-TOOLS.

CURRENT-TOOLS is the current list of tools.
TOOLS-SPEC is the tools specification (modifier, list, string, or similar).

Return the updated tools list.

Side effects: None."
  (let ((merged-tools (gptel--modify-value current-tools tools-spec)))
    (cond
     ((null merged-tools) nil)
     ((and (fboundp 'gptel-tool-p) (gptel-tool-p merged-tools))
      (list merged-tools))
     ((and (consp merged-tools) (keywordp (car merged-tools)))
      (list (apply #'gptel-make-tool merged-tools)))
     ((and (listp merged-tools)
           (not (and (consp merged-tools) (keywordp (car merged-tools))))
           (let ((single (ignore-errors (gptel-get-tool merged-tools))))
             (and single (fboundp 'gptel-tool-p) (gptel-tool-p single))))
      (list (gptel-get-tool merged-tools)))
     ((listp merged-tools)
      (cl-loop for t-obj in merged-tools
               append (let ((res (macher-agent--resolve-single-tool-object t-obj)))
                        (if (and (listp res)
                                 (not (and (fboundp 'gptel-tool-p) (gptel-tool-p res))))
                            res
                          (list res)))))
     (t
      (list (macher-agent--resolve-single-tool-object merged-tools))))))

(defun macher-agent--resolve-preset-or-tool (sym known)
  "Resolve SYM against KNOWN presets or gptel tools.

SYM is the preset or tool symbol, string, or gptel-tool structure.
KNOWN is an alist of known preset specifications.

Return a cons cell (TYPE . VALUE) where TYPE is \\'preset or \\'tool, or nil.

Side effects: None."
  (cond
   ((and (fboundp 'gptel-tool-p) (gptel-tool-p sym))
    (cons 'tool sym))
   (t
    (when-let* ((clean-sym (macher-normalise-preset-name sym)))
      (if-let*
          ((spec (or (alist-get clean-sym known)
                     (when-let* ((ctx (ignore-errors (macher-agent-resolve-context))))
                       (alist-get clean-sym (macher-agent-workspace-skills-alist ctx)))
                     (alist-get clean-sym (bound-and-true-p macher-agent-global-skills-alist)))))
          (cons 'preset spec)
        (when-let*
            ((tool
              (or (ignore-errors (gptel-get-tool (symbol-name clean-sym)))
                  (ignore-errors
                    (gptel-get-tool (replace-regexp-in-string "-" "_" (symbol-name clean-sym)))))))
          (cons 'tool tool)))))))

(defvar macher-agent-preset-pipeline-functions
  '(macher-agent-preset-pipe--exclusive
    macher-agent-preset-pipe--system
    macher-agent-preset-pipe--tools
    macher-agent-preset-pipe--ptc
    macher-agent-preset-pipe--boot
    macher-agent-preset-pipe--parameters)
  "Pipeline functions for preset payload composition.

Each function takes accumulated STATE plist and an ITEM tuple
`(preset SYM SPEC)' or `(tool TOOL)', returning updated STATE.")

(defun macher-agent-preset-pipe--exclusive (state item)
  "Apply the `:exclusive' preset modifier in ITEM to STATE."
  (pcase item
    (`(preset ,_sym ,spec)
     (if (plist-get spec :exclusive)
         (progn
           (plist-put state :system nil)
           (plist-put state :tools nil)
           (plist-put state :ptc-primitives nil)
           (plist-put state :boot-directive nil)
           state)
       state))
    (_ state)))

(defun macher-agent-preset-pipe--system (state item)
  "Merge the system prompt specification in ITEM into STATE.

STATE is the accumulated payload plist.
ITEM is a resolved item tuple `(preset SYM SPEC)' or `(tool TOOL)'.

Return the updated STATE plist.

Side effects: None."
  (pcase item
    (`(preset ,sym ,spec)
     (if-let* ((sys-spec (or (plist-get spec :system)
                             (plist-get spec :system-message))))
         (plist-put state :system
                    (macher-agent--compose-merge-system-prompt
                     (plist-get state :system) sym sys-spec))
       state))
    (_ state)))

(defun macher-agent-preset-pipe--tools (state item)
  "Merge allowed tools specification or standalone tool in ITEM into STATE.

STATE is the accumulated payload plist.
ITEM is a resolved item tuple `(preset SYM SPEC)' or `(tool TOOL)'.

Return the updated STATE plist.

Side effects: None."
  (pcase item
    (`(preset ,_sym ,spec)
     (if-let* ((tools-spec (or (plist-get spec :tools)
                               (plist-get spec :allowed-tools))))
         (plist-put state :tools
                    (macher-agent--compose-merge-tools
                     (plist-get state :tools) tools-spec))
       state))
    (`(tool ,tool-obj)
     (plist-put state :tools
                (macher-agent--compose-merge-tools
                 (plist-get state :tools)
                 (list :append (if (or (and (consp tool-obj) (keywordp (car tool-obj)))
                                       (not (listp tool-obj))
                                       (and (fboundp 'gptel-tool-p)
                                            (gptel-tool-p tool-obj)))
                                   (list tool-obj)
                                 tool-obj)))))
    (_ state)))

(defun macher-agent-preset-pipe--ptc (state item)
  "Merge PTC primitives specification in ITEM into STATE.

STATE is the accumulated payload plist.
ITEM is a resolved item tuple `(preset SYM SPEC)' or `(tool TOOL)'.

Return the updated STATE plist.

Side effects: None."
  (pcase item
    (`(preset ,_sym ,spec)
     (if-let* ((ptc-spec (plist-get spec :ptc-primitives)))
         (let ((current-ptc (plist-get state :ptc-primitives))
               (ptc-list (if (listp ptc-spec) ptc-spec (list ptc-spec))))
           (plist-put state :ptc-primitives
                      (cl-delete-duplicates (append current-ptc ptc-list)
                                            :test #'equal)))
       state))
    (_ state)))

(defun macher-agent-preset-pipe--boot (state item)
  "Merge boot directive specification in ITEM into STATE.

STATE is the accumulated payload plist.
ITEM is a resolved item tuple `(preset SYM SPEC)' or `(tool TOOL)'.

Return the updated STATE plist.

Side effects: None."
  (pcase item
    (`(preset ,_sym ,spec)
     (if-let* ((bd (plist-get spec :boot-directive)))
         (plist-put state :boot-directive bd)
       state))
    (_ state)))

(defun macher-agent-preset-pipe--parameters (state item)
  "Merge model parameters in ITEM into STATE.

STATE is the accumulated payload plist.
ITEM is a resolved item tuple `(preset SYM SPEC)' or `(tool TOOL)'.

Return the updated STATE plist.

Side effects: None."
  (pcase item
    (`(preset ,_sym ,spec)
     (let ((st state))
       (dolist (key '(:model :backend :temperature :max-tokens))
         (when-let* ((val (plist-get spec key)))
           (setq st (plist-put st key val))))
       st))
    (_ state)))

(defun macher-agent--flatten-preset-dependencies (inline-presets known)
  "Pre-flatten INLINE-PRESETS and their parent preset dependencies.

INLINE-PRESETS is a list of preset symbols, strings, or tools.
KNOWN is an alist of known preset specifications.

Return a list of resolved item tuples `(preset SYM SPEC)' or `(tool TOOL)'.

Side effects: None."
  (let ((result nil)
        (visited nil))
    (cl-labels
        ((flatten-item (sym)
           (cond
            ((and (symbolp sym) (member sym visited))
             nil)
            (t
             (when (symbolp sym)
               (push sym visited))
             (pcase (macher-agent--resolve-preset-or-tool sym known)
               (`(preset . ,spec)
                (when-let* ((parents (plist-get spec :parents)))
                  (dolist (parent (if (listp parents) parents (list parents)))
                    (flatten-item parent)))
                (push (list 'preset sym spec) result))
               (`(tool . ,tool)
                (push (list 'tool tool) result))
               (_
                (when sym
                  (push (list 'tool sym) result))))))))
      (dolist (sym inline-presets)
        (flatten-item sym))
      (nreverse result))))

(defun macher-agent-compose-payload (base-state inline-presets)
  "Compose a transmission payload by merging BASE-STATE with INLINE-PRESETS.

Merges BASE-STATE with the resolved configuration of INLINE-PRESETS
using native gptel modification values.

First, pre-flattens INLINE-PRESETS and any parent preset dependencies.
Then reduces over `macher-agent-preset-pipeline-functions' for each item.
Finally, if available, `macher-agent-normalize-tools' is invoked on the
composed tools list.

BASE-STATE is the base state property list.
INLINE-PRESETS is the list of inline preset symbols.

Return the unified state property list.

Side effects: None."
  (let* ((state (copy-sequence base-state))
         (known (plist-get base-state :known-presets))
         (flattened (macher-agent--flatten-preset-dependencies inline-presets known)))
    (setq state
          (cl-reduce
           (lambda (st item)
             (cl-reduce (lambda (s pipe-fn)
                          (funcall pipe-fn s item))
                        macher-agent-preset-pipeline-functions
                        :initial-value st))
           flattened
           :initial-value state))
    (setq state (plist-put state :tools (macher-agent-normalize-tools (plist-get state :tools))))
    state))

(defun macher-agent--apply-payload-locally (payload)
  "Apply a composed payload to the current buffer variables.

PAYLOAD is the composed state property list.

Return nil.

Side effects: Sets buffer-local values for gptel and PTC variables."
  (when payload
    (when (plist-member payload :system)
      (setq-local gptel-system-prompt (plist-get payload :system)))
    (when (plist-member payload :model)
      (setq-local gptel-model (plist-get payload :model)))
    (when (plist-member payload :backend)
      (setq-local gptel-backend (plist-get payload :backend)))
    (when (plist-member payload :temperature)
      (setq-local gptel-temperature (plist-get payload :temperature)))
    (when (plist-member payload :max-tokens)
      (setq-local gptel-max-tokens (plist-get payload :max-tokens)))
    (when (plist-member payload :tools)
      (setq-local gptel-tools (plist-get payload :tools)))
    (when (plist-member payload :ptc-primitives)
      (setq-local macher-agent--active-ptc-primitives (plist-get payload :ptc-primitives)))
    (when (plist-member payload :boot-directive)
      (setq-local macher-agent--boot-directive (plist-get payload :boot-directive)))))

(defun macher-agent--apply-preset (preset-or-presets)
  "Apply PRESET-OR-PRESETS to current buffer using payload compositor.

PRESET-OR-PRESETS represents the preset symbol, list, or vector.

Return nil.

Side effects: Updates buffer-local gptel and PTC settings."
  (let* ((presets (cond ((listp preset-or-presets) preset-or-presets)
                        ((vectorp preset-or-presets) (append preset-or-presets nil))
                        (t (list preset-or-presets))))
         (base-state (list :model gptel-model
                           :backend (bound-and-true-p gptel-backend)
                           :system (bound-and-true-p gptel-system-prompt)
                           :temperature (bound-and-true-p gptel-temperature)
                           :max-tokens (bound-and-true-p gptel-max-tokens)
                           :tools gptel-tools
                           :known-presets (bound-and-true-p gptel--known-presets)))
         (payload (macher-agent-compose-payload base-state presets)))

    (when presets
      (let* ((primary-sym (macher-agent--extract-first-preset-symbol presets))
             (known (plist-get base-state :known-presets))
             (spec (when primary-sym
                     (or (alist-get primary-sym known)
                         (when-let* ((ctx (ignore-errors (macher-agent-resolve-context))))
                           (alist-get primary-sym (macher-agent-workspace-skills-alist ctx)))
                         (alist-get primary-sym (bound-and-true-p macher-agent-global-skills-alist)))))
             (raw-sys (or (plist-get spec :system) (plist-get spec :system-message)))
             (boot-dir (plist-get spec :boot-directive)))
        (when (stringp raw-sys)
          (setq payload (plist-put payload :system raw-sys)))
        (when boot-dir
          (setq payload (plist-put payload :boot-directive boot-dir)))))

    (macher-agent--apply-payload-locally payload)))

(defun macher-agent-use-skill (skill &optional buf)
  "Apply SKILL preset to BUF or current buffer.

SKILL is a skill symbol, string, or list of skills.
BUF is the target buffer, defaulting to current buffer.

Return nil.

Side effects: Sets buffer-local presets, gptel settings, and boot-directive."
  (interactive "sSkill: ")
  (let ((target-buf (cond ((bufferp buf) buf)
                          ((stringp buf) (get-buffer buf))
                          (t (current-buffer)))))
    (when (buffer-live-p target-buf)
      (with-current-buffer target-buf
        (let ((presets (cond ((listp skill) skill)
                             ((vectorp skill) (append skill nil))
                             (t (list skill)))))
          (setq-local macher-agent-presets presets)
          (macher-agent--apply-preset presets))))))

(defun macher-agent--push-parent
    (parent-buf &optional parent-cb a2a-cb task-id suppress-patch)
  "Push a parent context frame onto `macher-agent--parent-stack'
and update local vars.

PARENT-BUF is the target parent buffer object.
PARENT-CB is the optional completion callback function.
A2A-CB is the optional A2A callback closure.
TASK-ID is the optional task identifier string.
SUPPRESS-PATCH is optional patch suppression flag.

Return the pushed frame plist.

Side effects: Modifies `macher-agent--parent-stack' and
syncs legacy local variables."
  (let ((frame (list :parent-buffer parent-buf
                     :parent-callback parent-cb
                     :a2a-callback a2a-cb
                     :task-id task-id
                     :suppress-patch suppress-patch)))
    (push frame macher-agent--parent-stack)
    (setq-local macher-agent--parent-buffer parent-buf)
    (setq-local macher-agent--parent-callback parent-cb)
    (setq-local macher-agent--a2a-callback a2a-cb)
    (setq-local macher-agent--current-task-id task-id)
    (setq-local macher-agent--suppress-patch suppress-patch)
    frame))

(defun macher-agent--pop-parent ()
  "Pop top parent context frame off `macher-agent--parent-stack'.

Restores local buffer state to match the new top frame of the stack.

Return popped frame plist, or a fallback frame if stack was empty.

Side effects: Modifies `macher-agent--parent-stack' and syncs local state."
  (if macher-agent--parent-stack
      (let ((popped (pop macher-agent--parent-stack))
            (top (car macher-agent--parent-stack)))
        (setq-local macher-agent--parent-buffer (plist-get top :parent-buffer))
        (setq-local macher-agent--parent-callback (plist-get top :parent-callback))
        (setq-local macher-agent--a2a-callback (plist-get top :a2a-callback))
        (setq-local macher-agent--current-task-id (plist-get top :task-id))
        (when top
          (setq-local macher-agent--suppress-patch (plist-get top :suppress-patch)))
        popped)
    (list :parent-buffer (bound-and-true-p macher-agent--parent-buffer)
          :parent-callback (bound-and-true-p macher-agent--parent-callback)
          :a2a-callback (bound-and-true-p macher-agent--a2a-callback)
          :task-id (bound-and-true-p macher-agent--current-task-id)
          :suppress-patch (bound-and-true-p macher-agent--suppress-patch))))

(put 'macher-agent--is-subagent 'permanent-local t)
(put 'macher-agent--ready-to-reap 'permanent-local t)
(put 'macher-agent-presets 'permanent-local t)
(put 'macher-agent--parent-buffer 'permanent-local t)
(put 'macher-agent--parent-stack 'permanent-local t)
(put 'macher-agent--active-ptc-primitives 'permanent-local t)
(put 'macher-agent--suppress-patch 'permanent-local t)
(put 'macher-agent--boot-directive 'permanent-local t)

(defun macher-agent--ptc-primitive-p (sym)
  "Check whether SYM is an active Programmatic Tool Calling primitive.

Matches both direct symbol equivalence and dynamic translation between
hyphens and underscores for SYM.

SYM is the symbol to test.

Return t if SYM is an active primitive, otherwise nil.

Side effects: None."
  (let* ((sym-name (symbol-name sym))
         (norm-sym (replace-regexp-in-string "_" "-" sym-name))

         (fsm-obj (or (bound-and-true-p macher-agent--active-fsm)
                      (macher-agent--get-fsm-latest)))
         (info (when fsm-obj (gptel-fsm-info fsm-obj)))

         (active (or (when info (plist-get info :ptc-primitives))
                     (bound-and-true-p macher-agent--active-ptc-primitives)))
         (tools (or (when info (plist-get info :tools))
                    (bound-and-true-p gptel-tools))))

    (and (or (memq sym active)
             (cl-some (lambda (prim)
                        (let* ((prim-name (if (symbolp prim) (symbol-name prim) prim))
                               (norm-prim (replace-regexp-in-string "_" "-" prim-name)))
                          (string= norm-sym norm-prim)))
                      active)
             (and (null active)
                  (cl-some (lambda (tool)
                             (let* ((t-name (if (gptel-tool-p tool)
                                                (gptel-tool-name tool)
                                              (if (stringp tool) tool
                                                (format "%s" tool))))
                                    (norm-tool (replace-regexp-in-string "_" "-" t-name)))
                               (string= norm-sym norm-tool)))
                           tools)))
         t)))

(defun macher-agent--extract-first-preset-symbol (resolved-presets)
  "Extract the normalised first preset symbol from RESOLVED-PRESETS.

RESOLVED-PRESETS is a string, symbol, list, or vector of presets.

Return the clean symbol, or nil.

Side effects: None."
  (when-let* ((first-preset (cond ((stringp resolved-presets) resolved-presets)
                                  ((symbolp resolved-presets) (symbol-name resolved-presets))
                                  ((and (listp resolved-presets) (car resolved-presets))
                                   (if (symbolp (car resolved-presets))
                                       (symbol-name (car resolved-presets))
                                     (car resolved-presets)))
                                  ((and (vectorp resolved-presets) (> (length resolved-presets) 0))
                                   (if (symbolp (aref resolved-presets 0))
                                       (symbol-name (aref resolved-presets 0))
                                     (aref resolved-presets 0)))
                                  (t nil))))
    (intern (replace-regexp-in-string "^@+" "" first-preset))))

(defvar macher-agent-subagent-pipeline-functions
  '(macher-agent-subagent-pipe--normalize-args
    macher-agent-subagent-pipe--resolve-context
    macher-agent-subagent-pipe--init-buffer
    macher-agent-subagent-pipe--register)
  "Chained reducer pipeline to construct and initialize a subagent buffer.")

(defun macher-agent-subagent-pipe--normalize-args (state)
  "Pipeline step 1: Normalize overloaded arguments."
  (let ((presets (plist-get state :presets))
        (parent (plist-get state :parent-buf))
        (dir (plist-get state :dir))
        (ctx (plist-get state :context)))

    (when-let* ((is-path-string (and (stringp presets)
                                     (or (file-directory-p presets)
                                         (string-prefix-p "/" presets)
                                         (string-suffix-p "/" presets)))))
      (unless dir
        (setq dir presets))
      (if-let* ((ctx-is-list (listp ctx))
                (ctx-not-obj (not (and (fboundp 'macher-context-p)
                                       (macher-context-p ctx)))))
          (progn
            (setq presets ctx)
            (setq ctx nil))
        (setq presets nil)))

    (when-let* ((has-fbound (fboundp 'macher-context-p))
                (is-ctx (macher-context-p presets)))
      (setq ctx presets)
      (setq presets nil))

    (when-let* ((has-fbound (fboundp 'macher-context-p))
                (is-ctx (macher-context-p dir)))
      (setq ctx dir)
      (setq dir (if-let* ((is-str (stringp presets))
                          (not-eq (not (equal presets ctx))))
                    presets
                  nil)))

    (when-let* ((has-fbound (fboundp 'macher-context-p))
                (is-ctx (macher-context-p parent)))
      (setq ctx parent)
      (setq parent nil))

    (unless parent
      (setq parent (current-buffer)))

    (plist-put (plist-put (plist-put (plist-put state :presets presets)
                                     :parent-buf parent)
                          :dir dir)
               :context ctx)))

(defun macher-agent-subagent-pipe--resolve-context (state)
  "Pipeline step 2: Resolve context, clone it, and determine target directory."
  (let* ((parent (plist-get state :parent-buf))
         (dir (plist-get state :dir))
         (ctx (plist-get state :context))
         (resolved-ctx (or ctx
                           (when-let* ((is-live (buffer-live-p parent)))
                             (buffer-local-value 'macher-agent--persistent-context parent))
                           (ignore-errors (macher-agent-resolve-context dir))))
         (cloned-ctx (when-let* ((r-ctx resolved-ctx))
                       (if (fboundp 'macher-agent--clone-context)
                           (macher-agent--clone-context r-ctx)
                         r-ctx)))
         (target-dir (or dir
                         (when-let* ((c-ctx cloned-ctx))
                           (ignore-errors (macher-agent-context-root c-ctx)))
                         default-directory)))
    (plist-put (plist-put (plist-put state :resolved-ctx resolved-ctx)
                          :cloned-ctx cloned-ctx)
               :target-dir target-dir)))

(defun macher-agent-subagent-pipe--init-buffer (state)
  "Pipeline step 3: Create and initialize the subagent buffer."
  (let* ((name (plist-get state :name))
         (target-dir (plist-get state :target-dir))
         (parent (plist-get state :parent-buf))
         (cloned-ctx (plist-get state :cloned-ctx))
         (presets (plist-get state :presets))
         (buf (get-buffer-create name)))

    (with-current-buffer buf
      (when-let* ((has-markdown (fboundp 'markdown-mode))
                  (needs-markdown (not (derived-mode-p 'markdown-mode))))
        (markdown-mode))

      (setq-local macher-agent--is-subagent t)
      (setq-local macher-agent--is-workspace t)
      (setq-local gptel-stream nil)
      (setq-local macher-agent--parent-buffer parent)
      (setq-local default-directory (file-name-as-directory target-dir))

      (when-let* ((has-gptel (fboundp 'gptel-mode))
                  (needs-gptel (not gptel-mode)))
        (gptel-mode 1))

      (when-let* ((is-live (buffer-live-p parent)))
        (setq-local gptel-model (buffer-local-value 'gptel-model parent))
        (setq-local gptel-backend (buffer-local-value 'gptel-backend parent))
        (setq-local gptel-temperature (buffer-local-value 'gptel-temperature parent))
        (setq-local gptel-max-tokens (buffer-local-value 'gptel-max-tokens parent))
        (setq-local
         macher-agent--suppress-patch (buffer-local-value 'macher-agent--suppress-patch parent))
        (setq-local
         macher-agent--boot-directive (buffer-local-value 'macher-agent--boot-directive parent))
        (setq-local gptel--known-presets (buffer-local-value 'gptel--known-presets parent))
        (setq-local gptel-directives (buffer-local-value 'gptel-directives parent)))

      (when-let* ((c-ctx cloned-ctx))
        (setq-local macher-agent--persistent-context c-ctx))

      (when-let* ((p presets))
        (let ((preset-list (cond ((listp p) p)
                                 ((vectorp p) (append p nil))
                                 (t (list p)))))
          (setq-local macher-agent-presets preset-list)
          (macher-agent--apply-preset preset-list)))

      (add-hook 'gptel-prompt-transform-functions
                #'macher-agent-sync-prompt-transformer nil t)
      (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t)
      (add-hook 'gptel-post-response-functions #'macher-agent-post-response-reaper nil t))

    (plist-put state :target-buf buf)))

(defun macher-agent-subagent-pipe--register (state)
  "Pipeline step 4: Register the subagent in tracking lists."
  (let* ((name (plist-get state :name))
         (target-dir (plist-get state :target-dir))
         (cloned-ctx (plist-get state :cloned-ctx)))

    (when-let* ((is-bound (boundp 'macher-agent-active-subagents)))
      (setq macher-agent-active-subagents
            (cons (cons name target-dir)
                  (cl-delete name macher-agent-active-subagents :key #'car :test #'equal))))

    (when-let* ((c-ctx cloned-ctx)
                (ws (ignore-errors (macher-agent--get-context-workspace c-ctx))))
      (let ((subs (ignore-errors (macher-agent-workspace-active-subagents ws))))
        (macher-agent--set-workspace-active-subagents
         ws
         (cons (cons name target-dir)
               (cl-delete name subs :key #'car :test #'equal)))))
    state))

(defun macher-agent-add-subagent (name &optional presets parent-buf dir context)
  "Interactively or programmatically create sub-agent buffer NAME.

NAME is the target sub-agent buffer name string.
PRESETS is optional preset specification, list, vector, or string.
PARENT-BUF is optional parent orchestrator buffer.
DIR is optional directory path string.
CONTEXT is optional VFS context structure.

Return the created sub-agent buffer object.

Side effects: Creates buffer, updates local state, registers in global
tracking lists."
  (interactive "sSub-agent name: ")
  (let ((initial-state (list :name name
                             :presets presets
                             :parent-buf parent-buf
                             :dir dir
                             :context context)))
    (plist-get (seq-reduce (lambda (state pipe-fn)
                             (funcall pipe-fn state))
                           macher-agent-subagent-pipeline-functions
                           initial-state)
               :target-buf)))

(defun macher-agent--apply-single-virtual-buffer (entry)
  "Apply a single VFS virtual edit ENTRY to a live Emacs buffer.

ENTRY is a VFS context entry structure or cell.

Return non-nil if applied to a live buffer, otherwise nil.

Side effects: Modifies the target live buffer contents."
  (when entry
    (let* ((path (ignore-errors (macher-agent-vfs-entry-path entry)))
           (curr (ignore-errors (macher-agent-vfs-entry-curr entry)))
           (buf-name (when path (macher-agent--resolve-buffer-name path)))
           (buf (when buf-name (get-buffer buf-name))))
      (when (and buf (buffer-live-p buf) (stringp curr))
        (with-current-buffer buf
          (erase-buffer)
          (insert curr))
        t))))

(defun macher-agent-apply-virtual-buffers (&optional context)
  "Apply pending VFS virtual edits to live Emacs buffers.

CONTEXT is the optional VFS context structure. If omitted, resolves
the active context.

Return nil.

Side effects: Modifies contents of live buffers matching VFS entries."
  (let* ((ctx (or context (ignore-errors (macher-agent-resolve-context))))
         (contents (when ctx (ignore-errors (macher-agent--get-context-contents ctx)))))
    (dolist (entry contents)
      (macher-agent--apply-single-virtual-buffer entry))
    (when (fboundp 'macher-agent--auto-sync-context)
      (macher-agent--auto-sync-context ctx))))

(defun macher-agent-submit-task-result (result)
  "Submit the final RESULT for the current agent task."
  (setq-local macher-agent--final-result result)
  (let* ((frame (if (fboundp 'macher-agent--pop-parent)
                    (macher-agent--pop-parent)
                  (list :parent-buffer (bound-and-true-p macher-agent--parent-buffer)
                        :parent-callback (bound-and-true-p macher-agent--parent-callback)
                        :a2a-callback (bound-and-true-p macher-agent--a2a-callback)
                        :task-id (bound-and-true-p macher-agent--current-task-id))))
         (a2a-cb (plist-get frame :a2a-callback))
         (parent-cb (plist-get frame :parent-callback))
         (task-id
          (or (plist-get frame :task-id) (bound-and-true-p macher-agent--current-task-id))))
    (cond
     (a2a-cb
      (funcall a2a-cb
               (list :type 'ARTIFACT_UPDATE
                     :task-id task-id
                     :message (list :status 'success
                                    :data result
                                    :buffer-name (buffer-name)))))
     (parent-cb
      (funcall parent-cb
               (list :status 'success :data result :buffer-name (buffer-name)))))))

(fmakunbound 'macher-agent-emit)
(makunbound 'macher-agent-workspace-event-bus)

(provide 'macher-agent-orchestration)
;;; macher-agent-orchestration.el ends here
