;;; macher-agent-orchestration.el --- Orchestration -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive orchestration commands for Macher Agent.  This file provides
;; functions to coordinate and execute tasks.

;;; Code:

(require 'macher)
(require 'macher-agent-macher-bridge)
(require 'gptel nil t)
(require 'macher-agent-vfs-client)
(require 'macher-agent-gptel-tools)
(require 'generator)
(require 'subr-x)

(declare-function macher-agent-gptel-transmit "macher-agent-gptel-bridge" (task-context callbacks))
(declare-function macher-agent-sync-prompt-transformer "macher-agent-gptel-bridge" (async-fn fsm))
(declare-function macher-agent-post-response-reaper "macher-agent-gptel-bridge" (beg end))
(declare-function macher-agent-resolve-context "macher-agent-vfs-client")
(declare-function macher-agent--inject-context-state "macher-agent-vfs-client" (context &optional directives))
(declare-function macher-agent--init-workspace-state "macher-agent-vfs-client")
(declare-function macher-agent--auto-sync-context "macher-agent-vfs-client" (&optional ctx fsm))
(declare-function macher-agent-normalize-tools "macher-agent-api" (tools))
(declare-function macher-agent-resolve-tool "macher-agent-api" (tool-name tools-registry &optional dir-context context))
(declare-function macher-agent-initialize-skills "macher-agent-api" (&optional context dir))
(declare-function macher-agent-scope-add-file "macher-agent-api" (buffer-name context))
(declare-function macher-agent-sandbox--eval-iter "macher-agent-sandbox" (ast env))
(declare-function macher-agent--read-file-vfs-aware "macher-agent-gptel-tools" (file-path context))
(declare-function macher-agent-context-root "macher-agent-vfs-client" (context))

(defvar macher-agent-submit-task-result-tool nil
  "Store the tool object for task result submission.

Holds the gptel tool object that sub-agents call to submit their final result.

Return the submit task result tool object, or nil.

Side effects: None.")

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

(defun macher-agent--handle-parallel-task-result (idx result completed-flags results counter-box total final-callback)
  "Record RESULT for task IDX, updating COMPLETED-FLAGS and RESULTS.

IDX is the zero-based task index.
RESULT is the task result payload object.
COMPLETED-FLAGS is a vector tracking completion status per task index.
RESULTS is a list storing results in task index order.
COUNTER-BOX is a cons cell storing the number of completed tasks.
TOTAL is the total number of tasks.
FINAL-CALLBACK is the function to run when all tasks complete.

Return nil.

Side effects: Mutates COMPLETED-FLAGS, RESULTS, and COUNTER-BOX."
  (unless (aref completed-flags idx)
    (aset completed-flags idx t)
    (setf (nth idx results) result)
    (setcar counter-box (1+ (car counter-box)))
    (when (= (car counter-box) total)
      (funcall final-callback results))))

(defun macher-agent-execute-parallel (tasks final-callback)
  "Execute TASKS in parallel and run FINAL-CALLBACK upon completion.

TASKS is the list of tasks to spawn.
FINAL-CALLBACK is the function to run with the results.

Return nil.

Side effects: Spawns parallel tasks."
  (let* ((task-list (append tasks nil))
         (total (length task-list))
         (counter-box (cons 0 nil))
         (results (make-list total nil))
         (completed-flags (make-vector total nil)))
    (if (= total 0)
        (funcall final-callback nil)
      (cl-loop for task in task-list
               for index from 0
               do (let ((idx index))
                    (macher-agent-spawn-task
                     task
                     (lambda (result)
                       (macher-agent--handle-parallel-task-result
                        idx result completed-flags results counter-box total final-callback))))))))

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

(defun macher-agent--compose-merge-tools (current-tools tools-spec)
  "Merge TOOLS-SPEC into CURRENT-TOOLS.

CURRENT-TOOLS is the current list of tools.
TOOLS-SPEC is the tools specification (modifier, list, string, or similar).

Return the updated tools list.

Side effects: None."
  (let ((merged-tools (gptel--modify-value current-tools tools-spec)))
    (cl-loop for t-obj in (if (listp merged-tools) merged-tools (list merged-tools))
             collect (if (stringp t-obj)
                         (or (ignore-errors (gptel-get-tool t-obj)) t-obj)
                       t-obj))))

(defun macher-agent--resolve-preset-or-tool (sym known)
  "Resolve SYM against KNOWN presets or gptel tools.

SYM is the preset or tool symbol or string.
KNOWN is an alist of known preset specifications.

Return a cons cell (TYPE . VALUE) where TYPE is 'preset or 'tool, or nil.

Side effects: None."
  (when-let* ((clean-sym (macher-normalise-preset-name sym)))
    (if-let* ((spec (alist-get clean-sym known)))
        (cons 'preset spec)
      (when-let* ((tool (or (ignore-errors (gptel-get-tool (symbol-name clean-sym)))
                            (ignore-errors (gptel-get-tool (replace-regexp-in-string "-" "_" (symbol-name clean-sym)))))))
        (cons 'tool tool)))))

(defun macher-agent-compose-payload (base-state inline-presets)
  "Compose a transmission payload by merging BASE-STATE with INLINE-PRESETS.

Merges BASE-STATE with the resolved configuration of INLINE-PRESETS
using native gptel modification values.

The function clones the initial base state and processes each preset or
standalone tool sequentially.  For each preset, it recursively resolves
any declared parent dependencies first.

If a preset declares the `:exclusive' flag, it completely clears the
accumulated system prompt and tools before applying its own configurations,
preventing base environment bleed.

System prompts and tools are merged using gptel's declarative modifier logic.
Legacy string-based system prompts are automatically wrapped in native
append operations, and the original `\n---\n' delimiter is dynamically
injected if a preceding system prompt exists.  Flat properties like model,
temperature, and token limits are applied as direct overrides.

Finally, if available, `macher-agent-normalize-tools' is invoked on the
composed tools list.

BASE-STATE is the base state property list.
INLINE-PRESETS is the list of inline preset symbols.

Return the unified state property list.

Side effects: None."
  (let ((state (copy-sequence base-state))
        (known (plist-get base-state :known-presets)))

    (cl-labels
        ((apply-spec (sym spec)
           (when-let* ((parents (plist-get spec :parents)))
             (dolist (parent (if (listp parents) parents (list parents)))
               (when-let* ((parent-spec (alist-get parent known)))
                 (apply-spec parent parent-spec))))

           (when (plist-get spec :exclusive)
             (setq state (plist-put state :system nil))
             (setq state (plist-put state :tools nil))
             (setq state (plist-put state :ptc-primitives nil)))

           (when-let* ((sys-spec (or (plist-get spec :system) (plist-get spec :system-message))))
             (setq state (plist-put state :system
                                    (macher-agent--compose-merge-system-prompt
                                     (plist-get state :system) sym sys-spec))))

           (when-let* ((tools-spec (or (plist-get spec :tools) (plist-get spec :allowed-tools))))
             (setq state (plist-put state :tools
                                    (macher-agent--compose-merge-tools
                                     (plist-get state :tools) tools-spec))))

           (when-let* ((ptc-spec (plist-get spec :ptc-primitives)))
             (let ((current-ptc (plist-get state :ptc-primitives))
                   (ptc-list (if (listp ptc-spec) ptc-spec (list ptc-spec))))
               (setq state (plist-put state :ptc-primitives
                                      (cl-delete-duplicates (append current-ptc ptc-list) :test #'equal)))))

           (dolist (key '(:model :temperature :max-tokens))
             (when-let* ((val (plist-get spec key)))
               (setq state (plist-put state key val))))))

      (dolist (sym inline-presets)
        (pcase (macher-agent--resolve-preset-or-tool sym known)
          (`(preset . ,spec)
           (apply-spec sym spec))
          (`(tool . ,tool)
           (setq state (plist-put state :tools (append (plist-get state :tools) (list tool))))))))

    (setq state (plist-put state :tools (macher-agent-normalize-tools (plist-get state :tools))))

    state))

(defun macher-agent--apply-payload-locally (payload)
  "Apply a composed payload to the current buffer variables.

PAYLOAD is the composed state property list.

Return nil.

Side effects: Sets buffer-local values for gptel and PTC variables."
  (when payload
    (when (plist-member payload :system) (setq-local gptel-system-prompt (plist-get payload :system)))
    (when (plist-member payload :model) (setq-local gptel-model (plist-get payload :model)))
    (when (plist-member payload :temperature) (setq-local gptel-temperature (plist-get payload :temperature)))
    (when (plist-member payload :max-tokens) (setq-local gptel-max-tokens (plist-get payload :max-tokens)))
    (when (plist-member payload :tools) (setq-local gptel-tools (plist-get payload :tools)))
    (when (plist-member payload :ptc-primitives)
      (setq-local macher-agent--active-ptc-primitives (plist-get payload :ptc-primitives)))))

(defun macher-agent--apply-preset (preset-or-presets)
  "Apply PRESET-OR-PRESETS to current buffer using payload compositor.

PRESET-OR-PRESETS represents the preset symbol, list, or vector.

Return nil.

Side effects: Updates buffer-local gptel and PTC settings."
  (let* ((presets (cond ((listp preset-or-presets) preset-or-presets)
                        ((vectorp preset-or-presets) (append preset-or-presets nil))
                        (t (list preset-or-presets))))
         (base-state (list :model gptel-model
                           :system (bound-and-true-p gptel-system-prompt)
                           :temperature (bound-and-true-p gptel-temperature)
                           :max-tokens (bound-and-true-p gptel-max-tokens)
                           :tools gptel-tools
                           :known-presets (bound-and-true-p gptel--known-presets)))
         (payload (macher-agent-compose-payload base-state presets)))
    (macher-agent--apply-payload-locally payload)))

(defvar macher-agent--is-subagent nil
  "Flag whether the current buffer is managed as a sub-agent.

Non-nil indicates that the buffer is an isolated sub-agent buffer.

Return non-nil if current buffer is a sub-agent, otherwise nil.

Side effects: Buffer-local variable.")

(defvar macher-agent--ready-to-reap nil
  "Flag whether the sub-agent buffer is ready for garbage collection.

Non-nil indicates that the sub-agent task has completed and can be reaped.

Return non-nil if sub-agent buffer is ready to be reaped, otherwise nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent-presets nil
  "Store list of active preset or skill symbols for current buffer.

Holds the active preset and skill symbols configured for the current buffer.

Return list of active preset or skill symbols, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--parent-buffer nil
  "Store parent orchestrator buffer for current sub-agent.

Holds the buffer object of the parent agent that spawned this sub-agent.

Return parent buffer object, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--pending-children nil
  "Track child sub-agent buffer names pending completion.

Holds a list of string buffer names for child agents that have not finished.

Return list of child buffer names, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--child-results nil
  "Store accumulated results from completed child sub-agents.

Holds an association list mapping sub-agent names to their result payloads.

Return alist of child sub-agent results, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--resume-callback nil
  "Store callback function for resuming parent orchestrator.

Holds the function to invoke when all child sub-agent tasks complete.

Return resume callback function, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--active-ptc-primitives nil
  "Store active Programmatic Tool Calling primitives for buffer.

Holds list of symbols representing enabled PTC primitive operations.

Return list of PTC primitive symbols, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--suppress-patch nil
  "Flag whether patch review generation is suppressed.

Non-nil suppresses virtual patch review generation on task completion.

Return non-nil if patch creation is suppressed, otherwise nil.

Side effects: Buffer-local variable.")

(put 'macher-agent--is-subagent 'permanent-local t)
(put 'macher-agent--ready-to-reap 'permanent-local t)
(put 'macher-agent-presets 'permanent-local t)
(put 'macher-agent--parent-buffer 'permanent-local t)
(put 'macher-agent--pending-children 'permanent-local t)
(put 'macher-agent--child-results 'permanent-local t)
(put 'macher-agent--resume-callback 'permanent-local t)
(put 'macher-agent--active-ptc-primitives 'permanent-local t)
(put 'macher-agent--suppress-patch 'permanent-local t)

(defun macher-agent--ptc-primitive-p (sym)
  "Check whether SYM is an active Programmatic Tool Calling primitive.

Matches both direct symbol equivalence and dynamic translation between
hyphens and underscores for SYM.

SYM is the symbol to test.

Return t if SYM is an active primitive, otherwise nil.

Side effects: None."
  (let* ((sym-name (symbol-name sym))
         (norm-sym (replace-regexp-in-string "_" "-" sym-name))
         (underscore-sym (replace-regexp-in-string "-" "_" sym-name))
         (active (bound-and-true-p macher-agent--active-ptc-primitives))
         (tools (bound-and-true-p gptel-tools)))
    (and (or (memq sym active)
             (cl-some (lambda (prim)
                        (let* ((prim-name (if (symbolp prim) (symbol-name prim) prim))
                               (norm-prim (replace-regexp-in-string "_" "-" prim-name)))
                          (string= norm-sym norm-prim)))
                      active)
             (and (null active)
                  (or (cl-some (lambda (tool)
                                 (let* ((t-name (if (gptel-tool-p tool)
                                                    (gptel-tool-name tool)
                                                  (if (stringp tool) tool
                                                    (format "%s" tool))))
                                        (norm-tool (replace-regexp-in-string "_" "-" t-name)))
                                   (string= norm-sym norm-tool)))
                               tools)
                      (let ((resolved (ignore-errors
                                        (macher-agent-resolve-tool underscore-sym nil nil (and (fboundp 'macher-agent-resolve-context) (macher-agent-resolve-context))))))
                        (and resolved (not (equal resolved underscore-sym)))))))
         t)))

(defun macher-agent--make-lisp-task-callback (buf callback)
  "Create a parent callback for Lisp task execution in BUF.

BUF is the Lisp task sub-agent buffer.
CALLBACK is the completion callback function.

Return a unary callback function taking RES.

Side effects: Returns a closure that marks BUF ready to reap on completion."
  (let ((fired nil))
    (lambda (res)
      (unless fired
        (setq fired t)
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq-local macher-agent--ready-to-reap t)))
        (funcall callback res)))))

(defun macher-agent-execute-lisp-task (payload callback)
  "Execute an asynchronous Lisp PAYLOAD in an isolated buffer.

PAYLOAD is a function taking a single callback argument, or an immediate value.
CALLBACK is the function to run with the final result.

Return nil.

Side effects: Spawns a temporary buffer and executes PAYLOAD."
  (let* ((parent-buf (current-buffer))
         (buf-name (generate-new-buffer-name " *macher-agent-lisp-task*"))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (setq-local macher-agent--is-subagent t)
      (setq-local macher-agent--parent-buffer parent-buf)
      (setq-local macher-agent--parent-callback
                  (macher-agent--make-lisp-task-callback buf callback))
      (condition-case err
          (if (functionp payload)
              (funcall payload macher-agent--parent-callback)
            (funcall macher-agent--parent-callback payload))
        (error
         (funcall macher-agent--parent-callback (format "LISP EXECUTION ERROR: %s" err)))))))

(defun macher-agent--extract-first-preset-symbol (resolved-presets)
  "Extract the normalised first preset symbol from RESOLVED-PRESETS.

RESOLVED-PRESETS is a string, symbol, list, or vector of presets.

Return the clean symbol, or nil.

Side effects: None."
  (let ((first-preset (cond ((stringp resolved-presets) resolved-presets)
                            ((symbolp resolved-presets) (symbol-name resolved-presets))
                            ((and (listp resolved-presets) (stringp (car resolved-presets))) (car resolved-presets))
                            ((and (vectorp resolved-presets) (> (length resolved-presets) 0) (stringp (aref resolved-presets 0))) (aref resolved-presets 0))
                            (t nil))))
    (when first-preset
      (intern (replace-regexp-in-string "^@+" "" first-preset)))))

(defun macher-agent--merge-subagent-parent-context (buf parent-buf)
  "Merge persistent context from child BUF into PARENT-BUF.

BUF is the sub-agent child buffer.
PARENT-BUF is the parent orchestrator buffer.

Return nil.

Side effects: Merges persistent context into PARENT-BUF."
  (when (and (buffer-live-p buf) (buffer-live-p parent-buf))
    (let ((child-ctx (with-current-buffer buf (bound-and-true-p macher-agent--persistent-context)))
          (parent-ctx (or (with-current-buffer parent-buf (bound-and-true-p macher-agent--persistent-context))
                          (ignore-errors (with-current-buffer parent-buf (macher-agent-resolve-context))))))
      (when (and child-ctx parent-ctx (not (eq child-ctx parent-ctx)))
        (macher-agent--merge-contexts parent-ctx child-ctx)))))

(defun macher-agent--make-subagent-parent-callback (buf parent-buf is-background callback)
  "Create a parent callback for BUF and PARENT-BUF.

BUF is the sub-agent buffer.
PARENT-BUF is the parent orchestrator buffer.
IS-BACKGROUND is non-nil if sub-agent task runs in background.
CALLBACK is the user-supplied task completion callback.

Return a unary callback function taking RES.

Side effects: Returns a closure that updates parent context and reaper state."
  (let ((fired nil))
    (lambda (res)
      (macher-agent--merge-subagent-parent-context buf parent-buf)
      (unless fired
        (setq fired t)
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (unless is-background
              (setq-local macher-agent--ready-to-reap t))))
        (funcall callback res)))))

(defun macher-agent-spawn-task (task callback)
  "Spawn TASK inside a target sub-agent buffer.

TASK is the task specification list or buffer name.
CALLBACK is the completion callback function.

Return nil.

Side effects: Transmits task context to gptel and triggers CALLBACK on result."
  (let* ((parent-buf (current-buffer))
         (buf-name (if (listp task) (plist-get task :buffer_name) task))
         (instructions (if (listp task) (plist-get task :instructions) ""))
         (presets (if (listp task) (or (plist-get task :presets) (plist-get task :preset)) nil))
         (is-background (and (listp task) (plist-get task :background)))
         (suppress-patch (and (listp task) (plist-get task :suppress_patch)))
         (buf (get-buffer buf-name)))
    (if (not buf)
        (funcall callback (make-macher-agent-delegate-response :status 'error :error (format "ERROR: Sub-agent buffer '%s' not found." buf-name) :buffer-name buf-name))
      (let* ((parent-active-presets (with-current-buffer parent-buf (bound-and-true-p macher-agent-presets)))
             (target-active-presets (with-current-buffer buf (bound-and-true-p macher-agent-presets)))
             (resolved-presets (or presets target-active-presets parent-active-presets '("macher-agent-worker"))))
        (macher-agent--prepare-subagent-instructions buf instructions resolved-presets)
        (with-current-buffer buf
          (unless is-background
            (macher-agent--show-ui buf))

          (setq-local macher-agent--is-subagent t)
          (setq-local macher-agent--parent-buffer parent-buf)
          (setq-local macher-agent--suppress-patch suppress-patch)

          (setq-local macher-agent--parent-callback
                      (macher-agent--make-subagent-parent-callback buf parent-buf is-background callback))

          (let* ((clean-sym (macher-agent--extract-first-preset-symbol resolved-presets))
                 (task-ctx (make-macher-agent-task-context
                            :workspace nil
                            :target-buffer buf
                            :skill-sym clean-sym
                            :system-message gptel-system-prompt)))

            (macher-agent-gptel-transmit
             task-ctx
             (list :on-success (lambda (res)
                                 (funcall macher-agent--parent-callback (make-macher-agent-delegate-response :status 'success :data res :buffer-name buf-name)))
                   :on-error (lambda (err)
                               (funcall macher-agent--parent-callback (make-macher-agent-delegate-response :status 'error :error (format "ERROR: %s" err) :buffer-name buf-name)))))))))))

(defvar macher-agent-subagent-setup-hook nil
  "Hook run after preparing a sub-agent buffer.

Runs during sub-agent initialisation after default directory, mode, and gptel
settings have been applied.

Return value of hook when executed.

Side effects: Runs registered hook functions.")

(defun macher-agent--add-buffer-to-scope-headless (buf-name persistent-context)
  "Add BUF-NAME to PERSISTENT-CONTEXT scope headlessly.

BUF-NAME is the string buffer name.
PERSISTENT-CONTEXT is the active context structure.

Return nil.

Side effects: Mutates PERSISTENT-CONTEXT and runs `macher-agent-context-mutated-hook'."
  (get-buffer-create buf-name)
  (when persistent-context
    (let* ((contents (macher-agent--get-context-contents persistent-context))
           (entry (cl-find buf-name contents :key #'car :test #'equal)))
      (unless entry
        (let ((orig (with-current-buffer buf-name (buffer-substring-no-properties (point-min) (point-max)))))
          (macher-agent--set-context-contents persistent-context
                                              (cons (cons buf-name (cons orig orig)) contents))))))
  (run-hooks 'macher-agent-context-mutated-hook))

;;;###autoload
(defun macher-agent-add-buffer-to-scope (buffer)
  "Add BUFFER to the scope of the current agent.

BUFFER is the buffer object or string name.

Return nil.

Side effects: Mutates active context and displays message."
  (interactive "BAdd buffer to current agent's scope: ")
  (let ((buf-name (if (stringp buffer) buffer (buffer-name buffer)))
        (ctx (macher-agent-resolve-context)))
    (if (not ctx)
        (error "No active agent session found")
      (macher-agent--add-buffer-to-scope-headless buf-name ctx)
      (message "SUCCESS: Added '%s' to the agent's restricted scope." buf-name))))

(defun macher-agent--resolve-buffer-name (name)
  "Coerce NAME into a string buffer name.

NAME is the string representing buffer name.

Return the coerced buffer name string.

Side effects: None."
  (substring-no-properties name))

(defun macher-agent--prepare-subagent-buffer (buf full-dir context &optional presets parent-tools parent-model parent-backend parent-presets parent-directives parent-temp parent-tokens)
  "Prepare sub-agent BUF with locked directory and composed skills.

BUF is the target buffer.
FULL-DIR is the path to lock.
CONTEXT is the active context structure.
PRESETS represents the requested presets.
PARENT-TOOLS is the parent's tools list.
PARENT-MODEL is the parent's model.
PARENT-BACKEND is the parent's backend.
PARENT-PRESETS represents the parent's known presets.
PARENT-DIRECTIVES represents the parent's directives.
PARENT-TEMP is the parent's temperature.
PARENT-TOKENS is the parent's max tokens.

Return nil.

Side effects: Configures buffer-local variables, mode, and hooks in BUF."
  (with-current-buffer buf
    (setq default-directory (file-name-as-directory (macher-agent-root full-dir)))

    (when (and (fboundp 'markdown-mode) (not (derived-mode-p 'markdown-mode)))
      (markdown-mode))

    (setq-local macher-agent--is-subagent t)
    (setq-local macher-agent--is-workspace t)

    (when (and (fboundp 'gptel-mode) (not gptel-mode))
      (gptel-mode 1))

    (setq-local gptel-stream nil)

    (when context
      (setq-local macher--workspace (macher-agent--get-context-workspace context))
      (macher-agent--inject-context-state context))

    (when parent-model (setq-local gptel-model parent-model))
    (when parent-backend (setq-local gptel-backend parent-backend))
    (when parent-presets (setq-local gptel--known-presets parent-presets))
    (when parent-directives (setq-local gptel-directives parent-directives))
    (when parent-temp (setq-local gptel-temperature parent-temp))
    (when parent-tokens (setq-local gptel-max-tokens parent-tokens))

    (unless (boundp 'gptel-tools) (setq gptel-tools nil))
    (make-local-variable 'gptel-tools)

    (when parent-tools
      (setq gptel-tools (macher-agent-normalize-tools (append gptel-tools parent-tools))))

    (when presets
      (let* ((preset-list (if (listp presets) presets (list presets)))
             (base-state (list :model gptel-model
                               :system nil
                               :temperature (bound-and-true-p gptel-temperature)
                               :max-tokens (bound-and-true-p gptel-max-tokens)
                               :tools gptel-tools
                               :known-presets (bound-and-true-p gptel--known-presets)))
             (payload (macher-agent-compose-payload base-state preset-list)))
        (setq-local macher-agent-presets (delete-dups (append macher-agent-presets preset-list)))
        (macher-agent--apply-payload-locally payload)))

    (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)
    (add-hook 'gptel-post-response-functions #'macher-agent-post-response-reaper nil t)

    (run-hooks 'macher-agent-subagent-setup-hook)))

(defun macher-agent-add-subagent (name dir &optional instructions context presets)
  "Create a new sub-agent buffer inheriting parent state.

NAME is the sub-agent name string.
DIR is the workspace directory path string.
INSTRUCTIONS is the optional instructions string.
CONTEXT is the optional active context structure.
PRESETS represents optional requested presets.

Return the newly created sub-agent buffer.

Side effects: Creates buffer and updates workspace active sub-agents."
  (when context
    (macher-agent-initialize-skills context))
  (let* ((parent-buf (current-buffer))
         (parent-tools (bound-and-true-p gptel-tools))
         (parent-model (bound-and-true-p gptel-model))
         (parent-backend (bound-and-true-p gptel-backend))
         (parent-presets (bound-and-true-p gptel--known-presets))
         (parent-directives (bound-and-true-p gptel-directives))
         (parent-temp (bound-and-true-p gptel-temperature))
         (parent-tokens (bound-and-true-p gptel-max-tokens))
         (parent-active-presets (bound-and-true-p macher-agent-presets))
         (resolved-presets (or presets parent-active-presets '("macher-agent-worker")))
         (resolved-ctx (or context
                           (with-current-buffer parent-buf
                             (bound-and-true-p macher-agent--persistent-context))
                           (ignore-errors (macher-agent-resolve-context))))
         (child-ctx (when resolved-ctx (macher-agent--clone-context resolved-ctx)))
         (buf (get-buffer-create name)))

    (macher-agent--prepare-subagent-buffer
     buf dir child-ctx resolved-presets parent-tools parent-model parent-backend
     parent-presets parent-directives parent-temp parent-tokens)

    (when (and instructions (not (string-empty-p instructions)))
      (macher-agent--prepare-subagent-instructions buf instructions resolved-presets))

    (with-current-buffer buf
      (setq-local macher-agent--parent-buffer parent-buf))

    (when resolved-ctx
      (let ((workspace (macher-agent--get-context-workspace resolved-ctx)))
        (when workspace
          (push (cons name buf) (macher-agent-workspace-active-subagents workspace))))
      (macher-agent-scope-add-file name resolved-ctx))
    buf))

(defun macher-agent--prepare-subagent-instructions (buf instructions &optional presets)
  "Insert INSTRUCTIONS into BUF and compose its system message.

BUF is the target buffer.
INSTRUCTIONS is the instructions string.
PRESETS represents the optional requested presets.

Return nil.

Side effects: Modifies buffer content and buffer-local variables in BUF."
  (with-current-buffer buf
    (unless (string-empty-p instructions)
      (insert (substring-no-properties instructions)))
    (when presets
      (let* ((preset-list (if (listp presets) presets (list presets)))
             (base-state (list :model gptel-model
                               :system nil
                               :temperature (bound-and-true-p gptel-temperature)
                               :max-tokens (bound-and-true-p gptel-max-tokens)
                               :tools gptel-tools
                               :known-presets (bound-and-true-p gptel--known-presets)))
             (payload (macher-agent-compose-payload base-state preset-list)))
        (setq-local macher-agent-presets (delete-dups (append macher-agent-presets preset-list)))
        (macher-agent--apply-payload-locally payload)))))

(defun macher-agent--apply-single-virtual-buffer (entry)
  "Apply new content from VFS ENTRY to its buffer if present.

ENTRY is a VFS context entry cons cell.

Return nil.

Side effects: Erases and updates buffer text if target buffer exists."
  (let* ((path-or-buf (car entry))
         (new-content (if (consp (cdr entry)) (cddr entry) (cdr entry))))
    (when (and new-content (get-buffer path-or-buf))
      (with-current-buffer (get-buffer path-or-buf)
        (erase-buffer)
        (insert new-content)))))

(defun macher-agent-apply-virtual-buffers ()
  "Apply uncommitted VFS memory content back to virtual buffers.

Iterates over entries in active VFS context and writes uncommitted
virtual content back into matching Emacs buffers.

Return nil.

Side effects: Updates buffer contents and auto-syncs context."
  (interactive)
  (when-let* ((ctx (macher-agent-resolve-context))
              (contents (macher-agent--get-context-contents ctx)))
    (mapc #'macher-agent--apply-single-virtual-buffer contents)
    (macher-agent--auto-sync-context ctx)
    (message "Virtual buffers applied successfully.")))

(add-hook 'gptel-pre-response-hook
          (lambda ()
            (setq-local macher-agent--pending-children nil)
            (setq-local macher-agent--child-results nil)
            (setq-local macher-agent--resume-callback nil)
            (when-let* ((ctx (ignore-errors (macher-agent-resolve-context))))
              (macher-agent--auto-sync-context ctx))))

(defun macher-agent--dispatch-resume-callback (parent-buf callback results)
  "Schedule execution of CALLBACK on PARENT-BUF with RESULTS.

PARENT-BUF is the parent orchestrator buffer.
CALLBACK is the resume callback function.
RESULTS is the list of child results.

Return nil.

Side effects: Schedules a timer to execute CALLBACK."
  (when callback
    (run-at-time 0 nil
                 (lambda ()
                   (when (buffer-live-p parent-buf)
                     (with-current-buffer parent-buf
                       (funcall callback
                                (make-macher-agent-delegate-response
                                 :status 'success
                                 :payload (vconcat results)
                                 :buffer-name (buffer-name parent-buf)))))))))

(defun macher-agent-resume-parent-agent ()
  "Aggregate sub-agent results and resume the parent agent cycle.

Collects child results from buffer-local state and dispatches the stored
resume callback on a clean call stack.

Return nil.

Side effects: Resets child state variables and schedules resume callback."
  (let ((parent-buf (current-buffer))
        (callback macher-agent--resume-callback)
        (results (copy-sequence macher-agent--child-results)))
    (setq-local macher-agent--child-results nil)
    (setq-local macher-agent--pending-children nil)
    (setq-local macher-agent--resume-callback nil)
    (macher-agent--dispatch-resume-callback parent-buf callback results)))

(provide 'macher-agent-orchestration)
;;; macher-agent-orchestration.el ends here
