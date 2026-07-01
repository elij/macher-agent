;;; macher-agent-orchestration.el --- Interactive sub-agent commands -*- lexical-binding: t; -*-

(require 'macher)
(require 'macher-agent-macher-bridge)
(require 'gptel nil t)
(require 'macher-agent-vfs-client)

(declare-function macher-agent-gptel-transmit "macher-agent-gptel-bridge" (task-context callbacks))
(declare-function macher-agent-sync-prompt-transformer "macher-agent-gptel-bridge" (async-fn fsm))
(declare-function macher-agent-post-response-reaper "macher-agent-gptel-bridge" (beg end))
(declare-function macher-agent-resolve-context "macher-agent-vfs-client")
(declare-function macher-agent--inject-context-state "macher-agent-vfs-client" (context &optional directives))
(declare-function macher-agent--init-workspace-state "macher-agent-vfs-client")
(declare-function macher-agent--auto-sync-context "macher-agent-vfs-client" (&optional ctx fsm))
(declare-function macher-agent-vfs-entry-path "macher-agent-vfs-client")
(declare-function macher-agent-vfs-entry-curr "macher-agent-vfs-client")

(defvar macher-agent-submit-task-result-tool)

(cl-defstruct macher-agent-task-context
  workspace
  target-buffer
  skill-sym
  system-message)

(defun macher-agent-execute-parallel (tasks final-callback)
  "Execute TASKS in parallel and run FINAL-CALLBACK upon completion.

TASKS is the list of tasks to spawn.
FINAL-CALLBACK is the function to run with the results.

Return nil."
  (let* ((task-list (append tasks nil))
         (total (length task-list))
         (completed 0)
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
                       (unless (aref completed-flags idx)
                         (aset completed-flags idx t)
                         (setf (nth idx results) result)
                         (cl-incf completed)
                         (when (= completed total)
                           (funcall final-callback results))))))))))

(defun macher-agent-subagent-p (&optional buffer)
  "Return non-nil if BUFFER is a subagent.

BUFFER is the optional buffer to check, defaulting to the current buffer.

Return non-nil if buffer is a subagent, otherwise nil."
  (with-current-buffer (or buffer (current-buffer))
    (bound-and-true-p macher-agent--is-subagent)))

(defun macher-agent-ready-to-reap-p (&optional buffer)
  "Return non-nil if BUFFER is ready to be reaped.

BUFFER is the optional buffer to check, defaulting to the current buffer.

Return non-nil if buffer is ready to be reaped, otherwise nil."
  (with-current-buffer (or buffer (current-buffer))
    (bound-and-true-p macher-agent--ready-to-reap)))

(defun macher-agent--reap-buffer (buf)
  "A garbage collector that natively aborts hidden gptel networks.

BUF is the buffer to reap.

Return nil."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (macher-agent-subagent-p)
                 (macher-agent-ready-to-reap-p))
        (set-buffer-modified-p nil)
        (when (fboundp 'gptel-abort)
          (gptel-abort buf))
        (let ((kill-buffer-query-functions nil)
              (kill-buffer-hook nil))
          (kill-buffer buf))))))

(defun macher-normalise-preset-name (preset)
  "Normalise a preset name, stripping @ symbols safely even if passed a list.

PRESET is the preset identifier (string, symbol, or list).

Return the normalised preset symbol, or nil."
  (let ((str (cond ((stringp preset) preset)
                   ((symbolp preset) (symbol-name preset))
                   ((and (listp preset) (stringp (car preset))) (car preset))
                   (t (format "%s" preset)))))
    (intern (replace-regexp-in-string "^@+" "" str))))

(defun macher-agent-compose-payload (base-state inline-presets)
  "Pure function to compose a transmission payload.
Merges BASE-STATE with the resolved configuration of INLINE-PRESETS
using native gptel modification values.

The function clones the initial base state and processes each preset or
standalone tool sequentially. For each preset, it recursively resolves
any declared parent dependencies first.

If a preset declares the `:exclusive' flag, it completely clears the
accumulated system prompt and tools before applying its own configurations,
preventing base environment bleed.

System prompts and tools are merged using gptel's declarative modifier logic.
Legacy string-based system prompts are automatically wrapped in native
append operations, and the original `\n---\n' delimiter is dynamically
injected if a preceding system prompt exists. Flat properties like model,
temperature, and token limits are applied as direct overrides.

Finally, if available, `macher-agent-normalize-tools' is invoked on the
composed tools list.

BASE-STATE is the base state property list.
INLINE-PRESETS is the list of inline preset symbols.

Return the unified state property list."
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
             (setq state (plist-put state :tools nil)))

           (when-let* ((sys-spec (or (plist-get spec :system) (plist-get spec :system-message))))
             (when (stringp sys-spec)
               (let ((current-sys (plist-get state :system)))
                 (setq sys-spec `(:append ,(concat (if current-sys "\n---\n" "")
                                                   (format "### Skill: %s\n%s\n" sym sys-spec))))))
             (setq state (plist-put state :system
                                    (gptel--modify-value (plist-get state :system) sys-spec))))

           (when-let* ((tools-spec (or (plist-get spec :tools) (plist-get spec :allowed-tools))))
             (let ((merged-tools (gptel--modify-value (plist-get state :tools) tools-spec)))
               (setq state (plist-put state :tools 
                                      (cl-loop for t-obj in (if (listp merged-tools) merged-tools (list merged-tools))
                                               collect (if (stringp t-obj) 
                                                           (or (ignore-errors (gptel-get-tool t-obj)) t-obj) 
                                                         t-obj))))))

           (dolist (key '(:model :temperature :max-tokens))
             (when-let* ((val (plist-get spec key)))
               (setq state (plist-put state key val))))))

      (dolist (sym inline-presets)
        (let* ((clean-sym (if (fboundp 'macher-normalise-preset-name)
                              (macher-normalise-preset-name sym) sym))
               (spec (when clean-sym (alist-get clean-sym known)))
               (tool (when (and (not spec) clean-sym (fboundp 'gptel-get-tool))
                       (or (ignore-errors (gptel-get-tool (symbol-name clean-sym)))
                           (ignore-errors (gptel-get-tool (replace-regexp-in-string "-" "_" (symbol-name clean-sym))))))))
          (cond
           (spec (apply-spec sym spec))
           (tool (setq state (plist-put state :tools (append (plist-get state :tools) (list tool)))))))))

    (when (fboundp 'macher-agent-normalize-tools)
      (setq state (plist-put state :tools (macher-agent-normalize-tools (plist-get state :tools)))))

    state))

(defun macher-agent--apply-payload-locally (payload)
  "Apply a composed payload to the current buffer variables.

PAYLOAD is the composed state property list.

Return nil."
  (when payload
    (when (plist-member payload :system) (setq-local gptel-system-prompt (plist-get payload :system)))
    (when (plist-member payload :model) (setq-local gptel-model (plist-get payload :model)))
    (when (plist-member payload :temperature) (setq-local gptel-temperature (plist-get payload :temperature)))
    (when (plist-member payload :max-tokens) (setq-local gptel-max-tokens (plist-get payload :max-tokens)))
    (when (plist-member payload :tools) (setq-local gptel-tools (plist-get payload :tools)))))

(defun macher-agent--apply-preset (preset-or-presets)
  "Polymorphic wrapper to safely route legacy calls into the modern compositor.

PRESET-OR-PRESETS represents the preset symbol, list, or vector.

Return nil."
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

(defvar macher-agent--is-subagent nil)
(defvar macher-agent--ready-to-reap nil)

(defvar-local macher-agent-presets nil
  "List of active preset or skill symbols for the current buffer.")

(put 'macher-agent--is-subagent 'permanent-local t)
(put 'macher-agent--ready-to-reap 'permanent-local t)
(put 'macher-agent-presets 'permanent-local t)

(defun macher-agent-spawn-task (task callback)
  "Spawn a task inside a target subagent.

TASK is the task specification list or name.
CALLBACK is the completion callback function.

Return nil."
  (let* ((buf-name (if (listp task) (plist-get task :buffer_name) task))
         (instructions (if (listp task) (plist-get task :instructions) ""))
         (presets (if (listp task) (or (plist-get task :presets) (plist-get task :preset)) nil))
         (is-background (and (listp task) (plist-get task :background)))
         (buf (get-buffer buf-name))
         (callback-fired nil))
    (if (not buf)
        (funcall callback (make-macher-agent-delegate-response :status 'error :error (format "ERROR: Sub-agent buffer '%s' not found." buf-name) :buffer-name buf-name))
      (macher-agent--prepare-subagent-instructions buf instructions presets)
      (with-current-buffer buf
        (unless is-background
          (macher-agent--show-ui buf))
        
        (setq-local macher-agent--is-subagent t)
        
        (setq-local macher-agent--parent-callback 
                    (lambda (res)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf 
                          (unless is-background
                            (setq-local macher-agent--ready-to-reap t))))
                      
                      (unless callback-fired
                        (setq callback-fired t)
                        (when (buffer-live-p buf)
                          (with-current-buffer buf 
                            (unless is-background
                              (setq-local macher-agent--ready-to-reap t))))
                        (funcall callback res))))

        (let* ((first-preset (cond ((stringp presets) presets)
                                   ((symbolp presets) (symbol-name presets))
                                   ((and (listp presets) (stringp (car presets))) (car presets))
                                   ((and (vectorp presets) (> (length presets) 0) (stringp (aref presets 0))) (aref presets 0))
                                   (t nil)))
               (clean-sym (when first-preset (intern (replace-regexp-in-string "^@+" "" first-preset))))
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
                             (funcall macher-agent--parent-callback (make-macher-agent-delegate-response :status 'error :error (format "ERROR: %s" err) :buffer-name buf-name))))))))))

(defvar macher-agent-subagent-setup-hook nil)

(defun macher-agent--add-buffer-to-scope-headless (buf-name persistent-context)
  "Add BUF-NAME to PERSISTENT-CONTEXT scope headlessly.

BUF-NAME is the string buffer name.
PERSISTENT-CONTEXT is the active context structure.

Return nil."
  (get-buffer-create buf-name)
  (when persistent-context
    (let* ((contents (macher-agent--get-context-contents persistent-context))
           (entry (cl-find buf-name contents :key #'macher-agent-vfs-entry-path :test #'equal)))
      (unless entry
        (let ((orig (with-current-buffer buf-name (buffer-substring-no-properties (point-min) (point-max)))))
          (macher-agent--set-context-contents persistent-context
                                              (cons (macher-agent-vfs-make-entry buf-name orig orig) contents))))))
  (run-hooks 'macher-agent-context-mutated-hook))

;;;###autoload
(defun macher-agent-add-buffer-to-scope (buffer)
  "Add BUFFER to the scope of the current agent.

BUFFER is the buffer object or string name.

Return nil."
  (interactive "BAdd buffer to current agent's scope: ")
  (let* ((buf-name (if (stringp buffer) buffer (buffer-name buffer)))
         (ctx (macher-agent-resolve-context)))
    (unless ctx
      (error "No active agent session found"))
    (macher-agent--add-buffer-to-scope-headless buf-name ctx)
    (message "SUCCESS: Added '%s' to the agent's restricted scope." buf-name)))

(defun macher-agent--resolve-buffer-name (name)
  "Coerce NAME into a string name.

NAME is the string representing buffer name.

Return the coerced buffer name string."
  (substring-no-properties name))

(defun macher-agent--prepare-subagent-buffer (buf full-dir context &optional presets parent-tools parent-model parent-backend parent-presets parent-directives parent-temp parent-tokens)
  "Prepare a subagent buffer, locking its directory and composing its requested skills.

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

Return nil."
  (with-current-buffer buf
    (setq default-directory (file-name-as-directory (macher-agent-root full-dir)))
    
    (when (and (fboundp 'markdown-mode) (not (derived-mode-p 'markdown-mode)))
      (markdown-mode))
    
    (setq-local macher-agent--is-subagent t)
    
    (when (and (fboundp 'gptel-mode) (not gptel-mode))
      (gptel-mode 1))
    
    (setq-local gptel-stream nil)
    
    (when context
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
  "Create and prepare a new subagent buffer, inheriting parent state and composing PRESETS.

NAME is the subagent name string.
DIR is the workspace directory path string.
INSTRUCTIONS is the optional instructions string.
CONTEXT is the optional active context structure.
PRESETS represents optional requested presets.

Return the newly created subagent buffer."
  (let* ((parent-tools (bound-and-true-p gptel-tools))
         (parent-model (bound-and-true-p gptel-model))
         (parent-backend (bound-and-true-p gptel-backend))
         (parent-presets (bound-and-true-p gptel--known-presets))
         (parent-directives (bound-and-true-p gptel-directives))
         (parent-temp (bound-and-true-p gptel-temperature))
         (parent-tokens (bound-and-true-p gptel-max-tokens))
         (buf (get-buffer-create name)))
    
    (macher-agent--prepare-subagent-buffer
     buf dir context presets parent-tools parent-model parent-backend
     parent-presets parent-directives parent-temp parent-tokens)
    
    (when context
      (let ((workspace (macher-agent--get-context-workspace context)))
        (when workspace
          (push (cons name buf) (macher-agent-workspace-active-subagents workspace))))
      (macher-agent-scope-add-file name context))
    buf))

(defun macher-agent--prepare-subagent-instructions (buf instructions &optional presets)
  "Insert INSTRUCTIONS into BUF and compose its system message.

BUF is the target buffer.
INSTRUCTIONS is the instructions string.
PRESETS represents the optional requested presets.

Return nil."
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

(defun macher-agent-apply-virtual-buffers ()
  "Apply the uncommitted VFS memory content back to virtual buffers.

Return nil."
  (interactive)
  (let* ((ctx (macher-agent-resolve-context))
         (contents (and ctx (macher-agent--get-context-contents ctx))))
    (when contents
      (dolist (entry contents)
        (let* ((path-or-buf (macher-agent-vfs-entry-path entry))
               (new-content (macher-agent-vfs-entry-curr entry)))
          (when (and new-content (get-buffer path-or-buf))
            (with-current-buffer (get-buffer path-or-buf)
              (erase-buffer)
              (insert new-content))))))
    (macher-agent--auto-sync-context ctx)
    (message "Virtual buffers applied successfully.")))

(add-hook 'gptel-pre-response-hook
          (lambda ()
            (let ((ctx (ignore-errors (macher-agent-resolve-context))))
              (when ctx (macher-agent--auto-sync-context ctx)))))

(provide 'macher-agent-orchestration)
