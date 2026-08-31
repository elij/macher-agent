;;; macher-agent-api.el --- Public API for macher-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Public API implementation for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'generator)
(require 'macher-agent-core)
(require 'macher-agent-gptel)
(require 'macher-agent-presets)
(require 'macher-agent-tools)
(require 'macher-agent-sandbox)
(require 'macher-agent-vfs)
(require 'macher-agent-zero-mem)
(require 'macher-agent-orchestration)
(require 'macher-agent-macher nil t)

(defmacro macher-agent-with-project-root (&rest body)
  "Execute BODY with default directory bound to the project root.

BODY forms are evaluated sequentially with `default-directory` set to
the resolved project root directory.

Return the result of evaluating the last form in BODY.

Side effects: Temporarily rebinds `default-directory` during execution."
  `(let ((default-directory (file-name-as-directory (macher-agent-root))))
     ,@body))

(defun macher-agent-workspace-resolve-path (path)
  "Resolve PATH into an absolute buffer name string.

PATH is the relative or absolute path string to resolve.

Return the resolved absolute buffer name string.

Side effects: None."
  (macher-agent--resolve-buffer-name path))

(defun macher-agent-context-read (context file)
  "Read content of FILE from CONTEXT.

CONTEXT is the active context structure.
FILE is the relative file path string.

Return the file content string, or nil if not found.

Side effects: None."
  (if (fboundp 'macher-agent--read-context-file)
      (macher-agent--read-context-file context file)))

(defun macher-agent-context-update (context file content)
  "Update FILE in CONTEXT with new CONTENT string.

CONTEXT is the active context structure.
FILE is the relative file path string.
CONTENT is the new text content string.

Return nil.

Side effects: Mutates CONTEXT in place to store updated file content."
  (if (fboundp 'macher-agent--update-context-file)
      (macher-agent--update-context-file context file content)))

(defun macher-agent-scope-add-file (buffer-name context)
  "Add BUFFER-NAME to the authorised scope in CONTEXT.

BUFFER-NAME is the name of the buffer string to authorise.
CONTEXT is the active context structure.

Return nil.

Side effects: Mutates CONTEXT scope list."
  (when-let* ((buf (get-buffer buffer-name))
              (content (with-current-buffer buf
                         (buffer-substring-no-properties (point-min) (point-max)))))
    (macher-agent-context-update context buffer-name content)))

(defun macher-agent-add-buffer-to-scope (buffer)
  "Interactively add a BUFFER to the agent's authorised virtual file system scope.

BUFFER is the name of the buffer to authorise.

Return nil.

Side effects: Mutates the active agent context and displays a message."
  (interactive "bAdd buffer to agent scope: ")
  (let* ((buf-name (if (bufferp buffer) (buffer-name buffer) buffer))
         (ctx (bound-and-true-p macher-agent--persistent-context)))
    (if (not ctx)
        (error "ERROR: No active Macher Agent context found")
      (macher-agent-scope-add-file buf-name ctx)
      (message "SUCCESS: Added '%s' to the agent's authorised scope." buf-name))))

(defun macher-agent-prepare-instructions (buf instructions preset)
  "Prepare and inject sub-agent INSTRUCTIONS into BUF using PRESET.

BUF is the target sub-agent buffer.
INSTRUCTIONS is the directive text string.
PRESET is the preset symbol or string representing requested skills.

Return target buffer object.

Side effects: Modifies sub-agent buffer local state and directives."
  (when-let* ((target-buf (if (bufferp buf) buf (get-buffer buf))))
    (with-current-buffer target-buf
      (when preset
        (let ((preset-list (cond ((listp preset) preset)
                                 ((vectorp preset) (append preset nil))
                                 (t (list preset)))))
          (setq-local macher-agent-presets preset-list)
          (macher-agent--apply-preset preset-list)))
      (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)
      (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t)
      (when (and (stringp instructions) (not (string-empty-p instructions)))
        (erase-buffer)
        (insert instructions)))
    target-buf))

(defun macher-agent-workspace-root (workspace)
  "Retrieve the project root directory path of WORKSPACE.

WORKSPACE is the target workspace structure or object.

Return the project root path string, or nil if unresolved.

Side effects: None."
  (macher-agent-workspace-project-root workspace))

(defun macher-agent-context-workspace-root (context)
  "Retrieve the project root directory from CONTEXT structure.

CONTEXT is the active context structure.

Return the project root path string, or nil if unresolved.

Side effects: None."
  (when context
    (or (when (macher-agent-context-p context)
          (macher-agent-context-project-root context))
        (when-let* ((workspace (macher-agent-context-workspace context)))
          (macher-agent-workspace-project-root workspace)))))

(defalias 'macher-context-workspace-root #'macher-agent-context-workspace-root)

(defun macher-agent-log-tool-intent (context type target args)
  "Log tool execution intent entry to CONTEXT audit log.

CONTEXT is the active context structure.
TYPE is the invocation type string, such as gptel-tool or ptc.
TARGET is the tool identifier name string or PTC primitive symbol.
ARGS is the list of parameters passed to the tool.

Return the updated audit log list.

Side effects: Mutates CONTEXT audit log property list."
  (when (and context (macher-agent-context-p context))
    (let* ((plugins (macher-agent-context-plugins context))
           (log (when (macher-agent--plist-p plugins)
                  (plist-get plugins :audit-log)))
           (preset-sym (bound-and-true-p macher-agent--active-skill-sym))
           (preset-str (if preset-sym
                           (if (symbolp preset-sym)
                               (symbol-name preset-sym)
                             (format "%s" preset-sym))
                         "unknown"))
           (entry `((timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S"))
                    (buffer . ,(buffer-name))
                    (preset . ,preset-str)
                    (type . ,type)
                    (target . ,target)
                    (args . ,args))))
      (setf (macher-agent-context-plugins context)
            (plist-put (copy-sequence plugins) :audit-log (append log (list entry)))))))

(defun macher-agent--log-gptel-pre-tool (tool &optional fsm &rest args)
  "Log GPTEL pre-tool call intent to context audit log.

TOOL is the tool structure or property list.
FSM is the optional state machine object.
ARGS represents additional arguments passed to the tool call.

Return nil.

Side effects: Appends tool call entry to active context audit log."
  (let ((context (or (when fsm (or (macher-agent-gptel--fsm-context fsm)
                                   (ignore-errors (macher-agent-resolve-context fsm))))
                     (bound-and-true-p macher-agent--persistent-context)
                     (ignore-errors (macher-agent-resolve-context (current-buffer)))
                     (ignore-errors (macher-agent-resolve-context))))
        (tool-name (macher-agent-canonical-tool-name tool))
        (tool-args (if (and (macher-agent--plist-p tool) (plist-member tool :args))
                       (plist-get tool :args)
                     args)))
    (when context
      (macher-agent-log-tool-intent context "gptel-tool" tool-name tool-args))))

(defun macher-agent-force-review (&optional context)
  "Trigger task flush hook for pending reviews.

CONTEXT is the optional active context structure.  When nil, resolves
the active context.

Return nil.

Side effects: Invokes task flush hooks."
  (interactive)
  (let ((ctx (or context
                 (bound-and-true-p macher-agent--persistent-context)
                 (ignore-errors (macher-agent-resolve-context)))))
    (macher-agent-run-task-flush-hook ctx)))


(cl-defmacro macher-agent-execute-detached-ptc ((&key (buffer-name "*workspace-root*")
                                                      (reap t)
                                                      primitives
                                                      presets
                                                      on-success on-error)
                                                &rest script-body)
  "Execute SCRIPT-BODY as a sandboxed PTC script in a background workspace root."
  (declare (indent 1) (debug t))
  `(let ((root-buf (get-buffer-create ,buffer-name))
         (allowed-primitives ,primitives)
         (active-presets ,presets))
     (with-current-buffer root-buf
       (unless (bound-and-true-p macher-agent--persistent-context)
         (setq-local macher-agent--persistent-context
                     (macher-agent--make-context :id "proxy-ctx" :project-root default-directory)))
       (when (fboundp 'macher-agent-resolve-tools)
         (macher-agent-resolve-tools macher-agent--persistent-context active-presets))
       (let ((native-fsm (gptel-request nil :dry-run t))
             (script-str (prin1-to-string ',(if (> (length script-body) 1)
                                                `(progn ,@script-body)
                                              (car script-body))))
             (success-cb (or ,on-success (lambda (res) (message "PTC Success: %s" res))))
             (error-cb (or ,on-error (lambda (err) (message "PTC Error: %s" err)))))

         (cl-letf (((symbol-function 'macher-agent-get-active-fsm)
                    (lambda (&rest _) native-fsm))
                   ((symbol-function 'macher-agent--ptc-primitive-p)
                    (lambda (sym &rest _)
                      (or (memq sym allowed-primitives)
                          (and (fboundp 'macher-agent-sandbox--primitives)
                               (hash-table-p macher-agent-sandbox--primitives)
                               (gethash sym macher-agent-sandbox--primitives))))))

           (macher-agent-execute-ptc-script
            script-str
            macher-agent--persistent-context
            (lambda (res)
              (funcall success-cb res)
              (when ,reap (kill-buffer root-buf)))
            (lambda (err)
              (funcall error-cb err)
              (when ,reap (kill-buffer root-buf)))
            nil
            root-buf))))))

(provide 'macher-agent-api)
;;; macher-agent-api.el ends here
