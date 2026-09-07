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
  "Add explicitly resolved BUFFER to active scope."
  (interactive "bAdd buffer to agent scope: ")
  (when (stringp buffer)
    (setq buffer (get-buffer buffer)))
  (cl-check-type buffer buffer)
  (let ((buf-name (buffer-name buffer))
        (ctx (bound-and-true-p macher-agent--persistent-context)))
    (if (not ctx)
        (error "ERROR: No active Macher Agent context found")
      (macher-agent-scope-add-file buf-name ctx)
      (message "SUCCESS: Added '%s' to the agent's authorised scope." buf-name))))

(defun macher-agent-prepare-instructions (buf preset instructions)
  "Format string INSTRUCTIONS in BUF using explicitly resolved PRESET list."
  (when (and (stringp preset) (not (stringp instructions)))
    (cl-rotatef preset instructions))
  (when (symbolp preset)
    (setq preset (list preset)))
  (cl-check-type buf buffer)
  (cl-check-type preset list)
  (cl-check-type instructions string)
  (with-current-buffer buf
    (when preset
      (setq-local macher-agent-presets preset)
      (macher-agent--apply-preset preset))
    (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)
    (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t)
    (unless (string-empty-p instructions)
      (erase-buffer)
      (insert instructions)))
  buf)

(defun macher-agent-workspace-root (workspace)
  "Return the root directory of WORKSPACE."
  (cl-check-type workspace macher-agent-context)
  (macher-agent-context-project-root workspace))

(defun macher-agent--set-workspace-root (workspace root-path)
  "Set the root directory of WORKSPACE."
  (cl-check-type workspace macher-agent-context)
  (setf (macher-agent-context-project-root workspace) root-path))

(gv-define-setter macher-agent-workspace-root (val workspace)
  `(macher-agent--set-workspace-root ,workspace ,val))

(defun macher-agent-context-workspace-root (context)
  "Extract the active workspace root directly from CONTEXT."
  (cl-check-type context macher-agent-context)
  (macher-agent-workspace-project-root context))

(defun macher--validate-path-in-workspace (path workspace)
  "Validate that PATH is strictly within WORKSPACE, expanding symlinks with `file-truename'.

PATH is the file path string to validate.
WORKSPACE is the workspace structure or object.

Return the canonical resolved path string.
Side effects: Signals error if PATH escapes WORKSPACE."
  (let* ((ws-root (cond
                   ((and (fboundp 'macher-agent-workspace-root)
                         (macher-agent-context-p workspace))
                    (macher-agent-workspace-root workspace))
                   ((consp workspace)
                    (file-truename (expand-file-name (if (stringp (cdr workspace)) (cdr workspace) (car workspace)))))
                   ((stringp workspace)
                    (file-truename (expand-file-name workspace)))
                   (t (file-truename default-directory))))
         (canonical-ws (and ws-root (file-name-as-directory (file-truename (expand-file-name ws-root)))))
         (canonical-path (and path (file-truename (expand-file-name path canonical-ws)))))
    (if (or (null canonical-ws)
            (file-in-directory-p canonical-path canonical-ws)
            (string-prefix-p canonical-ws canonical-path)
            (string-prefix-p canonical-ws (file-name-as-directory canonical-path))
            (string= canonical-path (directory-file-name canonical-ws)))
        canonical-path
      (error "SECURITY ERROR: You do not have permission to access '%s'" path))))

(defun macher-agent-log-tool-intent (context tool args target &optional extra)
  "Log intent directly mapped to CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (let* ((type (cond
                ((and (stringp tool) (member tool '("gptel-tool" "ptc"))) tool)
                ((stringp extra) extra)
                (t "gptel-tool")))
         (actual-target (cond
                         ((and (stringp args) (listp target)) args)
                         ((stringp target) target)
                         ((stringp tool) tool)
                         (t (format "%s" tool))))
         (actual-args (cond
                       ((listp target) target)
                       ((listp args) args)
                       (t nil)))
         (plugins (macher-agent-context-plugins context))
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
                  (target . ,actual-target)
                  (args . ,actual-args)))
         (updated-log (append log (list entry))))
    (setf (macher-agent-context-plugins context)
          (plist-put (copy-sequence plugins) :audit-log updated-log))
    updated-log))

(defun macher-agent-force-review (&optional context)
  "Trigger task flush hook for pending reviews.

CONTEXT is the optional active context structure.  When nil, resolves
the active context from `macher-agent--persistent-context'.

Return nil.

Side effects: Invokes task flush hooks."
  (interactive)
  (let ((ctx (or context
                 (bound-and-true-p macher-agent--persistent-context))))
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
         (active-presets ,presets)
         (caller-buf (current-buffer)))
     (with-current-buffer root-buf
       (unless (bound-and-true-p macher-agent--persistent-context)
         (setq macher-agent--persistent-context
               (or (macher-agent-context-from-buffer caller-buf)
                   (macher-agent--make-context :id "proxy-ctx" :project-root default-directory))))

       (if (fboundp 'markdown-mode)
           (markdown-mode)
         (text-mode))
       (when (fboundp 'gptel-mode)
         (gptel-mode 1))

       (when (buffer-live-p caller-buf)
         (setq-local gptel-model (buffer-local-value 'gptel-model caller-buf))
         (setq-local gptel-backend (buffer-local-value 'gptel-backend caller-buf))
         (setq-local gptel-temperature (buffer-local-value 'gptel-temperature caller-buf))
         (setq-local gptel-max-tokens (buffer-local-value 'gptel-max-tokens caller-buf))
         (setq-local gptel--known-presets (buffer-local-value 'gptel--known-presets caller-buf))
         (setq-local gptel-directives (buffer-local-value 'gptel-directives caller-buf)))

       (when (fboundp 'macher-agent-initialize-skills)
         (macher-agent-initialize-skills macher-agent--persistent-context))
       (when active-presets
         (setq-local macher-agent-presets active-presets)
         (macher-agent--apply-preset active-presets))

       (setq macher-agent--active-ptc-primitives allowed-primitives)

       (let ((script-str (prin1-to-string ',(if (> (length script-body) 1)
                                                `(progn ,@script-body)
                                              (car script-body))))
             (success-fn (or ,on-success (lambda (res) (message "PTC Success: %s" res))))
             (error-fn (or ,on-error (lambda (err) (message "PTC Error: %s" err)))))

         (macher-agent-execute-ptc-script
          script-str
          macher-agent--persistent-context
          root-buf
          (lambda (res)
            (funcall success-fn res)
            (when ,reap (kill-buffer root-buf)))
          (lambda (err)
            (funcall error-fn err)
            (when ,reap (kill-buffer root-buf)))
          nil)))))

(provide 'macher-agent-api)
;;; macher-agent-api.el ends here
