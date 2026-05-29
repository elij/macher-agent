;;; macher-agent-orchestration.el --- Interactive sub-agent commands -*- lexical-binding: t; -*-

(require 'macher)

(declare-function macher-agent--set-system-message "macher-agent-gptel-tools" (msg))
(declare-function macher-agent-current-context "macher-agent-context")
(declare-function macher-agent--auto-sync-context "macher-agent-context" (&optional ctx fsm))

(defvar macher-agent-subagent-setup-hook nil
  "Hook run after a subagent buffer is prepared.
This allows external packages (like macher-agent-skills) to apply
presets such as system messages and tools.")

(defun macher-agent--add-buffer-to-scope-headless (buf-name persistent-context)
  "Headless core logic for adding BUF-NAME to PERSISTENT-CONTEXT.
This bypasses user prompts to ensure orchestration remains decoupled
from the UI, strictly mutating the payload context."
  (get-buffer-create buf-name)
  (when persistent-context
    (let* ((contents (macher-context-contents persistent-context))
           (entry (assoc buf-name contents)))
      (unless entry
        (let ((orig (with-current-buffer buf-name
                      (buffer-substring-no-properties (point-min) (point-max)))))
          (setf (macher-context-contents persistent-context)
                (cons (cons buf-name (cons orig orig)) contents))))))
  (run-hooks 'macher-agent-context-mutated-hook))

;;;###autoload
(defun macher-agent-add-buffer-to-scope (buffer)
  "Manually add an Emacs BUFFER (existing or new) to the current agent's scope.
Acts as the interactive wrapper that passes the user's selected buffer
to the headless execution layer and reports success via message."
  (interactive "BAdd buffer to current agent's scope: ")
  (let* ((buf-name (if (stringp buffer) buffer (buffer-name buffer)))
         (ctx (macher-agent-current-context)))
    (macher-agent--add-buffer-to-scope-headless buf-name ctx)
    (message "SUCCESS: Added '%s' to the agent's restricted scope." buf-name)))

(defun macher-agent--resolve-buffer-name (name)
  "Return the clean buffer name.
Prefix forcing is removed as scope is handled explicitly by the
persistent context payload."
  (substring-no-properties name))

(defun macher-agent--prepare-subagent-buffer (buf full-dir context)
  "Set up internal state and clean mode initialisation for sub-agent BUF.
This configures internal state variables which are invisible but critical
for tools to function. It applies the worker preset silently in the
background and establishes the workspace directory to FULL-DIR alongside
the inherited CONTEXT."
  (with-current-buffer buf
    (setq default-directory full-dir)
    (setq-local macher-agent--is-workspace t)
    (setq-local macher-agent--is-subagent t)
    (setq-local macher--workspace (cons 'agent full-dir))
    (when context
      (setq-local macher-agent--persistent-context context))
    
    (unless (derived-mode-p 'markdown-mode)
      (markdown-mode))
    (unless gptel-mode
      (gptel-mode 1))
    
    (run-hooks 'macher-agent-subagent-setup-hook)))

;;;###autoload
(defun macher-agent-add-subagent (name dir &optional _display context)
  "Create a new sub-agent buffer completely silently.
No workspace path or system directives are printed to the buffer.
If CONTEXT is provided, the sub-agent inherits the persistent context.
Finally, this keeps the global tracking updated by pushing the new
sub-agent to `macher-agent-active-subagents`."
  (let* ((buf-name (format "*macher-agent: %s*" name))
         (buf (get-buffer-create buf-name))
         (safe-dir (if (and dir (stringp dir)) dir default-directory))
         (full-dir (file-name-as-directory (expand-file-name safe-dir))))
    
    (macher-agent--prepare-subagent-buffer buf full-dir context)
    
    (when (boundp 'macher-agent-active-subagents)
      (push (cons name full-dir) macher-agent-active-subagents))
    buf))

(add-hook 'gptel-pre-response-functions
          (lambda (&rest _)
            (let ((ctx (macher-agent-current-context)))
              (when ctx (macher-agent--auto-sync-context ctx)))))

(provide 'macher-agent-orchestration)
;;; macher-agent-orchestration.el ends here
