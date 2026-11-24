;;; macher-agent-api.el --- Public API facade for Macher Agent -*- lexical-binding: t; -*-

;;; Commentary:
;; Public user-facing API facade and top-level installer for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'macher-agent-core)
(require 'macher-agent-presets)
(require 'macher-agent-gptel)
(require 'macher-agent-tools)
(require 'macher-agent-orchestration)

(declare-function transient-suffix-put "transient" (prefix loc prop value))

;;;###autoload
(defun macher-agent-force-review (&optional context)
  "Trigger task flush hook for pending reviews in CONTEXT."
  (interactive)
  (let ((ctx (or context (macher-agent-resolve-context (current-buffer)))))
    (unless (macher-agent-valid-context-p ctx)
      (user-error "No active Macher Agent context found"))
    (macher-agent-run-task-flush-hook ctx)))

;;;###autoload
(defun macher-agent-install ()
  "Install core pipeline steps, hooks, and registered plugin extensions."
  (interactive)
  (macher-agent-transmission-install)
  (setq macher-agent-direct-task-submit-function
        #'macher-agent-submit-direct-task-result)
  (unless macher-agent-global-skills-alist
    (ignore-errors (macher-agent-initialize-skills)))
  (run-hooks 'macher-agent-install-hook)
  (run-hooks 'macher-agent-wrap-tools-hook)

  (add-hook 'macher-agent-buffer-flush-hook #'macher-agent-flush-sweep-buffer)
  (add-hook 'gptel-prompt-transform-functions #'macher-agent-fsm-hijack-transform)
  (add-hook 'hack-local-variables-hook #'macher-agent-restore-local-state -50)
  (add-hook 'gptel-save-state-hook #'macher-agent-prepare-local-vars-before-save -50)
  (add-hook 'gptel-save-state-hook #'macher-agent-sync-preset-before-save)
  (add-hook 'before-save-hook #'macher-agent-externalize-oversized-gptel-bounds 85)
  (add-hook 'before-save-hook #'macher-agent-persist-missing-preset-tools 90)
  (add-hook 'gptel-pre-tool-call-functions #'macher-agent-log-gptel-pre-tool)
  (add-hook 'macher-agent-context-mutated-hook #'macher-agent-mutation-dispatcher)

  (when (require 'gptel-transient nil t)
    (ignore-errors
      (transient-suffix-put 'gptel-menu 'gptel--infix-tools :save-history nil))))

;;;###autoload
(defun macher-agent-uninstall ()
  "Remove core hooks and tear down registered plugin integrations."
  (interactive)
  (run-hooks 'macher-agent-uninstall-hook)
  (remove-hook 'macher-agent-buffer-flush-hook #'macher-agent-flush-sweep-buffer)
  (remove-hook 'gptel-prompt-transform-functions #'macher-agent-fsm-hijack-transform)
  (remove-hook 'hack-local-variables-hook #'macher-agent-restore-local-state)
  (remove-hook 'gptel-save-state-hook #'macher-agent-prepare-local-vars-before-save)
  (remove-hook 'gptel-save-state-hook #'macher-agent-sync-preset-before-save)
  (remove-hook 'before-save-hook #'macher-agent-externalize-oversized-gptel-bounds)
  (remove-hook 'before-save-hook #'macher-agent-persist-missing-preset-tools)
  (remove-hook 'gptel-pre-tool-call-functions #'macher-agent-log-gptel-pre-tool)
  (remove-hook 'macher-agent-context-mutated-hook #'macher-agent-mutation-dispatcher))

(provide 'macher-agent-api)
;;; macher-agent-api.el ends here
