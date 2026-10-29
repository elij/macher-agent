;;; macher-agent-api.el --- Public API facade for Macher Agent -*- lexical-binding: t; -*-

;;; Commentary:
;; Public user-facing API facade and top-level plugin installer for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'macher-agent-core)
(require 'macher-agent-presets)
(require 'macher-agent-gptel)
(require 'macher-agent-tools)
(require 'macher-agent-orchestration)
(require 'macher-agent-vfs)
(require 'macher-agent-sandbox)
(require 'macher-agent-zero-mem)

(declare-function transient-suffix-put "transient" (prefix loc prop value))

;;;###autoload
(defun macher-agent-force-review (&optional context)
  "Trigger task flush hook for pending reviews in CONTEXT."
  (interactive)
  (let ((ctx (or context (macher-agent-resolve-context (current-buffer)))))
    (unless (macher-agent-valid-context-p ctx)
      (user-error "No active Macher Agent context found"))
    (macher-agent-run-task-flush-hook ctx)))

(defun macher-agent-install ()
  "Install all core pipeline steps, VFS hooks, and plugin components."
  (interactive)
  (macher-agent-sandbox-install)
  (macher-agent-zero-mem-install)
  (macher-agent-vfs-install)
  (macher-agent-transmission-install)

  (add-hook 'macher-agent-buffer-flush-hook #'macher-agent--flush-sweep-buffer)
  (add-hook 'gptel-prompt-transform-functions #'macher-agent--fsm-hijack-transform)
  (add-hook 'hack-local-variables-hook #'macher-agent--restore-local-state)
  (add-hook 'gptel-pre-tool-call-functions #'macher-agent--log-gptel-pre-tool)
  (add-hook 'macher-agent-context-mutated-hook #'macher-agent--mutation-dispatcher)

  (with-eval-after-load 'gptel-transient
    (ignore-errors
      (transient-suffix-put 'gptel-menu 'gptel--infix-tools :save-history nil))))

(provide 'macher-agent-api)
;;; macher-agent-api.el ends here
