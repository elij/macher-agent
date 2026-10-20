;;; macher-agent.el --- Sandboxed, Language-Agnostic AI Workflows -*- lexical-binding: t; -*-

;; Author: Elijah Charles
;; Version: 0.8.3.56
;; Package-Requires: ((emacs "30.1") (gptel "0.9.9.6") (macher "0.5.2"))
;; Keywords: convenience, gptel, llm, macher
;; URL: https://github.com/elij/macher-agent
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; An Emacs-native LLM agent harness featuring isolated sandboxing,
;; asynchronous sub-agent orchestration, and fail-fast sync file merging.
;;

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'project)
(require 'macher-agent-api)

(defgroup macher-agent nil
  "Agent tools within the macher edit context."
  :group 'gptel
  :prefix "macher-agent-")

(define-minor-mode macher-agent-mode
  "Minor mode for macher-agent session management."
  :lighter " MA"
  (if macher-agent-mode
      (condition-case err
          (progn
            (macher-agent-gptel-mode-setup)
            (macher-agent-setup-gptel-buffer)
            (macher-agent--wrap-macher-tools))
        (user-error
         (setq macher-agent-mode nil)
         (user-error "%s" (error-message-string err))))
    (remove-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer t)
    (dolist (transformer macher-agent-prompt-transformers)
      (remove-hook 'gptel-prompt-transform-functions transformer t))
    (remove-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope t)))

(defun macher-agent-force-enable ()
  "Force Macher Agent to treat current directory as a workspace root."
  (interactive)
  (setq-local macher-agent--is-workspace nil)
  (macher-agent--init-workspace-state
   (file-name-as-directory (expand-file-name default-directory)))
  (setq-local macher-agent--is-workspace t)
  (macher-agent-mode 1)
  (message "Macher-Agent manually forced on for: %s" default-directory))

(provide 'macher-agent)
;;; macher-agent.el ends here
