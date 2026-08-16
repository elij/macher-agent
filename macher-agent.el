;;; macher-agent.el --- Sandboxed, Language-Agnostic AI Workflows -*- lexical-binding: t; -*-

;; Author: Elijah Charles
;; Version: 0.8.1.1
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

(require 'macher-agent-core)
(require 'macher-agent-sandbox)
(require 'macher-agent-vfs)
(require 'macher-agent-presets)
(require 'macher-agent-macher)
(require 'macher-agent-gptel)
(require 'macher-agent-orchestration)
(require 'macher-agent-tools)
(require 'macher-agent-api)

(defgroup macher-agent nil
  "Agent tools within the macher edit context."
  :group 'gptel
  :prefix "macher-agent-")

(defun macher-agent-inject-thought (instruction)
  "Inject a user directive while the agent is processing a tool.

INSTRUCTION is a string representing the user directive to inject into
the pending queue.

Side effects: Appends INSTRUCTION formatted as a user override to the
pending instructions list for the current session.

Return the result message string displayed in the echo area."
  (interactive "sSteer the agent: ")
  (macher-agent-add-pending-instruction (format "USER OVERRIDE: %s" instruction))
  (message "Instruction queued! The agent will see this when its current tool finishes."))

(define-minor-mode macher-agent-mode
  "Minor mode for macher-agent session management."
  :lighter " MA"
  (if macher-agent-mode
      (progn
        (macher-agent-gptel-mode-setup)
        (macher-agent-setup-gptel-buffer)
        (macher-agent--wrap-macher-tools))
    (remove-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer t)
    (remove-hook 'gptel-prompt-transform-functions #'macher-agent--transform-inject-context t)
    (dolist (transformer macher-agent-prompt-transformers)
      (remove-hook 'gptel-prompt-transform-functions transformer t))
    (remove-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope t)))

(defun macher-agent-install ()
  "Explicitly install macher-agent global hooks, advices, and wrap tools."
  (interactive)
  (advice-add 'gptel--fsm-transition :around #'macher-agent--inject-media-fsm-advice)
  (advice-add 'gptel--insert-response :around #'macher-agent--protect-nil-responses)
  (advice-add 'gptel-curl--stream-insert-response
              :around #'macher-agent--protect-nil-responses)
  (advice-add 'gptel--restore-state :around #'macher-agent--gptel-restore-advice)
  (advice-add 'gptel--handle-pre-tool :around #'macher-agent--bind-active-fsm-advice)
  (advice-add 'gptel--handle-tool-use :around #'macher-agent--bind-active-fsm-advice)
  (advice-add 'gptel--handle-post-tool :around #'macher-agent--bind-active-fsm-advice)
  (advice-add 'macher--workspace-hash :override #'macher-agent--safe-workspace-hash)
  (advice-add 'gptel--fsm-transition :after #'macher-agent--trigger-patch-on-complete)
  (advice-add 'gptel--base64-encode :around #'macher-agent--gptel-base64-encode-advice)
  
  (add-hook 'gptel-pre-tool-call-functions #'macher-agent--log-gptel-pre-tool)
  (add-hook 'macher-agent-context-mutated-hook #'macher-agent--mutation-dispatcher)
  
  (with-eval-after-load 'macher
    (add-to-list
     'macher-workspace-types-alist
     '(agent . (:get-root macher-agent--get-root
                          :get-name macher-agent--get-name
                          :get-files macher-agent--get-files)))

    (defun macher-agent-workspace-agent ()
      "Identify if the current buffer is a workspace and return the workspace.

Return the workspace struct, or nil."
      (when (bound-and-true-p macher-agent--is-workspace)
        (bound-and-true-p macher--workspace)))

    (add-hook 'macher-workspace-functions #'macher-agent-workspace-agent))

  (with-eval-after-load 'gptel-transient
    (ignore-errors
      (transient-suffix-put 'gptel-menu 'gptel--infix-tools :save-history nil)
      (transient-suffix-put 'gptel-menu 'gptel--infix-system-message :save-history nil))))

(provide 'macher-agent)
;;; macher-agent.el ends here
