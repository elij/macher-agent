;;; macher-agent.el --- Sandboxed, Language-Agnostic AI Workflows -*- lexical-binding: t; -*-

;; Author: Elijah Charles
;; Version: 0.8.2.5
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
    (dolist (transformer macher-agent-prompt-transformers)
      (remove-hook 'gptel-prompt-transform-functions transformer t))
    (remove-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope t)))

(defun macher-agent-install ()
  "Explicitly install macher-agent global hooks, tools, and FSM integrations."
  (interactive)
  (macher-agent-context-resolution-install)
  (macher-agent-sandbox-install)
  (macher-agent-zero-mem-install)
  (macher-agent-vfs-install)
  (macher-agent-transmission-install)

  (add-hook 'gptel-prompt-transform-functions #'macher-agent--fsm-hijack-transform)
  (add-hook 'hack-local-variables-hook #'macher-agent--restore-local-state)
  (add-hook 'gptel-pre-tool-call-functions #'macher-agent--log-gptel-pre-tool)
  (add-hook 'macher-agent-context-mutated-hook #'macher-agent--mutation-dispatcher)

  (with-eval-after-load 'gptel-transient
    (ignore-errors
      (transient-suffix-put 'gptel-menu 'gptel--infix-tools :save-history nil))))

(defalias 'macher-agent-setup #'macher-agent-install)

(provide 'macher-agent)
;;; macher-agent.el ends here
