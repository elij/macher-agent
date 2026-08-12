;;; macher-agent.el --- Sandboxed, Language-Agnostic AI Workflows -*- lexical-binding: t; -*-

;; Author: Elijah Charles
;; Version: 0.8.0.97
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

(defun macher-agent--main-gptel-setup ()
  "Initialise `macher-agent' configuration in `gptel' buffers.

Perform the complete setup sequence for gptel integration by executing
`macher-agent-gptel-mode-setup', `macher-agent-setup-gptel-buffer',
and wrapping available macher tools with `macher-agent--wrap-macher-tools'.

Side effects: Configures buffer-local variables, mode hooks, and tool
wrappers for `gptel-mode'.

Return the result of `macher-agent--wrap-macher-tools'."
  (macher-agent-gptel-mode-setup)
  (macher-agent-setup-gptel-buffer)
  (macher-agent--wrap-macher-tools))

(add-hook 'gptel-mode-hook #'macher-agent--main-gptel-setup)

(provide 'macher-agent)
;;; macher-agent.el ends here
