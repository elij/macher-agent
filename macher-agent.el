;;; macher-agent.el --- Sandboxed, Language-Agnostic AI Workflows -*- lexical-binding: t; -*-

;; Author: Elijah Charles
;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.0") (macher "0.5.0"))
;; Keywords: convenience, gptel, llm, macher
;; URL: https://github.com/elij/macher-agent
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Macher-Agent provides sandboxed, language-agnostic AI workflows
;; using sub-agent orchestration and virtual 3-way file merging.
;;

;;; Code:

(require 'cl-lib)
(require 'macher)
(require 'gptel)
(require 'project)

(require 'macher-agent-context)
(require 'macher-agent-async)
(require 'macher-agent-orchestration)
(require 'macher-agent-context-tools)
(require 'macher-agent-gptel-tools)

(defgroup macher-agent nil
  "Agent tools within the macher edit context ."
  :group 'gptel
  :prefix "macher-agent-")

(defvar macher-agent-active-subagents nil
  "Alist of active sub-agents and their locked directories.")

(defcustom macher-agent-global-skills-directory nil
  "Directory containing global agent skills."
  :type 'directory
  :group 'macher-agent)

(provide 'macher-agent)
;;; macher-agent.el ends here
