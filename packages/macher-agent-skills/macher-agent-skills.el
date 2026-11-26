;;; macher-agent-skills.el --- Bundled skill pack for macher-agent -*- lexical-binding: t; -*-

;; Author: Elijah Charles
;; Version: 0.8.3.67
;; Package-Requires: ((emacs "30.1") (gptel "0.9.0") (macher-agent "0.8.3.66"))
;; Keywords: convenience, tools
;; URL: https://github.com/elij/macher-agent
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Bundled orchestration skills, slash commands, and tools for `macher-agent'.
;; Registers its package directory via `macher-agent-register-skill-directory'
;; without overwriting user `macher-agent-skill-directories' settings.

;;; Code:

(require 'macher-agent)

(defconst macher-agent-skills-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory containing bundled `macher-agent-skills' assets.")

;;;###autoload
(defun macher-agent-skills-install ()
  "Register the `macher-agent-skills' directory with `macher-agent'."
  (interactive)
  (when (and macher-agent-skills-directory
             (file-directory-p macher-agent-skills-directory))
    (macher-agent-register-skill-directory macher-agent-skills-directory)))

;;;###autoload
(defun macher-agent-skills-uninstall ()
  "Unregister the `macher-agent-skills' directory from `macher-agent'."
  (interactive)
  (when macher-agent-skills-directory
    (macher-agent-unregister-skill-directory macher-agent-skills-directory)))

(add-hook 'macher-agent-install-hook #'macher-agent-skills-install)
(add-hook 'macher-agent-uninstall-hook #'macher-agent-skills-uninstall)

(provide 'macher-agent-skills)
;;; macher-agent-skills.el ends here
