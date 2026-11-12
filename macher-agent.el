;;; macher-agent.el --- Sandboxed, Language-Agnostic AI Workflows -*- lexical-binding: t; -*-

;; Author: Elijah Charles
;; Version: 0.8.3.59
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

(defun macher-agent--strip-yanked-properties (beg end)
  "Clear all text properties in the yanked region between BEG and END."
  (when (and (integer-or-marker-p beg)
             (integer-or-marker-p end))
    (let ((start (max (point-min) (min beg end)))
          (stop (min (point-max) (max beg end)))
          (inhibit-read-only t))
      (when (< start stop)
        (set-text-properties start stop nil)))))

(defun macher-agent-yank (&optional arg)
  "Yank text with ARG and clear all text properties in `macher-agent-mode'."
  (interactive "*P")
  (let ((yank-excluded-properties t))
    (yank arg))
  (macher-agent--strip-yanked-properties (mark t) (point)))

(defun macher-agent-yank-pop (&optional arg)
  "Replace just-yanked text with ARG and clear all text properties."
  (interactive "*p")
  (let ((yank-excluded-properties t))
    (yank-pop arg))
  (macher-agent--strip-yanked-properties (mark t) (point)))

(defvar macher-agent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap yank] #'macher-agent-yank)
    (define-key map [remap yank-pop] #'macher-agent-yank-pop)
    map)
  "Keymap for `macher-agent-mode'.")

(define-minor-mode macher-agent-mode
  "Minor mode for macher-agent session management."
  :lighter " MA"
  :keymap macher-agent-mode-map
  (if macher-agent-mode
      (condition-case err
          (progn
            (macher-agent-gptel-mode-setup)
            (macher-agent-setup-gptel-buffer)
            (macher-agent--wrap-macher-tools))
        (user-error
         (setq macher-agent-mode nil)
         (user-error "%s" (error-message-string err))))
    (macher-agent--teardown-gptel-buffer)))

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
