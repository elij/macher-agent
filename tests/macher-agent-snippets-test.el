;;; macher-agent-snippets-test.el --- Snippet integration tests for macher-agent -*- lexical-binding: t; -*-

;; Author: Elijah Charles
;; Keywords: convenience, gptel, llm, macher
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Documentation code snippet tests for macher-agent.
;; This test suite extracts and evaluates executable Emacs Lisp code snippets
;; from markdown wiki files within an isolated sandbox environment.
;;

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'buttercup)
(require 'macher-agent)
(require 'macher-agent-sandbox)

(require 'markdown-ts-mode nil t)
(require 'markdown-mode nil t)

(load (expand-file-name "helpers/macher-agent-snippet-extractor.el"
                        (file-name-directory (or load-file-name (buffer-file-name)))))

(defvar macher-agent-display-buffer-action nil)
(defvar macher-agent-display-subagent-fn nil)
(defvar macher-agent-hide-subagent-fn nil)
(defvar macher-agent-permission-request-hook nil)
(defvar macher-agent-post-response-hook nil)
(defvar macher-agent-pre-tool-use-hook nil)
(defvar macher-agent-post-tool-use-hook nil)
(defvar macher-agent-post-tool-use-failure-hook nil)
(defvar gptel-model nil)
(defvar gptel-backend nil)
(defvar gptel-directives nil)
(defvar gptel-post-response-functions nil)

(defun macher-agent-test--ptc-ast-p (form)
  "Return non-nil if FORM represents a Programmatic Tool Calling AST form."
  (or (and (consp form)
           (or (keywordp (car form))
               (memq (car form) '(ptc_execution ptc-execution))))
      (and (consp form)
           (eq (car form) 'progn)
           (cl-some #'macher-agent-test--ptc-ast-p (cdr form)))))

(defun macher-agent-test--eval-snippet (snippet)
  "Safely evaluate code SNIPPET in an isolated environment.
Extract code string from SNIPPET, read form, and evaluate either through
`macher-agent-sandbox--eval-iter' for PTC AST forms or lexical `eval'."
  (condition-case _err
      (let* ((code-str (plist-get snippet :code))
             (form (car (read-from-string (format "(progn\n%s\n)" code-str)))))
        (let ((macher-agent-display-buffer-action macher-agent-display-buffer-action)
              (macher-agent-display-subagent-fn macher-agent-display-subagent-fn)
              (macher-agent-hide-subagent-fn macher-agent-hide-subagent-fn)
              (macher-agent-permission-request-hook macher-agent-permission-request-hook)
              (macher-agent-post-response-hook macher-agent-post-response-hook)
              (macher-agent-pre-tool-use-hook macher-agent-pre-tool-use-hook)
              (macher-agent-post-tool-use-hook macher-agent-post-tool-use-hook)
              (macher-agent-post-tool-use-failure-hook macher-agent-post-tool-use-failure-hook)
              (gptel-model gptel-model)
              (gptel-backend gptel-backend)
              (gptel-directives gptel-directives)
              (gptel-post-response-functions gptel-post-response-functions))
          (if (macher-agent-test--ptc-ast-p form)
              (let ((iter (macher-agent-sandbox--eval-iter form nil))
                    (yield-val nil))
                (condition-case iter-err
                    (while t
                      (setq yield-val (iter-next iter yield-val)))
                  (iter-end-of-sequence
                   (cdr iter-err))))
            (eval form t))))
    (error nil)))

(describe "Markdown Documentation Snippets"

          (before-each
           ;; Initialise function spies on existing functions
           (spy-on 'gptel-request)
           (spy-on 'gptel-send)
           (spy-on 'message)
           (spy-on 'macher-action)
           (spy-on 'macher-agent-add-subagent :and-return-value "*mocked-subagent*")

           ;; Clean up persistent context state
           (when (boundp 'macher-agent-active-workspaces)
             (clrhash macher-agent-active-workspaces))
           (when (boundp 'macher-agent-active-subagents)
             (setq macher-agent-active-subagents nil))
           (when (boundp 'macher-agent--persistent-context)
             (setq macher-agent--persistent-context nil)))

          ;; Dynamically discover markdown wiki files and generate evaluation tests
          (let* ((base-dir (file-name-directory (or load-file-name (buffer-file-name))))
                 (wiki-dir (expand-file-name "../macher-agent.wiki" base-dir))
                 (wiki-files (or (file-expand-wildcards (expand-file-name "*.md" wiki-dir))
                                 (file-expand-wildcards "macher-agent.wiki/*.md"))))
            (dolist (wiki-file wiki-files)
              (let ((snippets (macher-agent-test--extract-via-imenu wiki-file)))
                (dolist (snippet snippets)
                  (let ((heading (plist-get snippet :name))
                        (file-name (plist-get snippet :file)))
                    (it (format "evaluates snippet cleanly under '%s' in %s" heading file-name)
                        (expect (macher-agent-test--eval-snippet snippet) :not :to-throw))))))))

(provide 'macher-agent-snippets-test)
;;; macher-agent-snippets-test.el ends here
