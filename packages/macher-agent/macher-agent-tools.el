;;; macher-agent-tools.el --- Pure gptel orchestration tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure gptel orchestration tools for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'macher-agent-core)
(require 'macher-agent-gptel)

;;; Harness Issue Logging

(defcustom macher-agent-harness-issue-log-file
  (expand-file-name "macher-agent-harness-issues.md" user-emacs-directory)
  "Path to the persistent Markdown log file for `log_harness_issue'."
  :type 'file
  :group 'macher-agent)

(defun macher-agent-log-harness-issue (summary details &optional category severity context)
  "Log a harness issue with SUMMARY and DETAILS to the harness issue log.
Optional CATEGORY defaults to \"other\", SEVERITY defaults to \"medium\",
and CONTEXT defaults to the active buffer context.  Appends a timestamped
Markdown entry to `macher-agent-harness-issue-log-file', records a plist
entry in CONTEXT `:plugins' under `:harness-issues' when available, and
returns a concise confirmation string."
  (unless (and (stringp summary) (not (string-empty-p (string-trim summary))))
    (error "Harness issue `summary' must be a non-empty string"))
  (unless (and (stringp details) (not (string-empty-p (string-trim details))))
    (error "Harness issue `details' must be a non-empty string"))
  (unless (and (stringp macher-agent-harness-issue-log-file)
               (not (string-empty-p (string-trim macher-agent-harness-issue-log-file))))
    (error "`macher-agent-harness-issue-log-file' must be a non-empty file path string"))
  (let* ((sanitize-heading
          (lambda (str)
            (string-trim (replace-regexp-in-string "[\r\n]+" " " str))))
         (clean-summary (funcall sanitize-heading summary))
         (clean-details (string-trim details))
         (clean-category (if (and (stringp category)
                                  (not (string-empty-p (string-trim category))))
                             (funcall sanitize-heading category)
                           "other"))
         (clean-severity (if (and (stringp severity)
                                  (not (string-empty-p (string-trim severity))))
                             (funcall sanitize-heading severity)
                           "medium"))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
         (buf-name (buffer-name (current-buffer)))
         (ctx (or (and (macher-agent-valid-context-p context) context)
                  (macher-agent-resolve-context (current-buffer))))
         (ws-root (when (macher-agent-valid-context-p ctx)
                    (macher-agent-context-project-root ctx)))
         (log-file (expand-file-name (string-trim macher-agent-harness-issue-log-file)))
         (log-dir (file-name-directory log-file))
         (entry-text
          (concat
           (format "## [%s] [%s] [%s] %s\n\n"
                   timestamp clean-severity clean-category clean-summary)
           (format "- **Buffer**: `%s`\n" buf-name)
           (if (and (stringp ws-root) (not (string-empty-p ws-root)))
               (format "- **Workspace**: `%s`\n" ws-root)
             "")
           "\n"
           clean-details
           "\n\n---\n\n")))
    (when log-dir
      (make-directory log-dir t))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region entry-text nil log-file 'append 'silent))
    (when (macher-agent-valid-context-p ctx)
      (let* ((plugins (macher-agent-context-plugins ctx))
             (existing (when (macher-agent-plist-p plugins)
                         (plist-get plugins :harness-issues)))
             (record (list :timestamp timestamp
                           :severity clean-severity
                           :category clean-category
                           :buffer buf-name
                           :workspace ws-root
                           :summary clean-summary
                           :details clean-details
                           :file log-file)))
        (setf (macher-agent-context-plugins ctx)
              (plist-put (copy-sequence plugins)
                         :harness-issues
                         (append existing (list record))))))
    (format "Logged harness issue (%s/%s) to %s."
            clean-severity clean-category log-file)))

;;; Presentation Macro

(defmacro macher-agent-with-presentation-context (args &rest body)
  "Wrap BODY taking ARGS to bind `context' and route errors via callback.
Extracts `context' from active FSM or persistent buffer context."
  (declare (indent 1))
  (let ((fsm-sym (gensym "fsm-")))
    `(lambda (callback ,@args)
       (let* ((,fsm-sym (macher-agent-get-active-fsm))
              (context (or (when ,fsm-sym
                             (macher-agent-gptel-context-from-fsm ,fsm-sym))
                           (bound-and-true-p macher-agent-persistent-context)))
              (target-buf (when ,fsm-sym
                            (or (ignore-errors (plist-get (gptel-fsm-info ,fsm-sym) :buffer))
                                (when (fboundp 'gptel-fsm-buffer)
                                  (ignore-errors (gptel-fsm-buffer ,fsm-sym)))))))
         (ignore context)
         (condition-case err
             (if (and target-buf (buffer-live-p target-buf))
                 (with-current-buffer target-buf
                   (let ((res (progn ,@body)))
                     (when (and res (functionp callback))
                       (funcall callback res))
                     res))
               (let ((res (progn ,@body)))
                 (when (and res (functionp callback))
                   (funcall callback res))
                 res))
           (error
            (let ((err-msg (format "ERROR: %s" (error-message-string err))))
              (if (and target-buf (buffer-live-p target-buf))
                  (with-current-buffer target-buf
                    (when (functionp callback)
                      (funcall callback err-msg)))
                (when (functionp callback)
                  (funcall callback err-msg)))
              err-msg)))))))

(provide 'macher-agent-tools)
;;; macher-agent-tools.el ends here
