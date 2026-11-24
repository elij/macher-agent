;;; test-harness-issue.el --- Tests for log_harness_issue metatool -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Tests for the user-enabled `log_harness_issue' metatool and
;; `macher-agent-log-harness-issue' persistence and context recording.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'buttercup)
(require 'macher-agent-test-helpers)

(describe "Harness issue logging metatool"

  (before-each
    (macher-agent-test-reset-env))

  (it "HRI-01: resolves log_harness_issue as a pure gptel-tool without ptc-function and does not auto-inject it"
    (macher-agent-test-with-git-workspace (_root ctx owner-buf)
      (with-current-buffer owner-buf
        (let ((tool (macher-agent-resolve-tool "log_harness_issue" nil nil ctx)))
          (expect (gptel-tool-p tool) :to-be t)
          (expect (gptel-tool-name tool) :to-equal "log_harness_issue")
          (expect (gptel-tool-category tool) :to-equal "meta")
          (expect (gptel-tool-async tool) :to-be t)
          (expect (get 'macher-agent-log-harness-issue-tool 'ptc-function) :to-be nil)
          (expect (get 'log_harness_issue 'ptc-function) :to-be nil))
        ;; Verify log_harness_issue is NOT auto-injected by the default transmission pipeline
        (let* ((tx (macher-agent-run-pipeline
                    'transmission
                    (make-macher-agent-transmission-state
                     :target-buffer owner-buf
                     :context ctx
                     :tools nil)))
               (tool-names (mapcar #'macher-agent-canonical-tool-name
                                   (macher-agent-transmission-state-tools tx))))
          (expect (member "log_harness_issue" tool-names) :to-be nil)))))

  (it "HRI-02: appends timestamped Markdown entries, creates parent dirs, and records context plugin state"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (let* ((tmp-dir (make-temp-file "macher-hri-" t))
             (log-path (expand-file-name "nested/subdir/issues.md" tmp-dir))
             (macher-agent-harness-issue-log-file log-path))
        (unwind-protect
            (with-current-buffer owner-buf
              (let* ((tool (macher-agent-resolve-tool "log_harness_issue" nil nil ctx))
                     (res1 (macher-agent-test-invoke-tool
                            tool
                            "Wire prefix dropped on retry"
                            "Observed missing prefix after retry.\nExpected identical prefix."
                            "wire-bug"
                            "high"))
                     (res2 (macher-agent-test-invoke-tool
                            tool
                            "Minor prompt spacing glitch"
                            "Extra blank line before directive.")))
                (expect res1 :to-match (regexp-quote log-path))
                (expect res1 :to-match "high/wire-bug")
                (expect res2 :to-match "medium/other")
                (expect (file-exists-p log-path) :to-be t)
                (let ((content (with-temp-buffer
                                 (insert-file-contents log-path)
                                 (buffer-string))))
                  (expect content :to-match "^## \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z\\] \\[high\\] \\[wire-bug\\] Wire prefix dropped on retry$")
                  (expect content :to-match (regexp-quote (buffer-name owner-buf)))
                  (expect content :to-match (regexp-quote root))
                  (expect content :to-match "Observed missing prefix after retry\\.")
                  (expect content :to-match "\\[medium\\] \\[other\\] Minor prompt spacing glitch")
                  (expect content :to-match "Extra blank line before directive\\."))
                ;; Context :plugins records both entries chronologically under :harness-issues
                (let ((issues (plist-get (macher-agent-context-plugins ctx) :harness-issues)))
                  (expect (length issues) :to-equal 2)
                  (expect (plist-get (nth 0 issues) :summary) :to-equal "Wire prefix dropped on retry")
                  (expect (plist-get (nth 0 issues) :category) :to-equal "wire-bug")
                  (expect (plist-get (nth 0 issues) :severity) :to-equal "high")
                  (expect (plist-get (nth 0 issues) :file) :to-equal log-path)
                  (expect (plist-get (nth 1 issues) :category) :to-equal "other")
                  (expect (plist-get (nth 1 issues) :severity) :to-equal "medium"))))
          (ignore-errors (delete-directory tmp-dir t))))))

  (it "HRI-03: rejects missing or blank summary and details without creating log file"
    (macher-agent-test-with-git-workspace (_root ctx owner-buf)
      (let* ((tmp-dir (make-temp-file "macher-hri-err-" t))
             (log-path (expand-file-name "should-not-exist.md" tmp-dir))
             (macher-agent-harness-issue-log-file log-path))
        (unwind-protect
            (with-current-buffer owner-buf
              (expect (macher-agent-log-harness-issue "" "Some details" nil nil ctx)
                      :to-throw 'error)
              (expect (macher-agent-log-harness-issue "Valid summary" "   \n  " nil nil ctx)
                      :to-throw 'error)
              (expect (macher-agent-log-harness-issue nil "Some details" nil nil ctx)
                      :to-throw 'error)
              (let* ((tool (macher-agent-resolve-tool "log_harness_issue" nil nil ctx))
                     (err1 (macher-agent-test-invoke-tool tool "   " "Details"))
                     (err2 (macher-agent-test-invoke-tool tool "Summary" "")))
                (expect err1 :to-match "^ERROR:")
                (expect err2 :to-match "^ERROR:")
                (expect (file-exists-p log-path) :to-be nil)
                (expect (plist-get (macher-agent-context-plugins ctx) :harness-issues) :to-be nil)))
          (ignore-errors (delete-directory tmp-dir t))))))

  (it "HRI-04: sanitises newlines in heading fields, persists UTF-8 cleanly, and guards invalid log-file"
    (macher-agent-test-with-git-workspace (_root ctx owner-buf)
      (let* ((tmp-dir (make-temp-file "macher-hri-utf8-" t))
             (log-path (expand-file-name "utf8-issues.md" tmp-dir))
             (macher-agent-harness-issue-log-file log-path))
        (unwind-protect
            (with-current-buffer owner-buf
              ;; Invalid custom log-file values must raise clean errors
              (let ((macher-agent-harness-issue-log-file nil))
                (expect (macher-agent-log-harness-issue "Summary" "Details" nil nil ctx)
                        :to-throw 'error))
              (let ((macher-agent-harness-issue-log-file "   \n  "))
                (expect (macher-agent-log-harness-issue "Summary" "Details" nil nil ctx)
                        :to-throw 'error))
              ;; Restrictive buffer coding system must not hinder UTF-8 persistence
              (setq-local buffer-file-coding-system 'us-ascii)
              (let ((res (macher-agent-log-harness-issue
                          "Multi\r\nline\nsummary — café"
                          "UTF-8 details: 日本語 — résumé"
                          "wire\nbug"
                          "high\r\nprio"
                          ctx)))
                (expect res :to-match "high prio/wire bug")
                (let ((content (with-temp-buffer
                                 (let ((coding-system-for-read 'utf-8))
                                   (insert-file-contents log-path))
                                 (buffer-string))))
                  (expect content :to-match
                          "^## \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z\\] \\[high prio\\] \\[wire bug\\] Multi line summary — café$")
                  (expect content :to-match "UTF-8 details: 日本語 — résumé"))
                (let* ((issues (plist-get (macher-agent-context-plugins ctx) :harness-issues))
                       (rec (car issues)))
                  (expect (plist-get rec :summary) :to-equal "Multi line summary — café")
                  (expect (plist-get rec :category) :to-equal "wire bug")
                  (expect (plist-get rec :severity) :to-equal "high prio"))))
          (ignore-errors (delete-directory tmp-dir t)))))))

(provide 'test-harness-issue)
;;; test-harness-issue.el ends here
