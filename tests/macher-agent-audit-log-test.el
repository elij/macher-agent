;;; macher-agent-audit-log-test.el --- Tests for macher-agent audit logging -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'json)
(require 'cl-lib)
(require 'macher-agent)
(let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "helpers" current-dir)))

(require 'macher-agent-test-harness)

(load (expand-file-name "skills/scripts/read_context_audit_log.el") nil t)

(describe
 "Macher-Agent Audit Logging"
 
 (describe
  "macher-agent-log-tool-intent"
  (it "appends entries with timestamp buffer preset type target args into context data audit log"
      (let ((ctx (macher--make-context))
            (macher-agent--active-skill-sym 'alpha-preset))
        (macher-agent-log-tool-intent ctx "gptel-tool" "tool-one" '(:key "value-one"))
        (let* ((log (macher-agent--get-context-data ctx :audit-log))
               (entry (car log)))
          (expect (length log) :to-equal 1)
          (expect (cdr (assoc 'buffer entry)) :to-equal (buffer-name))
          (expect (cdr (assoc 'preset entry)) :to-equal "alpha-preset")
          (expect (cdr (assoc 'type entry)) :to-equal "gptel-tool")
          (expect (cdr (assoc 'target entry)) :to-equal "tool-one")
          (expect (cdr (assoc 'args entry)) :to-equal '(:key "value-one"))
          (expect (stringp (cdr (assoc 'timestamp entry))) :to-be t))
        (macher-agent-log-tool-intent ctx "gptel-tool" "tool-two" '(:key "value-two"))
        (expect (length (macher-agent--get-context-data ctx :audit-log)) :to-equal 2))))

 (describe "macher-agent--log-gptel-pre-tool"
           (it "logs standard gptel tool calls into active context audit log"
               (let ((ctx (macher--make-context)))
                 (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                 (macher-agent--log-gptel-pre-tool 'test-gptel-tool nil :param "val")
                 (let* ((log (macher-agent--get-context-data ctx :audit-log))
                        (entry (car log)))
                   (expect (length log) :to-equal 1)
                   (expect (cdr (assoc 'type entry)) :to-equal "gptel-tool")
                   (expect (cdr (assoc 'target entry)) :to-equal "test-gptel-tool")
                   (expect (cdr (assoc 'args entry)) :to-equal '(:param "val"))))))

 (describe "macher-agent-sandbox-run"
           (it "logs PTC tool yields into active context audit log"
               (let ((ctx (macher--make-context))
                     (macher-agent--active-ptc-primitives '(test-ptc-tool)))
                 (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                 (macher-agent-sandbox-run '(test-ptc-tool :path "file.txt") nil)
                 (let* ((log (macher-agent--get-context-data ctx :audit-log))
                        (entry (car log)))
                   (expect (length log) :to-equal 1)
                   (expect (cdr (assoc 'type entry)) :to-equal "ptc")
                   (expect (cdr (assoc 'target entry)) :to-equal 'test-ptc-tool)
                   (expect (cdr (assoc 'args entry)) :to-equal '(:path "file.txt"))))))

 (describe "macher-agent-read-context-audit-log-tool"
           (it "filters audit log by preset and limit returning JSON string of entries"
               (let ((ctx (macher--make-context))
                     (callback-result nil))
                 (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                 (let ((macher-agent--active-skill-sym 'PresetAlpha))
                   (macher-agent-log-tool-intent ctx "gptel-tool" "tool-1" '(:a 1)))
                 (let ((macher-agent--active-skill-sym 'PresetBeta))
                   (macher-agent-log-tool-intent ctx "gptel-tool" "tool-2" '(:b 2)))
                 (let ((macher-agent--active-skill-sym 'PresetAlpha))
                   (macher-agent-log-tool-intent ctx "gptel-tool" "tool-3" '(:c 3)))
                 (let ((macher-agent--active-skill-sym 'PresetAlpha))
                   (macher-agent-log-tool-intent ctx "gptel-tool" "tool-4" '(:d 4)))
                 (with-macher-agent-mock-fsm
                  ctx
                  (funcall (gptel-tool-function macher-agent-read-context-audit-log-tool)
                           (lambda (res) (setq callback-result res))
                           :preset "PresetAlpha" :limit 2))
                 (expect callback-result :to-match "SUCCESS: Audit log retrieved.")
                 (when (string-match "\\[.*\\]" callback-result)
                   (let* ((json-str (match-string 0 callback-result))
                          (parsed (json-read-from-string json-str)))
                     (expect (length parsed) :to-equal 2)
                     (let ((first-entry (elt parsed 0))
                           (second-entry (elt parsed 1)))
                       (expect (cdr (assoc 'target first-entry)) :to-equal "tool-3")
                       (expect (cdr (assoc 'target second-entry)) :to-equal "tool-4")
                       (expect (cdr (assoc 'preset first-entry)) :to-equal "PresetAlpha"))))))))

(provide 'macher-agent-audit-log-test)
;;; macher-agent-audit-log-test.el ends here
