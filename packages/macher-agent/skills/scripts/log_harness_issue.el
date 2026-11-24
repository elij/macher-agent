;;; log_harness_issue.el --- Log harness issue metatool -*- lexical-binding: t; -*-

(setq macher-agent-log-harness-issue-tool
      (gptel-make-tool
       :name "log_harness_issue"
       :description "Log a harness bug, tool failure, or unexpected agent-runtime issue to the persistent harness issue log for later review."
       :category "meta"
       :include nil
       :args '((:name "summary" :type "string"
                      :description "Short one-line summary of the harness issue.")
               (:name "details" :type "string"
                      :description "Detailed reproduction steps, observed vs expected behaviour, or error messages.")
               (:name "category" :type "string"
                      :description "Optional issue category (e.g. tool-error, prompt-bug, wire-bug, vfs-bug, ux, other). Defaults to \"other\"."
                      :optional t)
               (:name "severity" :type "string"
                      :description "Optional severity level (low, medium, high, critical). Defaults to \"medium\"."
                      :optional t))
       :async t
       :function (macher-agent-with-presentation-context (summary details &optional category severity)
                   (macher-agent-log-harness-issue summary details category severity context))))
