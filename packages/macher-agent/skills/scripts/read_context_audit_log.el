;;; read_context_audit_log.el --- Read context audit log tool -*- lexical-binding: t; -*-

(require 'json)

(setq macher-agent-read-context-audit-log-tool
      (gptel-make-tool
       :name "read_context_audit_log"
       :description "Read the ephemeral tool-intent log from the current task context to evaluate past subagent behaviour. Analyses parameters to determine why previous tasks failed."
       :category "perception"
       :args '((:name "preset" :type "string" :description "Filter by specific preset name (for example, 'PandasDataFilter')." :optional t)
               (:name "limit" :type "integer" :description "Max records to return. Defaults to 5." :optional t))
       :async t
       :function (macher-agent-with-presentation-context (&optional preset limit)
                   (let* ((preset-str (when (and (stringp preset) (not (string-empty-p preset)))
                                        preset))
                          (parsed-limit (if (and (numberp limit) (> limit 0))
                                            (round limit)
                                          5))
                          (raw-log (when (macher-agent-valid-context-p context)
                                     (plist-get (macher-agent-context-plugins context) :audit-log)))
                          (filtered-log
                           (if preset-str
                               (cl-remove-if-not
                                (lambda (entry)
                                  (when-let* ((val (cdr (or (assoc 'preset entry)
                                                            (assoc :preset entry)
                                                            (assoc "preset" entry)))))
                                    (string-equal (replace-regexp-in-string "^:" "" (format "%s" val))
                                                  preset-str)))
                                raw-log)
                             raw-log))
                          (recent-entries (last filtered-log parsed-limit)))
                     (format "SUCCESS: Audit log retrieved.\n%s"
                             (json-encode (vconcat recent-entries)))))))
