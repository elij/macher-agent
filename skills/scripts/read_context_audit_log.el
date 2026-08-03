;;; read_context_audit_log.el --- Read context audit log -*- lexical-binding: t; -*-

(macher-agent-make-tool
    macher-agent-read-context-audit-log-tool
    "Read the ephemeral tool-intent log from the current task context to evaluate past subagent behaviour. Analyses parameters to determine why previous tasks failed."
  :category "perception"
  :args
  '((:name "preset"
           :type string
           :description "Filter by specific preset name (for example, 'PandasDataFilter')."
           :optional t)
    (:name "limit"
           :type integer
           :description "Max records to return. Defaults to 5."
           :optional t))
  :command-fn
  (lambda (payload context _root)
    (require 'json)
    (let* ((preset (plist-get payload :preset))
           (preset-str (when-let* ((p preset)
                                   ((not (eq p :json-false)))
                                   ((stringp p))
                                   ((not (string-empty-p p))))
                         p))
           (raw-limit (plist-get payload :limit))
           (limit (if (and (numberp raw-limit) (> raw-limit 0))
                      (round raw-limit)
                    5))
           (raw-log (macher-agent--get-context-data context :audit-log))
           (filtered-log
            (if preset-str
                (cl-remove-if-not
                 (lambda (entry)
                   (when-let* ((val (cdr (or (assoc 'preset entry)
                                             (assoc :preset entry)
                                             (assoc "preset" entry)))))
                     (let* ((str-val (format "%s" val))
                            (clean-val (if (string-prefix-p ":" str-val)
                                           (substring str-val 1)
                                         str-val)))
                       (string-equal clean-val preset-str))))
                 raw-log)
              raw-log))
           (recent-entries (last filtered-log limit)))
      (json-encode (vconcat recent-entries))))
  :success-fn
  (lambda (res _payload)
    (format "SUCCESS: Audit log retrieved.\n%s" res)))
