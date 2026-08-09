(require 'subr-x)

(macher-agent-make-tool
    macher-agent-list-available-tools-tool
    "List all registered gptel-tools in the current workspace. Use this to discover tools to assign to a preset."
  :category "perception"
  :args nil
  :command-fn
  (lambda (&optional _payload context _root)
    (let ((tools-list nil)
          (seen (make-hash-table :test 'equal)))
      (dolist (table (list (when (and (boundp 'macher-agent-tools-registry)
                                      (hash-table-p macher-agent-tools-registry))
                             macher-agent-tools-registry)
                           (when (fboundp 'macher-agent-workspace-tools-registry)
                             (let ((ws-reg (condition-case nil
                                               (macher-agent-workspace-tools-registry context)
                                             (error nil))))
                               (when (hash-table-p ws-reg)
                                 ws-reg)))))
        (when (hash-table-p table)
          (maphash
           (lambda (name tool)
             (let ((name-str (if (stringp name) name (format "%s" name))))
               (when (and tool (not (gethash name-str seen)))
                 (puthash name-str t seen)
                 (let ((cat (condition-case nil
                                (when (fboundp 'gptel-tool-category)
                                  (gptel-tool-category tool))
                              (error nil)))
                       (desc (or (condition-case nil
                                     (when (fboundp 'gptel-tool-description)
                                       (gptel-tool-description tool))
                                   (error nil))
                                 "")))
                   (when (and cat (member (if (stringp cat) cat (format "%s" cat))
                                          '("perception" "collaboration" "event" "execution")))
                     (push (format "- %s: %s" name-str desc) tools-list))))))
           table)))
      (if tools-list
          (string-join (nreverse tools-list) "\n")
        "No tools registered.")))
  :success-fn (lambda (res _payload) (format "SUCCESS:\n%s" res)))
