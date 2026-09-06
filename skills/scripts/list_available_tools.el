;;; list_available_tools.el --- List available tools -*- lexical-binding: t; -*-

(setq macher-agent-list-available-tools-tool
      (gptel-make-tool
       :name "list_available_tools"
       :description "List all registered gptel-tools in the current workspace. Use this to discover tools to assign to a preset."
       :category "perception"
       :args nil
       :async t
       :function (macher-agent-with-presentation-context ()
                   (let* ((native-fn (get 'macher-agent-list-available-tools-tool 'ptc-function))
                          (res (funcall native-fn context)))
                     (format "SUCCESS:\n%s" res)))))

(put 'macher-agent-list-available-tools-tool 'ptc-function
     (lambda (&optional context)
       (let* ((registry (or (when (macher-agent-valid-context-p context)
                              (macher-agent-context-tools context))
                            (bound-and-true-p macher-agent-tools-registry)))
              (tools-list nil))
         (when (hash-table-p registry)
           (maphash
            (lambda (name tool)
              (let ((name-str (if (stringp name) name (symbol-name name)))
                    (desc (if (and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
                              (gptel-tool-description tool)
                            "")))
                (push (format "- %s: %s" name-str desc) tools-list)))
            registry))
         (if tools-list
             (string-join (nreverse tools-list) "\n")
           "No tools registered."))))
