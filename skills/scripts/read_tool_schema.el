;;; read_tool_schema.el --- Read tool schema tool -*- lexical-binding: t; -*-

(setq macher-agent-read-tool-schema-tool
      (gptel-make-tool
       :name "read_tool_schema"
       :description "Get the exact parameter schema for a specific tool. Use this to understand what a tool does before adding it to a preset."
       :category "perception"
       :args '((:name "tool_name" :type "string" :description "The exact string name of the tool to inspect."))
       :async t
       :function (macher-agent-with-presentation-context (tool-name)
                   (let ((native-fn (get 'macher-agent-read-tool-schema-tool 'ptc-function)))
                     (format "SCHEMA: %s" (funcall native-fn tool-name context))))))

(put 'macher-agent-read-tool-schema-tool 'ptc-function
     (lambda (tool-name &optional context)
       (let* ((name-str (if (stringp tool-name) tool-name (symbol-name tool-name)))
              (tool (or 
                     (when (fboundp 'gptel-get-tool)
                       (ignore-errors (gptel-get-tool name-str)))
                     (when (bound-and-true-p gptel--known-tools)
                       (cl-loop for cat in gptel--known-tools
                                thereis (alist-get name-str (cdr cat) nil nil #'equal)))
                     (let ((registry (or (when (macher-agent-valid-context-p context)
                                           (macher-agent-context-tools context))
                                         (bound-and-true-p macher-agent-tools-registry))))
                       (when (hash-table-p registry)
                         (gethash name-str registry))))))
         (if tool
             (json-encode (gptel-tool-args tool))
           (format "ERROR: Tool '%s' not found." name-str)))))
