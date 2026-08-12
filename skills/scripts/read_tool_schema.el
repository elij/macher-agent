;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
    macher-agent-read-tool-schema-tool
    "Get the exact parameter schema for a specific tool. Use this to understand what a tool does before adding it to a preset."
  :category "perception"
  :args '((:name "tool_name" :type string :description "The exact string name of the tool to inspect."))
  :command-fn
  (lambda (payload _context _root)
    (let* ((tool-name (plist-get payload :tool_name))
           (tool (gethash tool-name macher-agent-tools-registry)))
      (if tool
          (json-encode (gptel-tool-args tool))
        (format "ERROR: Tool '%s' not found." tool-name))))
  :success-fn (lambda (res _payload) (format "SCHEMA: %s" res)))
