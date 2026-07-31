(macher-agent-make-tool
    macher-agent-list-available-tools-tool
    "List all registered gptel-tools in the current workspace. Use this to discover tools to assign to a preset."
  :category "perception"
  :args nil
  :command-fn
  (lambda (_payload _context _root)
    (let ((tools-list nil))
      (maphash (lambda (name tool)
                 (push (format "- %s: %s" name (gptel-tool-description tool)) tools-list))
               (if (bound-and-true-p macher-agent-tools-registry)
                   macher-agent-tools-registry
                 (make-hash-table)))
      (if tools-list
          (string-join (nreverse tools-list) "\n")
        "No tools registered.")))
  :success-fn (lambda (res _payload) (format "SUCCESS:\n%s" res)))
