;;; search_parent_conversation_history.el --- Search parent conversation history tool -*- lexical-binding: t; -*-

(setq macher-agent-search-parent-conversation-history-tool
      (gptel-make-tool
       :name "search_parent_conversation_history"
       :description "Search the parent (originating orchestrator) conversation buffer for a regular expression or keyword."
       :category "perception"
       :args '((:name "query" :type "string" :description "The regular expression or keyword to search for in the parent buffer.")
               (:name "context_lines" :type "number" :description "Number of surrounding context lines to include (default 5)." :optional t))
       :include nil
       :async nil
       :function (lambda (query &optional context-lines)
                   (let ((parent-buf (macher-agent-zero-mem-resolve-parent-buffer)))
                     (if (not (and parent-buf (buffer-live-p parent-buf)))
                         "ERROR: Parent conversation buffer not available."
                       (macher-agent-search-dispatch (or query "") parent-buf (or context-lines 5)))))))
