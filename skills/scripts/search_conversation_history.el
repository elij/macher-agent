;;; search_conversation_history.el --- Search conversation history tool -*- lexical-binding: t; -*-

(setq macher-agent-search-conversation-history-tool
      (gptel-make-tool
       :name "search_conversation_history"
       :description "Search the truncated earlier conversation history. Use this when the SYSTEM ALERT indicates context was removed. Returns matching lines and surrounding context."
       :category "execution"
       :args '((:name "query" :type "string" :description "The keywords with space delimiters to search for in the history.")
               (:name "context_lines" :type "number" :optional t :description "Number of lines to return before and after the match (default 5)."))
       :include nil
       :async t
       :function (macher-agent-with-presentation-context (query context-lines)
                   (let ((native-fn (get 'macher-agent-search-conversation-history-tool 'ptc-function)))
                     (funcall native-fn query context-lines context)))))

(put 'macher-agent-search-conversation-history-tool 'ptc-function
     (lambda (query &optional context-lines context)
       (let ((orig-buf (or (when (macher-agent-valid-context-p context)
                             (macher-agent-context-origin-buffer context))
                           (current-buffer))))
         (if (not (buffer-live-p orig-buf))
             "Error: Original buffer not found."
           (macher-agent-search-dispatch (or query "") orig-buf context-lines)))))
