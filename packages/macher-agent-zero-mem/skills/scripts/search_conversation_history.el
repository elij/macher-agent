;;; search_conversation_history.el --- Search conversation history tool -*- lexical-binding: t; -*-

(setq macher-agent-search-conversation-history-tool
      (gptel-make-tool
       :name "search_conversation_history"
       :description "Search the truncated earlier conversation history. Use this when the SYSTEM ALERT indicates context was removed. Returns matching lines and surrounding context."
       :category "perception"
       :args '((:name "query" :type "string" :description "The keywords with space delimiters to search for in the history.")
               (:name "context_lines" :type "number" :optional t :description "Number of lines to return before and after the match (default 5)."))
       :include nil
       :async t
       :function (macher-agent-with-presentation-context (query &optional context-lines)
                   (let ((orig-buf
                          (or (when (and (macher-agent-get-active-fsm)
                                         (buffer-live-p (current-buffer)))
                                (current-buffer))
                              (when (macher-agent-valid-context-p context)
                                (let ((ob (macher-agent-context-owner-buffer context)))
                                  (when (and (bufferp ob) (buffer-live-p ob))
                                    ob)))
                              (current-buffer))))
                     (if (not (buffer-live-p orig-buf))
                         "Error: Original buffer not found."
                       (with-current-buffer orig-buf
                         (macher-agent-search-dispatch (or query "") orig-buf context-lines)))))))
