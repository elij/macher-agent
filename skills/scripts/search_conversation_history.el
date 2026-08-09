(macher-agent-make-tool
    macher-agent-search-conversation-history-tool
    "Search the truncated earlier conversation history. Use this when the SYSTEM ALERT indicates context was removed. Returns matching lines and surrounding context."
  :category "execution"
  :args '((:name "query" :type string :description "The exact text or regular expression to search for in the history.")
          (:name "context_lines" :type number :optional t :description "Number of lines to return before and after the match (default 5)."))
  :command-fn (lambda (payload context _root)
                (let* ((query (plist-get payload :query))
                       (context-lines (plist-get payload :context_lines))
                       (fsm (or (and context
                                     (cond
                                      ((and (fboundp 'gptel-fsm-p) (gptel-fsm-p context)) context)
                                      ((and (fboundp 'macher-agent--get-context-data)
                                            (macher-agent--get-context-data context :fsm)))
                                      ((and (listp context) (plist-get context :buffer)) context)))
                                (bound-and-true-p macher-agent--active-fsm)
                                (when (fboundp 'macher-agent--get-fsm-latest)
                                  (macher-agent--get-fsm-latest))))
                       (info (when fsm
                               (if (fboundp 'macher-agent--extract-fsm-info)
                                   (macher-agent--extract-fsm-info fsm)
                                 (when (fboundp 'gptel-fsm-info)
                                   (ignore-errors (gptel-fsm-info fsm))))))
                       (orig-buf (or (when info (plist-get info :buffer))
                                     (when (and context (fboundp 'macher-agent--get-context-data))
                                       (macher-agent--get-context-data context :buffer))
                                     (when (and (listp context) (bufferp (plist-get context :buffer)))
                                       (plist-get context :buffer)))))
                  (macher-agent-search-dispatch query orig-buf context-lines))))
