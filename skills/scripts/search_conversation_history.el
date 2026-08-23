;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
    macher-agent-search-conversation-history-tool
    "Search the truncated earlier conversation history. Use this when the SYSTEM ALERT indicates context was removed. Returns matching lines and surrounding context."
  :category "execution"
  :args '((:name "query" :type string :description "The exact text or regular expression to search for in the history.")
          (:name "context_lines" :type number :optional t :description "Number of lines to return before and after the match (default 5)."))
  :command-fn
  (lambda (payload context _root)
    (let* ((query (plist-get payload :query))
           (context-lines (plist-get payload :context_lines))
           (fsm (or (and context
                         (cond
                          ((and (fboundp 'gptel-fsm-p) (gptel-fsm-p context)) context)
                          ((and (fboundp 'macher-agent--get-context-data)
                                (macher-agent--get-context-data context :fsm)))
                          ((and (fboundp 'macher-agent--plist-p)
                                (macher-agent--plist-p context)
                                (plist-get context :buffer)) context)))
                    (macher-agent-get-active-fsm)))
           (info (when fsm
                   (if (fboundp 'macher-agent--extract-fsm-info)
                       (macher-agent--extract-fsm-info fsm)
                     (when (fboundp 'gptel-fsm-info)
                       (ignore-errors (gptel-fsm-info fsm))))))
           (orig-buf (or (when (and info (fboundp 'macher-agent--plist-p) (macher-agent--plist-p info))
                           (plist-get info :buffer))
                         (when (and context (fboundp 'macher-agent--get-context-data))
                           (macher-agent--get-context-data context :buffer))
                         (when (and (fboundp 'macher-agent--plist-p)
                                    (macher-agent--plist-p context)
                                    (bufferp (plist-get context :buffer)))
                           (plist-get context :buffer)))))
      (if (not (buffer-live-p orig-buf))
          "Error: Original buffer not found."
        (let* ((max-chars (macher-agent--get-max-context-chars orig-buf))
               (buf-len (with-current-buffer orig-buf (buffer-size)))
               (event-horizon-pt (with-current-buffer orig-buf
                                   (max (point-min) (- (point-max) max-chars)))))
          (if (<= buf-len max-chars)
              "Notice: No conversation history has been truncated yet. All context is active."
            (with-current-buffer orig-buf
              (save-restriction
                (narrow-to-region (point-min) event-horizon-pt)
                (macher-agent-search-dispatch query (current-buffer) context-lines)))))))))
