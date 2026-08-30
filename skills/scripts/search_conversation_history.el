;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
    macher-agent-search-conversation-history-tool
    "Search the truncated earlier conversation history. Use this when the SYSTEM ALERT indicates context was removed. Returns matching lines and surrounding context."
  :category "execution"
  :args '((:name "query" :type string :description "The keywords with space delimiters to search for in the history.")
          (:name "context_lines" :type number :optional t :description "Number of lines to return before and after the match (default 5)."))
  :command-fn
  (lambda (payload context _root)
    (let* ((query (plist-get payload :query))
           (context-lines (plist-get payload :context_lines))
           (fsm (or (and context
                         (cond
                          ((and (fboundp 'gptel-fsm-p) (gptel-fsm-p context)) context)
                          ((and (fboundp 'macher-agent-context-p)
                                (macher-agent-context-p context))
                           (let ((plugins (macher-agent-context-plugins context)))
                             (when (macher-agent--plist-p plugins)
                               (plist-get plugins :fsm))))
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
                         (when (and context (fboundp 'macher-agent-context-p) (macher-agent-context-p context))
                           (or (macher-agent-context-origin-buffer context)
                               (let ((plugins (macher-agent-context-plugins context)))
                                 (when (macher-agent--plist-p plugins)
                                   (plist-get plugins :buffer)))))
                         (when (and (fboundp 'macher-agent--plist-p)
                                    (macher-agent--plist-p context)
                                    (bufferp (plist-get context :buffer)))
                           (plist-get context :buffer)))))
      (if (not (buffer-live-p orig-buf))
          "Error: Original buffer not found."
        (macher-agent-search-dispatch query orig-buf context-lines)))))
