;;; search_parent_conversation_history.el --- Search parent conversation history tool -*- lexical-binding: t; -*-

(setq macher-agent-search-parent-conversation-history-tool
      (gptel-make-tool
       :name "search_parent_conversation_history"
       :description "Search the parent (originating orchestrator) conversation buffer for a regular expression or keyword."
       :category "perception"
       :args '((:name "query" :type "string" :description "The regular expression or keyword to search for in the parent buffer.")
               (:name "context_lines" :type "number" :description "Number of surrounding context lines to include (default 5)." :optional t))
       :async nil
       :function (lambda (query &optional context-lines)
                   (funcall (get 'macher-agent-search-parent-conversation-history-tool 'ptc-function)
                            query context-lines))))

(put 'macher-agent-search-parent-conversation-history-tool 'ptc-function
     (lambda (query &optional context-lines context)
       (let* ((fsm (macher-agent-get-active-fsm))
              (cur-buf (or (when fsm
                             (or (ignore-errors (plist-get (gptel-fsm-info fsm) :buffer))
                                 (when (fboundp 'gptel-fsm-buffer)
                                   (ignore-errors (gptel-fsm-buffer fsm)))))
                           (current-buffer)))
              (ctx (or (macher-agent-resolve-context context)
                       (macher-agent-resolve-context cur-buf)))
              (route-frame (with-current-buffer cur-buf
                             (car-safe (bound-and-true-p macher-agent--routing-stack))))
              (route-parent-name (plist-get route-frame :originator-name))
              (parent-buf
               (or (and route-parent-name (get-buffer route-parent-name))
                   (when (and ctx (fboundp 'macher-agent-context-parent-buffer))
                     (let ((pb (macher-agent-context-parent-buffer ctx)))
                       (if (stringp pb) (get-buffer pb) pb)))
                   (when ctx
                     (let ((ob (macher-agent-context-origin-buffer ctx)))
                       (when (and ob (not (eq ob cur-buf)))
                         (if (stringp ob) (get-buffer ob) ob)))))))
         (if (not (and parent-buf (buffer-live-p parent-buf)))
             "ERROR: Parent conversation buffer not available."
           (macher-agent-search-dispatch query parent-buf (or context-lines 5))))))
