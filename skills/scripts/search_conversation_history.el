(macher-agent-make-tool
    macher-agent-search-conversation-history-tool
    "Search the truncated earlier conversation history. Use this when the SYSTEM ALERT indicates context was removed. Returns matching lines and surrounding context."
  :category "execution"
  :args '((:name "query" :type string :description "The exact text or regular expression to search for in the history.")
          (:name "context_lines" :type number :optional t :description "Number of lines to return before and after the match (default 5)."))
  :command-fn (lambda (payload _context _root)
                (let* ((query (plist-get payload :query))
                       (context-lines-arg (plist-get payload :context_lines))
                       (fsm (bound-and-true-p macher-agent--active-fsm))
                       (info (when fsm (gptel-fsm-info fsm)))
                       (orig-buf (when info (plist-get info :buffer)))
                       (ctx-lines (if (and context-lines-arg (> context-lines-arg 0)) context-lines-arg 5))
                       (results nil))
                  
                  (if (not (buffer-live-p orig-buf))
                      "Error: Cannot locate original conversation buffer."
                    
                    (with-current-buffer orig-buf
                      (save-excursion
                        (goto-char (point-min))
                        (while (re-search-forward query nil t)
                          (let* ((match-pt (point))
                                 (start-pt (save-excursion 
                                             (forward-line (- ctx-lines)) 
                                             (line-beginning-position)))
                                 (end-pt (save-excursion 
                                           (forward-line ctx-lines) 
                                           (line-end-position)))
                                 (snippet (buffer-substring-no-properties start-pt end-pt)))
                            (push (format "--- Match near line %d ---\n%s\n" 
                                          (line-number-at-pos match-pt) snippet) 
                                  results)))))
                    
                    (if results
                        (string-join (nreverse results) "\n")
                      (format "No matches found in history for: %s" query))))))
