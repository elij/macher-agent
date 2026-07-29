(macher-agent-make-tool
 macher-agent-search-in-workspace-tool
 "Search for a regular expression pattern within the strictly bounded workspace."
 :category "perception"
 :args '((:name "pattern" :type string :description "The regex pattern to search for"))
 :command-fn (lambda (payload context _root)
               (if-let* ((pattern (plist-get payload :pattern))
                         (trimmed (string-trim pattern))
                         ((not (string-empty-p trimmed)))
                         ((not (string-equal trimmed ".*")))
                         ((not (string-match-p "^\\s-*$" trimmed))))
                   (let
                       ((output ""))
                     (macher-agent-with-strict-vfs-pipeline
                      context
                      (let ((cmd (format "rg \
--line-number --color=never --max-columns=150 '%s' . || echo 'No matches found.'" 
                                         (replace-regexp-in-string "'" "'\\''" trimmed))))
                        (setq output
                              (shell-command-to-string cmd))))
                     output)
                 (error "Regex pattern too broad. Provide a more specific search term."))))
