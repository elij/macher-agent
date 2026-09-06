;;; search_in_workspace.el --- Search workspace pattern tool -*- lexical-binding: t; -*-

(setq macher-agent-search-in-workspace-tool
      (gptel-make-tool
       :name "search_in_workspace"
       :description "Search for a regular expression pattern within the strictly bounded workspace."
       :category "perception"
       :include nil
       :args '((:name "pattern" :type "string" :description "The regex pattern to search for"))
       :async t
       :function (macher-agent-with-presentation-context (pattern)
                   (let ((native-fn (get 'macher-agent-search-in-workspace-tool 'ptc-function)))
                     (funcall native-fn pattern context)))))

(put 'macher-agent-search-in-workspace-tool 'ptc-function
     (lambda (pattern context)
       (let ((trimmed (string-trim (or pattern ""))))
         (if (or (string-empty-p trimmed)
                 (string-equal trimmed ".*")
                 (string-match-p "^\\s-*$" trimmed))
             (error "Regex pattern too broad. Provide a more specific search term.")
           (let ((output ""))
             (macher-agent-with-strict-vfs context
               (let ((cmd (format "rg --line-number --color=never --max-columns=150 -- '%s' . || echo 'No matches found.'"
                                  (replace-regexp-in-string "'" "'\\''" trimmed))))
                 (setq output (shell-command-to-string cmd))))
             output)))))
