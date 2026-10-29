;;; search_in_workspace.el --- Non-blocking VFS workspace search tool -*- lexical-binding: t; -*-

(setq macher-agent-search-in-workspace-tool
      (gptel-make-tool
       :name "search_in_workspace"
       :description "Search for regex patterns across files in the workspace (including uncommitted VFS changes) using ripgrep (rg)."
       :category "perception"
       :args '((:name "regexp" :type "string" :description "The regular expression pattern to search for.")
               (:name "path" :type "string" :description "Optional relative path or file glob to restrict the search." :optional t))
       :async t
       :function (lambda (callback regexp &optional path)
                   (let ((thunk (funcall (get 'macher-agent-search-in-workspace-tool 'ptc-function)
                                         regexp path)))
                     (funcall thunk callback)))))

(put 'macher-agent-search-in-workspace-tool 'ptc-function
     (lambda (regexp &optional path context)
       (let ((ctx (or (macher-agent-resolve-context context)
                      (macher-agent-resolve-context (current-buffer)))))
         (lambda (resolve-fn)
           (cond
            ((not (and (stringp regexp) (not (string-empty-p (string-trim regexp)))))
             (funcall resolve-fn "ERROR: Search regexp must be a non-empty string."))
            ((not (executable-find "rg"))
             (funcall resolve-fn "ERROR: ripgrep (rg) executable not found in PATH."))
            (t
             (let ((rg-args (append (list "rg" "--color=never" "--no-heading"
                                          "--with-filename" "--line-number" "--smart-case")
                                    (when (and (stringp path) (not (string-empty-p (string-trim path))))
                                      (if (string-match-p "[*?\\[\\]{}]" path)
                                          (list "--glob" path)
                                        (list "--" regexp path)))
                                    (unless (and (stringp path)
                                                 (not (string-empty-p (string-trim path)))
                                                 (not (string-match-p "[*?\\[\\]{}]" path)))
                                      (list "--" regexp ".")))))
               (macher-agent-run-in-vfs-sandbox
                ctx rg-args resolve-fn
                (lambda (exit-code raw-out)
                  (let ((output (string-trim-right raw-out)))
                    (cond
                     ((= exit-code 0)
                      (if (string-empty-p output)
                          (format "No matches found for: %s" regexp)
                        output))
                     ((= exit-code 1)
                      (format "No matches found for: %s" regexp))
                     (t
                      (format "ERROR: Search failed (exit code %d): %s"
                              exit-code output)))))))))))))
