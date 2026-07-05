(macher-agent-make-tool macher-agent-commit-buffer-tool
    "Directly appends an Emacs buffer and synchronise the agent's memory immediately, bypassing the patch review step."
  :category "plan"
  :args (list '(:name "buffer_name" :type string)
              '(:name "content" :type string))
  :command-fn (lambda (payload context _root)
                (let ((buffer_name (plist-get payload :buffer_name))
                      (content (plist-get payload :content)))
                  (let ((actual-name (macher-agent--resolve-buffer-name buffer_name)))
                    (macher-agent--ensure-access context actual-name)
                    (let ((target-buffer (get-buffer-create actual-name)))
                      (with-current-buffer target-buffer
                        (when (bound-and-true-p auto-save-visited-mode)
                          (auto-save-visited-mode -1))
                        (goto-char (point-max))
                        (insert content)
                        (set-buffer-modified-p t))
                      (when context
                        (macher-agent--update-context-file context actual-name content)
                        (macher-agent--auto-sync-context context))
                      (make-macher-agent-lisp-result-response
                       :payload (format "SUCCESS: Buffer '%s' has been directly overwritten and synchronised. Awaiting user save." actual-name)))))))
