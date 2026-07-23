(macher-agent-make-tool macher-agent-list-buffers-in-workspace-tool
    "List all buffers you currently have explicit access to. You cannot access buffers \
outside this list."
  :category "ro"
  :args nil
  :command-fn (lambda (_payload context root-dir)
                (let ((active-buffers
                       (when-let* ((entries (and context
                                                 (macher-agent--get-context-contents context))))
                         (let (acc)
                           (dolist (entry entries (nreverse acc))
                             (let* ((buf-name (macher-agent-vfs-entry-path entry))
                                    (classification
                                     (macher-agent-context-classify-entry buf-name root-dir)))
                               (when (memq classification '(buffer external))
                                 (push buf-name acc))))))))
                  (make-macher-agent-lisp-result-response
                   :payload (if active-buffers
                                (mapconcat #'identity active-buffers "\n")
                              "No buffers are currently in your scope.")))))
