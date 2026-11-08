;;; list_buffers_in_workspace.el --- List buffers in workspace -*- lexical-binding: t; -*-

(setq macher-agent-list-buffers-in-workspace-tool
      (gptel-make-tool
       :name "list_buffers_in_workspace"
       :description "List all buffers you currently have explicit access to. You cannot access buffers outside this list."
       :category "perception"
       :args nil
       :async t
       :function (macher-agent-with-presentation-context ()
                   (let* ((native-fn (get 'macher-agent-list-buffers-in-workspace-tool 'ptc-function))
                          (active-buffers (funcall native-fn context)))
                     (if active-buffers
                         (mapconcat #'identity active-buffers "\n")
                       "No buffers are currently in your scope.")))))

(put 'macher-agent-list-buffers-in-workspace-tool 'ptc-function
     (lambda (&optional context)
       (let ((ctx (or (macher-agent-resolve-context context)
                      (macher-agent-resolve-context (current-buffer))
                      (macher-agent-resolve-context nil))))
         (when (macher-agent-valid-context-p ctx)
           (let* ((scoped-ht (macher-agent-workspace-scoped-buffers ctx))
                  (vfs-ht (macher-agent-workspace-vfs-buffers ctx))
                  acc)
             (when (hash-table-p scoped-ht)
               (maphash (lambda (k _v)
                          (unless (member k acc)
                            (push k acc)))
                        scoped-ht))
             (when (hash-table-p vfs-ht)
               (maphash (lambda (k _v)
                          (unless (member k acc)
                            (push k acc)))
                        vfs-ht))
             (nreverse acc))))))
