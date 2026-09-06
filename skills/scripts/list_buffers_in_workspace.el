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
       (when (macher-agent-valid-context-p context)
         (let* ((root-dir (macher-agent-context-project-root context))
                (entries (macher-agent--get-context-contents context))
                acc)
           (dolist (entry entries (nreverse acc))
             (let* ((buf-name (macher-agent-vfs-entry-path entry))
                    (classification (macher-agent--classify-file-path buf-name root-dir)))
               (when (memq classification '(buffer file external media))
                 (push buf-name acc))))))))
