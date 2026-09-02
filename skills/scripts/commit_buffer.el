;;; commit_buffer.el --- Commit buffer tool -*- lexical-binding: t; -*-

(setq macher-agent-commit-buffer-tool
      (gptel-make-tool
       :name "commit_buffer"
       :description "Propose and virtualise changes for a live Emacs buffer and synchronise the agent's memory via VFS."
       :category "execution"
       :args '((:name "buffer_name" :type "string" :description "The name of the target buffer")
               (:name "content" :type "string" :description "The new content to virtualise for the buffer"))
       :async t
       :function (macher-agent-with-presentation-context (buffer-name content)
                   (let* ((native-fn (get 'macher-agent-commit-buffer-tool 'ptc-function))
                          (res (funcall native-fn buffer-name content context)))
                     (format "SUCCESS: Virtual edit recorded for buffer '%s'. A patch will be generated at the end of the turn."
                             (cdr (assoc 'buffer res)))))))

(put 'macher-agent-commit-buffer-tool 'ptc-function
     (lambda (buffer-name content context)
       (let* ((ws-root (if (macher-agent-valid-context-p context)
                           (macher-agent-context-project-root context)
                         default-directory))
              (is-disk-file (or (file-exists-p buffer-name)
                                (file-exists-p (expand-file-name buffer-name ws-root)))))
         (unless is-disk-file
           (get-buffer-create buffer-name))
         (let ((actual-name (macher-agent--resolve-buffer-name buffer-name)))
           (when (macher-agent-valid-context-p context)
             (macher-agent--ensure-access context actual-name)
             (let ((norm-key (macher-agent--normalize-path-key actual-name context)))
               (macher-agent--update-context-file context norm-key content)))
           `((status . "success") (buffer . ,actual-name))))))
