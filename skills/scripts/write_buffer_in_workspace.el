;;; write_buffer_in_workspace.el --- Write buffer in workspace tool -*- lexical-binding: t; -*-

(setq macher-agent-write-buffer-in-workspace-tool
      (gptel-make-tool
       :name "write_buffer_in_workspace"
       :description "Propose new content for a live Emacs buffer. This creates a virtual patch that will be presented for review rather than mutating the buffer immediately."
       :category "execution"
       :args '((:name "buffer_name" :type "string" :description "The name of the target buffer")
               (:name "content" :type "string" :description "The proposed new content for the buffer"))
       :async t
       :function (macher-agent-with-presentation-context (buffer-name content)
                   (let* ((native-fn (get 'macher-agent-write-buffer-in-workspace-tool 'ptc-function))
                          (res (funcall native-fn buffer-name content context)))
                     (format "SUCCESS: Virtual edit recorded for buffer '%s'. A patch will be generated at the end of the turn."
                             (cdr (assoc 'buffer res)))))))

(put 'macher-agent-write-buffer-in-workspace-tool 'ptc-function
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
             (let ((norm-key (macher-agent--normalize-path-key actual-name context)))
               (macher-agent--update-context-file context norm-key content)))
           `((status . "success") (buffer . ,actual-name))))))
