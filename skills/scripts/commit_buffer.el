;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
 macher-agent-commit-buffer-tool
 "Propose and virtualise changes for a live Emacs buffer and synchronise the agent's memory via VFS."
 :category "execution"
 :args '((:name "buffer_name" :type string :description "The name of the target buffer")
         (:name "content" :type string :description "The new content to virtualise for the buffer"))
 :command-fn
 (lambda (payload context root)
   (when-let* ((buffer_name (plist-get payload :buffer_name))
               (content (plist-get payload :content)))
     (let* ((ws-root (or (and context (macher-agent-context-root context))
                         root
                         default-directory))
            (is-disk-file (and ws-root
                               (or (file-exists-p buffer_name)
                                   (file-exists-p (expand-file-name buffer_name ws-root))))))
       (unless is-disk-file
         (get-buffer-create buffer_name))
       (let* ((actual-name (macher-agent--resolve-buffer-name buffer_name)))
         (when context
           (macher-agent--ensure-access context actual-name)
           (let ((norm-key (macher-agent--normalize-path-key actual-name context)))
             (macher-agent--update-context-file context norm-key content)))
         `((status . "success") (buffer . ,actual-name))))))
 :success-fn
 (lambda (res _payload)
   (format "SUCCESS: Virtual edit recorded for buffer '%s'. A patch will be generated at the end of the turn."
           (cdr (assoc 'buffer res)))))

