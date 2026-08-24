;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
 macher-agent-write-buffer-in-workspace-tool
 "Propose new content for a live Emacs buffer. This creates a virtual patch that will be presented for review rather than mutating the buffer immediately."
 :category "execution"
 :args '((:name "buffer_name" :type string :description "The name of the target buffer")
         (:name "content" :type string :description "The proposed new content for the buffer"))
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
           (let ((norm-key (macher-agent--normalize-path-key actual-name context)))
             (macher-agent--update-context-file context norm-key content)))
         `((status . "success") (buffer . ,actual-name))))))
 :success-fn
 (lambda (res _payload)
   (format "SUCCESS: Virtual edit recorded for buffer '%s'. A patch will be generated at the end of the turn."
           (cdr (assoc 'buffer res)))))
