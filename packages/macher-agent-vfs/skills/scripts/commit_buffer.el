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
     (lambda (buffer-name content &optional context)
       (unless (and (fboundp 'macher-agent-vfs-ensure-access)
                    (fboundp 'macher-agent-vfs-normalize-path-key)
                    (fboundp 'macher-agent-vfs-update-context-file))
         (error "Macher-Agent VFS plugin (`macher-agent-vfs') is not loaded"))
       (let* ((fsm (macher-agent-get-active-fsm))
              (ctx (or (when context (macher-agent-resolve-context context))
                       (and fsm (macher-agent-gptel-context-from-fsm fsm))
                       (macher-agent-resolve-context (current-buffer))
                       (macher-agent-resolve-context nil)))
              (ws-root (if (macher-agent-valid-context-p ctx)
                           (macher-agent-context-project-root ctx)
                         default-directory))
              (actual-name (macher-agent-resolve-buffer-name buffer-name))
              (is-disk-file (or (file-exists-p actual-name)
                                (file-exists-p (expand-file-name actual-name ws-root)))))
         (when (macher-agent-valid-context-p ctx)
           (unless (or is-disk-file (get-buffer actual-name))
             (let ((scoped-ht (when (fboundp 'macher-agent-vfs-scoped-buffers)
                                (funcall (symbol-function 'macher-agent-vfs-scoped-buffers) ctx)))
                   (vfs-ht (when (fboundp 'macher-agent-vfs-buffers)
                             (funcall (symbol-function 'macher-agent-vfs-buffers) ctx))))
               (unless (or (and (hash-table-p scoped-ht) (gethash actual-name scoped-ht))
                           (and (hash-table-p vfs-ht) (gethash actual-name vfs-ht)))
                 (error "SECURITY ERROR: Buffer '%s' is not in the agent's authorised scope" actual-name))))
           (funcall (symbol-function 'macher-agent-vfs-ensure-access) ctx actual-name))
         (unless is-disk-file
           (get-buffer-create actual-name))
         (when (macher-agent-valid-context-p ctx)
           (let ((norm-key (funcall (symbol-function 'macher-agent-vfs-normalize-path-key)
                                    actual-name ctx)))
             (funcall (symbol-function 'macher-agent-vfs-update-context-file)
                      ctx norm-key content)))
         `((status . "success") (buffer . ,actual-name)))))
