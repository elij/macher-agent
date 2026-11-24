;;; wait_for_vfs_semaphore.el --- Asynchronous VFS semaphore tool -*- lexical-binding: t; -*-

(setq macher-agent-wait-for-vfs-semaphore-tool
      (gptel-make-tool
       :name "wait_for_vfs_semaphore"
       :description "Suspends the agent until the specified file or buffer is created or modified."
       :category "event"
       :args '((:name "path" :type "string" :description "File path or buffer name to monitor"))
       :async t
       :function (lambda (callback path)
                   (let ((thunk (funcall (get 'macher-agent-wait-for-vfs-semaphore-tool 'ptc-function)
                                         path)))
                     (funcall thunk callback)))))

(put 'macher-agent-wait-for-vfs-semaphore-tool 'ptc-function
     (lambda (path &optional context)
       (let* ((fsm (macher-agent-get-active-fsm))
              (ctx (or (when context (macher-agent-resolve-context context))
                       (and fsm (macher-agent-gptel-context-from-fsm fsm))
                       (macher-agent-resolve-context (current-buffer)))))
         (lambda (resolve-fn)
           (let* ((effective-root (when (macher-agent-valid-context-p ctx)
                                    (macher-agent-context-project-root ctx)))
                  (norm-path (when (and (macher-agent-valid-context-p ctx)
                                        (fboundp 'macher-agent-vfs-normalize-path-key))
                               (ignore-errors
                                 (funcall (symbol-function 'macher-agent-vfs-normalize-path-key)
                                          path ctx))))
                  (cb-fn (lambda (result)
                           (when (functionp resolve-fn)
                             (funcall resolve-fn
                                      (format "SYSTEM: Semaphore unlocked! Target resource was modified.\n\n=== NEW CONTENT ===\n%s"
                                              result))))))
             (unless (hash-table-p macher-agent-pending-callbacks)
               (setq macher-agent-pending-callbacks (make-hash-table :test 'equal)))
             (unless (hash-table-p macher-agent-vfs-callbacks)
               (setq macher-agent-vfs-callbacks (make-hash-table :test 'equal)))
             (when (macher-agent-valid-context-p ctx)
               (setf (macher-agent-session-state (macher-agent-context-session ctx)) :waiting))
             (dolist (key (delete-dups
                           (delq nil
                                 (list path
                                       norm-path
                                       (when effective-root
                                         (expand-file-name path effective-root))))))
               (puthash key cb-fn macher-agent-pending-callbacks)
               (puthash key cb-fn macher-agent-vfs-callbacks)))))))
