;;; wait_for_vfs_semaphore.el --- Asynchronous VFS semaphore tool -*- lexical-binding: t; -*-

(setq macher-agent-wait-for-vfs-semaphore-tool
      (gptel-make-tool
       :name "wait_for_vfs_semaphore"
       :description "Suspends the agent until the specified file or buffer is created or modified."
       :category "event"
       :args '((:name "path" :type "string" :description "File path or buffer name to monitor"))
       :async t
       :function (lambda (callback path)
                   (let ((context (macher-agent-resolve-context (current-buffer))))
                     (funcall (get 'macher-agent-wait-for-vfs-semaphore-tool 'ptc-function)
                              path context callback)))))

(put 'macher-agent-wait-for-vfs-semaphore-tool 'ptc-function
     (lambda (path context callback)
       (let* ((effective-root (when (macher-agent-valid-context-p context)
                                (macher-agent-context-project-root context)))
              (cb-fn (lambda (result)
                       (when (functionp callback)
                         (funcall callback
                                  (format "SYSTEM: Semaphore unlocked! Target resource was modified.\n\n=== NEW CONTENT ===\n%s"
                                          result))))))
         (unless (hash-table-p macher-agent--pending-callbacks)
           (setq macher-agent--pending-callbacks (make-hash-table :test 'equal)))
         (unless (hash-table-p macher-agent--vfs-callbacks)
           (setq macher-agent--vfs-callbacks (make-hash-table :test 'equal)))
         (when (macher-agent-valid-context-p context)
           (setf (macher-agent-session-state (macher-agent-context-session context)) :waiting))
         (puthash path cb-fn macher-agent--pending-callbacks)
         (puthash path cb-fn macher-agent--vfs-callbacks)
         (when effective-root
           (let ((abs-path (expand-file-name path effective-root)))
             (puthash abs-path cb-fn macher-agent--pending-callbacks)
             (puthash abs-path cb-fn macher-agent--vfs-callbacks))))))
