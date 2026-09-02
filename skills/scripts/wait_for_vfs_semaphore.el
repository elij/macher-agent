;;; wait_for_vfs_semaphore.el --- Asynchronous VFS semaphore tool -*- lexical-binding: t; -*-

(setq macher-agent-wait-for-vfs-semaphore-tool
      (gptel-make-tool
       :name "wait_for_vfs_semaphore"
       :description "Suspends the agent until the specified file or buffer is created or modified in the virtual file system."
       :category "event"
       :args '((:name "path" :type "string" :description "File path or buffer name to monitor"))
       :async t
       :function (lambda (callback path)
                   (let* ((fsm (macher-agent-get-active-fsm))
                          (context (if fsm
                                       (macher-agent-gptel-context-from-fsm fsm)
                                     (bound-and-true-p macher-agent--persistent-context))))
                     (funcall (get 'macher-agent-wait-for-vfs-semaphore-tool 'ptc-function)
                              path context callback)))))

(put 'macher-agent-wait-for-vfs-semaphore-tool 'ptc-function
     (lambda (path context callback)
       (let* ((effective-root (when (macher-agent-valid-context-p context)
                                (macher-agent-context-project-root context)))
              (my-task-id (or (bound-and-true-p macher-agent--current-task-id)
                              (buffer-name (current-buffer)))))
         (unless (hash-table-p macher-agent--task-registry)
           (setq macher-agent--task-registry (make-hash-table :test 'equal)))
         (unless (hash-table-p macher-agent--pending-callbacks)
           (setq macher-agent--pending-callbacks (make-hash-table :test 'equal)))
         (unless (hash-table-p macher-agent--vfs-lock-table)
           (setq macher-agent--vfs-lock-table (make-hash-table :test 'equal)))
         (puthash my-task-id (buffer-name (current-buffer)) macher-agent--task-registry)
         (when (fboundp 'macher-agent--vfs-a2a-callback)
           (macher-agent--vfs-a2a-callback
            `(:type ACQUIRE_LOCK
              :task-id ,my-task-id
              :callback 'ignore
              :metadata (:resource_path ,path :path ,path))))
         (let ((cb-fn (lambda (result)
                        (when (functionp callback)
                          (funcall callback
                                   (format "SYSTEM: Semaphore unlocked! Target resource was modified.\n\n=== NEW CONTENT (Preview) ===\n%s"
                                           result))))))
           (puthash path cb-fn macher-agent--pending-callbacks)
           (when effective-root
             (puthash (expand-file-name path effective-root) cb-fn macher-agent--pending-callbacks))))))
