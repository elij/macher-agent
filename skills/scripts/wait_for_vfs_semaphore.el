(macher-agent-make-tool
    macher-agent-wait-for-vfs-semaphore
    "Suspends the agent until the specified file or buffer is created or modified in the virtual file system."
  :category "event"
  :args '((:name "path" :type string :description "File path or buffer name to monitor"))
  :command-fn
  (lambda (payload _context _root)
    (let ((path (plist-get payload :path))
          (my-task-id (bound-and-true-p macher-agent--current-task-id)))
      (lambda (resolve-fn)
        ;; Register callback BEFORE invoking A2A callback directly
        (puthash path resolve-fn macher-agent--pending-callbacks)
        (macher-agent--vfs-a2a-callback
         `(:type ACQUIRE_LOCK
                 :task-id ,my-task-id
                 :metadata (:resource_path ,path))))))
  :success-fn
  (lambda (result)
    (format "SYSTEM: Semaphore unlocked! Target resource was modified.\n\n=== NEW CONTENT (Preview) ===\n%s"
            result)))
