(macher-agent-make-tool
 macher-agent-wait-for-vfs-semaphore
 "Suspends the agent until the specified file or buffer is created or modified in the virtual file system."
 :category "event"
 :args '((:name "path" :type string :description "File path or buffer name to monitor"))
 :command-fn
 (lambda (payload context _root)
   (let* ((raw-path (plist-get payload :path))
          (target-key (macher-agent--normalize-path-key raw-path context))
          (get-vfs-content
           (lambda ()
             (let ((found-content nil))
               (dolist (entry (macher-agent--get-context-contents context))
                 (when (equal (car entry) target-key)
                   (setq found-content (if (consp (cdr entry)) (cddr entry) (cdr entry)))))
               found-content))))

     (lambda (resolve-fn)
       (let ((mutation-listener nil)
             (fallback-timer nil)
             (resolved nil))

         (setq mutation-listener
               (lambda (&rest args)
                 (unless resolved
                   (let ((mutated-key (cl-find-if #'stringp args)))
                     (when (and mutated-key (equal mutated-key target-key))
                       (setq resolved t)
                       (remove-hook 'macher-agent-context-mutated-hook mutation-listener)
                       (when fallback-timer (cancel-timer fallback-timer))
                       (funcall resolve-fn (or (funcall get-vfs-content) "Resource updated.")))))))

         (add-hook 'macher-agent-context-mutated-hook mutation-listener)

         (setq
          fallback-timer
          (run-at-time 1 1
                       (lambda ()
                         (unless resolved
                           (let
                               ((entry
                                 (cl-find
                                  target-key
                                  (macher-agent--get-context-contents context) :key #'car :test #'equal)))
                             (when entry
                               (let ((orig (if (consp (cdr entry)) (cadr entry) nil))
                                     (curr (if (consp (cdr entry)) (cddr entry) (cdr entry))))
                                 (when (and curr (not (equal orig curr)))
                                   (setq resolved t)
                                   (remove-hook 'macher-agent-context-mutated-hook mutation-listener)
                                   (cancel-timer fallback-timer)
                                   (funcall resolve-fn
                                            (or (funcall get-vfs-content) "Resource updated."))))))))))))))

 :success-fn
 (lambda (result _payload)
   (format "SYSTEM: Semaphore unlocked! Resource '%s' was modified.\n\n=== NEW CONTENT (Preview) ===\n%s"
           (plist-get _payload :path)
           (if (stringp result)
               (substring result 0 (min (length result) 500))
             "Resource updated."))))
