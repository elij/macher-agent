(macher-agent-make-tool
    macher-agent-wait-for-vfs-semaphore
    "Suspends the agent until the specified file or buffer is created or modified in the virtual file system."
  :category "event"
  :args '((:name "path" :type string :description "File path or buffer name to monitor"))
  :command-fn
  (lambda (payload context _root)
    (let* ((raw-path (plist-get payload :path))
           (actual-name (if (fboundp 'macher-agent--resolve-buffer-name)
                            (macher-agent--resolve-buffer-name raw-path)
                          raw-path))
           (norm-key (macher-agent--normalize-path-key actual-name context))
           (valid-keys (list norm-key actual-name))
           (get-vfs-content
            (lambda ()
              (let ((found-content nil))
                (dolist (entry (macher-agent--get-context-contents context))
                  (when (member (car entry) valid-keys)
                    (setq found-content (if (consp (cdr entry)) (cddr entry) (cdr entry)))))
                found-content))))

      (lambda (resolve-fn)
        (let ((mutation-listener nil)
              (fallback-timer nil)
              (resolved nil)
              (safe-resolve
               (lambda ()
                 (let ((content (funcall get-vfs-content)))
                   (if (stringp content)
                       (substring content 0 (min (length content) 500))
                     "Resource updated.")))))

          (setq mutation-listener
                (lambda (&rest args)
                  (unless resolved
                    (let ((mutated-key (cl-find-if #'stringp args)))
                      (when (and mutated-key (member mutated-key valid-keys))
                        (setq resolved t)
                        (remove-hook 'macher-agent-context-mutated-hook mutation-listener)
                        (when fallback-timer (cancel-timer fallback-timer))
                        (funcall resolve-fn (funcall safe-resolve)))))))

          (add-hook 'macher-agent-context-mutated-hook mutation-listener)

          (setq
           fallback-timer
           (run-at-time 1 1
                        (lambda ()
                          (unless resolved
                            (let ((entry (or (cl-find norm-key (macher-agent--get-context-contents context) :key #'car :test #'equal)
                                             (cl-find actual-name (macher-agent--get-context-contents context) :key #'car :test #'equal))))
                              (when entry
                                (let ((orig (if (consp (cdr entry)) (cadr entry) nil))
                                      (curr (if (consp (cdr entry)) (cddr entry) (cdr entry))))
                                  (when (and curr (not (equal orig curr)))
                                    (setq resolved t)
                                    (remove-hook 'macher-agent-context-mutated-hook mutation-listener)
                                    (cancel-timer fallback-timer)
                                    (funcall resolve-fn (funcall safe-resolve))))))))))))))

  :success-fn
  (lambda (result)
    (format "SYSTEM: Semaphore unlocked! Target resource was modified.\n\n=== NEW CONTENT (Preview) ===\n%s"
            result)))
