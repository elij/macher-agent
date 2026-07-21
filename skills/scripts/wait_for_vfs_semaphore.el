(macher-agent-make-tool macher-agent-wait-for-vfs-semaphore
    "Suspends the agent until the specified file is created or modified in the virtual \
file system."
  :category "worker"
  :args '((:name "path" :type string :description "File path to monitor"))
  :command-fn (lambda (payload context _root)
                (let* ((raw-path (plist-get payload :path))
                       (workspace-root (macher-agent-context-root context))
                       (abs-path (expand-file-name raw-path workspace-root))
                       (get-vfs-content (lambda ()
                                          (let ((found-content nil))
                                            (dolist (entry (macher-agent--get-context-contents context))
                                              (let* ((k (car entry))
                                                     (expanded-k (if (file-name-absolute-p k) k (expand-file-name k workspace-root))))
                                                (when (equal expanded-k abs-path)
                                                  (setq found-content (if (consp (cdr entry)) (cddr entry) (cdr entry))))))
                                            found-content))))
                  (make-macher-agent-lisp-result-response
                   :payload (lambda (resolve-fn)
                              (let ((mutation-listener nil)
                                    (fallback-timer nil)
                                    (resolved nil))
                                (setq mutation-listener
                                      (lambda (&rest args)
                                        (unless resolved
                                          (let* ((mutated-path (cl-find-if #'stringp args))
                                                 (mutated-abs (when mutated-path (expand-file-name mutated-path workspace-root))))
                                            (when (and mutated-abs (equal mutated-abs abs-path))
                                              (setq resolved t)
                                              (remove-hook 'macher-agent-context-mutated-hook mutation-listener)
                                              (when fallback-timer (cancel-timer fallback-timer))
                                              (funcall resolve-fn (or (funcall get-vfs-content) "File updated.")))))))
                                (add-hook 'macher-agent-context-mutated-hook mutation-listener)
                                (setq fallback-timer
                                      (run-at-time 1 1
                                                   (lambda ()
                                                     (unless resolved
                                                       (let ((entry nil))
                                                         (dolist (e (macher-agent--get-context-contents context))
                                                           (when (equal (if (file-name-absolute-p (car e)) (car e) (expand-file-name (car e) workspace-root)) abs-path)
                                                             (setq entry e)))
                                                         (when entry
                                                           (let ((orig (if (consp (cdr entry)) (cadr entry) nil))
                                                                 (curr (if (consp (cdr entry)) (cddr entry) (cdr entry))))
                                                             (when (and curr (not (equal orig curr)))
                                                               (setq resolved t)
                                                               (remove-hook 'macher-agent-context-mutated-hook mutation-listener)
                                                               (cancel-timer fallback-timer)
                                                               (funcall resolve-fn curr))))))))))))))

  :success-fn (lambda (output)
                (if (string-prefix-p "ERROR:" output)
                    output
                  (format "File was safely updated in the VFS. New contents:\n%s" output))))
