;;; execute_subagents.el --- Execute subagents tool -*- lexical-binding: t; -*-

(setq macher-agent-execute-subagents-tool
      (gptel-make-tool
       :name "execute_subagents"
       :description "Execute background subagent tasks asynchronously without blocking current flow."
       :category "orchestration"
       :args '((:name "tasks"
                      :type "array"
                      :description "A vector of background task specifications."
                      :items (:type "object"
                                    :properties (:buffer_name (:type "string"
                                                                     :description "The name of the target sub-agent buffer.")
                                                              :presets (:type "array"
                                                                              :items (:type "string")
                                                                              :description "Optional vector of skill or preset names to apply.")
                                                              :instructions (:type "string"
                                                                                   :description "The task instructions to send to the sub-agent."))
                                    :required ["buffer_name" "instructions"])))
       :async t
       :function (lambda (callback tasks)
                   (let ((thunk (funcall (get 'macher-agent-execute-subagents-tool 'ptc-function)
                                         tasks)))
                     (funcall thunk
                              (lambda (res)
                                (funcall callback
                                         (if (sequencep res)
                                             (if (stringp res)
                                                 res
                                               (string-join (append res nil) "\n\n"))
                                           (format "%s" res)))))))))

(put 'macher-agent-execute-subagents-tool 'ptc-function
     (lambda (tasks &optional context)
       (let ((ctx (or (macher-agent-resolve-context context)
                      (macher-agent-resolve-context (current-buffer)))))
         (lambda (resolve-fn)
           (if (not (vectorp tasks))
               (funcall resolve-fn "ERROR: Tasks parameter must be a vector of task specifications.")
             (let* ((dispatched-names nil)
                    (resolved nil)
                    (payloads
                     (cl-loop for t-spec across tasks
                              for raw-buf = (plist-get t-spec :buffer_name)
                              for buf-name = (cond ((stringp raw-buf) raw-buf)
                                                   ((symbolp raw-buf) (symbol-name raw-buf))
                                                   (t (format "%s" raw-buf)))
                              for presets = (or (plist-get t-spec :presets)
                                                (plist-get t-spec :skill))
                              for instructions = (or (plist-get t-spec :instructions)
                                                     (plist-get t-spec :prompt))
                              for task-id = (if (fboundp 'macher-agent--generate-uuid)
                                                (macher-agent--generate-uuid)
                                              "task-bg")
                              do (push buf-name dispatched-names)
                              collect
                              (macher-agent-make-a2a-payload
                               :transit-type :root-to-subagent
                               :type 'TASK_DELEGATION
                               :task-id task-id
                               :target-buffer buf-name
                               :target-context ctx
                               :parent-context ctx
                               :child-context ctx
                               :payload (list :instructions instructions)
                               :metadata (list :buffer_name buf-name
                                               :presets presets
                                               :background t
                                               :ephemeral t
                                               :suppress-patch t)))))
               (setq dispatched-names (nreverse dispatched-names))
               (macher-agent-a2a-dispatch
                payloads
                (lambda (results)
                  (let ((extracted
                         (mapcar (lambda (r)
                                   (cond
                                    ((stringp r) r)
                                    ((and (listp r) (plist-get r :error))
                                     (format "ERROR: %s" (plist-get r :error)))
                                    ((and (listp r) (plist-get r :payload))
                                     (let ((p (plist-get r :payload)))
                                       (if (and (listp p) (plist-get p :payload))
                                           (format "%s" (plist-get p :payload))
                                         (format "%s" p))))
                                    ((and (listp r) (plist-get r :data))
                                     (format "%s" (plist-get r :data)))
                                    (t (format "%s" r))))
                                 (append results nil))))
                    (if (not resolved)
                        (progn
                          (setq resolved t)
                          (funcall resolve-fn
                                   (if macher-agent--active-ptc-execution
                                       extracted
                                     (string-join extracted "\n\n"))))
                      (dolist (msg extracted)
                        (when (string-prefix-p "ERROR:" msg)
                          (message "Macher-Agent background task error: %s" msg))))))
                ctx)
               (unless resolved
                 (setq resolved t)
                 (let ((launch-msgs
                        (mapcar (lambda (name)
                                  (format "SUCCESS: Background sub-agent '%s' dispatched." name))
                                dispatched-names)))
                   (funcall resolve-fn
                            (if macher-agent--active-ptc-execution
                                launch-msgs
                              (string-join launch-msgs "\n")))))))))))
