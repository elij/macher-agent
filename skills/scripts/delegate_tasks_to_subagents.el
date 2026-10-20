;;; delegate_tasks_to_subagents.el --- Delegate tasks to subagents tool -*- lexical-binding: t; -*-

(setq macher-agent-delegate-tasks-to-subagents-tool
      (gptel-make-tool
       :name "delegate_tasks_to_subagents"
       :description "Spawn or address multiple sub-agents concurrently, delegating structured tasks and awaiting their aggregated results."
       :category "orchestration"
       :args '((:name "tasks"
                      :type "array"
                      :description "A vector of task plists containing buffer_name, presets, and instructions."
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
                   (let ((thunk (funcall (get 'macher-agent-delegate-tasks-to-subagents-tool 'ptc-function)
                                         tasks)))
                     (funcall thunk
                              (lambda (res)
                                (funcall callback
                                         (if (sequencep res)
                                             (if (stringp res)
                                                 res
                                               (string-join (append res nil) "\n\n"))
                                           (format "%s" res)))))))))

(put 'macher-agent-delegate-tasks-to-subagents-tool 'ptc-function
     (lambda (tasks &optional context)
       (let ((ctx (or (macher-agent-resolve-context context)
                      (macher-agent-resolve-context (current-buffer)))))
         (lambda (resolve-fn)
           (if (not (vectorp tasks))
               (funcall resolve-fn "ERROR: Tasks parameter must be a vector of task specifications.")
             (let ((payloads
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
                                             "task-default")
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
                                              :ephemeral t
                                              :suppress-patch t)))))
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
                    (funcall resolve-fn
                             (if macher-agent--active-ptc-execution
                                 extracted
                               (string-join extracted "\n\n")))))
                ctx)))))))
