;;; delegate_tasks_to_subagents.el --- Delegate tasks to subagents -*- lexical-binding: t; -*-

(setq macher-agent-delegate-tasks-to-subagents-tool
      (gptel-make-tool
       :name "delegate_tasks_to_subagents"
       :description "Delegate tasks concurrently to sub-agents."
       :category "collaboration"
       :args '((:name "tasks" :type "array"
                      :items (:type "object"
                                    :properties (:buffer_name (:type "string")
                                                              :instructions (:type "string")
                                                              :presets (:type "array" :items (:type "string"))
                                                              :ephemeral (:type "boolean"))
                                    :required ["buffer_name" "instructions"])
                      :description "List of tasks to delegate. Set ephemeral to true unless subagents are to be reused."))
       :async t
       :function (lambda (callback tasks)
                   (let* ((fsm (macher-agent-get-active-fsm))
                          (context (if fsm
                                       (macher-agent-gptel-context-from-fsm fsm)
                                     (bound-and-true-p macher-agent--persistent-context))))
                     (funcall (get 'macher-agent-delegate-tasks-to-subagents-tool 'ptc-function)
                              tasks context callback)))))

(put 'macher-agent-delegate-tasks-to-subagents-tool 'ptc-function
     (lambda (tasks context callback)
       (let* ((tasks-vec (if (vectorp tasks) tasks (vconcat tasks)))
              (dispatch-payloads
               (cl-loop for task across tasks-vec
                        for instructions = (plist-get task :instructions)
                        do (when (or (null instructions)
                                     (and (stringp instructions)
                                          (string-empty-p (string-trim instructions))))
                             (error "ERROR: Instructions for delegated tasks cannot be empty."))
                        collect
                        (macher-agent-make-a2a-payload
                         :schema-version (if (boundp 'macher-agent-a2a-schema-version)
                                             macher-agent-a2a-schema-version
                                           :a2a-v1)
                         :transit-type :root-to-subagent
                         :type 'SEND_MESSAGE
                         :task-id (macher-agent--generate-uuid)
                         :parent-context context
                         :payload (list :instructions instructions)
                         :metadata (list :buffer_name (plist-get task :buffer_name)
                                         :presets (or (plist-get task :presets) (plist-get task :preset))
                                         :ephemeral (if (plist-member task :ephemeral)
                                                        (not (eq (plist-get task :ephemeral) :json-false))
                                                      t)
                                         :suppress-patch t)))))
         (macher-agent-a2a-dispatch
          dispatch-payloads
          (lambda (results)
            (let ((formatted-results
                   (cl-loop for task across tasks-vec
                            for res across results
                            for agent-name = (plist-get task :buffer_name)
                            for unwrapped = (cond
                                             ((and (fboundp 'macher-agent-transit-payload-p)
                                                   (macher-agent-transit-payload-p res))
                                              (let ((pl (macher-agent-transit-payload-payload res)))
                                                (if (listp pl)
                                                    (or (plist-get pl :payload) (plist-get pl :message) pl)
                                                  pl)))
                                             ((listp res)
                                              (or (plist-get res :payload) (plist-get res :data) (plist-get res :error) res))
                                             (t res))
                            for text-val = (if (stringp unwrapped) unwrapped (prin1-to-string unwrapped))
                            collect (format "=== Response from %s ===\n%s\n" agent-name text-val))))
              (when (functionp callback)
                (funcall callback
                         (format "All sub-agents completed:\n\n%s"
                                 (string-join formatted-results "\n"))))))
          context))))
