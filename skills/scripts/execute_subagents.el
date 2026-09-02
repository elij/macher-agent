;;; execute_subagents.el --- Execute subagents tool -*- lexical-binding: t; -*-

(setq macher-agent-execute-subagents-tool
      (gptel-make-tool
       :name "execute_subagents"
       :description "Execute tasks across multiple sub-agents in parallel in a fire-and-forget, non-blocking manner using A2A payloads. You must supply at least one preset."
       :category "collaboration"
       :args '((:name "tasks" :type "array"
                      :description "An array of task objects to execute in parallel in the background."
                      :items (:type "object"
                                    :properties (:buffer_name (:type "string")
                                                              :instructions (:type "string")
                                                              :presets (:type "array" :items (:type "string")))
                                    :required ["buffer_name" "instructions"])))
       :async t
       :function (macher-agent-with-presentation-context (tasks)
                   (let* ((native-fn (get 'macher-agent-execute-subagents-tool 'ptc-function))
                          (buffer-names (funcall native-fn tasks context)))
                     (format "SUCCESS: Dispatched %d sub-agents in the background. They are executing independently and asynchronously. Your current buffer remains unblocked and you can proceed with other tasks immediately."
                             (length buffer-names))))))

(put 'macher-agent-execute-subagents-tool 'ptc-function
     (lambda (tasks &optional context)
       (let* ((tasks-vec (if (vectorp tasks) tasks (vconcat tasks)))
              (a2a-payloads
               (cl-loop for task-obj across tasks-vec
                        collect
                        (let ((task-id (macher-agent--generate-uuid)))
                          (macher-agent-make-a2a-payload
                           :schema-version (if (boundp 'macher-agent-a2a-schema-version)
                                               macher-agent-a2a-schema-version
                                             :a2a-v1)
                           :transit-type :root-to-subagent
                           :type 'SEND_MESSAGE
                           :task-id task-id
                           :parent-context context
                           :payload (list :instructions (plist-get task-obj :instructions))
                           :metadata (list :buffer_name (plist-get task-obj :buffer_name)
                                           :presets (append (plist-get task-obj :presets) nil)
                                           :background t
                                           :suppress-patch t))))))
         (dolist (a2a-payload a2a-payloads)
           (macher-agent-a2a-dispatch
            (list a2a-payload)
            (lambda (a2a-results)
              (let* ((res (aref (if (vectorp a2a-results) a2a-results (vconcat a2a-results)) 0))
                     (meta (macher-agent-transit-payload-metadata a2a-payload))
                     (buf-name (plist-get meta :buffer_name)))
                (if (eq (plist-get res :status) 'error)
                    (message "Background subagent %s task failed: %s" buf-name (plist-get res :error))
                  (message "Background subagent %s task execution completed successfully." buf-name))))
            context))
         (mapcar (lambda (p)
                   (plist-get (macher-agent-transit-payload-metadata p) :buffer_name))
                 a2a-payloads))))
