(macher-agent-make-tool macher-agent-execute-subagents-tool
    "Execute tasks across multiple sub-agents in parallel in a fire-and-forget, non-blocking manner."
  :category "orchestrate"
  :args '((:name "tasks" :type array :description "An array of task objects to execute in parallel in the background."
                 :items (:type object
                               :properties (:buffer_name (:type string)
                                                         :instructions (:type string)
                                                         :presets (:type array :items (:type string)))
                               :required ["buffer_name" "instructions"])))
  :command-fn (lambda (payload)
                (let* ((raw-tasks (plist-get payload :tasks))
                       (normalized-tasks
                        (cl-loop for task-obj in (append raw-tasks nil)
                                 collect (let* ((raw-presets (or (plist-get task-obj :presets)
                                                                 (alist-get 'presets task-obj)
                                                                 (alist-get "presets" task-obj nil nil #'equal)))
                                                (preset-list (cond ((vectorp raw-presets) (append raw-presets nil))
                                                                   ((stringp raw-presets) (list raw-presets))
                                                                   ((listp raw-presets) raw-presets)
                                                                   (t nil))))
                                           (list :buffer_name (or (plist-get task-obj :buffer_name)
                                                                  (alist-get 'buffer_name task-obj)
                                                                  (alist-get "buffer_name" task-obj nil nil #'equal))
                                                 :instructions (or (plist-get task-obj :instructions)
                                                                   (alist-get 'instructions task-obj)
                                                                   (alist-get "instructions" task-obj nil nil #'equal))
                                                 :presets preset-list
                                                 :background t)))))
                  (dolist (task normalized-tasks)
                    (macher-agent-spawn-task task (lambda (res) 
                                                    (message "Background subagent %s task execution completed with status: %s"
                                                             (macher-agent-tool-response-buffer-name res)
                                                             (macher-agent-tool-response-status res)))))
                  (make-macher-agent-lisp-result-response
                   :payload (format "SUCCESS: Dispatched %d sub-agents in the background. They are executing independently and asynchronously. Your current buffer remains unblocked and you can proceed with other tasks immediately."
                                    (length normalized-tasks))))))
