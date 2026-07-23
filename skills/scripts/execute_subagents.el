(macher-agent-make-tool macher-agent-execute-subagents-tool
    "Execute tasks across multiple sub-agents in parallel in a fire-and-forget, non-blocking manner."
  :category "orchestrate"
  :args '((:name "tasks" :type array :description "An array of task objects to execute in parallel in the background."
                 :items (:type object
                               :properties (:buffer_name (:type string)
                                                         :instructions (:type string)
                                                         :presets (:type array :items (:type string)))
                               :required ["buffer_name" "instructions"])))
  :command-fn (lambda (payload _context _root)
                (let* ((raw-tasks (plist-get payload :tasks))
                       (normalized-tasks
                        (cl-loop for task-obj in (append raw-tasks nil)
                                 for raw-presets = (plist-get task-obj :presets)
                                 for preset-list = (if (stringp raw-presets)
                                                       (list raw-presets)
                                                     (append raw-presets nil))
                                 collect (list :buffer_name (plist-get task-obj :buffer_name)
                                               :instructions (plist-get task-obj :instructions)
                                               :presets preset-list
                                               :background t))))
                  (dolist (task normalized-tasks)
                    (macher-agent-spawn-task task (lambda (res)
                                                    (message "Background subagent %s task execution completed with status: %s"
                                                             (macher-agent-tool-response-buffer-name res)
                                                             (macher-agent-tool-response-status res)))))
                  (make-macher-agent-lisp-result-response
                   :payload (format "SUCCESS: Dispatched %d sub-agents in the background. They are executing independently and asynchronously. Your current buffer remains unblocked and you can proceed with other tasks immediately."
                                    (length normalized-tasks))))))
