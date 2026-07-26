(macher-agent-make-tool macher-agent-execute-subagents-tool
			"Execute tasks across multiple sub-agents in parallel in a fire-and-forget, \
non-blocking manner."
			:category "orchestrate"
			:args '(
				(:name "tasks" :type array
				       :description "An array of task objects to execute in parallel in `
the background."
				       :items (:type object :properties (
									 :buffer_name (:type string)
									 :instructions (:type string)
									 :presets (
										   :type array :items (:type string)))
						     :required ["buffer_name" "instructions"])))
			:command-fn (lambda (payload _context _root)
				      (let* ((raw-tasks (plist-get payload :tasks))
					     (normalized-tasks
					      (cl-loop for task-obj in (append raw-tasks nil)
						       collect (list
								:buffer_name (plist-get task-obj :buffer_name)
								:instructions (plist-get task-obj :instructions)
								:presets (append (plist-get task-obj :presets) nil)
								:background t))))
					(dolist (task normalized-tasks)
					  (macher-agent-spawn-task task (lambda (res)
									  (message "Background subagent %s task \
execution completed with status: %s"
										   (macher-agent-tool-response-buffer-name res)
										   (macher-agent-tool-response-status res)))))
					(mapcar (lambda (task) (plist-get task :buffer_name)) normalized-tasks)))
			:success-fn (lambda (buffer-names _payload)
				      (format "SUCCESS: Dispatched %d sub-agents in the background. They are executing independently and asynchronously. Your current buffer remains unblocked and you can proceed with other tasks immediately."
					      (length buffer-names))))
