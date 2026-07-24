(macher-agent-make-tool macher-agent-delegate-tasks-to-subagents-tool
			"Delegate tasks to multiple sub-agents asynchronously."
			:category "orchestrate"
			:args (list '(:name "tasks" :type array :items
					    (:type object :properties
						   (:buffer_name (:type string)
								 :instructions (:type string)
								 :presets (:type array :items (:type string))))))
			:command-fn (lambda (payload _context _root)
				      (let* ((raw-tasks (plist-get payload :tasks))
					     (normalized-tasks
					      (cl-loop for task-obj in (append raw-tasks nil)
						       collect (list
								:buffer_name (plist-get task-obj :buffer_name)
								:instructions (plist-get task-obj :instructions)
								:presets (append (plist-get task-obj :presets) nil)
								:suppress_patch t))))
					(make-macher-agent-delegate-response :payload (vconcat normalized-tasks))))
			:success-fn (lambda (results)
				      (let ((output (list "All sub-agents completed. Outputs:\n")))
					(cl-loop for res across (if (vectorp results) results (vconcat results))
						 do (push
						     (format "=== Response from %s ===%s\n"
							     (macher-agent-tool-response-buffer-name res)
							     (if (eq
								  (macher-agent-tool-response-status res) 'success)
								 (macher-agent-tool-response-data res)
							       (macher-agent-tool-response-error res)))
						     output))
					(string-join (nreverse output) "\n"))))
