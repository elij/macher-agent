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
       (let ((in-ptc (bound-and-true-p macher-agent-sandbox-active-ptc-execution)))
         (lambda (resolve-fn)
           (macher-agent-delegate-tasks tasks resolve-fn context in-ptc)))))
