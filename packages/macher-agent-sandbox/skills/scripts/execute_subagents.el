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
       (let ((in-ptc (bound-and-true-p macher-agent-sandbox-active-ptc-execution)))
         (lambda (resolve-fn)
           (macher-agent-execute-background-subagents tasks resolve-fn context in-ptc)))))
