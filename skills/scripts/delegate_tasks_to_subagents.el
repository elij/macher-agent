;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
    macher-agent-delegate-tasks-to-subagents-tool
    "Delegate tasks concurrently to sub-agents."
  :category "collaboration"
  :args '((:name "tasks" :type array
                 :items (:type object
                               :properties (:buffer_name (:type string)
                                                         :instructions (:type string)
                                                         :presets (:type array :items (:type string))
                                                         :ephemeral (:type boolean))
                               :required ["buffer_name" "instructions"])
                 :description "List of tasks to delegate"))
  :command-fn
  (lambda (payload context _root callback)
    (let* ((tasks (plist-get payload :tasks))
           (dispatch-payloads
            (cl-loop for task across (if (vectorp tasks) tasks (vconcat tasks))
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
                      :parent-context context
                      :payload (list :instructions instructions)
                      :message (list :instructions instructions)
                      :metadata (list :buffer_name (plist-get task :buffer_name)
                                      :presets (plist-get task :presets)
                                      :ephemeral (plist-get task :ephemeral)
                                      :suppress-patch t))))) 
      (let ((saved-fsm (macher-agent-get-active-fsm)))
        (macher-agent-a2a-dispatch
         dispatch-payloads
         (let ((fsm saved-fsm))
           (lambda (results)
             (let ((formatted-results
                    (cl-loop for res across results
                             collect (format "=== Response from sub-agent ===\n%s\n"
                                             (or (plist-get res :message)
                                                 (plist-get res :data)
                                                 (plist-get res :error)
                                                 res)))))
               (funcall callback
                        (format "All sub-agents completed:\n\n%s"
                                (string-join formatted-results "\n"))))))
         context)))))
