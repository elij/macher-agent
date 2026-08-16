;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
    macher-agent-execute-subagents-tool
    "Execute tasks across multiple sub-agents in parallel in a fire-and-forget, \
non-blocking manner using A2A payloads.  You must supply at least one preset."
  :category "collaboration"
  :args '((
           :name "tasks" :type array
           :description "An array of task objects to execute in parallel in the background."
           :items
           (:type object :properties
                  (:buffer_name (:type string)
                                :instructions (:type string)
                                :presets
                                (:type array
                                       :items (:type string)))
                  :required ["buffer_name" "instructions"])))
  :command-fn
  (lambda (payload context _root)
    (let*
        ((raw-tasks (plist-get payload :tasks))
         (a2a-payloads
          (cl-loop
           for task-obj in (append raw-tasks nil)
           collect
           (let
               ((task-id
                 (if (fboundp 'org-id-uuid)
                     (org-id-uuid) (format "task-%04x" (random #xffff)))))
             (list
              :type 'SEND_MESSAGE
              :task-id task-id
              :message (list :instructions (plist-get task-obj :instructions))
              :metadata (list :buffer_name (plist-get task-obj :buffer_name)
                              :presets (append (plist-get task-obj :presets) nil)
                              :background t
                              :suppress-patch t))))))

      (dolist (payload a2a-payloads)
        (macher-agent-a2a-dispatch
         (list payload)
         (lambda (a2a-results)
           (let* ((res (aref (if (vectorp a2a-results) a2a-results (vconcat a2a-results)) 0))
                  (msg (plist-get res :message))
                  (status (or (plist-get res :status) (plist-get msg :status)))
                  (buf-name (or (plist-get res :buffer-name) (plist-get msg :buffer-name))))
             (message "Background subagent %s task execution completed with status: %s"
                      buf-name status)))
         context))
      
      (mapcar (lambda (p) (plist-get (plist-get p :metadata) :buffer_name)) a2a-payloads)))
  :success-fn
  (lambda (buffer-names _payload)
    (format "SUCCESS: Dispatched %d sub-agents in the background. They are executing independently and \\asynchronously. Your current buffer remains unblocked and you can proceed with other tasks immediately."
            (length buffer-names))))
