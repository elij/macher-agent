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
           for task-obj across (if (vectorp raw-tasks) raw-tasks (vconcat raw-tasks))
           collect
           (let
               ((task-id
                 (if (fboundp 'org-id-uuid)
                     (org-id-uuid) (format "task-%04x" (random #xffff)))))
             (macher-agent-make-a2a-payload
              :schema-version (if (boundp 'macher-agent-a2a-schema-version)
                                  macher-agent-a2a-schema-version
                                :a2a-v1)
              :transit-type :root-to-subagent
              :type 'SEND_MESSAGE
              :task-id task-id
              :parent-context (when (bound-and-true-p macher-agent--persistent-context)
                                macher-agent--persistent-context)
              :payload (list :instructions (plist-get task-obj :instructions))
              :message (list :instructions (plist-get task-obj :instructions))
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
                  (buf-name (plist-get meta :buffer_name))
                  (is-error (eq (plist-get res :status) 'error)))
             (if is-error
                 (message "Background subagent %s task failed: %s" buf-name (plist-get res :error))
               (message "Background subagent %s task execution completed successfully." buf-name))))
         context))
      
      (mapcar (lambda (p)
                (let ((meta (macher-agent-transit-payload-metadata p)))
                  (plist-get meta :buffer_name)))
              a2a-payloads)))
  :success-fn
  (lambda (buffer-names _payload)
    (format "SUCCESS: Dispatched %d sub-agents in the background. They are executing independently and asynchronously. Your current buffer remains unblocked and you can proceed with other tasks immediately."
            (length buffer-names))))
