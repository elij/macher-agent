;;; send_message.el --- Send message tool -*- lexical-binding: t; -*-

(setq macher-agent-send-message-tool
      (gptel-make-tool
       :name "send_message"
       :description "Sends a message or payload to a background subagent. This is a non-blocking operation that delivers the message and continues."
       :category "collaboration"
       :args '((:name "buffer_name" :type "string" :description "The target buffer name of the subagent")
               (:name "message" :type "string" :description "The message, instructions, or payload to send"))
       :async t
       :function (macher-agent-with-presentation-context (buffer-name message)
                   (let ((native-fn (get 'macher-agent-send-message-tool 'ptc-function)))
                     (funcall native-fn buffer-name message context)))))

(put 'macher-agent-send-message-tool 'ptc-function
     (lambda (buffer-name message &optional context)
       (let* ((actual-name (macher-agent--resolve-buffer-name buffer-name))
              (target-buf (get-buffer actual-name)))
         (if (not (and target-buf (buffer-live-p target-buf)))
             (format "ERROR: Buffer '%s' does not exist." actual-name)
           (let* ((task-id (macher-agent--generate-uuid))
                  (a2a-payload (macher-agent-make-a2a-payload
                                :schema-version (if (boundp 'macher-agent-a2a-schema-version)
                                                    macher-agent-a2a-schema-version
                                                  :a2a-v1)
                                :transit-type :peer-to-peer
                                :type 'SEND_MESSAGE
                                :task-id task-id
                                :target-buffer actual-name
                                :parent-context context
                                :shared-state (list :task-id task-id)
                                :payload (list :instructions message)
                                :metadata (list :buffer_name actual-name
                                                :background t
                                                :suppress-patch t))))
             (macher-agent-a2a-dispatch (list a2a-payload) nil context)
             (format "SYSTEM: Message successfully dispatched to %s." actual-name))))))
