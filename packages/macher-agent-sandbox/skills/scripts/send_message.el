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
       (let ((ctx (or (when (macher-agent-valid-context-p context) context)
                      (macher-agent-resolve-context (current-buffer)))))
         (macher-agent-send-peer-message buffer-name message ctx))))
