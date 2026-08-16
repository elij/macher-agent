;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
    macher-agent-send-message-tool
    "Sends a message or payload to a background subagent. This is a non-blocking operation that delivers the message and continues."
  :category "collaboration"
  :args '((:name "buffer_name" :type string :description "The target buffer name of the subagent")
          (:name "message" :type string :description "The message, instructions, or payload to send"))
  :command-fn
  (lambda (payload _context _root)
    (let* ((buffer_name (plist-get payload :buffer_name))
           (actual-name (if (fboundp 'macher-agent--resolve-buffer-name)
                            (macher-agent--resolve-buffer-name buffer_name)
                          buffer_name))
           (target-buf (get-buffer actual-name)))
      (if (not (and target-buf (buffer-live-p target-buf) (gethash actual-name macher-agent--pending-callbacks)))
          (format "ERROR: Target agent buffer '%s' does not exist or is not registered for callbacks." buffer_name)
        (let* ((task-id (if (fboundp 'org-id-uuid) (org-id-uuid) (format "task-%04x" (random #xffff))))
               (a2a-payload (list :type 'SEND_MESSAGE
                                  :task-id task-id
                                  :message (list :instructions (plist-get payload :message))
                                  :metadata (list :buffer_name actual-name
                                                  :background t
                                                  :suppress-patch t))))
          (macher-agent-a2a-dispatch (list a2a-payload) nil)
          
          (format "SYSTEM: Message successfully dispatched to %s." actual-name)))))
  :success-fn
  (lambda (result) result))
