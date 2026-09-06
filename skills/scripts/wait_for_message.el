;;; wait_for_message.el --- Wait for message tool -*- lexical-binding: t; -*-

(setq macher-agent-wait-for-message-tool
      (gptel-make-tool
       :name "wait_for_message"
       :description "Suspends the agent until a message, task, or payload is received from a sender."
       :category "event"
       :args nil
       :async t
       :function (lambda (callback)
                   (funcall (get 'macher-agent-wait-for-message-tool 'ptc-function) callback))))

(put 'macher-agent-wait-for-message-tool 'ptc-function
     (lambda (&optional callback-or-payload _context _root)
       (let* ((callback (when (functionp callback-or-payload) callback-or-payload))
              (thunk
               (lambda (resolve-fn)
                 (puthash (buffer-name)
                          (lambda (result)
                            (funcall resolve-fn (format "SYSTEM: Message received! Waking up.\n\n=== MESSAGE ===\n%s" result)))
                          macher-agent--pending-callbacks))))
         (if callback
             (funcall thunk callback)
           thunk))))
