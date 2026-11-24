;;; wait_for_message.el --- Wait for message tool -*- lexical-binding: t; -*-

(setq macher-agent-wait-for-message-tool
      (gptel-make-tool
       :name "wait_for_message"
       :description "Suspends the agent until a message, task, or payload is received from a sender."
       :category "event"
       :args nil
       :async t
       :function (lambda (callback)
                   (let ((thunk (funcall (get 'macher-agent-wait-for-message-tool 'ptc-function))))
                     (funcall thunk callback)))))

(put 'macher-agent-wait-for-message-tool 'ptc-function
     (lambda (&optional callback context _root)
       (let ((cb (when (functionp callback) callback))
             (thunk (lambda (resolve-fn)
                      (macher-agent-wait-for-peer-message resolve-fn context))))
         (if cb
             (funcall thunk cb)
           thunk))))
