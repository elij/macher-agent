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
       (let* ((fsm (macher-agent-get-active-fsm))
              (resolved-ctx (or (macher-agent-resolve-context context)
                                (macher-agent-resolve-context (current-buffer))))
              (target-buf (or (when fsm
                                (or (ignore-errors (plist-get (gptel-fsm-info fsm) :buffer))
                                    (when (fboundp 'gptel-fsm-buffer)
                                      (ignore-errors (gptel-fsm-buffer fsm)))))
                              (when (and resolved-ctx (fboundp 'macher-agent-context-owner-buffer))
                                (macher-agent-context-owner-buffer resolved-ctx))
                              (current-buffer)))
              (cb (when (functionp callback) callback))
              (thunk
               (lambda (resolve-fn)
                 (with-current-buffer target-buf
                   (let* ((ctx (macher-agent-resolve-context target-buf))
                          (session (when (macher-agent-valid-context-p ctx)
                                     (macher-agent-context-session ctx)))
                          (queue (or (bound-and-true-p macher-agent--message-queue)
                                     (when session
                                       (macher-agent-session-message-queue session)))))
                     (if (consp queue)
                         (let ((msg (car queue))
                               (rest-queue (cdr queue)))
                           (setq-local macher-agent--message-queue rest-queue)
                           (when session
                             (setf (macher-agent-session-message-queue session) rest-queue)
                             (setf (macher-agent-session-state session) :running))
                           (funcall resolve-fn
                                    (format "SYSTEM: Message received! Waking up.\n\n=== MESSAGE ===\n%s" msg)))
                       (when session
                         (setf (macher-agent-session-state session) :suspended))
                       (macher-agent-register-wake-callback
                        (buffer-name target-buf)
                        (lambda (result)
                          (with-current-buffer target-buf
                            (when session
                              (setf (macher-agent-session-state session) :running))
                            (funcall resolve-fn
                                     (format "SYSTEM: Message received! Waking up.\n\n=== MESSAGE ===\n%s" result)))))))))))
         (if cb
             (funcall thunk cb)
           thunk))))
