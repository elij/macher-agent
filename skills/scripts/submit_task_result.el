;;; submit_task_result.el --- Submit task result tool -*- lexical-binding: t; -*-

(setq macher-agent-submit-task-result-tool
      (gptel-make-tool
       :name "submit_task_result"
       :description "Submit the final result of your assigned task back to the orchestrator. CRITICAL DIRECTIVE: You MUST use the `submit_task_result` tool to submit your final answer when you are completely finished. Do NOT output your final answer as standard text. IMMEDIATELY STOP after."
       :category "event"
       :args '((:name "final_answer" :type "string" :description "The final answer, data, or summary of completed work."))
       :async t
       :function (macher-agent-with-presentation-context (final-answer)
                   (let ((native-fn (get 'macher-agent-submit-task-result-tool 'ptc-function)))
                     (funcall native-fn final-answer nil context)))))

(put 'macher-agent-submit-task-result-tool 'ptc-function
     (lambda (final-answer &optional task-id context)
       (let* ((fsm (macher-agent-get-active-fsm))
              (target-buf (or (when fsm
                                (or (ignore-errors (plist-get (gptel-fsm-info fsm) :buffer))
                                    (when (fboundp 'gptel-fsm-buffer)
                                      (ignore-errors (gptel-fsm-buffer fsm)))))
                              (when (macher-agent-context-p context)
                                (let ((plugins (macher-agent-context-plugins context)))
                                  (when (macher-agent--plist-p plugins)
                                    (plist-get plugins :buffer))))
                              (current-buffer))))
         (with-current-buffer target-buf
           (let* ((route-frame (macher-agent--pop-routing))
                  (target-name (plist-get route-frame :originator-name))
                  (callback-id (or (plist-get route-frame :task-id)
                                   (bound-and-true-p macher-agent--current-task-id)
                                   task-id))
                  (suppress (plist-get route-frame :suppress-patch))
                  (meta (list :suppress-patch suppress
                              :ephemeral (bound-and-true-p macher-agent--is-ephemeral)
                              :background (bound-and-true-p macher-agent--is-background))))

             (setq-local macher-agent--task-result final-answer)

             (when (and suppress (macher-agent-valid-context-p context))
               (setf (macher-agent-context-plugins context)
                     (plist-put (copy-sequence (macher-agent-context-plugins context)) :suppress-patch t)))

             (let* ((valid-ctx (when (macher-agent-valid-context-p context) context))
                    (raw-msg (list :payload final-answer
                                   :child-context valid-ctx
                                   :metadata meta))
                    (msg-payload (if (fboundp 'macher-agent-run-pipeline)
                                     (macher-agent-run-pipeline 'artifact-compose raw-msg)
                                   raw-msg))
                    (a2a-payload (macher-agent-make-a2a-payload
                                  :transit-type :subagent-to-parent
                                  :type 'ARTIFACT_UPDATE
                                  :task-id callback-id
                                  :target-buffer target-name
                                  :target-context valid-ctx
                                  :parent-context valid-ctx
                                  :child-context valid-ctx
                                  :shared-state (list :task-id callback-id)
                                  :payload msg-payload
                                  :metadata meta)))

               (macher-agent-a2a-dispatch (list a2a-payload) nil valid-ctx)

               (unless (bound-and-true-p macher-agent--routing-stack)
                 (setq-local macher-agent-task-finished t)
                 (when suppress
                   (setq-local macher-agent--suppress-patch t)
                   (when (bound-and-true-p macher-agent--is-ephemeral)
                     (setq-local macher-agent--ready-to-reap t))))

               "SUCCESS: Result submitted. STOP NOW."))))))
