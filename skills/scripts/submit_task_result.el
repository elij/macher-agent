;;; submit_task_result.el --- Submit task result tool -*- lexical-binding: t; -*-

(setq macher-agent-submit-task-result-tool
      (gptel-make-tool
       :name "submit_task_result"
       :description "Submit the final result of your assigned task back to the orchestrator. CRITICAL DIRECTIVE: You MUST use the `submit_task_result` tool to submit your final answer when you are completely finished. Do NOT output your final answer as standard text. IMMEDIATELY STOP after."
       :category "meta"
       :args '((:name "final_answer" :type "string" :description "The final answer, data, or summary of completed work."))
       :async t
       :function (macher-agent-with-presentation-context (final-answer)
                                                         (let ((native-fn (get 'macher-agent-submit-task-result-tool 'ptc-function)))
                                                           (funcall native-fn final-answer nil context)))))

(put 'macher-agent-submit-task-result-tool 'ptc-function
     (lambda (final-answer &optional task-id context)
       (let* ((explicit-ctx (macher-agent-resolve-context context))
              (caller-fsm (macher-agent-get-active-fsm))
              (target-buf (or (when (and explicit-ctx (fboundp 'macher-agent-context-owner-buffer))
                                (let ((ob (macher-agent-context-owner-buffer explicit-ctx)))
                                  (when (and (bufferp ob) (buffer-live-p ob)) ob)))
                              (when caller-fsm
                                (or (ignore-errors (plist-get (gptel-fsm-info caller-fsm) :buffer))
                                    (when (fboundp 'gptel-fsm-buffer)
                                      (ignore-errors (gptel-fsm-buffer caller-fsm)))))
                              (current-buffer)))
              (ctx (or (macher-agent-resolve-context target-buf)
                       explicit-ctx
                       (macher-agent-resolve-context (current-buffer)))))
         (with-current-buffer target-buf
           (let* ((route-frame (car-safe (bound-and-true-p macher-agent--routing-stack)))
                  (target-name (plist-get route-frame :originator-name))
                  (callback-id (or (plist-get route-frame :task-id)
                                   (bound-and-true-p macher-agent--current-task-id)
                                   task-id))
                  (suppress (plist-get route-frame :suppress-patch))
                  (meta (list :suppress-patch suppress
                              :ephemeral (and (bound-and-true-p macher-agent--is-ephemeral)
                                              (not (bound-and-true-p macher-agent--explicitly-persistent))))))

             (unless callback-id
               (error "STRICT CONTRACT VIOLATION: Payload missing required task-id"))

             (setq-local macher-agent--current-task-id callback-id)
             (setq-local macher-agent--task-result final-answer)

             (when (and suppress (macher-agent-valid-context-p ctx))
               (setf (macher-agent-context-plugins ctx)
                     (plist-put (copy-sequence (macher-agent-context-plugins ctx))
                                :suppress-patch t)))

             (let* ((valid-ctx (when (macher-agent-valid-context-p ctx) ctx))
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
                                  :metadata meta))
                    (buf-fsm (bound-and-true-p macher-agent--active-fsm))
                    (active-tool-fsm
                     (and buf-fsm
                          (fboundp 'gptel-fsm-state)
                          (memq (ignore-errors (gptel-fsm-state buf-fsm)) '(TOOL TPRE))
                          (let ((inf (ignore-errors (gptel-fsm-info buf-fsm))))
                            (eq (plist-get inf :buffer) target-buf)))))
               (setq-local macher-agent--staged-submission
                           (list :a2a-payload a2a-payload
                                 :context valid-ctx
                                 :suppress suppress
                                 :task-id callback-id))
               (if active-tool-fsm
                   (progn
                     (when (fboundp 'macher-agent--install-tret-done-edge)
                       (macher-agent--install-tret-done-edge buf-fsm))
                     (setq-local macher-agent-task-finished
                                 (<= (length (bound-and-true-p macher-agent--routing-stack)) 1))
                     "SUCCESS: Result submitted.")
                 (macher-agent--deliver-staged-submission target-buf t)
                 "SUCCESS: Result submitted. STOP NOW.")))))))
