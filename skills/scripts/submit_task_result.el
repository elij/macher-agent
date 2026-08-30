;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
    macher-agent-submit-task-result-tool
    "Submit the final result of your assigned task back to the orchestrator. CRITICAL DIRECTIVE: You MUST use the `submit_task_result` tool to submit your final answer when you are completely finished. Do NOT output your final answer as standard text. IMMEDIATELY STOP after."
  :category "event"
  :args '((:name "final_answer" :type string :description "The final answer, data, or summary of completed work."))
  :command-fn
  (lambda (payload context _root)
    (if (bound-and-true-p macher-agent-task-finished)
        (progn
          (when (bound-and-true-p macher-agent--suppress-patch)
            (setq-local macher-agent--ready-to-reap t))
          (when (fboundp 'gptel-abort)
            (ignore-errors
              (condition-case nil
                  (gptel-abort (current-buffer))
                (wrong-number-of-arguments
                 (gptel-abort)))))
          (when (fboundp 'macher-agent-bridge-abort)
            (ignore-errors (macher-agent-bridge-abort (current-buffer))))
          "ERROR: Task has already been submitted.")
      (let* ((final_answer (plist-get payload :final_answer))
             
             (route-frame (when (bound-and-true-p macher-agent--routing-stack)
                            (pop macher-agent--routing-stack)))
             
             (target-name (or (plist-get route-frame :originator-name)
                              (car-safe (bound-and-true-p macher-agent--parent-stack))))
             
             (callback-id (or (plist-get route-frame :task-id)
                              (bound-and-true-p macher-agent--current-task-id)
                              (buffer-name)))
             
             (meta (list :suppress-patch (bound-and-true-p macher-agent--suppress-patch)
                         :ephemeral (bound-and-true-p macher-agent--is-ephemeral)
                         :background (bound-and-true-p macher-agent--is-background))))
        
        (setq-local macher-agent--task-result final_answer)
        
        (when (and (bound-and-true-p macher-agent--suppress-patch)
                   (macher-agent-valid-context-p context))
          (if (macher-agent-context-p context)
              (setf (macher-agent-context-plugins context)
                    (plist-put (copy-sequence (macher-agent-context-plugins context)) :suppress-patch t))
            (when (macher-agent--plist-p context)
              (plist-put context :suppress-patch t))))
        
        (let* ((raw-msg (list :message final_answer
                              :child-context context
                              :metadata meta))
               (msg-payload (if (fboundp 'macher-agent-run-pipeline)
                                (macher-agent-run-pipeline 'artifact-compose raw-msg)
                              raw-msg))
               (a2a-payload (macher-agent-make-a2a-payload
                             :schema-version (if (boundp 'macher-agent-a2a-schema-version)
                                                 macher-agent-a2a-schema-version
                                               :a2a-v1)
                             :transit-type :subagent-to-parent
                             :type 'ARTIFACT_UPDATE
                             :task-id callback-id
                             :target target-name
                             :target-context (or (when (bound-and-true-p macher-agent--persistent-context)
                                                   macher-agent--persistent-context)
                                                 context)
                             :parent-context (when (bound-and-true-p macher-agent--persistent-context)
                                               macher-agent--persistent-context)
                             :child-context context
                             :shared-state (list :task-id callback-id)
                             :payload msg-payload
                             :message msg-payload
                             :metadata meta)))
          
          (macher-agent-a2a-dispatch (list a2a-payload) nil))
        
        (unless (or (bound-and-true-p macher-agent--routing-stack)
                    (bound-and-true-p macher-agent--parent-stack))
          (setq-local macher-agent-task-finished t)
          (when (bound-and-true-p macher-agent--suppress-patch)
            (setq-local macher-agent--ready-to-reap t)))
        
        final_answer)))
  :success-fn
  (lambda (res _payload)
    (if (and (stringp res) (string-prefix-p "ERROR:" res))
        res
      "SUCCESS: Result submitted. STOP NOW.")))
