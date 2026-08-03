(macher-agent-make-tool
    macher-agent-submit-task-result-tool
    "Submit the final result of your assigned task back to the orchestrator. CRITICAL DIRECTIVE: You MUST use the `submit_task_result` tool to submit your final answer when you are completely finished. Do NOT output your final answer as standard text. IMMEDIATELY STOP after."
  :category "event"
  :args '((:name "final_answer" :type string :description "The final answer, data, or summary of completed work."))
  :command-fn
  (lambda (payload _context _root)
    (let* ((final_answer (plist-get payload :final_answer))
           (frame (if (fboundp 'macher-agent--pop-parent)
                      (macher-agent--pop-parent)
                    (list :parent-buffer (bound-and-true-p macher-agent--parent-buffer)
                          :parent-callback (bound-and-true-p macher-agent--parent-callback)
                          :a2a-callback (bound-and-true-p macher-agent--a2a-callback)
                          :task-id (bound-and-true-p macher-agent--current-task-id))))
           (a2a-cb (plist-get frame :a2a-callback))
           (parent-cb (plist-get frame :parent-callback))
           (task-id (or (plist-get frame :task-id) (bound-and-true-p macher-agent--current-task-id))))
      
      (setq-local macher-agent--task-result final_answer)
      (setq-local macher-agent-task-finished t)
      (setq-local macher-agent--ready-to-reap t)
      
      (cond
       (a2a-cb
        (funcall a2a-cb
                 (list :type 'ARTIFACT_UPDATE
                       :task-id task-id
                       :message (list :status 'success
                                      :data final_answer
                                      :buffer-name (buffer-name)))))
       (parent-cb
        (funcall parent-cb
                 (list :status 'success :data final_answer :buffer-name (buffer-name)))))
      
      final_answer))
  :success-fn (lambda (_res _payload) "SUCCESS: Result submitted. STOP NOW."))
