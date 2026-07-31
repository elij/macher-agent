(macher-agent-make-tool
    macher-agent-submit-task-result-tool
    "Submit the final result of your assigned task back to the orchestrator."
  :category "event"
  :args '((:name "final_answer" :type string :description "The final answer, data, or summary of completed work."))
  :command-fn
  (lambda (payload _context _root)
    (let ((final_answer (plist-get payload :final_answer)))
      
      (setq-local macher-agent--task-result final_answer)
      (setq-local macher-agent-task-finished t)
      
      (when-let* (((boundp 'macher-agent--parent-callback))
                  (callback macher-agent--parent-callback))
        (funcall callback
                 (make-macher-agent-tool-response
                  :status 'success
                  :data final_answer
                  :buffer-name (buffer-name (current-buffer)))))
      final_answer))
  :success-fn (lambda (_res _payload) "SUCCESS: Result submitted. Yielding control."))
