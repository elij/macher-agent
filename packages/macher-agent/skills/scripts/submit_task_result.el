;;; submit_task_result.el --- Submit task result tool -*- lexical-binding: t; -*-

(setq macher-agent-submit-task-result-tool
      (gptel-make-tool
       :name "submit_task_result"
       :description "Submit the final result of your assigned task back to the orchestrator. CRITICAL DIRECTIVE: You MUST use the `submit_task_result` tool to submit your final answer when you are completely finished. Do NOT output your final answer as standard text. IMMEDIATELY STOP after."
       :category "meta"
       :args '((:name "final_answer" :type "string" :description "The final answer, data, or summary of completed work."))
       :async t
       :function (macher-agent-with-presentation-context (final-answer)
                   (macher-agent-submit-task-result final-answer nil context))))
