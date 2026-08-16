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
          (when (or (bound-and-true-p macher-agent--is-ephemeral)
                    (bound-and-true-p macher-agent--is-background))
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
      (let ((final_answer (plist-get payload :final_answer)))
        (setq-local macher-agent--task-result final_answer)
        (when (fboundp 'macher-agent-submit-task-result)
          (macher-agent-submit-task-result final_answer context))
        (unless (or (bound-and-true-p macher-agent--routing-stack)
                    (bound-and-true-p macher-agent--parent-stack))
          (setq-local macher-agent-task-finished t)
          (when (or (bound-and-true-p macher-agent--is-background)
                    (bound-and-true-p macher-agent--is-ephemeral))
            (setq-local macher-agent--ready-to-reap t)))
        final_answer)))
  :success-fn
  (lambda (res _payload)
    (if (and (stringp res) (string-prefix-p "ERROR:" res))
        res
      "SUCCESS: Result submitted. STOP NOW.")))
