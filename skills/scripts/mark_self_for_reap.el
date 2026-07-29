(macher-agent-make-tool
 macher-agent-mark-self-for-reap
 "Marks the current subagent buffer for termination. Call this exactly once when you \
have completely finished your assigned task."
 :category "collaboration"
 :args nil
 :command-fn
 (lambda (_payload _context _root)
   (setq-local macher-agent--ready-to-reap t)
   t)
 :success-fn
 (lambda (_res _payload)
   "Success. This buffer has been marked for reaping and will be terminated shortly. Please finish your \
response."))
