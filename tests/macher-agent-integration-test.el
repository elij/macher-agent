;;; macher-agent-integration-test.el --- Tests for macher-agent-skills -*- lexical-binding: t -*-
(require 'buttercup)
(require 'macher-agent-macher-bridge)
(require 'macher-agent)
(require 'macher-agent-orchestration)

(defvar macher-agent--garbage-queue nil)
(put 'macher-agent--is-subagent 'permanent-local t)
(put 'macher-agent--ready-to-reap 'permanent-local t)

(defun macher-agent--reap-buffers-on-idle ()
  "Reap all buffers that are subagents and marked ready-to-reap."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (bound-and-true-p macher-agent--is-subagent)
                   (bound-and-true-p macher-agent--ready-to-reap))
          (macher-agent--reap-buffer buf))))))

(describe "Macher-Agent Orchestration Integration"
          (before-each
           (setq macher-agent--garbage-queue nil)
           (spy-on 'macher-agent-resolve-context :and-return-value
                   (let ((ctx (macher-agent--make-vfs-context :workspace (cons 'agent (make-macher-agent-workspace :project-root "/mock/proj")) :contents nil)))
                     (macher-agent-initialize-skills ctx (or (bound-and-true-p macher-agent--bundled-skills-dir) macher-agent-bundled-skills-directory))
                     ctx))
           
           ;; 1. Mock the LLM: Intercept gptel-send to act as the AI for the sub-agents
           (spy-on 'gptel-send :and-call-fake
                   (lambda (&rest _)
                     (let ((buf (current-buffer))
                           (name (buffer-name (current-buffer))))
                       (cond
                        ((string-match-p "agent-france" name)
                         (with-current-buffer buf
                           (setq-local macher-agent--is-subagent t)
                           (setq-local macher-agent--ready-to-reap t))
                         (macher-agent-submit-task-result "The capital of France is Paris."))
                        
                        ((string-match-p "agent-spain" name)
                         (with-current-buffer buf
                           (setq-local macher-agent--is-subagent t)
                           (setq-local macher-agent--ready-to-reap t))
                         (macher-agent-submit-task-result "The capital of Spain is Madrid."))))))

           ;; 2. Mock timers: Force deferred buffer cleanups to happen synchronously
           (spy-on 'run-at-time :and-call-fake
                   (lambda (_time _repeat fn &rest args)
                     (apply fn args)))
           (spy-on 'run-with-idle-timer :and-call-fake
                   (lambda (_secs _repeat fn &rest args)
                     (apply fn args))))

          (it "executes the full workflow: spawn -> delegate -> await responses -> return combined result"
              (let* ((master-buf (get-buffer-create "*orchestrator-test*"))
                     (final-result nil))
                
                (with-current-buffer master-buf
                  
                  ;; --- A. Setup the Master Orchestrator Context ---
                  (let ((macher-agent--allow-lazy-init t))
                    (let* ((spawn-tool (or (gethash "spawn_subagent" (macher-agent-workspace-tools-registry (macher-agent--get-context-workspace (macher-agent-resolve-context))))
                                           (bound-and-true-p macher-agent-spawn-subagent-tool)))
                           (delegate-tool (or (gethash "delegate_tasks_to_subagents" (macher-agent-workspace-tools-registry (macher-agent--get-context-workspace (macher-agent-resolve-context))))
                                              (bound-and-true-p macher-agent-delegate-tasks-to-subagents-tool))))
                      
                      ;; --- B. Spawn Sub-agents via Tool ---
                      (let ((spawn-fn (gptel-tool-function spawn-tool)))
                        (if (gptel-tool-async spawn-tool)
                            (progn
                              (funcall spawn-fn "agent-france" (lambda (res) (setq final-result (cons 'spawn1 res))))
                              (funcall spawn-fn "agent-spain" (lambda (res) (setq final-result (cons 'spawn2 res)))))
                          (funcall spawn-fn "agent-france")
                          (funcall spawn-fn "agent-spain")))

                      (unless (buffer-live-p (get-buffer "agent-france"))
                        (error "SPAWN FAILED! final-result=%S spawn-tool=%S" final-result spawn-tool))
                      
                      (expect (buffer-live-p (get-buffer "agent-france")) :to-be t)
                      (expect (buffer-live-p (get-buffer "agent-spain")) :to-be t)

                      ;; --- C. Delegate Tasks via Tool ---
                      (let ((tasks (vector
                                    (list :buffer_name "agent-france"
                                          :instructions "What is the capital of France?"
                                          :preset "@macher-agent-worker")
                                    (list :buffer_name "agent-spain"
                                          :instructions "What is the capital of Spain?"
                                          :preset "@macher-agent-worker")))
                            (delegate-fn (gptel-tool-function delegate-tool)))
                        
                        (funcall delegate-fn
                                 tasks
                                 (lambda (result)
                                   (setq final-result result))))

                      ;; --- D. Assertions ---
                      (expect final-result :to-be-truthy)
                      
                      (expect final-result :to-match "=== Response from agent-france ===")
                      (expect final-result :to-match "The capital of France is Paris.")
                      
                      (expect final-result :to-match "=== Response from agent-spain ===")
                      (expect final-result :to-match "The capital of Spain is Madrid.")
                      
                      ;; --- E. Reaper Invocation ---
                      (macher-agent--reap-buffers-on-idle)
                      (expect (buffer-live-p (get-buffer "agent-france")) :to-be nil)
                      (expect (buffer-live-p (get-buffer "agent-spain")) :to-be nil)))))))
