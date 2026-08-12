;;; macher-agent-integration-test.el --- Tests for macher-agent-skills -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'macher-agent-macher)
(require 'macher-agent)
(require 'macher-agent-orchestration)
(require 'macher-agent-vfs)
(let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "helpers" current-dir)))

(require 'macher-agent-test-harness)
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

(describe
 "Macher-Agent Orchestration Integration"
 (before-each
  (setq macher-agent--garbage-queue nil)
  (spy-on 'macher-agent-resolve-context :and-return-value
          (let* ((ws (make-macher-agent-workspace :project-root "/mock/proj"))
                 (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
            (puthash (expand-file-name "/mock/proj") ctx macher-agent-active-workspaces)
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
                     (funcall spawn-fn (lambda (res) (setq final-result (cons 'spawn1 res))) "agent-france")
                     (funcall spawn-fn (lambda (res) (setq final-result (cons 'spawn2 res))) "agent-spain"))
                 (funcall spawn-fn nil "agent-france")
                 (funcall spawn-fn nil "agent-spain")))

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
               
               (cl-letf (((symbol-function 'gptel-send)
                          (lambda ()
                            (let ((cb (bound-and-true-p macher-agent--a2a-callback))
                                  (task-id (bound-and-true-p macher-agent--current-task-id))
                                  (bname (buffer-name)))
                              (when cb
                                (with-current-buffer (get-buffer bname)
                                  (setq-local macher-agent--ready-to-reap t))
                                (funcall cb (list :type 'ARTIFACT_UPDATE
                                                  :task-id task-id
                                                  :message (list :status 'success :buffer-name bname :data (if (string-match-p "france" bname) "The capital of France is Paris." "The capital of Spain is Madrid.")))))))))
                 (funcall delegate-fn
                          (lambda (result)
                            (setq final-result result))
                          :tasks tasks))

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

 (it "routes subagent task execution and artifact updates via point-to-point A2A dispatch"
     (let* ((parent-buf (get-buffer-create "*event-bus-parent*"))
            (results nil))
       (with-current-buffer parent-buf
         (macher-agent-add-subagent "agent-france")
         (cl-letf (((symbol-function 'gptel-send)
                    (lambda ()
                      (let ((cb (bound-and-true-p macher-agent--a2a-callback))
                            (task-id (bound-and-true-p macher-agent--current-task-id)))
                        (when cb
                          (funcall cb (list :type 'ARTIFACT_UPDATE
                                            :task-id task-id
                                            :message (list :status 'success :data "Paris"))))))))
           (macher-agent-a2a-dispatch
            (list (list :type 'SEND_MESSAGE
                        :task-id "fake-task"
                        :metadata (list :buffer_name "agent-france" :presets "@macher-agent-worker")
                        :message (list :instructions "What is the capital of France?")))
            (lambda (res)
              (setq results res))))
         (expect results :to-be-truthy)
         (let ((m (or (plist-get (aref results 0) :message) (aref results 0))))
           (expect (plist-get m :status) :to-equal 'success)
           (expect (plist-get m :data) :to-match "Paris")))))

 (it "handles VFS resource lock acquisition and notification via point-to-point A2A callback"
     (let* ((parent-buf (get-buffer-create "*lock-test-parent*"))
            (lock-result nil)
            (task-id "task-lock-001")
            (resource-path "src/critical-file.el"))
       (with-current-buffer parent-buf
         (puthash task-id (buffer-name parent-buf) macher-agent--task-registry)
         (puthash resource-path (lambda (res) (setq lock-result res)) macher-agent--pending-callbacks)
         (macher-agent--vfs-a2a-callback
          `(:type ACQUIRE_LOCK
                  :task-id ,task-id
                  :metadata (:resource_path ,resource-path)))
         (expect (gethash resource-path macher-agent--vfs-lock-table) :to-equal task-id)
         (expect lock-result :to-equal "Resource lock acquired."))))

 (it "invokes macher-agent--vfs-a2a-callback directly from wait_for_vfs_semaphore"
     (let* ((lock-result nil)
            (resource-path "src/semaphore-test.el")
            (sem-tool (or (gethash "wait_for_vfs_semaphore"
                                   (macher-agent-workspace-tools-registry
                                    (macher-agent--get-context-workspace (macher-agent-resolve-context))))
                          (bound-and-true-p macher-agent-wait-for-vfs-semaphore)))
            (tool-fn (gptel-tool-function sem-tool)))
       (funcall tool-fn
                (lambda (res) (setq lock-result res))
                :path resource-path)
       (expect (gethash resource-path macher-agent--vfs-lock-table) :to-be-truthy)
       (expect lock-result :to-match "Resource lock acquired")))

 (it "verifies removal of global event bus and emit function"
     (expect (fboundp 'macher-agent-emit) :to-be nil)
     (expect (boundp 'macher-agent-workspace-event-bus) :to-be nil)))

(provide 'macher-agent-integration-test)
;;; macher-agent-integration-test.el ends here
