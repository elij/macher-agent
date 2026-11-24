;;; test-a2a.el --- Agent-to-agent orchestration, focus semantics, and reaping -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Specification 6: Agent-to-agent (A2A) orchestration, focus semantics, and reaping
;; A2A-01, A2A-02, A2A-03, A2A-04, A2A-05, A2A-06, A2A-07, A2A-08, A2A-09, A2A-10, A2A-11, A2A-12, A2A-13, A2A-14, A2A-15, A2A-16, A2A-17, A2A-18, A2A-19

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name default-directory)))
(require 'macher-agent-test-helpers)

(describe "6. Agent-to-agent (A2A) orchestration, focus semantics, and reaping"

          (before-each
           (macher-agent-test-reset-env))

          ;; --------------------------------------------------------------------------
          ;; A2A-01
          ;; --------------------------------------------------------------------------
          (it "A2A-01: delegates tasks via delegate_tasks_to_subagents with routing stack and ordered results"
              (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
                                                    (let* ((tasks (vector (list :buffer_name "*a2a-01-worker-1*" :presets ["default"] :instructions "Compute 2+2")
                                                                          (list :buffer_name "*a2a-01-worker-2*" :presets ["default"] :instructions "Compute 3*3")))
                                                           (aggregated-result nil))
                                                      (with-current-buffer parent-buf
                                                        (cl-letf (((symbol-function 'gptel-send)
                                                                   (lambda ()
                                                                     (let* ((cur-buf (current-buffer))
                                                                            (tid macher-agent-current-task-id)
                                                                            (ans (if (string-match-p "worker-1" (buffer-name cur-buf)) "Answer: 4" "Answer: 9")))
                                                                       ;; Verify sub-agent has routing frame pushed and receives submit_task_result and directive
                                                                       (expect (length macher-agent-routing-stack) :to-equal 1)
                                                                       (expect (plist-get (car macher-agent-routing-stack) :originator-name)
                                                                               :to-equal (buffer-name parent-buf))
                                                                       (let ((tx-state (macher-agent-run-pipeline
                                                                                        'transmission
                                                                                        (make-macher-agent-transmission-state
                                                                                         :target-buffer cur-buf
                                                                                         :tools (when (boundp 'gptel-tools) gptel-tools)))))
                                                                         (expect (cl-some (lambda (tl) (equal (macher-agent-canonical-tool-name tl) "submit_task_result"))
                                                                                          (macher-agent-transmission-state-tools tx-state))
                                                                                 :to-be-truthy)
                                                                         (expect (macher-agent-transmission-state-compiled-prompt tx-state)
                                                                                 :to-match "`submit_task_result`"))
                                                                       ;; Call submit_task_result
                                                                       (macher-agent-submit-task-result ans tid macher-agent-persistent-context)))))
                                                          ;; Execute delegation
                                                          (macher-agent-delegate-tasks
                                                           tasks
                                                           (lambda (res) (setq aggregated-result res))
                                                           parent-ctx)))

                                                      ;; Await aggregated results in exact order of input vector
                                                      (macher-agent-test-wait-until (lambda () aggregated-result) 5.0)
                                                      (expect (stringp aggregated-result) :to-be t)
                                                      (expect aggregated-result :to-match "Answer: 4")
                                                      (expect aggregated-result :to-match "Answer: 9")
                                                      (expect (string-match-p "Answer: 4" aggregated-result)
                                                              :to-be-less-than
                                                              (string-match-p "Answer: 9" aggregated-result)))))

          ;; --------------------------------------------------------------------------
          ;; A2A-02
          ;; --------------------------------------------------------------------------
          (it "A2A-02: dispatches tasks via execute_subagents immediately with empty routing stacks"
              (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
                                                    (let* ((tasks (vector (list :buffer_name "*a2a-02-bg-1*" :presets ["default"] :instructions "Background 1")
                                                                          (list :buffer_name "*a2a-02-bg-2*" :presets ["default"] :instructions "Background 2")))
                                                           (dispatch-resolved nil))
                                                      (with-current-buffer parent-buf
                                                        (cl-letf (((symbol-function 'gptel-send) #'ignore))
                                                          (macher-agent-execute-background-subagents
                                                           tasks
                                                           (lambda (res) (setq dispatch-resolved res))
                                                           parent-ctx)))

                                                      (expect dispatch-resolved :not :to-be nil)
                                                      (expect (stringp dispatch-resolved) :to-be t)
                                                      (expect dispatch-resolved :to-match "SUCCESS: Background sub-agent '\\*a2a-02-bg-1\\*' dispatched")
                                                      (expect dispatch-resolved :to-match "SUCCESS: Background sub-agent '\\*a2a-02-bg-2\\*' dispatched")

                                                      ;; Sub-agents have empty routing stacks and do not have submit_task_result in tool lists
                                                      (let ((bg-buf1 (get-buffer "*a2a-02-bg-1*"))
                                                            (bg-buf2 (get-buffer "*a2a-02-bg-2*")))
                                                        (expect (buffer-live-p bg-buf1) :to-be t)
                                                        (expect (buffer-live-p bg-buf2) :to-be t)
                                                        (with-current-buffer bg-buf1
                                                          (expect macher-agent-routing-stack :to-be nil)
                                                          (expect (cl-some (lambda (tl) (equal (gptel-tool-name tl) "submit_task_result"))
                                                                           gptel-tools)
                                                                  :to-be nil))
                                                        (with-current-buffer bg-buf2
                                                          (expect macher-agent-routing-stack :to-be nil)
                                                          (expect (cl-some (lambda (tl) (equal (gptel-tool-name tl) "submit_task_result"))
                                                                           gptel-tools)
                                                                  :to-be nil))))))

          ;; --------------------------------------------------------------------------
          ;; A2A-03
          ;; --------------------------------------------------------------------------
          (it "A2A-03: invokes macher-agent-display-subagent-fn only for foreground subagents"
              (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
                                                    (let* ((displayed-buffers nil)
                                                           (recorder (lambda (b) (push (buffer-name b) displayed-buffers)))
                                                           (macher-agent-display-subagent-fn recorder)
                                                           (fg-buf (get-buffer-create "*a2a-03-fg*"))
                                                           (bg-buf (get-buffer-create "*a2a-03-bg*")))
                                                      (unwind-protect
                                                          (progn
                                                            ;; Dispatch foreground sub-agent (:background nil)
                                                            (macher-agent-a2a-pipe--acquire-target
                                                             (list :a2a-msg (make-macher-agent-transit-payload :metadata '(:background nil))
                                                                   :target (buffer-name fg-buf)
                                                                   :target-name (buffer-name fg-buf)
                                                                   :shared-state (list :parent-buffer parent-buf)))
                                                            ;; Dispatch background sub-agent (:background t)
                                                            (macher-agent-a2a-pipe--acquire-target
                                                             (list :a2a-msg (make-macher-agent-transit-payload :metadata '(:background t))
                                                                   :target (buffer-name bg-buf)
                                                                   :target-name (buffer-name bg-buf)
                                                                   :shared-state (list :parent-buffer parent-buf)))
                                                            ;; Display function called for foreground, but not for background
                                                            (expect (member "*a2a-03-fg*" displayed-buffers) :to-be-truthy)
                                                            (expect (member "*a2a-03-bg*" displayed-buffers) :to-be nil))
                                                        (when (buffer-live-p fg-buf) (kill-buffer fg-buf))
                                                        (when (buffer-live-p bg-buf) (kill-buffer bg-buf))))))

          ;; --------------------------------------------------------------------------
          ;; A2A-04
          ;; --------------------------------------------------------------------------
          (it "A2A-04: signals contract violation error when submit_task_result is invoked without task-id or routing stack"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (progn
                                                      ;; In root orchestrator buffer: routing-stack and current-task-id are empty
                                                      (with-current-buffer owner-buf
                                                        (setq-local macher-agent-routing-stack nil)
                                                        (setq-local macher-agent-current-task-id nil)
                                                        (expect (macher-agent-submit-task-result "Premature answer" nil ctx)
                                                                :to-throw 'error)
                                                        (expect (macher-agent-test-invoke-tool
                                                                 macher-agent-submit-task-result-tool
                                                                 "Premature answer")
                                                                :to-match "^ERROR:"))

                                                      ;; Inside a sub-agent launched with :background t
                                                      (let ((bg-buf (generate-new-buffer "*a2a-04-bg-orphan*")))
                                                        (unwind-protect
                                                            (with-current-buffer bg-buf
                                                              (setq-local macher-agent-routing-stack nil)
                                                              (setq-local macher-agent-current-task-id nil)
                                                              (expect (macher-agent-submit-task-result "Orphan answer" nil nil)
                                                                      :to-throw 'error))
                                                          (when (buffer-live-p bg-buf) (kill-buffer bg-buf)))))))

          ;; --------------------------------------------------------------------------
          ;; A2A-05
          ;; --------------------------------------------------------------------------
          (it "A2A-05: detects circular routing and halts transmission with error payload"
              (let* ((parent-name "*a2a-05-circular-parent*")
                     (parent-buf (get-buffer-create parent-name))
                     (msg (make-macher-agent-transit-payload
                           :target-buffer parent-name
                           :metadata (list :buffer_name parent-name)))
                     (state (list :a2a-msg msg
                                  :shared-state (list :parent-buffer parent-buf))))
                (unwind-protect
                    (let* ((res (macher-agent-a2a-pipe--validate-routing state))
                           (err-payload (plist-get res :error-payload)))
                      (expect err-payload :not :to-be nil)
                      (expect (plist-get err-payload :status) :to-equal 'error)
                      (expect (plist-get err-payload :error) :to-match "Circular routing detected"))
                  (when (buffer-live-p parent-buf) (kill-buffer parent-buf)))))

          ;; --------------------------------------------------------------------------
          ;; A2A-06
          ;; --------------------------------------------------------------------------
          (it "A2A-06: assigns fresh unique UUID when task-id collides with existing registered callback"
              (let* ((existing-id "task-collision-id-100")
                     (child-buf (get-buffer-create "*a2a-06-child*"))
                     (state (list :a2a-msg (make-macher-agent-transit-payload :task-id existing-id)
                                  :child-buf child-buf
                                  :shared-state (list :results (make-hash-table :test 'equal)
                                                      :total 1
                                                      :final-callback nil
                                                      :parent-buffer (current-buffer)
                                                      :original-payloads nil))))
                (unwind-protect
                    (progn
                      ;; Pre-register the existing ID
                      (puthash existing-id #'ignore macher-agent-task-callbacks)
                      (let* ((bound-state (macher-agent-a2a-pipe--bind-closure state))
                             (msg (plist-get bound-state :a2a-msg))
                             (assigned-id (macher-agent-transit-payload-task-id msg)))
                        ;; Pipeline assigns a fresh unique UUID
                        (expect assigned-id :not :to-equal existing-id)
                        (expect (string-prefix-p "task-" assigned-id) :to-be t)
                        ;; Existing callback is preserved
                        (expect (gethash existing-id macher-agent-task-callbacks) :to-equal #'ignore)))
                  (remhash existing-id macher-agent-task-callbacks)
                  (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

          ;; --------------------------------------------------------------------------
          ;; A2A-07
          ;; --------------------------------------------------------------------------
          (it "A2A-07: triggers parent error callback when awaited sub-agent halts without submitting result"
              (let* ((task-id "task-halt-07")
                     (child-buf (get-buffer-create "*a2a-07-child*"))
                     (error-received nil))
                (unwind-protect
                    (progn
                      (puthash task-id (lambda (res) (setq error-received res)) macher-agent-pending-callbacks)
                      (with-current-buffer child-buf
                        (setq-local macher-agent-current-task-id task-id)
                        (setq-local macher-agent-task-finished nil)
                        (setq-local macher-agent-is-ephemeral t)
                        ;; Simulate LLM turn response completing without invoking submit_task_result
                        (macher-agent-post-response-reaper (point-min) (point-max)))

                      ;; Parent error callback is invoked with failure notification
                      (expect error-received :not :to-be nil)
                      (expect (plist-get error-received :status) :to-equal 'error)
                      (expect (plist-get error-received :error) :to-match "Agent halted without invoking submit_task_result\\|Sub-agent halted before submitting task result")
                      ;; Pending callback is cleaned up
                      (expect (gethash task-id macher-agent-pending-callbacks) :to-be nil))
                  (when (buffer-live-p child-buf) (kill-buffer child-buf)))))

          ;; --------------------------------------------------------------------------
          ;; A2A-08
          ;; --------------------------------------------------------------------------
          (it "A2A-08: kills ephemeral sub-agent upon turn completion while persistent sub-agent survives"
              (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
                                                    (let* ((eph-buf (macher-agent-add-subagent "*a2a-08-ephemeral*" nil parent-buf nil parent-ctx))
                                                           (pers-buf (macher-agent-add-subagent "*a2a-08-persistent*" nil parent-buf nil parent-ctx)))
                                                      ;; Configure ephemeral sub-agent
                                                      (with-current-buffer eph-buf
                                                        (setq-local macher-agent-is-ephemeral t)
                                                        (setq-local macher-agent-task-finished t)
                                                        (setq-local macher-agent-ready-to-reap t))
                                                      ;; Configure persistent sub-agent
                                                      (with-current-buffer pers-buf
                                                        (setq-local macher-agent-is-ephemeral nil)
                                                        (setq-local macher-agent-task-finished t)
                                                        (setq-local macher-agent-ready-to-reap nil))

                                                      ;; Register in active subagents and ownership
                                                      (setq macher-agent-active-subagents
                                                            (list (cons (buffer-name eph-buf) root)
                                                                  (cons (buffer-name pers-buf) root)))
                                                      (puthash (buffer-name parent-buf)
                                                               (list (buffer-name eph-buf) (buffer-name pers-buf))
                                                               macher-agent-a2a-ownership)

                                                      ;; Trigger reaping on both
                                                      (macher-agent-reap-buffer eph-buf)
                                                      (macher-agent-reap-buffer pers-buf)

                                                      ;; Ephemeral sub-agent is killed and cleaned from ownership
                                                      (expect (buffer-live-p eph-buf) :to-be nil)
                                                      (expect (assoc (buffer-name eph-buf) macher-agent-active-subagents) :to-be nil)
                                                      (expect (member "*a2a-08-ephemeral*"
                                                                      (gethash (buffer-name parent-buf) macher-agent-a2a-ownership))
                                                              :to-be nil)

                                                      ;; Persistent sub-agent buffer remains live and registered
                                                      (expect (buffer-live-p pers-buf) :to-be t)
                                                      (expect (assoc "*a2a-08-persistent*" macher-agent-active-subagents) :not :to-be nil)
                                                      (kill-buffer pers-buf))))

          ;; --------------------------------------------------------------------------
          ;; A2A-09
          ;; --------------------------------------------------------------------------
          (it "A2A-09: explicitly reaps persistent sub-agent when marked ready-to-reap"
              (let ((pers-buf (get-buffer-create "*a2a-09-pers*"))
                    (parent-name "*a2a-09-parent*"))
                (unwind-protect
                    (progn
                      (with-current-buffer pers-buf
                        (setq-local macher-agent-is-ephemeral nil)
                        (setq-local macher-agent-task-finished t)
                        ;; Explicitly mark as reapable
                        (setq-local macher-agent-ready-to-reap t))
                      (setq macher-agent-active-subagents (list (cons "*a2a-09-pers*" "/tmp")))
                      (puthash parent-name (list "*a2a-09-pers*") macher-agent-a2a-ownership)

                      ;; Call reaper
                      (macher-agent-reap-buffer pers-buf)

                      ;; Buffer is killed and cleaned from all registries regardless of ephemeral flag
                      (expect (buffer-live-p pers-buf) :to-be nil)
                      (expect (assoc "*a2a-09-pers*" macher-agent-active-subagents) :to-be nil)
                      (expect (gethash parent-name macher-agent-a2a-ownership) :to-equal nil))
                  (when (buffer-live-p pers-buf) (kill-buffer pers-buf)))))

          ;; --------------------------------------------------------------------------
          ;; A2A-10
          ;; --------------------------------------------------------------------------
          (it "A2A-10: rejects sub-agent task batch containing duplicate target buffer names"
              (let* ((payload-1 (make-macher-agent-transit-payload
                                 :transit-type :task-delegation
                                 :type 'TASK_DELEGATION
                                 :task-id "task-dup-1"
                                 :metadata (list :buffer_name "*a2a-worker-dup*")))
                     (payload-2 (make-macher-agent-transit-payload
                                 :transit-type :task-delegation
                                 :type 'TASK_DELEGATION
                                 :task-id "task-dup-2"
                                 :metadata (list :buffer_name "*a2a-worker-dup*")))
                     (batch (vector payload-1 payload-2))
                     (dispatch-result nil))
                (macher-agent-a2a-dispatch
                 batch
                 (lambda (res)
                   (setq dispatch-result res)))
                (expect dispatch-result :to-match "ERROR: Duplicate target buffer in sub-agent batch.")))

          ;; --------------------------------------------------------------------------
          ;; A2A-11
          ;; --------------------------------------------------------------------------
          (it "A2A-11: rejects delegation to target sub-agent buffer already running a task"
              (let* ((busy-buf (get-buffer-create "*a2a-busy-worker*"))
                     (task-id "task-busy-01")
                     (ctx (macher-agent-make-context :id "busy-ctx"))
                     (session (macher-agent-context-session ctx)))
                (unwind-protect
                    (progn
                      (setf (macher-agent-session-state session) :running)
                      (with-current-buffer busy-buf
                        (setq-local macher-agent-persistent-context ctx))
                      (let* ((msg (make-macher-agent-transit-payload
                                   :transit-type :task-delegation
                                   :type 'TASK_DELEGATION
                                   :task-id task-id
                                   :metadata (list :buffer_name (buffer-name busy-buf))))
                             (state (list :a2a-msg msg
                                          :shared-state (list :parent-buffer (current-buffer))))
                             (res (macher-agent-a2a-pipe--validate-routing state))
                             (err-payload (plist-get res :error-payload)))
                        (expect err-payload :not :to-be nil)
                        (expect (plist-get err-payload :status) :to-equal 'error)
                        (expect (plist-get err-payload :error) :to-match "already busy running a task")))
                  (when (buffer-live-p busy-buf) (kill-buffer busy-buf)))))

          ;; --------------------------------------------------------------------------
          ;; A2A-12
          ;; --------------------------------------------------------------------------
          (it "A2A-12: unwinds all routing stack frames when sub-agent halts prematurely"
              (let* ((task-id-1 "nested-task-parent")
                     (task-id-2 "nested-task-child")
                     (subagent-buf (get-buffer-create "*a2a-nested-subagent*"))
                     (errors-received nil))
                (unwind-protect
                    (progn
                      ;; Register callbacks for both nested callers
                      (puthash task-id-1
                               (lambda (res) (push (cons task-id-1 res) errors-received))
                               macher-agent-pending-callbacks)
                      (puthash task-id-2
                               (lambda (res) (push (cons task-id-2 res) errors-received))
                               macher-agent-pending-callbacks)
                      (with-current-buffer subagent-buf
                        ;; Push two routing frames
                        (macher-agent-push-routing task-id-1 "*parent-caller-1*" t)
                        (macher-agent-push-routing task-id-2 "*parent-caller-2*" t)
                        (setq-local macher-agent-task-finished nil)
                        (setq-local macher-agent-is-ephemeral nil)
                        ;; Trigger premature halt reaper
                        (macher-agent-post-response-reaper (point-min) (point-max)))

                      ;; Both callbacks were invoked and routing stack is completely empty
                      (expect (length errors-received) :to-equal 2)
                      (expect (with-current-buffer subagent-buf macher-agent-routing-stack) :to-equal nil)
                      (expect (with-current-buffer subagent-buf macher-agent-current-task-id) :to-equal nil)
                      (let ((err1 (cdr (assoc task-id-1 errors-received)))
                            (err2 (cdr (assoc task-id-2 errors-received))))
                        (expect (plist-get err1 :status) :to-equal 'error)
                        (expect (plist-get err2 :status) :to-equal 'error)
                        (expect (plist-get err1 :error) :to-match "Sub-agent halted before submitting task result")
                        (expect (plist-get err2 :error) :to-match "Sub-agent halted before submitting task result")))
                  (when (buffer-live-p subagent-buf) (kill-buffer subagent-buf)))))

          ;; --------------------------------------------------------------------------
          ;; A2A-13
          ;; --------------------------------------------------------------------------
          (it "A2A-13: preserves sub-agent header directive, fires ui-show once, routes TRET->DONE with nil tracking-marker, returns PTC list, and reaps ephemeral worker"
              (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
                                                    (let* ((worker-sys "You are the dedicated worker preset.")
                                                           (ui-show-calls 0)
                                                           (wait-entries 0)
                                                           (done-entries 0)
                                                           (header-directive-matched nil)
                                                           (ptc-result nil)
                                                           (ptc-error nil))
                                                      (with-current-buffer parent-buf
                                                        (setq-local gptel--known-presets
                                                                    (cons (cons 'worker-preset (list :system worker-sys :description "Worker"))
                                                                          gptel--known-presets))
                                                        (setq-local gptel-directives
                                                                    (cons (cons 'worker-preset worker-sys)
                                                                          gptel-directives)))
                                                      (let ((macher-agent-display-subagent-fn
                                                             (lambda (_buf) (setq ui-show-calls (1+ ui-show-calls)))))
                                                        (cl-letf (((symbol-function 'gptel-send)
                                                                   (lambda ()
                                                                     (let* ((child-buf (current-buffer))
                                                                            (start-m (copy-marker (point-max)))
                                                                            (track-m (copy-marker (point-max) t))
                                                                            (info (list :buffer child-buf
                                                                                        :backend (or gptel-backend
                                                                                                     (bound-and-true-p gptel--openai)
                                                                                                     (gptel-make-openai "test-openai" :stream nil))
                                                                                        :data (list :messages (vector))
                                                                                        :position start-m
                                                                                        :tracking-marker track-m
                                                                                        :include-tool-results t
                                                                                        :callback #'ignore
                                                                                        :tool-result (list (list nil nil "SUCCESS: Result submitted."))
                                                                                        :tool-use (list (list :id "call_1" :name "submit_task_result" :result "SUCCESS: Result submitted."))))
                                                                            (fsm (gptel-make-fsm
                                                                                  :state 'TOOL
                                                                                  :table (copy-alist gptel-send--transitions)
                                                                                  :handlers (list (cons 'WAIT (list (lambda (_f) (setq wait-entries (1+ wait-entries)))))
                                                                                                  (cons 'TRET (list #'gptel--handle-tool-result))
                                                                                                  (cons 'DONE (list #'gptel--handle-post-insert
                                                                                                                    #'gptel--fsm-last
                                                                                                                    (lambda (_f) (setq done-entries (1+ done-entries))))))
                                                                                  :info info)))
                                                                       ;; Verify sub-agent buffer's gptel-system-prompt matches gptel-directives for header line
                                                                       (setq header-directive-matched
                                                                             (car-safe (rassoc gptel-system-prompt gptel-directives)))
                                                                       (macher-agent-fsm-hijack-transform fsm)
                                                                       ;; Run submit_task_result while child FSM is in TOOL state (as inside PTC/tool execution)
                                                                       (run-at-time
                                                                        0.01 nil
                                                                        (lambda ()
                                                                          (when (buffer-live-p child-buf)
                                                                            (with-current-buffer child-buf
                                                                              (macher-agent-submit-task-result
                                                                               "Worker-Report-42" nil macher-agent-persistent-context)
                                                                              ;; Complete TOOL -> TRET (sets :tracking-marker nil) -> DONE transition
                                                                              (gptel--fsm-transition fsm)))))))))
                                                          (with-current-buffer parent-buf
                                                            (macher-agent-delegate-tasks
                                                             (vector (list :buffer_name "*a2a-13-worker*"
                                                                           :presets ["worker-preset"]
                                                                           :instructions "Run check"))
                                                             (lambda (answers)
                                                               (setq ptc-result (format "ORCH:%s" (car answers))))
                                                             parent-ctx
                                                             t)))
                                                        (macher-agent-test-wait-until (lambda () (or ptc-result ptc-error)) 5.0)
                                                        (expect ptc-error :to-be nil)
                                                        (expect ptc-result :to-equal "ORCH:Worker-Report-42")
                                                        (expect header-directive-matched :to-equal 'worker-preset)
                                                        (expect ui-show-calls :to-equal 1)
                                                        (expect wait-entries :to-equal 0)
                                                        (expect done-entries :to-equal 1)
                                                        ;; Ephemeral sub-agent buffer is reaped on timer after DONE even though TRET cleared :tracking-marker
                                                        (macher-agent-test-wait-until (lambda () (null (get-buffer "*a2a-13-worker*"))) 5.0)
                                                        (expect (get-buffer "*a2a-13-worker*") :to-be nil)))))

          ;; --------------------------------------------------------------------------
          ;; A2A-14
          ;; --------------------------------------------------------------------------
          (it "A2A-14: reaps spawn_subagent and nil-preset workers upon task completion while keeping halted/errored workers open"
              (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
                                                    (unwind-protect
                                                        (with-current-buffer parent-buf
                                                          (let* ((spawned-buf (macher-agent-add-subagent "*a2a-14-spawned*" nil parent-buf (macher-agent-context-project-root parent-ctx) parent-ctx))
                                                                 (spawned-name (buffer-name spawned-buf))
                                                                 (deleg-res nil)
                                                                 (halt-res nil))
                                                            (with-current-buffer spawned-buf
                                                              (setq-local macher-agent-is-ephemeral nil))
                                                            (expect (buffer-live-p (get-buffer spawned-name)) :to-be t)
                                                            ;; 1. Delegating to both a spawn_subagent buffer and a nil-preset buffer reaps both upon completion
                                                            (cl-letf (((symbol-function 'gptel-send)
                                                                       (lambda ()
                                                                         (macher-agent-submit-task-result "Done-OK" nil macher-agent-persistent-context)
                                                                         (macher-agent-flush-sweep-buffer (current-buffer)))))
                                                              (macher-agent-delegate-tasks
                                                               (vector (list :buffer_name spawned-name :instructions "Task A")
                                                                       (list :buffer_name "*a2a-14-nil-preset*" :instructions "Task B"))
                                                               (lambda (r) (setq deleg-res r))
                                                               parent-ctx))
                                                            (macher-agent-test-wait-until (lambda () deleg-res) 5.0)
                                                            (macher-agent-test-wait-until
                                                             (lambda () (and (null (get-buffer spawned-name))
                                                                             (null (get-buffer "*a2a-14-nil-preset*"))))
                                                             5.0)
                                                            (expect (get-buffer spawned-name) :to-be nil)
                                                            (expect (get-buffer "*a2a-14-nil-preset*") :to-be nil)

                                                            ;; 2. Halted worker (finishes without submit_task_result) notifies parent but stays open for inspection
                                                            (cl-letf (((symbol-function 'gptel-send)
                                                                       (lambda ()
                                                                         (macher-agent-flush-sweep-buffer (current-buffer)))))
                                                              (macher-agent-delegate-tasks
                                                               (vector (list :buffer_name "*a2a-14-halted*" :instructions "Task C"))
                                                               (lambda (r) (setq halt-res r))
                                                               parent-ctx))
                                                            (macher-agent-test-wait-until (lambda () halt-res) 5.0)
                                                            (sleep-for 0.05)
                                                            (expect halt-res :to-match "halted")
                                                            (expect (buffer-live-p (get-buffer "*a2a-14-halted*")) :to-be t)))
                                                      (dolist (bn '("*a2a-14-spawned*" "*a2a-14-nil-preset*" "*a2a-14-halted*"))
                                                        (when-let* ((b (get-buffer bn)))
                                                          (kill-buffer b))))))

          ;; --------------------------------------------------------------------------
          ;; A2A-15
          ;; --------------------------------------------------------------------------
          (it "A2A-15: formats submit_task_result directive with backticks and compiles it at final footer position"
              (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
                                                    (let ((subagent-buf (get-buffer-create "*test-a2a-15*")))
                                                      (unwind-protect
                                                          (with-current-buffer subagent-buf
                                                            (setq-local gptel-system-prompt "Base system instructions.")
                                                            (setq-local macher-agent-boot-directive "Initial boot directive.")
                                                            (macher-agent-push-routing "task-a2a-15" (buffer-name parent-buf) t)
                                                            (setq-local gptel-tools (list macher-agent-submit-task-result-tool))
                                                            (let* ((tools (list macher-agent-submit-task-result-tool))
                                                                   (state (make-macher-agent-transmission-state
                                                                           :target-buffer subagent-buf
                                                                           :directives '("<available_skills>\nSome skills\n</available_skills>")
                                                                           :tools tools))
                                                                   (tx-state (macher-agent-run-pipeline 'transmission state))
                                                                   (compiled (macher-agent-transmission-state-compiled-prompt tx-state)))
                                                              ;; 1. Directive contains markdown backticks `submit_task_result`
                                                              (expect compiled :to-match "CRITICAL DIRECTIVE: You MUST use the `submit_task_result` tool")
                                                              ;; 2. Directive does not use old Lisp quote `submit_task_result'
                                                              (expect compiled :not :to-match "the `submit_task_result' tool")
                                                              ;; 3. submit_task_result directive appears at the end after <available_skills>
                                                              (let ((skills-pos (string-match "<available_skills>" compiled))
                                                                    (submit-pos (string-match "CRITICAL DIRECTIVE: You MUST use the `submit_task_result` tool" compiled)))
                                                                (expect skills-pos :not :to-be nil)
                                                                (expect submit-pos :not :to-be nil)
                                                                (expect submit-pos :to-be-greater-than skills-pos))))
                                                        (when (buffer-live-p subagent-buf) (kill-buffer subagent-buf))))))

          ;; --------------------------------------------------------------------------
          ;; A2A-16
          ;; --------------------------------------------------------------------------
          (it "A2A-16: triggers corrective recovery on first plain-text miss and succeeds when second turn submits result"
              (let ((send-count 0)
                    (deleg-res nil)
                    (recovery-buf-text nil)
                    (gptel-prompt-prefix-alist '((fundamental-mode . "### ")
                                                 (text-mode . "### ")
                                                 (markdown-mode . "### ")
                                                 (org-mode . "### "))))
                (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
                                                      (unwind-protect
                                                          (with-current-buffer parent-buf
                                                            (cl-letf (((symbol-function 'gptel-send)
                                                                       (lambda ()
                                                                         (setq send-count (1+ send-count))
                                                                         (let* ((cur (current-buffer))
                                                                                (beg (point-max)))
                                                                           (if (= send-count 1)
                                                                               ;; Turn 1: model returns plain text without calling submit_task_result
                                                                               (progn
                                                                                 (goto-char (point-max))
                                                                                 (insert "\nAssistant: Preliminary answer in plain text.\n")
                                                                                 (let ((end (point-max)))
                                                                                   (put-text-property beg end 'gptel 'response)
                                                                                   (run-hook-with-args 'gptel-post-response-functions beg end)))
                                                                             ;; Turn 2: model invokes submit_task_result
                                                                             (setq recovery-buf-text (with-current-buffer cur (buffer-string)))
                                                                             (let ((tid macher-agent-current-task-id)
                                                                                   (ctx macher-agent-persistent-context))
                                                                               (goto-char (point-max))
                                                                               (insert "\nAssistant: Submitting final result.\n")
                                                                               (let ((end (point-max)))
                                                                                 (put-text-property beg end 'gptel 'response)
                                                                                 (macher-agent-submit-task-result "Recovery Answer 42" tid ctx)
                                                                                 (run-hook-with-args 'gptel-post-response-functions beg end))))))))
                                                              (macher-agent-delegate-tasks
                                                               (vector (list :buffer_name "*a2a-16-worker*" :instructions "Compute 42"))
                                                               (lambda (r) (setq deleg-res r))
                                                               parent-ctx)
                                                              (macher-agent-test-wait-until (lambda () deleg-res) 5.0)
                                                              ;; 1. gptel-send was invoked twice (initial turn + recovery turn)
                                                              (expect send-count :to-equal 2)
                                                              ;; 2. Recovery prompt avoids double-space after trailing-space prefix ("### ")
                                                              (expect recovery-buf-text :to-match "^### CRITICAL:")
                                                              (expect recovery-buf-text :not :to-match "###  CRITICAL:")
                                                              ;; 3. Aggregated result received the recovery answer
                                                              (expect deleg-res :to-match "Recovery Answer 42")
                                                              ;; 4. Ephemeral worker was cleanly reaped upon completion
                                                              (macher-agent-test-wait-until (lambda () (null (get-buffer "*a2a-16-worker*"))) 5.0)
                                                              (expect (get-buffer "*a2a-16-worker*") :to-be nil)
                                                              ;; 5. Verify prefix spacing for trailing-space, non-trailing-space, and empty prefixes
                                                              (dolist (spec '(("### " . "^### CRITICAL:")
                                                                              (">"    . "^> CRITICAL:")
                                                                              (""     . "^CRITICAL:")))
                                                                (with-temp-buffer
                                                                  (insert "Preliminary plain text.\n")
                                                                  (setq-local macher-agent-current-task-id "task-a2a-16-prefix")
                                                                  (setq-local macher-agent-task-finished nil)
                                                                  (let* ((gptel-prompt-prefix-alist (list (cons major-mode (car spec))))
                                                                         (captured nil)
                                                                         (hook-fn (macher-agent-make-transmit-response-hook #'ignore)))
                                                                    (cl-letf (((symbol-function 'gptel-send)
                                                                               (lambda () (setq captured (buffer-string)))))
                                                                      (funcall hook-fn (point-min) (point-max)))
                                                                    (expect captured :to-match (cdr spec))
                                                                    (expect captured :not :to-match "###  CRITICAL:"))))))
                                                        (when-let* ((b (get-buffer "*a2a-16-worker*")))
                                                          (kill-buffer b))))))

          ;; --------------------------------------------------------------------------
          ;; A2A-17
          ;; --------------------------------------------------------------------------
          (it "A2A-17: aborts on second consecutive plain-text miss, clears recovery pending, and unwinds routing frames"
              (let ((send-count 0)
                    (deleg-res nil))
                (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
                                                      (unwind-protect
                                                          (with-current-buffer parent-buf
                                                            (cl-letf (((symbol-function 'gptel-send)
                                                                       (lambda ()
                                                                         (setq send-count (1+ send-count))
                                                                         (let* ((cur (current-buffer))
                                                                                (beg (point-max)))
                                                                           (goto-char (point-max))
                                                                           (insert (format "\nAssistant: Plain text miss %d\n" send-count))
                                                                           (let ((end (point-max)))
                                                                             (put-text-property beg end 'gptel 'response)
                                                                             (run-hook-with-args 'gptel-post-response-functions beg end))))))
                                                              (macher-agent-delegate-tasks
                                                               (vector (list :buffer_name "*a2a-17-worker*" :instructions "Fail to submit"))
                                                               (lambda (r) (setq deleg-res r))
                                                               parent-ctx)
                                                              (macher-agent-test-wait-until (lambda () deleg-res) 5.0)
                                                              ;; 1. Exactly two sends occurred; no third send triggered
                                                              (expect send-count :to-equal 2)
                                                              ;; 2. Parent callback receives error stating the agent halted without submit_task_result
                                                              (expect (stringp deleg-res) :to-be t)
                                                              (expect deleg-res :to-match "Agent halted without invoking submit_task_result")
                                                              ;; 3. Recovery pending is cleared on worker buffer
                                                              (when-let* ((wb (get-buffer "*a2a-17-worker*")))
                                                                (expect (buffer-local-value 'macher-agent-submit-recovery-pending wb) :to-be nil)
                                                                ;; 4. Routing stack is completely unwound
                                                                (expect (buffer-local-value 'macher-agent-routing-stack wb) :to-be nil))))
                                                        (when-let* ((wb (get-buffer "*a2a-17-worker*")))
                                                          (kill-buffer wb))))))

          ;; --------------------------------------------------------------------------
          ;; A2A-18
          ;; --------------------------------------------------------------------------
          (it "A2A-18: hydrates base state preserving caller tools, merging with buffer tools, and respecting explicit local suppression"
              (let* ((tool-caller (gptel-make-tool :name "caller_tool" :description "Caller tool"))
                     (tool-buf (gptel-make-tool :name "buffer_tool" :description "Buffer tool"))
                     (tool-dup (gptel-make-tool :name "caller_tool" :description "Duplicate caller tool")))
                ;; 1. Caller tools and buffer-local tools are merged without duplicates
                (with-temp-buffer
                  (setq-local gptel-tools (list tool-buf tool-dup))
                  (let* ((st (make-macher-agent-transmission-state
                              :target-buffer (current-buffer)
                              :tools (list tool-caller)))
                         (hydrated (macher-agent-pipe--hydrate-base-state st))
                         (names (mapcar #'macher-agent-canonical-tool-name
                                        (macher-agent-transmission-state-tools hydrated))))
                    (expect (length names) :to-equal 2)
                    (expect (member "caller_tool" names) :to-be-truthy)
                    (expect (member "buffer_tool" names) :to-be-truthy)))

                ;; 2. Explicit local suppression (setq-local gptel-tools nil) disables tools and does not resurrect caller tools
                (with-temp-buffer
                  (setq-local gptel-tools nil)
                  (let* ((st (make-macher-agent-transmission-state
                              :target-buffer (current-buffer)
                              :tools (list tool-caller)))
                         (hydrated (macher-agent-pipe--hydrate-base-state st)))
                    (expect (macher-agent-transmission-state-tools hydrated) :to-be nil)))

                ;; 3. When gptel-tools is not buffer-local, caller-provided tools on state are preserved
                (with-temp-buffer
                  (kill-local-variable 'gptel-tools)
                  (let* ((st (make-macher-agent-transmission-state
                              :target-buffer (current-buffer)
                              :tools (list tool-caller)))
                         (hydrated (macher-agent-pipe--hydrate-base-state st))
                         (names (mapcar #'macher-agent-canonical-tool-name
                                        (macher-agent-transmission-state-tools hydrated))))
                    (expect (length names) :to-equal 1)
                    (expect (car names) :to-equal "caller_tool")))))

          ;; --------------------------------------------------------------------------
          ;; A2A-19
          ;; --------------------------------------------------------------------------
          (it "A2A-19: keeps macher-agent-task-finished nil and session :idle on staged submit_task_result with nested routing frames"
              (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
                                                    (let* ((child-buf (macher-agent-add-subagent "*a2a-19-nested*" nil parent-buf nil parent-ctx))
                                                           (child-ctx (with-current-buffer child-buf macher-agent-persistent-context))
                                                           (outer-task-id "nested-outer-19")
                                                           (inner-task-id "nested-inner-19")
                                                           (outer-result nil)
                                                           (inner-result nil)
                                                           (hook-error nil)
                                                           (recovery-send-called nil)
                                                           (done-entries 0))
                                                      (unwind-protect
                                                          (progn
                                                            (macher-agent-register-task-callback
                                                             outer-task-id (lambda (res) (setq outer-result res)))
                                                            (macher-agent-register-task-callback
                                                             inner-task-id (lambda (res) (setq inner-result res)))
                                                            (with-current-buffer child-buf
                                                              (setq-local macher-agent-is-ephemeral t)
                                                              (macher-agent-push-routing outer-task-id (buffer-name parent-buf) t)
                                                              (macher-agent-push-routing inner-task-id "*a2a-19-inner-caller*" t)
                                                              (setf (macher-agent-session-state (macher-agent-context-session child-ctx)) :running)
                                                              (add-hook 'gptel-post-response-functions
                                                                        (macher-agent-make-transmit-response-hook
                                                                         (lambda (err) (setq hook-error err)))
                                                                        nil t)
                                                              (let* ((start-m (copy-marker (point-max)))
                                                                     (track-m (copy-marker (point-max) t))
                                                                     (info (list :buffer child-buf
                                                                                 :backend (or gptel-backend
                                                                                              (bound-and-true-p gptel--openai)
                                                                                              (gptel-make-openai "test-openai" :stream nil))
                                                                                 :data (list :messages (vector))
                                                                                 :position start-m
                                                                                 :tracking-marker track-m
                                                                                 :include-tool-results t
                                                                                 :callback #'ignore
                                                                                 :tool-result (list (list nil nil "SUCCESS: Result submitted."))
                                                                                 :tool-use (list (list :id "call_inner_19"
                                                                                                       :name "submit_task_result"
                                                                                                       :result "SUCCESS: Result submitted."))))
                                                                     (fsm (gptel-make-fsm
                                                                           :state 'TOOL
                                                                           :table (copy-alist gptel-send--transitions)
                                                                           :handlers (list (cons 'TRET (list #'gptel--handle-tool-result))
                                                                                           (cons 'DONE (list #'gptel--handle-post-insert
                                                                                                             #'gptel--fsm-last
                                                                                                             (lambda (_f) (setq done-entries (1+ done-entries))))))
                                                                           :info info)))
                                                                (macher-agent-fsm-hijack-transform fsm)
                                                                ;; Stage inner submission while FSM is in TOOL state
                                                                (macher-agent-submit-task-result "Inner-Nested-Answer" nil child-ctx)
                                                                (expect macher-agent-staged-submission :not :to-be nil)
                                                                (expect macher-agent-task-finished :to-be nil)
                                                                ;; Complete TOOL -> TRET -> DONE transition
                                                                (cl-letf (((symbol-function 'gptel-send)
                                                                           (lambda () (setq recovery-send-called t))))
                                                                  (gptel--fsm-transition fsm))
                                                                (expect done-entries :to-equal 1)
                                                                (expect hook-error :to-be nil)
                                                                (expect recovery-send-called :to-be nil)
                                                                (expect inner-result :not :to-be nil)
                                                                (expect (plist-get (macher-agent-transit-payload-payload inner-result) :payload)
                                                                        :to-equal "Inner-Nested-Answer")
                                                                (expect outer-result :to-be nil)
                                                                (expect macher-agent-staged-submission :to-be nil)
                                                                (expect (length macher-agent-routing-stack) :to-equal 1)
                                                                (expect (plist-get (car macher-agent-routing-stack) :task-id)
                                                                        :to-equal outer-task-id)
                                                                (expect macher-agent-current-task-id :to-equal outer-task-id)
                                                                (expect macher-agent-task-finished :to-be nil)
                                                                (expect (macher-agent-session-state (macher-agent-context-session child-ctx))
                                                                        :to-equal :idle)
                                                                (expect macher-agent-ready-to-reap :to-be nil)
                                                                ;; Direct macher-agent-deliver-staged-submission also clears stale macher-agent-task-finished
                                                                (macher-agent-push-routing "nested-inner-19b" "*a2a-19-inner-caller*" t)
                                                                (let ((delivered-second nil))
                                                                  (macher-agent-register-task-callback
                                                                   "nested-inner-19b" (lambda (res) (setq delivered-second res)))
                                                                  (setq-local macher-agent-task-finished t)
                                                                  (setq-local macher-agent-staged-submission
                                                                              (list :a2a-payload (macher-agent-make-a2a-payload
                                                                                                  :transit-type :subagent-to-parent
                                                                                                  :type 'ARTIFACT_UPDATE
                                                                                                  :task-id "nested-inner-19b"
                                                                                                  :payload (list :payload "Second-Inner"))
                                                                                    :context child-ctx
                                                                                    :suppress t))
                                                                  (macher-agent-deliver-staged-submission child-buf)
                                                                  (expect delivered-second :not :to-be nil)
                                                                  (expect (length macher-agent-routing-stack) :to-equal 1)
                                                                  (expect macher-agent-task-finished :to-be nil)
                                                                  (expect (macher-agent-session-state (macher-agent-context-session child-ctx))
                                                                          :to-equal :idle)))))
                                                        (macher-agent-pop-task-callback outer-task-id)
                                                        (macher-agent-pop-task-callback inner-task-id)
                                                        (macher-agent-pop-task-callback "nested-inner-19b")
                                                        (when (buffer-live-p child-buf) (kill-buffer child-buf))))))

          ;; --------------------------------------------------------------------------
          ;; A2A-20
          ;; --------------------------------------------------------------------------
          (it "A2A-20: keeps macher-agent-active-fsm intact across TYPE/TOOL/TRET callbacks until DONE/ERRS/ABRT"
              (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
                                                    (let* ((saved-global-skills macher-agent-global-skills-alist)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (tool-rel "skills/a2a20-skill/scripts/a2a20_tool.el")
                                                           (skill-rel "skills/a2a20-skill/SKILL.md")
                                                           (tool-abs (expand-file-name tool-rel root))
                                                           (skill-abs (expand-file-name skill-rel root))
                                                           (task-id "a2a-20-task")
                                                           (delivered-result nil)
                                                           (child-buf nil))
                                                      (unwind-protect
                                                          (progn
                                                            (make-directory (file-name-directory tool-abs) t)
                                                            (with-temp-file tool-abs
                                                              (insert "(gptel-make-tool :name \"a2a20_tool\" :description \"A2A20 v1\" :category \"misc\" :args nil :function (lambda () \"v1-result\"))\n"))
                                                            (with-temp-file skill-abs
                                                              (insert "---\nname: a2a20-skill\ndescription: A2A20 skill\nallowed-tools:\n  - a2a20_tool\n---\nA2A20 persona v1.\n"))
                                                            (macher-agent-register-task-callback
                                                             task-id (lambda (res) (setq delivered-result res)))
                                                            (setq child-buf (macher-agent-add-subagent "*a2a-20-worker*" nil parent-buf nil parent-ctx))
                                                            (with-current-buffer child-buf
                                                              (unless (bound-and-true-p macher-agent-mode)
                                                                (macher-agent-mode 1))
                                                              (let ((child-ctx macher-agent-persistent-context))
                                                                (macher-agent-refresh-skills-and-tools child-ctx)
                                                                (setq-local macher-agent-presets '(a2a20-skill))
                                                                (macher-agent-apply-preset '(a2a20-skill))
                                                                (macher-agent-push-routing task-id (buffer-name parent-buf) t)
                                                                (let* ((backend (or gptel-backend
                                                                                    (bound-and-true-p gptel--openai)
                                                                                    (gptel-make-openai "a2a20-openai" :stream nil)))
                                                                       (start-m (copy-marker (point-max)))
                                                                       (track-m (copy-marker (point-max) t))
                                                                       (cb-events nil)
                                                                       (info (list :buffer child-buf
                                                                                   :backend backend
                                                                                   :macher-agent-context child-ctx
                                                                                   :tools (copy-sequence gptel-tools)
                                                                                   :data (list :model "test-model"
                                                                                               :system "A2A20 persona v1."
                                                                                               :tools (gptel--parse-tools backend gptel-tools)
                                                                                               :messages (vector (list :role "user" :content "run")))
                                                                                   :position start-m
                                                                                   :tracking-marker track-m
                                                                                   :include-tool-results t
                                                                                   :callback (lambda (&rest args) (push args cb-events))))
                                                                       (fsm (gptel-make-fsm
                                                                             :state 'TYPE
                                                                             :table (copy-alist gptel-send--transitions)
                                                                             :handlers (list (cons 'TRET (list #'gptel--handle-tool-result))
                                                                                             (cons 'DONE (list #'gptel--handle-post-insert
                                                                                                               #'gptel--fsm-last)))
                                                                             :info info)))
                                                                  (setq-local gptel-backend backend)
                                                                  (macher-agent-fsm-hijack-transform fsm)
                                                                  (expect macher-agent-active-fsm :to-be fsm)

                                                                  ;; 1. Streaming chunk callback in TYPE state preserves active FSM
                                                                  (let ((wrapped-cb (plist-get (gptel-fsm-info fsm) :callback)))
                                                                    (funcall wrapped-cb "chunk-1" (gptel-fsm-info fsm))
                                                                    (expect (caar cb-events) :to-equal "chunk-1")
                                                                    (expect macher-agent-active-fsm :to-be fsm)

                                                                    ;; 2. tool-call callback in TOOL state preserves active FSM
                                                                    (setf (gptel-fsm-state fsm) 'TOOL)
                                                                    (funcall wrapped-cb (cons 'tool-call (list (list nil nil #'ignore)))
                                                                             (gptel-fsm-info fsm))
                                                                    (expect (car (caar cb-events)) :to-equal 'tool-call)
                                                                    (expect macher-agent-active-fsm :to-be fsm)

                                                                    ;; Mid-turn script edit still sees macher-agent-active-fsm and updates FSM :tools
                                                                    (with-temp-file tool-abs
                                                                      (insert "(gptel-make-tool :name \"a2a20_tool\" :description \"A2A20 v2\" :category \"misc\" :args nil :function (lambda () \"v2-result\"))\n"))
                                                                    (run-hook-with-args 'macher-agent-context-mutated-hook tool-rel child-ctx)
                                                                    (let ((fsm-tool (cl-find-if (lambda (tl) (equal (gptel-tool-name tl) "a2a20_tool"))
                                                                                                (plist-get (gptel-fsm-info fsm) :tools))))
                                                                      (expect fsm-tool :not :to-be nil)
                                                                      (expect (gptel-tool-description fsm-tool) :to-equal "A2A20 v2"))

                                                                    ;; submit_task_result still sees macher-agent-active-fsm in TOOL state and stages
                                                                    (let ((submit-res (macher-agent-submit-task-result "A2A20-Final" nil child-ctx)))
                                                                      (expect submit-res :to-equal "SUCCESS: Result submitted.")
                                                                      (expect macher-agent-staged-submission :not :to-be nil)
                                                                      (expect delivered-result :to-be nil))

                                                                    ;; 3. tool-result callback in TRET state preserves active FSM
                                                                    (let ((inf (copy-sequence (gptel-fsm-info fsm))))
                                                                      (setq inf (plist-put inf :tool-use
                                                                                           (list (list :id "call_a2a20"
                                                                                                       :name "submit_task_result"
                                                                                                       :result "SUCCESS: Result submitted."))))
                                                                      (setq inf (plist-put inf :tool-result
                                                                                           (list (list nil nil "SUCCESS: Result submitted."))))
                                                                      (setf (gptel-fsm-info fsm) inf))
                                                                    (setf (gptel-fsm-state fsm) 'TRET)
                                                                    (funcall wrapped-cb (cons 'tool-result (list (list nil nil "SUCCESS: Result submitted.")))
                                                                             (gptel-fsm-info fsm))
                                                                    (expect (car (caar cb-events)) :to-equal 'tool-result)
                                                                    (expect macher-agent-active-fsm :to-be fsm)

                                                                    ;; 4. Transitioning TRET -> DONE delivers staged submission and clears active FSM
                                                                    (gptel--fsm-transition fsm)
                                                                    (expect (gptel-fsm-state fsm) :to-equal 'DONE)
                                                                    (expect macher-agent-active-fsm :to-be nil)
                                                                    (expect delivered-result :not :to-be nil)
                                                                    (expect (plist-get (macher-agent-transit-payload-payload delivered-result) :payload)
                                                                            :to-equal "A2A20-Final")

                                                                    ;; 5. Direct callback in DONE/ERRS/ABRT clears matching active FSM only
                                                                    (setq-local macher-agent-active-fsm fsm)
                                                                    (setf (gptel-fsm-state fsm) 'ERRS)
                                                                    (funcall wrapped-cb nil (gptel-fsm-info fsm))
                                                                    (expect macher-agent-active-fsm :to-be nil)

                                                                    (setq-local macher-agent-active-fsm fsm)
                                                                    (setf (gptel-fsm-state fsm) 'ABRT)
                                                                    (funcall wrapped-cb 'abort (gptel-fsm-info fsm))
                                                                    (expect macher-agent-active-fsm :to-be nil)

                                                                    (let ((other-fsm (gptel-make-fsm :state 'WAIT)))
                                                                      (setq-local macher-agent-active-fsm other-fsm)
                                                                      (setf (gptel-fsm-state fsm) 'DONE)
                                                                      (funcall wrapped-cb t (gptel-fsm-info fsm))
                                                                      (expect macher-agent-active-fsm :to-be other-fsm)))))))
                                                        (macher-agent-pop-task-callback task-id)
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (cl-remprop 'a2a20-skill 'macher-agent-preset-meta)
                                                        (when (buffer-live-p child-buf) (kill-buffer child-buf))))))

          ;; --------------------------------------------------------------------------
          ;; A2A-21
          ;; --------------------------------------------------------------------------
          (it "A2A-21: scopes nested gptel-post-response-functions hooks by task-id so inner completion preserves outer hook"
              (macher-agent-test-with-git-workspace (_root parent-ctx parent-buf)
                                                    (let* ((worker-buf (macher-agent-add-subagent "*a2a-21-persistent*" nil parent-buf nil parent-ctx))
                                                           (outer-id "a2a-21-outer")
                                                           (inner-id "a2a-21-inner")
                                                           (inner-sync-id "a2a-21-inner-sync")
                                                           (outer-hook nil)
                                                           (inner-hook nil)
                                                           (outer-err nil)
                                                           (inner-err nil)
                                                           (inner-sync-err nil)
                                                           (outer-res nil)
                                                           (inner-res nil)
                                                           (inner-sync-res nil)
                                                           (recovery-sends 0))
                                                      (unwind-protect
                                                          (progn
                                                            (macher-agent-register-task-callback
                                                             outer-id (lambda (r) (setq outer-res r)))
                                                            (macher-agent-register-task-callback
                                                             inner-id (lambda (r) (setq inner-res r)))
                                                            (macher-agent-register-task-callback
                                                             inner-sync-id (lambda (r) (setq inner-sync-res r)))
                                                            (with-current-buffer worker-buf
                                                              (setq-local macher-agent-is-ephemeral nil)
                                                              ;; 1. Outer task transmits and installs outer hook
                                                              (macher-agent-push-routing outer-id (buffer-name parent-buf) t)
                                                              (cl-letf (((symbol-function 'gptel-send)
                                                                         (lambda ()
                                                                           (insert "\nAssistant: outer working...\n"))))
                                                                (macher-agent-gptel-transmit
                                                                 (make-macher-agent-task-context :target-buffer worker-buf :task-id outer-id)
                                                                 (list :on-error (lambda (e) (setq outer-err e)))))
                                                              (setq outer-hook (car gptel-post-response-functions))
                                                              (expect (functionp outer-hook) :to-be t)

                                                              ;; 2. Staged inner submission belonging to inner-id removes inner-hook only
                                                              (macher-agent-push-routing inner-id "*a2a-21-inner-parent*" t)
                                                              (cl-letf (((symbol-function 'gptel-send)
                                                                         (lambda ()
                                                                           (insert "\nAssistant: inner staged...\n"))))
                                                                (macher-agent-gptel-transmit
                                                                 (make-macher-agent-task-context :target-buffer worker-buf :task-id inner-id)
                                                                 (list :on-error (lambda (e) (setq inner-err e)))))
                                                              (setq inner-hook
                                                                    (cl-find-if (lambda (fn)
                                                                                  (and (functionp fn)
                                                                                       (not (eq fn outer-hook))
                                                                                       (not (eq fn #'macher-agent-post-response-reaper))))
                                                                                gptel-post-response-functions))
                                                              (expect (functionp inner-hook) :to-be t)
                                                              (setq-local macher-agent-staged-submission
                                                                          (list :a2a-payload (macher-agent-make-a2a-payload
                                                                                              :transit-type :subagent-to-parent
                                                                                              :type 'ARTIFACT_UPDATE
                                                                                              :task-id inner-id
                                                                                              :payload (list :payload "Inner-Staged"))
                                                                                :context macher-agent-persistent-context
                                                                                :suppress t
                                                                                :task-id inner-id))
                                                              (cl-letf (((symbol-function 'gptel-send)
                                                                         (lambda () (setq recovery-sends (1+ recovery-sends)))))
                                                                (run-hook-with-args 'gptel-post-response-functions (point-min) (point-max)))
                                                              (expect (memq inner-hook gptel-post-response-functions) :to-be nil)
                                                              (expect (memq outer-hook gptel-post-response-functions) :not :to-be nil)
                                                              (expect inner-res :not :to-be nil)
                                                              (expect outer-res :to-be nil)
                                                              (expect recovery-sends :to-equal 0)
                                                              (expect outer-err :to-be nil)
                                                              (expect inner-err :to-be nil)
                                                              (expect macher-agent-current-task-id :to-equal outer-id)
                                                              (setq-local macher-agent-staged-submission nil)

                                                              ;; 3. Synchronous inner submission removes a newly installed inner-hook while keeping outer-hook
                                                              (macher-agent-push-routing inner-sync-id "*a2a-21-inner-parent*" t)
                                                              (cl-letf (((symbol-function 'gptel-send)
                                                                         (lambda ()
                                                                           (insert "\nAssistant: inner sync...\n")
                                                                           (macher-agent-submit-task-result "Inner-Sync" nil macher-agent-persistent-context)
                                                                           (run-hook-with-args 'gptel-post-response-functions (point-min) (point-max)))))
                                                                (macher-agent-gptel-transmit
                                                                 (make-macher-agent-task-context :target-buffer worker-buf :task-id inner-sync-id)
                                                                 (list :on-error (lambda (e) (setq inner-sync-err e)))))
                                                              (expect inner-sync-res :not :to-be nil)
                                                              (expect inner-sync-err :to-be nil)
                                                              (expect outer-res :to-be nil)
                                                              (expect outer-err :to-be nil)
                                                              (expect (memq outer-hook gptel-post-response-functions) :not :to-be nil)
                                                              (expect (length (cl-remove-if-not
                                                                               (lambda (fn)
                                                                                 (and (functionp fn)
                                                                                      (not (eq fn #'macher-agent-post-response-reaper))))
                                                                               gptel-post-response-functions))
                                                                      :to-equal 1)
                                                              (expect macher-agent-current-task-id :to-equal outer-id)

                                                              ;; 4. Outer task finishes via submit_task_result -> outer-hook removes itself cleanly
                                                              (macher-agent-submit-task-result "Outer-Done" nil macher-agent-persistent-context)
                                                              (run-hook-with-args 'gptel-post-response-functions (point-min) (point-max))
                                                              (expect outer-res :not :to-be nil)
                                                              (expect (memq outer-hook gptel-post-response-functions) :to-be nil)))
                                                        (macher-agent-pop-task-callback outer-id)
                                                        (macher-agent-pop-task-callback inner-id)
                                                        (macher-agent-pop-task-callback inner-sync-id)
                                                        (when (buffer-live-p worker-buf) (kill-buffer worker-buf))))))

          ;; --------------------------------------------------------------------------
          ;; A2A-22
          ;; --------------------------------------------------------------------------
          (it "A2A-22: synchronous submit_task_result pops routing and updates session state before a2a-dispatch so re-delegation succeeds"
              (macher-agent-test-with-git-workspace (_root parent-ctx parent-buf)
                                                    (let* ((worker-buf (macher-agent-add-subagent "*a2a-22-persistent*" nil parent-buf nil parent-ctx))
                                                           (worker-ctx (with-current-buffer worker-buf macher-agent-persistent-context))
                                                           (t1-seen-state nil)
                                                           (t1-seen-stack-len nil)
                                                           (t2-res nil)
                                                           (t1-res nil)
                                                           (nested-outer-id "a2a-22-outer")
                                                           (nested-t1-id "a2a-22-nested-t1")
                                                           (nested-t2-id "a2a-22-nested-t2")
                                                           (nested-seen-state nil)
                                                           (nested-seen-finished :unset)
                                                           (nested-t2-delivered nil))
                                                      (unwind-protect
                                                          (with-current-buffer parent-buf
                                                            (with-current-buffer worker-buf
                                                              (setq-local macher-agent-is-ephemeral nil)
                                                              (setq-local macher-agent-explicitly-persistent t)
                                                              (setf (macher-agent-session-state (macher-agent-context-session worker-ctx)) :idle))
                                                            ;; 1. Synchronous re-delegation from task-1's parent callback onto the same persistent worker
                                                            (let ((turn 0))
                                                              (cl-letf (((symbol-function 'gptel-send)
                                                                         (lambda ()
                                                                           (setq turn (1+ turn))
                                                                           (let ((ans (format "Answer-%d" turn))
                                                                                 (beg (point-max)))
                                                                             (goto-char (point-max))
                                                                             (insert (format "\n%s\n" ans))
                                                                             (macher-agent-submit-task-result ans nil macher-agent-persistent-context)
                                                                             (run-hook-with-args 'gptel-post-response-functions beg (point-max))))))
                                                                (macher-agent-delegate-tasks
                                                                 (vector (list :buffer_name "*a2a-22-persistent*"
                                                                               :instructions "Run task 1"))
                                                                 (lambda (r1)
                                                                   (setq t1-res r1)
                                                                   (with-current-buffer worker-buf
                                                                     (setq t1-seen-state
                                                                           (macher-agent-session-state
                                                                            (macher-agent-context-session worker-ctx)))
                                                                     (setq t1-seen-stack-len (length macher-agent-routing-stack)))
                                                                   (macher-agent-delegate-tasks
                                                                    (vector (list :buffer_name "*a2a-22-persistent*"
                                                                                  :instructions "Run task 2"))
                                                                    (lambda (r2) (setq t2-res r2))
                                                                    parent-ctx))
                                                                 parent-ctx)))
                                                            (macher-agent-test-wait-until (lambda () (and t1-res t2-res)) 5.0)
                                                            (expect t1-seen-state :to-equal :finished)
                                                            (expect t1-seen-stack-len :to-equal 0)
                                                            (expect t1-res :to-match "Answer-1")
                                                            (expect t2-res :to-match "Answer-2")
                                                            (expect (buffer-live-p worker-buf) :to-be t)
                                                            (with-current-buffer worker-buf
                                                              (expect macher-agent-is-ephemeral :to-be nil)
                                                              (expect macher-agent-ready-to-reap :to-be nil)
                                                              (expect macher-agent-routing-stack :to-be nil)
                                                              (expect macher-agent-current-task-id :to-be nil))

                                                            ;; 2. Nested outer routing frame present: callback sees :idle and nil macher-agent-task-finished,
                                                            ;;    and synchronous re-delegation's new routing frame is not popped by the returning call
                                                            (macher-agent-register-task-callback
                                                             nested-t1-id
                                                             (lambda (_p1)
                                                               (with-current-buffer worker-buf
                                                                 (setq nested-seen-state
                                                                       (macher-agent-session-state (macher-agent-context-session worker-ctx)))
                                                                 (setq nested-seen-finished macher-agent-task-finished)
                                                                 (macher-agent-push-routing nested-t2-id (buffer-name parent-buf) t))))
                                                            (macher-agent-register-task-callback
                                                             nested-t2-id
                                                             (lambda (p2) (setq nested-t2-delivered p2)))
                                                            (with-current-buffer worker-buf
                                                              (macher-agent-push-routing nested-outer-id (buffer-name parent-buf) t)
                                                              (macher-agent-push-routing nested-t1-id (buffer-name parent-buf) t)
                                                              (setf (macher-agent-session-state (macher-agent-context-session worker-ctx)) :running)
                                                              (macher-agent-submit-task-result "Nested-Sync-1" nil worker-ctx)
                                                              (expect nested-seen-state :to-equal :idle)
                                                              (expect nested-seen-finished :to-be nil)
                                                              (expect (length macher-agent-routing-stack) :to-equal 2)
                                                              (expect (plist-get (car macher-agent-routing-stack) :task-id)
                                                                      :to-equal nested-t2-id)
                                                              (expect macher-agent-current-task-id :to-equal nested-t2-id)
                                                              ;; Submitting nested-t2-id now delivers nested-t2-delivered and leaves outer frame
                                                              (macher-agent-submit-task-result "Nested-Sync-2" nil worker-ctx)
                                                              (expect nested-t2-delivered :not :to-be nil)
                                                              (expect (length macher-agent-routing-stack) :to-equal 1)
                                                              (expect (plist-get (car macher-agent-routing-stack) :task-id)
                                                                      :to-equal nested-outer-id)))
                                                        (macher-agent-pop-task-callback nested-outer-id)
                                                        (macher-agent-pop-task-callback nested-t1-id)
                                                        (macher-agent-pop-task-callback nested-t2-id)
                                                        (when (buffer-live-p worker-buf) (kill-buffer worker-buf))))))

          ;; --------------------------------------------------------------------------
          ;; A2A-23
          ;; --------------------------------------------------------------------------
          (it "A2A-23: delegates external server-side allowed-tools skill without submit_task_result injection and completes on plain text"
              (require 'gptel-gemini)
              (macher-agent-test-with-git-workspace (_root parent-ctx parent-buf)
                                                    (let* ((saved-global-skills macher-agent-global-skills-alist)
                                                           (saved-skill-dirs macher-agent-skill-directories)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (ext-skills-dir (make-temp-file "a2a23-ext-skills-" t))
                                                           (gptel--known-backends nil)
                                                           (gemini (gptel-make-gemini "A2A23Gemini" :models '(gemini-flash-latest) :stream nil))
                                                           (captured-info nil)
                                                           (send-count 0)
                                                           (delegated-result nil))
                                                      (unwind-protect
                                                          (progn
                                                            (make-directory (expand-file-name "google-search" ext-skills-dir) t)
                                                            (with-temp-file (expand-file-name "google-search/SKILL.md" ext-skills-dir)
                                                              (insert "---\n"
                                                                      "name: google-search\n"
                                                                      "description: Grounds responses in live Google Search results via Gemini without local workspace tools.\n"
                                                                      "model: gemini-flash-latest\n"
                                                                      "exclusive: true\n"
                                                                      "disable-local-tools: true\n"
                                                                      "allowed-tools: [google_search]\n"
                                                                      "---\n"
                                                                      "Ground answers in live Google Search results.\n"))
                                                            (setq macher-agent-skill-directories (list ext-skills-dir))
                                                            (macher-agent-initialize-skills nil)

                                                            (with-current-buffer parent-buf
                                                              (markdown-mode)
                                                              (gptel-mode 1)
                                                              (macher-agent-mode 1)
                                                              (macher-agent-refresh-skills-and-tools parent-ctx)
                                                              (cl-letf (((symbol-function 'gptel-send)
                                                                         (lambda ()
                                                                           (setq send-count (1+ send-count))
                                                                           (let ((dry-fsm (gptel-request nil
                                                                                            :dry-run t
                                                                                            :stream nil
                                                                                            :transforms gptel-prompt-transform-functions)))
                                                                             (setq captured-info (gptel-fsm-info dry-fsm)))
                                                                           (let ((beg (point-max)))
                                                                             (goto-char beg)
                                                                             (insert "Grounded result: Arsenal 1-0 Hull City (Martin Odegaard).\n")
                                                                             (put-text-property beg (point) 'gptel 'response)
                                                                             (run-hook-with-args 'gptel-post-response-functions beg (point-max))))))
                                                                (macher-agent-delegate-tasks
                                                                 (vector (list :buffer_name "*a2a-23-search*"
                                                                               :presets ["google-search"]
                                                                               :instructions "Find the Arsenal vs Hull City result."))
                                                                 (lambda (res) (setq delegated-result res))
                                                                 parent-ctx)))

                                                            (macher-agent-test-wait-until
                                                             (lambda ()
                                                               (and delegated-result
                                                                    (null (get-buffer "*a2a-23-search*"))))
                                                             5.0)
                                                            (expect send-count :to-equal 1)
                                                            (expect captured-info :not :to-be nil)
                                                            (expect (plist-get captured-info :backend) :to-equal gemini)
                                                            (expect (plist-get captured-info :model) :to-equal 'gemini-flash-latest)
                                                            (expect (plist-get captured-info :tools) :to-be nil)
                                                            (expect (plist-get captured-info :system) :not :to-match "submit_task_result")
                                                            (let ((data (plist-get captured-info :data)))
                                                              (expect (plist-get data :tools) :to-equal [(:google_search nil)])
                                                              (expect (json-serialize data :null-object :null :false-object :json-false)
                                                                      :not :to-match "functionDeclarations"))
                                                            (let ((flat (if (stringp delegated-result)
                                                                            delegated-result
                                                                          (string-join (append delegated-result nil) "\n"))))
                                                              (expect flat :to-match "Arsenal 1-0 Hull City"))
                                                            (expect (get-buffer "*a2a-23-search*") :to-be nil))
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (setq macher-agent-skill-directories saved-skill-dirs)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (cl-remprop 'google-search 'macher-agent-preset-meta)
                                                        (when (get-buffer "*a2a-23-search*")
                                                          (kill-buffer "*a2a-23-search*"))
                                                        (when (and ext-skills-dir (file-directory-p ext-skills-dir))
                                                          (ignore-errors (delete-directory ext-skills-dir t)))))))

          ;; --------------------------------------------------------------------------
          ;; A2A-24
          ;; --------------------------------------------------------------------------
          (it "A2A-24: skip-clone re-delegation aligns gptel--preset with updated :presets"
              (macher-agent-test-with-git-workspace (_root parent-ctx parent-buf)
                                                    (let* ((orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (worker-buf nil))
                                                      (unwind-protect
                                                          (progn
                                                            (gptel-make-preset 'a2a24-alpha
                                                              :description "Alpha preset"
                                                              :system "Alpha prompt.")
                                                            (gptel-make-preset 'a2a24-beta
                                                              :description "Beta preset"
                                                              :system "Beta prompt.")
                                                            (with-current-buffer parent-buf
                                                              (setq-local gptel--known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                              (setq-local gptel-directives (copy-alist (default-value 'gptel-directives))))
                                                            (setq worker-buf (macher-agent-add-subagent "*a2a-24-worker*" '(a2a24-alpha) parent-buf nil parent-ctx))
                                                            (with-current-buffer worker-buf
                                                              (expect gptel--preset :to-equal 'a2a24-alpha)
                                                              (expect macher-agent-presets :to-equal '(a2a24-alpha)))
                                                            (macher-agent-init-subagent-state
                                                             worker-buf "a2a24-task-2" (list :presets ["a2a24-beta"]) parent-ctx t)
                                                            (with-current-buffer worker-buf
                                                              (expect macher-agent-presets :to-equal '("a2a24-beta"))
                                                              (expect gptel--preset :to-equal 'a2a24-beta))
                                                            (macher-agent-init-subagent-state
                                                             worker-buf "a2a24-task-3" (list :presets ["a2a24-unknown"]) parent-ctx t)
                                                            (with-current-buffer worker-buf
                                                              (expect gptel--preset :to-be nil)))
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (when (buffer-live-p worker-buf)
                                                          (kill-buffer worker-buf)))))))

(provide 'test-a2a)
;;; test-a2a.el ends here
