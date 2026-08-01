;;; macher-agent-resilience-test.el --- Resilience tests for macher-agent -*- lexical-binding: t -*-

(require 'buttercup)
(require 'cl-lib)
(require 'macher-agent)
(require 'macher-agent-gptel-bridge)
(require 'macher-agent-vfs-client)
(require 'macher-agent-orchestration)

(describe "Macher-Agent Resilience Specifications"

          (describe "1. Test Suite Resilience: The Headless Timer Trap"
                    (defvar active-timers nil)

                    (after-each
                     (dolist (timer active-timers)
                       (when (timerp timer)
                         (cancel-timer timer)))
                     (setq active-timers nil)
                     (dolist (timer timer-list)
                       (when (timerp timer)
                         (cancel-timer timer))))

                    (it "enforces liveness checks in asynchronous callbacks"
                        (let* ((buf (generate-new-buffer "test-resilience-timer-liveness"))
                               (executed nil)
                               (callback (lambda ()
                                           (when (buffer-live-p buf)
                                             (setq executed t)))))
                          (with-current-buffer buf
                            (setq executed nil))
                          (funcall callback)
                          (expect executed :to-be t)
                          (kill-buffer buf)
                          (setq executed nil)
                          (funcall callback)
                          (expect executed :to-be nil)))

                    (it "handles killed buffers gracefully without throwing errors when executing pending handlers"
                        (let* ((buf (generate-new-buffer "test-resilience-terminal-parity"))
                               (timer (with-current-buffer buf
                                        (macher-agent--schedule-buffer-reap buf))))
                          (push timer active-timers)
                          (kill-buffer buf)
                          (expect (lambda () (funcall (timer--function timer))) :not :to-throw))))

          (describe "2. LLM Payload Edge Cases: The Nil-Response Guardrail"
                    (it "coerces nil text responses to empty strings when valid tool use is present"
                        (let* ((captured-response "unset")
                               (captured-info nil)
                               (mock-orig-fun (lambda (resp info &optional _raw)
                                                (setq captured-response resp)
                                                (setq captured-info info)
                                                "success")))
                          (expect (macher-agent--protect-nil-responses
                                   mock-orig-fun
                                   nil
                                   (list :tool-use '((:name "execute_command"))))
                                  :to-equal "success")
                          (expect captured-response :to-equal "")
                          (expect (plist-get captured-info :tool-use) :to-be-truthy)))

                    (it "handles tool-only sub-agent responses during silent delegation"
                        (let* ((buf (generate-new-buffer "test-resilience-subagent-silent"))
                               (success-called nil)
                               (error-called nil)
                               (hook-fn (macher-agent--make-transmit-response-hook
                                         (lambda (res) (setq success-called res))
                                         (lambda (err) (setq error-called err)))))
                          (with-current-buffer buf
                            (setq-local macher-agent--is-subagent t)
                            (insert "Tool execution result payload")
                            (funcall hook-fn (point-min) (point-max)))
                          (expect success-called :to-equal "Tool execution result payload")
                          (expect error-called :to-be nil)
                          (when (buffer-live-p buf)
                            (kill-buffer buf)))))

          (describe "3. JIT Composition Boundaries: State Bleed Prevention"
                    (it "preserves permanent buffer gptel-system-prompt without inline tag bleed or Skill wrappers"
                        (let* ((orig-buf (generate-new-buffer "test-resilience-state-bleed-orig"))
                               (base-sys "Permanent Clean System Directive")
                               (temp-buf (generate-new-buffer "test-resilience-state-bleed-temp")))
                          (with-current-buffer orig-buf
                            (setq-local gptel-system-prompt base-sys)
                            (setq-local gptel-model 'test-model)
                            (setq-local gptel-tools nil)
                            (insert "@elisp-expert Perform task with inline tag"))
                          (with-current-buffer temp-buf
                            (let ((fsm (gptel-make-fsm)))
                              (setf (gptel-fsm-info fsm) (list :buffer orig-buf))
                              (macher-agent-sync-prompt-transformer nil fsm)))
                          (with-current-buffer orig-buf
                            (expect gptel-system-prompt :to-equal base-sys)
                            (expect (string-match-p "PROGRAMMATIC TOOL CALLING" gptel-system-prompt) :to-be nil)
                            (expect (string-match-p "@elisp-expert" gptel-system-prompt) :to-be nil))
                          (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                          (when (buffer-live-p temp-buf) (kill-buffer temp-buf))))

                    (it "verifies sub-agent buffer gptel-system-prompt matches directive registered in gptel-directives"
                        (let* ((directive-key 'resilience-test-directive)
                               (directive-msg "Subagent Directive System Message")
                               (old-directives (default-value 'gptel-directives))
                               (old-presets (default-value 'gptel--known-presets))
                               (parent-buf (generate-new-buffer "test-resilience-parent"))
                               (sub-buf nil))
                          (unwind-protect
                              (progn
                                (setq-default gptel-directives (cons (cons directive-key directive-msg) old-directives))
                                (setq-default gptel--known-presets (cons (cons directive-key (list :system directive-msg)) old-presets))
                                (with-current-buffer parent-buf
                                  (setq-local gptel-directives (default-value 'gptel-directives))
                                  (setq-local gptel--known-presets (default-value 'gptel--known-presets))
                                  (setq sub-buf (macher-agent-add-subagent "test-resilience-subagent-directive" (list directive-key))))
                                (with-current-buffer sub-buf
                                  (expect gptel-system-prompt :to-equal directive-msg)))
                            (setq-default gptel-directives old-directives)
                            (setq-default gptel--known-presets old-presets)
                            (when (buffer-live-p parent-buf) (kill-buffer parent-buf))
                            (when (buffer-live-p sub-buf) (kill-buffer sub-buf))))))

          (describe "4. VFS and Context Resolution: The Hidden Buffer Trap"
                    (it "extracts orig-buf context from FSM inside temporary transmission buffer"
                        (let* ((temp-dir (file-name-as-directory (make-temp-file "macher-resilience-ctx-" t)))
                               (workspace (make-macher-agent-workspace :project-root temp-dir))
                               (ctx (macher--make-context :workspace workspace :contents nil))
                               (orig-buf (generate-new-buffer "test-resilience-orig-buf"))
                               (trans-buf (generate-new-buffer "test-resilience-trans-buf"))
                               (fsm (gptel-make-fsm))
                               (resolved-ctx nil))
                          (puthash (expand-file-name temp-dir) ctx macher-agent-active-workspaces)
                          (setf (gptel-fsm-info fsm) (list :buffer orig-buf :macher-agent-context ctx))
                          (with-current-buffer trans-buf
                            (setq resolved-ctx (macher-agent-resolve-context fsm)))
                          (expect resolved-ctx :to-equal ctx)
                          (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                          (when (buffer-live-p trans-buf) (kill-buffer trans-buf))
                          (delete-directory temp-dir t)))

                    (it "clears VFS memory after Emacs cache desync is resolved by buffer revert"
                        (let* ((temp-dir (file-name-as-directory (make-temp-file "macher-resilience-desync-" t)))
                               (file-path (expand-file-name "desync-target.txt" temp-dir))
                               (initial-text "Initial File Content\n")
                               (updated-text "Updated VFS Content\n"))
                          (write-region initial-text nil file-path nil 'silent)
                          (let* ((file-buf (find-file-noselect file-path))
                                 (workspace (make-macher-agent-workspace :project-root temp-dir))
                                 (ctx (macher--make-context :workspace workspace :contents nil)))
                            (puthash (expand-file-name temp-dir) ctx macher-agent-active-workspaces)
                            (macher-agent-vfs-write workspace file-path updated-text)
                            (expect (macher-agent--get-context-dirty-p ctx) :to-be t)
                            (write-region updated-text nil file-path nil 'silent)
                            (macher-agent--auto-sync-context ctx)
                            (expect (macher-agent--get-context-dirty-p ctx) :to-be t)
                            (with-current-buffer file-buf
                              (revert-buffer t t))
                            (macher-agent--auto-sync-context ctx)
                            (expect (macher-agent--get-context-dirty-p ctx) :to-be nil)
                            (when (buffer-live-p file-buf) (kill-buffer file-buf)))
                          (delete-directory temp-dir t)))))

(provide 'macher-agent-resilience-test)
