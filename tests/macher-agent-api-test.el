;;; macher-agent-api-test.el --- Tests for public API -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'macher-agent-api)
(require 'macher-agent)
(require 'macher-agent-core)

(describe "Macher-Agent Public API Suite"

          (describe "API Contract"
                    (it "ensures all public API bridge functions are defined"
                        (expect (fboundp 'macher-agent-workspace-resolve-path) :to-be t)
                        (expect (fboundp 'macher-agent-context-read) :to-be t)
                        (expect (fboundp 'macher-agent-context-update) :to-be t)
                        (expect (fboundp 'macher-agent-scope-add-file) :to-be t)
                        (expect (fboundp 'macher-agent-a2a-dispatch) :to-be t)
                        (expect (fboundp 'macher-agent-prepare-instructions) :to-be t)
                        (expect (fboundp 'macher-agent-submit-task-result) :to-be t)
                        (expect (fboundp 'macher-agent-sandbox-run) :to-be t)
                        (expect (fboundp 'macher-agent-workspace-root) :to-be t)
                        (expect (fboundp 'macher-agent-api-register-skills-in-directory) :to-be t)
                        (expect (fboundp 'macher-agent-ui-show) :to-be t)))

          (describe "Sandboxed Evaluator Basic Features"
                    (it "evaluates basic expressions safely"
                        (expect (macher-agent-sandbox-run 42 nil) :to-equal 42)
                        (expect (macher-agent-sandbox-run "test" nil) :to-equal "test")
                        (expect (macher-agent-sandbox-run t nil) :to-equal t)
                        (expect (macher-agent-sandbox-run nil nil) :to-be nil)
                        (expect (macher-agent-sandbox-run '(quote (1 2 3)) nil) :to-equal '(1 2 3))
                        (expect (macher-agent-sandbox-run '(progn 1 2 3) nil) :to-equal 3)
                        (expect (macher-agent-sandbox-run '(if t 'yes 'no) nil) :to-equal 'yes)
                        (expect (macher-agent-sandbox-run '(if nil 'yes 'no) nil) :to-equal 'no)
                        (expect (macher-agent-sandbox-run '(let ((x 10) (y 20)) (progn (setq x 15) (+ x y))) '(+)) :to-equal 35)
                        (expect (macher-agent-sandbox-run '(funcall (lambda (x) (* x x)) 5) '(*)) :to-equal 25)))

          (describe "Sandboxed Evaluator Comprehensive Features"
                    (it "evaluates newly implemented sandboxed features safely"
                        ;; Keyword self-evaluation
                        (expect (macher-agent-sandbox-run ':foo nil) :to-equal :foo)
                        (expect (macher-agent-sandbox-run '(let ((x :bar)) x) nil) :to-equal :bar)

                        ;; Built-in and whitelisted functions
                        (expect (macher-agent-sandbox-run '(not nil) nil) :to-equal t)
                        (expect (macher-agent-sandbox-run '(not t) nil) :to-be nil)
                        (expect (macher-agent-sandbox-run '(reverse '(1 2 3)) nil) :to-equal '(3 2 1))
                        (expect (macher-agent-sandbox-run '(split-string "foo bar" " ") nil) :to-equal '("foo" "bar"))
                        (expect (macher-agent-sandbox-run '(plist-get '(:a 1 :b 2) :b) nil) :to-equal 2)

                        ;; Functional application
                        (expect (macher-agent-sandbox-run '(apply '+ 1 2 '(3 4)) '(+)) :to-equal 10)
                        (expect (macher-agent-sandbox-run '(apply '+ '(1 2 3)) '(+)) :to-equal 6)
                        (expect (macher-agent-sandbox-run '(mapcar (lambda (x) (* x 2)) '(1 2 3)) '(*)) :to-equal '(2 4 6))

                        ;; Control flow and Error handling
                        (expect (macher-agent-sandbox-run '(condition-case err (/ 1 0) (error 'caught)) '(/)) :to-equal 'caught)
                        (expect (macher-agent-sandbox-run '(unwind-protect 1 2) nil) :to-equal 1)
                        (expect (macher-agent-sandbox-run '(catch 'tag (throw 'tag 42)) nil) :to-equal 42)

                        ;; Introspection
                        (expect (macher-agent-sandbox-run '(fboundp 'car) nil) :to-equal t)
                        (expect (macher-agent-sandbox-run '(fboundp 'nonexistent) nil) :to-be nil)
                        (expect (macher-agent-sandbox-run '(let ((x 1)) (boundp 'x)) nil) :to-equal t)
                        (expect (macher-agent-sandbox-run '(boundp 'nonexistent) nil) :to-be nil)

                        ;; Macros
                        (expect (macher-agent-sandbox-run '(progn
                                                             (defalias 'my-when '(macro lambda (cond &rest body)
                                                                                        (list 'if cond (cons 'progn body))))
                                                             (my-when t 42))
                                                          nil)
                                :to-equal 42)))

          (describe "Context Resolution Regression"
                    (before-each
                     (clrhash macher-agent-active-workspaces))

                    (it "short-circuits waterfall pipeline when explicit context is passed"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/explicit-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
                          (expect (macher-agent-resolve-context ctx) :to-be ctx)))

                    (it "extracts context from FSM payload when FSM object is supplied"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/fsm-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (fsm (gptel-make-fsm)))
                          (setf (gptel-fsm-info fsm) (list :macher-agent-context ctx))
                          (expect (macher-agent-resolve-context fsm) :to-be ctx)))

                    (it "resolves context from active workspace registry via ancestor directory walk"
                        (let* ((macher-agent-active-workspaces (make-hash-table :test 'equal))
                               (root-dir "/mock/parent-proj/")
                               (sub-dir "/mock/parent-proj/deep/sub/dir/")
                               (ws (make-macher-agent-workspace :project-root root-dir))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
                          (puthash (expand-file-name root-dir) ctx macher-agent-active-workspaces)
                          (let ((default-directory sub-dir))
                            (expect (macher-agent-resolve-context) :to-be ctx))))

                    (it "clones canonical context when resolving inside subagent buffer to ensure isolation"
                        (let* ((macher-agent-active-workspaces (make-hash-table :test 'equal))
                               (root-dir "/mock/subagent-proj/")
                               (ws (make-macher-agent-workspace :project-root root-dir))
                               (canonical-ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
                          (puthash (expand-file-name root-dir) canonical-ctx macher-agent-active-workspaces)
                          (let ((default-directory root-dir)
                                (macher-agent--is-subagent t)
                                (macher-agent--persistent-context nil))
                            (spy-on 'macher-agent-root :and-return-value root-dir)
                            (let ((resolved (macher-agent-resolve-context)))
                              (expect resolved :not :to-be canonical-ctx)
                              (expect (macher-context-p resolved) :to-be t)))))

                    (it "signals error when context resolution fails across all pipeline steps"
                        (let ((macher-agent-active-workspaces (make-hash-table :test 'equal))
                              (macher-agent--persistent-context nil)
                              (macher-agent--active-fsm nil)
                              (gptel--fsm-last nil)
                              (macher--fsm-latest nil))
                          (spy-on 'macher-agent--resolve-context-lazy-init :and-return-value nil)
                          (expect (macher-agent-resolve-context) :to-throw 'error)))

                    (it "reads and updates files via public context API functions"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/api-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (file "src/main.el"))
                          (macher-agent-context-update ctx file "(message \"hello\")")
                          (expect (macher-agent-context-read ctx file) :to-equal "(message \"hello\")")
                          (macher-agent-context-update ctx file "(message \"updated\")")
                          (expect (macher-agent-context-read ctx file) :to-equal "(message \"updated\")"))))

          (describe "Strict VFS Pipeline Regression"

                    (it "executes pipeline steps in strict sequential order"
                        (let* ((step-order nil)
                               (ws (make-macher-agent-workspace :project-root "/mock/vfs-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents (list 'dummy))))
                          (spy-on 'macher-agent--vfs-verify-clean-merge :and-call-fake (lambda (&rest _) (push 'verify step-order)))
                          (spy-on 'macher-agent--vfs-sync-baseline :and-call-fake (lambda (&rest _) (push 'sync step-order)))
                          (spy-on 'macher-agent--vfs-apply-overlay-stateless :and-call-fake (lambda (&rest _) (push 'overlay step-order)))
                          (spy-on 'macher-agent-context-root :and-return-value "/mock/vfs-proj/")
                          (spy-on 'delete-directory :and-call-through)

                          (macher-agent-with-strict-vfs-pipeline ctx
                                                                 'done)

                          (expect (reverse step-order) :to-equal '(verify sync overlay))))

                    (it "binds default-directory to isolated temporary sandbox directory during body execution"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/vfs-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (captured-dir nil))
                          (spy-on 'macher-agent--vfs-verify-clean-merge)
                          (spy-on 'macher-agent--vfs-sync-baseline)
                          (spy-on 'macher-agent--vfs-apply-overlay-stateless)
                          (spy-on 'macher-agent-context-root :and-return-value "/mock/vfs-proj/")

                          (macher-agent-with-strict-vfs-pipeline ctx
                                                                 (setq captured-dir default-directory))

                          (expect captured-dir :not :to-equal "/mock/vfs-proj/")
                          (expect (string-match-p "macher-sandbox-" captured-dir) :to-be-truthy)))

                    (it "overlays virtual context edits into sandbox and returns body evaluation result"
                        (spy-on 'macher-agent--vfs-sync-baseline)
                        (let* ((temp-proj (file-name-as-directory (make-temp-file "macher-strict-vfs-" t)))
                               (file-rel "config.txt")
                               (file-abs (expand-file-name file-rel temp-proj))
                               (ws (make-macher-agent-workspace :project-root temp-proj))
                               (ctx (macher-agent--make-vfs-context
                                     :workspace ws
                                     :contents (list (macher-agent-vfs-make-entry file-abs "orig-disk" "virtual-overlay"))))
                               (result nil))
                          (unwind-protect
                              (progn
                                (write-region "orig-disk" nil file-abs nil 'silent)
                                (setq result
                                      (macher-agent-with-strict-vfs-pipeline ctx
                                                                             (if (file-exists-p file-rel)
                                                                                 (with-temp-buffer
                                                                                   (insert-file-contents file-rel)
                                                                                   (buffer-string))
                                                                               "not-found")))
                                (expect result :to-equal "virtual-overlay"))
                            (delete-directory temp-proj t))))

                    (it "guarantees sandbox directory cleanup even when body signals an error"
                        (let* ((ws (make-macher-agent-workspace :project-root "/mock/vfs-proj/"))
                               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                               (created-sandbox nil))
                          (spy-on 'macher-agent--vfs-verify-clean-merge)
                          (spy-on 'macher-agent--vfs-sync-baseline)
                          (spy-on 'macher-agent--vfs-apply-overlay-stateless)
                          (spy-on 'macher-agent-context-root :and-return-value "/mock/vfs-proj/")

                          (expect
                           (macher-agent-with-strict-vfs-pipeline ctx
                                                                  (setq created-sandbox default-directory)
                                                                  (error "Forced pipeline failure"))
                           :to-throw 'error)

                          (expect created-sandbox :not :to-be nil)
                          (expect (file-exists-p created-sandbox) :to-be nil)))
                    ))

(provide 'macher-agent-api-test)
;;; macher-agent-api-test.el ends here
