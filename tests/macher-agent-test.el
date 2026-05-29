;;; macher-agent-test.el --- Comprehensive BDD tests for Macher-Agent -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'buttercup)
(require 'macher-agent)
(require 'macher-agent-async)
(require 'macher-agent-context)
(require 'macher-agent-context-tools)
(require 'macher-agent-gptel-tools)
(require 'macher-agent-orchestration)


(describe "Macher-Agent BDD Test Suite"

          (before-each
           (spy-on 'macher-action)
           (spy-on 'gptel-send)
           (spy-on 'macher--add-termination-handler)
           (setq macher-agent--persistent-context nil)
           (setq macher-agent-active-subagents nil))

          (describe "Asynchronous Logic (macher-agent-async.el)"
                    (it "reports an explicit error when a transition event is 'failed'"
                        (let* ((callback-called nil)
                               (callback (lambda (msg) (setq callback-called msg)))
                               (payload (list :actual-name "test-agent" :callback callback :err "Panic")))
                          (macher-agent--fsm-transition 'any 'error payload)
                          (expect callback-called :to-equal "ERROR: Panic")))

                    (it "detects and handles a killed buffer during polling"
                        (let* ((buf (generate-new-buffer "killed-buf"))
                               (callback-called nil)
                               (callback (lambda (msg) (setq callback-called msg)))
                               (payload (list :buf buf :actual-name "test-agent" :callback callback)))
                          (kill-buffer buf)
                          (macher-agent--fsm-transition 'polling 'check-continuation payload)
                          (expect callback-called :to-equal "ERROR: Buffer 'test-agent' was killed."))))

          (describe "Context & Security (macher-agent-context.el)"
                    (it "throws a security error if accessing a path outside of the allowed context"
                        (let ((ctx (macher--make-context :contents '(("allowed.txt" . ("old" . "new"))))))
                          (expect (macher-agent--ensure-access ctx "forbidden.txt") :to-throw 'error)))

                    (it "successfully records a virtual edit to an existing scoped buffer"
                        (let* ((ctx (macher--make-context :contents '(("test.txt" . ("orig" . "orig"))))))
                          (macher-agent--update-context-file ctx "test.txt" "modified")
                          (expect (macher-context-dirty-p ctx) :to-be t)
                          (expect (cdr (cdr (assoc "test.txt" (macher-context-contents ctx)))) :to-equal "modified")))


                    (describe "Three-way Merge Logic"
                              (it "invalidates the local cache if both local and remote diverged"
                                  (let* ((test-dir (make-temp-file "macher-test-dir" t))
                                         (test-file (expand-file-name "test.txt" test-dir))
                                         (ctx (macher--make-context :dirty-p t)))
                                    
                                    (setf (macher-context-contents ctx) 
                                          (list (cons test-file (cons "v1" "v2-local"))))
                                    
                                    (with-temp-file test-file (insert "v2-remote"))
                                    
                                    (macher-agent--auto-sync-context ctx)
                                    
                                    (let ((pair (cdr (assoc test-file (macher-context-contents ctx)))))
                                      (expect (car pair) :to-equal "v2-remote")
                                      (expect (cdr pair) :to-equal "v2-remote"))
                                    
                                    (delete-directory test-dir t)))

                              (it "discards unapplied buffer patches to prevent ghost diffs"
                                  (let* ((ctx (macher--make-context))
                                         (buf-name "ghost-buffer")
                                         (buf (get-buffer-create buf-name)))
                                    (with-current-buffer buf 
                                      (erase-buffer)
                                      (insert "original state"))
                                    
                                    ;; 1. Simulate the agent proposing a change that hasn't been applied yet
                                    (push (cons buf-name (cons "original state" "proposed ghost state")) 
                                          (macher-context-contents ctx))
                                    (setf (macher-context-dirty-p ctx) t)
                                    
                                    ;; 2. Run the sync (simulating the start of a new turn where the user ignored the patch)
                                    (macher-agent--auto-sync-context ctx)
                                    
                                    ;; 3. Verify the edit was wiped from memory
                                    (let* ((entry (assoc buf-name (macher-context-contents ctx)))
                                           (new-content (cddr entry)))
                                      (expect new-content :to-equal "original state")
                                      (expect (macher-context-dirty-p ctx) :to-be nil))
                                    
                                    (kill-buffer buf))))

                    (it "splits pure buffers from physical files for independent diff generation"
                        (let* ((ctx (macher--make-context))
                               (file-path (expand-file-name "dummy-file.txt" temporary-file-directory))
                               (pure-name "*macher-dummy-buf*")
                               (file-buf (find-file-noselect file-path))
                               (pure-buf (get-buffer-create pure-name)))
                          
                          ;; 1. Add one physical file and one pure buffer to the context
                          (push (cons file-path (cons "a" "b")) (macher-context-contents ctx))
                          (push (cons pure-name (cons "x" "y")) (macher-context-contents ctx))
                          
                          ;; 2. Run the splitter
                          (let* ((split (macher-agent--split-context ctx))
                                 (file-ctx (car split))
                                 (buf-ctx (cdr split)))
                            
                            ;; 3. Verify physical files went to the left (car)
                            (expect (assoc file-path (macher-context-contents file-ctx)) :to-be-truthy)
                            (expect (assoc pure-name (macher-context-contents file-ctx)) :to-be nil)
                            
                            ;; 4. Verify pure buffers went to the right (cdr)
                            (expect (assoc pure-name (macher-context-contents buf-ctx)) :to-be-truthy)
                            (expect (assoc file-path (macher-context-contents buf-ctx)) :to-be nil))
                          
                          (kill-buffer file-buf)
                          (kill-buffer pure-buf)))

                    (it "suppresses diff presentation when executing as a subagent"
                        (let* ((ctx (macher--make-context :dirty-p t))
                               (fsm (gptel-make-fsm)))
                          (push (cons "*subagent-edit*" (cons "a" "b")) (macher-context-contents ctx))
                          (spy-on 'macher--build-patch)
                          
                          ;; When not a subagent, it should call macher--build-patch
                          (setq-default macher-agent--is-subagent nil)
                          (macher-agent-process-request-split 'complete ctx fsm)
                          (expect 'macher--build-patch :to-have-been-called)
                          
                          ;; Reset spy
                          (spy-on 'macher--build-patch)
                          
                          ;; When a subagent, it should NOT call macher--build-patch
                          (setq macher-agent--is-subagent t)
                          (macher-agent-process-request-split 'complete ctx fsm)
                          (expect 'macher--build-patch :not :to-have-been-called)
                          (setq macher-agent--is-subagent nil)))

                    (it "forcefully injects :macher--context into the FSM info plist to awaken the native UI"
                        (let* ((buf (generate-new-buffer "test-bridge"))
                               (fsm (gptel-make-fsm :info (list :buffer buf)))
                               (ctx (macher--make-context :dirty-p t))
                               (get-context (lambda () ctx)))
                          (with-current-buffer buf
                            (setq-local macher-agent--is-workspace t))
                          
                          (spy-on 'macher-process-request)
                          
                          ;; 1. Run our Proxy Bridge
                          (macher-agent--bridge-context-advice 
                           (lambda (f get) nil) ; Mock the native orig-fun
                           fsm 
                           get-context)
                          
                          ;; 2. Extract the termination handler safely from the spy records.
                          ;; FIX: spy-calls-args-for requires the exact call index (0)
                          (let* ((args (spy-calls-args-for 'macher--add-termination-handler 0))
                                 (term-handler (cadr args)))
                            
                            (funcall term-handler fsm)
                            
                            ;; 3. Verify the bridge successfully injected the context into the precise native slot
                            (let ((info (gptel-fsm-info fsm)))
                              (expect (plist-get info :macher--context) :to-be ctx))
                            
                            ;; 4. Verify the native UI engine was called
                            (expect 'macher-process-request :to-have-been-called-with 'complete fsm)))))



          (describe "Sandbox Execution (macher-agent-context-tools.el)"
                    (describe "read_media_in_workspace"
                              (it "errors if gptel-track-media is nil"
                                  (let* ((gptel-track-media nil)
                                         (ctx (macher--make-context :contents '(("test.png" . ("" . "img-data")))))
                                         (tool-fn (gptel-tool-function macher-agent-read-media-in-workspace-tool)))
                                    (spy-on 'macher-agent-current-context :and-return-value ctx)
                                    (let ((result (funcall tool-fn "test.png")))
                                      (expect result :to-match "gptel media send option is off"))))
                              (it "errors if the image is outside allowed context"
                                  (let* ((gptel-track-media t)
                                         (ctx (macher--make-context :contents nil))
                                         (tool-fn (gptel-tool-function macher-agent-read-media-in-workspace-tool)))
                                    (spy-on 'macher-agent-current-context :and-return-value ctx)
                                    (expect (funcall tool-fn "test.png") :to-throw 'error)))
                              (it "stages media in the pending global alist instead of polluting gptel-context"
                                  (let* ((gptel-track-media t)
                                         (gptel-context nil)
                                         (macher-agent--pending-tool-media-alist nil)
                                         (ctx (macher--make-context :contents '(("test.png" . ("" . "img-data")))))
                                         (tool-fn (gptel-tool-function macher-agent-read-media-in-workspace-tool)))
                                    (spy-on 'macher-agent-current-context :and-return-value ctx)
                                    (spy-on 'mailcap-file-name-to-mime-type :and-return-value "image/png")
                                    (spy-on 'file-exists-p :and-return-value t)
                                    (let ((result (funcall tool-fn "test.png")))
                                      (expect result :to-match "SUCCESS: Media")
                                      (expect gptel-context :to-be nil)
                                      (expect (alist-get (current-buffer) macher-agent--pending-tool-media-alist) :to-be-truthy))))

                              (it "injects pending media into FSM payload and clears the queue mid-flight"
                                  (let* ((buf (current-buffer))
                                         (macher-agent--pending-tool-media-alist (list (cons buf '(("/test.png" :mime "image/png")))))
                                         (info (list :buffer buf :backend 'mock-backend :data (list :messages ["a"])))
                                         (fsm (gptel-make-fsm :info info))
                                         (orig-called nil)
                                         (orig-fun (lambda (f &rest _) (setq orig-called f))))
                                    
                                    (spy-on 'gptel--inject-media)
                                    (spy-on 'gptel--inject-prompt)
                                    
                                    (macher-agent--inject-media-fsm-advice orig-fun fsm)
                                    
                                    (expect 'gptel--inject-media :to-have-been-called)
                                    (expect 'gptel--inject-prompt :to-have-been-called)
                                    (expect orig-called :to-equal fsm)
                                    (expect (alist-get buf macher-agent--pending-tool-media-alist) :to-be nil))))

                    (describe "rsync command building"
                              (it "constructs a safe rsync command with all necessary exclusions"
                                  (let* ((src "/my/project/")
                                         (dest "/tmp/sandbox/")
                                         (cmd (macher-agent--build-rsync-cmd src dest)))
                                    (expect cmd :to-match "^rsync -a")
                                    (expect cmd :to-match "--exclude=\\.git/")
                                    (expect cmd :to-match "--exclude=node_modules/")))))

          (describe "Interactive Commands & State (macher-agent-orchestration.el)"
                    (it "macher-agent-add-buffer-to-scope explicitly errors out if no existing session is found"
                        (let ((buf (generate-new-buffer "lazy-target")))
                          (let ((macher--fsm-latest nil)
                                (macher-agent--persistent-context nil))
                            (cl-letf (((symbol-function 'buffer-list) (lambda () nil)))
                              (expect (macher-agent-add-buffer-to-scope "lazy-target") :to-throw 'error)))
                          (kill-buffer buf)))
                    (it "macher-agent-add-subagent creates a buffer and tracks it globally"
                        (let ((buf (macher-agent-add-subagent "test-worker" "/tmp/")))
                          (expect (buffer-live-p buf) :to-be t)
                          (expect (assoc "test-worker" macher-agent-active-subagents) :to-be-truthy)
                          (kill-buffer buf)))

                    (it "macher-agent-apply-virtual-buffers applies pending context edits to live Emacs buffers"
                        (let* ((buf (generate-new-buffer "live-target"))
                               (ctx (macher--make-context :contents (list (cons (buffer-name buf) (cons "old" "new text"))))))
                          (with-current-buffer buf (insert "old"))
                          
                          (setq macher--fsm-latest (gptel-make-fsm))
                          (spy-on 'macher-agent--fsm-get-context :and-return-value ctx)
                          (spy-on 'macher-agent--auto-sync-context)
                          
                          (macher-agent-apply-virtual-buffers)
                          
                          (with-current-buffer buf
                            (expect (buffer-string) :to-equal "new text"))
                          (kill-buffer buf)))

                    (it "clears persistent context and latest FSM upon user request"
                        (let ((buf (generate-new-buffer "active-session")))
                          (with-current-buffer buf
                            (setq-local macher-agent--persistent-context 'some-data)
                            (setq-local macher--fsm-latest 'some-fsm)
                            (macher-agent-clear-context)
                            (expect macher-agent--persistent-context :to-be nil)
                            (expect macher--fsm-latest :to-be nil))
                          (kill-buffer buf))))

          (describe "Tool Signatures (Macro Contracts)"
                    (before-all
                     (macher-agent-define-tool mock-async-contract-tool
                                               ("Mock async tool" "test" :args '((:name "arg1" :type string) (:name "arg2" :type string)) :async t)
                                               (arg1 arg2)
                                               (funcall gptel-callback (format "Async %s %s" arg1 arg2)))

                     (macher-agent-define-tool mock-sync-contract-tool
                                               ("Mock sync tool" "test" :args '((:name "arg1" :type string)))
                                               (arg1)
                                               (format "Sync %s" arg1)))

                    (it "generates exact signatures for async tools (callback + args)"
                        (let* ((tool-fn (gptel-tool-function mock-async-contract-tool))
                               (arity (func-arity tool-fn)))
                          (expect (car arity) :to-equal 3)
                          (expect (cdr arity) :to-equal 3)))

                    (it "generates exact signatures for sync tools (only args)"
                        (let* ((tool-fn (gptel-tool-function mock-sync-contract-tool))
                               (arity (func-arity tool-fn)))
                          (expect (car arity) :to-equal 1)
                          (expect (cdr arity) :to-equal 1)))))



(provide 'macher-agent-test)
