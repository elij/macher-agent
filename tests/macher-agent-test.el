;;; macher-agent-test.el --- Comprehensive BDD tests for Macher-Agent -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'buttercup)
(require 'macher-agent-macher-bridge)
(require 'macher-agent)
(require 'macher-agent-vfs-client)
(require 'macher-agent-gptel-tools)
(require 'macher-agent-orchestration)


(describe "Macher-Agent BDD Test Suite"

          (before-each
           (spy-on 'macher-action)
           (spy-on 'gptel-send)
           (spy-on 'macher--add-termination-handler)
           (setq macher-agent--persistent-context nil)
           (let* ((ctx (ignore-errors (macher-agent-resolve-context)))
                  (ws (when ctx (macher-agent--get-context-workspace ctx))))
             (when ws (setf (macher-agent-workspace-active-subagents ws) nil))))

          (describe "Context and Security (macher-agent-vfs-client.el)"
                    (it "throws a security error if accessing a path outside of the allowed context"
                        (let ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "allowed.txt" "old" "new")))))
                          (expect (macher-agent--ensure-access ctx "forbidden.txt") :to-throw 'error)))

                    (it "successfully records a virtual edit to an existing scoped buffer"
                        (let* ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "test.txt" "orig" "orig")))))
                          (macher-agent--update-context-file ctx "test.txt" "modified")
                          (expect (macher-context-dirty-p ctx) :to-be t)
                          (expect (macher-agent-vfs-entry-curr (cl-find "test.txt" (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)) :to-equal "modified")))

                    (it "resolves context matching the active project root rather than selecting an arbitrary workspace"
                        (let* ((proj1-dir "/mock/proj1/")
                               (proj2-dir "/mock/proj2/")
                               (ws1 (make-macher-agent-workspace :project-root proj1-dir))
                               (ws2 (make-macher-agent-workspace :project-root proj2-dir))
                               (ctx1 (macher-agent--make-vfs-context :workspace ws1 :contents nil))
                               (ctx2 (macher-agent--make-vfs-context :workspace ws2 :contents nil))
                               (buf1 (generate-new-buffer "buf1"))
                               (buf2 (generate-new-buffer "buf2")))
                          (puthash (expand-file-name proj1-dir) ctx1 macher-agent-active-workspaces)
                          (puthash (expand-file-name proj2-dir) ctx2 macher-agent-active-workspaces)
                          (with-current-buffer buf1
                            (setq-local macher-agent--is-workspace t)
                            (setq-local macher-agent--persistent-context ctx1))
                          (with-current-buffer buf2
                            (setq-local macher-agent--is-workspace t)
                            (setq-local macher-agent--persistent-context ctx2))
                          (unwind-protect
                              (let ((default-directory proj2-dir))
                                ;; When we call macher-agent-resolve-context, it should resolve to ctx2 because proj2-dir is active
                                (expect (macher-agent-resolve-context) :to-be ctx2))
                            (kill-buffer buf1)
                            (kill-buffer buf2))))

                    (it "bypasses UI and reaper flag when spawning background tasks"
                        (let* ((buf (generate-new-buffer "subagent-bg-buf"))
                               (task `(:buffer_name ,(buffer-name buf) :instructions "run" :background t))
                               (callback-called nil)
                               (ui-shown nil))
                          (spy-on 'macher-agent--show-ui :and-call-fake (lambda (&rest _args) (setq ui-shown t)))
                          (spy-on 'macher-agent-gptel-transmit :and-call-fake
                                  (lambda (_ctx plist)
                                    (funcall (plist-get plist :on-success) "success-result")))
                          (unwind-protect
                              (progn
                                (macher-agent-spawn-task task (lambda (_res) (setq callback-called t)))
                                (expect callback-called :to-be t)
                                (expect ui-shown :to-be nil)
                                (with-current-buffer buf
                                  (expect macher-agent--ready-to-reap :to-be nil)))
                            (kill-buffer buf))))

                    (describe "Three-way Merge Logic"
                              (it "invalidates the local cache if both local and remote diverged"
                                  (let* ((test-dir (make-temp-file "macher-test-dir" t))
                                         (test-file (expand-file-name "test.txt" test-dir))
                                         (ctx (macher--make-context :dirty-p t)))
                                    
                                    (setf (macher-context-contents ctx) 
                                          (list (macher-agent-vfs-make-entry test-file "v1" "v2-local")))
                                    
                                    (with-temp-file test-file (insert "v2-remote"))
                                    
                                    (macher-agent--auto-sync-context ctx)
                                    
                                    (let ((entry (cl-find test-file (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                                      (expect (macher-agent-vfs-entry-orig entry) :to-equal "v2-remote")
                                      (expect (macher-agent-vfs-entry-curr entry) :to-equal "v2-remote"))
                                    
                                    (delete-directory test-dir t)))

                              (it "preserves unapplied virtual edits across tool calls if the physical state has not mutated"
                                  (let* ((entry (macher-agent-vfs-make-entry "test-file.el" "original state" "proposed ghost state")))
                                    
                                    ;; Mock the disk returning the exact same original state
                                    (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "original state")
                                    
                                    (macher-agent--sync-context-entry entry)
                                    
                                    ;; The agent's unapplied virtual edit MUST survive!
                                    (expect (macher-agent-vfs-entry-curr entry) :to-equal "proposed ghost state")))

                              (it "invalidates edits and prevents ghost diffs if the underlying buffer or file is destroyed"
                                  (let* ((entry (macher-agent-vfs-make-entry "test-file.el" "original state" "proposed ghost state")))
                                    
                                    ;; Mock the buffer being killed or file being deleted (returns nil)
                                    (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value nil)
                                    
                                    (macher-agent--sync-context-entry entry)
                                    
                                    ;; The concurrency control detects the change and wipes the ghost edits
                                    (expect (macher-agent-vfs-entry-orig entry) :to-be nil)
                                    (expect (macher-agent-vfs-entry-curr entry) :to-be nil)))

                              (it "splits pure buffers from physical files for independent diff generation"
                                  (let* ((ctx (macher--make-context))
                                         (file-path (expand-file-name "dummy-file.txt" temporary-file-directory))
                                         (pure-name "*macher-dummy-buf*")
                                         (file-buf (find-file-noselect file-path))
                                         (pure-buf (get-buffer-create pure-name)))
                                    
                                    ;; 1. Add one physical file and one pure buffer to the context
                                    (push (macher-agent-vfs-make-entry file-path "a" "b") (macher-context-contents ctx))
                                    (push (macher-agent-vfs-make-entry pure-name "x" "y") (macher-context-contents ctx))
                                    (expect (length (macher-context-contents ctx)) :to-equal 2)
                                    
                                    ;; 2. Run the splitter
                                    (let* ((split (macher-agent--split-context ctx))
                                           (file-ctx (car split))
                                           (buf-ctx (cdr split)))
                                      
                                      ;; 3. Verify physical files went to the left (car)
                                      (expect (length (macher-context-contents file-ctx)) :to-equal 1)
                                      (expect (cl-find file-path (macher-context-contents file-ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :to-be-truthy)
                                      (expect (cl-find pure-name (macher-context-contents file-ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :to-be nil)
                                      
                                      ;; 4. Verify pure buffers went to the right (cdr)
                                      (expect (cl-find pure-name (macher-context-contents buf-ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :to-be-truthy)
                                      (expect (cl-find file-path (macher-context-contents buf-ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :to-be nil))
                                    
                                    (kill-buffer file-buf)
                                    (kill-buffer pure-buf)))

                              (it "triggers the UI safely on completion without modifying the FSM"
                                  (let* ((buf (generate-new-buffer "test-bridge"))
                                         (ctx (macher--make-context :dirty-p t))
                                         (file-path (expand-file-name "test.txt")))
                                    (push (macher-agent-vfs-make-entry file-path "old" "new") (macher-context-contents ctx))
                                    
                                    (with-current-buffer buf
                                      (setq-local macher-agent--is-workspace t)
                                      (setq-local macher-agent--persistent-context ctx)
                                      (setq-local macher-agent--active-fsm nil))
                                    
                                    (with-current-buffer buf
                                      (macher-agent-apply-virtual-buffers))
                                    (kill-buffer buf))))
                    (describe "Macher-Agent Skill Model Selection"

                              (it "applies the correct model from the skill metadata to gptel-model"
                                  (spy-on 'macher-agent-resolve-context :and-return-value
                                          (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/mock/proj") :contents nil))
                                  (let* ((skill-name 'rust-skill)
                                         ;; Register a skill with a specific model
                                         (skill-data '(:description "Test" :model gpt-4o :has-tools nil :context-dir nil :system "test"))
                                         (execution (macher--make-action-execution :action skill-name)))
                                    
                                    (let ((workspace (macher-agent--get-context-workspace (macher-agent-resolve-context))))
                                      (setf (alist-get skill-name (macher-agent-workspace-skills-alist workspace)) skill-data))
                                    
                                    ;; Execute the initialisation
                                    (with-temp-buffer
                                      (let ((gptel--known-presets nil))
                                        (macher-agent-initialize-skills (macher-agent-resolve-context))
                                        
                                        (let ((preset-def (alist-get skill-name gptel--known-presets)))
                                          (expect preset-def :not :to-be nil)
                                          (expect (plist-get preset-def :model) :to-equal 'gpt-4o))))))

                              (it "does not change gptel-model if no model is specified in the skill"
                                  (spy-on 'macher-agent-resolve-context :and-return-value
                                          (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/mock/proj") :contents nil))
                                  (let* ((skill-name 'plain-skill)
                                         ;; Register a skill with NO model
                                         (skill-data '(:description "Test" :model nil :has-tools nil :context-dir nil :system "test"))
                                         (execution (macher--make-action-execution :action skill-name))
                                         (original-model gptel-model))
                                    
                                    (let ((workspace (macher-agent--get-context-workspace (macher-agent-resolve-context))))
                                      (setf (alist-get skill-name (macher-agent-workspace-skills-alist workspace)) skill-data))
                                    
                                    (with-temp-buffer
                                      (let ((gptel--known-presets nil))
                                        (macher-agent-initialize-skills (macher-agent-resolve-context))
                                        
                                        (let ((preset-def (alist-get skill-name gptel--known-presets)))
                                          (expect preset-def :not :to-be nil)
                                          ;; plist-member returns the tail if found, nil if not. Since model is not present, it should not exist in the plist.
                                          (expect (plist-member preset-def :model) :to-be nil)))))))
                    (describe "Virtual File System (VFS) Concurrency Control"
                              
                              (describe "macher-agent--sync-context-entry"
                                        
                                        (it "preserves unapplied virtual edits if the physical disk has NOT mutated"
                                            (let* ((entry (macher-agent-vfs-make-entry "test.el" "original state" "agent edit")))
                                              
                                              ;; Mock the disk returning the exact same original state
                                              (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "original state")
                                              
                                              (let ((mutated (macher-agent--sync-context-entry entry)))
                                                (expect mutated :to-be nil)
                                                (expect (macher-agent-vfs-entry-orig entry) :to-equal "original state")
                                                (expect (macher-agent-vfs-entry-curr entry) :to-equal "agent edit"))))

                                        (it "fast-forwards a clean virtual memory if the physical disk mutates naturally"
                                            (let* ((entry (macher-agent-vfs-make-entry "test.el" "original state" "original state")))
                                              
                                              ;; Mock a physical edit happening while the agent had NO pending edits
                                              (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "new physical state")
                                              
                                              (let ((mutated (macher-agent--sync-context-entry entry)))
                                                (expect mutated :to-be t)
                                                (expect (macher-agent-vfs-entry-orig entry) :to-equal "new physical state")
                                                (expect (macher-agent-vfs-entry-curr entry) :to-equal "new physical state"))))

                                        (it "OPTIMISTIC CONCURRENCY: invalidates virtual edits if a hostile physical mutation occurs"
                                            (let* ((entry (macher-agent-vfs-make-entry "test.el" "original state" "agent edit")))
                                              
                                              ;; Mock the user manually editing the file while the agent was thinking
                                              (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "user physical edit")
                                              
                                              (let ((mutated (macher-agent--sync-context-entry entry)))
                                                ;; The system MUST detect the conflict and aggressively drop the agent's delta
                                                (expect mutated :to-be t)
                                                (expect (macher-agent-vfs-entry-orig entry) :to-equal "user physical edit")
                                                (expect (macher-agent-vfs-entry-curr entry) :to-equal "user physical edit"))))

                                        (it "fast-forwards virtual memory if the physical mutation perfectly matches the virtual delta (patch applied)"
                                            (let* ((entry (macher-agent-vfs-make-entry "test.el" "original state" "agent edit")))
                                              
                                              ;; Mock the state immediately after the user applies the patch.
                                              ;; The disk now matches the agent's unapplied edit perfectly.
                                              (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "agent edit")
                                              
                                              (let ((mutated (macher-agent--sync-context-entry entry)))
                                                ;; The system MUST recognise the patch was applied and fast-forward the baseline.
                                                ;; It returns t (mutated) and updates orig to match new, preventing duplicate patches.
                                                (expect mutated :to-be t)
                                                (expect (macher-agent-vfs-entry-orig entry) :to-equal "agent edit")
                                                (expect (macher-agent-vfs-entry-curr entry) :to-equal "agent edit")))))

                              )
                    (describe "Macher-Agent Tool Registry Resilience"
                              
                              (it "ensures custom tools survive the preset purge and retain correct category"
                                  (let* (;; 1. Define a tool mimicking your agent tools
                                         (custom-tool (gptel-make-tool 
                                                       :name "cargo_check_tool" 
                                                       :function #'ignore 
                                                       :category "macher-agent-rust" 
                                                       :description "Test tool" 
                                                       :args nil))
                                         ;; 2. Simulate the clearing function used by presets like macher-ro
                                         (clear-fn (plist-get (plist-get macher--preset-clear-tools :tools) :function))
                                         ;; 3. Simulate a scenario where a preset attempts to purge everything but 'macher' category tools
                                         (tools-list (list custom-tool 
                                                           (gptel-make-tool :name "native_tool" :function #'ignore :category "macher" :description "native" :args nil)))
                                         (filtered-tools (funcall clear-fn tools-list)))
                                    
                                    ;; PROOF: The custom tool MUST survive because it does not match the 'macher' category purge
                                    (expect (seq-find (lambda (tool) (string= (gptel-tool-name tool) "cargo_check_tool")) filtered-tools) :not :to-be nil)
                                    (expect (gptel-tool-category custom-tool) :to-equal "macher-agent-rust")))

                              (it "verifies that tools identified as 'macher' category get context injected"
                                  (let* ((mock-fsm (gptel-make-fsm))
                                         (mock-tool (gptel-make-tool :name "test_tool" 
                                                                     :function (lambda (ctx) ctx) 
                                                                     :category "macher" 
                                                                     :description "test" :args nil))
                                         (mock-context 'injected-context))
                                    
                                    (setf (gptel-fsm-info mock-fsm) (list :tools (list mock-tool) :buffer (current-buffer)))
                                    
                                    ;; Run the macher setup logic
                                    (macher--setup-tools mock-fsm (lambda () mock-context))
                                    
                                    (let* ((processed-tools (plist-get (gptel-fsm-info mock-fsm) :tools))
                                           (processed-tool (car processed-tools)))
                                      
                                      ;; PROOF: The tool has been wrapped and is now expecting the context
                                      (expect (funcall (gptel-tool-function processed-tool)) :to-equal 'injected-context))))

                              (it "correctly initialises and binds the context during session restoration, preventing buffer-list fallback leakage"
                                  (let* ((other-buf (generate-new-buffer "other-chat-buffer"))
                                         (restore-buf (generate-new-buffer "restored-chat-buffer"))
                                         (other-dir "/mock/proj/other")
                                         (restore-dir "/mock/proj/restore")
                                         (macher-agent--allow-gptel-restore t)
                                         (orig-called nil)
                                         (orig-fun (lambda (&rest _args) (setq orig-called t))))
                                    
                                    (unwind-protect
                                        (progn
                                          ;; 1. Set up the other buffer to simulate an active workspace that could be leaked
                                          (with-current-buffer other-buf
                                            (setq-local default-directory other-dir)
                                            (macher-agent--init-workspace-state other-dir))
                                          
                                          ;; 2. Set up the restored buffer with its correct default-directory
                                          (with-current-buffer restore-buf
                                            (setq-local default-directory restore-dir)
                                            
                                            ;; Verify that prior to restore advice, persistent-context is nil
                                            (expect (bound-and-true-p macher-agent--persistent-context) :to-be nil)
                                            
                                            ;; Execute the restore advice
                                            (macher-agent--gptel-restore-advice orig-fun)
                                            
                                            ;; Verify the original restore function was called
                                            (expect orig-called :to-be t)
                                            
                                            ;; Verify that the correct workspace context was created and bound
                                            (let ((local-ctx (bound-and-true-p macher-agent--persistent-context)))
                                              (expect local-ctx :not :to-be nil)
                                              (let ((workspace (macher-agent--get-context-workspace local-ctx)))
                                                (expect (macher-agent-root workspace) :to-equal (expand-file-name restore-dir))))))
                                      
                                      ;; Clean up temp buffers
                                      (when (buffer-live-p other-buf) (kill-buffer other-buf))
                                      (when (buffer-live-p restore-buf) (kill-buffer restore-buf))))))
                    (describe "Sandbox Execution (macher-agent-vfs-client.el)"
                              (describe "read_media_in_workspace"
                                        (it "errors if gptel-track-media is nil"
                                            (let* ((gptel-track-media nil)
                                                   (ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "test.png" "" "img-data"))))
                                                   (tool-fn (gptel-tool-function macher-agent-read-media-in-workspace-tool)))
                                              (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                                              (let ((result (funcall tool-fn "test.png")))
                                                (expect result :to-match "gptel media send option is off"))))
                                        (it "permits access to valid media files inside the workspace without triggering VFS text security locks"
                                            (let* ((gptel-track-media t)
                                                   (session (make-macher-agent-session :id "test"))
                                                   (macher-agent--active-fsm 'mock-fsm)
                                                   (mock-info (list :macher-agent-session session))
                                                   (ctx (macher--make-context :contents nil))
                                                   (tool-fn (gptel-tool-function macher-agent-read-media-in-workspace-tool)))
                                              (spy-on 'gptel-fsm-info :and-return-value mock-info)
                                              (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                                              (spy-on 'macher-agent-context-classify-entry :and-return-value 'media)
                                              (spy-on 'file-exists-p :and-return-value t)
                                              (spy-on 'mailcap-file-name-to-mime-type :and-return-value "image/png")
                                              (spy-on 'insert-file-contents-literally :and-call-fake (lambda (&rest _args) (insert "mock-image-data")))
                                              (let ((result (funcall tool-fn "test_workspace_image.png")))
                                                (expect result :to-match "SUCCESS: Media 'test_workspace_image.png'"))))
                                        (it "throws an error if the tool cannot determine MIME type"
                                            (let* ((gptel-track-media t)
                                                   (ctx (macher--make-context :contents nil))
                                                   (tool-fn (gptel-tool-function macher-agent-read-media-in-workspace-tool)))
                                              (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                                              (spy-on 'file-exists-p :and-return-value t)
                                              (spy-on 'mailcap-file-name-to-mime-type :and-return-value nil)
                                              (let ((result (funcall tool-fn "unauthorized_script.sh")))
                                                (expect result :to-match "Could not determine MIME type"))))
                                        (it "stages media in the session pending-media instead of polluting gptel-context"
                                            (let* ((gptel-track-media t)
                                                   (gptel-context nil)
                                                   (session (make-macher-agent-session :id "test"))
                                                   (macher-agent--active-fsm 'mock-fsm)
                                                   (mock-info (list :macher-agent-session session))
                                                   (ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "test.png" "" "img-data"))))
                                                   (tool-fn (gptel-tool-function macher-agent-read-media-in-workspace-tool)))
                                              (spy-on 'gptel-fsm-info :and-return-value mock-info)
                                              (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                                              (spy-on 'mailcap-file-name-to-mime-type :and-return-value "image/png")
                                              (spy-on 'file-exists-p :and-return-value t)
                                              (let ((result (funcall tool-fn "test.png")))
                                                (expect result :to-match "SUCCESS: Media")
                                                (expect gptel-context :to-be nil)
                                                (expect (car (car (macher-agent-session-pending-media session))) :to-equal "aW1nLWRhdGE="))))

                                        (it "injects pending media into FSM payload and clears the queue pre-flight"
                                            (let* ((buf (current-buffer))
                                                   (mock-backend 'mock-backend)
                                                   (mock-data '((:role "system" :content "sys")))
                                                   (session (make-macher-agent-session :id "test" :pending-media '(("mockbase64" :mime "image/png"))))
                                                   (mock-info (list :buffer buf :backend mock-backend :data mock-data :macher-agent-session session))
                                                   
                                                   ;; We can safely use a mock symbol again!
                                                   (mock-fsm 'mock-fsm)
                                                   
                                                   (orig-called nil)
                                                   (orig-fun (lambda (fsm &rest _) (setq orig-called fsm))))
                                              
                                              ;; Spy on the accessor to intercept the dynamic funcall
                                              (spy-on 'gptel-fsm-info :and-return-value mock-info)
                                              (spy-on 'gptel--inject-media)
                                              (spy-on 'gptel--inject-prompt)
                                              
                                              ;; Execute the restored advice
                                              (macher-agent--inject-media-fsm-advice orig-fun mock-fsm)
                                              
                                              ;; Verify injection lifecycle
                                              (expect 'gptel-fsm-info :to-have-been-called-with mock-fsm)
                                              (expect 'gptel--inject-media :to-have-been-called)
                                              (expect 'gptel--inject-prompt :to-have-been-called)
                                              (expect orig-called :to-equal mock-fsm)
                                              (expect (macher-agent-session-pending-media session) :to-be nil))))

                              (describe "rsync command building"
                                        
                                        (it "constructs a shell string using git ls-files"
                                            ;; Intercept call-process to return 0 (success).
                                            ;; This simulates a valid git repository and bypasses the physical directory check.
                                            (spy-on 'call-process :and-return-value 0)
                                            
                                            (let* ((src "/my/project/")
                                                   (dest "/tmp/sandbox/")
                                                   (cmd (macher-agent--build-rsync-cmd src dest)))
                                              
                                              (expect (stringp cmd) :to-be t)
                                              (expect (string-match-p "git ls-files" cmd) :to-be-truthy)
                                              (expect (string-match-p "rsync -aLC --delete --files-from=-" cmd) :to-be-truthy)
                                              
                                              ;; Optional but good practice: verify our mock was actually triggered
                                              (expect 'call-process :to-have-been-called-with
                                                      "git" nil nil nil "rev-parse" "--is-inside-work-tree")))))

                    (describe "Macher-Agent Tool Category Isolation"
                              
                              (it "preserves the custom category to avoid being purged by upstream read-only presets"
                                  (let ((mock-tool (gptel-make-tool :name "my_custom_tool" 
                                                                    :function #'ignore 
                                                                    :category "macher-agent-calendar" 
                                                                    :description "test" 
                                                                    :args nil))
                                        ;; Extract the clearing function from the upstream preset definition
                                        (clear-fn (plist-get (plist-get macher--preset-clear-tools :tools) :function)))
                                    
                                    ;; The custom tool MUST maintain its distinct category boundary
                                    (expect (gptel-tool-category mock-tool) :not :to-equal macher-tool-category)
                                    
                                    ;; It MUST survive the upstream framework's aggressive tool purge
                                    (let ((filtered-tools (funcall clear-fn (list mock-tool))))
                                      (expect (length filtered-tools) :to-equal 1)
                                      (expect (gptel-tool-name (car filtered-tools)) :to-equal "my_custom_tool")))))
                    (describe "Virtual File System (VFS) Sandbox Isolation"

                              (describe "deletions and moves"
                                        (it "preserves explicit nil values to correctly register deletions"
                                            (let* ((entry (cons "test.txt" nil))
                                                   (hydrated (macher-agent--hydrate-vfs-entry entry "/mock/root")))
                                              (expect (macher-agent-vfs-entry-curr hydrated) :to-be nil)))

                                        (it "physically removes a file from sandbox during process-entries if new-content is nil"
                                            (let* ((entries '("test.txt"))
                                                   (sandbox-dir (make-temp-file "macher-test-sandbox-" t))
                                                   (target-file (expand-file-name "test.txt" sandbox-dir)))
                                              (with-temp-file target-file (insert "existing"))
                                              (expect (file-exists-p target-file) :to-be t)
                                              (macher-agent--vfs-process-entries
                                               entries
                                               sandbox-dir
                                               (lambda (e) e)
                                               (lambda (_) nil))
                                              (expect (file-exists-p target-file) :to-be nil)
                                              (delete-directory sandbox-dir t))))

                              (describe "macher-agent--vfs-apply-overlay"
                                        
                                        (it "reroutes virtual edits to the ephemeral sandbox, protecting the physical disk"
                                            (let* ((workspace-root "/my/project/")
                                                   (sandbox-dir "/tmp/sandbox-12345/")
                                                   ;; Create a mock context representing a dirty workspace
                                                   (mock-ws (cons 'agent workspace-root))
                                                   (mock-ctx (macher--make-context :dirty-p t 
                                                                                   :workspace mock-ws
                                                                                   :contents (list (macher-agent-vfs-make-entry "/my/project/src/main.rs" "orig" "new content"))))
                                                   (write-region-called-with nil))
                                              
                                              ;; Spy on file-in-directory-p to allow mock/non-existent sandbox directories
                                              (spy-on 'file-in-directory-p :and-return-value t)

                                              ;; Mock the context root provider (struct access)
                                              (spy-on 'macher--workspace-root :and-return-value workspace-root)
                                              
                                              ;; Intercept the destructive write action
                                              (spy-on 'write-region :and-call-fake 
                                                      (lambda (start _end filename &rest _)
                                                        (push (list start filename) write-region-called-with)))
                                              
                                              (macher-agent--vfs-apply-overlay mock-ctx sandbox-dir)
                                              
                                              ;; The orchestrator MUST execute a file write...
                                              (expect 'write-region :to-have-been-called)
                                              
                                              ;; ...it MUST write the new virtual content...
                                              (expect (caar write-region-called-with) :to-equal "new content")
                                              
                                              ;; ...and CRITICALLY, it MUST write it to the sandbox, NOT the physical /my/project/ path!
                                              (expect (cadar write-region-called-with) :to-equal "/tmp/sandbox-12345/src/main.rs")))

                                        (it "does not flush anything if the virtual memory is clean"
                                            (let* ((mock-ctx (macher--make-context :dirty-p nil :contents nil)))
                                              
                                              (spy-on 'write-region)
                                              
                                              (macher-agent--vfs-apply-overlay mock-ctx "/tmp/sandbox-12345/")
                                              
                                              ;; Ensure no ghost files are created in the sandbox
                                              (expect 'write-region :not :to-have-been-called))))

                              (describe "VFS Strict Pipeline Execution"
                                        (it "always executes the 3-step composition in exact order for every tool"
                                            (let ((call-order nil))
                                              (spy-on 'macher-agent--vfs-verify-clean-merge :and-call-fake (lambda (&rest _) (push 'merge call-order)))
                                              (spy-on 'macher-agent--vfs-sync-baseline :and-call-fake (lambda (&rest _) (push 'sync call-order)))
                                              (spy-on 'macher-agent--vfs-apply-overlay :and-call-fake (lambda (&rest _) (push 'overlay call-order)))
                                              
                                              ;; Execute a dummy tool
                                              (let ((mock-context (macher--make-context :workspace nil :contents nil)))
                                                (spy-on 'macher-agent-context-root :and-return-value "/my/project/")
                                                (spy-on 'make-temp-file :and-return-value "/tmp/sandbox-12345/")
                                                (spy-on 'delete-directory)
                                                (spy-on 'shell-command-to-string :and-return-value "running")
                                                
                                                (macher-agent-with-strict-vfs-pipeline mock-context
                                                                                       (shell-command-to-string "echo 'running'")))
                                              
                                              ;; 1. Assert they were all called
                                              (expect 'macher-agent--vfs-verify-clean-merge :to-have-been-called)
                                              (expect 'macher-agent--vfs-sync-baseline :to-have-been-called)
                                              (expect 'macher-agent--vfs-apply-overlay :to-have-been-called)
                                              
                                              ;; 2. Assert exact execution order
                                              (expect (reverse call-order) :to-equal '(merge sync overlay))))
                                        
                                        (it "does not mangle OS-level absolute sandbox paths during rsync"
                                            ;; This specific test prevents the regression we just experienced
                                            (spy-on 'macher-agent--build-rsync-cmd :and-return-value "echo dummy")
                                            (spy-on 'delete-directory)
                                            
                                            (let ((mock-context (macher--make-context :workspace nil :contents nil)))
                                              (spy-on 'macher-agent-context-root :and-return-value "/my/project/")
                                              (macher-agent-with-strict-vfs-pipeline mock-context nil))
                                            
                                            ;; The destination argument to rsync MUST be an absolute path in /tmp or /var
                                            (let ((rsync-dest-arg (nth 1 (spy-calls-args-for 'macher-agent--build-rsync-cmd 0))))
                                              (expect (file-name-absolute-p rsync-dest-arg) :to-be t)))))
                    (describe "Interactive Commands and State (macher-agent-orchestration.el)"
                              (it "macher-agent-add-buffer-to-scope explicitly errors out if no existing session is found"
                                  (let ((buf (generate-new-buffer "lazy-target")))
                                    (let ((macher-agent--active-fsm nil)
                                          (macher-agent-active-workspaces (make-hash-table :test 'equal))
                                          (macher-agent--persistent-context nil))
                                      (cl-letf (((symbol-function 'buffer-list) (lambda () nil)))
                                        (expect (macher-agent-add-buffer-to-scope "lazy-target") :to-throw 'error)))
                                    (kill-buffer buf)))
                              (it "macher-agent-add-subagent creates a buffer and tracks it globally"
                                  (let* ((mock-workspace (make-macher-agent-workspace :project-root "/tmp/"))
                                         (mock-context (macher-agent--make-vfs-context :workspace (cons 'agent mock-workspace) :contents nil)))
                                    (spy-on 'macher-agent-resolve-context :and-return-value mock-context)
                                    (let ((buf (macher-agent-add-subagent "test-worker" "/tmp/" nil mock-context)))
                                      (expect (buffer-live-p buf) :to-be t)
                                      (expect (assoc "test-worker" (macher-agent-workspace-active-subagents (macher-agent--get-context-workspace (macher-agent-resolve-context)))) :to-be-truthy)
                                      (kill-buffer buf))))

                              (it "macher-agent-apply-virtual-buffers applies pending context edits to live Emacs buffers"
                                  (let* ((buf (generate-new-buffer "live-target"))
                                         (ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry (buffer-name buf) "old" "new text")))))
                                    (with-current-buffer buf (insert "old"))
                                    
                                    (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                                    (spy-on 'macher-agent--auto-sync-context)
                                    
                                    (macher-agent-apply-virtual-buffers)
                                    
                                    (with-current-buffer buf
                                      (expect (buffer-string) :to-equal "new text"))
                                    (kill-buffer buf)))

                              (it "clears persistent context upon user request"
                                  (let ((buf (generate-new-buffer "active-session")))
                                    (with-current-buffer buf
                                      (setq-local macher-agent--persistent-context 'some-data)
                                      (macher-agent-clear-context)
                                      (expect macher-agent--persistent-context :to-be nil))
                                    (kill-buffer buf)))

                              (it "clears active presets during setup if the restored session tag is present"
                                  (let ((buf (generate-new-buffer "restored-session-buf"))
                                        (ctx (macher--make-context :workspace (make-macher-agent-workspace :project-root "/tmp/") :contents nil)))
                                    (with-current-buffer buf
                                      (setq-local macher-agent--is-workspace t)
                                      (setq-local macher-agent--persistent-context ctx)
                                      (setq-local macher-agent-presets '(some-preset))
                                      (setq-local macher-agent--is-restored-session t)
                                      (macher-agent-setup-gptel-buffer)
                                      (expect macher-agent-presets :to-be nil)
                                      (expect macher-agent--is-restored-session :to-be nil))
                                    (kill-buffer buf)))

                              (it "handles exclusive override flag by resetting accumulated base and preceding state"
                                  (let ((base-state '(:model gpt-3.5-turbo
                                                             :system "Base system message"
                                                             :temperature 0.7
                                                             :max-tokens 100
                                                             :tools ("tool1")
                                                             :known-presets ((preset-a . (:system "Preset A prompt" :temperature 0.5 :tools ("toolA")))
                                                                             (preset-b . (:system "Preset B prompt" :exclusive t :temperature 0.2 :tools ("toolB"))))))
                                        (presets '(preset-a preset-b)))
                                    (let ((payload (macher-agent-compose-payload base-state presets)))
                                      ;; Since preset-b has exclusive t, it resets preset-a and base-state!
                                      (expect (plist-get payload :system) :to-equal "### Skill: preset-b\nPreset B prompt\n")
                                      (expect (plist-get payload :temperature) :to-equal 0.2)
                                      (expect (plist-get payload :tools) :to-equal '("toolB"))))))

                    (describe "Tool Signatures (Macro Contracts)"
                              (before-all
                               (macher-agent-make-tool mock-async-contract-tool
                                   "Mock async tool"
                                 :category "test"
                                 :args (list (list :name "arg1" :type 'string) (list :name "arg2" :type 'string))
                                 :command-fn (lambda (payload)
                                               (make-macher-agent-lisp-result-response :payload (format "Async %s %s" (plist-get payload :arg1) (plist-get payload :arg2)))))

                               (macher-agent-make-tool mock-sync-contract-tool
                                   "Mock sync tool"
                                 :category "test"
                                 :args (list (list :name "arg1" :type 'string))
                                 :command-fn (lambda (payload)
                                               (make-macher-agent-lisp-result-response :payload (format "Sync %s" (plist-get payload :arg1))))))

                              (it "generates variadic signatures for async tools to safely absorb FSM contexts"
                                  (let* ((tool-fn (gptel-tool-function mock-async-contract-tool))
                                         (arity (func-arity tool-fn)))
                                    ;; A variadic &rest signature always has a minimum of 0 and a maximum of 'many
                                    (expect (car arity) :to-equal 0)
                                    (expect (cdr arity) :to-equal 'many)))

                              (it "generates variadic signatures for sync tools to safely absorb FSM contexts"
                                  (let* ((tool-fn (gptel-tool-function mock-sync-contract-tool))
                                         (arity (func-arity tool-fn)))
                                    (expect (car arity) :to-equal 0)
                                    (expect (cdr arity) :to-equal 'many))))

                    (describe "Tool Lifecycle Hooks"
                              (before-each
                               (setq macher-agent-pre-tool-use-hook nil)
                               (setq macher-agent-permission-request-hook nil)
                               (setq macher-agent-post-tool-use-hook nil)
                               (setq macher-agent-post-tool-use-failure-hook nil))

                              (it "runs pre-tool-use-hook and aborts if it returns nil"
                                  (let ((pre-called nil)
                                        (callback-result nil))
                                    (add-hook 'macher-agent-pre-tool-use-hook
                                              (lambda (name args)
                                                (setq pre-called (list name args))
                                                nil))
                                    (funcall (gptel-tool-function mock-sync-contract-tool)
                                             "hello"
                                             (lambda (res) (setq callback-result res)))
                                    (expect pre-called :to-equal (list 'mock-sync-contract-tool '(:arg1 "hello")))
                                    (expect callback-result :to-match "Execution blocked by macher-agent-pre-tool-use-hook")))

                              (it "runs pre-tool-use-hook and aborts if it signals an error"
                                  (let ((callback-result nil))
                                    (add-hook 'macher-agent-pre-tool-use-hook
                                              (lambda (_name _args)
                                                (error "Custom pre error")))
                                    (funcall (gptel-tool-function mock-sync-contract-tool)
                                             "hello"
                                             (lambda (res) (setq callback-result res)))
                                    (expect callback-result :to-match "Execution blocked by error in macher-agent-pre-tool-use-hook: Custom pre error")))

                              (it "runs permission-request-hook and succeeds by default if empty"
                                  (let ((callback-result nil))
                                    (funcall (gptel-tool-function mock-sync-contract-tool)
                                             "hello"
                                             (lambda (res) (setq callback-result res)))
                                    (expect callback-result :to-match "Sync hello")))

                              (it "runs permission-request-hook and aborts if it returns nil"
                                  (let ((perm-called nil)
                                        (callback-result nil))
                                    (add-hook 'macher-agent-permission-request-hook
                                              (lambda (name args)
                                                (setq perm-called (list name args))
                                                nil))
                                    (funcall (gptel-tool-function mock-sync-contract-tool)
                                             "hello"
                                             (lambda (res) (setq callback-result res)))
                                    (expect perm-called :to-equal (list 'mock-sync-contract-tool '(:arg1 "hello")))
                                    (expect callback-result :to-match "Permission denied by macher-agent-permission-request-hook")))

                              (it "runs post-tool-use-hook upon successful execution"
                                  (let ((post-called nil)
                                        (callback-result nil))
                                    (add-hook 'macher-agent-post-tool-use-hook
                                              (lambda (name args result)
                                                (setq post-called (list name args result))))
                                    (funcall (gptel-tool-function mock-sync-contract-tool)
                                             "world"
                                             (lambda (res) (setq callback-result res)))
                                    (expect callback-result :to-match "Sync world")
                                    (expect post-called :to-equal (list 'mock-sync-contract-tool '(:arg1 "world") "Sync world"))))

                              (it "runs post-tool-use-failure-hook if the tool body throws an error"
                                  (macher-agent-make-tool mock-error-tool
                                      "Mock error tool"
                                    :category "test"
                                    :args (list (list :name "arg1" :type 'string))
                                    :command-fn (lambda (_payload)
                                                  (error "Failing intentionally")))
                                  (let ((failure-called nil)
                                        (callback-result nil))
                                    (add-hook 'macher-agent-post-tool-use-failure-hook
                                              (lambda (name args err)
                                                (setq failure-called (list name args err))))
                                    (funcall (gptel-tool-function mock-error-tool)
                                             "fail"
                                             (lambda (res) (setq callback-result res)))
                                    (expect callback-result :to-match "Failing intentionally")
                                    (expect (car failure-called) :to-be 'mock-error-tool)
                                    (expect (cadr failure-called) :to-equal '(:arg1 "fail"))
                                    (expect (error-message-string (caddr failure-called)) :to-equal "Failing intentionally"))))
                    
                    (describe "Agent Skills (macher-agent-skills.el)"
                              (before-each
                               (spy-on 'macher-agent-resolve-context :and-return-value
                                       (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root "/mock/proj") :contents nil)))
                              (it "parses SKILL.md files correctly extracting frontmatter and markdown body"
                                  (let* ((parsed (macher-agent-parse-skill-file "tests/fixtures/skills/global/SKILL.md")))
                                    (expect (plist-get parsed :name) :to-equal "mock-skill")
                                    (expect (plist-get parsed :name-sym) :to-equal 'mock-skill)
                                    (expect (plist-get parsed :description) :to-equal "A mock skill for testing")
                                    (expect (plist-get parsed :allowed-tools) :to-equal '("mock-tool-1" "mock-tool-2"))
                                    (expect (plist-get parsed :body) :to-equal "This is the system prompt for the mock skill.\nIt spans multiple lines.")))

                              (it "resolves global skill tools by loading their script if not registered"
                                  (let* ((loaded-tool-object (gptel-make-tool :name "mock-tool-load" :category "test")))
                                    (setq mock-tool-load-global loaded-tool-object)
                                    (spy-on 'file-exists-p :and-return-value t)
                                    (spy-on 'insert-file-contents :and-call-fake (lambda (f) (insert "(setq mock-tool-load mock-tool-load-global)")))
                                    (let ((resolved (macher-agent-resolve-tool "mock-tool-load" nil "tests/fixtures/skills/global/")))
                                      (expect resolved :to-equal loaded-tool-object))))

                              (it "refuses to load workspace skill tools (security context)"
                                  (let* ((mock-script-dir (expand-file-name "tests/fixtures/skills/workspace/scripts"))
                                         (mock-script-path (expand-file-name "workspace-tool-1.el" mock-script-dir)))
                                    ;; Setup mock script
                                    (make-directory mock-script-dir t)
                                    (with-temp-file mock-script-path
                                      (insert "(setq workspace-tool-1 'workspace-loaded)"))
                                    
                                    ;; Test workspace parsing logic
                                    (macher-agent--load-skill-from-path "tests/fixtures/skills/workspace/" (macher-agent-resolve-context))
                                    (let* ((workspace (macher-agent--get-context-workspace (macher-agent-resolve-context)))
                                           (skill-meta (alist-get 'workspace-skill (macher-agent-workspace-skills-alist workspace))))
                                      (expect (plist-get skill-meta :context-dir) :to-be nil))
                                    
                                    ;; Resolution should fail to load because context-dir is nil,
                                    ;; returning the raw string fallback instead of a loaded tool object.
                                    (let ((resolved (macher-agent-resolve-tool "workspace-tool-1" nil nil)))
                                      (expect resolved :to-equal "workspace-tool-1"))
                                    
                                    (delete-directory mock-script-dir t)))

                              (it "verifies tool resolution hierarchy (workspace shadows package tools)"
                                  (let* ((pkg-dir (make-temp-file "macher-pkg" t))
                                         (ws-dir (make-temp-file "macher-ws" t))
                                         (pkg-scripts (expand-file-name "scripts" pkg-dir))
                                         (ws-scripts (expand-file-name "scripts" ws-dir))
                                         (ws-a (gptel-make-tool :name "tool-a" :category "test1"))
                                         (pkg-b (gptel-make-tool :name "tool-b" :category "test2")))
                                    (setq mock-ws-a-global ws-a)
                                    (setq mock-pkg-b-global pkg-b)
                                    (make-directory pkg-scripts t)
                                    (make-directory ws-scripts t)
                                    ;; Package provides tool-a and tool-b
                                    (with-temp-file (expand-file-name "tool-a.el" pkg-scripts) (insert "(setq tool-a 'pkg-a)"))
                                    (with-temp-file (expand-file-name "tool-b.el" pkg-scripts) (insert "(setq tool-b mock-pkg-b-global)"))
                                    ;; Workspace overrides tool-a
                                    (with-temp-file (expand-file-name "tool-a.el" ws-scripts) (insert "(setq tool-a mock-ws-a-global)"))
                                    
                                    ;; Clear registry
                                    (let* ((ctx (macher-agent-resolve-context))
                                           (ws (macher-agent--get-context-workspace ctx)))
                                      (clrhash (macher-agent-workspace-tools-registry ws)))
                                    
                                    ;; Resolve pkg first, then workspace shadows
                                    (let* ((res-pkg-b (macher-agent-resolve-tool "tool-b" nil pkg-dir))
                                           (res-ws-a (macher-agent-resolve-tool "tool-a" nil ws-dir)))
                                      (expect res-pkg-b :to-equal pkg-b)
                                      (expect res-ws-a :to-equal ws-a))
                                    
                                    (delete-directory pkg-dir t)
                                    (delete-directory ws-dir t)))
                              
                              (it "applies skill tools correctly into gptel-tools when selected"
                                  (let* ((gptel-tools nil)
                                         (gptel--known-presets nil)
                                         (gptel-directives nil)
                                         (mock-tool-obj (if (fboundp 'gptel-make-tool)
                                                            (gptel-make-tool :name "the_tool" :function (lambda () nil) :description "A tool")
                                                          'the-tool)))
                                    (spy-on 'gptel-tool-p :and-return-value t)
                                    (let* ((ctx (macher-agent-resolve-context))
                                           (workspace (macher-agent--get-context-workspace ctx)))
                                      (puthash "selected-tool" mock-tool-obj (macher-agent-workspace-tools-registry workspace))
                                      
                                      (setf (alist-get 'test-preset (macher-agent-workspace-skills-alist workspace))
                                            (list :description "test" :system "Directive body" :tools (list mock-tool-obj) :context-dir nil))
                                      
                                      (with-temp-buffer
                                        (let ((gptel--known-presets nil))
                                          (macher-agent-initialize-skills ctx)
                                          (let ((preset-def (buffer-local-value 'gptel--known-presets (current-buffer))))
                                            (setq preset-def (alist-get 'test-preset preset-def))
                                            (expect preset-def :not :to-be nil)
                                            (expect (plist-get preset-def :tools) :to-equal '(:append ("the_tool")))))))))

                              (it "creates a preset when allowed-tools is provided"
                                  (let* ((mock-dir (make-temp-file "macher-test-skills-preset" t))
                                         (skill-dir (expand-file-name "test-skill" mock-dir)))
                                    (make-directory skill-dir t)
                                    (with-temp-file (expand-file-name "SKILL.md" skill-dir)
                                      (insert "---\nname: my-preset\ndescription: test\nallowed-tools:\n  - some-tool\nmodel: gpt-4o\n---\nPreset body"))
                                    (spy-on 'macher-agent-resolve-tool :and-return-value "some-tool")
                                    (with-temp-buffer
                                      (let ((gptel-directives nil)
                                            (gptel--known-presets nil))
                                        (macher-agent-initialize-skills nil mock-dir)
                                        
                                        (let ((preset-def (alist-get 'my-preset gptel--known-presets)))
                                          (expect preset-def :not :to-be nil)
                                          (expect (plist-get preset-def :system) :to-equal "Preset body")
                                          (expect (plist-get preset-def :model) :to-equal 'gpt-4o)
                                          (expect (plist-get preset-def :tools) :to-equal '(:append ("some-tool"))))
                                        (delete-directory mock-dir t)))))

                              (it "injects directly into gptel-directives when allowed-tools is omitted"
                                  (let* ((mock-dir (make-temp-file "macher-test-skills-directive" t))
                                         (skill-dir (expand-file-name "test-skill" mock-dir)))
                                    (make-directory skill-dir t)
                                    (with-temp-file (expand-file-name "SKILL.md" skill-dir)
                                      (insert "---\nname: my-directive\n---\nDirective body"))
                                    (with-temp-buffer
                                      (let ((gptel-directives nil)
                                            (gptel--known-presets nil))
                                        (macher-agent-initialize-skills nil mock-dir)
                                        (expect (alist-get 'my-directive gptel-directives) :to-equal "Directive body")
                                        (let ((preset-def (alist-get 'my-directive gptel--known-presets)))
                                          (expect preset-def :not :to-be nil)
                                          (expect (plist-get preset-def :system) :to-equal "Directive body"))
                                        (delete-directory mock-dir t))))))))

(provide 'macher-agent-test)
