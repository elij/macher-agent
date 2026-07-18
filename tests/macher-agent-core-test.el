;;; macher-agent-core-test.el --- Core behaviour tests for macher-agent -*- lexical-binding: t -*-

;; This test suite enforces the specification for macher-agent,
;; focusing on VFS optimistic concurrency, lexical state management,
;; sandbox isolation, and diff splitting behaviours.

(require 'buttercup)
(require 'macher-agent-macher-bridge)
(require 'cl-lib)
(require 'macher-agent)

;; Dummy gptel structures for mocking
(cl-defstruct mock-gptel-fsm info state)

(describe "Macher-Agent Core Behaviours"
          (after-each
           (setq macher-agent--pause-auto-sync nil))
          
          (describe "1. VFS and Optimistic Concurrency"
                    (it "asserts that a VFS write is rejected if the underlying file has drifted"
                        (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                               (file-path "/mock/proj/test.el")
                               (original-mtime '(25000 12345))
                               (drifted-mtime '(25000 99999)))
                          
                          ;; Initialise tracking
                          (puthash file-path original-mtime (macher-agent-workspace-mtime-tracker workspace))
                          
                          ;; Mock the filesystem returning a newer mtime
                          (spy-on 'file-attributes :and-call-fake
                                  (lambda (file)
                                    (if (string= file file-path)
                                        `(t 1 1 1 ,drifted-mtime ,drifted-mtime ,drifted-mtime 100 "mode" t 1 1)
                                      nil)))
                          
                          (let ((threw nil))
                            (condition-case err
                                (macher-agent-vfs-write workspace file-path "New content")
                              (error
                               (setq threw t)
                               (expect (cadr err) :to-equal "Your previous edits to test.el were discarded due to external file modifications.  Please re-read and re-apply")))
                            (expect threw :to-be t))))

                    (it "asserts that different agent sessions within the same workspace share uncommitted VFS state"
                        (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                               (session-a (make-macher-agent-session :id "Agent-A" :workspace workspace))
                               (session-b (make-macher-agent-session :id "Agent-B" :workspace workspace))
                               (file-path "/mock/proj/shared.el"))
                          
                          ;; Agent A writes to the shared VFS
                          (macher-agent-vfs-write (macher-agent-session-workspace session-a) file-path "Agent A changes")
                          
                          ;; Agent B reads from the shared VFS
                          (let ((read-content (macher-agent-vfs-read (macher-agent-session-workspace session-b) file-path)))
                            (expect read-content :to-equal "Agent A changes")))))

          (describe "2. Execution Environments (Sandbox)"
                    (it "asserts that sandbox inflation overlays the uncommitted VFS changes"
                        (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                               (session (make-macher-agent-session :id "Agent-A" 
                                                                   :workspace workspace
                                                                   :sandbox-path "/tmp/macher-sandbox/")))
                          
                          (puthash "/mock/proj/overlay.el" "VFS Overlay Content" (macher-agent-workspace-vfs-buffers workspace))
                          
                          (let ((written-to-sandbox nil))
                            (spy-on 'file-in-directory-p :and-return-value t)
                            (spy-on 'write-region :and-call-fake
                                    (lambda (start end filename &rest _args)
                                      (when (string-suffix-p "overlay.el" filename)
                                        (setq written-to-sandbox (substring-no-properties start end)))))
                            
                            (macher-agent-sandbox-inflate session)
                            
                            (expect written-to-sandbox :to-equal "VFS Overlay Content")))))

          (describe "3. Context and Isolation (Lexical Survival)"
                    (it "asserts that lexical context survives async gptel callbacks without buffer bleeding"
                        (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                               (session (make-macher-agent-session :id "Agent-A" :workspace workspace))
                               (fsm 'mock-fsm)
                               (executed-workspace nil))
                          
                          (spy-on 'gptel-fsm-info :and-return-value (list :macher-agent-session session))
                          
                          ;; Mock the behaviour where current-buffer changes asynchronously
                          (with-temp-buffer
                            (let ((original-buffer (current-buffer)))
                              
                              ;; Execute a mock async tool callback
                              (with-temp-buffer ;; "Wandering" buffer context
                                (let* ((info (macher-agent--extract-fsm-info fsm))
                                       (fsm-session (plist-get info :macher-agent-session)))
                                  (when fsm-session
                                    (setq executed-workspace (macher-agent-session-workspace fsm-session)))))
                              
                              (expect executed-workspace :to-be workspace))))))

          (describe "4. Media Injection Isolation"
                    (it "asserts that media injection strictly checks FSM properties"
                        (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                               (session (make-macher-agent-session :workspace workspace))
                               (fsm 'mock-fsm)
                               (macher-agent--active-fsm fsm))
                          
                          (spy-on 'gptel-fsm-info :and-return-value (list :macher-agent-session session))
                          (setf (macher-agent-session-pending-media session) (list (list "mockbase64" :mime "image/png")))
                          
                          (spy-on 'gptel--inject-media :and-return-value nil)
                          (spy-on 'gptel--inject-prompt :and-return-value nil)
                          
                          (macher-agent--inject-media-fsm-advice (lambda (f) f) fsm)
                          
                          (expect 'gptel--inject-media :to-have-been-called)
                          (expect (macher-agent-session-pending-media session) :to-be nil))))

          (describe "5. Diff Splitting Behaviour"
                    (it "asserts that virtual buffer modifications are split from physical file modifications"
                        (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                               (context (macher--make-context :workspace (cons 'project "/mock/proj/")
                                                              :contents (list (macher-agent-vfs-make-entry "/mock/proj/disk-file.el" "old" "new")
                                                                              (macher-agent-vfs-make-entry "*scratch*" "old" "new"))))
                               (fsm 'mock-fsm))
                          
                          (spy-on 'macher-agent-resolve-context :and-return-value context)
                          (spy-on 'gptel-fsm-info :and-return-value (list :macher-agent-session (make-macher-agent-session :workspace workspace)))
                          (setf (macher-context-dirty-p context) t)
                          
                          ;; Spy on upstream Emacs UI commands to prevent actual buffers from rendering
                          (spy-on 'rename-buffer)
                          (spy-on 'macher--get-buffer :and-return-value (list (get-buffer-create "*patch*")))
                          
                          (let ((orig-called-with nil))
                            ;; Execute our bridge interceptor, mocking the core function to capture the splits
                            (macher-agent--override-build-patch 
                             (lambda (ctx _fsm) 
                               (push (macher-context-contents ctx) orig-called-with)
                               (run-hooks 'macher-patch-ready-hook))
                             context fsm)
                            
                            ;; Assert that the core patch builder was called twice
                            (expect (length orig-called-with) :to-equal 2)
                            
                            ;; The virtual pass (executed second, so it sits at the head of the list)
                            (expect (car (car orig-called-with)) :to-equal '("*scratch*" "old" . "new"))
                            
                            ;; The physical pass (executed first, so it sits in the second slot)
                            (expect (car (cadr orig-called-with)) :to-equal '("/mock/proj/disk-file.el" "old" . "new"))))))

          (describe "5. Sandbox Security and Path Traversal (Jailbreaks)"
                    
                    (before-each
                     (setq sandbox-root "/tmp/macher-sandbox/"))

                    (it "REGRESSION: completely neutralises absolute path injections"
                        ;; The LLM or VFS hallucinates an absolute path to overwrite a system file
                        (let ((malicious-path "/etc/passwd")
                              (threw nil))
                          (condition-case err
                              (macher-agent--resolve-safe-path malicious-path sandbox-root)
                            (error (setq threw t)))
                          (expect threw :to-be t)))

                    (it "prevents relative path traversal (Directory Climbing)"
                        ;; The LLM tries to use `../` to climb out of the sandbox
                        (let ((malicious-path "../../../../etc/passwd")
                              (threw nil))
                          (condition-case err
                              (macher-agent--resolve-safe-path malicious-path sandbox-root)
                            (error (setq threw t)))
                          (expect threw :to-be t)))

                    (it "prevents tilde (~) home directory escapes"
                        ;; Emacs `expand-file-name` natively treats `~/` as an absolute escape
                        (let ((malicious-path "~/.ssh/id_rsa")
                              (threw nil))
                          (condition-case err
                              (macher-agent--resolve-safe-path malicious-path sandbox-root)
                            (error (setq threw t)))
                          (expect threw :to-be t))))

          (describe "6. Agent Orchestration and Sub-agent Delegation"
                    
                    (it "handles missing buffers gracefully and returns the buffer_name in the error payload"
                        (spy-on 'macher-agent-resolve-context :and-return-value nil)
                        (spy-on 'macher-agent-add-subagent :and-return-value nil)
                        (let* ((callback-result nil)
                               (task '(:buffer_name "non_existent_agent" :instructions "Do something"))
                               (callback (lambda (res) (setq callback-result res))))
                          (macher-agent-spawn-task task callback)
                          (expect (macher-agent-tool-response-status callback-result) :to-be 'error)
                          (expect (macher-agent-tool-response-buffer-name callback-result) :to-equal "non_existent_agent")
                          (expect (macher-agent-tool-response-error callback-result) :to-match "ERROR: Sub-agent buffer 'non_existent_agent' not found.")))))
