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
                               (ctx (macher--make-context :workspace workspace :contents nil))
                               (file-path "/mock/proj/test.el")
                               (original-mtime '(25000 12345))
                               (drifted-mtime '(25000 99999)))
                          (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)

                          (puthash file-path original-mtime (macher-agent-workspace-mtime-tracker workspace))

                          (spy-on 'file-attributes :and-call-fake
                                  (lambda (&rest args)
                                    (let ((file (car args)))
                                      (if (string= file file-path)
                                          `(t 1 1 1 ,drifted-mtime ,drifted-mtime ,drifted-mtime 100 "mode" t 1 1)
                                        nil))))

                          (let ((threw nil))
                            (condition-case err
                                (macher-agent-vfs-write workspace file-path "New content")
                              (error
                               (setq threw t)
                               (expect (cadr err) :to-equal "Your previous edits to test.el were discarded due to external file modifications.  Please re-read and re-apply")))
                            (expect threw :to-be t))))

                    (it "asserts that different agent sessions within the same workspace share uncommitted VFS state"
                        (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                               (ctx-a (macher--make-context :workspace workspace :contents nil))
                               (ctx-b (macher--make-context :workspace workspace :contents nil))
                               (file-path "/mock/proj/shared.el"))
                          (puthash (expand-file-name "/mock/proj/") ctx-a macher-agent-active-workspaces)

                          (macher-agent-vfs-write workspace file-path "Agent A changes")

                          (let ((read-content (macher-agent-vfs-read workspace file-path)))
                            (expect read-content :to-equal "Agent A changes")))))

          (describe "2. Execution Environments (Sandbox)"
                    (it "asserts that sandbox inflation overlays the uncommitted VFS changes"
                        (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                               (ctx (macher--make-context :workspace workspace :contents nil)))
                          (macher-agent--set-context-data ctx :sandbox-path "/tmp/macher-sandbox/")
                          (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)

                          (puthash "/mock/proj/overlay.el" "VFS Overlay Content" (macher-agent-workspace-vfs-buffers workspace))

                          (let ((written-to-sandbox nil))
                            (spy-on 'file-in-directory-p :and-return-value t)
                            (spy-on 'write-region :and-call-fake
                                    (lambda (start end filename &rest _args)
                                      (when (string-suffix-p "overlay.el" filename)
                                        (setq written-to-sandbox (substring-no-properties start end)))))

                            (macher-agent-sandbox-inflate ctx)

                            (expect written-to-sandbox :to-equal "VFS Overlay Content")))))

          (describe "3. Context and Isolation (Lexical Survival)"
                    (it "asserts that lexical context survives async gptel callbacks without buffer bleeding"
                        (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                               (ctx (macher--make-context :workspace workspace :contents nil))
                               (fsm (gptel-make-fsm))
                               (executed-workspace nil))

                          (setf (gptel-fsm-info fsm) (list :macher-agent-context ctx))

                          (with-temp-buffer
                            (let ((_original-buffer (current-buffer)))
                              (with-temp-buffer
                                (let* ((info (macher-agent--extract-fsm-info fsm))
                                       (fsm-ctx (plist-get info :macher-agent-context)))
                                  (when fsm-ctx
                                    (setq executed-workspace (macher-agent--get-context-workspace fsm-ctx)))))

                              (expect executed-workspace :to-be workspace)))))))

(describe "4. Media Injection Isolation"
          (it "asserts that media injection strictly checks FSM properties"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (ctx (macher--make-context :workspace workspace :contents nil))
                     (fsm (gptel-make-fsm))
                     (_gptel--fsm-last fsm))

                (setf (gptel-fsm-info fsm) (list :macher-agent-context ctx))
                (macher-agent--set-context-data ctx :pending-media (list (list "mockbase64" :mime "image/png")))

                (spy-on 'gptel--inject-media :and-return-value nil)
                (spy-on 'gptel--inject-prompt :and-return-value nil)

                (macher-agent--inject-media-fsm-advice (lambda (f) f) fsm)

                (expect 'gptel--inject-media :to-have-been-called)
                (expect (macher-agent--get-context-data ctx :pending-media) :to-be nil))))

(describe "5. Diff Splitting Behaviour"
          (it "asserts that virtual buffer modifications are split from physical file modifications"
              (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                     (context (macher--make-context :workspace (cons 'project "/mock/proj/")
                                                    :contents (list (macher-agent-vfs-make-entry "/mock/proj/disk-file.el" "old" "new")
                                                                    (macher-agent-vfs-make-entry "*scratch*" "old" "new"))))
                     (fsm (gptel-make-fsm)))

                (spy-on 'macher-agent-resolve-context :and-return-value context)
                (setf (gptel-fsm-info fsm) (list :macher-agent-context context))
                (setf (macher-context-dirty-p context) t)

                (spy-on 'rename-buffer)
                (spy-on 'macher--get-buffer :and-return-value (list (get-buffer-create "*patch*")))

                (let ((orig-called-with nil)
                      (prompt-seen nil))
                  (spy-on 'macher--build-patch :and-call-fake
                          (lambda (ctx _fsm)
                            (push (macher-context-prompt ctx) prompt-seen)
                            (push (macher-context-contents ctx) orig-called-with)
                            (run-hooks 'macher-patch-ready-hook)))
                  (setf (gptel-fsm-info fsm) (plist-put (gptel-fsm-info fsm) :prompt "Test Prompt Message"))
                  (macher-agent-process-request context fsm)

                  (expect (car prompt-seen) :to-equal "Test Prompt Message")
                  (expect (length orig-called-with) :to-equal 2)
                  (expect (car (car orig-called-with)) :to-equal '("*scratch*" "old" . "new"))
                  (expect (car (cadr orig-called-with)) :to-equal '("/mock/proj/disk-file.el" "old" . "new")))))

          (describe "5. Sandbox Security and Path Traversal (Jailbreaks)"

                    (before-each
                     (setq sandbox-root "/tmp/macher-sandbox/"))

                    (it "REGRESSION: completely neutralises absolute path injections"
                        (let ((malicious-path "/etc/passwd")
                              (threw nil))
                          (condition-case _err
                              (macher-agent--resolve-safe-path malicious-path sandbox-root)
                            (error (setq threw t)))
                          (expect threw :to-be t)))

                    (it "prevents relative path traversal (Directory Climbing)"
                        (let ((malicious-path "../../../../etc/passwd")
                              (threw nil))
                          (condition-case _err
                              (macher-agent--resolve-safe-path malicious-path sandbox-root)
                            (error (setq threw t)))
                          (expect threw :to-be t)))

                    (it "prevents tilde (~) home directory escapes"
                        (let ((malicious-path "~/.ssh/id_rsa")
                              (threw nil))
                          (condition-case _err
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
                          (expect (macher-agent-tool-response-error callback-result) :to-match "ERROR: Sub-agent buffer 'non_existent_agent' not found."))))

          (describe "7. Prompt Transformer Pipeline and Media Watcher"
                    (it "deduplicates tool usage blocks keeping the most recent occurrences"
                        (with-temp-buffer
                          (insert "User prompt\n")
                          (let ((str1 "```tool (:name \"read_file\" :args (\"file.el\"))\nargs\n```\n"))
                            (put-text-property 0 (length str1) 'gptel t str1)
                            (insert str1))
                          (insert "Intermediate response\n")
                          (let ((str2 "```tool (:name \"read_file\" :args (\"file.el\"))\nargs\n```\n"))
                            (put-text-property 0 (length str2) 'gptel t str2)
                            (insert str2))
                          (insert "Latest prompt\n")
                          (let ((macher-agent-max-duplicate-tools 1))
                            (macher-agent-transformer-deduplicate-tools nil nil))
                          (expect (buffer-string) :to-match "{\"status\": \"omitted\", \"reason\": \"duplicate\"}")))

                    (it "deduplicates Org-mode tool usage blocks and preserves distinct signatures"
                        (with-temp-buffer
                          (insert "User prompt\n")
                          (let ((str1 "#+begin_src tool (:name \"cargo_test\" :args nil)\noutput1\n#+end_src\n"))
                            (put-text-property 0 (length str1) 'gptel-tool t str1)
                            (insert str1))
                          (insert "Intermediate prompt\n")
                          (let ((str2 "#+begin_src tool (:name \"cargo_test\" :args nil)\noutput2\n#+end_src\n"))
                            (put-text-property 0 (length str2) 'gptel-tool t str2)
                            (insert str2))
                          (let ((str3 "#+begin_src tool (:name \"read_file\" :args (\"a.txt\"))\noutput3\n#+end_src\n"))
                            (put-text-property 0 (length str3) 'gptel-tool t str3)
                            (insert str3))
                          (let ((macher-agent-max-duplicate-tools 1))
                            (macher-agent-transformer-deduplicate-tools nil nil))
                          (expect (buffer-string) :to-match "{\"status\": \"omitted\", \"reason\": \"duplicate\"}")
                          (expect (buffer-string) :to-match "output2")
                          (expect (buffer-string) :to-match "output3")))

                    (it "snips context history exceeding character limits cleanly at turn boundaries"
                        (with-temp-buffer
                          (insert "Early user prompt content\n")
                          (let ((resp "Previous response boundary\n"))
                            (put-text-property 0 (length resp) 'gptel 'response resp)
                            (insert resp))
                          (insert "Latest user query content")
                          (let ((macher-agent-max-context-chars 25))
                            (macher-agent-transformer-snip-context nil nil))
                          (expect (buffer-string) :to-match "Latest user query content")))

                    (it "protects frontmatter header when snipping context history"
                        (with-temp-buffer
                          (insert "---\nkey: value\n---\nEarly user prompt content\n")
                          (let ((resp "Previous response boundary\n"))
                            (put-text-property 0 (length resp) 'gptel 'response resp)
                            (insert resp))
                          (insert "Latest user query content")
                          (let ((macher-agent-max-context-chars 25))
                            (macher-agent-transformer-snip-context nil nil))
                          (expect (buffer-string) :to-match "^---\nkey: value\n---")))

                    (it "registers sync prompt transformer and transformers in setup-gptel-buffer"
                        (let ((macher-agent-prompt-transformers '(t1 t2)))
                          (spy-on 'macher-agent-resolve-context :and-return-value t)
                          (with-temp-buffer
                            (macher-agent-setup-gptel-buffer)
                            (expect gptel-prompt-transform-functions
                                    :to-equal '(macher-agent-sync-prompt-transformer t t1 t2)))))

                    (it "triggers pending media injection on FSM updates"
                        (let* ((ctx (macher--make-context :workspace nil :contents nil))
                               (fsm (gptel-make-fsm)))
                          (setf (gptel-fsm-info fsm) (list :macher-agent-context ctx))
                          (macher-agent--set-context-data ctx :pending-media (list "data"))
                          (spy-on 'macher-agent--perform-pending-media-injection)
                          (macher-agent--inject-media-fsm-advice (lambda (&rest _) nil) fsm 'WAIT)
                          (expect 'macher-agent--perform-pending-media-injection :to-have-been-called-with fsm)))))

