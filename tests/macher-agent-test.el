;;; macher-agent-test.el --- Comprehensive BDD tests for Macher-Agent -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'buttercup)
(require 'macher-agent-macher-bridge)
(require 'macher-agent)
(require 'macher-agent-vfs-client)
(require 'macher-agent-gptel-tools)
(require 'macher-agent-orchestration)
(let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "helpers" current-dir)))

(require 'macher-agent-test-harness)

(describe "Macher-Agent BDD Test Suite"

          (before-each
           (spy-on 'macher-action)
           (spy-on 'gptel-send)
           (spy-on 'macher--add-termination-handler)
           (setq macher-agent--persistent-context nil)
           (let* ((ctx (ignore-errors (macher-agent-resolve-context)))
                  (ws (when ctx (macher-agent--get-context-workspace ctx))))
             (when ws (setf (macher-agent-workspace-active-subagents ws) nil))))

          (describe
           "Context and Security (macher-agent-vfs-client.el)"
           (describe
            "macher-agent--get-active-context"
            (it "returns nil when no FSM is active without accessing disk"
                (let ((macher-agent--active-fsm nil)
                      (macher--fsm-latest nil)
                      (gptel--fsm-last nil)
                      (macher-agent--persistent-context nil))
                  (spy-on 'macher-agent-resolve-context :and-return-value nil)
                  (expect (macher-agent--get-active-context) :to-be nil)
                  (expect 'macher-agent-resolve-context :not :to-have-been-called)))

            (it "successfully mocks an active FSM context using with-macher-agent-mock-fsm"
                (let ((mock-ctx (macher--make-context :contents nil)))
                  (with-macher-agent-mock-fsm mock-ctx
                                              (expect (macher-agent--get-active-context) :to-be mock-ctx)))))

           (it
            "ensures buffer persistent-context remains aligned with canonical active workspace instance"
            (let* ((proj-dir "/mock/aligned-proj/")
                   (ws (make-macher-agent-workspace :project-root proj-dir))
                   (canonical-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                   (stale-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                   (buf (generate-new-buffer "aligned-buf")))
              (puthash (expand-file-name proj-dir) canonical-ctx macher-agent-active-workspaces)
              (unwind-protect
                  (with-current-buffer buf
                    (setq-local default-directory proj-dir)
                    (setq-local macher-agent--is-workspace t)
                    (setq-local macher-agent--persistent-context stale-ctx)
                    (let ((resolved (macher-agent-resolve-context)))
                      (expect resolved :to-be stale-ctx)))
                (kill-buffer buf))))

           (it "throws a security error if accessing a path outside of the allowed context"
               (let ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "allowed.txt" "old" "new")))))
                 (expect (macher-agent--ensure-access ctx "forbidden.txt") :to-throw 'error)))

           (it "successfully records a virtual edit to an existing scoped buffer"
               (let* ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "test.txt" "orig" "orig")))))
                 (macher-agent--update-context-file ctx "test.txt" "modified")
                 (expect (macher-context-dirty-p ctx) :to-be t)
                 (expect (macher-agent-vfs-entry-curr (cl-find "test.txt" (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)) :to-equal "modified")))

           (it
            "resolves context matching the active project root rather than selecting an arbitrary workspace"
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

           (describe
            "Phase 2: Context resolution waterfall"
            (it "defines macher-agent-context-pipeline-functions with all 6 step functions in order"
                (expect macher-agent-context-pipeline-functions
                        :to-equal '(macher-agent-ctx-pipe--explicit
                                    macher-agent-ctx-pipe--fsm
                                    macher-agent-ctx-pipe--subagent
                                    macher-agent-ctx-pipe--canonical
                                    macher-agent-ctx-pipe--fsm-fallback
                                    macher-agent-ctx-pipe--lazy-init)))

            (describe
             "macher-agent-ctx-pipe--explicit"
             (it "short-circuits when :resolved is already non-nil"
                 (let* ((mock-ctx (macher--make-context))
                        (state (list :input "dummy" :resolved mock-ctx :expanded-root nil))
                        (res (macher-agent-ctx-pipe--explicit state)))
                   (expect (plist-get res :resolved) :to-be mock-ctx)))

             (it "resolves context when :input is a valid macher-context"
                 (let* ((mock-ctx (macher--make-context))
                        (state (list :input mock-ctx :resolved nil :expanded-root nil))
                        (res (macher-agent-ctx-pipe--explicit state)))
                   (expect (plist-get res :resolved) :to-be mock-ctx)))

             (it "leaves :resolved nil when :input is not a macher-context"
                 (let* ((state (list :input "not-a-context" :resolved nil :expanded-root nil))
                        (res (macher-agent-ctx-pipe--explicit state)))
                   (expect (plist-get res :resolved) :to-be nil))))

            (describe
             "macher-agent-ctx-pipe--fsm"
             (it "short-circuits when :resolved is already non-nil"
                 (let* ((mock-ctx (macher--make-context))
                        (state (list :input "dummy" :resolved mock-ctx :expanded-root nil))
                        (res (macher-agent-ctx-pipe--fsm state)))
                   (expect (plist-get res :resolved) :to-be mock-ctx)))

             (it "extracts context from FSM input when present"
                 (let* ((mock-ctx (macher--make-context))
                        (fsm (list :macher-agent-context mock-ctx))
                        (state (list :input fsm :resolved nil :expanded-root nil)))
                   (spy-on 'macher-agent--extract-fsm-info :and-return-value fsm)
                   (let ((res (macher-agent-ctx-pipe--fsm state)))
                     (expect (plist-get res :resolved) :to-be mock-ctx))))

             (it "leaves :resolved nil when FSM input contains no context"
                 (let* ((fsm '(:some-other-key 123))
                        (state (list :input fsm :resolved nil :expanded-root nil)))
                   (spy-on 'macher-agent--extract-fsm-info :and-return-value fsm)
                   (let ((res (macher-agent-ctx-pipe--fsm state)))
                     (expect (plist-get res :resolved) :to-be nil)))))

            (describe
             "macher-agent-ctx-pipe--subagent"
             (it "short-circuits when :resolved is already non-nil without evaluating root"
                 (let* ((mock-ctx (macher--make-context))
                        (state (list :input nil :resolved mock-ctx :expanded-root nil)))
                   (spy-on 'macher-agent-root)
                   (let ((res (macher-agent-ctx-pipe--subagent state)))
                     (expect (plist-get res :resolved) :to-be mock-ctx)
                     (expect 'macher-agent-root :not :to-have-been-called))))

             (it "resolves persistent context when buffer is a subagent"
                 (let* ((proj-dir "/mock/root/")
                        (ws (make-macher-agent-workspace :project-root proj-dir))
                        (mock-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                        (state (list :input nil :resolved nil :expanded-root nil)))
                   (let ((macher-agent--persistent-context mock-ctx)
                         (macher-agent--is-subagent t)
                         (default-directory proj-dir))
                     (spy-on 'macher-agent-root :and-return-value proj-dir)
                     (let ((res (macher-agent-ctx-pipe--subagent state)))
                       (expect (plist-get res :resolved) :to-be mock-ctx)
                       (expect (plist-get res :expanded-root) :to-equal (expand-file-name proj-dir))))))

             (it "resolves persistent context when it matches the active workspace root"
                 (let* ((proj-dir "/mock/proj/")
                        (ws (make-macher-agent-workspace :project-root proj-dir))
                        (mock-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                        (state (list :input nil :resolved nil :expanded-root nil)))
                   (let ((macher-agent--persistent-context mock-ctx)
                         (macher-agent--is-subagent nil)
                         (default-directory proj-dir))
                     (spy-on 'macher-agent-root :and-return-value proj-dir)
                     (let ((res (macher-agent-ctx-pipe--subagent state)))
                       (expect (plist-get res :resolved) :to-be mock-ctx)
                       (expect (plist-get res :expanded-root) :to-equal (expand-file-name proj-dir)))))))

            (describe
             "macher-agent-ctx-pipe--canonical"
             (it "short-circuits when :resolved is already non-nil"
                 (let* ((mock-ctx (macher--make-context))
                        (state (list :input nil :resolved mock-ctx :expanded-root nil)))
                   (spy-on 'macher-agent-root)
                   (let ((res (macher-agent-ctx-pipe--canonical state)))
                     (expect (plist-get res :resolved) :to-be mock-ctx)
                     (expect 'macher-agent-root :not :to-have-been-called))))

             (it "resolves canonical context from active workspaces registry"
                 (let* ((proj-dir "/mock/canonical-proj/")
                        (ws (make-macher-agent-workspace :project-root proj-dir))
                        (canonical-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                        (state (list :input nil :resolved nil :expanded-root (expand-file-name proj-dir))))
                   (puthash (expand-file-name proj-dir) canonical-ctx macher-agent-active-workspaces)
                   (let ((macher-agent--is-subagent nil)
                         (default-directory proj-dir))
                     (spy-on 'macher-agent-root :and-return-value proj-dir)
                     (let ((res (macher-agent-ctx-pipe--canonical state)))
                       (expect (plist-get res :resolved) :to-be canonical-ctx)
                       (expect macher-agent--persistent-context :to-be canonical-ctx)))))

             (it "clones canonical context when resolving in a subagent buffer"
                 (let* ((proj-dir "/mock/subagent-canonical/")
                        (ws (make-macher-agent-workspace :project-root proj-dir))
                        (canonical-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                        (state (list :input nil :resolved nil :expanded-root (expand-file-name proj-dir))))
                   (puthash (expand-file-name proj-dir) canonical-ctx macher-agent-active-workspaces)
                   (let ((macher-agent--is-subagent t)
                         (default-directory proj-dir))
                     (spy-on 'macher-agent-root :and-return-value proj-dir)
                     (let ((res (macher-agent-ctx-pipe--canonical state)))
                       (expect (plist-get res :resolved) :not :to-be canonical-ctx)
                       (expect (macher-context-p (plist-get res :resolved)) :to-be t))))))

            (describe
             "macher-agent-ctx-pipe--fsm-fallback"
             (it "short-circuits when :resolved is already non-nil"
                 (let* ((mock-ctx (macher--make-context))
                        (state (list :input nil :resolved mock-ctx :expanded-root nil)))
                   (spy-on 'macher-agent--get-fsm-latest)
                   (let ((res (macher-agent-ctx-pipe--fsm-fallback state)))
                     (expect (plist-get res :resolved) :to-be mock-ctx)
                     (expect 'macher-agent--get-fsm-latest :not :to-have-been-called))))

             (it "resolves context from latest FSM when available"
                 (let* ((mock-ctx (macher--make-context))
                        (fsm (list :macher-agent-context mock-ctx))
                        (state (list :input nil :resolved nil :expanded-root nil)))
                   (spy-on 'macher-agent--get-fsm-latest :and-return-value fsm)
                   (spy-on 'macher-agent--extract-fsm-info :and-return-value fsm)
                   (let ((res (macher-agent-ctx-pipe--fsm-fallback state)))
                     (expect (plist-get res :resolved) :to-be mock-ctx)))))

            (describe
             "macher-agent-ctx-pipe--lazy-init"
             (it "short-circuits when :resolved is already non-nil"
                 (let* ((mock-ctx (macher--make-context))
                        (state (list :input nil :resolved mock-ctx :expanded-root nil)))
                   (spy-on 'macher-agent--resolve-context-lazy-init)
                   (let ((res (macher-agent-ctx-pipe--lazy-init state)))
                     (expect (plist-get res :resolved) :to-be mock-ctx)
                     (expect 'macher-agent--resolve-context-lazy-init :not :to-have-been-called))))

             (it "resolves context via lazy initialization when permitted"
                 (let* ((mock-ctx (macher--make-context))
                        (state (list :input nil :resolved nil :expanded-root nil)))
                   (spy-on 'macher-agent--resolve-context-lazy-init :and-return-value mock-ctx)
                   (let ((res (macher-agent-ctx-pipe--lazy-init state)))
                     (expect (plist-get res :resolved) :to-be mock-ctx))))

             (it "leaves :resolved nil when lazy initialization fails or returns nil"
                 (let ((state (list :input nil :resolved nil :expanded-root nil)))
                   (spy-on 'macher-agent--resolve-context-lazy-init :and-return-value nil)
                   (let ((res (macher-agent-ctx-pipe--lazy-init state)))
                     (expect (plist-get res :resolved) :to-be nil)))))

            (describe
             "macher-agent-resolve-context pipeline reduction"
             (it "passes initial state through seq-reduce over macher-agent-context-pipeline-functions"
                 (let ((mock-ctx (macher--make-context)))
                   (spy-on 'macher-agent-ctx-pipe--explicit :and-return-value (list :input mock-ctx :resolved mock-ctx :expanded-root nil))
                   (spy-on 'macher-agent-ctx-pipe--fsm :and-call-through)
                   (spy-on 'macher-agent-ctx-pipe--subagent :and-call-through)
                   (spy-on 'macher-agent-ctx-pipe--canonical :and-call-through)
                   (spy-on 'macher-agent-ctx-pipe--fsm-fallback :and-call-through)
                   (spy-on 'macher-agent-ctx-pipe--lazy-init :and-call-through)
                   (let ((res (macher-agent-resolve-context mock-ctx)))
                     (expect res :to-be mock-ctx)
                     (expect 'macher-agent-ctx-pipe--explicit :to-have-been-called)
                     (expect 'macher-agent-ctx-pipe--fsm :to-have-been-called)
                     (expect 'macher-agent-ctx-pipe--subagent :to-have-been-called)
                     (expect 'macher-agent-ctx-pipe--canonical :to-have-been-called)
                     (expect 'macher-agent-ctx-pipe--fsm-fallback :to-have-been-called)
                     (expect 'macher-agent-ctx-pipe--lazy-init :to-have-been-called))))

             (it "throws error when context resolution fails across all steps"
                 (spy-on 'macher-agent--resolve-context-lazy-init :and-return-value nil)
                 (let ((macher-agent-active-workspaces (make-hash-table :test 'equal))
                       (macher-agent--persistent-context nil)
                       (macher--fsm-latest nil)
                       (gptel--fsm-last nil)
                       (macher-agent--active-fsm nil))
                   (expect (macher-agent-resolve-context) :to-throw 'error)))

             (it "registers active workspace root when resolving context"
                 (let* ((mock-ws (make-macher-agent-workspace :project-root "/tmp/test-workspace-resolve"))
                        (mock-ctx (macher-agent--make-vfs-context :workspace mock-ws :contents nil)))
                   (spy-on 'macher-agent-ctx-pipe--explicit :and-return-value (list :input mock-ctx :resolved mock-ctx :expanded-root nil))
                   (clrhash macher-agent-active-workspaces)
                   (let ((res (macher-agent-resolve-context mock-ctx)))
                     (expect res :to-be mock-ctx)
                     (expect (gethash "/tmp/test-workspace-resolve" macher-agent-active-workspaces) :to-be mock-ctx)))))

            (it "bypasses UI when spawning background tasks via A2A dispatch"
                (let* ((buf (generate-new-buffer "subagent-bg-buf"))
                       (payload (list (list :type 'SEND_MESSAGE
                                            :task-id "bg-task-1"
                                            :message "run"
                                            :metadata (list :buffer_name (buffer-name buf) :background t))))
                       (callback-called nil)
                       (ui-shown nil))
                  (spy-on 'macher-agent-ui-show :and-call-fake (lambda (&rest _args) (setq ui-shown t)))
                  (cl-letf (((symbol-function 'gptel-send)
                             (lambda ()
                               (let ((cb (bound-and-true-p macher-agent--a2a-callback))
                                     (task-id (bound-and-true-p macher-agent--current-task-id)))
                                 (when cb
                                   (funcall cb (list :status 'success :data "success-result" :task-id task-id)))))))
                    (unwind-protect
                        (progn
                          (macher-agent-a2a-dispatch payload (lambda (_res) (setq callback-called t)))
                          (expect callback-called :to-be t)
                          (expect ui-shown :to-be nil))
                      (kill-buffer buf)))))

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
                              (setq-local gptel--fsm-last nil))

                            (with-current-buffer buf
                              (macher-agent-apply-virtual-buffers))
                            (kill-buffer buf))))
            (describe
             "Macher-Agent Skill Model Selection"

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

            (describe
             "Model-Specific Context Character Limits"
             (it "truncates context history using model-specific alist limits"
                 (with-temp-buffer
                   (insert "Early user prompt content\n")
                   (let ((resp "Previous response boundary\n"))
                     (put-text-property 0 (length resp) 'gptel 'response resp)
                     (insert resp))
                   (insert "Latest user query content")
                   (let ((macher-agent-max-context-chars '((gpt-4o . 25) (nil . 2000000)))
                         (gptel-model 'gpt-4o))
                     (macher-agent-transformer-snip-context nil nil))
                   (expect (buffer-string) :to-match "Latest user query content"))))


            (describe
             "Virtual File System (VFS) Concurrency Control"

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
                               (expect (macher-agent-vfs-entry-curr entry) :to-equal "agent edit"))))

                       (it "bypasses stale live buffer and reads directly from physical disk if disk mtime is newer than stored mtime"
                           (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                                  (ctx (macher--make-context :workspace workspace :contents nil))
                                  (file-path "/mock/proj/test.el")
                                  (entry (macher-agent-vfs-make-entry file-path "original state" "original state"))
                                  (old-mtime '(25000 10000))
                                  (new-mtime '(25000 20000)))
                             (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                             (let ((tracker (macher-agent-workspace-mtime-tracker workspace)))
                               (puthash file-path old-mtime tracker)
                               (spy-on 'file-attributes :and-call-fake
                                       (lambda (path)
                                         (if (equal path file-path)
                                             `(t 1 1 1 ,new-mtime ,new-mtime ,new-mtime 100 "mode" t 1 1)
                                           nil)))
                               (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "fresh disk state")
                               (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "stale buffer state")

                               (let ((mutated (macher-agent--sync-context-entry entry workspace)))
                                 (expect mutated :to-be t)
                                 (expect (macher-agent-vfs-entry-orig entry) :to-equal "fresh disk state")
                                 (expect (macher-agent-vfs-entry-curr entry) :to-equal "fresh disk state")
                                 (expect (gethash file-path tracker) :to-equal new-mtime)))))

                       (it "passes workspace argument from macher-agent--sync-and-check-dirty-entries to macher-agent--sync-context-entry"
                           (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                                  (entry (macher-agent-vfs-make-entry "/mock/proj/test.el" "a" "b"))
                                  (contents (list entry))
                                  (passed-ws nil))
                             (spy-on 'macher-agent--sync-context-entry :and-call-fake
                                     (lambda (e &optional ws)
                                       (setq passed-ws ws)
                                       nil))
                             (macher-agent--sync-and-check-dirty-entries contents workspace)
                             (expect passed-ws :to-equal workspace)))

                       (it "resolves attrs for virtual paths via file-attributes without requiring file-exists-p"
                           (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                                  (ctx (macher--make-context :workspace workspace :contents nil))
                                  (file-path "/mock/proj/virtual-file.el")
                                  (entry (macher-agent-vfs-make-entry file-path "original state" "original state"))
                                  (old-mtime '(25000 10000))
                                  (new-mtime '(25000 20000)))
                             (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                             (let ((tracker (macher-agent-workspace-mtime-tracker workspace))
                                   (attrs-called nil))
                               (puthash file-path old-mtime tracker)
                               (spy-on 'file-attributes :and-call-fake
                                       (lambda (path)
                                         (when (equal path file-path)
                                           (setq attrs-called t)
                                           `(t 1 1 1 ,new-mtime ,new-mtime ,new-mtime 100 "mode" t 1 1))))
                               (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "mock disk state")
                               (spy-on 'macher-agent--read-content-from-disk-or-buffer :and-return-value "mock disk state")
                               (let ((mutated (macher-agent--sync-context-entry entry workspace)))
                                 (expect attrs-called :to-be t)
                                 (expect mutated :to-be t)
                                 (expect (macher-agent-vfs-entry-orig entry) :to-equal "mock disk state")))))

                       (it "invalidates and bypasses stale live buffers when an externally modified disk file has a newer mtime"
                           (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                                  (ctx (macher--make-context :workspace workspace :contents nil))
                                  (file-path "/mock/proj/desynced-buffer.el")
                                  (entry (macher-agent-vfs-make-entry file-path "original state" "original state"))
                                  (old-mtime '(25000 10000))
                                  (new-mtime '(25000 20000))
                                  (buf (get-buffer-create file-path)))
                             (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                             (let ((tracker (macher-agent-workspace-mtime-tracker workspace)))
                               (unwind-protect
                                   (progn
                                     (with-current-buffer buf
                                       (erase-buffer)
                                       (insert "desynced live buffer content"))
                                     (puthash file-path old-mtime tracker)
                                     (spy-on 'file-attributes :and-call-fake
                                             (lambda (path)
                                               (if (equal path file-path)
                                                   `(t 1 1 1 ,new-mtime ,new-mtime ,new-mtime 100 "mode" t 1 1)
                                                 nil)))
                                     (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "new disk content")
                                     (let ((mutated (macher-agent--sync-context-entry entry workspace)))
                                       (expect mutated :to-be t)
                                       (expect (macher-agent-vfs-entry-orig entry) :to-equal "new disk content")
                                       (expect (macher-agent-vfs-entry-curr entry) :to-equal "new disk content")))
                                 (when (buffer-live-p buf)
                                   (kill-buffer buf))))))

                       (it "treats desynced live buffer content as current-state when physical disk is NOT newer"
                           (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                                  (ctx (macher--make-context :workspace workspace :contents nil))
                                  (file-path "/mock/proj/desynced-buffer.el")
                                  (entry (macher-agent-vfs-make-entry file-path "original state" "original state"))
                                  (same-mtime '(25000 10000))
                                  (buf (get-buffer-create file-path)))
                             (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                             (let ((tracker (macher-agent-workspace-mtime-tracker workspace)))
                               (unwind-protect
                                   (progn
                                     (with-current-buffer buf
                                       (erase-buffer)
                                       (insert "desynced live buffer content"))
                                     (puthash file-path same-mtime tracker)
                                     (spy-on 'file-attributes :and-call-fake
                                             (lambda (path)
                                               (if (equal path file-path)
                                                   `(t 1 1 1 ,same-mtime ,same-mtime ,same-mtime 100 "mode" t 1 1)
                                                 nil)))
                                     (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "disk content")
                                     (let ((mutated (macher-agent--sync-context-entry entry workspace)))
                                       (expect mutated :to-be t)
                                       (expect (macher-agent-vfs-entry-orig entry) :to-equal "desynced live buffer content")
                                       (expect (macher-agent-vfs-entry-curr entry) :to-equal "desynced live buffer content")))
                                 (when (buffer-live-p buf)
                                   (kill-buffer buf))))))

                       (it "gives precedence to desynced live buffer when its content matches orig even if disk is newer"
                           (let* ((workspace (make-macher-agent-workspace :project-root "/mock/proj/"))
                                  (ctx (macher--make-context :workspace workspace :contents nil))
                                  (file-path "/mock/proj/dirty-buffer.el")
                                  (entry (macher-agent-vfs-make-entry file-path "original state" "original state"))
                                  (old-mtime '(25000 10000))
                                  (new-mtime '(25000 20000))
                                  (buf (get-buffer-create file-path)))
                             (puthash (expand-file-name "/mock/proj/") ctx macher-agent-active-workspaces)
                             (let ((tracker (macher-agent-workspace-mtime-tracker workspace)))
                               (unwind-protect
                                   (progn
                                     (with-current-buffer buf
                                       (erase-buffer)
                                       (insert "original state"))
                                     (puthash file-path old-mtime tracker)
                                     (spy-on 'file-attributes :and-call-fake
                                             (lambda (path)
                                               (if (equal path file-path)
                                                   `(t 1 1 1 ,new-mtime ,new-mtime ,new-mtime 100 "mode" t 1 1)
                                                 nil)))
                                     (spy-on 'macher-agent--read-content-from-disk-direct :and-return-value "new disk content")
                                     (let ((mutated (macher-agent--sync-context-entry entry workspace)))
                                       (expect mutated :to-be nil)
                                       (expect (macher-agent-vfs-entry-orig entry) :to-equal "original state")
                                       (expect (macher-agent-vfs-entry-curr entry) :to-equal "original state")))
                                 (when (buffer-live-p buf)
                                   (kill-buffer buf)))))))
             (describe
              "Macher-Agent Tool Registry Resilience"

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
                                (expect (macher-agent-workspace-project-root workspace) :to-equal (expand-file-name restore-dir))))))

                      ;; Clean up temp buffers
                      (when (buffer-live-p other-buf) (kill-buffer other-buf))
                      (when (buffer-live-p restore-buf) (kill-buffer restore-buf)))))

              (it "synchronises deserialised persistent-context into active-workspaces after restore"
                  (let* ((restore-buf (generate-new-buffer "restore-test-buf"))
                         (restore-dir "/mock/proj/restored-ws/")
                         (ws (make-macher-agent-workspace :project-root restore-dir))
                         (deserialised-ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
                         (macher-agent--allow-gptel-restore t)
                         (orig-fun (lambda (&rest _args)
                                     (setq-local macher-agent--persistent-context deserialised-ctx))))
                    (unwind-protect
                        (with-current-buffer restore-buf
                          (setq-local default-directory restore-dir)
                          (macher-agent--gptel-restore-advice orig-fun)
                          (let ((registered (gethash (expand-file-name restore-dir) macher-agent-active-workspaces)))
                            (expect registered :to-equal deserialised-ctx))
                          (let ((resolved-sub (macher-agent--resolve-context-from-ws (concat restore-dir "submodule-a/"))))
                            (expect resolved-sub :to-equal deserialised-ctx)))
                      (when (buffer-live-p restore-buf) (kill-buffer restore-buf)))))))

            (describe "Sandbox Execution (macher-agent-vfs-client.el)"
                      (describe "read_media_in_workspace"
                                (it "errors if gptel-track-media is nil"
                                    (let* ((gptel-track-media nil)
                                           (ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "test.png" "" "img-data"))))
                                           (tool-fn (gptel-tool-function macher-agent-read-media-in-workspace-tool)))
                                      (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                                      (let ((result (funcall tool-fn nil "test.png")))
                                        (expect result :to-match "gptel media send option is off"))))
                                (it "permits access to valid media files inside the workspace without triggering VFS text security locks"
                                    (let* ((gptel-track-media t)
                                           (ctx (macher--make-context :contents nil))
                                           (gptel--fsm-last (gptel-make-fsm))
                                           (mock-info (list :buffer (current-buffer) :macher-agent-context ctx))
                                           (tool-fn (gptel-tool-function macher-agent-read-media-in-workspace-tool)))
                                      (setf (gptel-fsm-info gptel--fsm-last) mock-info)
                                      (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                                      (spy-on 'macher-agent-context-classify-entry :and-return-value 'media)
                                      (spy-on 'file-exists-p :and-return-value t)
                                      (spy-on 'mailcap-file-name-to-mime-type :and-return-value "image/png")
                                      (spy-on 'insert-file-contents-literally :and-call-fake (lambda (&rest _args) (insert "mock-image-data")))
                                      (let ((result (funcall tool-fn nil "test_workspace_image.png")))
                                        (expect result :to-match "SUCCESS: Media 'test_workspace_image.png'"))))
                                (it "throws an error if the tool cannot determine MIME type"
                                    (let* ((gptel-track-media t)
                                           (ctx (macher--make-context :contents nil))
                                           (tool-fn (gptel-tool-function macher-agent-read-media-in-workspace-tool)))
                                      (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                                      (spy-on 'file-exists-p :and-return-value t)
                                      (spy-on 'mailcap-file-name-to-mime-type :and-return-value nil)
                                      (let ((result (funcall tool-fn nil "unauthorized_script.sh")))
                                        (expect result :to-match "Could not determine MIME type"))))
                                (it "stages media in the session pending-media instead of polluting gptel-context"
                                    (let* ((gptel-track-media t)
                                           (gptel-context nil)
                                           (ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "test.png" "" "img-data"))))
                                           (gptel--fsm-last (gptel-make-fsm))
                                           (mock-info (list :buffer (current-buffer) :macher-agent-context ctx))
                                           (tool-fn (gptel-tool-function macher-agent-read-media-in-workspace-tool)))
                                      (setf (gptel-fsm-info gptel--fsm-last) mock-info)
                                      (spy-on 'macher-agent-resolve-context :and-return-value ctx)
                                      (spy-on 'mailcap-file-name-to-mime-type :and-return-value "image/png")
                                      (spy-on 'file-exists-p :and-return-value t)
                                      (let ((result (funcall tool-fn nil "test.png")))
                                        (expect result :to-match "SUCCESS: Media")
                                        (expect gptel-context :to-be nil)
                                        (expect (car (car (macher-agent--get-context-data ctx :pending-media))) :to-equal "aW1nLWRhdGE="))))

                                (it "injects pending media into FSM payload and clears the queue pre-flight"
                                    (let* ((buf (current-buffer))
                                           (mock-backend 'mock-backend)
                                           (mock-data '((:role "system" :content "sys")))
                                           (ctx (macher--make-context :contents nil))
                                           (mock-info (list :buffer buf :backend mock-backend :data mock-data :macher-agent-context ctx))

                                           ;; We can safely use a mock symbol again!
                                           (mock-fsm (gptel-make-fsm))

                                           (orig-called nil)
                                           (orig-fun (lambda (fsm &rest _) (setq orig-called fsm))))

                                      (macher-agent--set-context-data ctx :pending-media '(("mockbase64" :mime "image/png")))

                                      (setf (gptel-fsm-info mock-fsm) mock-info)
                                      (spy-on 'gptel--inject-media)
                                      (spy-on 'gptel--inject-prompt)

                                      ;; Execute the restored advice
                                      (macher-agent--inject-media-fsm-advice orig-fun mock-fsm)

                                      ;; Verify injection lifecycle
                                      (expect 'gptel--inject-media :to-have-been-called)
                                      (expect 'gptel--inject-prompt :to-have-been-called)
                                      (expect orig-called :to-equal mock-fsm)
                                      (expect (macher-agent--get-context-data ctx :pending-media) :to-be nil))))

                      (describe
                       "rsync command building"

                       (it "constructs a shell string using git ls-files"
                           ;; Intercept call-process to return 0 (success).
                           ;; This simulates a valid git repository and bypasses the physical directory check.
                           (spy-on 'call-process :and-return-value 0)

                           (let* ((src "/my/project/")
                                  (dest "/tmp/sandbox/")
                                  (cmd (macher-agent--build-rsync-cmd src dest)))

                             (expect (stringp cmd) :to-be t)
                             (expect (string-match-p "git .*ls-files -z -c --recurse-submodules" cmd) :to-be-truthy)
                             (expect (string-match-p "git .*ls-files -z -o --exclude-standard" cmd) :to-be-truthy)
                             (expect (string-match-p "rsync -aLC --delete --from0 --files-from=-" cmd) :to-be-truthy)

                             ;; Optional but good practice: verify our mock was actually triggered
                             (expect 'call-process :to-have-been-called-with
                                     "git" nil nil nil "rev-parse" "--is-inside-work-tree")))

                       (it "throws an error if the directory is not inside a git repository"
                           (spy-on 'call-process :and-return-value 1)
                           (expect (macher-agent--build-rsync-cmd "/my/project/" "/tmp/sandbox/")
                                   :to-throw 'error)))

                      (describe
                       "Macher-Agent Tool Category Isolation"

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
                      (describe
                       "Virtual File System (VFS) Sandbox Isolation"

                       (describe
                        "deletions and moves"
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

                       (describe
                        "macher-agent--vfs-apply-overlay"

                        (it "reroutes virtual edits to the ephemeral sandbox, protecting the physical disk"
                            (let* ((workspace-root "/my/project/")
                                   (sandbox-dir "/tmp/sandbox-12345/")
                                   ;; Create a mock context representing a dirty workspace
                                   (mock-ws (make-macher-agent-workspace :project-root workspace-root))
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

                       (describe
                        "VFS Strict Pipeline Execution"
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
                              (expect (file-name-absolute-p rsync-dest-arg) :to-be t)))))))

           (describe "Interactive Commands and State (macher-agent-orchestration.el)"
                     (it "macher-agent-add-buffer-to-scope explicitly errors out if no existing session is found"
                         (let ((buf (generate-new-buffer "lazy-target")))
                           (let ((gptel--fsm-last nil)
                                 (macher-agent-active-workspaces (make-hash-table :test 'equal))
                                 (macher-agent--persistent-context nil))
                             (cl-letf (((symbol-function 'buffer-list) (lambda () nil)))
                               (expect (macher-agent-add-buffer-to-scope "lazy-target") :to-throw 'error)))
                           (kill-buffer buf)))
                     (it "macher-agent-add-subagent creates a buffer and tracks it globally"
                         (let* ((mock-workspace (make-macher-agent-workspace :project-root "/tmp/"))
                                (mock-context (macher-agent--make-vfs-context :workspace mock-workspace :contents nil)))
                           (puthash (expand-file-name "/tmp/") mock-context macher-agent-active-workspaces)
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
                         (let* ((ws (make-macher-agent-workspace :project-root "/mock/proj/"))
                                (ctx (macher--make-context :workspace ws :contents (list (macher-agent-vfs-make-entry "file.txt" "orig" "mod"))))
                                (buf (generate-new-buffer "active-session")))
                           (with-current-buffer buf
                             (setq-local macher-agent--persistent-context ctx)
                             (macher-agent-clear-context)
                             (expect (macher-agent--get-context-contents macher-agent--persistent-context) :to-be nil))
                           (kill-buffer buf)))

                     (it "isolates subagent context from workspace and merges changes on completion"
                         (let* ((temp-dir (file-name-as-directory (make-temp-file "isolated-proj-" t)))
                                (doc-file (expand-file-name "doc.txt" temp-dir)))
                           (unwind-protect
                               (progn
                                 (with-temp-file doc-file (insert "v1"))
                                 (let* ((ws (make-macher-agent-workspace :project-root temp-dir))
                                        (parent-ctx (macher-agent--make-vfs-context :workspace ws :contents (list (macher-agent-vfs-make-entry "doc.txt" "v1" "v1"))))
                                        (parent-buf (generate-new-buffer "parent-agent")))
                                   (unwind-protect
                                       (progn
                                         (puthash (expand-file-name temp-dir) parent-ctx macher-agent-active-workspaces)
                                         (with-current-buffer parent-buf
                                           (setq-local default-directory temp-dir)
                                           (setq-local macher-agent--persistent-context parent-ctx)
                                           (let ((sub-buf (macher-agent-add-subagent "child-agent" temp-dir nil parent-ctx)))
                                             (with-current-buffer sub-buf
                                               ;; Verify child has an isolated cloned context
                                               (expect (eq macher-agent--persistent-context parent-ctx) :to-be nil)
                                               ;; Mutate child context
                                               (macher-agent--update-context-file macher-agent--persistent-context "doc.txt" "v2")
                                               ;; Parent remains unchanged before completion
                                               (expect (macher-agent-vfs-read parent-ctx "doc.txt") :to-equal "v1")
                                               ;; Clearing child context resets child without affecting parent
                                               (macher-agent-clear-context)
                                               (expect (macher-agent-vfs-read macher-agent--persistent-context "doc.txt") :to-equal "v1")
                                               ;; Update child context again
                                               (macher-agent--update-context-file macher-agent--persistent-context "doc.txt" "v3"))
                                             
                                             ;; Trigger A2A callback on task execution
                                             (with-current-buffer parent-buf
                                               (macher-agent-a2a-dispatch
                                                (list (list :type 'SEND_MESSAGE
                                                            :task-id "task-child"
                                                            :message "run"
                                                            :metadata (list :buffer_name "child-agent")))
                                                (lambda (_res)
                                                  ;; Verify parent context now reflects child modifications
                                                  (expect (macher-agent-vfs-read parent-ctx "doc.txt") :to-equal "v3"))))
                                             (let ((a2a-cb (with-current-buffer sub-buf macher-agent--a2a-callback)))
                                               (when a2a-cb
                                                 (funcall a2a-cb (list :task-id "task-child" :data "Done"))))
                                             (kill-buffer sub-buf))))
                                     (kill-buffer parent-buf))))
                             (delete-directory temp-dir t))))

                     (it "clears active presets during setup if the restored session tag is present"
                         (let ((buf (generate-new-buffer "restored-session-buf"))
                               (ctx (macher--make-context :workspace (make-macher-agent-workspace :project-root "/tmp/") :contents nil)))
                           (unwind-protect
                               (with-current-buffer buf
                                 (setq-local default-directory "/tmp/")
                                 (setq-local macher-agent--is-workspace t)
                                 (setq-local macher-agent--persistent-context ctx)
                                 (setq-local macher-agent-presets '(some-preset))
                                 (setq-local macher-agent--is-restored-session t)
                                 (spy-on 'macher-agent-root :and-return-value "/tmp/")
                                 (macher-agent-setup-gptel-buffer)
                                 (expect macher-agent-presets :to-be nil)
                                 (expect macher-agent--is-restored-session :to-be nil))
                             (when (buffer-live-p buf)
                               (with-current-buffer buf
                                 (setq-local macher-agent-presets nil))
                               (kill-buffer buf)))))

                     (it "handles exclusive override flag by resetting accumulated base and preceding state"
                         (let* ((tool1 (gptel-make-tool :name "tool1" :description "tool1"))
                                (toolA (gptel-make-tool :name "toolA" :description "toolA"))
                                (toolB (gptel-make-tool :name "toolB" :description "toolB"))
                                (gptel-tools (list tool1 toolA toolB))
                                (base-state `(:model gpt-3.5-turbo
                                                     :system "Base system message"
                                                     :temperature 0.7
                                                     :max-tokens 100
                                                     :tools (,tool1)
                                                     :known-presets ((preset-a . (:system "Preset A prompt" :temperature 0.5 :tools (,toolA)))
                                                                     (preset-b . (:system "Preset B prompt" :exclusive t :temperature 0.2 :tools (,toolB))))))
                                (presets '(preset-a preset-b)))
                           (let ((payload (macher-agent-compose-payload base-state presets)))
                             ;; Since preset-b has exclusive t, it resets preset-a and base-state!
                             (expect (plist-get payload :system) :to-equal "### Skill: preset-b\nPreset B prompt\n")
                             (expect (plist-get payload :temperature) :to-equal 0.2)
                             (expect (mapcar #'gptel-tool-name (plist-get payload :tools)) :to-equal '("toolB")))))

                     (describe "Phase 1: Refactored Preset Payload Composition Reducer Pipeline"
                               (it "defines macher-agent-preset-pipeline-functions with all step functions in order"
                                   (expect macher-agent-preset-pipeline-functions
                                           :to-equal '(macher-agent-preset-pipe--exclusive
                                                       macher-agent-preset-pipe--system
                                                       macher-agent-preset-pipe--tools
                                                       macher-agent-preset-pipe--ptc
                                                       macher-agent-preset-pipe--boot
                                                       macher-agent-preset-pipe--parameters)))

                               (it "clears base and preceding state in macher-agent-preset-pipe--exclusive when exclusive is t"
                                   (let ((state (list :system "existing sys" :tools '(t1) :ptc-primitives '(p1) :boot-directive "boot" :model 'gpt-4o))
                                         (item-exclusive '(preset my-preset (:exclusive t)))
                                         (item-non-exclusive '(preset my-preset (:exclusive nil)))
                                         (item-tool '(tool tool1)))
                                     (let ((res (macher-agent-preset-pipe--exclusive state item-exclusive)))
                                       (expect res :to-be state)
                                       (expect (plist-get res :system) :to-be nil)
                                       (expect (plist-get res :tools) :to-be nil)
                                       (expect (plist-get res :ptc-primitives) :to-be nil)
                                       (expect (plist-get res :boot-directive) :to-be nil)
                                       (expect (plist-get res :model) :to-equal 'gpt-4o))
                                     (expect (macher-agent-preset-pipe--exclusive state item-non-exclusive) :to-equal state)
                                     (expect (macher-agent-preset-pipe--exclusive state item-tool) :to-equal state)))

                               (it "directly mutates state in-place without memory allocation in macher-agent-preset-pipe--exclusive"
                                   (let* ((state (list :system "prompt" :tools '(tool1) :ptc-primitives '(p1) :boot-directive "boot" :model 'gpt-4))
                                          (item '(preset my-preset (:exclusive t)))
                                          (res (macher-agent-preset-pipe--exclusive state item)))
                                     (expect res :to-be state)
                                     (expect (plist-get state :system) :to-be nil)
                                     (expect (plist-get state :tools) :to-be nil)
                                     (expect (plist-get state :ptc-primitives) :to-be nil)
                                     (expect (plist-get state :boot-directive) :to-be nil)
                                     (expect (plist-get state :model) :to-equal 'gpt-4)))

                               (it "merges system prompt in macher-agent-preset-pipe--system"
                                   (let ((state '(:system "Initial prompt"))
                                         (item '(preset sys-preset (:system "Added prompt"))))
                                     (let ((res (macher-agent-preset-pipe--system state item)))
                                       (expect (plist-get res :system) :to-match "Initial prompt")
                                       (expect (plist-get res :system) :to-match "Added prompt"))
                                     (expect (macher-agent-preset-pipe--system state '(tool tool1)) :to-equal state)))

                               (it "merges tools and standalone tools in macher-agent-preset-pipe--tools"
                                   (let* ((tool-obj (if (fboundp 'gptel-make-tool)
                                                        (gptel-make-tool :name "t1" :description "d1")
                                                      "t1"))
                                          (state nil)
                                          (item-preset `(preset tool-preset (:tools (,tool-obj))))
                                          (item-tool `(tool ,tool-obj)))
                                     (spy-on 'gptel-tool-p :and-return-value t)
                                     (let ((res1 (macher-agent-preset-pipe--tools state item-preset)))
                                       (expect (plist-get res1 :tools) :not :to-be nil))
                                     (let ((res2 (macher-agent-preset-pipe--tools state item-tool)))
                                       (expect (plist-get res2 :tools) :to-equal (list tool-obj)))))

                               (it "handles fully instantiated gptel-tool structures intact in macher-agent--compose-merge-tools"
                                   (let* ((instantiated-tool (gptel-make-tool :name "instantiated_tool"
                                                                              :function (lambda () "ok")
                                                                              :description "Fully instantiated tool struct"))
                                          (current-tools nil)
                                          (res (macher-agent--compose-merge-tools current-tools instantiated-tool)))
                                     (expect res :to-equal (list instantiated-tool))
                                     (expect (car res) :to-equal instantiated-tool)))

                               (it "converts strings, symbols, and abstract lists to gptel-tool objects in macher-agent--compose-merge-tools"
                                   (let* ((mock-tool-str (gptel-make-tool :name "tool_str" :description "str tool"))
                                          (mock-tool-sym (gptel-make-tool :name "tool_sym" :description "sym tool"))
                                          (mock-tool-cat (gptel-make-tool :name "tool_cat" :description "cat tool")))
                                     (spy-on 'gptel-get-tool :and-call-fake
                                             (lambda (path)
                                               (cond
                                                ((equal path "tool_str") mock-tool-str)
                                                ((equal path "tool_sym") mock-tool-sym)
                                                ((equal path '("emacs" "tool_cat")) mock-tool-cat)
                                                (t nil))))
                                     (let ((res-str (macher-agent--compose-merge-tools nil "tool_str"))
                                           (res-sym (macher-agent--compose-merge-tools nil 'tool_sym))
                                           (res-cat (macher-agent--compose-merge-tools nil '("emacs" "tool_cat"))))
                                       (expect (car res-str) :to-equal mock-tool-str)
                                       (expect (car res-sym) :to-equal mock-tool-sym)
                                       (expect (car res-cat) :to-equal mock-tool-cat))))

                               (it "passes gptel-tool objects intact and resolves strings, symbols, and abstract lists in macher-agent-preset-pipe--tools"
                                   (let* ((tool-struct (gptel-make-tool :name "struct_tool" :description "struct tool"))
                                          (tool-str-obj (gptel-make-tool :name "str_tool" :description "str tool"))
                                          (state nil)
                                          (preset-item `(preset my-preset (:tools (,tool-struct "str_tool"))))
                                          (tool-item `(tool ,tool-struct)))
                                     (spy-on 'gptel-get-tool :and-call-fake
                                             (lambda (path)
                                               (cond
                                                ((equal path "str_tool") tool-str-obj)
                                                (t nil))))
                                     (let ((res1 (macher-agent-preset-pipe--tools state preset-item)))
                                       (expect (plist-get res1 :tools) :to-equal (list tool-struct tool-str-obj)))
                                     (let ((res2 (macher-agent-preset-pipe--tools state tool-item)))
                                       (expect (plist-get res2 :tools) :to-equal (list tool-struct)))))

                               (it "handles raw property lists for tool definitions in macher-agent--compose-merge-tools"
                                   (let* ((raw-plist '(:name "plist_tool" :description "Plist tool description"))
                                          (res (macher-agent--compose-merge-tools nil raw-plist)))
                                     (expect (length res) :to-equal 1)
                                     (expect (gptel-tool-p (car res)) :to-be t)
                                     (expect (gptel-tool-name (car res)) :to-equal "plist_tool")))

                               (it "handles raw property lists for standalone tool definitions in macher-agent-preset-pipe--tools"
                                   (let* ((raw-plist '(:name "standalone_plist_tool" :description "Standalone plist description"))
                                          (state nil)
                                          (tool-item `(tool ,raw-plist))
                                          (res (macher-agent-preset-pipe--tools state tool-item))
                                          (tools (plist-get res :tools)))
                                     (expect (length tools) :to-equal 1)
                                     (expect (gptel-tool-p (car tools)) :to-be t)
                                     (expect (gptel-tool-name (car tools)) :to-equal "standalone_plist_tool")))

                               (it "merges and deduplicates ptc primitives in macher-agent-preset-pipe--ptc"
                                   (let ((state '(:ptc-primitives (prim1 prim2)))
                                         (item '(preset ptc-preset (:ptc-primitives (prim2 prim3)))))
                                     (let ((res (macher-agent-preset-pipe--ptc state item)))
                                       (expect (plist-get res :ptc-primitives) :to-equal '(prim1 prim2 prim3)))))

                               (it "applies boot directive in macher-agent-preset-pipe--boot"
                                   (let ((state nil)
                                         (item '(preset boot-preset (:boot-directive "Run boot instructions."))))
                                     (let ((res (macher-agent-preset-pipe--boot state item)))
                                       (expect (plist-get res :boot-directive) :to-equal "Run boot instructions."))))

                               (it "applies model parameters in macher-agent-preset-pipe--parameters"
                                   (let ((state '(:temperature 0.5))
                                         (item '(preset param-preset (:model claude-3-5-sonnet :temperature 0.2 :max-tokens 4000))))
                                     (let ((res (macher-agent-preset-pipe--parameters state item)))
                                       (expect (plist-get res :model) :to-equal 'claude-3-5-sonnet)
                                       (expect (plist-get res :temperature) :to-equal 0.2)
                                       (expect (plist-get res :max-tokens) :to-equal 4000))))

                               (it "pre-flattens preset parent dependencies in topological order and avoids cycles"
                                   (let ((known '((grandparent . (:system "Grandparent"))
                                                  (parent . (:parents (grandparent) :system "Parent"))
                                                  (child . (:parents (parent) :system "Child"))
                                                  (cyclic-a . (:parents (cyclic-b) :system "Cyclic A"))
                                                  (cyclic-b . (:parents (cyclic-a) :system "Cyclic B")))))
                                     (let ((flattened (macher-agent--flatten-preset-dependencies '(child) known)))
                                       (expect (mapcar #'cadr flattened) :to-equal '(grandparent parent child)))
                                     (let ((flattened-cycle (macher-agent--flatten-preset-dependencies '(cyclic-a) known)))
                                       (expect (length flattened-cycle) :to-equal 2))))

                               (it "composes payload by pre-flattening and reducing over step functions"
                                   (let* ((tool1 (if (fboundp 'gptel-make-tool) (gptel-make-tool :name "tool1" :description "t1") "tool1"))
                                          (tool2 (if (fboundp 'gptel-make-tool) (gptel-make-tool :name "tool2" :description "t2") "tool2"))
                                          (base-state `(:system "Base"
                                                                :temperature 0.8
                                                                :known-presets ((parent-preset . (:system "Parent prompt" :ptc-primitives (p1)))
                                                                                (child-preset . (:parents (parent-preset) :system "Child prompt" :boot-directive "Boot child" :ptc-primitives (p2) :temperature 0.3)))))
                                          (composed (macher-agent-compose-payload base-state '(child-preset))))
                                     (expect (plist-get composed :system) :to-match "Parent prompt")
                                     (expect (plist-get composed :system) :to-match "Child prompt")
                                     (expect (plist-get composed :ptc-primitives) :to-equal '(p1 p2))
                                     (expect (plist-get composed :boot-directive) :to-equal "Boot child")
                                     (expect (plist-get composed :temperature) :to-equal 0.3))))

                     (describe "Refactored Unified Transmission Reducer Pipeline"
                               (it "inits core subagent directive when buffer is a subagent"
                                   (let* ((orig-buf (generate-new-buffer "test-subagent-buf"))
                                          (payload nil))
                                     (with-current-buffer orig-buf
                                       (setq-local macher-agent--is-subagent t))
                                     (setq payload (macher-agent-pipe--init-core-directives payload orig-buf nil nil nil))
                                     (expect (length (plist-get payload :directives)) :to-equal 1)
                                     (expect (car (plist-get payload :directives)) :to-match "CRITICAL DIRECTIVE:")
                                     (kill-buffer orig-buf)))

                               (it "appends boot directive on initial request when no gptel response property exists"
                                   (let* ((orig-buf (generate-new-buffer "test-initial-request-boot-buf"))
                                          (payload nil))
                                     (with-current-buffer orig-buf
                                       (setq-local macher-agent--boot-directive "Execute boot setup now."))
                                     (setq payload (macher-agent-pipe--append-boot-directive payload orig-buf nil nil nil))
                                     (expect (length (plist-get payload :directives)) :to-equal 1)
                                     (expect (car (plist-get payload :directives)) :to-equal "Execute boot setup now.")
                                     (kill-buffer orig-buf)))

                               (it "does not append boot directive on subsequent request when gptel response property exists"
                                   (let* ((orig-buf (generate-new-buffer "test-subsequent-request-boot-buf"))
                                          (payload nil))
                                     (with-current-buffer orig-buf
                                       (setq-local macher-agent--boot-directive "Execute boot setup now.")
                                       (insert "Previous assistant response")
                                       (put-text-property (point-min) (point-max) 'gptel 'response))
                                     (setq payload (macher-agent-pipe--append-boot-directive payload orig-buf nil nil nil))
                                     (expect (plist-get payload :directives) :to-be nil)
                                     (kill-buffer orig-buf)))

                               (it "drains thought queue and compiles directives into system prompt"
                                   (let* ((orig-buf (generate-new-buffer "test-thought-queue-buf"))
                                          (payload (list :system "Base System Prompt")))
                                     (with-current-buffer orig-buf
                                       (macher-agent-add-pending-instruction "Thought 1"))
                                     (setq payload (macher-agent-pipe--drain-thought-queue payload orig-buf nil nil nil))
                                     (expect (length (plist-get payload :directives)) :to-equal 1)
                                     (with-current-buffer orig-buf
                                       (expect macher-agent--pending-instructions-queue :to-be nil))
                                     (setq payload (macher-agent-pipe--compile-directives payload orig-buf nil nil nil))
                                     (expect (plist-get payload :system) :to-match "Base System Prompt\n\nUSER OVERRIDE DIRECTIVE:\nThought 1")
                                     (expect (plist-get payload :directives) :to-be nil)
                                     (kill-buffer orig-buf))))

                     (describe "Tool Schema Validation and Lifecycle Hooks"
                               (before-all
                                (macher-agent-make-tool mock-async-contract-tool
                                    "Mock async tool"
                                  :category "test"
                                  :args (list (list :name "arg1" :type 'string) (list :name "arg2" :type 'string))
                                  :command-fn (lambda (payload _context _root)
                                                (format "Async %s %s" (plist-get payload :arg1) (plist-get payload :arg2)))) ; <-- Missing parenthesis added here

                                (macher-agent-make-tool mock-sync-contract-tool
                                    "Mock sync tool"
                                  :category "test"
                                  :args (list (list :name "arg1" :type 'string))
                                  :command-fn (lambda (payload _context _root)
                                                (format "Sync %s" (plist-get payload :arg1))))

                                (macher-agent-make-tool mock-complex-schema-tool
                                    "Mock complex schema tool"
                                  :category "test"
                                  :args (list (list :name "tasks"
                                                    :type 'array
                                                    :items '(:type object
                                                                   :properties (:name (:type string)
                                                                                      :presets (:type array :items (:type string))))))
                                  :command-fn (lambda (_payload _context _root)
                                                "Valid schema")))

                               (it "validates schema parameters correctly for valid payloads"
                                   (let ((callback-result nil))
                                     (funcall (gptel-tool-function mock-complex-schema-tool)
                                              (lambda (res) (setq callback-result res))
                                              '[(:name "task1" :presets ["preset1" "preset2"])])
                                     (expect callback-result :to-match "Valid schema")))

                               (it "returns a descriptive error for malformed schema parameters"
                                   (let ((callback-result nil))
                                     (funcall (gptel-tool-function mock-complex-schema-tool)
                                              (lambda (res) (setq callback-result res))
                                              '[(:name "task1" :presets "worker")])
                                     (expect callback-result :to-match "The 'tasks\\[0\\]\\.presets' parameter must be an array, not string")))

                               (it "generates variadic signatures for async tools to safely absorb FSM contexts"
                                   (let* ((tool-fn (gptel-tool-function mock-async-contract-tool))
                                          (arity (func-arity tool-fn)))
                                     (expect (car arity) :to-equal 1)
                                     (expect (cdr arity) :to-equal 'many)))

                               (it "generates variadic signatures for sync tools to safely absorb FSM contexts"
                                   (let* ((tool-fn (gptel-tool-function mock-sync-contract-tool))
                                          (arity (func-arity tool-fn)))
                                     (expect (car arity) :to-equal 1)
                                     (expect (cdr arity) :to-equal 'many)))

                               (before-each
                                (setq macher-agent-pre-tool-use-hook nil)
                                (setq macher-agent-permission-request-hook nil)
                                (setq macher-agent-post-tool-use-hook nil)
                                (setq macher-agent-post-tool-use-failure-hook nil))

                               (after-all
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
                                              (lambda (res) (setq callback-result res))
                                              "hello")
                                     (expect pre-called :to-equal (list 'mock-sync-contract-tool '(:arg1 "hello")))
                                     (expect callback-result :to-match "Execution blocked by macher-agent-pre-tool-use-hook")))

                               (it "runs pre-tool-use-hook and aborts if it signals an error"
                                   (let ((callback-result nil))
                                     (add-hook 'macher-agent-pre-tool-use-hook
                                               (lambda (_name _args)
                                                 (error "Custom pre error")))
                                     (funcall (gptel-tool-function mock-sync-contract-tool)
                                              (lambda (res) (setq callback-result res))
                                              "hello")
                                     (expect callback-result :to-match "Execution blocked by error in macher-agent-pre-tool-use-hook: Custom pre error")))

                               (it "runs permission-request-hook and succeeds by default if empty"
                                   (let ((callback-result nil))
                                     (funcall (gptel-tool-function mock-sync-contract-tool)
                                              (lambda (res) (setq callback-result res))
                                              "hello")
                                     (expect callback-result :to-match "Sync hello")))

                               (it "runs permission-request-hook and aborts if it returns nil"
                                   (let ((perm-called nil)
                                         (callback-result nil))
                                     (add-hook 'macher-agent-permission-request-hook
                                               (lambda (name args)
                                                 (setq perm-called (list name args))
                                                 nil))
                                     (funcall (gptel-tool-function mock-sync-contract-tool)
                                              (lambda (res) (setq callback-result res))
                                              "hello")
                                     (expect perm-called :to-equal (list 'mock-sync-contract-tool '(:arg1 "hello")))
                                     (expect callback-result :to-match "Permission denied by macher-agent-permission-request-hook")))

                               (it "runs post-tool-use-hook upon successful execution"
                                   (let ((post-called nil)
                                         (callback-result nil))
                                     (add-hook 'macher-agent-post-tool-use-hook
                                               (lambda (name args result)
                                                 (setq post-called (list name args result))))
                                     (funcall (gptel-tool-function mock-sync-contract-tool)
                                              (lambda (res) (setq callback-result res))
                                              "world")
                                     (expect callback-result :to-match "Sync world")
                                     (expect post-called :to-equal (list 'mock-sync-contract-tool '(:arg1 "world") "Sync world"))))

                               (it "runs post-tool-use-failure-hook if the tool body throws an error"
                                   (macher-agent-make-tool mock-error-tool
				                       "Mock error tool"
			                         :category "test"
			                         :args (list (list :name "arg1" :type 'string))
			                         :command-fn (lambda (_payload _context _root)
							                       (error "Failing intentionally")))
                                   (let ((failure-called nil)
                                         (callback-result nil))
                                     (add-hook 'macher-agent-post-tool-use-failure-hook
                                               (lambda (name args err)
                                                 (setq failure-called (list name args err))))
                                     (funcall (gptel-tool-function mock-error-tool)
                                              (lambda (res) (setq callback-result res))
                                              "fail")
                                     (expect callback-result :to-match "Failing intentionally")
                                     (expect (car failure-called) :to-be 'mock-error-tool)
                                     (expect (cadr failure-called) :to-equal '(:arg1 "fail"))
                                     (expect (error-message-string (caddr failure-called)) :to-equal "Failing intentionally"))))

                     (describe "macher-agent--extract-prop"
                               (it "extracts properties from plists with keyword and string keys"
                                   (let ((plist '(:foo-bar "val1" :baz 123)))
                                     (expect (macher-agent--extract-prop plist :foo-bar) :to-equal "val1")
                                     (expect (macher-agent--extract-prop plist "foo_bar") :to-equal "val1")
                                     (expect (macher-agent--extract-prop plist :baz) :to-equal 123)
                                     (expect (macher-agent--extract-prop plist :missing) :to-equal 'macher-missing)))

                               (it "extracts properties from alists with keyword and string keys"
                                   (let ((alist '((:foo-bar . "val1") (:baz . 456))))
                                     (expect (macher-agent--extract-prop alist :foo-bar) :to-equal "val1")
                                     (expect (macher-agent--extract-prop alist "foo_bar") :to-equal "val1")
                                     (expect (macher-agent--extract-prop alist :baz) :to-equal 456)
                                     (expect (macher-agent--extract-prop alist :missing) :to-equal 'macher-missing)))

                               (it "extracts properties from hash-tables with keyword and string keys"
                                   (let ((ht (make-hash-table :test 'equal)))
                                     (puthash :foo-bar "val1" ht)
                                     (puthash :baz 789 ht)
                                     (expect (macher-agent--extract-prop ht :foo-bar) :to-equal "val1")
                                     (expect (macher-agent--extract-prop ht "foo_bar") :to-equal "val1")
                                     (expect (macher-agent--extract-prop ht :baz) :to-equal 789)
                                     (expect (macher-agent--extract-prop ht :missing) :to-equal 'macher-missing))))

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
                                         (delete-directory mock-dir t))))))

                     (describe "Programmatic Tool Calling (PTC)"
                               (it "executes a PTC script, yielding for subagents and resuming with results"
                                   (let* ((macher-agent--active-ptc-primitives '(spawn-subagent delegate-tasks-to-subagents read-file))
                                          (ctx (macher--make-context :workspace (make-macher-agent-workspace :project-root "/mock/ptc/") :contents nil))
                                          (script "(let* ((a1 (spawn-subagent \"agent-alpha\" nil))
                                     (a2 (spawn-subagent \"agent-beta\" nil))
                                     (tasks (list (list :buffer_name a1 :instructions \"task1\")
                                                  (list :buffer_name a2 :instructions \"task2\"))))
                                (delegate-tasks-to-subagents tasks))")
                                          (spawn-calls nil)
                                          (success-result nil)
                                          (error-result nil))
                                     (spy-on 'macher-agent-add-subagent :and-call-fake
                                             (lambda (name _root _presets _context _user-presets)
                                               (push name spawn-calls)
                                               (generate-new-buffer name)))
                                     (spy-on 'macher-agent-a2a-dispatch :and-call-fake
                                             (lambda (_tasks callback)
                                               (funcall callback
                                                        (vector
                                                         (list :status 'success :data "result-alpha" :buffer-name "agent-alpha")
                                                         (list :status 'success :data "result-beta" :buffer-name "agent-beta")))))

                                     (macher-agent-execute-ptc-script
                                      script
                                      ctx
                                      (lambda (val) (setq success-result val))
                                      (lambda (err) (setq error-result err)))

                                     (expect error-result :to-be nil)
                                     (expect (reverse spawn-calls) :to-equal '("agent-alpha" "agent-beta"))
                                     (expect success-result :to-equal [(:status success :data "result-alpha" :buffer-name "agent-alpha")
                                                                       (:status success :data "result-beta" :buffer-name "agent-beta")])))

                               (it "safely evaluates synchronous expressions without leaking iter-end-of-sequence"
                                   (let ((val (macher-agent-sandbox-run '(+ 10 20) nil)))
                                     (expect val :to-equal 30)))

                               (it "yields PTC tool interrupts when evaluated via eval-iter"
                                   (let* ((macher-agent--active-ptc-primitives '(spawn-subagent))
                                          (iter (macher-agent-sandbox--eval-iter '(spawn-subagent "test" nil) nil))
                                          (yield-val (iter-next iter)))
                                     (expect (plist-get yield-val :interrupt) :to-equal 'tool-call)
                                     (expect (plist-get yield-val :name) :to-equal 'spawn-subagent)))



                               (it "injects PTC prompt instructions conditionally based on primitive matches"
                                   (let* ((tool (gptel-make-tool
                                                 :name "spawn-subagent"
                                                 :description "Spawn subagent"
                                                 :args '((:name "path" :type "string"))))
                                          (gptel-tools (list tool))
                                          (macher-agent--active-ptc-primitives '(spawn-subagent))
                                          (base-prompt "Initial system prompt.")
                                          (injected (macher-agent--inject-ptc-prompt base-prompt)))
                                     (expect injected :to-match "^Initial system prompt\\.")
                                     (expect injected :to-match "=== PROGRAMMATIC TOOL CALLING (PTC) ===")
                                     (expect injected :to-match "spawn-subagent -> (defun spawn-subagent (path)")
                                     (let ((macher-agent--active-ptc-primitives '(unmatched-primitive)))
                                       (expect (macher-agent--inject-ptc-prompt base-prompt) :to-equal base-prompt))))

                               (it "extracts raw payload and applies success-fn formatting"
                                   (let* ((res "raw payload")
                                          (cb-raw-val nil)
                                          (cb-formatted-val nil)
                                          (cb-raw (macher-agent--build-success-callback
                                                   'test-tool nil nil nil
                                                   (lambda (res) (setq cb-raw-val (plist-get res :data)))))
                                          (cb-fmt (macher-agent--build-success-callback
                                                   'test-tool nil
                                                   (lambda (p) (format "Formatted: %s" p))
                                                   nil
                                                   (lambda (res) (setq cb-formatted-val (plist-get res :data))))))
                                     (funcall cb-raw res)
                                     (expect cb-raw-val :to-equal "raw payload")
                                     (funcall cb-fmt res)
                                     (expect cb-formatted-val :to-equal "Formatted: raw payload")))

                               (it "suppresses patch creation during request processing when patch suppression is active"
                                   (let* ((ws (make-macher-agent-workspace :project-root "/mock/proj/"))
                                          (context (macher--make-context
                                                    :workspace ws
                                                    :contents (list (macher-agent-vfs-make-entry "/mock/proj/file.el" "old" "new"))))
                                          (fsm (gptel-make-fsm))
                                          (macher-agent--suppress-patch t))
                                     (spy-on 'macher--build-patch)
                                     (macher-agent-process-request context fsm)
                                     (expect 'macher--build-patch :not :to-have-been-called)))

                               (it "allows indirect PTC primitive calls via funcall and apply inside sandbox"
                                   (let* ((macher-agent--active-ptc-primitives '(spawn-subagent))
                                          (iter1 (macher-agent-sandbox--funcall-iter 'spawn-subagent '("agent-alpha")))
                                          (yield1 (iter-next iter1)))
                                     (expect (plist-get yield1 :interrupt) :to-equal 'tool-call)
                                     (expect (plist-get yield1 :name) :to-equal 'spawn-subagent)
                                     (expect (plist-get yield1 :args) :to-equal '("agent-alpha"))))

                               (it "converts alist arguments passed to PTC primitives to plists on tool call interrupts"
                                   (let* ((macher-agent--active-ptc-primitives '(spawn-subagent))
                                          (iter1 (macher-agent-sandbox--funcall-iter 'spawn-subagent '((path . "/tmp") (name . "test"))))
                                          (yield1 (iter-next iter1))
                                          (iter2 (macher-agent-sandbox--funcall-iter 'spawn-subagent '(((path . "/tmp") (name . "test")))))
                                          (yield2 (iter-next iter2)))
                                     (expect (plist-get yield1 :interrupt) :to-equal 'tool-call)
                                     (expect (plist-get yield1 :name) :to-equal 'spawn-subagent)
                                     (expect (plist-get yield1 :args) :to-equal '(:path "/tmp" :name "test"))
                                     (expect (plist-get yield2 :interrupt) :to-equal 'tool-call)
                                     (expect (plist-get yield2 :name) :to-equal 'spawn-subagent)
                                     (expect (plist-get yield2 :args) :to-equal '(:path "/tmp" :name "test"))))

                               (it "matches PTC primitives bidirectionally and falls back to gptel-tools when active list is nil"
                                   (let ((macher-agent--active-ptc-primitives '(spawn-subagent)))
                                     (expect (macher-agent--ptc-primitive-p 'spawn_subagent) :to-be t)
                                     (expect (macher-agent--ptc-primitive-p 'spawn-subagent) :to-be t))
                                   (let ((macher-agent--active-ptc-primitives '("spawn_subagent")))
                                     (expect (macher-agent--ptc-primitive-p 'spawn-subagent) :to-be t))
                                   (let* ((macher-agent--active-ptc-primitives nil)
                                          (tool (gptel-make-tool :name "spawn_subagent" :description "test" :args nil))
                                          (gptel-tools (list tool)))
                                     (expect (macher-agent--ptc-primitive-p 'spawn-subagent) :to-be t)
                                     (expect (macher-agent--ptc-primitive-p 'spawn_subagent) :to-be t)))

                               (it "bypasses success-fn formatting and preserves raw command-fn payload during PTC execution"
                                   (let* ((mock-tool (macher-agent-make-tool mock-ptc-format-tool
                                                         "Test tool"
                                                       :category "test"
                                                       :args '((:name "val" :type string))
                                                       :command-fn (lambda (payload _context _root)
                                                                     `((status . "success") (value . ,(plist-get payload :val))))
                                                       :success-fn (lambda (res _payload)
                                                                     (format "Formatted output: %s" (cdr (assoc 'value res))))))
                                          (normal-res nil)
                                          (ptc-res nil))
                                     ;; Standard execution (outside PTC)
                                     (funcall (gptel-tool-function mock-tool)
                                              (lambda (res) (setq normal-res res))
                                              "hello")
                                     (expect normal-res :to-equal "Formatted output: hello")

                                     ;; PTC execution
                                     (let ((macher-agent--active-ptc-execution t))
                                       (funcall (gptel-tool-function mock-tool)
                                                (lambda (res) (setq ptc-res res))
                                                "hello"))
                                     (expect ptc-res :to-equal '((status . "success") (value . "hello")))))))))




(provide 'macher-agent-test)
