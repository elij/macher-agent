;;; macher-agent-core-test.el --- Core behaviour tests for macher-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; This test suite enforces the specification for macher-agent,
;; focusing on VFS optimistic concurrency, lexical state management,
;; sandbox isolation, and diff splitting behaviours.

;;; Code:

(require 'subr-x)
(require 'buttercup)
(require 'macher-agent-macher)
(require 'cl-lib)
(require 'macher-agent)

(defvar gptel--fsm)
(defvar macher-agent--active-fsm)
(defvar gptel--fsm-last)

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
                                (macher-agent-vfs-write (macher-agent-workspace-vfs-buffers workspace) (macher-agent-workspace-mtime-tracker workspace) file-path "New content")
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

                          (macher-agent-vfs-write (macher-agent-workspace-vfs-buffers workspace) (macher-agent-workspace-mtime-tracker workspace) file-path "Agent A changes")

                          (let ((read-content (macher-agent-vfs-read (macher-agent-workspace-vfs-buffers workspace) nil file-path)))
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

                            (macher-agent-sandbox-inflate "/tmp/macher-sandbox/" (macher-agent-workspace-vfs-buffers workspace) (macher-agent-context-root ctx) (macher-agent--get-context-contents ctx))

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

                    (it "dispatches point-to-point A2A payloads and invokes completion callback"
                        (spy-on 'macher-agent-resolve-context :and-return-value nil)
                        (let* ((callback-result nil)
                               (sub-buf (get-buffer-create "sub-agent-buf"))
                               (payloads (list (list :type 'SEND_MESSAGE
                                                     :task-id "task-001"
                                                     :message "Do something"
                                                     :metadata (list :buffer_name "sub-agent-buf" :background t))))
                               (callback (lambda (res) (setq callback-result res))))
                          (unwind-protect
                              (progn
                                (cl-letf (((symbol-function 'gptel-send)
                                           (lambda ()
                                             (let ((cb (bound-and-true-p macher-agent--a2a-callback))
                                                   (task-id (bound-and-true-p macher-agent--current-task-id)))
                                               (when cb
                                                 (funcall cb (list :status 'success :data "Done" :task-id task-id)))))))
                                  (macher-agent-a2a-dispatch payloads callback))
                                (expect (length callback-result) :to-equal 1)
                                (expect (plist-get (aref callback-result 0) :status) :to-be 'success)
                                (with-current-buffer sub-buf
                                  (expect (bound-and-true-p macher-agent--ready-to-reap) :to-be nil)))
                            (kill-buffer sub-buf))))

                    (it "returns a controlled error state when dispatching to a missing or invalid sub-agent buffer"
                        (let* ((callback-result nil)
                               (payloads (list (list :type 'SEND_MESSAGE
                                                     :task-id "task-err-001"
                                                     :message "Do something"
                                                     :metadata (list :buffer_name "non-existent-buffer"))))
                               (callback (lambda (res) (setq callback-result res))))
                          (macher-agent-a2a-dispatch payloads callback)
                          (expect (length callback-result) :to-equal 1)
                          (expect (plist-get (aref callback-result 0) :status) :to-be 'error)
                          (expect (plist-get (aref callback-result 0) :error)
                                  :to-equal "ERROR: Sub-agent buffer 'non-existent-buffer' not found."))))

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
                          (let ((macher-agent-max-context-chars '((nil . 25))))
                            (macher-agent-transformer-snip-context nil nil))
                          (expect (buffer-string) :to-match "Latest user query content")))

                    (it "protects frontmatter header when snipping context history"
                        (with-temp-buffer
                          (insert "---\nkey: value\n---\nEarly user prompt content\n")
                          (let ((resp "Previous response boundary\n"))
                            (put-text-property 0 (length resp) 'gptel 'response resp)
                            (insert resp))
                          (insert "Latest user query content")
                          (let ((macher-agent-max-context-chars '((nil . 25))))
                            (macher-agent-transformer-snip-context nil nil))
                          (expect (buffer-string) :to-match "^---\nkey: value\n---")))

                    (it "registers sync prompt transformer and transformers in setup-gptel-buffer"
                        (let ((macher-agent-prompt-transformers '(t1 t2)))
                          (spy-on 'macher-agent-resolve-context :and-return-value t)
                          (with-temp-buffer
                            (macher-agent-setup-gptel-buffer)
                            (expect gptel-prompt-transform-functions
                                    :to-equal '(macher-agent--transform-inject-context macher-agent-sync-prompt-transformer t t1 t2)))))

                    (it "triggers pending media injection on FSM updates"
                        (let* ((ctx (macher--make-context :workspace nil :contents nil))
                               (fsm (gptel-make-fsm)))
                          (setf (gptel-fsm-info fsm) (list :macher-agent-context ctx))
                          (macher-agent--set-context-data ctx :pending-media (list "data"))
                          (spy-on 'macher-agent--perform-pending-media-injection)
                          (macher-agent--inject-media-fsm-advice (lambda (&rest _) nil) fsm 'WAIT)
                          (expect 'macher-agent--perform-pending-media-injection :to-have-been-called-with fsm))))

          (describe "8. Buffer Resolution and User Interface Delegation (Phase 4)"
                    (describe "macher-agent--resolve-buffer-name"
                              (it "resolves buffer objects to buffer name string"
                                  (let ((buf (get-buffer-create "test-resolve-buf-obj")))
                                    (unwind-protect
                                        (expect (macher-agent--resolve-buffer-name buf) :to-equal "test-resolve-buf-obj")
                                      (kill-buffer buf))))

                              (it "resolves string buffer name when buffer exists"
                                  (let ((buf (get-buffer-create "test-resolve-buf-str")))
                                    (unwind-protect
                                        (expect (macher-agent--resolve-buffer-name "test-resolve-buf-str") :to-equal "test-resolve-buf-str")
                                      (kill-buffer buf))))

                              (it "resolves file paths to buffer names via native get-file-buffer"
                                  (let* ((temp-file (make-temp-file "macher-resolve-test"))
                                         (buf (find-file-noselect temp-file)))
                                    (unwind-protect
                                        (progn
                                          (expect (macher-agent--resolve-buffer-name temp-file) :to-equal (buffer-name buf))
                                          (expect (macher-agent--resolve-buffer-name (expand-file-name temp-file)) :to-equal (buffer-name buf)))
                                      (when (buffer-live-p buf) (kill-buffer buf))
                                      (when (file-exists-p temp-file) (delete-file temp-file)))))

                              (it "returns original name string when buffer or file buffer is unmapped"
                                  (expect (macher-agent--resolve-buffer-name "/tmp/nonexistent-file-path-xyz.el")
                                          :to-equal "/tmp/nonexistent-file-path-xyz.el")))

                    (describe "macher-agent-ui-show"
                              (it "invokes macher-agent-display-subagent-fn with target buffer"
                                  (let* ((buf (get-buffer-create "test-ui-show-buf"))
                                         (displayed-buf nil)
                                         (macher-agent-display-subagent-fn (lambda (b) (setq displayed-buf b))))
                                    (unwind-protect
                                        (progn
                                          (macher-agent-ui-show buf)
                                          (expect displayed-buf :to-be buf))
                                      (kill-buffer buf))))

                              (it "defaults target buffer to current-buffer when omitted"
                                  (let* ((displayed-buf nil)
                                         (macher-agent-display-subagent-fn (lambda (b) (setq displayed-buf b))))
                                    (with-temp-buffer
                                      (let ((cur (current-buffer)))
                                        (macher-agent-ui-show)
                                        (expect displayed-buf :to-be cur)))))

                              (it "handles nil display function gracefully without throwing"
                                  (let ((macher-agent-display-subagent-fn nil))
                                    (expect (macher-agent-ui-show) :to-be nil)))

                              (it "confirms redundant macher-agent--show-ui function is removed"
                                  (expect (fboundp 'macher-agent--show-ui) :to-be nil)))))

(describe "9. Context Resolution and Prompt Injection"
          (describe "macher-agent--resolve-context"
                    (it "resolves context from FSM info plist"
                        (let* ((mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :macher-agent-context mock-ctx))))
                          (expect (macher-agent--resolve-context fsm) :to-be mock-ctx)))

                    (it "returns nil when FSM info plist does not contain context"
                        (let ((fsm (gptel-make-fsm :info (list :buffer "dummy"))))
                          (expect (macher-agent--resolve-context fsm) :to-be nil)))

                    (it "resolves context from buffer-local persistent context"
                        (let ((buf (get-buffer-create "test-resolve-ctx-buf"))
                              (mock-ctx (macher--make-context :contents nil)))
                          (unwind-protect
                              (with-current-buffer buf
                                (setq-local macher-agent--persistent-context mock-ctx)
                                (expect (macher-agent--resolve-context buf) :to-be mock-ctx))
                            (kill-buffer buf))))

                    (it "returns nil when buffer has no persistent context"
                        (let ((buf (get-buffer-create "test-resolve-no-ctx-buf")))
                          (unwind-protect
                              (with-current-buffer buf
                                (setq-local macher-agent--persistent-context nil)
                                (expect (macher-agent--resolve-context buf) :to-be nil))
                            (kill-buffer buf))))

                    (it "returns nil for non-FSM, non-buffer input values"
                        (expect (macher-agent--resolve-context nil) :to-be nil)
                        (expect (macher-agent--resolve-context "not-a-buffer") :to-be nil)
                        (expect (macher-agent--resolve-context 'some-symbol) :to-be nil)
                        (expect (macher-agent--resolve-context 12345) :to-be nil)
                        (expect (macher-agent--resolve-context '(:key "val")) :to-be nil)))

          (describe "macher-agent--get-active-context"
                    (it "returns active context using gptel--fsm via macher-agent--resolve-context"
                        (let* ((mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :macher-agent-context mock-ctx))))
                          (dlet ((gptel--fsm fsm)
                                 (macher-agent--active-fsm nil)
                                 (gptel--fsm-last nil))
                            (expect (macher-agent--get-active-context) :to-be mock-ctx))))

                    (it "returns active context using macher-agent--active-fsm when gptel--fsm is nil"
                        (let* ((mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :macher-agent-context mock-ctx))))
                          (dlet ((gptel--fsm nil)
                                 (macher-agent--active-fsm fsm)
                                 (gptel--fsm-last nil))
                            (expect (macher-agent--get-active-context) :to-be mock-ctx))))

                    (it "returns active context using gptel--fsm-last fallback"
                        (let* ((mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :macher-agent-context mock-ctx))))
                          (dlet ((gptel--fsm nil)
                                 (macher-agent--active-fsm nil)
                                 (gptel--fsm-last fsm))
                            (expect (macher-agent--get-active-context) :to-be mock-ctx))))

                    (it "returns nil when all FSM variables are unbound or nil"
                        (dlet ((gptel--fsm nil)
                               (macher-agent--active-fsm nil)
                               (gptel--fsm-last nil))
                          (expect (macher-agent--get-active-context) :to-be nil))))

          (describe "macher-agent--transform-inject-context"
                    (it "injects context from live originating buffer into FSM info"
                        (let* ((origin-buf (get-buffer-create "test-origin-buf"))
                               (mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :buffer origin-buf :model "test-model")))
                               (called nil))
                          (unwind-protect
                              (progn
                                (with-current-buffer origin-buf
                                  (setq-local macher-agent--persistent-context mock-ctx))
                                (macher-agent--transform-inject-context
                                 (lambda () (setq called t))
                                 fsm)
                                (expect called :to-be t)
                                (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be mock-ctx)
                                (expect (plist-get (gptel-fsm-info fsm) :model) :to-equal "test-model"))
                            (kill-buffer origin-buf))))

                    (it "does not inject context if originating buffer context is nil"
                        (let* ((origin-buf (get-buffer-create "test-origin-buf-no-ctx"))
                               (fsm (gptel-make-fsm :info (list :buffer origin-buf :model "test-model")))
                               (called nil))
                          (unwind-protect
                              (progn
                                (with-current-buffer origin-buf
                                  (setq-local macher-agent--persistent-context nil))
                                (macher-agent--transform-inject-context
                                 (lambda () (setq called t))
                                 fsm)
                                (expect called :to-be t)
                                (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be nil))
                            (kill-buffer origin-buf))))

                    (it "does not inject context if originating buffer is dead"
                        (let* ((origin-buf (get-buffer-create "test-origin-buf-dead"))
                               (mock-ctx (macher--make-context :contents nil))
                               (fsm nil)
                               (called nil))
                          (with-current-buffer origin-buf
                            (setq-local macher-agent--persistent-context mock-ctx))
                          (setq fsm (gptel-make-fsm :info (list :buffer origin-buf)))
                          (kill-buffer origin-buf)
                          (macher-agent--transform-inject-context
                           (lambda () (setq called t))
                           fsm)
                          (expect called :to-be t)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be nil)))

                    (it "executes callback when fsm is nil"
                        (let ((called nil))
                          (macher-agent--transform-inject-context
                           (lambda () (setq called t))
                           nil)
                          (expect called :to-be t)))

                    (it "executes callback when fsm has no buffer in info"
                        (let* ((fsm (gptel-make-fsm :info (list :model "test-model")))
                               (called nil))
                          (macher-agent--transform-inject-context
                           (lambda () (setq called t))
                           fsm)
                          (expect called :to-be t)
                          (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be nil)))

                    (it "updates existing context in FSM info"
                        (let* ((origin-buf (get-buffer-create "test-origin-buf-update"))
                               (old-ctx (macher--make-context :contents nil))
                               (new-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :buffer origin-buf :macher-agent-context old-ctx :model "test-model")))
                               (called nil))
                          (unwind-protect
                              (progn
                                (with-current-buffer origin-buf
                                  (setq-local macher-agent--persistent-context new-ctx))
                                (macher-agent--transform-inject-context
                                 (lambda () (setq called t))
                                 fsm)
                                (expect called :to-be t)
                                (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be new-ctx)
                                (expect (plist-get (gptel-fsm-info fsm) :model) :to-equal "test-model"))
                            (kill-buffer origin-buf))))

                    (it "handles nil async-fn gracefully"
                        (let* ((origin-buf (get-buffer-create "test-origin-buf-nil-fn"))
                               (mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :buffer origin-buf :model "test-model"))))
                          (unwind-protect
                              (progn
                                (with-current-buffer origin-buf
                                  (setq-local macher-agent--persistent-context mock-ctx))
                                (macher-agent--transform-inject-context nil fsm)
                                (expect (plist-get (gptel-fsm-info fsm) :macher-agent-context) :to-be mock-ctx))
                            (kill-buffer origin-buf)))))

          (describe "macher-agent--transformer-sync-context"
                    (it "synchronizes context and initializes skills with buffer presets"
                        (let* ((orig-buf (get-buffer-create "test-ert-sync-ctx-orig"))
                               (mock-ctx (macher--make-context :contents nil))
                               (synced-ctx nil)
                               (init-skills-ctx nil))
                          (unwind-protect
                              (progn
                                (cl-letf (((symbol-function 'macher-agent--auto-sync-context)
                                           (lambda (ctx) (setq synced-ctx ctx)))
                                          ((symbol-function 'macher-agent-initialize-skills)
                                           (lambda (ctx) (setq init-skills-ctx ctx))))
                                  (with-current-buffer orig-buf
                                    (setq-local gptel-directives '((custom-preset . "Custom directive text")))
                                    (setq-local gptel-system-prompt "Custom directive text")
                                    (setq-local macher-agent-presets nil))
                                  (let ((res (macher-agent--transformer-sync-context mock-ctx orig-buf)))
                                    (expect res :to-be mock-ctx)
                                    (expect synced-ctx :to-be mock-ctx)
                                    (expect init-skills-ctx :to-be mock-ctx)
                                    (with-current-buffer orig-buf
                                      (expect macher-agent-presets :to-equal '(custom-preset))))))
                            (kill-buffer orig-buf))))

                    (it "returns nil when context is nil"
                        (let ((orig-buf (get-buffer-create "test-ert-sync-ctx-nil")))
                          (unwind-protect
                              (progn
                                (with-current-buffer orig-buf
                                  (setq-local gptel-directives nil)
                                  (setq-local macher-agent-presets nil))
                                (let ((res (macher-agent--transformer-sync-context nil orig-buf)))
                                  (expect res :to-be nil)))
                            (kill-buffer orig-buf)))))

          (describe "macher-agent-sync-prompt-transformer"
                    (it "resolves context and syncs"
                        (let* ((orig-buf (get-buffer-create "test-ert-sync-prompt-orig"))
                               (temp-buf (get-buffer-create "test-ert-sync-prompt-temp"))
                               (mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :buffer orig-buf :macher-agent-context mock-ctx)))
                               (passed-ctx nil)
                               (passed-buf nil))
                          (unwind-protect
                              (progn
                                (cl-letf (((symbol-function 'macher-agent--transformer-sync-context)
                                           (lambda (ctx buf)
                                             (setq passed-ctx ctx)
                                             (setq passed-buf buf)
                                             ctx)))
                                  (with-current-buffer temp-buf
                                    (macher-agent-sync-prompt-transformer nil fsm))
                                  (expect passed-ctx :to-be mock-ctx)
                                  (expect passed-buf :to-be orig-buf)))
                            (kill-buffer orig-buf)
                            (kill-buffer temp-buf)))))

          (describe "macher-agent completion and patch triggering"
                    (it "verifies macher-agent--get-buffer-persistent-context is deleted"
                        (expect (fboundp 'macher-agent--get-buffer-persistent-context) :to-be nil))

                    (it "processes completed FSM buffer with injected context"
                        (let* ((target-buf (get-buffer-create "test-ert-completed-fsm-buf"))
                               (mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :buffer target-buf)))
                               (called-action nil)
                               (called-ctx nil)
                               (called-fsm nil))
                          (unwind-protect
                              (with-current-buffer target-buf
                                (setq-local macher-agent--pending-instructions-queue '("pending-1"))
                                (let ((macher-process-request-function
                                       (lambda (action ctx f)
                                         (setq called-action action
                                               called-ctx ctx
                                               called-fsm f))))
                                  (macher-agent--process-completed-fsm-buffer mock-ctx target-buf fsm)
                                  (expect called-action :to-be 'complete)
                                  (expect called-ctx :to-be mock-ctx)
                                  (expect called-fsm :to-be fsm)
                                  (expect macher-agent--pending-instructions-queue :to-be nil))))
                            (kill-buffer target-buf)))

                    (it "resolves context and forwards on trigger patch on complete"
                        (let* ((target-buf (get-buffer-create "test-ert-trigger-patch-buf"))
                               (mock-ctx (macher--make-context :contents nil))
                               (fsm (gptel-make-fsm :info (list :buffer target-buf :macher-agent-context mock-ctx)
                                                    :state 'DONE))
                               (forwarded-ctx nil)
                               (forwarded-buf nil)
                               (forwarded-fsm nil))
                          (unwind-protect
                              (cl-letf (((symbol-function 'macher-agent--process-completed-fsm-buffer)
                                         (lambda (ctx buf f)
                                           (setq forwarded-ctx ctx
                                                 forwarded-buf buf
                                                 forwarded-fsm f))))
                                (macher-agent--trigger-patch-on-complete fsm)
                                (expect forwarded-ctx :to-be mock-ctx)
                                (expect forwarded-buf :to-be target-buf)
                                (expect forwarded-fsm :to-be fsm))
                            (kill-buffer target-buf))))))

(provide 'macher-agent-core-test)
;;; macher-agent-core-test.el ends here

