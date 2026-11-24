;;; test-vfs-sandbox.el --- Non-blocking VFS sandbox pipeline and workspace tools -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Specification 4: Non-blocking VFS sandbox pipeline and workspace tools
;; SBX-01, SBX-02, SBX-03, SBX-04, SBX-05, SBX-06, SBX-07, SBX-08, SBX-09

;;; Code:

(unless (featurep 'macher-agent-vfs-test-setup)
  (load (expand-file-name "macher-agent-vfs-test-setup.el"
                          (file-name-directory (or load-file-name buffer-file-name)))
        nil t))

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-helpers)
(require 'macher-agent-sandbox)
(require 'macher-agent-vfs)

(describe "4. Non-blocking VFS sandbox pipeline and workspace tools"

  (before-each
    (macher-agent-test-reset-env)
    (when (hash-table-p macher-agent-vfs-lock-table)
      (clrhash macher-agent-vfs-lock-table))
    (when (hash-table-p macher-agent-vfs-lock-queues)
      (clrhash macher-agent-vfs-lock-queues))
    (macher-agent-sandbox-install)
    (macher-agent-vfs-install))

  ;; --------------------------------------------------------------------------
  ;; SBX-01
  ;; --------------------------------------------------------------------------
  (it "SBX-01: searches workspace asynchronously returning VFS overlay match and zero deleted matches"
    (macher-agent-test-with-git-workspace (root ctx owner-buf
                                                '(("mod.txt" . "original disk line\n")
                                                  ("del.txt" . "DELETED_TOKEN_ABC\n")))
      ;; Stage modified file with uncommitted VFS token
      (macher-agent-vfs-update-context-file ctx "mod.txt" "UNIQUE_VFS_TOKEN_XYZ\n")
      ;; Stage file and then delete it in VFS
      (macher-agent-vfs-update-context-file ctx "del.txt" "DELETED_TOKEN_ABC\n")
      (macher-agent-vfs-update-context-file ctx "del.txt" nil)

      (let* ((ptc-fn (get 'macher-agent-search-in-workspace-tool 'ptc-function))
             (thunk (funcall ptc-fn "UNIQUE_VFS_TOKEN_XYZ" nil ctx))
             (search-result nil))
        ;; Tool returns immediately without blocking (returns continuation thunk)
        (expect (functionp thunk) :to-be t)
        ;; Invoke continuation with callback
        (funcall thunk (lambda (res) (setq search-result res)))

        ;; Await asynchronous process completion barrier
        (macher-agent-test-wait-until (lambda () search-result) 5.0
                                      "Timed out waiting for search_in_workspace result")

        ;; Callback receives the match from uncommitted VFS overlay
        (expect search-result :to-match "UNIQUE_VFS_TOKEN_XYZ")
        (expect search-result :to-match "mod.txt"))

      ;; Search for deleted file token: zero matches
      (let* ((ptc-fn (get 'macher-agent-search-in-workspace-tool 'ptc-function))
             (thunk (funcall ptc-fn "DELETED_TOKEN_ABC" nil ctx))
             (del-result nil))
        (funcall thunk (lambda (res) (setq del-result res)))
        (macher-agent-test-wait-until (lambda () del-result) 5.0)
        (expect del-result :to-match "No matches found"))))

  ;; --------------------------------------------------------------------------
  ;; SBX-02
  ;; --------------------------------------------------------------------------
  (it "SBX-02: runs verification and cargo_test tools with passing and failing commands against VFS code"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("src/main.rs" . "fn main() {}\n")))
      ;; Stage uncommitted VFS code change
      (macher-agent-vfs-update-context-file ctx "src/main.rs" "fn main() { println!(\"VFS\"); }\n")

      ;; Define verification tool helper executing inside strict VFS pipeline
      (cl-flet ((run-cmd-in-sbx (cmd callback)
                  (macher-agent-vfs-run-in-sandbox
                   ctx cmd callback
                   (lambda (code output)
                     (if (= code 0)
                         (format "SUCCESS: %s" (string-trim output))
                       (format "ERROR: Verification failed (exit %d): %s"
                               code (string-trim output)))))))

        ;; 1. Passing verification command: resolves with success prefix and output
        (let ((pass-result nil))
          (run-cmd-in-sbx "grep -q 'println!(\"VFS\")' src/main.rs && echo 'All tests passed'"
                          (lambda (res) (setq pass-result res)))
          (macher-agent-test-wait-until (lambda () pass-result) 5.0)
          (expect pass-result :to-match "^SUCCESS: All tests passed"))

        ;; 2. Failing verification command: resolves with error prefix and captured output
        (let ((fail-result nil))
          (run-cmd-in-sbx "echo 'Syntax error on line 42' >&2 && exit 1"
                          (lambda (res) (setq fail-result res)))
          (macher-agent-test-wait-until (lambda () fail-result) 5.0)
          (expect fail-result :to-match "^ERROR: Verification failed")
          (expect fail-result :to-match "Syntax error on line 42")))))

  ;; --------------------------------------------------------------------------
  ;; SBX-03
  ;; --------------------------------------------------------------------------
  (it "SBX-03: cleans up temporary sandbox directory on both success and error"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      ;; Case 1: Inner process succeeds
      (let ((captured-sbx-success nil)
            (success-completed nil))
        (macher-agent-vfs-with-strict-pipeline ctx (lambda (_res) (setq success-completed t))
          (setq captured-sbx-success default-directory)
          (funcall done "ok"))
        (macher-agent-test-wait-until (lambda () success-completed) 5.0)
        ;; Poll barrier until rm cleanup completes
        (macher-agent-test-wait-until (lambda () (not (file-exists-p captured-sbx-success))) 5.0
                                      "Sandbox directory was not cleaned up after success")
        (expect (file-exists-p captured-sbx-success) :to-be nil))

      ;; Case 2: Inner process signals an error
      (let ((captured-sbx-error nil)
            (error-completed nil))
        (macher-agent-vfs-with-strict-pipeline ctx (lambda (_res) (setq error-completed t))
          (setq captured-sbx-error default-directory)
          (error "Simulated error in sandbox body"))
        (macher-agent-test-wait-until (lambda () error-completed) 5.0)
        (macher-agent-test-wait-until (lambda () (not (file-exists-p captured-sbx-error))) 5.0
                                      "Sandbox directory was not cleaned up after error")
        (expect (file-exists-p captured-sbx-error) :to-be nil))))

  ;; --------------------------------------------------------------------------
  ;; SBX-04
  ;; --------------------------------------------------------------------------
  (it "SBX-04: rejects non-git context, resolves with git-backed error, and removes sandbox directory"
    (let* ((pre-sbx (directory-files temporary-file-directory t "^macher-sandbox-"))
           (non-git-dir (make-temp-file "macher-non-git-sbx-" t))
           (canonical (file-name-as-directory (file-truename non-git-dir)))
           (ctx (macher-agent-make-context :project-root canonical))
           (captured-msg nil))
      (unwind-protect
          (progn
            (macher-agent-vfs-with-strict-pipeline ctx (lambda (res) (setq captured-msg res))
              (funcall done "unexpected success"))
            (macher-agent-test-wait-until (lambda () captured-msg) 5.0)
            ;; Error states git-backed workspace is required
            (expect captured-msg :to-match "requires a git-backed workspace")
            ;; Verify no dangling macher-sandbox-* directory exists for this operation
            (let* ((post-sbx (directory-files temporary-file-directory t "^macher-sandbox-"))
                   (leaked (cl-set-difference post-sbx pre-sbx :test #'equal)))
              (dolist (d leaked)
                (ignore-errors (delete-directory d t)))
              (expect leaked :to-be nil)))
        (ignore-errors (delete-directory non-git-dir t)))))

  ;; --------------------------------------------------------------------------
  ;; SBX-05
  ;; --------------------------------------------------------------------------
  (it "SBX-05: serialises concurrent lock acquisitions and triggers queued callbacks on release"
    (let* ((resource "src/database.rs")
           (task-1 "task-worker-alpha")
           (task-2 "task-worker-beta")
           (t1-acquired nil)
           (t2-acquired nil))

      ;; 1. Task 1 sends ACQUIRE_LOCK
      (macher-agent-vfs-a2a-callback
       (list :type 'ACQUIRE_LOCK
             :task-id task-1
             :resource_path resource
             :callback (lambda (_msg) (setq t1-acquired t))))
      ;; First task acquires lock immediately
      (expect t1-acquired :to-be t)

      ;; 2. Task 2 sends ACQUIRE_LOCK concurrently for same path
      (macher-agent-vfs-a2a-callback
       (list :type 'ACQUIRE_LOCK
             :task-id task-2
             :resource_path resource
             :callback (lambda (_msg) (setq t2-acquired t))))
      ;; Second task's callback is queued, not fired immediately
      (expect t2-acquired :to-be nil)
      (let ((queue (gethash resource macher-agent-vfs-lock-queues)))
        (expect (length queue) :to-equal 1)
        (expect (caar queue) :to-equal task-2))

      ;; 3. Task 1 sends RELEASE_LOCK
      (macher-agent-vfs-a2a-callback
       (list :type 'RELEASE_LOCK
             :task-id task-1
             :resource_path resource))

      ;; Task 2's callback fires automatically
      (expect t2-acquired :to-be t)
      ;; Clean up Task 2 lock
      (macher-agent-vfs-release-lock resource task-2)))

  ;; --------------------------------------------------------------------------
  ;; SBX-06: Output truncation helper
  ;; --------------------------------------------------------------------------
  (it "SBX-06: truncates strings exceeding limits preserving head and tail"
    ;; Unmodified when within limits
    (expect (macher-agent-truncate-string "hello world" 100)
            :to-equal "hello world")
    (expect (macher-agent-truncate-string "short" 0)
            :to-equal "short")
    ;; Truncation preserving head and tail
    (let* ((text "AAAAABBBBBCCCCCDDDDD")
           (truncated (macher-agent-truncate-string text 10)))
      (expect (string-prefix-p "AAAAA" truncated) :to-be t)
      (expect (string-suffix-p "DDDDD" truncated) :to-be t)
      (expect truncated :to-match "\\[truncated 10 chars\\]"))
    ;; Default limit from defcustom
    (let ((long-str (make-string (+ macher-agent-max-tool-output-chars 100) ?x)))
      (expect (macher-agent-truncate-string long-str)
              :to-match "\\[truncated 100 chars\\]")))

  ;; --------------------------------------------------------------------------
  ;; SBX-07: Asynchronous command execution in VFS sandbox
  ;; --------------------------------------------------------------------------
  (it "SBX-07: executes commands asynchronously via macher-agent-vfs-run-in-sandbox with format-fn and cleanup"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("test.txt" . "hello sandbox\n")))
      ;; Modify file in VFS
      (macher-agent-vfs-update-context-file ctx "test.txt" "hello modified vfs\n")

      ;; 1. Run command with custom format-fn
      (let ((res-custom nil))
        (macher-agent-vfs-run-in-sandbox
         ctx (list "cat" "test.txt")
         (lambda (res) (setq res-custom res))
         (lambda (code out) (format "STATUS=%d:%s" code (string-trim out))))
        (macher-agent-test-wait-until (lambda () res-custom) 5.0)
        (expect res-custom :to-equal "STATUS=0:hello modified vfs"))

      ;; 2. Run shell command string with default formatting on failure
      (let ((res-fail nil))
        (macher-agent-vfs-run-in-sandbox
         ctx "echo 'sandbox error' >&2 && exit 7"
         (lambda (res) (setq res-fail res)))
        (macher-agent-test-wait-until (lambda () res-fail) 5.0)
        (expect res-fail :to-match "^ERROR (exit 7):")
        (expect res-fail :to-match "sandbox error"))

      ;; Verify no leaking macher-sandbox-cmd buffers
      (expect (cl-remove-if-not
               (lambda (b) (string-match-p "\\*macher-sandbox-cmd\\*" (buffer-name b)))
               (buffer-list))
              :to-be nil)))

  ;; --------------------------------------------------------------------------
  ;; SBX-08: Flexible pipeline invocation signatures
  ;; --------------------------------------------------------------------------
  (it "SBX-08: supports 2-argument pipeline invocation and 1-argument/2-argument body-fn"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("code.el" . "(message \"vfs\")\n")))
      ;; Modify in VFS
      (macher-agent-vfs-update-context-file ctx "code.el" "(message \"vfs-modified\")\n")

      ;; 1. macher-agent-vfs-call-with-strict-pipeline with 2 arguments (resolve-fn body-fn)
      ;; and 1-argument body-fn (done)
      (let ((res-2arg-1fn nil))
        (with-current-buffer owner-buf
          (macher-agent-vfs-call-with-strict-pipeline
           (lambda (res) (setq res-2arg-1fn res))
           (lambda (done)
             (funcall done "success-2arg-1fn"))))
        (macher-agent-test-wait-until (lambda () res-2arg-1fn) 5.0)
        (expect res-2arg-1fn :to-equal "success-2arg-1fn"))

      ;; 2. macher-agent-vfs-call-with-strict-pipeline with 2 arguments (resolve-fn body-fn)
      ;; and 2-argument body-fn (sandbox-dir done)
      (let ((res-2arg-2fn nil)
            (captured-dir nil))
        (with-current-buffer owner-buf
          (macher-agent-vfs-call-with-strict-pipeline
           (lambda (res) (setq res-2arg-2fn res))
           (lambda (sbx done)
             (setq captured-dir sbx)
             (funcall done (format "DIR-OK:%s" (file-directory-p sbx))))))
        (macher-agent-test-wait-until (lambda () res-2arg-2fn) 5.0)
        (expect res-2arg-2fn :to-equal "DIR-OK:t")
        (macher-agent-test-wait-until (lambda () (not (file-exists-p captured-dir))) 5.0)
        (expect (file-exists-p captured-dir) :to-be nil))

      ;; 3. macher-agent-vfs-run-in-sandbox without ctx (2 arguments: command resolve-fn)
      (let ((res-run-noctx nil))
        (with-current-buffer owner-buf
          (macher-agent-vfs-run-in-sandbox
           (list "cat" "code.el")
           (lambda (res) (setq res-run-noctx res))))
        (macher-agent-test-wait-until (lambda () res-run-noctx) 5.0)
        (expect res-run-noctx :to-equal "(message \"vfs-modified\")\n"))))

  ;; --------------------------------------------------------------------------
  ;; SBX-09: Command timeout termination and non-empty error reporting
  ;; --------------------------------------------------------------------------
  (it "SBX-09: terminates hung sandbox commands on timeout and reports non-empty diagnostics"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("ok.txt" . "ok\n")))
      (let ((macher-agent-sandbox-command-timeout 0.15)
            (res-timeout nil))
        (macher-agent-vfs-run-in-sandbox
         ctx "sleep 10"
         (lambda (res) (setq res-timeout res))
         (lambda (_code out) (string-trim out)))
        (macher-agent-test-wait-until (lambda () res-timeout) 5.0)
        (expect res-timeout :to-match "^ERROR: Sandbox command timed out")))))

(provide 'test-vfs-sandbox)
;;; test-vfs-sandbox.el ends here
