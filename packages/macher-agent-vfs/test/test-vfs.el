;;; test-vfs.el --- Virtual file system (VFS) and optimistic concurrency -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Specification 3: Virtual file system (VFS) and optimistic concurrency
;; VFS-01..VFS-21, VFS-TOOLS-01, VFS-LIFE-01, VFS-BOUND-01

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

(describe "3. Virtual file system (VFS) and optimistic concurrency"

  (before-each
    (macher-agent-test-reset-env)
    (when (hash-table-p macher-agent-vfs-lock-table)
      (clrhash macher-agent-vfs-lock-table))
    (when (hash-table-p macher-agent-vfs-lock-queues)
      (clrhash macher-agent-vfs-lock-queues))
    (setq macher-agent-vfs-pause-auto-sync nil)
    (macher-agent-sandbox-install)
    (macher-agent-vfs-install))

  ;; --------------------------------------------------------------------------
  ;; VFS-01
  ;; --------------------------------------------------------------------------
  (it "VFS-01: modifies tracked file and creates new file in VFS without touching physical disk"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("README.md" . "# Original README\n")))
      (let ((initial-readme (with-temp-buffer
                              (insert-file-contents (expand-file-name "README.md" root))
                              (buffer-string))))
        ;; Modify tracked file in VFS
        (macher-agent-vfs-update-context-file ctx "README.md" "# Modified in VFS\n")
        ;; Create new file in VFS
        (macher-agent-vfs-update-context-file ctx "new-doc.md" "# Brand New Document\n")

        ;; Subsequent reads return updated VFS content
        (expect (macher-agent-vfs-read-context-file ctx "README.md")
                :to-equal "# Modified in VFS\n")
        (expect (macher-agent-vfs-read-context-file ctx "new-doc.md")
                :to-equal "# Brand New Document\n")

        ;; Dirty flag is set
        (expect (macher-agent-vfs-context-dirty-p ctx) :to-be t)

        ;; Physical files on disk remain untouched
        (expect (with-temp-buffer
                  (insert-file-contents (expand-file-name "README.md" root))
                  (buffer-string))
                :to-equal initial-readme)
        (expect (file-exists-p (expand-file-name "new-doc.md" root)) :to-be nil))))

  ;; --------------------------------------------------------------------------
  ;; VFS-02
  ;; --------------------------------------------------------------------------
  (it "VFS-02: creates and edits in-memory buffer in VFS and applies only on macher-agent-vfs-apply-virtual-buffers"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (let ((vbuf (get-buffer-create "*in-memory-scratch*")))
        (unwind-protect
            (progn
              (with-current-buffer vbuf
                (erase-buffer)
                (insert "initial buffer text"))
              ;; Edit via workspace buffer tool
              (funcall (get 'macher-agent-write-buffer-in-workspace-tool 'ptc-function)
                       "*in-memory-scratch*" "staged virtual content" ctx)

              ;; Stored as virtual buffer entry in :vfs-buffers (not :contents)
              (let ((entry (gethash "*in-memory-scratch*" (macher-agent-vfs-buffers ctx))))
                (expect (macher-agent-vfs-entry-p entry) :to-be t)
                (expect (macher-agent-vfs-entry-curr entry) :to-equal "staged virtual content"))
              (expect (cl-find "*in-memory-scratch*" (macher-agent-vfs-context-contents ctx)
                               :key #'macher-agent-vfs-entry-path :test #'equal)
                      :to-be nil)

              ;; Live buffer has NOT mutated yet
              (expect (with-current-buffer vbuf (buffer-string))
                      :to-equal "initial buffer text")

              ;; Render into live buffer on macher-agent-vfs-apply-virtual-buffers
              (macher-agent-vfs-apply-virtual-buffers ctx)
              (expect (with-current-buffer vbuf (buffer-string))
                      :to-equal "staged virtual content"))
          (when (buffer-live-p vbuf)
            (kill-buffer vbuf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-03
  ;; --------------------------------------------------------------------------
  (it "VFS-03: deletes tracked VFS entries with nil content, purging virtual buffers and marking physical deletions"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("tracked.el" . "(defvar x 1)")))
      (let ((vbuf (get-buffer-create "*temp-vbuf*")))
        (unwind-protect
            (progn
              ;; Stage virtual buffer
              (funcall (get 'macher-agent-write-buffer-in-workspace-tool 'ptc-function)
                       "*temp-vbuf*" "vbuf content" ctx)
              ;; Stage physical file
              (macher-agent-vfs-update-context-file ctx "tracked.el" "(defvar x 2)")

              ;; 1. Delete virtual buffer entry by passing nil content
              (macher-agent-vfs-update-context-file ctx "*temp-vbuf*" nil)
              (expect (gethash "*temp-vbuf*" (macher-agent-vfs-buffers ctx)) :to-be nil)

              ;; 2. Delete physical file entry by passing nil content
              (macher-agent-vfs-update-context-file ctx "tracked.el" nil)
              (let ((p-entry (cl-find "tracked.el" (macher-agent-vfs-context-contents ctx)
                                      :key #'macher-agent-vfs-entry-path :test #'equal)))
                (expect p-entry :not :to-be nil)
                ;; curr content is set to nil so diff represents file deletion
                (expect (macher-agent-vfs-entry-curr p-entry) :to-be nil)
                (expect (macher-agent-vfs-entry-modified-p p-entry) :to-be t)))
          (when (buffer-live-p vbuf)
            (kill-buffer vbuf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-04
  ;; --------------------------------------------------------------------------
  (it "VFS-04: detects disk modifications, synchronises baseline orig and curr, and clears dirty flag"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("config.json" . "{\"version\": 1}")))
      (let ((file-path (expand-file-name "config.json" root))
            (message-captured nil))
        ;; Stage agent edit in VFS
        (macher-agent-vfs-update-context-file ctx "config.json" "{\"version\": 2, \"agent\": true}")
        (expect (macher-agent-vfs-context-dirty-p ctx) :to-be t)

        ;; Modify physical file on disk outside agent
        (with-temp-file file-path
          (insert "{\"version\": 3, \"external\": true}"))

        ;; Trigger auto-sync and capture replacement message
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq message-captured (apply #'format fmt args)))))
          (macher-agent-vfs-auto-sync-context ctx))

        ;; VFS replaces both orig and curr with new disk content and clears pending-patch
        (let ((entry (cl-find "config.json" (macher-agent-vfs-context-contents ctx)
                              :key #'macher-agent-vfs-entry-path :test #'equal)))
          (expect (macher-agent-vfs-entry-orig entry) :to-equal "{\"version\": 3, \"external\": true}")
          (expect (macher-agent-vfs-entry-curr entry) :to-equal "{\"version\": 3, \"external\": true}")
          (expect (macher-agent-vfs-entry-pending-patch entry) :to-be nil)
          (expect (macher-agent-vfs-entry-modified-p entry) :to-be nil))

        ;; Dirty flag is cleared when no other files modified
        (expect (macher-agent-vfs-context-dirty-p ctx) :to-be nil)
        ;; Replacement message emitted
        (expect message-captured :to-match "Replaced entry 'config.json' due to external edits"))))

  ;; --------------------------------------------------------------------------
  ;; VFS-05
  ;; --------------------------------------------------------------------------
  (it "VFS-05: anchors physical files to disk despite unsaved buffers and applies generated patches cleanly"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("module.el" . ";; Disk Version\n(defun alpha () 1)\n")))
      (let* ((file-path (expand-file-name "module.el" root))
             (live-buf (find-file-noselect file-path))
             (messages nil)
             (patch-buf nil))
        (unwind-protect
            (progn
              ;; Make unsaved edit in live buffer before agent reads/edits
              (with-current-buffer live-buf
                (goto-char (point-max))
                (insert "(defun live-unsaved-fn () t)\n")
                (expect (buffer-modified-p) :to-be t))

              ;; VFS reads physical disk rather than unsaved buffer edits
              (let ((vfs-read (macher-agent-vfs-read-context-file ctx "module.el")))
                (expect vfs-read :to-equal ";; Disk Version\n(defun alpha () 1)\n")
                (expect (string-match-p "live-unsaved-fn" vfs-read) :to-be nil))

              ;; Stage agent edit in VFS
              (macher-agent-vfs-update-context-file
               ctx "module.el" ";; Disk Version\n(defun alpha () 2)\n")

              ;; User edits live buffer further without saving midway
              (with-current-buffer live-buf
                (goto-char (point-max))
                (insert "(defun another-unsaved-fn () t)\n"))

              ;; Auto-sync must NOT overwrite VFS from the unsaved buffer
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) messages))))
                (macher-agent-vfs-auto-sync-context ctx))
              (expect (cl-some (lambda (m) (string-match-p "Replaced entry" m)) messages)
                      :to-be nil)
              (let ((entry (cl-find "module.el" (macher-agent-vfs-context-contents ctx)
                                    :key #'macher-agent-vfs-entry-path :test #'equal)))
                (expect (macher-agent-vfs-entry-orig entry)
                        :to-equal ";; Disk Version\n(defun alpha () 1)\n")
                (expect (macher-agent-vfs-entry-curr entry)
                        :to-equal ";; Disk Version\n(defun alpha () 2)\n"))

              ;; Build real patch via macher--build-patch and apply cleanly to disk
              (with-current-buffer owner-buf
                (setq-local macher-agent-suppress-patch nil))
              (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
                (macher-agent-vfs-build-patch-from-hook ctx))
              (setq patch-buf
                    (cl-find-if (lambda (b)
                                  (string-prefix-p "*macher-diff-patch:" (buffer-name b)))
                                (buffer-list)))
              (expect patch-buf :not :to-be nil)
              (with-current-buffer patch-buf
                (expect (string-suffix-p "/" default-directory) :to-be t)
                (macher-agent-macher-apply-patch))

              ;; Physical disk reflects agent edit cleanly
              (expect (with-temp-buffer
                        (insert-file-contents file-path)
                        (buffer-string))
                      :to-equal ";; Disk Version\n(defun alpha () 2)\n")
              (expect (get-buffer "*macher-patch-out*") :to-be nil)

              ;; Subsequent physical disk change triggers external-edit sync
              (macher-agent-vfs-auto-sync-context ctx)
              (macher-agent-vfs-update-context-file
               ctx "module.el" ";; Disk Version\n(defun alpha () 3)\n")
              (with-temp-file file-path
                (insert ";; External Disk Save\n(defun alpha () 99)\n"))
              (setq messages nil)
              (cl-letf (((symbol-function 'message)
                         (lambda (fmt &rest args)
                           (push (apply #'format fmt args) messages))))
                (macher-agent-vfs-auto-sync-context ctx))
              (expect (cl-some (lambda (m) (string-match-p "Replaced entry 'module.el' due to external edits" m))
                               messages)
                      :to-be-truthy)
              (expect (macher-agent-vfs-read-context-file ctx "module.el")
                      :to-equal ";; External Disk Save\n(defun alpha () 99)\n"))
          (when (and patch-buf (buffer-live-p patch-buf))
            (kill-buffer patch-buf))
          (dolist (b (buffer-list))
            (when (string-match-p "\\*macher-.*-patch:" (buffer-name b))
              (kill-buffer b)))
          (when (buffer-live-p live-buf)
            (with-current-buffer live-buf
              (set-buffer-modified-p nil))
            (kill-buffer live-buf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-06
  ;; --------------------------------------------------------------------------
  (it "VFS-06: blocks path traversal via relative segments or symlinks outside workspace root"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      ;; Directory climbing via ../
      (expect (macher-agent-vfs-resolve-safe-path "../outside.txt" root)
              :to-throw 'error)
      (expect (macher-agent-vfs-resolve-safe-path "../../../../etc/passwd" root)
              :to-throw 'error)
      ;; Symlink resolving outside root
      (let* ((outside-dir (make-temp-file "macher-outside-" t))
             (outside-target (expand-file-name "secret.txt" outside-dir))
             (symlink-path (expand-file-name "escape_link" root)))
        (unwind-protect
            (progn
              (with-temp-file outside-target (insert "secret data"))
              (make-symbolic-link outside-dir symlink-path t)
              (expect (macher-agent-vfs-resolve-safe-path "escape_link/secret.txt" root)
                      :to-throw 'error))
          (ignore-errors (delete-file symlink-path))
          (ignore-errors (delete-directory outside-dir t))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-07
  ;; --------------------------------------------------------------------------
  (it "VFS-07: merges sub-agent VFS entries into parent context with zero sub-agent patch buffers"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
      (let* ((child-buf (macher-agent-add-subagent "*test-vfs-07-child*" '("default") parent-buf nil parent-ctx))
             (child-ctx (with-current-buffer child-buf macher-agent-persistent-context)))
        (unwind-protect
            (progn
              (macher-agent-register-task-callback
               "task-vfs-07"
               (lambda (msg)
                 (when (macher-agent-transit-payload-p msg)
                   (setf (macher-agent-transit-payload-target-context msg) parent-ctx))
                 (when (fboundp 'macher-agent-run-pipeline)
                   (macher-agent-run-pipeline 'payload-merge msg))))
              ;; Sub-agent operates with suppressed patches
              (with-current-buffer child-buf
                (setq-local macher-agent-suppress-patch t)
                (macher-agent-push-routing "task-vfs-07" (buffer-name parent-buf) t)
                ;; Modify two files
                (macher-agent-vfs-update-context-file child-ctx "a.txt" "content A")
                (macher-agent-vfs-update-context-file child-ctx "b.txt" "content B")
                ;; Submit task result
                (macher-agent-submit-task-result "Completed subtask" nil child-ctx)
                ;; Simulate FSM reaching DONE state
                (let ((child-fsm (gptel-make-fsm :info (list :buffer child-buf :macher-agent-context child-ctx)
                                                 :state 'DONE)))
                  (macher-agent-gptel-trigger-flush child-fsm)))

              ;; Sub-agent's VFS entries merge into parent orchestrator's context
              (expect (macher-agent-vfs-read-context-file parent-ctx "a.txt") :to-equal "content A")
              (expect (macher-agent-vfs-read-context-file parent-ctx "b.txt") :to-equal "content B")

              ;; Zero patch buffers created or displayed for child
              (let ((patch-bufs (cl-remove-if-not
                                 (lambda (b)
                                   (string-match-p "\\*macher-.*-patch:.*test-vfs-07-child.*\\*" (buffer-name b)))
                                 (buffer-list))))
                (expect (length patch-bufs) :to-equal 0)))
          (when (buffer-live-p child-buf)
            (kill-buffer child-buf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-08
  ;; --------------------------------------------------------------------------
  (it "VFS-08: generates exactly one diff patch and one buffer patch in split windows for root turn"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("app.el" . "(defvar app 1)")))
      (let ((vbuf (get-buffer-create "*vfs-08-buf*")))
        (unwind-protect
            (with-current-buffer owner-buf
              (setq-local macher-agent-suppress-patch nil)
              ;; Modify physical file
              (macher-agent-vfs-update-context-file ctx "app.el" "(defvar app 2)")
              ;; Modify virtual buffer
              (funcall (get 'macher-agent-write-buffer-in-workspace-tool 'ptc-function)
                       "*vfs-08-buf*" "new virtual app" ctx)

              ;; Stub upstream patch builder to produce distinct mock patch buffers
              (cl-letf (((symbol-function 'macher-agent-macher-build-patch)
                         (lambda (_ctx _prompt files)
                           (let* ((is-virtual (cl-some (lambda (e) (string-prefix-p "*" (macher-agent-vfs-entry-path e))) files))
                                  (raw-buf (generate-new-buffer (if is-virtual "*macher-patch-buf*" "*macher-patch*"))))
                             raw-buf))))
                (macher-agent-vfs-build-patch-from-hook ctx))

              ;; Exactly one diff patch and one buffer patch buffer exist
              (let ((diff-patches (cl-remove-if-not
                                   (lambda (b) (string-prefix-p "*macher-diff-patch:" (buffer-name b)))
                                   (buffer-list)))
                    (buf-patches (cl-remove-if-not
                                  (lambda (b) (string-prefix-p "*macher-buffer-patch:" (buffer-name b)))
                                  (buffer-list))))
                (expect (length diff-patches) :to-equal 1)
                (expect (length buf-patches) :to-equal 1)
                ;; Raw upstream *macher-patch* buffers renamed in place, not duplicated
                (expect (get-buffer "*macher-patch*") :to-be nil)
                ;; Windows are split
                (expect (> (length (window-list)) 1) :to-be t)))
          (when (buffer-live-p vbuf) (kill-buffer vbuf))
          (dolist (b (buffer-list))
            (when (string-match-p "\\*macher-.*-patch:" (buffer-name b))
              (kill-buffer b)))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-09
  ;; --------------------------------------------------------------------------
  (it "VFS-09: applies generated diff from patch buffer to physical files in git workspace and cleans output buffer"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("file.txt" . "alpha\nbeta\n")))
      (let* ((patch-content "--- a/file.txt\n+++ b/file.txt\n@@ -1,2 +1,2 @@\n alpha\n-beta\n+BETA_MODIFIED\n")
             (patch-buf (generate-new-buffer "*macher-diff-patch:test*")))
        (unwind-protect
            (with-current-buffer patch-buf
              (diff-mode)
              (setq-local default-directory root)
              (setq-local macher-agent-persistent-context ctx)
              (insert patch-content)
              ;; Apply patch
              (macher-agent-macher-apply-patch)
              ;; Physical file on disk is modified
              (let ((file-content (with-temp-buffer
                                    (insert-file-contents (expand-file-name "file.txt" root))
                                    (buffer-string))))
                (expect file-content :to-equal "alpha\nBETA_MODIFIED\n"))
              ;; Output buffer *macher-patch-out* is cleaned up on success
              (expect (get-buffer "*macher-patch-out*") :to-be nil))
          (when (buffer-live-p patch-buf)
            (kill-buffer patch-buf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-10
  ;; --------------------------------------------------------------------------
  (it "VFS-10: prevents sibling baseline entries from clobbering concurrent worker edits"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf '(("shared.txt" . "disk-base\n")))
      ;; Stage shared.txt in parent context
      (macher-agent-vfs-update-context-file parent-ctx "shared.txt" "base\n")
      ;; Child 1 and Child 2 cloned from parent context
      (let* ((child1-ctx (macher-agent-clone-context parent-ctx))
             (child2-ctx (macher-agent-clone-context parent-ctx)))
        ;; Initially both child entries are baseline (pending-patch is nil)
        (let ((c1-entry (cl-find "shared.txt"
                                 (macher-agent-vfs-context-contents child1-ctx)
                                 :key #'macher-agent-vfs-entry-path :test #'equal))
              (c2-entry (cl-find "shared.txt"
                                 (macher-agent-vfs-context-contents child2-ctx)
                                 :key #'macher-agent-vfs-entry-path :test #'equal)))
          (expect (macher-agent-vfs-entry-pending-patch c1-entry) :to-be nil)
          (expect (macher-agent-vfs-entry-pending-patch c2-entry) :to-be nil))

        ;; Child 1 modifies shared.txt (pending-patch becomes t)
        (macher-agent-vfs-update-context-file child1-ctx "shared.txt" "worker1-content\n")

        ;; Child 2 modifies a different file (other.txt), leaving shared.txt untouched
        (macher-agent-vfs-update-context-file child2-ctx "other.txt" "worker2-content\n")

        ;; 1. Merge Child 1 into parent
        (macher-agent-vfs-merge-contexts parent-ctx child1-ctx)
        (expect (macher-agent-vfs-read-context-file parent-ctx "shared.txt")
                :to-equal "worker1-content\n")

        ;; 2. Merge Child 2 into parent: Child 2's untouched shared.txt baseline MUST NOT
        ;; overwrite Child 1's "worker1-content", while other.txt is cleanly merged.
        (macher-agent-vfs-merge-contexts parent-ctx child2-ctx)
        (expect (macher-agent-vfs-read-context-file parent-ctx "shared.txt")
                :to-equal "worker1-content\n")
        (expect (macher-agent-vfs-read-context-file parent-ctx "other.txt")
                :to-equal "worker2-content\n"))))

  ;; --------------------------------------------------------------------------
  ;; VFS-11
  ;; --------------------------------------------------------------------------
  (it "VFS-11: ignores zero-diff updates, suppresses redundant hooks, and restores dirty state on revert"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("sample.txt" . "original content\n")))
      (let ((hook-calls 0))
        (add-hook 'macher-agent-context-mutated-hook
                  (lambda (&rest _) (setq hook-calls (1+ hook-calls))))
        (unwind-protect
            (progn
              ;; 1. Writing identical content to untracked clean file is zero-diff no-op
              (macher-agent-vfs-update-context-file ctx "sample.txt" "original content\n")
              (expect hook-calls :to-equal 0)
              (expect (macher-agent-vfs-context-dirty-p ctx) :to-be nil)

              ;; 2. Mutating content fires hook and marks dirty
              (macher-agent-vfs-update-context-file ctx "sample.txt" "modified content\n")
              (expect hook-calls :to-equal 1)
              (expect (macher-agent-vfs-context-dirty-p ctx) :to-be t)

              ;; 3. Writing the same mutated content again is zero-diff: hook not fired
              (macher-agent-vfs-update-context-file ctx "sample.txt" "modified content\n")
              (expect hook-calls :to-equal 1)

              ;; 4. Reverting back to original content removes zero-diff entry and clears dirty flag
              (macher-agent-vfs-update-context-file ctx "sample.txt" "original content\n")
              (expect hook-calls :to-equal 2)
              (expect (macher-agent-vfs-context-dirty-p ctx) :to-be nil)
              (expect (macher-agent-vfs-read-context-file ctx "sample.txt")
                      :to-equal "original content\n"))
          (setq macher-agent-context-mutated-hook nil)))))

  ;; --------------------------------------------------------------------------
  ;; VFS-12
  ;; --------------------------------------------------------------------------
  (it "VFS-12: releases actor locks and transfers to queued waiters via macher-agent-vfs-release-actor-locks"
    (let ((f1 "src/f1.rs")
          (f2 "src/f2.rs")
          (f3 "src/f3.rs")
          (t2-called nil)
          (t4-called nil))
      (unwind-protect
          (progn
            ;; Actor 1 holds locks on f1 and f2
            (macher-agent-vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-1" :resource_path f1 :callback #'ignore))
            (macher-agent-vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-1" :resource_path f2 :callback #'ignore))

            ;; Actor 2 and Actor 3 wait for f1
            (macher-agent-vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-2" :resource_path f1
                   :callback (lambda (_m) (setq t2-called t))))
            (macher-agent-vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-3" :resource_path f1 :callback #'ignore))

            ;; Actor 4 waits for f2
            (macher-agent-vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-4" :resource_path f2
                   :callback (lambda (_m) (setq t4-called t))))

            ;; Actor 5 holds f3, Actor 1 is queued on f3
            (macher-agent-vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-5" :resource_path f3 :callback #'ignore))
            (macher-agent-vfs-a2a-callback
             (list :type 'ACQUIRE_LOCK :task-id "actor-1" :resource_path f3 :callback #'ignore))

            ;; Release locks for actor-1
            (macher-agent-vfs-release-actor-locks '("actor-1"))

            ;; f1 transferred to actor-2
            (expect t2-called :to-be t)
            (expect (car (gethash f1 macher-agent-vfs-lock-table)) :to-equal "actor-2")
            ;; actor-3 still in queue for f1
            (expect (length (gethash f1 macher-agent-vfs-lock-queues)) :to-equal 1)

            ;; f2 transferred to actor-4
            (expect t4-called :to-be t)
            (expect (car (gethash f2 macher-agent-vfs-lock-table)) :to-equal "actor-4")

            ;; actor-1 removed from f3 waiting queue; actor-5 still holds f3
            (expect (car (gethash f3 macher-agent-vfs-lock-table)) :to-equal "actor-5")
            (expect (gethash f3 macher-agent-vfs-lock-queues) :to-be nil))

        ;; Cleanup
        (macher-agent-vfs-release-actor-locks '("actor-2" "actor-3" "actor-4" "actor-5")))))

  ;; --------------------------------------------------------------------------
  ;; VFS-13
  ;; --------------------------------------------------------------------------
  (it "VFS-13: stores strictly workspace-relative VFS keys and scrubs temporary macher-sandbox overlay paths"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("src/lib.el" . "(defvar lib 1)\n")))
      ;; 1. Updating via absolute path stores canonical workspace-relative key
      (macher-agent-vfs-update-context-file
       ctx (expand-file-name "src/lib.el" root) "(defvar lib 2)\n")
      (expect (macher-agent-vfs-workspace-list-files ctx) :to-equal '("src/lib.el"))
      (expect (cl-every (lambda (e) (not (file-name-absolute-p (macher-agent-vfs-entry-path e))))
                        (macher-agent-vfs-context-contents ctx))
              :to-be t)

      ;; 2. Sandbox commands emitting `pwd' or sandbox file paths never leak `macher-sandbox-'
      ;;    and mutating context from inside a sandbox strips the sandbox prefix
      (let ((cmd-out nil))
        (macher-agent-vfs-call-with-strict-pipeline
         ctx (lambda (res) (setq cmd-out res))
         (lambda (sbx done)
           ;; Simulate tool inside sandbox updating VFS with an expanded sandbox path
           (let ((sbx-file (expand-file-name "src/generated.el" sbx)))
             (with-temp-file sbx-file (insert "(provide 'generated)\n"))
             (macher-agent-vfs-update-context-file ctx sbx-file "(provide 'generated)\n"))
           ;; Simulate eshell/compiler output containing both truename and raw sandbox paths
           (funcall done (format "pwd=%s err=%s/src/lib.el:1:1" sbx (directory-file-name sbx)))))
        (macher-agent-test-wait-until (lambda () cmd-out) 5.0)
        (expect cmd-out :not :to-match "macher-sandbox-")
        (expect cmd-out :to-match "err=src/lib\\.el:1:1")
        ;; Generated file stored strictly under relative path "src/generated.el"
        (expect (member "src/generated.el" (macher-agent-vfs-workspace-list-files ctx))
                :not :to-be nil)
        (expect (cl-some (lambda (p) (string-match-p "macher-sandbox-" p))
                         (macher-agent-vfs-workspace-list-files ctx))
                :to-be nil))))

  ;; --------------------------------------------------------------------------
  ;; VFS-14
  ;; --------------------------------------------------------------------------
  (it "VFS-14: reverts or removes VFS entries deleted from proxy macher-context-contents by wrapped macher tools"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("tracked.el" . "(defvar tracked 1)\n")))
      (let* ((vbuf (get-buffer-create "*vfs-14-buf*"))
             (tool (gptel-make-tool
                    :name "test_macher_revert"
                    :description "Test macher tool that removes entries from proxy context contents"
                    :category "macher"
                    :args '((:name "target" :type string :description "Relative path or \"all\""))
                    :function
                    (lambda (proxy-ctx target)
                      (if (equal target "all")
                          (setf (macher-context-contents proxy-ctx) nil)
                        (let ((abs-target (expand-file-name target root)))
                          (setf (macher-context-contents proxy-ctx)
                                (assoc-delete-all abs-target (macher-context-contents proxy-ctx)))))
                      "reverted"))))
        (unwind-protect
            (progn
              (with-current-buffer vbuf
                (erase-buffer)
                (insert "initial buffer text"))
              (macher-agent-vfs-wrap-single-tool tool)
              ;; Stage modification to existing tracked file, a brand new file, and a virtual buffer
              (macher-agent-vfs-update-context-file ctx "tracked.el" "(defvar tracked 2)\n")
              (macher-agent-vfs-update-context-file ctx "new-file.el" "(defvar new 1)\n")
              (funcall (get 'macher-agent-write-buffer-in-workspace-tool 'ptc-function)
                       "*vfs-14-buf*" "staged virtual buf" ctx)
              (expect (macher-agent-vfs-context-dirty-p ctx) :to-be t)

              ;; 1. Wrapped tool removes only tracked.el from proxy contents
              (with-current-buffer owner-buf
                (expect (funcall (gptel-tool-function tool) "tracked.el") :to-equal "reverted"))

              ;; tracked.el is reverted and removed from :contents; read returns original disk content
              (expect (cl-find "tracked.el" (macher-agent-vfs-context-contents ctx)
                               :key #'macher-agent-vfs-entry-path :test #'equal)
                      :to-be nil)
              (expect (macher-agent-vfs-read-context-file ctx "tracked.el")
                      :to-equal "(defvar tracked 1)\n")
              ;; new-file.el and virtual buffer remain staged; dirty flag remains t
              (expect (macher-agent-vfs-read-context-file ctx "new-file.el")
                      :to-equal "(defvar new 1)\n")
              (expect (gethash "*vfs-14-buf*" (macher-agent-vfs-buffers ctx))
                      :not :to-be nil)
              (expect (macher-agent-vfs-context-dirty-p ctx) :to-be t)

              ;; 2. Clear virtual buffer, then wrapped tool clears all remaining proxy contents (returns nil)
              (macher-agent-vfs-update-context-file ctx "*vfs-14-buf*" nil)
              (expect (macher-agent-vfs-context-dirty-p ctx) :to-be t)
              (with-current-buffer owner-buf
                (expect (funcall (gptel-tool-function tool) "all") :to-equal "reverted"))

              ;; new-file.el is removed from :contents and dirty flag is cleared to nil
              (expect (macher-agent-vfs-context-contents ctx) :to-be nil)
              (expect (macher-agent-vfs-context-dirty-p ctx) :to-be nil))
          (when (buffer-live-p vbuf)
            (kill-buffer vbuf))
          (when (hash-table-p macher-agent-vfs--wrapped-tools-hash)
            (remhash tool macher-agent-vfs--wrapped-tools-hash))
          (macher-agent-test-remove-known-tool "macher" "test_macher_revert")))))

  ;; --------------------------------------------------------------------------
  ;; VFS-15
  ;; --------------------------------------------------------------------------
  (it "VFS-15: replaces upstream macher search_in_workspace across @macher/@macher-ro presets and anonymous fallbacks"
    (macher-agent-test-with-git-workspace (_root ctx owner-buf '(("hello.txt" . "disk-token\n")))
      (macher-agent-test-with-clean-gptel-registries
        (let* ((wire-1 (generate-new-buffer " *vfs15-wire-1*"))
               (wire-2 (generate-new-buffer " *vfs15-wire-2*"))
               (wire-3 (generate-new-buffer " *vfs15-wire-3*")))
          (unwind-protect
              (progn
                (macher-install)
                ;; Simulate a buffer whose local "macher" category previously stripped search_in_workspace
                (with-current-buffer owner-buf
                  (markdown-mode)
                  (gptel-mode 1)
                  (macher-agent-mode 1)
                  (macher-agent-test-remove-known-tool "macher" "search_in_workspace"))

                ;; Wrapping restores and replaces search_in_workspace in both default and local registries
                (macher-agent-vfs-wrap-macher-tools)
                (let ((def-siw (gptel-get-tool '("macher" "search_in_workspace")))
                      (loc-siw (with-current-buffer owner-buf
                                 (gptel-get-tool '("macher" "search_in_workspace")))))
                  (expect (gptel-tool-category def-siw) :to-equal "perception")
                  (expect (mapcar (lambda (a) (plist-get a :name)) (gptel-tool-args def-siw))
                          :to-equal '("regexp" "path"))
                  (expect (gptel-tool-category loc-siw) :to-equal "perception")
                  (expect (mapcar (lambda (a) (plist-get a :name)) (gptel-tool-args loc-siw))
                          :to-equal '("regexp" "path")))

                ;; 1. Inline @macher-ro resolves macher-agent search_in_workspace and VFS-wrapped read tool
                (macher-agent-vfs-update-context-file ctx "hello.txt" "vfs-staged-token\n")
                (with-current-buffer owner-buf
                  (macher-agent-test-localize-known-presets)
                  (let ((fsm-1 (gptel-make-fsm :info (list :buffer owner-buf :data wire-1))))
                    (with-current-buffer wire-1
                      (insert "### @macher-ro inspect workspace")
                      (macher-agent-sync-prompt-transformer nil fsm-1))
                    (let* ((tools (plist-get (gptel-fsm-info fsm-1) :tools))
                           (siw (cl-find "search_in_workspace" tools :key #'gptel-tool-name :test #'equal))
                           (rfiw (cl-find "read_file_in_workspace" tools :key #'gptel-tool-name :test #'equal)))
                      (expect siw :not :to-be nil)
                      (expect (gptel-tool-category siw) :to-equal "perception")
                      (expect (gptel-tool-async siw) :to-be t)
                      (expect (mapcar (lambda (a) (plist-get a :name)) (gptel-tool-args siw))
                              :to-equal '("regexp" "path"))
                      (expect (funcall (gptel-tool-function rfiw) "hello.txt")
                              :to-equal "vfs-staged-token\n")))

                  ;; 2. Anonymous fallback (when "macher" category is absent from local known tools)
                  ;; still replaces search_in_workspace and wraps remaining macher tools
                  (macher-agent-test-drop-local-tool-category "macher")
                  (let ((fsm-2 (gptel-make-fsm :info (list :buffer owner-buf :data wire-2))))
                    (with-current-buffer wire-2
                      (insert "### @macher edit workspace")
                      (macher-agent-sync-prompt-transformer nil fsm-2))
                    (let* ((tools (plist-get (gptel-fsm-info fsm-2) :tools))
                           (siw (cl-find "search_in_workspace" tools :key #'gptel-tool-name :test #'equal))
                           (efiw (cl-find "edit_file_in_workspace" tools :key #'gptel-tool-name :test #'equal)))
                      (expect (gptel-tool-category siw) :to-equal "perception")
                      (expect (mapcar (lambda (a) (plist-get a :name)) (gptel-tool-args siw))
                              :to-equal '("regexp" "path"))
                      (funcall (gptel-tool-function efiw) "hello.txt" "vfs-staged-token" "edited-via-anon")
                      (expect (macher-agent-vfs-read-context-file ctx "hello.txt")
                              :to-equal "edited-via-anon\n")))

                  ;; 3. Workspace VFS script override of search_in_workspace takes precedence in @macher-ro
                  (macher-agent-vfs-update-context-file
                   ctx "skills/scripts/search_in_workspace.el"
                   (concat "(setq macher-agent-search-in-workspace-tool\n"
                           "      (gptel-make-tool\n"
                           "       :name \"search_in_workspace\"\n"
                           "       :description \"Workspace custom search\"\n"
                           "       :category \"perception\"\n"
                           "       :args '((:name \"regexp\" :type \"string\" :description \"Pattern\"))\n"
                           "       :function (lambda (_r) \"ws-custom-search\")))\n"))
                  (let ((fsm-3 (gptel-make-fsm :info (list :buffer owner-buf :data wire-3))))
                    (with-current-buffer wire-3
                      (insert "### @macher-ro custom search")
                      (macher-agent-sync-prompt-transformer nil fsm-3))
                    (let* ((tools (plist-get (gptel-fsm-info fsm-3) :tools))
                           (siw (cl-find "search_in_workspace" tools :key #'gptel-tool-name :test #'equal)))
                      (expect (gptel-tool-description siw) :to-equal "Workspace custom search")))))
            (dolist (b (list wire-1 wire-2 wire-3))
              (when (buffer-live-p b) (kill-buffer b))))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-16
  ;; --------------------------------------------------------------------------
  (it "VFS-16: wrapped macher tools skip no-op updates on untouched staged entries while preserving revert pruning and hot skill loading"
    (macher-agent-test-with-git-workspace
        (_root ctx owner-buf
               '(("a.txt" . "orig-a\n")
                 ("b.txt" . "orig-b\n")
                 ("c.txt" . "orig-c\n")
                 ("unread.txt" . "orig-unread\n")))
      (macher-agent-test-with-clean-gptel-registries
        (let* ((wire-buf (generate-new-buffer " *vfs16-wire*"))
               (updated-paths nil)
               (real-update (symbol-function 'macher-agent-vfs-update-context-file)))
          (unwind-protect
              (progn
                (macher-install)
                (with-current-buffer owner-buf
                  (markdown-mode)
                  (gptel-mode 1)
                  (macher-agent-mode 1)
                  (macher-agent-vfs-wrap-macher-tools)
                  ;; Stage three modified files in VFS
                  (macher-agent-vfs-update-context-file ctx "a.txt" "staged-a\n")
                  (macher-agent-vfs-update-context-file ctx "b.txt" "staged-b\n")
                  (macher-agent-vfs-update-context-file ctx "c.txt" "staged-c\n")
                  (let ((fsm (gptel-make-fsm :info (list :buffer owner-buf :data wire-buf))))
                    (with-current-buffer wire-buf
                      (insert "### @macher edit workspace")
                      (macher-agent-sync-prompt-transformer nil fsm))
                    (let* ((tools (plist-get (gptel-fsm-info fsm) :tools))
                           (rfiw (cl-find "read_file_in_workspace" tools :key #'gptel-tool-name :test #'equal))
                           (efiw (cl-find "edit_file_in_workspace" tools :key #'gptel-tool-name :test #'equal))
                           (wfiw (cl-find "write_file_in_workspace" tools :key #'gptel-tool-name :test #'equal))
                           (dfiw (cl-find "delete_file_in_workspace" tools :key #'gptel-tool-name :test #'equal)))
                      (cl-letf (((symbol-function 'macher-agent-vfs-update-context-file)
                                 (lambda (c p content)
                                   (push p updated-paths)
                                   (funcall real-update c p content))))
                        ;; 1. Reading an unstaged file triggers zero update-context-file calls
                        (setq updated-paths nil)
                        (expect (funcall (gptel-tool-function rfiw) "unread.txt")
                                :to-equal "orig-unread\n")
                        (expect updated-paths :to-be nil)

                        ;; 2. Editing one file updates only that file, skipping untouched staged entries
                        (setq updated-paths nil)
                        (funcall (gptel-tool-function efiw) "b.txt" "staged-b" "edited-b")
                        (expect updated-paths :to-equal '("b.txt"))
                        (expect (macher-agent-vfs-read-context-file ctx "b.txt")
                                :to-equal "edited-b\n")

                        ;; 3. Reverting staged files back to disk content prunes zero-diff entries and clears dirty flag
                        (setq updated-paths nil)
                        (funcall (gptel-tool-function wfiw) "a.txt" "orig-a\n")
                        (funcall (gptel-tool-function wfiw) "b.txt" "orig-b\n")
                        (funcall (gptel-tool-function wfiw) "c.txt" "orig-c\n")
                        (expect (nreverse updated-paths) :to-equal '("a.txt" "b.txt" "c.txt"))
                        (expect (macher-agent-vfs-context-contents ctx) :to-be nil)
                        (expect (macher-agent-vfs-context-dirty-p ctx) :to-be nil)

                        ;; 4. Creating and deleting a skill via wrapped macher tools hot-loads and prunes it
                        (funcall (gptel-tool-function wfiw)
                                 "skills/hot-vfs-test/SKILL.md"
                                 "---\nname: hot-vfs-test\ndescription: Hot VFS skill\n---\nHot prompt.\n")
                        (expect (assq 'hot-vfs-test (macher-agent-context-skills ctx)) :not :to-be nil)
                        (funcall (gptel-tool-function dfiw) "skills/hot-vfs-test/SKILL.md")
                        (expect (assq 'hot-vfs-test (macher-agent-context-skills ctx)) :to-be nil)
                        (expect (macher-agent-vfs-context-dirty-p ctx) :to-be nil))))))
            (when (buffer-live-p wire-buf) (kill-buffer wire-buf)))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-17
  ;; --------------------------------------------------------------------------
  (it "VFS-17: wraps and replaces macher_search and custom macher-tool-prefix variants while preserving tool names and workspace overrides"
    (macher-agent-test-with-git-workspace (_root ctx owner-buf '(("notes.txt" . "disk-note\n")))
      (macher-agent-test-with-clean-gptel-registries
        (let* ((orig-macher-tools (copy-sequence macher-tools))
               (had-prefix (boundp 'macher-tool-prefix))
               (orig-prefix (when had-prefix (symbol-value 'macher-tool-prefix))))
          (unwind-protect
              (progn
                ;; 1. Post-rename macher_search: in-place replacement without spurious search_in_workspace,
                ;;    missing-tool re-injection, and singleton name preservation
                (set 'macher-tool-prefix "macher_")
                (setq macher-tools '((:name "search" :category "read" :function macher--tool-search)))
                (expect (macher-agent-macher-search-tool-name) :to-equal "macher_search")

                (let* ((upstream-ms (macher-agent-test-make-unregistered-tool
                                     :name "macher_search"
                                     :description "Upstream xref search"
                                     :category "macher"
                                     :args '((:name "pattern" :type string))
                                     :function (lambda (&rest _) "upstream-xref")))
                       (known-1 (list (cons "macher" (list (cons "macher_search" upstream-ms)))))
                       (known-stripped (list (cons "macher" nil))))
                  (macher-agent-vfs-wrap-macher-tools-in-alist known-1)
                  (expect (mapcar #'car (cdr (assoc "macher" known-1)))
                          :to-equal '("macher_search"))
                  (expect (gptel-tool-name upstream-ms) :to-equal "macher_search")
                  (expect (gptel-tool-category upstream-ms) :to-equal "perception")
                  (expect (gptel-tool-async upstream-ms) :to-be t)
                  (expect (mapcar (lambda (a) (plist-get a :name)) (gptel-tool-args upstream-ms))
                          :to-equal '("regexp" "path"))
                  ;; Singleton base tool retains its canonical name
                  (expect (gptel-tool-name (macher-agent-vfs-get-search-in-workspace-tool))
                          :to-equal "search_in_workspace")

                  ;; Missing macher_search is re-injected under macher_search (not search_in_workspace)
                  (macher-agent-vfs-wrap-macher-tools-in-alist known-stripped)
                  (expect (mapcar #'car (cdr (assoc "macher" known-stripped)))
                          :to-equal '("macher_search"))
                  (let ((reinjected (cdar (cdr (assoc "macher" known-stripped)))))
                    (expect (gptel-tool-name reinjected) :to-equal "macher_search")
                    (expect (gptel-tool-category reinjected) :to-equal "perception"))

                  ;; PTC execution dispatches (macher-search ...) against staged VFS overlay
                  (macher-agent-vfs-update-context-file ctx "notes.txt" "vfs-renamed-needle\n")
                  (with-current-buffer owner-buf
                    (setq-local gptel-tools (list upstream-ms))
                    (let ((ptc-out nil))
                      (macher-agent-sandbox-execute-ptc-script
                       "(macher-search \"vfs-renamed-needle\")"
                       ctx owner-buf
                       (lambda (res) (setq ptc-out res))
                       (lambda (err) (setq ptc-out err)))
                      (macher-agent-test-wait-until (lambda () ptc-out) 5.0)
                      (expect ptc-out :to-match "notes\\.txt:1:vfs-renamed-needle"))))

                ;; 2. Custom macher-tool-prefix ("custom_"), macher/macher__processed normalisation,
                ;;    and workspace script override creation + deletion
                (set 'macher-tool-prefix "custom_")
                (expect (macher-agent-macher-search-tool-name) :to-equal "custom_search")
                (let* ((raw-macher (macher-agent-test-make-unregistered-tool
                                    :name "custom_search"
                                    :description "Upstream custom search"
                                    :category "macher"
                                    :args '((:name "pattern" :type string))
                                    :function #'ignore))
                       (raw-proc (macher-agent-test-make-unregistered-tool
                                  :name "custom_search"
                                  :description "Processed custom search"
                                  :category "macher__processed"
                                  :args '((:name "pattern" :type string))
                                  :function #'ignore))
                       (norm-1 (macher-agent-vfs-normalize-tool-list (list raw-macher raw-proc) ctx)))
                  (dolist (tl norm-1)
                    (expect (gptel-tool-name tl) :to-equal "custom_search")
                    (expect (gptel-tool-category tl) :to-equal "perception")
                    (expect (mapcar (lambda (a) (plist-get a :name)) (gptel-tool-args tl))
                            :to-equal '("regexp" "path")))

                  ;; Stage workspace VFS override for search_in_workspace.el
                  (macher-agent-vfs-update-context-file
                   ctx "skills/scripts/search_in_workspace.el"
                   (concat "(setq macher-agent-search-in-workspace-tool\n"
                           "      (gptel-make-tool\n"
                           "       :name \"search_in_workspace\"\n"
                           "       :description \"Workspace custom override\"\n"
                           "       :category \"perception\"\n"
                           "       :args '((:name \"regexp\" :type \"string\" :description \"Pattern\"))\n"
                           "       :function (lambda (r) (format \"ws:%s\" r))))\n"
                           "(put 'macher-agent-search-in-workspace-tool 'ptc-function\n"
                           "     (lambda (r &optional _p _c) (format \"ws-ptc:%s\" r)))\n"))
                  (macher-agent-resolve-tool "search_in_workspace" nil nil ctx)
                  (let* ((norm-ws (macher-agent-vfs-normalize-tool-list norm-1 ctx))
                         (ws-copy (car norm-ws)))
                    (expect (gptel-tool-name ws-copy) :to-equal "custom_search")
                    (expect (gptel-tool-description ws-copy) :to-equal "Workspace custom override")
                    (expect (gethash ws-copy macher-agent-sandbox-workspace-tool-overrides) :to-be-truthy)

                    ;; Deleting workspace override reverts custom_search back to bundled ripgrep tool
                    (macher-agent-vfs-update-context-file ctx "skills/scripts/search_in_workspace.el" nil)
                    (let* ((norm-reverted (macher-agent-vfs-normalize-tool-list norm-ws ctx))
                           (rev-tl (car norm-reverted)))
                      (expect (gptel-tool-name rev-tl) :to-equal "custom_search")
                      (expect (gptel-tool-description rev-tl)
                              :to-equal (gptel-tool-description (macher-agent-vfs-get-search-in-workspace-tool)))
                      (expect (gethash rev-tl macher-agent-sandbox-workspace-tool-overrides) :to-be nil))))

                ;; 3. Empty prefix ("") only replaces macher/macher__processed tools, never unrelated tools
                (set 'macher-tool-prefix "")
                (expect (macher-agent-macher-search-tool-name) :to-equal "search")
                (let* ((web-search (macher-agent-test-make-unregistered-tool
                                    :name "search"
                                    :description "External web search"
                                    :category "web"
                                    :args '((:name "query" :type string))
                                    :function (lambda (q) q)))
                       (macher-empty (macher-agent-test-make-unregistered-tool
                                      :name "search"
                                      :description "Upstream macher empty-prefix search"
                                      :category "macher"
                                      :args '((:name "pattern" :type string))
                                      :function #'ignore))
                       (norm-empty (macher-agent-vfs-normalize-tool-list
                                    (list web-search macher-empty) ctx)))
                  (expect (car norm-empty) :to-be web-search)
                  (expect (gptel-tool-category (car norm-empty)) :to-equal "web")
                  (expect (gptel-tool-description (car norm-empty)) :to-equal "External web search")
                  (expect (gptel-tool-name (cadr norm-empty)) :to-equal "search")
                  (expect (gptel-tool-category (cadr norm-empty)) :to-equal "perception")))
            (setq macher-tools orig-macher-tools)
            (if had-prefix
                (set 'macher-tool-prefix orig-prefix)
              (makunbound 'macher-tool-prefix)))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-18: External-write single-shot invalidation regression (direct + wrapped tools)
  ;; --------------------------------------------------------------------------
  (it "VFS-18: invalidates ONCE on external write and allows subsequent VFS reads/edits/flushes on the new baseline"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("doc.txt" . "V1\n")))
      (macher-agent-test-with-clean-gptel-registries
        (let* ((wire-buf (generate-new-buffer " *vfs18-wire*"))
               (file-path (expand-file-name "doc.txt" root))
               (inval-msgs nil))
          (unwind-protect
              (progn
                (macher-install)
                (with-current-buffer owner-buf
                  (markdown-mode)
                  (gptel-mode 1)
                  (macher-agent-mode 1)
                  (macher-agent-vfs-wrap-macher-tools)
                  (let ((fsm (gptel-make-fsm :info (list :buffer owner-buf :data wire-buf))))
                    (with-current-buffer wire-buf
                      (insert "### @macher edit workspace")
                      (macher-agent-sync-prompt-transformer nil fsm))
                    (let* ((tools (plist-get (gptel-fsm-info fsm) :tools))
                           (rfiw (cl-find "read_file_in_workspace" tools :key #'gptel-tool-name :test #'equal))
                           (efiw (cl-find "edit_file_in_workspace" tools :key #'gptel-tool-name :test #'equal))
                           (wfiw (cl-find "write_file_in_workspace" tools :key #'gptel-tool-name :test #'equal)))
                      (cl-letf (((symbol-function 'message)
                                 (lambda (fmt &rest args)
                                   (let ((s (apply #'format fmt args)))
                                     (when (string-match-p "Replaced entry 'doc.txt'" s)
                                       (push s inval-msgs))))))
                        ;; 1. Agent stages V1 -> V2 via wrapped edit tool
                        (funcall (gptel-tool-function efiw) "doc.txt" "V1" "V2")
                        (expect (funcall (gptel-tool-function rfiw) "doc.txt") :to-equal "V2\n")
                        (expect (length inval-msgs) :to-equal 0)

                        ;; 2. External process writes V3 to disk mid-turn
                        (with-temp-file file-path (insert "V3\n"))

                        ;; 3. First wrapped read invalidates ONCE and returns V3
                        (expect (funcall (gptel-tool-function rfiw) "doc.txt") :to-equal "V3\n")
                        (expect (length inval-msgs) :to-equal 1)

                        ;; 4. Repeated reads do NOT re-fire invalidation
                        (expect (funcall (gptel-tool-function rfiw) "doc.txt") :to-equal "V3\n")
                        (expect (macher-agent-vfs-read-context-file ctx "doc.txt") :to-equal "V3\n")
                        (expect (length inval-msgs) :to-equal 1)

                        ;; 5. Agent edits V3 -> V4 via wrapped edit tool cleanly
                        (funcall (gptel-tool-function efiw) "doc.txt" "V3" "V4")
                        (expect (funcall (gptel-tool-function rfiw) "doc.txt") :to-equal "V4\n")
                        (expect (length inval-msgs) :to-equal 1)

                        ;; 6. Auto-sync and flush preserve V4 without wiping or re-invalidating
                        (setq-local macher-agent-suppress-patch t)
                        (macher-agent-vfs-auto-sync-context ctx)
                        (macher-agent-vfs-handle-flush ctx)
                        (expect (length inval-msgs) :to-equal 1)
                        (expect (macher-agent-vfs-read-context-file ctx "doc.txt") :to-equal "V4\n")
                        (expect (macher-agent-vfs-context-dirty-p ctx) :to-be t)

                        ;; 7. Mid-turn external write V3 -> V5 followed directly by write_file_in_workspace V6
                        ;;    invalidates ONCE before applying V6 so V6 survives auto-sync!
                        (with-temp-file file-path (insert "V5\n"))
                        (funcall (gptel-tool-function wfiw) "doc.txt" "V6\n")
                        (expect (length inval-msgs) :to-equal 2)
                        (macher-agent-vfs-auto-sync-context ctx)
                        (expect (length inval-msgs) :to-equal 2)
                        (expect (macher-agent-vfs-read-context-file ctx "doc.txt") :to-equal "V6\n")

                        ;; 8. Direct update-context-file after mid-turn external write V5 -> V7 invalidates ONCE
                        (with-temp-file file-path (insert "V7\n"))
                        (macher-agent-vfs-update-context-file ctx "doc.txt" "V8\n")
                        (expect (length inval-msgs) :to-equal 3)
                        (macher-agent-vfs-auto-sync-context ctx)
                        (expect (length inval-msgs) :to-equal 3)
                        (expect (macher-agent-vfs-read-context-file ctx "doc.txt") :to-equal "V8\n"))))))
            (when (buffer-live-p wire-buf) (kill-buffer wire-buf)))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-19: Visiting live Emacs buffer vs external on-disk write
  ;; --------------------------------------------------------------------------
  (it "VFS-19: never repeats invalidation when a live visiting buffer differs from external disk writes"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("live.el" . "(defvar v 1)\n")))
      (let* ((file-path (expand-file-name "live.el" root))
             (visiting-buf (find-file-noselect file-path))
             (inval-count 0))
        (unwind-protect
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (when (string-match-p "Replaced entry 'live.el'" (apply #'format fmt args))
                           (setq inval-count (1+ inval-count))))))
              ;; Live visiting buffer has unsaved edits differing from disk
              (with-current-buffer visiting-buf
                (goto-char (point-max))
                (insert ";; unsaved buffer edit\n"))

              ;; Agent edits live.el in VFS
              (macher-agent-vfs-update-context-file ctx "live.el" "(defvar v 2)\n")
              (expect inval-count :to-equal 0)

              ;; External write modifies live.el on disk (buffer still holds stale unsaved text)
              (with-temp-file file-path (insert "(defvar v 3)\n"))

              ;; First read invalidates ONCE; subsequent reads/edits/syncs NEVER re-invalidate
              (expect (macher-agent-vfs-read-context-file ctx "live.el") :to-equal "(defvar v 3)\n")
              (expect inval-count :to-equal 1)
              (expect (macher-agent-vfs-read-context-file ctx "live.el") :to-equal "(defvar v 3)\n")
              (macher-agent-vfs-auto-sync-context ctx)
              (expect inval-count :to-equal 1)

              ;; Subsequent VFS edit modifies new baseline v=3 -> v=4 cleanly
              (macher-agent-vfs-update-context-file ctx "live.el" "(defvar v 4)\n")
              (macher-agent-vfs-auto-sync-context ctx)
              (expect inval-count :to-equal 1)
              (expect (macher-agent-vfs-read-context-file ctx "live.el") :to-equal "(defvar v 4)\n"))
          (when (buffer-live-p visiting-buf)
            (with-current-buffer visiting-buf (set-buffer-modified-p nil))
            (kill-buffer visiting-buf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-20: External delete + recreate + virtual deletion reconciliation
  ;; --------------------------------------------------------------------------
  (it "VFS-20: reconciles external delete, external recreate, and virtual deletion with one invalidation per external change"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("ephem.txt" . "D1\n")))
      (let ((file-path (expand-file-name "ephem.txt" root))
            (inval-count 0))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (when (string-match-p "Replaced entry 'ephem.txt'" (apply #'format fmt args))
                       (setq inval-count (1+ inval-count))))))
          ;; 1. Stage edit D1 -> D2, then externally delete file on disk
          (macher-agent-vfs-update-context-file ctx "ephem.txt" "D2\n")
          (delete-file file-path)

          ;; First sync invalidates ONCE and purges stale entry; second sync is silent
          (macher-agent-vfs-auto-sync-context ctx)
          (expect inval-count :to-equal 1)
          (expect (macher-agent-vfs-context-contents ctx) :to-be nil)
          (expect (macher-agent-vfs-context-dirty-p ctx) :to-be nil)
          (macher-agent-vfs-auto-sync-context ctx)
          (expect inval-count :to-equal 1)

          ;; 2. External recreate D3 on disk -> VFS reads D3 and edits D3 -> D4 cleanly
          (with-temp-file file-path (insert "D3\n"))
          (expect (macher-agent-vfs-read-context-file ctx "ephem.txt") :to-equal "D3\n")
          (expect inval-count :to-equal 1)
          (macher-agent-vfs-update-context-file ctx "ephem.txt" "D4\n")
          (macher-agent-vfs-auto-sync-context ctx)
          (expect inval-count :to-equal 1)
          (expect (macher-agent-vfs-read-context-file ctx "ephem.txt") :to-equal "D4\n")

          ;; 3. Virtual deletion (curr = nil) followed by external disk edit D3 -> D5
          ;;    read-context-file MUST sync (not skip when curr is nil) and return D5!
          (macher-agent-vfs-update-context-file ctx "ephem.txt" nil)
          (expect (macher-agent-vfs-read-context-file ctx "ephem.txt") :to-throw 'error)
          (with-temp-file file-path (insert "D5\n"))
          (expect (macher-agent-vfs-read-context-file ctx "ephem.txt") :to-equal "D5\n")
          (expect inval-count :to-equal 2)
          ;; Subsequent access does NOT re-invalidate
          (expect (macher-agent-vfs-read-context-file ctx "ephem.txt") :to-equal "D5\n")
          (macher-agent-vfs-auto-sync-context ctx)
          (expect inval-count :to-equal 2)))))

  ;; --------------------------------------------------------------------------
  ;; VFS-21: Cloned sub-agent merging & peer baseline sync after external write
  ;; --------------------------------------------------------------------------
  (it "VFS-21: never re-introduces stale pre-external-write orig from cloned sub-agent on merge or baseline sync"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf '(("shared.el" . "V1\n")))
      (let ((file-path (expand-file-name "shared.el" root))
            (inval-count 0))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (when (string-match-p "Replaced entry 'shared.el'" (apply #'format fmt args))
                       (setq inval-count (1+ inval-count))))))
          ;; Parent stages V1 -> V2, then spawns child cloned with stale baseline (orig=V1, curr=V2, pending=nil)
          (macher-agent-vfs-update-context-file parent-ctx "shared.el" "V2\n")
          (let ((child-ctx (macher-agent-clone-context parent-ctx)))
            ;; External write updates disk to V3
            (with-temp-file file-path (insert "V3\n"))

            ;; Parent syncs to fresh V3 (1 invalidation), then clears clean entry
            (macher-agent-vfs-auto-sync-context parent-ctx)
            (expect inval-count :to-equal 1)
            (macher-agent-vfs-set-context-contents parent-ctx nil)

            ;; Child never touched shared.el, only wrote other.el; merging child MUST NOT re-inject stale orig=V1
            (macher-agent-vfs-update-context-file child-ctx "other.el" "CHILD\n")
            (macher-agent-vfs-merge-contexts parent-ctx child-ctx)
            (macher-agent-vfs-auto-sync-context parent-ctx)
            (expect inval-count :to-equal 1)
            (expect (macher-agent-vfs-read-context-file parent-ctx "shared.el") :to-equal "V3\n")
            (expect (macher-agent-vfs-read-context-file parent-ctx "other.el") :to-equal "CHILD\n")

            ;; Baseline sync from stale child-ctx into fresh parent-ctx MUST NOT overwrite parent's fresh orig=V3
            (macher-agent-vfs-update-context-file parent-ctx "shared.el" "V4\n")
            (let ((parent-entry (cl-find "shared.el" (macher-agent-vfs-context-contents parent-ctx)
                                         :key #'macher-agent-vfs-entry-path :test #'equal)))
              (setf (macher-agent-vfs-entry-pending-patch parent-entry) nil))
            (macher-agent-vfs-sync-baseline-context child-ctx parent-ctx)
            (let ((parent-entry (cl-find "shared.el" (macher-agent-vfs-context-contents parent-ctx)
                                         :key #'macher-agent-vfs-entry-path :test #'equal)))
              (expect (macher-agent-vfs-entry-orig parent-entry) :to-equal "V3\n"))
            (macher-agent-vfs-auto-sync-context parent-ctx)
            (expect inval-count :to-equal 1))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-22: Sandbox overlay sync, clean-entry preservation, and binary fidelity
  ;; --------------------------------------------------------------------------
  (it "VFS-22: syncs before sandbox hydration, skips unmodified post-invalidation entries, and preserves binary media bytes"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("data.txt" . "V1\n")))
      (let* ((file-path (expand-file-name "data.txt" root))
             (raw-bytes (unibyte-string #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a #x00 #xff #xfe)))
        ;; 1. Stage V1 -> V2, then external disk write V3 happens before sandbox hydration
        (macher-agent-vfs-update-context-file ctx "data.txt" "V2\n")
        (with-temp-file file-path (insert "V3\n"))
        (let ((sbx-read-1 nil))
          (macher-agent-vfs-call-with-strict-pipeline
           ctx (lambda (res) (setq sbx-read-1 res))
           (lambda (sbx done)
             (funcall done
                      (with-temp-buffer
                        (insert-file-contents (expand-file-name "data.txt" sbx))
                        (buffer-string)))))
          (macher-agent-test-wait-until (lambda () sbx-read-1) 5.0)
          (expect sbx-read-1 :to-equal "V3\n"))

        ;; 2. After invalidation left a clean entry (orig=V3, curr=V3), apply-overlay-stateless
        ;;    MUST skip unmodified entries so a fresh disk file V4 in sandbox is NOT clobbered!
        (let ((sbx-tmp (make-temp-file "macher-sbx-clean-" t)))
          (unwind-protect
              (progn
                (with-temp-file (expand-file-name "data.txt" sbx-tmp)
                  (insert "V4\n"))
                (macher-agent-vfs--apply-overlay-stateless
                 (macher-agent-vfs-context-contents ctx) root sbx-tmp)
                (expect (with-temp-buffer
                          (insert-file-contents (expand-file-name "data.txt" sbx-tmp))
                          (buffer-string))
                        :to-equal "V4\n")

                ;; 3. Modified unibyte binary entry is written with no-conversion without corruption
                (macher-agent-vfs-update-context-file ctx "raw.png" raw-bytes)
                (macher-agent-vfs--apply-overlay-stateless
                 (macher-agent-vfs-context-contents ctx) root sbx-tmp)
                (let ((written-bytes (with-temp-buffer
                                       (set-buffer-multibyte nil)
                                       (insert-file-contents-literally
                                        (expand-file-name "raw.png" sbx-tmp))
                                       (buffer-string))))
                  (expect (multibyte-string-p written-bytes) :to-be nil)
                  (expect written-bytes :to-equal raw-bytes)))
            (ignore-errors (delete-directory sbx-tmp t)))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-23: workspace-read-file only-staged external-write & deletion sync
  ;; --------------------------------------------------------------------------
  (it "VFS-23: syncs external disk writes and deletions in macher-agent-vfs-workspace-read-file even when only-staged is non-nil"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("staged.txt" . "S1\n")))
      (let ((file-path (expand-file-name "staged.txt" root)))
        (macher-agent-vfs-update-context-file ctx "staged.txt" "S2\n")
        (expect (macher-agent-vfs-workspace-read-file ctx "staged.txt" t) :to-equal "S2\n")

        ;; External disk write S3 invalidates staged entry on only-staged read
        (with-temp-file file-path (insert "S3\n"))
        (expect (macher-agent-vfs-workspace-read-file ctx "staged.txt" t) :to-equal "S3\n")
        (expect (macher-agent-vfs-context-dirty-p ctx) :to-be nil)

        ;; External deletion removes entry and returns nil on only-staged read
        (delete-file file-path)
        (expect (macher-agent-vfs-workspace-read-file ctx "staged.txt" t) :to-be nil)
        (expect (macher-agent-vfs-context-contents ctx) :to-be nil))))

  ;; --------------------------------------------------------------------------
  ;; VFS-24: Killed virtual buffer cleanup + read-only/missing buffer apply guards
  ;; --------------------------------------------------------------------------
  (it "VFS-24: purges killed virtual buffers cleanly from :vfs-buffers and applies edits safely to read-only buffers"
    (macher-agent-test-with-git-workspace (_root ctx owner-buf)
      (let ((vbuf (get-buffer-create "*vfs-24-ephem*"))
            (ro-buf (get-buffer-create "*vfs-24-readonly*")))
        (unwind-protect
            (progn
              (with-current-buffer vbuf
                (erase-buffer)
                (insert "live-v1"))
              (funcall (get 'macher-agent-write-buffer-in-workspace-tool 'ptc-function)
                       "*vfs-24-ephem*" "staged-v2" ctx)
              (expect (gethash "*vfs-24-ephem*" (macher-agent-vfs-buffers ctx)) :not :to-be nil)

              ;; Kill the virtual buffer externally
              (kill-buffer vbuf)

              ;; auto-sync removes the dead entry from :vfs-buffers instead of stranding (nil . nil)
              (macher-agent-vfs-auto-sync-context ctx)
              (expect (gethash "*vfs-24-ephem*" (macher-agent-vfs-buffers ctx)) :to-be nil)
              (expect (macher-agent-vfs-context-dirty-p ctx) :to-be nil)

              ;; Re-stage a fresh virtual buffer, kill it without auto-sync, then multi_edit must fail cleanly (not wrong-type-argument stringp nil)
              (setq vbuf (get-buffer-create "*vfs-24-ephem*"))
              (with-current-buffer vbuf (insert "live-again"))
              (funcall (get 'macher-agent-write-buffer-in-workspace-tool 'ptc-function)
                       "*vfs-24-ephem*" "staged-again" ctx)
              (kill-buffer vbuf)
              (let ((err-msg nil))
                (condition-case err
                    (funcall (get 'macher-agent-multi-edit-buffer-in-workspace-tool 'ptc-function)
                             "*vfs-24-ephem*" '((:old_text "staged" :new_text "next")) ctx)
                  (error (setq err-msg (error-message-string err))))
                (expect err-msg :to-match "does not exist")
                (expect err-msg :not :to-match "wrong-type-argument")
                (expect (gethash "*vfs-24-ephem*" (macher-agent-vfs-buffers ctx)) :to-be nil))

              ;; Scoped read-only buffer can be rendered by macher-agent-vfs-apply-virtual-buffers
              (with-current-buffer ro-buf
                (erase-buffer)
                (insert "ro-orig")
                (setq buffer-read-only t))
              (funcall (get 'macher-agent-write-buffer-in-workspace-tool 'ptc-function)
                       "*vfs-24-readonly*" "ro-updated" ctx)
              ;; Also insert a missing buffer entry to verify apply-virtual-buffers does not reset it
              (let ((missing-entry (macher-agent-vfs-create-entry
                                    :path "*vfs-24-missing*" :orig "a" :curr "b" :pending-patch t)))
                (puthash "*vfs-24-missing*" missing-entry (macher-agent-vfs-buffers ctx))
                (macher-agent-vfs-apply-virtual-buffers ctx)
                (expect (with-current-buffer ro-buf (buffer-string)) :to-equal "ro-updated")
                (let ((after-missing (gethash "*vfs-24-missing*" (macher-agent-vfs-buffers ctx))))
                  (expect (macher-agent-vfs-entry-orig after-missing) :to-equal "a")
                  (expect (macher-agent-vfs-entry-curr after-missing) :to-equal "b"))))
          (when (buffer-live-p vbuf) (kill-buffer vbuf))
          (when (buffer-live-p ro-buf)
            (with-current-buffer ro-buf (setq buffer-read-only nil))
            (kill-buffer ro-buf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-25: read_buffer_in_workspace & multi_edit_buffer_in_workspace delegation
  ;; --------------------------------------------------------------------------
  (it "VFS-25: read_buffer_in_workspace and multi_edit_buffer_in_workspace delegate via read-context-file, recompute dirty-p, and read staged :contents"
    (macher-agent-test-with-git-workspace (_root ctx owner-buf '(("src.el" . "(defvar a 1)\n")))
      (let ((vbuf (get-buffer-create "*vfs-25-buf*")))
        (unwind-protect
            (progn
              ;; 1. Virtual buffer staged, then externally edited -> read_buffer syncs and clears dirty-p
              (with-current-buffer vbuf
                (erase-buffer)
                (insert "buf-v1"))
              (funcall (get 'macher-agent-write-buffer-in-workspace-tool 'ptc-function)
                       "*vfs-25-buf*" "buf-v2" ctx)
              (expect (macher-agent-vfs-context-dirty-p ctx) :to-be t)
              (with-current-buffer vbuf
                (erase-buffer)
                (insert "buf-external-v3"))
              (expect (funcall (get 'macher-agent-read-buffer-in-workspace-tool 'ptc-function)
                               "*vfs-25-buf*" nil nil nil ctx)
                      :to-equal "buf-external-v3")
              (expect (macher-agent-vfs-context-dirty-p ctx) :to-be nil)

              ;; 2. Staged physical file in :contents is visible to read_buffer and editable via multi_edit_buffer
              (macher-agent-vfs-update-context-file ctx "src.el" "(defvar a 2)\n")
              (expect (funcall (get 'macher-agent-read-buffer-in-workspace-tool 'ptc-function)
                               "src.el" nil nil nil ctx)
                      :to-equal "(defvar a 2)\n")
              (funcall (get 'macher-agent-multi-edit-buffer-in-workspace-tool 'ptc-function)
                       "src.el" '((:old_text "a 2" :new_text "a 3")) ctx)
              (expect (macher-agent-vfs-read-context-file ctx "src.el")
                      :to-equal "(defvar a 3)\n"))
          (when (buffer-live-p vbuf) (kill-buffer vbuf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-26: sync-baseline-context removes externally deleted entries cleanly
  ;; --------------------------------------------------------------------------
  (it "VFS-26: removes externally deleted entries during macher-agent-vfs-sync-baseline-context"
    (macher-agent-test-with-git-workspace (root src-ctx _owner-buf '(("gone.txt" . "G1\n")))
      (let* ((file-path (expand-file-name "gone.txt" root))
             (_ (macher-agent-vfs-update-context-file src-ctx "gone.txt" "G2\n"))
             (tgt-ctx (macher-agent-clone-context src-ctx)))
        ;; Delete gone.txt on disk; both src-ctx and tgt-ctx hold stale orig=G1
        (delete-file file-path)
        (macher-agent-vfs-sync-baseline-context src-ctx tgt-ctx)
        ;; Entry must be removed from tgt-ctx (not mutated to orig=nil, curr=nil)
        (expect (macher-agent-vfs-context-contents tgt-ctx) :to-be nil)
        ;; Recreating gone.txt on disk allows read-context-file to read fresh disk content cleanly
        (with-temp-file file-path (insert "G3\n"))
        (expect (macher-agent-vfs-read-context-file tgt-ctx "gone.txt") :to-equal "G3\n"))))

  ;; --------------------------------------------------------------------------
  ;; VFS-TOOLS-01: Pure read_media_in_workspace + PTC scripts + runtime execution + clean subprocess byte-compile
  ;; --------------------------------------------------------------------------
  ;; --------------------------------------------------------------------------
  ;; VFS-27: Shallow Copy-on-Write layer isolation across parent/child forks
  ;; --------------------------------------------------------------------------
  (it "VFS-27: isolates Copy-on-Write layers across parent/child forks without mutating frozen snapshots"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf '(("a.el" . "A0\n") ("b.el" . "B0\n")))
      (macher-agent-vfs-update-context-file parent-ctx "a.el" "A1\n")
      (let ((child-1 (macher-agent-clone-context parent-ctx))
            (child-2 (macher-agent-clone-context parent-ctx)))
        ;; Inherited entries are visible as non-dirty baseline in both children
        (expect (macher-agent-vfs-context-dirty-p child-1) :to-be nil)
        (expect (macher-agent-vfs-read-context-file child-1 "a.el") :to-equal "A1\n")
        (expect (macher-agent-vfs-read-context-file child-2 "a.el") :to-equal "A1\n")
        ;; Parent mutation after fork does NOT leak into child-1 or child-2
        (macher-agent-vfs-update-context-file parent-ctx "a.el" "A2-parent\n")
        (expect (macher-agent-vfs-read-context-file parent-ctx "a.el") :to-equal "A2-parent\n")
        (expect (macher-agent-vfs-read-context-file child-1 "a.el") :to-equal "A1\n")
        (expect (macher-agent-vfs-read-context-file child-2 "a.el") :to-equal "A1\n")
        ;; Child-1 mutation does NOT leak to parent or child-2
        (macher-agent-vfs-update-context-file child-1 "b.el" "B1-child1\n")
        (expect (macher-agent-vfs-context-dirty-p child-1) :to-be t)
        (expect (macher-agent-vfs-read-context-file child-1 "b.el") :to-equal "B1-child1\n")
        (expect (macher-agent-vfs-read-context-file child-2 "b.el") :to-equal "B0\n")
        (expect (macher-agent-vfs-read-context-file parent-ctx "b.el") :to-equal "B0\n")
        ;; Merging read-only child-2 leaves parent untouched
        (macher-agent-vfs-merge-contexts parent-ctx child-2)
        (expect (macher-agent-vfs-read-context-file parent-ctx "a.el") :to-equal "A2-parent\n")
        (expect (macher-agent-vfs-read-context-file parent-ctx "b.el") :to-equal "B0\n")
        ;; Merging editing child-1 propagates ONLY b.el without clobbering parent's a.el
        (macher-agent-vfs-merge-contexts parent-ctx child-1)
        (expect (macher-agent-vfs-read-context-file parent-ctx "a.el") :to-equal "A2-parent\n")
        (expect (macher-agent-vfs-read-context-file parent-ctx "b.el") :to-equal "B1-child1\n"))))

  ;; --------------------------------------------------------------------------
  ;; VFS-28: Native O(1) read_file_in_workspace fast path
  ;; --------------------------------------------------------------------------
  (it "VFS-28: serves read_file_in_workspace via O(1) fast path without invoking macher--workspace-files"
    (macher-agent-test-with-git-workspace (root ctx owner-buf '(("lines.txt" . "one\ntwo\nthree\nfour\n")))
      (let* ((sym-path (expand-file-name "link.txt" root))
             (ws-files-calls 0))
        (make-symbolic-link "lines.txt" sym-path)
        (with-current-buffer owner-buf
          (setq-local macher-agent-persistent-context ctx)
          (macher-install)
          (macher-agent-vfs-wrap-macher-tools)
          (let ((rfiw (gptel-get-tool "read_file_in_workspace")))
            (cl-letf (((symbol-function 'macher--workspace-files)
                       (lambda (&rest _) (setq ws-files-calls (1+ ws-files-calls)) nil)))
              ;; 1. Full disk read + offset/limit/line-numbers
              (expect (funcall (gptel-tool-function rfiw) "lines.txt")
                      :to-equal "one\ntwo\nthree\nfour\n")
              (expect (funcall (gptel-tool-function rfiw) "lines.txt" 2 2 t)
                      :to-equal "     2\ttwo\n     3\tthree")
              ;; 2. Symlink returns target without dereferencing
              (expect (funcall (gptel-tool-function rfiw) "link.txt")
                      :to-equal "Symlink target: lines.txt")
              ;; 3. Staged VFS edit and virtual deletion honoured
              (macher-agent-vfs-update-context-file ctx "lines.txt" "alpha\nbeta\n")
              (expect (funcall (gptel-tool-function rfiw) "lines.txt")
                      :to-equal "alpha\nbeta\n")
              (macher-agent-vfs-update-context-file ctx "lines.txt" nil)
              (expect (funcall (gptel-tool-function rfiw) "lines.txt")
                      :to-throw 'error)
              ;; 4. Path traversal rejected
              (expect (funcall (gptel-tool-function rfiw) "../../etc/passwd")
                      :to-throw 'error)
              ;; 5. Zero calls to macher--workspace-files
              (expect ws-files-calls :to-equal 0)))))))

  (it "VFS-TOOLS-01: loads and executes read_media_in_workspace, wait_for_vfs_semaphore, commit_buffer, all VFS PTC scripts, and byte-compiles cleanly"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      ;; 1. read_media_in_workspace.el is pure gptel-tool with NO ptc-function, enforces path/deletion security
      (cl-remprop 'macher-agent-read-media-in-workspace-tool 'ptc-function)
      (cl-remprop 'read_media_in_workspace 'ptc-function)
      (let ((media-tool (macher-agent-resolve-tool "read_media_in_workspace" nil nil ctx))
            (img-path (expand-file-name "sample.png" root))
            (png-bytes (unibyte-string #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a)))
        (expect (macher-agent-tool-valid-p media-tool) :to-be t)
        (expect (gptel-tool-name media-tool) :to-equal "read_media_in_workspace")
        (expect (get 'macher-agent-read-media-in-workspace-tool 'ptc-function) :to-be nil)
        (expect (get 'read_media_in_workspace 'ptc-function) :to-be nil)

        (let ((coding-system-for-write 'no-conversion))
          (with-temp-file img-path
            (set-buffer-multibyte nil)
            (insert png-bytes)))
        (with-current-buffer owner-buf
          (let ((gptel-track-media t))
            ;; Valid workspace media succeeds and enqueues base64 media spec
            (let ((res (macher-agent-test-invoke-tool media-tool "sample.png")))
              (expect res :to-match "SUCCESS:")
              (let ((queued (macher-agent-context-media-queue ctx)))
                (expect (consp queued) :to-be t)
                (expect (plist-get (cdr queued) :mime) :to-equal "image/png")
                (expect (base64-decode-string (car queued)) :to-equal png-bytes)))

            ;; Path traversal outside workspace is blocked
            (let ((err-res (macher-agent-test-invoke-tool media-tool "../outside.png")))
              (expect err-res :to-match "SECURITY ERROR"))

            ;; Virtual deletion in VFS blocks fallback disk read
            (macher-agent-vfs-update-context-file ctx "sample.png" nil)
            (let ((del-res (macher-agent-test-invoke-tool media-tool "sample.png")))
              (expect del-res :to-match "ERROR:")
              (expect del-res :to-match "deleted in workspace")))))

      ;; 2. All 7 VFS PTC scripts load cleanly with ptc-function properties
      (dolist (entry '(("commit_buffer" . macher-agent-commit-buffer-tool)
                       ("read_buffer_in_workspace" . macher-agent-read-buffer-in-workspace-tool)
                       ("write_buffer_in_workspace" . macher-agent-write-buffer-in-workspace-tool)
                       ("multi_edit_buffer_in_workspace" . macher-agent-multi-edit-buffer-in-workspace-tool)
                       ("list_buffers_in_workspace" . macher-agent-list-buffers-in-workspace-tool)
                       ("search_in_workspace" . macher-agent-search-in-workspace-tool)
                       ("wait_for_vfs_semaphore" . macher-agent-wait-for-vfs-semaphore-tool)))
        (let ((tl (macher-agent-resolve-tool (car entry) nil nil ctx)))
          (expect (macher-agent-tool-valid-p tl) :to-be t)
          (expect (gptel-tool-name tl) :to-equal (car entry))
          (expect (functionp (get (cdr entry) 'ptc-function)) :to-be t)))

      ;; 2b. Runtime execution of wait_for_vfs_semaphore (direct JSON & PTC), firing callback ONCE
      (let ((sem-tool (macher-agent-resolve-tool "wait_for_vfs_semaphore" nil nil ctx))
            (json-calls nil))
        (with-current-buffer owner-buf
          (funcall (gptel-tool-function sem-tool)
                   (lambda (res) (push res json-calls))
                   "sem-json.txt")
          (macher-agent-vfs-update-context-file ctx "sem-json.txt" "json-payload\n")
          (expect (length json-calls) :to-equal 1)
          (expect (car json-calls) :to-match "Semaphore unlocked!")
          (expect (car json-calls) :to-match "json-payload")

          ;; PTC execution of (wait-for-vfs-semaphore ...) fires callback ONCE
          (setq-local gptel-tools (list sem-tool))
          (let ((ptc-calls nil))
            (macher-agent-sandbox-execute-ptc-script
             "(wait-for-vfs-semaphore \"sem-ptc.txt\")"
             ctx owner-buf
             (lambda (res) (push res ptc-calls))
             (lambda (err) (push err ptc-calls)))
            (macher-agent-test-wait-until
             (lambda () (gethash "sem-ptc.txt" macher-agent-vfs-callbacks))
             5.0)
            (macher-agent-vfs-update-context-file ctx "sem-ptc.txt" "ptc-payload\n")
            (macher-agent-test-wait-until (lambda () ptc-calls) 5.0)
            (expect (length ptc-calls) :to-equal 1)
            (expect (car ptc-calls) :to-match "Semaphore unlocked!")
            (expect (car ptc-calls) :to-match "ptc-payload"))))

      ;; 2c. commit_buffer rejects unauthorized non-existent buffer name without leaking a live buffer
      (let ((commit-tool (macher-agent-resolve-tool "commit_buffer" nil nil ctx)))
        (with-current-buffer owner-buf
          (let ((res (macher-agent-test-invoke-tool commit-tool "*unauth-leak-check*" "bad")))
            (expect res :to-match "SECURITY ERROR")
            (expect (get-buffer "*unauth-leak-check*") :to-be nil))))

      ;; 3. Subprocess byte-compilation of macher-agent-macher.el, macher-agent-vfs.el, and all skill scripts
      (let* ((vfs-lib (locate-library "macher-agent-vfs"))
             (vfs-file (if (string-suffix-p ".elc" vfs-lib)
                           (concat (file-name-sans-extension vfs-lib) ".el")
                         vfs-lib))
             (vfs-root (file-name-directory vfs-file))
             (macher-bridge-file (expand-file-name "macher-agent-macher.el" vfs-root))
             (core-root (file-name-directory (locate-library "macher-agent")))
             (sbx-root (file-name-directory (locate-library "macher-agent-sandbox")))
             (gptel-dir (file-name-directory (locate-library "gptel")))
             (macher-dir (file-name-directory (locate-library "macher")))
             (scripts-dir (expand-file-name "skills/scripts" vfs-root))
             (skill-scripts (when (file-directory-p scripts-dir)
                              (directory-files scripts-dir t "\\.el\\'")))
             (pkg-files (list macher-bridge-file vfs-file))
             (all-files (append pkg-files skill-scripts))
             (emacs-bin (expand-file-name invocation-name invocation-directory))
             (out-buf (generate-new-buffer " *vfs-compile-out*")))
        (expect (file-exists-p vfs-file) :to-be t)
        (expect (file-exists-p macher-bridge-file) :to-be t)
        (expect (length skill-scripts) :to-equal 8)
        (unwind-protect
            (dolist (file all-files)
              (with-current-buffer out-buf (erase-buffer))
              (let* ((is-pkg-mod (member file pkg-files))
                     (eval-code
                      (if is-pkg-mod
                          "(setq byte-compile-error-on-warn t byte-compile-warnings '(not obsolete docstrings))"
                        (format "(progn (setq byte-compile-error-on-warn t byte-compile-warnings '(not obsolete docstrings)) (dolist (feat '(cl-lib subr-x mailcap gptel macher macher-agent macher-agent-sandbox macher-agent-macher macher-agent-vfs)) (require feat nil t)) (let ((sdir %S)) (when (file-directory-p sdir) (dolist (f (directory-files sdir t \"\\\\.el\\\\'\")) (let* ((b (file-name-sans-extension (file-name-nondirectory f))) (h (intern (format \"macher-agent-%%s-tool\" (replace-regexp-in-string \"_\" \"-\" b)))) (u (intern (format \"macher-agent-%%s-tool\" (replace-regexp-in-string \"-\" \"_\" b))))) (eval `(defvar ,h nil)) (eval `(defvar ,u nil)))))))"
                                scripts-dir)))
                     (args (list "-Q" "--batch"
                                 "-L" (expand-file-name vfs-root)
                                 "-L" (expand-file-name core-root)
                                 "-L" (expand-file-name sbx-root)
                                 "-L" (expand-file-name gptel-dir)
                                 "-L" (expand-file-name macher-dir)
                                 "-L" (expand-file-name scripts-dir)
                                 "--eval" eval-code
                                 "-f" "batch-byte-compile"
                                 file))
                     (exit-code (apply #'call-process emacs-bin nil out-buf nil args))
                     (output (with-current-buffer out-buf (buffer-string)))
                     (filtered-output
                      (string-join
                       (cl-remove-if
                        (lambda (line)
                          (string-match-p
                           "macher/macher\\.el:.*Warning:.*is an obsolete macro"
                           line))
                        (split-string output "\n"))
                       "\n")))
                (let ((elc (concat (file-name-sans-extension file) ".elc")))
                  (when (file-exists-p elc)
                    (delete-file elc)))
                (when (/= exit-code 0)
                  (error "VFS-TOOLS-01 FAILED ON %s (exit %s): %s" file exit-code output))
                (expect exit-code :to-equal 0)
                (expect filtered-output :not :to-match "Warning:")
                (expect filtered-output :not :to-match "free variable")
                (expect filtered-output :not :to-match "unresolved call")))
          (when (buffer-live-p out-buf)
            (kill-buffer out-buf))))))

  ;; --------------------------------------------------------------------------
  ;; VFS-LIFE-01: Install / uninstall idempotency & plugin skill registration
  ;; --------------------------------------------------------------------------
  (it "VFS-LIFE-01: installs/uninstalls idempotently, registers skills dir, restores wrapped tools, and cleans injected search tools"
    (macher-agent-test-with-clean-gptel-registries
      (let ((skills-dir (file-name-as-directory
                         (expand-file-name (macher-agent-vfs-skills-directory))))
            (orig-fn (lambda (&rest _) "orig-result"))
            (sample-tool nil)
            (search-name (macher-agent-macher-search-tool-name)))
        (unwind-protect
            (progn
              (macher-install)
              (setq sample-tool (macher-agent-test-make-unregistered-tool
                                 :name "test_vfs_life_tool"
                                 :description "Sample macher tool"
                                 :category "macher"
                                 :args nil
                                 :function orig-fn))
              ;; Strip upstream search tool so wrap-macher-tools injects an :injected search tool
              (macher-agent-test-remove-known-tool "macher" search-name)
              (macher-agent-test-remove-known-tool "macher" "search_in_workspace")
              (macher-agent-test-remove-known-tool "macher" "macher_search")

              ;; Double install is strictly idempotent
              (macher-agent-vfs-install)
              (macher-agent-vfs-install)
              (expect (cl-count skills-dir macher-agent-plugin-skill-directories :test #'equal)
                      :to-equal 1)
              (expect macher-agent-workspace-read-file-function :to-be #'macher-agent-vfs-workspace-read-file)
              (expect macher-agent-workspace-list-files-function :to-be #'macher-agent-vfs-workspace-list-files)
              (expect (cl-count #'macher-agent-vfs--prepare-transmission
                                (macher-agent-get-pipeline-steps 'transmission))
                      :to-equal 1)
              (expect (cl-count #'macher-agent-vfs--wrap-composed-tools
                                (macher-agent-get-pipeline-steps 'preset-composition))
                      :to-equal 1)
              (expect (cl-count #'macher-agent-vfs--merge-payload
                                (macher-agent-get-pipeline-steps 'payload-merge))
                      :to-equal 1)
              (expect (cl-count #'macher-agent-vfs--clone-context-step
                                (macher-agent-get-pipeline-steps 'context-clone))
                      :to-equal 1)
              (expect (memq #'macher-agent-vfs-wrap-macher-tools macher-agent-wrap-tools-hook)
                      :to-be-truthy)
              (expect (assq 'agent macher-workspace-types-alist) :not :to-be nil)
              ;; Injected search tool is present under "macher" category
              (expect (ignore-errors (gptel-get-tool (list "macher" search-name))) :not :to-be nil)

              ;; Wrap sample tool, then uninstall -> restores orig-fn & removes injected search tool
              (macher-agent-vfs-wrap-single-tool sample-tool)
              (expect (gptel-tool-function sample-tool) :not :to-be orig-fn)

              (macher-agent-vfs-uninstall)
              (expect (cl-count skills-dir macher-agent-plugin-skill-directories :test #'equal)
                      :to-equal 0)
              (expect macher-agent-workspace-read-file-function :to-be nil)
              (expect macher-agent-workspace-list-files-function :to-be nil)
              (expect (memq #'macher-agent-vfs--prepare-transmission
                            (macher-agent-get-pipeline-steps 'transmission))
                      :to-be nil)
              (expect (memq #'macher-agent-vfs--wrap-composed-tools
                            (macher-agent-get-pipeline-steps 'preset-composition))
                      :to-be nil)
              (expect (memq #'macher-agent-vfs--merge-payload
                            (macher-agent-get-pipeline-steps 'payload-merge))
                      :to-be nil)
              (expect (memq #'macher-agent-vfs--clone-context-step
                            (macher-agent-get-pipeline-steps 'context-clone))
                      :to-be nil)
              (expect (memq #'macher-agent-vfs-wrap-macher-tools macher-agent-wrap-tools-hook)
                      :to-be nil)
              (expect (gptel-tool-function sample-tool) :to-be orig-fn)
              (expect (ignore-errors (gptel-get-tool (list "macher" search-name))) :to-be nil)
              (expect (assq 'agent macher-workspace-types-alist) :to-be nil))
          (macher-agent-vfs-install)))))

  ;; --------------------------------------------------------------------------
  ;; VFS-BOUND-01: Static package boundary verification
  ;; --------------------------------------------------------------------------
  (it "VFS-BOUND-01: enforces zero private core/sandbox/gptel or wire/zero-mem references in VFS modules, scripts, and tests"
    (let* ((vfs-lib (locate-library "macher-agent-vfs"))
           (vfs-file (if (string-suffix-p ".elc" vfs-lib)
                         (concat (file-name-sans-extension vfs-lib) ".el")
                       vfs-lib))
           (vfs-dir (file-name-directory vfs-file))
           (macher-bridge-file (expand-file-name "macher-agent-macher.el" vfs-dir))
           (scripts-dir (expand-file-name "skills/scripts" vfs-dir))
           (tests-dir (expand-file-name "test" vfs-dir))
           (re-core-priv (concat "\\bmacher-agent" "--"))
           (re-sbx-priv (concat "\\bmacher-agent-sandbox" "--"))
           (re-vfs-priv (concat "\\bmacher-agent-vfs" "--"))
           (re-macher-priv (concat "\\bmacher-agent-macher" "--"))
           (re-gptel-priv (concat "\\bgptel" "--"))
           (re-wire (concat "macher-agent-" "wire"))
           (re-zero-mem (concat "macher-agent-" "zero-mem")))
      (dolist (file (list vfs-file macher-bridge-file))
        (expect (file-exists-p file) :to-be t)
        (with-temp-buffer
          (insert-file-contents file)
          (let ((content (buffer-string)))
            (expect content :not :to-match re-core-priv)
            (expect content :not :to-match re-sbx-priv)
            (expect content :not :to-match re-gptel-priv)
            (expect content :not :to-match re-wire)
            (expect content :not :to-match re-zero-mem))))
      (expect (file-directory-p scripts-dir) :to-be t)
      (dolist (script (directory-files scripts-dir t "\\.el\\'"))
        (with-temp-buffer
          (insert-file-contents script)
          (let ((content (buffer-string)))
            (expect content :not :to-match re-core-priv)
            (expect content :not :to-match re-sbx-priv)
            (expect content :not :to-match re-vfs-priv)
            (expect content :not :to-match re-macher-priv)
            (expect content :not :to-match re-gptel-priv)
            (expect content :not :to-match re-wire)
            (expect content :not :to-match re-zero-mem)
            (when (string= (file-name-nondirectory script) "read_media_in_workspace.el")
              (expect content :not :to-match "ptc-function")))))
      (expect (file-directory-p tests-dir) :to-be t)
      (dolist (test-file (directory-files tests-dir t "\\.el\\'"))
        (with-temp-buffer
          (insert-file-contents test-file)
          (let ((content (buffer-string)))
            (expect content :not :to-match re-core-priv)
            (expect content :not :to-match re-sbx-priv)
            (expect content :not :to-match re-gptel-priv)
            (expect content :not :to-match re-wire)
            (expect content :not :to-match re-zero-mem)))))))

(provide 'test-vfs)
;;; test-vfs.el ends here
