;;; test-monorepo-integration.el --- 5-package monorepo integration tests -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; End-to-end integration test suite verifying all 5 monorepo packages
;; loaded together in a single hermetic Emacs session:
;;   1. `macher-agent'
;;   2. `macher-agent-wire'
;;   3. `macher-agent-zero-mem'
;;   4. `macher-agent-sandbox'
;;   5. `macher-agent-vfs'

;;; Code:

(unless (featurep 'macher-agent-monorepo-test-bootstrap)
  (setq load-prefer-newer t)
  (let* ((this-file (or load-file-name buffer-file-name))
         (this-dir (if this-file
                       (file-name-directory (expand-file-name this-file))
                     (expand-file-name default-directory)))
         (repo-root (or (locate-dominating-file this-dir "packages")
                        (locate-dominating-file default-directory "packages")
                        (expand-file-name ".." this-dir)))
         (push-front
          (lambda (dir)
            (when (and dir (file-directory-p dir))
              (let ((norm (file-name-as-directory (expand-file-name dir))))
                (setq load-path
                      (cons norm
                            (delete norm
                                    (delete (directory-file-name norm) load-path)))))))))
    (dolist (rel '("gptel"
                   "macher"
                   "packages/macher-agent"
                   "packages/macher-agent/test"
                   "packages/macher-agent/skills/scripts"
                   "packages/macher-agent-wire"
                   "packages/macher-agent-zero-mem"
                   "packages/macher-agent-sandbox"
                   "packages/macher-agent-vfs"
                   "packages/macher-agent-skills"
                   "test"))
      (funcall push-front (expand-file-name rel repo-root))))
  (provide 'macher-agent-monorepo-test-bootstrap))

(require 'cl-lib)
(require 'buttercup)
(require 'gptel)

;; Capture baseline prompt transforms BEFORE requiring any macher-agent package.
(defvar macher-agent-monorepo-test--pre-require-transforms
  (copy-sequence (default-value 'gptel-prompt-transform-functions))
  "Baseline `gptel-prompt-transform-functions' prior to requiring packages.")

(require 'macher-agent)
(require 'macher-agent-wire)
(require 'macher-agent-zero-mem)
(require 'macher-agent-sandbox)
(require 'macher-agent-vfs)
(require 'macher-agent-skills)

;; Capture prompt transforms AFTER requiring all 5 packages, BEFORE install.
(defvar macher-agent-monorepo-test--post-require-transforms
  (copy-sequence (default-value 'gptel-prompt-transform-functions))
  "Captured `gptel-prompt-transform-functions' immediately after `require'.")

(require 'macher-agent-test-helpers)

(defun macher-agent-monorepo-test--norm-dir (dir)
  "Return canonical directory string for DIR."
  (file-name-as-directory (expand-file-name dir)))

(describe "Monorepo 5-package end-to-end integration"

  ;; --------------------------------------------------------------------------
  ;; MONO-01: Pure require has zero load-time side effects
  ;; --------------------------------------------------------------------------
  (it "MONO-01: requiring all 5 packages does not mutate gptel-prompt-transform-functions before macher-agent-install"
    (expect (featurep 'macher-agent) :to-be-truthy)
    (expect (featurep 'macher-agent-wire) :to-be-truthy)
    (expect (featurep 'macher-agent-zero-mem) :to-be-truthy)
    (expect (featurep 'macher-agent-sandbox) :to-be-truthy)
    (expect (featurep 'macher-agent-vfs) :to-be-truthy)
    (expect (featurep 'macher-agent-skills) :to-be-truthy)

    ;; Current session check: requiring all 5 packages left transforms untouched
    (expect macher-agent-monorepo-test--post-require-transforms
            :to-equal macher-agent-monorepo-test--pre-require-transforms)
    (expect (memq #'macher-agent-fsm-hijack-transform
                  macher-agent-monorepo-test--post-require-transforms)
            :to-be nil)
    (expect (memq #'macher-agent-wire-finalize-transform
                  macher-agent-monorepo-test--post-require-transforms)
            :to-be nil)

    ;; All 4 plugins registered themselves on install/uninstall hooks
    (expect (memq #'macher-agent-wire-install macher-agent-install-hook) :to-be-truthy)
    (expect (memq #'macher-agent-zero-mem-install macher-agent-install-hook) :to-be-truthy)
    (expect (memq #'macher-agent-sandbox-install macher-agent-install-hook) :to-be-truthy)
    (expect (memq #'macher-agent-vfs-install macher-agent-install-hook) :to-be-truthy)
    (expect (memq #'macher-agent-skills-install macher-agent-install-hook) :to-be-truthy)
    (expect (memq #'macher-agent-wire-uninstall macher-agent-uninstall-hook) :to-be-truthy)
    (expect (memq #'macher-agent-zero-mem-uninstall macher-agent-uninstall-hook) :to-be-truthy)
    (expect (memq #'macher-agent-sandbox-uninstall macher-agent-uninstall-hook) :to-be-truthy)
    (expect (memq #'macher-agent-vfs-uninstall macher-agent-uninstall-hook) :to-be-truthy)
    (expect (memq #'macher-agent-skills-uninstall macher-agent-uninstall-hook) :to-be-truthy)

    ;; Hermetic subprocess check confirming clean require without any prior state
    (let* ((emacs-bin (expand-file-name invocation-name invocation-directory))
           (gptel-dir (file-name-directory (locate-library "gptel")))
           (macher-dir (file-name-directory (locate-library "macher")))
           (core-dir (file-name-directory (locate-library "macher-agent")))
           (wire-dir (file-name-directory (locate-library "macher-agent-wire")))
           (zm-dir (file-name-directory (locate-library "macher-agent-zero-mem")))
           (sbx-dir (file-name-directory (locate-library "macher-agent-sandbox")))
           (vfs-dir (file-name-directory (locate-library "macher-agent-vfs")))
           (eval-code
            (concat
             "(progn "
             "  (require 'gptel) "
             "  (let ((before (copy-sequence (default-value 'gptel-prompt-transform-functions)))) "
             "    (require 'macher-agent) "
             "    (require 'macher-agent-wire) "
             "    (require 'macher-agent-zero-mem) "
             "    (require 'macher-agent-sandbox) "
             "    (require 'macher-agent-vfs) "
             "    (let ((after (copy-sequence (default-value 'gptel-prompt-transform-functions)))) "
             "      (prin1 (list :unchanged (equal before after) "
             "                   :no-hijack (null (memq #'macher-agent-fsm-hijack-transform after)) "
             "                   :no-wire (null (memq #'macher-agent-wire-finalize-transform after)) "
             "                   :no-skill-dirs (null macher-agent-plugin-skill-directories))))))"))
           (out-buf (generate-new-buffer " *mono-01-subproc*")))
      (unwind-protect
          (let ((exit-code
                 (call-process emacs-bin nil (list out-buf nil) nil
                               "-Q" "--batch"
                               "-L" gptel-dir
                               "-L" macher-dir
                               "-L" core-dir
                               "-L" wire-dir
                               "-L" zm-dir
                               "-L" sbx-dir
                               "-L" vfs-dir
                               "--eval" "(setq load-prefer-newer t)"
                               "--eval" eval-code)))
            (expect exit-code :to-equal 0)
            (let ((res (with-current-buffer out-buf
                         (goto-char (point-min))
                         (read (current-buffer)))))
              (expect (listp res) :to-be t)
              (expect (plist-get res :unchanged) :to-be t)
              (expect (plist-get res :no-hijack) :to-be t)
              (expect (plist-get res :no-wire) :to-be t)
              (expect (plist-get res :no-skill-dirs) :to-be t)))
        (when (buffer-live-p out-buf)
          (kill-buffer out-buf)))))

  ;; --------------------------------------------------------------------------
  ;; MONO-02: macher-agent-install activates all 4 plugins, protocol & tools
  ;; --------------------------------------------------------------------------
  (it "MONO-02: macher-agent-install activates all 4 plugins, binds host protocols, registers skill dirs, and resolves tools across all packages"
    (macher-agent-test-reset-env)
    (macher-agent-install)

    ;; Core + Wire prompt transforms installed
    (expect (memq #'macher-agent-fsm-hijack-transform
                  (default-value 'gptel-prompt-transform-functions))
            :to-be-truthy)
    (expect (memq #'macher-agent-wire-finalize-transform
                  (default-value 'gptel-prompt-transform-functions))
            :to-be-truthy)

    ;; All plugin skill directories registered
    (let ((zm-skills (macher-agent-monorepo-test--norm-dir (macher-agent-zero-mem-skills-directory)))
          (sbx-skills (macher-agent-monorepo-test--norm-dir (macher-agent-sandbox-skills-directory)))
          (vfs-skills (macher-agent-monorepo-test--norm-dir (macher-agent-vfs-skills-directory)))
          (pack-skills (macher-agent-monorepo-test--norm-dir macher-agent-skills-directory)))
      (expect (member zm-skills macher-agent-plugin-skill-directories) :to-be-truthy)
      (expect (member sbx-skills macher-agent-plugin-skill-directories) :to-be-truthy)
      (expect (member vfs-skills macher-agent-plugin-skill-directories) :to-be-truthy)
      (expect (member pack-skills macher-agent-plugin-skill-directories) :to-be-truthy))

    ;; Host protocol bindings activated across all plugins
    (expect macher-agent-history-chars-function :to-be #'macher-agent-wire-history-chars)
    (expect macher-agent-conversation-source-buffer-function :to-be #'macher-agent-wire-source-buffer)
    (expect macher-agent-horizon-bounds-function :to-be #'macher-agent-wire-horizon-bounds)
    (expect macher-agent-evaluate-tool-forms-function :to-be #'macher-agent-sandbox-evaluate-tool-forms)
    (expect macher-agent-search-backend-function :to-be #'macher-agent-zero-mem-search)
    (expect macher-agent-workspace-read-file-function :to-be #'macher-agent-vfs-workspace-read-file)
    (expect macher-agent-workspace-list-files-function :to-be #'macher-agent-vfs-workspace-list-files)

    ;; Tools from all packages resolve cleanly through macher-agent-resolve-tool
    (macher-agent-test-with-git-workspace (_root ctx _owner-buf)
      (dolist (tool-name '("log_harness_issue"
                           "search_conversation_history"
                           "ptc_execution"
                           "delegate_tasks_to_subagents"
                           "read_media_in_workspace"
                           "read_buffer_in_workspace"
                           "wait_for_vfs_semaphore"
                           "replace_own_buffer"))
        (let ((resolved (macher-agent-resolve-tool tool-name nil nil ctx)))
          (expect (macher-agent-tool-valid-p resolved) :to-be t)
          (expect (gptel-tool-name resolved) :to-equal tool-name)))))

  ;; --------------------------------------------------------------------------
  ;; MONO-03: macher-agent-wire + macher-agent-zero-mem cooperation
  ;; --------------------------------------------------------------------------
  (it "MONO-03: wire and zero-mem cooperate end-to-end via host protocol and pre-truncate hook"
    (macher-agent-test-reset-env)
    (macher-agent-install)
    (let ((orig-buf (generate-new-buffer "*mono-03-chat*"))
          (tx-buf (generate-new-buffer "*mono-03-tx*")))
      (unwind-protect
          (let ((wbuf (macher-agent-wire-buffer-for orig-buf t))
                m-ob1 m-oe1 h1 m-ob2 m-oe2 h2)
            ;; Populate visible chat turns in orig-buf with live markers
            (with-current-buffer orig-buf
              (erase-buffer)
              (let ((b1 (point)))
                (insert "Turn 1: Quantum Lattice Protocol specification requires invariant alpha-9.\n\n")
                (let ((e1 (point)))
                  (setq m-ob1 (copy-marker b1)
                        m-oe1 (copy-marker e1)
                        h1 (macher-agent-wire-norm-hash orig-buf b1 e1))))
              (let ((b2 (point)))
                (insert "Turn 2: Latest active prompt discussing UI layout.\n\n")
                (let ((e2 (point)))
                  (setq m-ob2 (copy-marker b2)
                        m-oe2 (copy-marker e2)
                        h2 (macher-agent-wire-norm-hash orig-buf b2 e2)))))

            ;; Populate shadow wire buffer with two committed turns anchored to orig-buf
            (with-current-buffer wbuf
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (erase-buffer)
                (let ((p1 (point)))
                  (insert "Turn 1: Quantum Lattice Protocol specification requires invariant alpha-9.\n\n")
                  (put-text-property p1 (point) 'macher-agent-span-id 1)
                  (put-text-property p1 (point) 'macher-agent-turn-id 1)
                  (put-text-property p1 (point) 'macher-agent-span-kind 'user-prompt)
                  (put-text-property p1 (point) 'macher-agent-wire-chars 140)
                  (put-text-property p1 (point) 'macher-agent-orig-beg m-ob1)
                  (put-text-property p1 (point) 'macher-agent-orig-end m-oe1)
                  (put-text-property p1 (point) 'macher-agent-orig-nonempty t)
                  (put-text-property p1 (point) 'macher-agent-norm-hash h1)
                  (put-text-property p1 (point) 'macher-agent-wire-entries
                                     (list (list :role "user"
                                                 :content "Explain Quantum Lattice Protocol.")
                                           (list :role "assistant"
                                                 :content "Quantum Lattice Protocol specification requires invariant alpha-9."))))
                (let ((p2 (point)))
                  (insert "Turn 2: Latest active prompt discussing UI layout.\n\n")
                  (put-text-property p2 (point) 'macher-agent-span-id 2)
                  (put-text-property p2 (point) 'macher-agent-turn-id 2)
                  (put-text-property p2 (point) 'macher-agent-span-kind 'user-prompt)
                  (put-text-property p2 (point) 'macher-agent-wire-chars 60)
                  (put-text-property p2 (point) 'macher-agent-orig-beg m-ob2)
                  (put-text-property p2 (point) 'macher-agent-orig-end m-oe2)
                  (put-text-property p2 (point) 'macher-agent-orig-nonempty t)
                  (put-text-property p2 (point) 'macher-agent-norm-hash h2)
                  (put-text-property p2 (point) 'macher-agent-wire-entries
                                     (list (list :role "user"
                                                 :content "Latest active prompt discussing UI layout."))))))

            ;; Host protocol routes source buffer & history chars to shadow wire buffer
            (expect (funcall macher-agent-conversation-source-buffer-function orig-buf)
                    :to-be wbuf)
            (expect (funcall macher-agent-history-chars-function orig-buf)
                    :to-equal 200)

            ;; Wire horizon resolution returns :wire-managed t and triggers pre-truncate hook -> Zero-Mem indexing
            (let* ((macher-agent-context-compaction-ratio 0.25)
                   (bounds (funcall macher-agent-horizon-bounds-function orig-buf 120)))
              (expect (plist-get bounds :wire-managed) :to-be t)
              (expect (integerp (plist-get bounds :offset)) :to-be t)
              (expect (> (plist-get bounds :offset) 1) :to-be t)
              ;; Zero-Mem indexed the shadow wire buffer via macher-agent-wire-pre-truncate-hook
              (expect (gethash (buffer-name orig-buf) macher-agent-zero-mem-vector-storage)
                      :not :to-be nil)
              (let ((hz (buffer-local-value 'macher-agent-zero-mem--event-horizon orig-buf)))
                (expect (plist-get hz :wire-managed) :to-be t)
                (expect (plist-get hz :offset) :to-equal (plist-get bounds :offset))))

            ;; Zero-Mem truncate-buffer defers physical deletion of tx-buf because wire manages horizon,
            ;; and Zero-Mem injects search_conversation_history tool + system directive alongside wire
            (with-current-buffer tx-buf
              (erase-buffer)
              (insert "Visible turn 1: Quantum Lattice Protocol summary.\n"
                      "Visible turn 2: Latest active prompt discussing UI layout.\n"
                      (make-string 160 ?x))
              (let* ((before-tx (buffer-string))
                     (macher-agent-max-context-chars '((nil . 120)))
                     (state (make-macher-agent-transmission-state
                             :target-buffer orig-buf
                             :tools nil)))
                (macher-agent-zero-mem--pipe-inject-tool state)
                (macher-agent-zero-mem-truncate-buffer state)
                (macher-agent-zero-mem--pipe-inject-directive state)
                (expect (buffer-string) :to-equal before-tx)
                (expect (cl-some (lambda (tl)
                                   (equal (macher-agent-canonical-tool-name tl)
                                          "search_conversation_history"))
                                 (macher-agent-transmission-state-tools state))
                        :to-be-truthy)
                (expect (cl-some (lambda (d)
                                   (string-match-p "search_conversation_history" d))
                                 (macher-agent-transmission-state-directives state))
                        :to-be-truthy)))

            ;; Zero-Mem search recalls archived Turn 1 from the shadow wire buffer
            ;; even after max-context-chars reverts to default while horizon remains pinned
            (let ((recall (with-current-buffer orig-buf
                            (macher-agent-zero-mem-search orig-buf "Quantum Lattice Protocol"))))
              (expect recall :not :to-match "No matches found")
              (expect recall :to-match "Quantum Lattice Protocol specification"))

            ;; Wire clear invalidates Zero-Mem cache cleanly via macher-agent-wire-invalidated-hook
            (macher-agent-wire-clear-buffer orig-buf)
            (expect (gethash (buffer-name orig-buf) macher-agent-zero-mem-vector-storage) :to-be nil)
            (expect (buffer-local-value 'macher-agent-zero-mem--event-horizon orig-buf) :to-be nil))
        (when (buffer-live-p orig-buf)
          (when-let* ((wb (macher-agent-wire-buffer-for orig-buf nil)))
            (when (buffer-live-p wb) (kill-buffer wb)))
          (kill-buffer orig-buf))
        (when (buffer-live-p tx-buf)
          (kill-buffer tx-buf)))))

  ;; --------------------------------------------------------------------------
  ;; MONO-04: macher-agent-sandbox + macher-agent-vfs cooperation
  ;; --------------------------------------------------------------------------
  (it "MONO-04: sandbox PTC scripts execute VFS read_buffer_in_workspace and wait_for_vfs_semaphore cleanly"
    (macher-agent-test-reset-env)
    (macher-agent-install)
    (macher-agent-test-with-git-workspace (_root ctx owner-buf '(("hello.txt" . "hello-world\n")))
      (let* ((read-tool (macher-agent-resolve-tool "read_buffer_in_workspace" nil nil ctx))
             (sem-tool (macher-agent-resolve-tool "wait_for_vfs_semaphore" nil nil ctx))
             (vbuf (get-buffer-create "*mono-vfs-buf*")))
        (unwind-protect
            (progn
              (with-current-buffer vbuf
                (erase-buffer)
                (insert "alpha-line\nbeta-line\n"))
              (with-current-buffer owner-buf
                (macher-agent-vfs-add-buffer-to-scope vbuf)
                (setq-local gptel-tools (list read-tool sem-tool))

                ;; 1. PTC script calls (read-buffer-in-workspace ...)
                (let ((read-res nil))
                  (macher-agent-sandbox-execute-ptc-script
                   "(read-buffer-in-workspace \"*mono-vfs-buf*\")"
                   ctx owner-buf
                   (lambda (res) (setq read-res res))
                   (lambda (err) (setq read-res err)))
                  (macher-agent-test-wait-until (lambda () read-res) 5.0)
                  (expect read-res :to-equal "alpha-line\nbeta-line\n"))

                ;; 2. PTC script suspends on (wait-for-vfs-semaphore ...) and resumes on VFS update
                (let ((sem-calls nil))
                  (macher-agent-sandbox-execute-ptc-script
                   "(wait-for-vfs-semaphore \"mono-gate.txt\")"
                   ctx owner-buf
                   (lambda (res) (push res sem-calls))
                   (lambda (err) (push err sem-calls)))
                  (macher-agent-test-wait-until
                   (lambda () (gethash "mono-gate.txt" macher-agent-vfs-callbacks))
                   5.0)
                  (macher-agent-vfs-update-context-file ctx "mono-gate.txt" "unlocked-from-vfs\n")
                  (macher-agent-test-wait-until (lambda () sem-calls) 5.0)
                  (expect (length sem-calls) :to-equal 1)
                  (expect (car sem-calls) :to-match "Semaphore unlocked!")
                  (expect (car sem-calls) :to-match "unlocked-from-vfs"))))
          (when (buffer-live-p vbuf)
            (kill-buffer vbuf))))))

  ;; --------------------------------------------------------------------------
  ;; MONO-05: macher-agent-uninstall cleanly tears down all 5 packages
  ;; --------------------------------------------------------------------------
  (it "MONO-05: macher-agent-uninstall cleanly tears down all 5 packages and restores defaults"
    (macher-agent-test-reset-env)
    (macher-agent-install)
    (unwind-protect
        (progn
          (macher-agent-uninstall)

          ;; Core & Wire prompt transforms removed
          (expect (memq #'macher-agent-fsm-hijack-transform
                        (default-value 'gptel-prompt-transform-functions))
                  :to-be nil)
          (expect (memq #'macher-agent-wire-finalize-transform
                        (default-value 'gptel-prompt-transform-functions))
                  :to-be nil)

          ;; All plugin skill directories unregistered
          (let ((zm-skills (macher-agent-monorepo-test--norm-dir (macher-agent-zero-mem-skills-directory)))
                (sbx-skills (macher-agent-monorepo-test--norm-dir (macher-agent-sandbox-skills-directory)))
                (vfs-skills (macher-agent-monorepo-test--norm-dir (macher-agent-vfs-skills-directory))))
            (expect (member zm-skills macher-agent-plugin-skill-directories) :to-be nil)
            (expect (member sbx-skills macher-agent-plugin-skill-directories) :to-be nil)
            (expect (member vfs-skills macher-agent-plugin-skill-directories) :to-be nil))

          ;; Host protocol functions restored to core defaults
          (expect macher-agent-history-chars-function :to-be #'macher-agent-default-history-chars)
          (expect macher-agent-conversation-source-buffer-function :to-be #'identity)
          (expect macher-agent-horizon-bounds-function :to-be nil)
          (expect macher-agent-evaluate-tool-forms-function :to-be #'macher-agent-evaluate-trusted-tool-ast)
          (expect macher-agent-search-backend-function :to-be #'macher-agent-search-glob)
          (expect macher-agent-workspace-read-file-function :to-be nil)
          (expect macher-agent-workspace-list-files-function :to-be nil)

          ;; Plugin hooks removed
          (expect (memq #'macher-agent-wire--on-fsm-hijack macher-agent-fsm-hijack-hook) :to-be nil)
          (expect (memq #'macher-agent-zero-mem--on-wire-pre-truncate macher-agent-wire-pre-truncate-hook) :to-be nil)
          (expect (memq #'macher-agent-zero-mem--persist-interaction macher-agent-task-flush-hook) :to-be nil)
          (expect (memq #'macher-agent-sandbox--on-apply-payload macher-agent-apply-payload-hook) :to-be nil)
          (expect (memq #'macher-agent-vfs-handle-flush macher-agent-task-flush-hook) :to-be nil)
          (expect (memq #'macher-agent-vfs-auto-sync-context macher-agent-context-sync-hook) :to-be nil)

          ;; Plugin pipeline steps unregistered
          (let ((tx-steps (macher-agent-get-pipeline-steps 'transmission))
                (pc-steps (macher-agent-get-pipeline-steps 'preset-composition))
                (pm-steps (macher-agent-get-pipeline-steps 'payload-merge))
                (cc-steps (macher-agent-get-pipeline-steps 'context-clone)))
            (expect (memq #'macher-agent-zero-mem-truncate-buffer tx-steps) :to-be nil)
            (expect (memq #'macher-agent-sandbox-append-ptc-to-transmission tx-steps) :to-be nil)
            (expect (memq #'macher-agent-vfs--prepare-transmission tx-steps) :to-be nil)
            (expect (memq #'macher-agent-sandbox--inject-preset-ptc-tool pc-steps) :to-be nil)
            (expect (memq #'macher-agent-vfs--wrap-composed-tools pc-steps) :to-be nil)
            (expect (memq #'macher-agent-vfs--merge-payload pm-steps) :to-be nil)
            (expect (memq #'macher-agent-zero-mem--clone-context-step cc-steps) :to-be nil)
            (expect (memq #'macher-agent-sandbox--clone-context-step cc-steps) :to-be nil)
            (expect (memq #'macher-agent-vfs--clone-context-step cc-steps) :to-be nil)))
      ;; Restore clean installed state
      (macher-agent-install))))

(provide 'test-monorepo-integration)
;;; test-monorepo-integration.el ends here
