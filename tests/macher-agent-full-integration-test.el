;;; macher-agent-full-integration-test.el --- Full integration tests for macher-agent -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)
(require 'gptel)
(require 'macher-agent)
(let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "helpers" current-dir)))

(require 'macher-agent-test-harness)

(describe
 "Macher Agent Integration"

 (it "spawns sub-agents, delegates tasks, and concatenates responses via actual tool execution"
     (let ((call-count 0)
           (parent-buffer (generate-new-buffer "*macher-test-parent*")))
       (unwind-protect
           (with-current-buffer parent-buffer
             (when (fboundp 'markdown-mode) (markdown-mode))
             (when (fboundp 'gptel-mode) (gptel-mode 1))
             (when (fboundp 'macher-agent-mode) (macher-agent-mode 1))

             (insert "Delegate the data processing task to two sub-agents.")

             (with-macher-agent-test-context
              '(("macher-test-parent" .
                 ((:text nil :tool-use (("spawn_subagent" "subagent-1")
                                        ("spawn_subagent" "subagent-2")
                                        ("delegate_tasks_to_subagents"
                                         [(:buffer_name "subagent-1" :instructions "Process part A") 
                                          (:buffer_name "subagent-2" :instructions "Process part B")])))
                  (:text "Processed data part A and Processed data part B." :tool-use nil)))
                ("subagent-1" .
                 ((:text nil :tool-use (("submit_task_result" "Processed data part A.")))))
                ("subagent-2" .
                 ((:text nil :tool-use (("submit_task_result" "Processed data part B."))))))
              call-count

              (if (fboundp 'macher-agent-send)
                  (funcall (symbol-function 'macher-agent-send))
                (gptel-send))

              (let ((timeout 100))
                (while (and (< call-count 4) (> timeout 0))
                  (accept-process-output nil 0.05)
                  (cl-decf timeout)))

              (expect (>= call-count 4) :to-be t)
              (expect (buffer-string) :to-match "Processed data part A and Processed data part B.")

              (let ((s1 (get-buffer "subagent-1"))
                    (s2 (get-buffer "subagent-2")))
                (expect s1 :not :to-be nil)
                (expect s2 :not :to-be nil)
                (when s1
                  (with-current-buffer s1
                    (expect (buffer-string) :to-match "Process part A")))
                (when s2
                  (with-current-buffer s2
                    (expect (buffer-string) :to-match "Process part B"))))))

         (when (buffer-live-p parent-buffer) (kill-buffer parent-buffer))
         (when (get-buffer "subagent-1") (kill-buffer "subagent-1"))
         (when (get-buffer "subagent-2") (kill-buffer "subagent-2")))))

 (it "writes text to a workspace buffer and reads it back"
     (let ((call-count 0)
           (parent-buffer (generate-new-buffer "*macher-test-rw*")))
       (unwind-protect
           (with-current-buffer parent-buffer
             (when (fboundp 'markdown-mode) (markdown-mode))
             (when (fboundp 'gptel-mode) (gptel-mode 1))
             (when (fboundp 'macher-agent-mode) (macher-agent-mode 1))

             (insert "Write 'Hello Workspace' to target-buffer and read it back.")

             (with-macher-agent-test-context
              '(("macher-test-rw" .
                 ((:text nil :tool-use (("write_buffer_in_workspace" "target-buffer" "Hello Workspace")))
                  (:text nil :tool-use (("read_buffer_in_workspace" "target-buffer")))
                  (:text "Final output: Hello Workspace" :tool-use nil))))
              call-count

              (if (fboundp 'macher-agent-send)
                  (funcall (symbol-function 'macher-agent-send))
                (gptel-send))

              (let ((timeout 100))
                (while (and (< call-count 3) (> timeout 0))
                  (accept-process-output nil 0.05)
                  (cl-decf timeout)))

              (expect (>= call-count 3) :to-be t)
              (expect (buffer-string) :to-match "Final output: Hello Workspace")

              (let ((vfs-entry (or (cl-find "target-buffer" (macher-agent--get-context-contents ctx) :key #'car :test #'equal)
                                   (cl-find (expand-file-name "target-buffer") (macher-agent--get-context-contents ctx) :key #'car :test #'equal))))
                (expect vfs-entry :not :to-be nil)
                (when vfs-entry
                  (expect (macher-agent-vfs-entry-curr vfs-entry) :to-equal "Hello Workspace")))))

         (when (buffer-live-p parent-buffer) (kill-buffer parent-buffer)))))

 (it "inflates virtual workspace edits into the physical sandbox directory for tool execution"
     (let ((call-count 0)
           (sandbox-dir (make-temp-file "test-integration-sandbox-" t))
           (parent-buffer (generate-new-buffer "*macher-test-sandbox-inflate*")))
       (unwind-protect
           (with-current-buffer parent-buffer
             (when (fboundp 'markdown-mode) (markdown-mode))
             (when (fboundp 'gptel-mode) (gptel-mode 1))
             (when (fboundp 'macher-agent-mode) (macher-agent-mode 1))

             (insert "Write file via workspace tool and check sandbox inflation.")

             (with-macher-agent-test-context
              '(("macher-test-sandbox-inflate" .
                 ((:text nil :tool-use (("write_buffer_in_workspace" "sandbox_test.txt" "sandbox inflated content")))
                  (:text "Sandbox write verified." :tool-use nil))))
              call-count

              (macher-agent--set-context-data ctx :sandbox-path sandbox-dir)

              (if (fboundp 'macher-agent-send)
                  (funcall (symbol-function 'macher-agent-send))
                (gptel-send))

              (let ((timeout 100))
                (while (and (< call-count 2) (> timeout 0))
                  (accept-process-output nil 0.05)
                  (cl-decf timeout)))

              (expect (>= call-count 2) :to-be t)

              (let ((vfs-entry (or (cl-find "sandbox_test.txt" (macher-agent--get-context-contents ctx) :key #'car :test #'equal)
                                   (cl-find (expand-file-name "sandbox_test.txt") (macher-agent--get-context-contents ctx) :key #'car :test #'equal))))
                (expect vfs-entry :not :to-be nil)
                (expect (macher-agent-vfs-entry-curr vfs-entry) :to-equal "sandbox inflated content"))

              (macher-agent-sandbox-inflate sandbox-dir (macher-agent-workspace-vfs-buffers (macher-agent--get-context-workspace ctx)) (macher-agent-context-root ctx) (macher-agent--get-context-contents ctx))
              (let ((sandbox-file (expand-file-name "sandbox_test.txt" sandbox-dir)))
                (expect (file-exists-p sandbox-file) :to-be t)
                (when (file-exists-p sandbox-file)
                  (with-temp-buffer
                    (insert-file-contents sandbox-file)
                    (expect (buffer-string) :to-equal "sandbox inflated content"))))))

         (when (buffer-live-p parent-buffer) (kill-buffer parent-buffer))
         (when (file-exists-p sandbox-dir) (delete-directory sandbox-dir t)))))

 (it "reads a workspace buffer first and then applies multi-edit to it"
     (let ((call-count 0)
           (target-buf (generate-new-buffer "read-first-target.txt"))
           (parent-buffer (generate-new-buffer "*macher-test-read-then-edit*")))
       (unwind-protect
           (with-current-buffer target-buf
             (insert "initial content line 1\ninitial content line 2")
             (with-current-buffer parent-buffer
               (when (fboundp 'markdown-mode) (markdown-mode))
               (when (fboundp 'gptel-mode) (gptel-mode 1))
               (when (fboundp 'macher-agent-mode) (macher-agent-mode 1))

               (insert "Read read-first-target.txt and then edit line 2.")

               (with-macher-agent-test-context
                '(("macher-test-read-then-edit" .
                   ((:text nil :tool-use (("read_buffer_in_workspace" "read-first-target.txt")))
                    (:text nil :tool-use (("multi_edit_buffer_in_workspace" "read-first-target.txt" [(:old_text "initial content line 2" :new_text "modified line 2")])))
                    (:text "Edit complete" :tool-use nil))))
                call-count

                (if (fboundp 'macher-agent-send)
                    (funcall (symbol-function 'macher-agent-send))
                  (gptel-send))

                (let ((timeout 100))
                  (while (and (< call-count 3) (> timeout 0))
                    (accept-process-output nil 0.05)
                    (cl-decf timeout)))

                (expect (>= call-count 3) :to-be t)
                (let ((vfs-entry (cl-find "read-first-target.txt" (macher-agent--get-context-contents ctx)
                                          :key #'car :test #'equal)))
                  (expect vfs-entry :not :to-be nil)
                  (when vfs-entry
                    (expect (macher-agent-vfs-entry-curr vfs-entry) :to-equal "initial content line 1\nmodified line 2"))))))
         (when (buffer-live-p parent-buffer) (kill-buffer parent-buffer))
         (when (buffer-live-p target-buf) (kill-buffer target-buf)))))

 (it "interleaves macher-agent tools and macher tools seamlessly across turns"
     (let ((call-count 0)
           (parent-buffer (generate-new-buffer "*macher-test-interleave*")))
       (unwind-protect
           (with-current-buffer parent-buffer
             (when (fboundp 'markdown-mode) (markdown-mode))
             (when (fboundp 'gptel-mode) (gptel-mode 1))
             (when (fboundp 'macher-agent-mode) (macher-agent-mode 1))

             (insert "Interleave macher-agent and macher tool edits.")

             (with-macher-agent-test-context
              '(("macher-test-interleave" .
                 ((:text nil :tool-use (("write_buffer_in_workspace" "interleave-full.txt" "Agent Edit 1")))
                  (:text nil :tool-use (("read_buffer_in_workspace" "interleave-full.txt")))
                  (:text "Read final interleaved edit" :tool-use nil))))
              call-count

              ;; Simulate a macher tool (from macher.el) writing to the same context in between
              (fset 'macher-agent--mock-midturn
                    (lambda (&rest _)
                      (macher--tool-write-file ctx "interleave-full.txt" "Macher Edit 2 Overwrite")))
              (add-hook 'macher-agent-post-tool-use-hook #'macher-agent--mock-midturn)

              (unwind-protect
                  (progn
                    (if (fboundp 'macher-agent-send)
                        (funcall (symbol-function 'macher-agent-send))
                      (gptel-send))

                    (let ((timeout 100))
                      (while (and (< call-count 3) (> timeout 0))
                        (accept-process-output nil 0.05)
                        (cl-decf timeout)))

                    (expect (>= call-count 3) :to-be t)
                    (let ((norm-key (macher-agent--normalize-path-key "interleave-full.txt" ctx)))
                      (expect (macher-agent--read-context-file ctx norm-key) :to-equal "Macher Edit 2 Overwrite")))
                (remove-hook 'macher-agent-post-tool-use-hook #'macher-agent--mock-midturn))))

         (when (buffer-live-p parent-buffer) (kill-buffer parent-buffer)))))

 (it "writes a file via VFS write tool and reads it back via search_in_workspace"
     (let* ((call-count 0)
            (temp-dir (file-name-as-directory (make-temp-file "macher-test-" t)))
            (parent-buffer (generate-new-buffer "*macher-test-vfs-search*")))
       
       (unwind-protect
           (with-current-buffer parent-buffer
             
             (setq default-directory temp-dir)
             
             (call-process "git" nil nil nil "init")
             
             (call-process "git" nil nil nil "config" "user.email" "test@example.com")
             (call-process "git" nil nil nil "config" "user.name" "Test User")
             (call-process "git" nil nil nil "commit" "--allow-empty" "-m" "Initial commit")

             (when (fboundp 'markdown-mode) (markdown-mode))
             (when (fboundp 'gptel-mode) (gptel-mode 1))
             (when (fboundp 'macher-agent-mode) (macher-agent-mode 1))

             (insert "Write to VFS and search via tool.")

             (with-macher-agent-test-context
              '(("macher-test-vfs-search" .
                 ((:text nil :tool-use (("write_buffer_in_workspace" "vfs-integration-target.txt" "findme_vfs_token_12345")))
                  (:text nil :tool-use (("search_in_workspace" "findme_vfs_token_12345")))
                  (:text "Found token in VFS search" :tool-use nil))))
              call-count

              (if (fboundp 'macher-agent-send)
                  (funcall (symbol-function 'macher-agent-send))
                (gptel-send))

              (let ((timeout 100))
                (while (and (> timeout 0) 
                            (let ((current-fsm (macher-agent--get-fsm-latest)))
                              (or (not (string-match-p "Found token in VFS search" (buffer-string)))
                                  (and current-fsm (not (eq (gptel-fsm-state current-fsm) 'STOP))))))
                  (accept-process-output nil 0.05)
                  (cl-decf timeout)))

              (expect (>= call-count 3) :to-be t)
              (expect (buffer-string) :to-match "Found token in VFS search"))

             (when (buffer-live-p parent-buffer)
               (when (fboundp 'gptel-abort)
                 (gptel-abort parent-buffer))
               (kill-buffer parent-buffer))
             
             (when (file-directory-p temp-dir)
               (delete-directory temp-dir t)))))))
(provide 'macher-agent-full-integration-test)
;;; macher-agent-full-integration-test.el ends here
