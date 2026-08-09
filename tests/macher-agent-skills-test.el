;;; macher-agent-skills-test.el --- Tests for macher-agent-skills -*- lexical-binding: t -*-

(require 'buttercup)
(require 'macher-agent-macher-bridge)
(require 'macher-agent)
(let* ((file-name (or load-file-name buffer-file-name))
       (current-dir (if file-name (file-name-directory file-name) default-directory)))
  (add-to-list 'load-path (expand-file-name "helpers" current-dir)))

(require 'macher-agent-test-harness)

(describe
 "macher-agent-skills"

 (describe
  "Orchestration Tools (skills/scripts/*.el)"
  (before-all
   ;; Load all tool scripts to define their variables for testing
   (let* ((file-name (or load-file-name buffer-file-name))
          (test-dir (if file-name (file-name-directory file-name) default-directory))
          (root-dir (or (locate-dominating-file test-dir "skills") test-dir))
          (scripts-dir (expand-file-name "skills/scripts" root-dir)))
     (dolist (script (directory-files scripts-dir t "\\.el$"))
       (load script nil t))))
  (it "guarantees list_buffers_in_workspace output perfectly matches context-tree buffer categorisation"
      (let*
          ((ctx
            (macher--make-context
             :contents (list (macher-agent-vfs-make-entry "*pure-buffer*" "" "")
                             (macher-agent-vfs-make-entry "/external/path.txt" "" "")
                             (macher-agent-vfs-make-entry "/root/internal.txt" "" ""))))
           (list-tool-fn (gptel-tool-function macher-agent-list-buffers-in-workspace-tool)))

        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        (spy-on 'macher-agent-context-classify-entry :and-call-fake
                (lambda (path &rest _)
                  (pcase path
                    ("*pure-buffer*" 'buffer)
                    ("/external/path.txt" 'external)
                    ("/root/internal.txt" 'file))))

        (with-macher-agent-mock-fsm ctx
                                    (let ((result (funcall list-tool-fn nil)))
                                      (expect result :to-match "\\*pure-buffer\\*")
                                      (expect result :to-match "/external/path\\.txt")
                                      (expect result :not :to-match "internal\\.txt")))))
  (it "properly parses a JSON string into a vector for task delegation"
      (let* ((ctx (macher--make-context))
             (callback-called nil)
             (callback (lambda (res) (setq callback-called res)))
             (json-tasks (vector (list :buffer_name "test-sub" :instructions "do work")))
             (tool-fn (gptel-tool-function macher-agent-delegate-tasks-to-subagents-tool))
             (buf (get-buffer-create "test-sub")))
        
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        (spy-on 'macher-agent-a2a-dispatch)
        (spy-on 'macher-agent--ensure-access)
        
        (with-macher-agent-mock-fsm ctx
                                    (funcall tool-fn callback :tasks json-tasks))
        
        (expect 'macher-agent-a2a-dispatch :to-have-been-called)
        (kill-buffer buf)))

  (it "reports an error if gptel-send aborts or fails silently"
      (let* ((buf (generate-new-buffer "*macher-agent: worker*"))
             (callback-called nil)
             (callback (lambda (msg) (setq callback-called msg))))

        (spy-on 'macher-agent-resolve-context :and-return-value (macher--make-context :contents nil))
        ;; Simulate gptel-send firing and instantly triggering the post-response hook
        (spy-on 'gptel-send :and-call-fake
                (lambda ()
                  (with-current-buffer buf
                    (erase-buffer)
                    (run-hook-with-args 'gptel-post-response-functions (point-min) (point-max)))))

        (macher-agent-a2a-dispatch
         (list (list :type 'SEND_MESSAGE
                     :task-id "task-err"
                     :message "test"
                     :metadata (list :buffer_name (buffer-name buf))))
         callback)
        
        (let ((res (if (vectorp callback-called) (aref callback-called 0) callback-called)))
          (expect (plist-get res :status) :to-equal 'error)
          (expect (plist-get res :error) :to-match "stopped silently"))
        (kill-buffer buf)))

  (it "correctly aggregates results from multiple event-driven sub-agents"
      (let* ((buf1 (generate-new-buffer "worker1"))
             (buf2 (generate-new-buffer "worker2"))
             (callback-called nil)
             (callback (lambda (msg) (setq callback-called msg))))
        
        (spy-on 'macher-agent-resolve-context :and-return-value (macher--make-context))
        ;; Mock the dispatcher to instantly return a success payload rather than firing the network
        (cl-letf (((symbol-function 'gptel-send)
                   (lambda ()
                     (let ((cb (bound-and-true-p macher-agent--a2a-callback))
                           (task-id (bound-and-true-p macher-agent--current-task-id)))
                       (when cb
                         (funcall cb (list :status 'success :data (format "Output from %s" (buffer-name)))))))))
          (macher-agent-a2a-dispatch
           (list (list :type 'SEND_MESSAGE
                       :task-id "t1"
                       :message "do w1"
                       :metadata (list :buffer_name "worker1"))
                 (list :type 'SEND_MESSAGE
                       :task-id "t2"
                       :message "do w2"
                       :metadata (list :buffer_name "worker2")))
           callback))
        
        (expect (length callback-called) :to-equal 2)
        (expect (plist-get (aref callback-called 0) :status) :to-equal 'success)
        (expect (plist-get (aref callback-called 0) :data) :to-match "Output from worker1")
        (expect (plist-get (aref callback-called 1) :data) :to-match "Output from worker2")
        (kill-buffer buf1)
        (kill-buffer buf2)))

  (it "ensures target buffer exists when using write_buffer_in_workspace to support patch UI"
      (let* ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "dummy" "dummy" "dummy"))))
             (tool-fn (gptel-tool-function macher-agent-write-buffer-in-workspace-tool)))
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        
        (with-macher-agent-mock-fsm ctx
                                    (funcall tool-fn nil "*new-virtual-asset*" "Ghost content"))
        
        (expect (cl-find "*new-virtual-asset*" (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :not :to-be nil)
        (let ((contents (cl-find "*new-virtual-asset*" (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
          (expect (macher-agent-vfs-entry-curr contents) :to-equal "Ghost content"))))
  
  (it "rejects fuzzy security matching in read_buffer_in_workspace"
      (let* ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "*scratch*" "" "content"))))
             (tool-fn (gptel-tool-function macher-agent-read-buffer-in-workspace-tool)))
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        (with-macher-agent-mock-fsm ctx
                                    (let ((result (funcall tool-fn nil "scratch")))
                                      (expect result :to-match "SECURITY ERROR.*scratch.*")))))

  (it "submit_task_result triggers parent callback and flags completion"
      (let* ((ctx (macher--make-context))
             (buf (generate-new-buffer "worker-buf"))
             (tool-fn (gptel-tool-function macher-agent-submit-task-result-tool))
             (callback-data nil))
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        (expect (gptel-tool-description macher-agent-submit-task-result-tool)
                :to-match "CRITICAL DIRECTIVE: You MUST use the `submit_task_result` tool")
        (with-current-buffer buf
          (setq-local macher-agent--parent-callback (lambda (res) (setq callback-data res)))
          (with-macher-agent-mock-fsm ctx
                                      (funcall tool-fn nil "My final answer"))
          (expect (plist-get callback-data :data) :to-equal "My final answer")
          (expect macher-agent-task-finished :to-be t))
        (kill-buffer buf)))
  
  (it "write_buffer_in_workspace registers a virtual edit safely"
      (let* ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "test-buf" "orig" "orig"))))
             (tool-fn (gptel-tool-function macher-agent-write-buffer-in-workspace-tool)))
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        
        (with-macher-agent-mock-fsm ctx
                                    (let* ((response (funcall tool-fn nil "test-buf" "New virtual content")))
                                      (expect response :to-match "SUCCESS")
                                      (expect (macher-context-dirty-p ctx) :to-be t)
                                      (expect (macher-agent-vfs-entry-curr (cl-find "test-buf" (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)) :to-equal "New virtual content")))))

  (it "multi_edit_buffer_in_workspace uses a decoupled deterministic scratchpad"
      (let* ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "test-file.rs" "line1\nline2" "line1\nline2"))))
             (tool-fn (gptel-tool-function macher-agent-multi-edit-buffer-in-workspace-tool)))
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        
        (with-macher-agent-mock-fsm ctx
                                    (let* ((edits (vector (list :old_text "line2" :new_text "line3")))
                                           (response (funcall tool-fn nil "test-file.rs" edits)))
                                      (expect response :to-match "SUCCESS")
                                      (let ((contents (cl-find "test-file.rs" (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                                        (expect (macher-agent-vfs-entry-curr contents) :to-equal "line1\nline3"))))))

  (it "allows multi_edit_buffer_in_workspace after reading a live buffer"
      (let* ((buf (generate-new-buffer "read-then-edit.txt"))
             (ctx (macher--make-context :contents nil))
             (read-fn (gptel-tool-function macher-agent-read-buffer-in-workspace-tool))
             (edit-fn (gptel-tool-function macher-agent-multi-edit-buffer-in-workspace-tool)))
        (with-current-buffer buf
          (insert "hello world"))
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        (spy-on 'macher-agent--ensure-access)
        
        (with-macher-agent-mock-fsm ctx
                                    (funcall read-fn nil "read-then-edit.txt")
                                    (let* ((edits (vector (list :old_text "hello" :new_text "goodbye")))
                                           (response (funcall edit-fn nil "read-then-edit.txt" edits)))
                                      (expect response :to-match "SUCCESS")
                                      (let ((contents (cl-find "read-then-edit.txt" (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
                                        (expect contents :not :to-be nil)
                                        (when contents
                                          (expect (macher-agent-vfs-entry-curr contents) :to-equal "goodbye world")))))
        (kill-buffer buf)))

  (it "synchronises context seamlessly when interleaving macher-agent and macher tools"
      (let* ((proj-dir (file-name-as-directory (expand-file-name "tests/fixtures/interleave-proj")))
             (file-path (concat proj-dir "interleave.txt")))
        (make-directory proj-dir t)
        (with-temp-file file-path (insert "Initial file text"))
        (let* ((ws (cons 'directory proj-dir))
               (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
               (agent-write-fn (gptel-tool-function macher-agent-write-buffer-in-workspace-tool))
               (agent-read-fn (gptel-tool-function macher-agent-read-buffer-in-workspace-tool)))
          (spy-on 'macher-agent-resolve-context :and-return-value ctx)
          (with-macher-agent-mock-fsm ctx
                                      (funcall agent-write-fn nil "interleave.txt" "VFS Step 1: Agent Write")
                                      (macher--tool-write-file ctx "interleave.txt" "VFS Step 2: Macher Write")
                                      (let ((res (funcall agent-read-fn nil "interleave.txt")))
                                        (expect res :to-equal "VFS Step 2: Macher Write")
                                        (expect (length (macher-agent--get-context-contents ctx)) :to-equal 1)))
          (delete-file file-path)
          (delete-directory proj-dir))))

  (it "searches VFS content written by write_buffer_in_workspace using search_in_workspace"
      (let* ((ws (make-macher-agent-workspace :project-root default-directory))
             (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
             (write-fn (gptel-tool-function macher-agent-write-buffer-in-workspace-tool))
             (cmd-fn (or (get 'macher-agent-search-in-workspace-tool 'command-fn)
                         (get 'macher-agent-tool-search-in-workspace 'command-fn))))
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        (spy-on 'call-process :and-return-value 0)
        (with-macher-agent-mock-fsm ctx
                                    (funcall write-fn nil "vfs-search-target.txt" "unique-vfs-search-token line")
                                    (let ((res (funcall cmd-fn '(:pattern "unique-vfs-search-token") ctx default-directory)))
                                      (expect res :to-match "vfs-search-target\\.txt")
                                      (expect res :to-match "unique-vfs-search-token line")))))

  (it "list_available_tools filters tools to only perception, collaboration, event, execution categories and includes workspace tools"
      (let* ((global-reg (make-hash-table :test 'equal))
             (tool-perception (gptel-make-tool :name "tool-perc" :description "perception tool" :category "perception"))
             (tool-collab (gptel-make-tool :name "tool-collab" :description "collaboration tool" :category "collaboration"))
             (tool-event (gptel-make-tool :name "tool-event" :description "event tool" :category "event"))
             (tool-exec (gptel-make-tool :name "tool-exec" :description "execution tool" :category "execution"))
             (tool-other (gptel-make-tool :name "tool-other" :description "conversation tool" :category "conversation"))
             (tool-none (gptel-make-tool :name "tool-none" :description "uncategorized tool" :category "misc"))
             (tool-ws (gptel-make-tool :name "tool-workspace" :description "workspace tool" :category "perception"))
             (ws (make-macher-agent-workspace :project-root default-directory))
             (ctx (macher-agent--make-vfs-context :workspace ws :contents nil))
             (list-fn (gptel-tool-function macher-agent-list-available-tools-tool)))
        (puthash "tool-perc" tool-perception global-reg)
        (puthash "tool-collab" tool-collab global-reg)
        (puthash "tool-event" tool-event global-reg)
        (puthash "tool-exec" tool-exec global-reg)
        (puthash "tool-other" tool-other global-reg)
        (puthash "tool-none" tool-none global-reg)
        (puthash "tool-workspace" tool-ws (macher-agent-workspace-tools-registry ws))
        (let ((macher-agent-tools-registry global-reg))
          (let ((res (funcall list-fn nil ctx)))
            (expect res :to-match "tool-perc")
            (expect res :to-match "tool-collab")
            (expect res :to-match "tool-event")
            (expect res :to-match "tool-exec")
            (expect res :to-match "tool-workspace")
            (expect res :not :to-match "tool-other")
            (expect res :not :to-match "tool-none")))))

  (it "searches conversation history via glob search"
      (let ((buf (generate-new-buffer "test-conv-glob")))
        (with-current-buffer buf
          (insert "Line 1: system start\nLine 2: important keyword found\nLine 3: system end\n"))
        (let ((res (macher-agent-search-glob "keyword" buf 1)))
          (expect res :to-match "Match near line 2")
          (expect res :to-match "important keyword found"))
        (let ((res-none (macher-agent-search-glob "nonexistent" buf 1)))
          (expect res-none :to-match "No matches found in history for: nonexistent"))
        (kill-buffer buf)))

  (it "converts buffer lines into zero-mem trace plists"
      (let ((buf (generate-new-buffer "test-conv-traces")))
        (with-current-buffer buf
          (insert "First trace line\n\nSecond trace line\n"))
        (let ((traces (macher-agent--buffer-to-traces buf)))
          (expect (length traces) :to-equal 2)
          (expect (plist-get (nth 0 traces) :text) :to-equal "First trace line")
          (expect (plist-get (nth 0 traces) :timestamp) :to-equal 1.0)
          (expect (plist-get (nth 1 traces) :text) :to-equal "Second trace line")
          (expect (plist-get (nth 1 traces) :timestamp) :to-equal 3.0))
        (kill-buffer buf)))

  (it "searches conversation history via zero-mem PageRank search"
      (let ((buf (generate-new-buffer "test-conv-zeromem")))
        (with-current-buffer buf
          (insert "Trace 1: macher-agent initialization\nTrace 2: gptel-bridge configuration\nTrace 3: random text\n"))
        (let ((res (macher-agent-search-zero-mem "macher-agent" buf 2)))
          (expect res :to-match "Match near line 1")
          (expect res :to-match "macher-agent initialization"))
        (kill-buffer buf)))

  (it "defaults macher-agent-search-backend to zero-mem"
      (expect (default-value 'macher-agent-search-backend) :to-equal 'zero-mem))

  (it "dispatches search based on macher-agent-search-backend configuration"
      (let ((buf (generate-new-buffer "test-conv-dispatch"))
            (macher-agent-search-backend 'glob))
        (with-current-buffer buf
          (insert "Header text\nTarget query inside history\nFooter text\n"))
        (spy-on 'macher-agent-search-glob :and-call-through)
        (spy-on 'macher-agent-search-zero-mem :and-call-through)
        
        ;; Test glob backend
        (let ((res-glob (macher-agent-search-dispatch "query" buf 2)))
          (expect 'macher-agent-search-glob :to-have-been-called)
          (expect res-glob :to-match "Target query inside history"))
        
        ;; Test zero-mem backend
        (setq macher-agent-search-backend 'zero-mem)
        (let ((res-zm (macher-agent-search-dispatch "query" buf 2)))
          (expect 'macher-agent-search-zero-mem :to-have-been-called)
          (expect res-zm :to-match "Target query inside history"))
        
        ;; Test dead buffer error handling
        (kill-buffer buf)
        (expect (macher-agent-search-dispatch "query" buf 2) :to-match "Error: Cannot locate original conversation buffer.")))

  (it "executes search_conversation_history tool via dispatcher without inline loops or persistent global mutation"
      (let* ((buf (generate-new-buffer "test-conv-tool"))
             (ctx (macher--make-context))
             (mock-fsm (if (fboundp 'gptel-make-fsm)
                           (gptel-make-fsm :info (list :buffer buf :macher-agent-context ctx))
                         (list :buffer buf :macher-agent-context ctx)))
             (macher-agent--active-fsm mock-fsm)
             (cmd-fn (or (get 'macher-agent-search-conversation-history-tool 'command-fn)
                         (get 'macher-agent-tool-search-conversation-history 'command-fn)))
             (macher-agent-search-backend 'glob))
        (with-current-buffer buf
          (insert "Line 1: hello\nLine 2: target-match\nLine 3: world\n"))
        (spy-on 'macher-agent-search-dispatch :and-call-through)
        (let ((res (funcall cmd-fn '(:query "target-match" :context_lines 1) ctx default-directory)))
          (expect 'macher-agent-search-dispatch :to-have-been-called)
          (expect res :to-match "target-match"))
        (kill-buffer buf)))

  (it "retrieves conversation buffer from context in search_conversation_history tool"
      (let* ((buf (generate-new-buffer "test-conv-ctx"))
             (mock-fsm (if (fboundp 'gptel-make-fsm)
                           (gptel-make-fsm :info (list :buffer buf))
                         (list :buffer buf)))
             (ctx (macher-agent--make-vfs-context :workspace (make-macher-agent-workspace :project-root default-directory)
                                                  :contents nil))
             (cmd-fn (or (get 'macher-agent-search-conversation-history-tool 'command-fn)
                         (get 'macher-agent-tool-search-conversation-history 'command-fn)))
             (macher-agent-search-backend 'glob)
             (macher-agent--active-fsm nil))
        (with-current-buffer buf
          (insert "Line 1: foo\nLine 2: context-buffer-match\nLine 3: bar\n"))
        (macher-agent--set-context-data ctx :fsm mock-fsm)
        (let ((res (funcall cmd-fn '(:query "context-buffer-match" :context_lines 1) ctx default-directory)))
          (expect res :to-match "context-buffer-match"))
        (kill-buffer buf)))

  (it "handles zero-length matches and invalid-regexp safely in search_glob"
      (let ((buf (generate-new-buffer "test-conv-edge")))
        (with-current-buffer buf
          (insert "Line 1\nLine 2\n"))
        ;; Test zero-length match (e.g., "^" or ".*") does not infinite loop
        (let ((res (macher-agent-search-glob "^" buf 1)))
          (expect res :to-match "Match near line 1"))
        ;; Test invalid regexp handling
        (let ((res-err (macher-agent-search-glob "[unclosed" buf 1)))
          (expect res-err :to-match "Error: Invalid regular expression"))
        (kill-buffer buf)))

  )

 (describe "Agent Skills (macher-agent-skills.el)"
           (before-each
            (let* ((ws (make-macher-agent-workspace :project-root "/mock/proj"))
                   (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))
              (puthash (expand-file-name "/mock/proj") ctx macher-agent-active-workspaces)
              (spy-on 'macher-agent-resolve-context :and-return-value ctx)))
           
           (it "parses SKILL.md files correctly extracting frontmatter and markdown body"
               (let* ((parsed (macher-agent-parse-skill-file "tests/fixtures/skills/global/SKILL.md")))
                 (expect (plist-get parsed :name) :to-equal "mock-skill")
                 (expect (plist-get parsed :name-sym) :to-equal 'mock-skill)
                 (expect (plist-get parsed :description) :to-equal "A mock skill for testing")
                 (expect (plist-get parsed :allowed-tools) :to-equal '("mock-tool-1" "mock-tool-2"))
                 (expect (plist-get parsed :body) :to-equal "This is the system prompt for the mock skill.\nIt spans multiple lines.")))

           (it "extracts boot-directive from SKILL.md frontmatter"
               (let* ((ctx (macher-agent-resolve-context))
                      (vfs-file "tests/fixtures/skills/global/SKILL.md"))
                 (macher-agent--set-context-contents
                  ctx
                  (list (macher-agent-vfs-make-entry
                         vfs-file
                         "original"
                         "---\nname: boot-skill\ndescription: Skill with boot directive\nboot-directive: Perform initial setup before executing.\n---\nSkill body content")))
                 (let ((parsed (macher-agent-parse-skill-file vfs-file ctx)))
                   (expect (plist-get parsed :name) :to-equal "boot-skill")
                   (expect (plist-get parsed :boot-directive) :to-equal "Perform initial setup before executing."))))

           (it "persists boot-directive into gptel--known-presets and sets buffer-local macher-agent--boot-directive when applying preset"
               (let* ((ctx (macher-agent-resolve-context))
                      (workspace (macher-agent--get-context-workspace ctx)))
                 (setf (alist-get 'boot-preset (macher-agent-workspace-skills-alist workspace))
                       '(:system "System prompt" :description "Boot Preset" :boot-directive "Initial boot directive text"))
                 (macher-agent-initialize-skills ctx)
                 (let ((spec (alist-get 'boot-preset gptel--known-presets)))
                   (expect (plist-get spec :boot-directive) :to-equal "Initial boot directive text"))
                 (let ((test-buf (generate-new-buffer "test-boot-preset-buf")))
                   (with-current-buffer test-buf
                     (setq-local gptel--known-presets (copy-tree gptel--known-presets))
                     (macher-agent--apply-preset '(boot-preset))
                     (expect macher-agent--boot-directive :to-equal "Initial boot directive text"))
                   (kill-buffer test-buf))))

           (it "sets buffer-local macher-agent--boot-directive via macher-agent-use-skill, macher-agent-add-subagent, and macher-agent--apply-payload-locally"
               (let* ((ctx (macher-agent-resolve-context))
                      (workspace (macher-agent--get-context-workspace ctx)))
                 (setf (alist-get 'boot-preset (macher-agent-workspace-skills-alist workspace))
                       '(:system "System prompt" :description "Boot Preset" :boot-directive "Initial boot directive text"))
                 (macher-agent-initialize-skills ctx)
                 ;; Test macher-agent-use-skill
                 (let ((use-buf (generate-new-buffer "test-use-skill-buf")))
                   (macher-agent-use-skill 'boot-preset use-buf)
                   (with-current-buffer use-buf
                     (expect macher-agent--boot-directive :to-equal "Initial boot directive text"))
                   (kill-buffer use-buf))
                 ;; Test macher-agent-add-subagent
                 (let ((sub-buf (macher-agent-add-subagent "test-subagent-boot-buf" '(boot-preset))))
                   (with-current-buffer sub-buf
                     (expect macher-agent--boot-directive :to-equal "Initial boot directive text"))
                   (kill-buffer sub-buf))
                 ;; Test macher-agent--apply-payload-locally
                 (let ((local-buf (generate-new-buffer "test-apply-payload-buf")))
                   (with-current-buffer local-buf
                     (macher-agent--apply-payload-locally '(:boot-directive "Direct boot directive"))
                     (expect macher-agent--boot-directive :to-equal "Direct boot directive"))
                   (kill-buffer local-buf))))

           (it "prioritises virtual edits inside the VFS when parsing SKILL.md"
               (let* ((ctx (macher-agent-resolve-context))
                      (vfs-file "tests/fixtures/skills/global/SKILL.md"))
                 (macher-agent--set-context-contents 
                  ctx 
                  (list (macher-agent-vfs-make-entry 
                         vfs-file 
                         "original content on disk" 
                         "---\nname: virtual-skill\ndescription: A virtual test skill\n---\nThis body is completely virtual.")))
                 (let ((parsed (macher-agent-parse-skill-file vfs-file ctx)))
                   (expect (plist-get parsed :name) :to-equal "virtual-skill")
                   (expect (plist-get parsed :body) :to-equal "This body is completely virtual."))))

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
                 (let ((ctx (macher-agent-resolve-context)))
                   (macher-agent--load-skill-from-path "tests/fixtures/skills/workspace/" ctx)
                   (let* ((workspace (macher-agent--get-context-workspace ctx))
                          (skill-meta (alist-get 'workspace-skill (macher-agent-workspace-skills-alist workspace))))
                     (expect (plist-get skill-meta :context-dir) :to-be nil)))
                 
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
                         (list :description "test" :system "test system" :tools (list mock-tool-obj) :context-dir nil))
                   
                   (with-temp-buffer
                     (let ((gptel--known-presets nil))
                       (macher-agent-initialize-skills ctx)
                       (let ((preset-def (buffer-local-value 'gptel--known-presets (current-buffer))))
                         (setq preset-def (alist-get 'test-preset preset-def))
                         (expect preset-def :not :to-be nil)
                         (expect (plist-get preset-def :tools) :to-equal '(:append ("the_tool")))))))))

           (it "merges and applies buffer-local macher-agent-presets during composed skill evaluation"
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
                         (list :description "test" :system "test system" :tools (list mock-tool-obj) :context-dir nil))
                   
                   (with-temp-buffer
                     (let ((gptel--known-presets nil))
                       (macher-agent-initialize-skills ctx)
                       (setq-local macher-agent-presets '(test-preset))
                       (setq-local gptel-tools nil)
                       (let ((payload (macher-agent-compose-payload 
                                       (list :model nil :system nil :temperature nil :max-tokens nil :tools nil :known-presets gptel--known-presets)
                                       '(test-preset))))
                         (macher-agent--apply-payload-locally payload))
                       (expect gptel-tools :not :to-be nil)
                       (expect (car gptel-tools) :to-equal mock-tool-obj))))))

           (it "expands org-macros in SKILL.md body"
               (let* ((parsed (macher-agent-parse-skill-file "tests/fixtures/skills/macro-skill/SKILL.md")))
                 (expect (plist-get parsed :body) :to-match "Version: 0.1.0")))

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
                     (delete-directory mock-dir t)))))

           (it "stores ptc-primitives in gptel--known-presets and composes them into payload"
               (let* ((mock-dir (make-temp-file "macher-test-skills-ptc" t))
                      (skill-dir (expand-file-name "test-skill" mock-dir)))
                 (make-directory skill-dir t)
                 (with-temp-file (expand-file-name "SKILL.md" skill-dir)
                   (insert "---\nname: my-ptc-preset\ndescription: test ptc\nptc-primitives:\n  - spawn_subagent\n  - delegate_tasks\n---\nPTC preset body"))
                 (with-temp-buffer
                   (let ((gptel-directives nil)
                         (gptel--known-presets nil))
                     (macher-agent-initialize-skills nil mock-dir)
                     (let ((preset-def (alist-get 'my-ptc-preset gptel--known-presets)))
                       (expect preset-def :not :to-be nil)
                       (expect (plist-get preset-def :ptc-primitives) :to-equal '(spawn_subagent delegate_tasks))
                       (let ((composed (macher-agent-compose-payload
                                        (list :known-presets gptel--known-presets)
                                        '(my-ptc-preset))))
                         (expect (plist-get composed :ptc-primitives) :to-equal '(spawn_subagent delegate_tasks))))
                     (delete-directory mock-dir t)))))

           )

 )

(provide 'macher-agent-skills-test)
;;; macher-agent-skills-test.el ends here
