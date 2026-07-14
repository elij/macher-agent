;;; macher-agent-skills-test.el --- Tests for macher-agent-skills -*- lexical-binding: t -*-
(require 'buttercup)
(require 'macher-agent-macher-bridge)
(require 'macher-agent)

(describe "macher-agent-skills"

  (describe "Orchestration Tools (skills/scripts/*.el)"
    (before-all
      ;; Load all tool scripts to define their variables for testing
      (dolist (script (directory-files "skills/scripts" t "\\.el$"))
        (with-temp-buffer
          (insert-file-contents script)
          (let ((val nil))
            (condition-case nil
                (while t (setq val (eval (read (current-buffer)) t)))
              (end-of-file val))))))
    (it "guarantees list_buffers_in_workspace output perfectly matches context-tree buffer categorisation"
      (let* ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "*pure-buffer*" "" "")
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

        (let ((result (funcall list-tool-fn)))
          (expect result :to-match "\\*pure-buffer\\*")
          (expect result :to-match "/external/path\\.txt")
          (expect result :not :to-match "internal\\.txt"))))
    (it "properly parses a JSON string into a vector for task delegation"
      (let* ((callback-called nil)
             (callback (lambda (res) (setq callback-called res)))
             (json-tasks (vector (list :buffer_name "test-sub" :instructions "do work")))
             (tool-fn (gptel-tool-function macher-agent-delegate-tasks-to-subagents-tool))
             (buf (get-buffer-create "test-sub")))
        
        (spy-on 'macher-agent-resolve-context :and-return-value (macher--make-context))
        (spy-on 'macher-agent-execute-parallel)
        (spy-on 'macher-agent--prepare-subagent-instructions)
        (spy-on 'macher-agent--ensure-access)
        
        (funcall tool-fn json-tasks callback)
        
        (expect 'macher-agent-execute-parallel :to-have-been-called)
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
                    (run-hook-with-args 'gptel-post-response-functions (point-min) (point-max)))))

        (macher-agent-spawn-task (buffer-name buf) callback)
        
        (expect (macher-agent-tool-response-status callback-called) :to-equal 'error)
        (expect (macher-agent-tool-response-error callback-called) :to-match "stopped silently")
        (kill-buffer buf)))

    (it "correctly aggregates results from multiple event-driven sub-agents"
      (let* ((buf1 (generate-new-buffer "worker1"))
             (buf2 (generate-new-buffer "worker2"))
             (callback-called nil)
             (callback (lambda (msg) (setq callback-called msg))))
        
        (spy-on 'macher-agent-resolve-context :and-return-value (macher--make-context))
        ;; Mock the dispatcher to instantly return a success payload rather than firing the network
        (cl-letf (((symbol-function 'macher-agent-spawn-task)
                   (lambda (b cb)
                     (funcall cb (make-macher-agent-tool-response :status 'success :data (format "Output from %s" (buffer-name b)))))))
          (macher-agent-execute-parallel (list buf1 buf2) callback))
        
        (expect (length callback-called) :to-equal 2)
        (expect (macher-agent-tool-response-status (nth 0 callback-called)) :to-equal 'success)
        (expect (macher-agent-tool-response-data (nth 0 callback-called)) :to-match "Output from worker1")
        (expect (macher-agent-tool-response-data (nth 1 callback-called)) :to-match "Output from worker2")
        (kill-buffer buf1)
        (kill-buffer buf2)))

    (it "ensures target buffer exists when using write_buffer_in_workspace to support patch UI"
      (let* ((ctx (macher--make-context :contents nil))
             (tool-fn (gptel-tool-function macher-agent-write-buffer-in-workspace-tool)))
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        
        (funcall tool-fn "*new-virtual-asset*" "Ghost content")
        
        (expect (cl-find "*new-virtual-asset*" (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal) :not :to-be nil)
        (let ((contents (cl-find "*new-virtual-asset*" (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
          (expect (macher-agent-vfs-entry-curr contents) :to-equal "Ghost content"))))
    
    (it "rejects fuzzy security matching in read_buffer_in_workspace"
      (let* ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "*scratch*" "" "content"))))
             (tool-fn (gptel-tool-function macher-agent-read-buffer-in-workspace-tool)))
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        (let ((result (funcall tool-fn "scratch")))
          (expect result :to-match "SECURITY ERROR.*scratch.*"))))

    (it "submit_task_result triggers parent callback and flags completion"
      (let* ((buf (generate-new-buffer "worker-buf"))
             (tool-fn (gptel-tool-function macher-agent-submit-task-result-tool))
             (callback-data nil))
        (spy-on 'macher-agent-resolve-context :and-return-value (macher--make-context))
        (with-current-buffer buf
          (setq-local macher-agent--parent-callback (lambda (res) (setq callback-data res)))
          (funcall tool-fn "My final answer")
          (expect (macher-agent-tool-response-data callback-data) :to-equal "My final answer")
          (expect macher-agent-task-finished :to-be t))
        (kill-buffer buf)))
    
    (it "write_buffer_in_workspace registers a virtual edit safely"
      (let* ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "test-buf" "orig" "orig"))))
             (tool-fn (gptel-tool-function macher-agent-write-buffer-in-workspace-tool)))
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        
        (let* ((response (funcall tool-fn "test-buf" "New virtual content")))
          (expect response :to-match "SUCCESS")
          (expect (macher-context-dirty-p ctx) :to-be t)
          (expect (macher-agent-vfs-entry-curr (cl-find "test-buf" (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)) :to-equal "New virtual content"))))

    (it "multi_edit_buffer_in_workspace uses a decoupled deterministic scratchpad"
      (let* ((ctx (macher--make-context :contents (list (macher-agent-vfs-make-entry "test-file.rs" "line1\nline2" "line1\nline2"))))
             (tool-fn (gptel-tool-function macher-agent-multi-edit-buffer-in-workspace-tool)))
        (spy-on 'macher-agent-resolve-context :and-return-value ctx)
        
        (let* ((edits (vector (list :old_text "line2" :new_text "line3")))
               (response (funcall tool-fn "test-file.rs" edits)))
          (expect response :to-match "SUCCESS")
          
          (let ((contents (cl-find "test-file.rs" (macher-context-contents ctx) :key #'macher-agent-vfs-entry-path :test #'equal)))
            (expect (macher-agent-vfs-entry-curr contents) :to-equal "line1\nline3"))))))

  (describe "Agent Skills (macher-agent-skills.el)"
    (before-each
      (spy-on 'macher-agent-resolve-context :and-return-value
              (macher-agent--make-vfs-context :workspace (cons 'agent (make-macher-agent-workspace :project-root "/mock/proj")) :contents nil)))
    (it "parses SKILL.md files correctly extracting frontmatter and markdown body"
      (let* ((parsed (macher-agent-parse-skill-file "tests/fixtures/skills/global/SKILL.md")))
        (expect (plist-get parsed :name) :to-equal "mock-skill")
        (expect (plist-get parsed :name-sym) :to-equal 'mock-skill)
        (expect (plist-get parsed :description) :to-equal "A mock skill for testing")
        (expect (plist-get parsed :allowed-tools) :to-equal '("mock-tool-1" "mock-tool-2"))
        (expect (plist-get parsed :body) :to-equal "This is the system prompt for the mock skill.\nIt spans multiple lines.")))

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
            (delete-directory mock-dir t)))))))

(provide 'macher-agent-skills-test)
