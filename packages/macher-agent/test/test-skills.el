;;; test-skills.el --- Skill, preset, and command composition -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Specification 5: Skill, preset, and command composition
;; SKL-01, SKL-02, SKL-03, SKL-04, SKL-05, SKL-06, SKL-07, SKL-08, SKL-09, SKL-10

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-helpers)

(defun macher-agent-test--ws-files-ht (ctx)
  "Return per-context mock workspace files hash-table for CTX."
  (macher-agent-workspace-get-hash-table ctx :test-ws-files))

(defun macher-agent-test--ws-rel-path (ctx path)
  "Normalise PATH relative to CTX project root when inside workspace."
  (let ((ws-root (macher-agent-context-project-root ctx)))
    (if (and ws-root (file-name-absolute-p path))
        (file-relative-name (file-truename (expand-file-name path))
                            (file-name-as-directory (file-truename ws-root)))
      path)))

(defun macher-agent-test--ws-read-file (ctx path &optional only-staged)
  "Mock workspace storage file reader for CTX and PATH.
When ONLY-STAGED is non-nil, only return staged entries."
  (when (and (macher-agent-valid-context-p ctx) (stringp path))
    (let* ((ht (macher-agent-test--ws-files-ht ctx))
           (rel (macher-agent-test--ws-rel-path ctx path))
           (val (let ((v1 (gethash rel ht :missing)))
                  (if (eq v1 :missing)
                      (gethash path ht :missing)
                    v1))))
      (cond
       ((eq val :deleted) nil)
       ((not (eq val :missing)) val)
       (only-staged nil)
       (t
        (let* ((ws-root (macher-agent-context-project-root ctx))
               (abs (if (and ws-root (not (file-name-absolute-p path)))
                        (expand-file-name path ws-root)
                      path)))
          (when (and (stringp abs) (file-regular-p abs))
            (with-temp-buffer
              (insert-file-contents abs)
              (buffer-string)))))))))

(defun macher-agent-test--ws-list-files (ctx)
  "Mock workspace storage file lister for CTX."
  (when (macher-agent-valid-context-p ctx)
    (let ((ht (macher-agent-test--ws-files-ht ctx))
          (res nil))
      (maphash (lambda (k v)
                 (when (stringp v)
                   (push k res)))
               ht)
      (nreverse res))))

(defun macher-agent-test--stage-ws-file (ctx path content)
  "Stage CONTENT (or nil for deletion) for PATH in CTX and fire mutation hook."
  (let* ((ht (macher-agent-test--ws-files-ht ctx))
         (rel (macher-agent-test--ws-rel-path ctx path))
         (ws-root (macher-agent-context-project-root ctx))
         (abs (if (and ws-root (not (file-name-absolute-p rel)))
                  (expand-file-name rel ws-root)
                rel)))
    (if content
        (puthash rel content ht)
      (if (and (stringp abs) (file-exists-p abs))
          (puthash rel :deleted ht)
        (remhash rel ht)))
    (run-hook-with-args 'macher-agent-context-mutated-hook rel ctx)))

(describe "5. Skill, preset, and command composition"

          (before-each
           (macher-agent-test-reset-env)
           (setq macher-agent-workspace-read-file-function #'macher-agent-test--ws-read-file)
           (setq macher-agent-workspace-list-files-function #'macher-agent-test--ws-list-files))

          ;; --------------------------------------------------------------------------
          ;; SKL-01
          ;; --------------------------------------------------------------------------
          (it "SKL-01: parses new SKILL.md and tool script from VFS and registers in workspace registry and presets"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((skill-content (concat "---\n"
                                                                                  "name: dynamic-worker\n"
                                                                                  "description: Dynamic worker from VFS\n"
                                                                                  "allowed-tools:\n"
                                                                                  "  - dynamic_vfs_tool\n"
                                                                                  "---\n"
                                                                                  "Dynamic worker system instructions.\n"))
                                                           (tool-content (concat "(gptel-make-tool\n"
                                                                                 " :name \"dynamic_vfs_tool\"\n"
                                                                                 " :description \"Dynamic tool from VFS\"\n"
                                                                                 " :category \"misc\"\n"
                                                                                 " :args nil\n"
                                                                                 " :function #'ignore)\n"))
                                                           (skill-path "skills/dynamic-worker/SKILL.md")
                                                           (tool-path "skills/dynamic-worker/scripts/dynamic_vfs_tool.el"))
                                                      ;; Create SKILL.md and tool script in VFS without saving to disk
                                                      (macher-agent-test--stage-ws-file ctx skill-path skill-content)
                                                      (macher-agent-test--stage-ws-file ctx tool-path tool-content)
                                                      (expect (file-exists-p (expand-file-name skill-path root)) :to-be nil)
                                                      (expect (file-exists-p (expand-file-name tool-path root)) :to-be nil)

                                                      ;; Trigger mutation dispatcher
                                                      (macher-agent-mutation-dispatcher skill-path ctx)

                                                      ;; Tool is registered in the workspace-local registry
                                                      (let* ((tools-reg (macher-agent-workspace-tools-registry ctx))
                                                             (tool-obj (gethash "dynamic_vfs_tool" tools-reg)))
                                                        (expect tool-obj :not :to-be nil)
                                                        (expect (gptel-tool-name tool-obj) :to-equal "dynamic_vfs_tool"))

                                                      ;; Preset is published in the workspace buffer's gptel--known-presets without
                                                      ;; making gptel--known-presets buffer-local in unrelated caller buffers
                                                      (expect (local-variable-p 'gptel--known-presets) :to-be nil)
                                                      (with-current-buffer owner-buf
                                                        (let ((preset-entry (assq 'dynamic-worker gptel--known-presets)))
                                                          (expect preset-entry :not :to-be nil)
                                                          (expect (plist-get (cdr preset-entry) :description) :to-equal "Dynamic worker from VFS")
                                                          (expect (plist-get (cdr preset-entry) :system) :to-match "Dynamic worker system instructions"))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-02
          ;; --------------------------------------------------------------------------
          (it "SKL-02: composes additive skills with ### Skill: headers, merged tools, and parameter overrides"
              (let* ((tool1 (gptel-make-tool :name "tool_one" :description "Tool 1" :category "misc"))
                     (tool2 (gptel-make-tool :name "tool_two" :description "Tool 2" :category "misc"))
                     (gptel-tools (list tool1 tool2))
                     (base-state (list :system "Base instructions\n"
                                       :model 'gpt-3.5-turbo
                                       :temperature 0.7
                                       :tools nil
                                       :known-presets
                                       `((skill-alpha . (:system "Alpha instructions"
                                                                 :tools (:append (,tool1))
                                                                 :model gpt-4o
                                                                 :temperature 0.5))
                                         (skill-beta . (:system "Beta instructions"
                                                                :tools (:append (,tool2))
                                                                :model claude-3-5-sonnet
                                                                :temperature 0.2)))))
                     (composed (macher-agent-compose-payload base-state '(skill-alpha skill-beta))))

                ;; System prompts are concatenated with ### Skill: headers without duplication
                (let ((sys (plist-get composed :system)))
                  (expect sys :to-match "### Skill: skill-alpha\nAlpha instructions")
                  (expect sys :to-match "### Skill: skill-beta\nBeta instructions")
                  ;; No duplicate headers
                  (expect (cl-count "### Skill: skill-alpha" (split-string sys "\n") :test #'string=)
                          :to-equal 1))

                ;; Tool lists are merged and normalised into gptel-tool structs
                (let ((tools (plist-get composed :tools)))
                  (expect (length tools) :to-equal 2)
                  (expect (mapcar #'gptel-tool-name tools) :to-equal '("tool_one" "tool_two")))

                ;; Later model parameters override earlier ones
                (expect (plist-get composed :model) :to-equal 'claude-3-5-sonnet)
                (expect (plist-get composed :temperature) :to-equal 0.2)))

          ;; --------------------------------------------------------------------------
          ;; SKL-03
          ;; --------------------------------------------------------------------------
          (it "SKL-03: clears prior system prompt, tools, and boot directives when composing exclusive skill"
              (let* ((tool-base (gptel-make-tool :name "tool_base" :description "Base"))
                     (tool-excl (gptel-make-tool :name "tool_excl" :description "Excl"))
                     (gptel-tools (list tool-base tool-excl))
                     (base-state (list :system "Initial base prompt\n"
                                       :tools (list tool-base)
                                       :boot-directive "Setup base database\n"
                                       :model 'gpt-4o
                                       :temperature 0.7
                                       :known-presets
                                       `((base-skill . (:system "Base skill instructions"
                                                                :tools (,tool-base)
                                                                :boot-directive "Base boot"))
                                         (exclusive-skill . (:system "Strict exclusive persona"
                                                                     :exclusive t
                                                                     :tools (,tool-excl)
                                                                     :temperature 0.1)))))
                     (composed (macher-agent-compose-payload base-state '(base-skill exclusive-skill))))

                ;; Exclusive skill clears all prior prompt text and retains only its own
                (let ((sys (plist-get composed :system)))
                  (expect sys :to-match "### Skill: exclusive-skill\nStrict exclusive persona")
                  (expect sys :not :to-match "Initial base prompt")
                  (expect sys :not :to-match "Base skill instructions"))

                ;; Prior tools and boot directives are cleared
                (let ((tools (plist-get composed :tools)))
                  (expect (length tools) :to-equal 1)
                  (expect (gptel-tool-name (car tools)) :to-equal "tool_excl"))
                (expect (plist-get composed :boot-directive) :to-be nil)
                (expect (plist-get composed :temperature) :to-equal 0.1)))

          ;; --------------------------------------------------------------------------
          ;; SKL-04
          ;; --------------------------------------------------------------------------
          (it "SKL-04: strips inline @skill tags at start and after whitespace while preserving user@domain emails"
              (with-temp-buffer
                (let* ((orig-buf (current-buffer))
                       (prompt-buf (generate-new-buffer " *skl04-prompt*"))
                       (fsm (gptel-make-fsm :info (list :buffer orig-buf :data prompt-buf)))
                       (macher-agent-active-fsm fsm)
                       (coder-tool (gptel-make-tool :name "coder_tool" :description "Coder"))
                       (known-presets `((coder . (:system "You are an expert coder."
                                                          :tools (,coder-tool)))
                                        (tester . (:system "You are an expert tester.")))))
                  (unwind-protect
                      (progn
                        (setq-local gptel--known-presets known-presets)
                        (with-current-buffer prompt-buf
                          (insert "@coder Please review the patch. Send notice to dev@example.com or @tester for signoff.\n")
                          (macher-agent-sync-prompt-transformer nil fsm)
                          ;; Strips inline tags at start of line and after whitespace, leaves email untouched
                          (let ((text (buffer-string)))
                            (expect text :not :to-match "@coder")
                            (expect text :not :to-match "@tester")
                            (expect text :to-match "dev@example.com")
                            (expect text :to-match "Please review the patch. Send notice to dev@example.com or for signoff.")))
                        ;; Applies skill system prompt and tools to active FSM
                        (let ((inf (gptel-fsm-info fsm)))
                          (expect (plist-get inf :system) :to-match "You are an expert coder")
                          (expect (cl-some (lambda (tl) (equal (gptel-tool-name tl) "coder_tool"))
                                           (plist-get inf :tools))
                                  :to-be-truthy)))
                    (when (buffer-live-p prompt-buf)
                      (kill-buffer prompt-buf))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-05
          ;; --------------------------------------------------------------------------
          (it "SKL-05: interpolates markdown command arguments ($arguments, $args, $1, $2) and preserves base system prompt"
              (with-temp-buffer
                (let* ((buf (current-buffer))
                       (ctx (macher-agent-make-context :id "cmd-ctx" :project-root "/tmp/mock-proj"
                                                        :prompt "@review src/lib.rs \"check bounds\""))
                       (cmd-body "Audit file $1 focusing on $2.\nFull command args: $arguments (alias: $args)")
                       (cmd-preset (list :system cmd-body :is-command t)))
                  (setq-local macher-agent-persistent-context ctx)
                  (setq-local gptel-system-prompt "Base workspace system persona.")
                  (setq-local gptel--known-presets (list (cons 'review cmd-preset)))
                  ;; Compile transmission payload with command redirection
                  (let ((state (macher-agent--compile-transmission-payload buf nil '(review) 'review ctx)))
                    ;; User prompt text is replaced with interpolated command body
                    (let ((redirect (macher-agent-transmission-state-redirect-prompt state)))
                      (expect redirect :to-match "Audit file src/lib.rs focusing on check bounds.")
                      (expect redirect :to-match "Full command args: src/lib.rs \"check bounds\" (alias: src/lib.rs \"check bounds\")"))
                    ;; gptel-system-prompt retains base workspace system prompt
                    (expect (macher-agent-transmission-state-compiled-prompt state)
                            :to-equal "Base workspace system persona.")))))

          ;; --------------------------------------------------------------------------
          ;; SKL-06
          ;; --------------------------------------------------------------------------
          (it "SKL-06: replaces empty command arguments with empty string and leaves unfulfilled $1 literal"
              (let ((template "Command template: arguments=[$arguments], args=[$args], pos1=[$1]"))
                ;; Call with empty argument string
                (let ((interpolated-empty (macher-agent--interpolate-command-body template ""))
                      (interpolated-nil (macher-agent--interpolate-command-body template nil)))
                  ;; $arguments and $args are replaced with empty string
                  (expect interpolated-empty :to-match "arguments=\\[\\], args=\\[\\]")
                  (expect interpolated-nil :to-match "arguments=\\[\\], args=\\[\\]")
                  ;; Unfulfilled positional placeholder $1 remains literal without error
                  (expect interpolated-empty :to-match "pos1=\\[\\$1\\]")
                  (expect interpolated-nil :to-match "pos1=\\[\\$1\\]"))))

          ;; --------------------------------------------------------------------------
          ;; SKL-07
          ;; --------------------------------------------------------------------------
          (it "SKL-07: deduplicates tool blocks beyond limit and preserves deduplicated core tools on overflow"
              (with-temp-buffer
                (let* ((orig-buf (current-buffer))
                       (macher-agent-max-context-chars '((nil . 50)))
                       (macher-agent-max-duplicate-tools 2)
                       (other-tool (gptel-make-tool :name "other_tool" :description "Other"))
                       (dup-tool (gptel-make-tool :name "other-tool" :description "Other duplicate"))
                       (block "```tool (:name \"read_file\" :args (\"main.rs\"))\nfn main() {}\n```\n"))
                  ;; Insert 4 identical tool result blocks with gptel property
                  (insert "User initial request\n")
                  (dotimes (_ 4)
                    (let ((start (point)))
                      (insert block)
                      (put-text-property start (point) 'gptel t)))
                  (insert "Latest user prompt")

                  ;; 1. Deduplicate tools transformer replaces older duplicate blocks beyond limit 2
                  (macher-agent-transformer-deduplicate-tools nil nil)
                  (let ((content (buffer-string)))
                    (expect content :to-match "{\"status\": \"omitted\", \"reason\": \"duplicate\"}")
                    ;; Two recent occurrences retained, two older omitted
                    (expect (cl-count "{\"status\": \"omitted\", \"reason\": \"duplicate\"}"
                                      (split-string content "\n") :test #'string=)
                            :to-equal 2))

                  ;; 2. Standalone core overflow pipeline preserves valid tools cleanly without zero-mem
                  ;;    and core tool normalization deduplicates canonical name variants
                  (let* ((state (make-macher-agent-transmission-state
                                 :target-buffer orig-buf
                                 :tools (macher-agent-normalize-tools (list other-tool dup-tool))))
                         (updated (macher-agent-pipe--inject-dynamic-context-tools state))
                         (tool-names (mapcar #'macher-agent-canonical-tool-name
                                             (macher-agent-transmission-state-tools updated))))
                    (expect tool-names :to-equal '("other_tool"))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-08
          ;; --------------------------------------------------------------------------
          (it "SKL-08: exclusive skill with :parents retains its parent's system prompt/tools while wiping prior presets"
              (let* ((tool-prior (gptel-make-tool :name "tool_prior" :description "Prior"))
                     (tool-parent (gptel-make-tool :name "tool_parent" :description "Parent"))
                     (tool-child (gptel-make-tool :name "tool_child" :description "Child"))
                     (base-state (list :system "Prior base prompt\n"
                                       :tools (list tool-prior)
                                       :known-presets
                                       `((prior-skill . (:system "Prior skill instructions"
                                                                 :tools (,tool-prior)))
                                         (parent-skill . (:system "Parent skill instructions"
                                                                  :tools (,tool-parent)))
                                         (exclusive-child . (:system "Child exclusive persona"
                                                                     :exclusive t
                                                                     :parents (parent-skill)
                                                                     :tools (,tool-child))))))
                     (composed (macher-agent-compose-payload base-state '(prior-skill exclusive-child))))
                ;; Prior skill wiped
                (let ((sys (plist-get composed :system)))
                  (expect sys :not :to-match "Prior base prompt")
                  (expect sys :not :to-match "Prior skill instructions")
                  ;; Parent prompt retained
                  (expect sys :to-match "### Skill: parent-skill\nParent skill instructions")
                  ;; Child prompt retained
                  (expect sys :to-match "### Skill: exclusive-child\nChild exclusive persona"))
                ;; Tools: prior tool wiped, parent tool and child tool retained
                (let* ((tools (plist-get composed :tools))
                       (tool-names (mapcar #'gptel-tool-name tools)))
                  (expect (member "tool_prior" tool-names) :to-be nil)
                  (expect (member "tool_parent" tool-names) :to-be-truthy)
                  (expect (member "tool_child" tool-names) :to-be-truthy))))

          ;; --------------------------------------------------------------------------
          ;; SKL-09
          ;; --------------------------------------------------------------------------
          (it "SKL-09: single-pass command interpolation does not re-expand literal $2 inside $1's value"
              (let* ((template "Pos1: $1, Pos2: $2")
                     (user-args "$2 target-value")
                     (result (macher-agent--interpolate-command-body template user-args)))
                ;; $1 is "$2", $2 is "target-value"
                ;; In single-pass, pos1 must remain literal "$2" and not be expanded to "target-value"
                (expect result :to-equal "Pos1: $2, Pos2: target-value")))

          ;; --------------------------------------------------------------------------
          ;; SKL-10
          ;; --------------------------------------------------------------------------
          (it "SKL-10: macher-agent-secure-ast-p rejects fset, eval, advice-add, and setq-default"
              (expect (macher-agent-secure-ast-p '(fset 'foo (lambda () 1))) :to-be nil)
              (expect (macher-agent-secure-ast-p '(eval '(message "pwn"))) :to-be nil)
              (expect (macher-agent-secure-ast-p '(advice-add 'find-file :override #'ignore)) :to-be nil)
              (expect (macher-agent-secure-ast-p '(setq-default fill-column 80)) :to-be nil)
              ;; Nested inside other forms
              (expect (macher-agent-secure-ast-p '(progn (let ((x 1)) (eval x)))) :to-be nil)
              ;; Safe forms are permitted
              (expect (macher-agent-secure-ast-p '(setq foo 42)) :to-be t)
              (expect (macher-agent-secure-ast-p '(gptel-make-tool :name "test")) :to-be t))

          ;; --------------------------------------------------------------------------
          ;; SKL-11
          ;; --------------------------------------------------------------------------
          (it "SKL-11: strips gptel and text properties on yank inside macher-agent-mode"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (with-current-buffer owner-buf
                                                      (macher-agent-mode 1)
                                                      (expect (key-binding [remap yank]) :to-equal #'macher-agent-yank)
                                                      (let ((kill-ring (list (propertize "copied response snippet"
                                                                                         'gptel 'response
                                                                                         'face 'bold))))
                                                        (goto-char (point-max))
                                                        (let ((start (point)))
                                                          (macher-agent-yank)
                                                          (expect (buffer-substring-no-properties start (point))
                                                                  :to-equal "copied response snippet")
                                                          (expect (text-properties-at start) :to-be nil))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-12
          ;; --------------------------------------------------------------------------
          (it "SKL-12: executes mid-turn use_skill switch and restoration while preserving live buffer header directive"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((reader-tool (gptel-make-tool :name "reader_only_tool"
                                                                                         :description "Reader"
                                                                                         :args nil
                                                                                         :function #'ignore))
                                                           (coder-tool (gptel-make-tool :name "coder_only_tool"
                                                                                        :description "Coder"
                                                                                        :args nil
                                                                                        :function #'ignore))
                                                           (orch-sys "You are the base orchestrator.")
                                                           (coder-sys "You are the strict coder."))
                                                      (with-current-buffer owner-buf
                                                        (setq-local gptel--known-tools
                                                                    (list (cons "test" (list (cons "reader_only_tool" reader-tool)
                                                                                             (cons "coder_only_tool" coder-tool)
                                                                                             (cons "use_skill" macher-agent-use-skill-tool)))))
                                                        (setq-local gptel--known-presets
                                                                    (list (cons 'base-orch
                                                                                (list :system orch-sys
                                                                                      :description "Base orchestrator"
                                                                                      :tools '("reader_only_tool" "use_skill")))
                                                                          (cons 'strict-coder
                                                                                (list :system coder-sys
                                                                                      :description "Strict coder"
                                                                                      :boot-directive "BOOT: Coder ready."
                                                                                      :tools '("coder_only_tool")))))
                                                        (setq-local gptel-directives
                                                                    (list (cons 'base-orch orch-sys)
                                                                          (cons 'strict-coder coder-sys)))
                                                        (setq-local macher-agent-presets '(base-orch))
                                                        (macher-agent-apply-preset '(base-orch))
                                                        (expect (car-safe (rassoc gptel-system-prompt gptel-directives))
                                                                :to-equal 'base-orch)

                                                        ;; Simulate assistant turn already in buffer before use_skill runs
                                                        (let ((pt (point-max)))
                                                          (goto-char pt)
                                                          (insert "Assistant calling use_skill...\n")
                                                          (put-text-property pt (point) 'gptel 'response))

                                                        ;; Attach mock active FSM with :data
                                                        (let* ((init-data (list :model "test-model"
                                                                                :system orch-sys
                                                                                :messages (vector (list :role "user" :content "go"))))
                                                               (fsm (gptel-make-fsm
                                                                     :state 'TOOL
                                                                     :info (list :buffer owner-buf
                                                                                 :backend gptel-backend
                                                                                 :data init-data))))
                                                          (setq-local macher-agent-active-fsm fsm)

                                                          ;; 1. Activate strict-coder mid-turn
                                                          (let ((res (macher-agent-test-invoke-tool macher-agent-use-skill-tool "strict-coder" nil)))
                                                            (expect res :to-match "Activated skill 'strict-coder'")
                                                            ;; Header-line invariant: live buffer gptel-system-prompt still matches raw directive!
                                                            (expect gptel-system-prompt :to-equal coder-sys)
                                                            (expect (car-safe (rassoc gptel-system-prompt gptel-directives))
                                                                    :to-equal 'strict-coder)
                                                            ;; FSM :data received compiled prompt including boot directive despite prior assistant turn
                                                            (let* ((d (plist-get (gptel-fsm-info fsm) :data))
                                                                   (fsm-sys (plist-get d :system))
                                                                   (info-tools (mapcar #'macher-agent-canonical-tool-name
                                                                                       (plist-get (gptel-fsm-info fsm) :tools))))
                                                              (expect fsm-sys :to-match "You are the strict coder\\.")
                                                              (expect fsm-sys :to-match "BOOT: Coder ready\\.")
                                                              ;; No carry-over from base-orch, and use_skill is retained!
                                                              (expect (member "reader_only_tool" (mapcar #'macher-agent-canonical-tool-name gptel-tools))
                                                                      :to-be nil)
                                                              (expect (member "use_skill" (mapcar #'macher-agent-canonical-tool-name gptel-tools))
                                                                      :not :to-be nil)
                                                              (expect (not (null info-tools)) :to-be t))
                                                            ;; Boot directive force flag was consumed once so next turn won't repeat it
                                                            (expect macher-agent-force-boot-directive :to-be nil))

                                                          ;; 2. Exit skill via done: true
                                                          (let ((res-done (macher-agent-test-invoke-tool macher-agent-use-skill-tool "done" t)))
                                                            (expect res-done :to-match "Restored original presets")
                                                            (expect macher-agent-presets :to-equal '(base-orch))
                                                            ;; Header-line invariant restored to base-orch!
                                                            (expect gptel-system-prompt :to-equal orch-sys)
                                                            (expect (car-safe (rassoc gptel-system-prompt gptel-directives))
                                                                    :to-equal 'base-orch)
                                                            (expect (member "reader_only_tool" (mapcar #'macher-agent-canonical-tool-name gptel-tools))
                                                                    :not :to-be nil)
                                                            (expect (member "coder_only_tool" (mapcar #'macher-agent-canonical-tool-name gptel-tools))
                                                                    :to-be nil)))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-13
          ;; --------------------------------------------------------------------------
          (it "SKL-13: excludes :is-command entries from <available_skills> and rejects commands in use_skill"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (with-current-buffer owner-buf
                                                      (setq-local gptel--known-presets
                                                                  (list (cons 'real-skill (list :system "Real" :description "Valid skill"))
                                                                        (cons 'review-cmd (list :system "Cmd $1" :description "Slash cmd" :is-command t))))
                                                      (setq-local gptel-tools (list macher-agent-use-skill-tool))
                                                      (let* ((st (macher-agent--compile-transmission-payload owner-buf nil nil nil ctx))
                                                             (compiled (macher-agent-transmission-state-compiled-prompt st)))
                                                        (expect compiled :to-match "real-skill")
                                                        (expect compiled :not :to-match "review-cmd"))
                                                      (expect (macher-agent-validate-activatable-skill 'review-cmd ctx owner-buf)
                                                              :to-throw 'error)
                                                      (expect (macher-agent-test-invoke-tool macher-agent-use-skill-tool "review-cmd" nil)
                                                              :to-match "^ERROR:")
                                                      (expect (macher-agent-test-invoke-tool macher-agent-use-skill-tool "nonexistent-skill-xyz" nil)
                                                              :to-match "^ERROR:"))))

          ;; --------------------------------------------------------------------------
          ;; SKL-14
          ;; --------------------------------------------------------------------------
          (it "SKL-14: detects @command after mode prompt prefix ('### ') without erasing prior turns and ignores @tokens in code fences"
              (let ((orig-buf (generate-new-buffer "*skl-14-orig*"))
                    (wire-buf (generate-new-buffer "*skl-14-wire*")))
                (unwind-protect
                    (progn
                      (with-current-buffer orig-buf
                        (markdown-mode)
                        (setq-local gptel--known-presets
                                    (list (cons 'deploy-cmd
                                                (list :system "Deploy target=$1"
                                                      :is-command t))
                                          (cons 'decorator
                                                (list :system "Should not match inside fence")))))
                      (with-current-buffer wire-buf
                        (markdown-mode)
                        (insert "### Prior user turn\n")
                        (let ((pt (point)))
                          (insert "Prior assistant response\n")
                          (put-text-property pt (point) 'gptel 'response))
                        (insert "### @deploy-cmd staging\n```python\n@decorator\ndef f(): pass\n```")
                        (let ((fsm (gptel-make-fsm :info (list :buffer orig-buf))))
                          (macher-agent-sync-prompt-transformer #'ignore fsm)
                          (let ((txt (buffer-string)))
                            ;; Prior turns remain intact!
                            (expect txt :to-match "Prior user turn")
                            (expect txt :to-match "Prior assistant response")
                            ;; Command body interpolated after prompt prefix
                            (expect txt :to-match "Deploy target=staging")))))
                  (when (buffer-live-p orig-buf) (kill-buffer orig-buf))
                  (when (buffer-live-p wire-buf) (kill-buffer wire-buf)))))

          ;; --------------------------------------------------------------------------
          ;; SKL-15
          ;; --------------------------------------------------------------------------
          (it "SKL-15: loads bundled skills/tools in external workspaces and persists gptel--tool-names on save"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let ((saved-tools-reg (copy-hash-table macher-agent-tools-registry))
                                                          (saved-global-skills macher-agent-global-skills-alist)
                                                          (ext-bundled (make-temp-file "macher-ext-bundled-" t))
                                                          (wire-buf (generate-new-buffer "*skl-15-wire*"))
                                                          (chat-file (expand-file-name "chat-session.md" root)))
                                                      (unwind-protect
                                                          (let ((macher-agent-bundled-skills-dir ext-bundled))
                                                            ;; Populate external bundled skills/scripts directory outside `root'
                                                            (make-directory (expand-file-name "scripts" ext-bundled) t)
                                                            (make-directory (expand-file-name "ext-bundled-reader" ext-bundled) t)
                                                            (make-directory (expand-file-name "ext-bundled-coder" ext-bundled) t)
                                                            (with-temp-file (expand-file-name "scripts/ext_read_tool.el" ext-bundled)
                                                              (insert "(gptel-make-tool :name \"ext_read_tool\" :description \"External read tool\" :category \"misc\" :args nil :function #'ignore)\n"))
                                                            (with-temp-file (expand-file-name "scripts/ext_bundled_tool.el" ext-bundled)
                                                              (insert "(gptel-make-tool :name \"ext_bundled_tool\" :description \"External bundled tool\" :category \"misc\" :args nil :function #'ignore)\n"))
                                                            (with-temp-file (expand-file-name "ext-bundled-reader/SKILL.md" ext-bundled)
                                                              (insert "---\nname: ext-bundled-reader\ndescription: Bundled reader outside ws\nallowed-tools:\n  - ext_read_tool\n---\nBundled reader prompt.\n"))
                                                            (with-temp-file (expand-file-name "ext-bundled-coder/SKILL.md" ext-bundled)
                                                              (insert "---\nname: ext-bundled-coder\ndescription: Bundled coder outside ws\nallowed-tools:\n  - ext_bundled_tool\n---\nBundled coder prompt.\n"))
                                                            (clrhash macher-agent-tools-registry)
                                                            (setq macher-agent-global-skills-alist nil)
                                                            (with-current-buffer owner-buf
                                                              (markdown-mode)
                                                              (gptel-mode 1)
                                                              (setq-local buffer-file-name chat-file)
                                                              (macher-agent-refresh-skills-and-tools ctx)
                                                              ;; Bundled skills and scripts outside `root' must load from disk
                                                              (expect (gethash "ext_bundled_tool" (macher-agent-local-tools-table ctx)) :not :to-be nil)
                                                              (expect (gptel-get-tool "ext_bundled_tool") :not :to-be nil)
                                                              (expect (alist-get 'ext-bundled-coder gptel--known-presets) :not :to-be nil)
                                                              (gptel--apply-preset
                                                               'ext-bundled-reader
                                                               (lambda (sym val) (set (make-local-variable sym) val)))
                                                              ;; Saving with unmodified preset tools writes gptel--preset without system prompt or tool list
                                                              (gptel--save-state)
                                                              (let ((baseline-saved (buffer-string)))
                                                                (expect baseline-saved :to-match "gptel--preset: ext-bundled-reader")
                                                                (expect baseline-saved :not :to-match "gptel-system-prompt:")
                                                                (expect baseline-saved :not :to-match "gptel--tool-names:"))
                                                              ;; Transform a turn with an inline skill adding extra tools: wire-buf and FSM get
                                                              ;; the composed tools while owner-buf's tool count remains strictly unchanged
                                                              (let ((fsm (gptel-make-fsm :info (list :buffer owner-buf :data wire-buf))))
                                                                (with-current-buffer wire-buf
                                                                  (markdown-mode)
                                                                  (insert "### @ext-bundled-coder inspect\n")
                                                                  (macher-agent-sync-prompt-transformer #'ignore fsm)
                                                                  (expect (mapcar #'gptel-tool-name gptel-tools)
                                                                          :to-equal '("ext_read_tool" "ext_bundled_tool")))
                                                                (expect (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm) :tools))
                                                                        :to-equal '("ext_read_tool" "ext_bundled_tool")))
                                                              (expect (mapcar #'gptel-tool-name gptel-tools)
                                                                      :to-equal '("ext_read_tool"))
                                                              ;; Explicitly adding an extra tool to owner-buf persists gptel--tool-names alongside gptel--preset
                                                              (setq-local gptel-tools
                                                                          (append gptel-tools (list (gptel-get-tool "ext_bundled_tool"))))
                                                              (gptel--save-state)
                                                              (let ((saved-content (buffer-string)))
                                                                (expect saved-content :to-match "gptel--preset: ext-bundled-reader")
                                                                (expect saved-content :not :to-match "gptel-system-prompt:")
                                                                (expect saved-content :to-match "gptel--tool-names:")
                                                                (expect saved-content :to-match "ext_bundled_tool")))
                                                            (with-current-buffer owner-buf
                                                              (set-buffer-modified-p nil)
                                                              (setq-local buffer-file-name nil)))
                                                        (setq macher-agent-tools-registry saved-tools-reg
                                                              macher-agent-global-skills-alist saved-global-skills)
                                                        (when (and ext-bundled (file-directory-p ext-bundled))
                                                          (ignore-errors (delete-directory ext-bundled t)))
                                                        (when (buffer-live-p wire-buf) (kill-buffer wire-buf))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-16
          ;; --------------------------------------------------------------------------
          (it "SKL-16: persists gptel--preset instead of large gptel-system-prompt across directive selection, macher-agent-use-skill, use_skill, and file reload"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((arch-prompt (concat "# Architect Persona\n"
                                                                                (make-string 4000 ?A)
                                                                                "\nDesign systems carefully."))
                                                           (coder-prompt (concat "# Coder Persona\n"
                                                                                 (make-string 4000 ?C)
                                                                                 "\nImplement clean code."))
                                                           (ws-skills-dir (expand-file-name "skills" root))
                                                           (chat-file (expand-file-name "architect-session.md" root))
                                                           (reopened-buf nil))
                                                      (unwind-protect
                                                          (progn
                                                            ;; Create workspace-local skills and tool script on disk
                                                            (make-directory (expand-file-name "scripts" ws-skills-dir) t)
                                                            (make-directory (expand-file-name "ws-architect" ws-skills-dir) t)
                                                            (make-directory (expand-file-name "ws-coder" ws-skills-dir) t)
                                                            (with-temp-file (expand-file-name "scripts/ws_arch_tool.el" ws-skills-dir)
                                                              (insert "(gptel-make-tool :name \"ws_arch_tool\" :description \"Arch tool\" :category \"misc\" :args nil :function #'ignore)\n"))
                                                            (with-temp-file (expand-file-name "scripts/ws_code_tool.el" ws-skills-dir)
                                                              (insert "(gptel-make-tool :name \"ws_code_tool\" :description \"Code tool\" :category \"misc\" :args nil :function #'ignore)\n"))
                                                            (with-temp-file (expand-file-name "ws-architect/SKILL.md" ws-skills-dir)
                                                              (insert (format "---\nname: ws-architect\ndescription: Workspace architect\nallowed-tools:\n  - ws_arch_tool\n  - use_skill\n---\n%s\n" arch-prompt)))
                                                            (with-temp-file (expand-file-name "ws-coder/SKILL.md" ws-skills-dir)
                                                              (insert (format "---\nname: ws-coder\ndescription: Workspace coder\nallowed-tools:\n  - ws_code_tool\n---\n%s\n" coder-prompt)))

                                                            (with-current-buffer owner-buf
                                                              (markdown-mode)
                                                              (gptel-mode 1)
                                                              (macher-agent-mode 1)
                                                              (setq-local buffer-file-name chat-file)
                                                              (macher-agent-refresh-skills-and-tools macher-agent-persistent-context)

                                                              ;; 1. Selecting directive directly (gptel-directives / Agent Profile menu) without gptel--preset
                                                              (setq-local gptel--preset nil)
                                                              (setq-local gptel-system-prompt (alist-get 'ws-architect gptel-directives))
                                                              (gptel--save-state)
                                                              (expect gptel--preset :to-equal 'ws-architect)
                                                              (expect (buffer-string) :to-match "gptel--preset: ws-architect")
                                                              (expect (buffer-string) :not :to-match "gptel-system-prompt:")

                                                              ;; 2. Activating a skill via macher-agent-use-skill sets gptel--preset and omits gptel-system-prompt
                                                              (erase-buffer)
                                                              (setq-local gptel--preset nil)
                                                              (setq-local macher-agent-presets nil)
                                                              (setq-local gptel-tools nil)
                                                              (macher-agent-use-skill 'ws-architect owner-buf)
                                                              (expect gptel--preset :to-equal 'ws-architect)
                                                              (gptel--save-state)
                                                              (expect (buffer-string) :to-match "gptel--preset: ws-architect")
                                                              (expect (buffer-string) :not :to-match "gptel-system-prompt:")

                                                              ;; 3. Preset-only buffer (macher-agent-presets nil) switching via use_skill and restoring via done
                                                              (erase-buffer)
                                                              (setq-local macher-agent-presets nil)
                                                              (gptel--apply-preset
                                                               'ws-architect
                                                               (lambda (sym val) (set (make-local-variable sym) val)))
                                                              ;; Mid-skill save uses ws-coder preset instead of dumping coder-prompt
                                                              (macher-agent-test-invoke-tool macher-agent-use-skill-tool "ws-coder" nil)
                                                              (expect gptel--preset :to-equal 'ws-coder)
                                                              (gptel--save-state)
                                                              (expect (buffer-string) :to-match "gptel--preset: ws-coder")
                                                              (expect (buffer-string) :not :to-match "gptel-system-prompt:")

                                                              ;; Done restores ws-architect preset and system prompt cleanly
                                                              (macher-agent-test-invoke-tool macher-agent-use-skill-tool "done" t)
                                                              (expect gptel--preset :to-equal 'ws-architect)
                                                              (expect gptel-system-prompt :to-equal arch-prompt)
                                                              (gptel--save-state)
                                                              (expect (buffer-string) :to-match "gptel--preset: ws-architect")
                                                              (expect (buffer-string) :not :to-match "gptel-system-prompt:")

                                                              ;; 4. Write session to disk, kill buffer, reopen, and verify restoration when gptel-mode precedes macher-agent-mode
                                                              (goto-char (point-min))
                                                              (insert "### User question\n")
                                                              (let ((resp-start (point)))
                                                                (insert "Assistant architectural answer.\n")
                                                                (put-text-property resp-start (point) 'gptel 'response))
                                                              (let ((write-contents-functions nil))
                                                                (gptel--save-state)
                                                                (write-region (point-min) (point-max) chat-file nil 'silent))
                                                              (set-buffer-modified-p nil)
                                                              (setq-local buffer-file-name nil))

                                                            ;; 4a. Standard reopen from disk via hack-local-variables-hook
                                                            (setq reopened-buf (find-file-noselect chat-file))
                                                            (with-current-buffer reopened-buf
                                                              (expect gptel--preset :to-equal 'ws-architect)
                                                              (expect gptel-system-prompt :to-equal arch-prompt)
                                                              (expect (member "ws_arch_tool" (mapcar #'gptel-tool-name gptel-tools)) :not :to-be nil)
                                                              (set-buffer-modified-p nil))
                                                            (kill-buffer reopened-buf)
                                                            (setq reopened-buf nil)

                                                            ;; 4b. Reopen from disk simulating gptel-mode running before macher-agent-mode
                                                            (let ((hack-local-variables-hook
                                                                   (remq #'macher-agent-restore-local-state hack-local-variables-hook)))
                                                              (setq reopened-buf (find-file-noselect chat-file)))
                                                            (with-current-buffer reopened-buf
                                                              (let ((warning-minimum-log-level :error))
                                                                (gptel-mode 1))
                                                              (macher-agent-mode 1)
                                                              (expect gptel--preset :to-equal 'ws-architect)
                                                              (expect gptel-system-prompt :to-equal arch-prompt)
                                                              (expect (member "ws_arch_tool" (mapcar #'gptel-tool-name gptel-tools)) :not :to-be nil)
                                                              (gptel--save-state)
                                                              (expect (buffer-string) :to-match "gptel--preset: ws-architect")
                                                              (expect (buffer-string) :not :to-match "gptel-system-prompt:")
                                                              (set-buffer-modified-p nil)))
                                                        (when (buffer-live-p reopened-buf)
                                                          (with-current-buffer reopened-buf
                                                            (set-buffer-modified-p nil))
                                                          (kill-buffer reopened-buf))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-17
          ;; --------------------------------------------------------------------------
          (it "SKL-17: isolates inline @skill and @preset tool composition to temporary wire buffer without mutating main buffer"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((base-read (gptel-make-tool :name "base_read" :description "Read" :category "read" :args nil :function #'ignore))
                                                           (base-write (gptel-make-tool :name "base_write" :description "Write" :category "edit" :args nil :function #'ignore))
                                                           (extra-tool (gptel-make-tool :name "inline_extra" :description "Extra" :category "misc" :args nil :function #'ignore))
                                                           (ptc-prim-tool (gptel-make-tool :name "inline_ptc_prim" :description "PTC prim" :category "misc" :args nil :function #'ignore))
                                                           (wire-1 (generate-new-buffer "*skl-17-wire-1*"))
                                                           (wire-2 (generate-new-buffer "*skl-17-wire-2*"))
                                                           (wire-3 (generate-new-buffer "*skl-17-wire-3*"))
                                                           (wire-4 (generate-new-buffer "*skl-17-wire-4*")))
                                                      (unwind-protect
                                                          (with-current-buffer owner-buf
                                                            (markdown-mode)
                                                            (gptel-mode 1)
                                                            (macher-agent-mode 1)
                                                            ;; Hook list ends with `t' so gptel's `(nconc (remq t ...) ...)' never mutates owner-buf
                                                            (expect (car (last gptel-prompt-transform-functions)) :to-be t)
                                                            (expect (car gptel-prompt-transform-functions) :to-equal #'macher-agent-sync-prompt-transformer)

                                                            (dolist (tl (list base-read base-write extra-tool ptc-prim-tool))
                                                              (macher-agent-register-tool-in-gptel-tables tl (gptel-tool-category tl)))
                                                            (setq-local gptel--known-presets
                                                                        (append
                                                                         (list (cons 'full-editor
                                                                                     (list :system "Full editor prompt"
                                                                                           :tools '("base_read" "base_write")))
                                                                               (cons 'readonly-filter
                                                                                     (list :description "Read-only filter"
                                                                                           :tools (list :function
                                                                                                        (lambda (tools)
                                                                                                          (seq-filter
                                                                                                           (lambda (tl)
                                                                                                             (equal (gptel-tool-category tl) "read"))
                                                                                                           tools)))))
                                                                               (cons 'ptc-inline-skill
                                                                                     (list :system "Inline PTC skill"
                                                                                           :tools '("inline_extra" "inline_ptc_prim")
                                                                                           :ptc-primitives '(inline_ptc_prim)))
                                                                               (cons 'bare-exclusive
                                                                                     (list :system "No-tools exclusive persona"
                                                                                           :exclusive t)))
                                                                         gptel--known-presets))

                                                            ;; Baseline: main buffer uses `full-editor' with 2 tools
                                                            (gptel--apply-preset
                                                             'full-editor
                                                             (lambda (sym val) (set (make-local-variable sym) val)))
                                                            (expect (length gptel-tools) :to-equal 2)

                                                            ;; 1. Inline additive skill increases tools in wire-1/FSM only; owner-buf stays at 2 tools
                                                            (let ((fsm-1 (gptel-make-fsm :info (list :buffer owner-buf :data wire-1))))
                                                              (with-current-buffer wire-1
                                                                (markdown-mode)
                                                                (insert "### @ptc-inline-skill run batch\n")
                                                                (macher-agent-sync-prompt-transformer #'ignore fsm-1)
                                                                (expect (member "inline_extra" (mapcar #'gptel-tool-name gptel-tools)) :not :to-be nil)))
                                                            (expect (length gptel-tools) :to-equal 2)
                                                            (expect (mapcar #'gptel-tool-name gptel-tools) :to-equal '("base_read" "base_write"))
                                                            (expect gptel--preset :to-equal 'full-editor)
                                                            (expect gptel-system-prompt :to-equal "Full editor prompt")

                                                            ;; 2. Inline tool-reducing preset (like @macher-ro) reduces tools in wire-2/FSM to 1; owner-buf stays at 2
                                                            (let ((fsm-2 (gptel-make-fsm :info (list :buffer owner-buf :data wire-2))))
                                                              (with-current-buffer wire-2
                                                                (markdown-mode)
                                                                (insert "### @readonly-filter explain only\n")
                                                                (macher-agent-sync-prompt-transformer #'ignore fsm-2)
                                                                (expect (mapcar #'gptel-tool-name gptel-tools) :to-equal '("base_read")))
                                                              (expect (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm-2) :tools))
                                                                      :to-equal '("base_read")))
                                                            (expect (length gptel-tools) :to-equal 2)
                                                            (expect (mapcar #'gptel-tool-name gptel-tools) :to-equal '("base_read" "base_write"))

                                                            ;; 3. Inline exclusive skill with empty tools sets 0 tools in wire-3/FSM; owner-buf stays at 2
                                                            (let ((fsm-3 (gptel-make-fsm :info (list :buffer owner-buf :data wire-3))))
                                                              (with-current-buffer wire-3
                                                                (markdown-mode)
                                                                (insert "### @bare-exclusive answer in prose\n")
                                                                (macher-agent-sync-prompt-transformer #'ignore fsm-3)
                                                                (expect gptel-tools :to-be nil))
                                                              (expect (plist-get (gptel-fsm-info fsm-3) :tools) :to-be nil))
                                                            (expect (length gptel-tools) :to-equal 2)

                                                            ;; 4. Subsequent plain turn without inline prefix receives strictly the baseline 2 tools (no PTC/extra leak)
                                                            (let ((fsm-4 (gptel-make-fsm :info (list :buffer owner-buf :data wire-4))))
                                                              (with-current-buffer wire-4
                                                                (markdown-mode)
                                                                (insert "### normal follow-up turn\n")
                                                                (macher-agent-sync-prompt-transformer #'ignore fsm-4)
                                                                (expect (mapcar #'gptel-tool-name gptel-tools) :to-equal '("base_read" "base_write")))
                                                              (expect (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm-4) :tools))
                                                                      :to-equal '("base_read" "base_write"))))
                                                        (dolist (b (list wire-1 wire-2 wire-3 wire-4))
                                                          (when (buffer-live-p b) (kill-buffer b)))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-18
          ;; --------------------------------------------------------------------------
          (it "SKL-18: separates extension metadata from gptel--known-presets and supports post-creation put without gptel warnings"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((ws-skills-dir (expand-file-name "skills" root))
                                                           (ws-cmds-dir (expand-file-name "commands" root))
                                                           (chat-file (expand-file-name "restore-excl.md" root))
                                                           (wire-1 (generate-new-buffer "*skl-18-wire-1*"))
                                                           (wire-2 (generate-new-buffer "*skl-18-wire-2*"))
                                                           (reopened-buf nil)
                                                           (warnings-seen nil)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives))))
                                                      (unwind-protect
                                                          (progn
                                                            (make-directory (expand-file-name "excl-skill" ws-skills-dir) t)
                                                            (make-directory ws-cmds-dir t)
                                                            (with-temp-file (expand-file-name "excl-skill/SKILL.md" ws-skills-dir)
                                                              (insert "---\nname: skl18-excl-skill\ndescription: Exclusive skill\nexclusive: true\nboot-directive: BOOT SKL18\n---\nExclusive skill system prompt.\n"))
                                                            (with-temp-file (expand-file-name "excl-cmd.md" ws-cmds-dir)
                                                              (insert "---\nname: skl18-excl-cmd\ndescription: Exclusive command\nexclusive: true\n---\nReview $1 strictly.\n"))

                                                            ;; Elisp-defined exclusive command using gptel-make-preset + put
                                                            (gptel-make-preset 'skl18-elisp-cmd
                                                              :description "Elisp exclusive command"
                                                              :system "Audit $1 with $arguments.")
                                                            (macher-agent-set-preset-meta 'skl18-elisp-cmd :is-command t :exclusive t)

                                                            (cl-letf (((symbol-function 'display-warning)
                                                                       (lambda (type message &rest _args)
                                                                         (push (list type message) warnings-seen))))
                                                              (with-current-buffer owner-buf
                                                                (markdown-mode)
                                                                (gptel-mode 1)
                                                                (macher-agent-mode 1)
                                                                (setq-local buffer-file-name chat-file)
                                                                (macher-agent-refresh-skills-and-tools macher-agent-persistent-context)

                                                                ;; Extension keys must not pollute gptel--known-presets; commands from commands/ omitted
                                                                (let ((skill-entry (alist-get 'skl18-excl-skill gptel--known-presets)))
                                                                  (expect skill-entry :not :to-be nil)
                                                                  (expect (plist-member skill-entry :exclusive) :to-be nil)
                                                                  (expect (plist-member skill-entry :is-command) :to-be nil)
                                                                  (expect (plist-member skill-entry :boot-directive) :to-be nil)
                                                                  (expect (plist-member skill-entry :ptc-primitives) :to-be nil))
                                                                (expect (alist-get 'skl18-excl-cmd gptel--known-presets) :to-be nil)

                                                                ;; Apply exclusive skill, save session, and reopen via macher-agent--restore-saved-gptel-state
                                                                (macher-agent-use-skill 'skl18-excl-skill owner-buf)
                                                                (expect gptel--preset :to-equal 'skl18-excl-skill)
                                                                (expect macher-agent-boot-directive :to-equal "BOOT SKL18")
                                                                (gptel--save-state)
                                                                (let ((write-contents-functions nil))
                                                                  (write-region (point-min) (point-max) chat-file nil 'silent))
                                                                (set-buffer-modified-p nil)
                                                                (setq-local buffer-file-name nil))

                                                              (setq reopened-buf (find-file-noselect chat-file))
                                                              (with-current-buffer reopened-buf
                                                                (expect gptel--preset :to-equal 'skl18-excl-skill)
                                                                (expect gptel-system-prompt :to-equal "Exclusive skill system prompt.")

                                                                ;; Invoke markdown exclusive command in restored session
                                                                (let ((fsm-1 (gptel-make-fsm :info (list :buffer reopened-buf :data wire-1))))
                                                                  (with-current-buffer wire-1
                                                                    (markdown-mode)
                                                                    (insert "### @skl18-excl-cmd main.el\n")
                                                                    (macher-agent-sync-prompt-transformer #'ignore fsm-1)
                                                                    (expect (buffer-string) :to-match "Review main.el strictly\\.")
                                                                    (expect (plist-get (gptel-fsm-info fsm-1) :system)
                                                                            :to-equal "Exclusive skill system prompt.")))

                                                                ;; Invoke Elisp gptel-make-preset + put exclusive command in restored session
                                                                (let ((fsm-2 (gptel-make-fsm :info (list :buffer reopened-buf :data wire-2))))
                                                                  (with-current-buffer wire-2
                                                                    (markdown-mode)
                                                                    (insert "### @skl18-elisp-cmd lib.rs check-overflow\n")
                                                                    (macher-agent-sync-prompt-transformer #'ignore fsm-2)
                                                                    (expect (buffer-string) :to-match "Audit lib.rs with lib.rs check-overflow\\.")))

                                                                (set-buffer-modified-p nil))

                                                              ;; Zero gptel preset warnings emitted across save, restore, and command turns
                                                              (expect (cl-remove-if-not
                                                                       (lambda (w) (equal (car w) '(gptel presets)))
                                                                       warnings-seen)
                                                                      :to-be nil)))
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (put 'skl18-excl-skill 'macher-agent-preset-meta nil)
                                                        (put 'skl18-excl-cmd 'macher-agent-preset-meta nil)
                                                        (put 'skl18-elisp-cmd 'macher-agent-preset-meta nil)
                                                        (when (buffer-live-p reopened-buf)
                                                          (with-current-buffer reopened-buf (set-buffer-modified-p nil))
                                                          (kill-buffer reopened-buf))
                                                        (dolist (b (list wire-1 wire-2))
                                                          (when (buffer-live-p b) (kill-buffer b)))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-19
          ;; --------------------------------------------------------------------------
          (it "SKL-19: persists active gptel-tools on save_buffer when gptel--preset omits :tools in Markdown and Org buffers"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((ws-skills-dir (expand-file-name "skills" root))
                                                           (md-file (expand-file-name "no-tools-preset.md" root))
                                                           (org-file (expand-file-name "no-tools-preset.org" root))
                                                           (reopened-md nil)
                                                           (reopened-org nil))
                                                      (unwind-protect
                                                          (progn
                                                            (make-directory (expand-file-name "scripts" ws-skills-dir) t)
                                                            (make-directory (expand-file-name "tools-less-persona" ws-skills-dir) t)
                                                            (with-temp-file (expand-file-name "scripts/skl19_tool_a.el" ws-skills-dir)
                                                              (insert "(gptel-make-tool :name \"skl19_tool_a\" :description \"Tool A\" :category \"misc\" :args nil :function #'ignore)\n"))
                                                            (with-temp-file (expand-file-name "scripts/skl19_tool_b.el" ws-skills-dir)
                                                              (insert "(gptel-make-tool :name \"skl19_tool_b\" :description \"Tool B\" :category \"misc\" :args nil :function #'ignore)\n"))
                                                            (with-temp-file (expand-file-name "tools-less-persona/SKILL.md" ws-skills-dir)
                                                              (insert "---\nname: tools-less-persona\ndescription: Skill without allowed-tools\n---\nPersona prompt without explicit tools.\n"))

                                                            ;; 1. Markdown buffer: preset lacks :tools, user selects tools and saves via save-buffer
                                                            (with-current-buffer owner-buf
                                                              (markdown-mode)
                                                              (gptel-mode 1)
                                                              (macher-agent-mode 1)
                                                              (macher-agent-refresh-skills-and-tools macher-agent-persistent-context)
                                                              (set-visited-file-name md-file t)
                                                              (setq-local gptel--preset 'tools-less-persona)
                                                              (setq-local gptel-system-prompt "Persona prompt without explicit tools.")
                                                              (setq-local gptel-tools (list (gptel-get-tool "skl19_tool_a")
                                                                                            (gptel-get-tool "skl19_tool_b")))
                                                              (insert "### User question\n")
                                                              (let ((resp-start (point)))
                                                                (insert "Assistant reply.\n")
                                                                (put-text-property resp-start (point) 'gptel 'response))
                                                              (save-buffer)
                                                              (let ((saved-text (buffer-string)))
                                                                (expect saved-text :to-match "gptel--preset: tools-less-persona")
                                                                (expect saved-text :to-match "gptel--tool-names:")
                                                                (expect saved-text :to-match "skl19_tool_a")
                                                                (expect saved-text :to-match "skl19_tool_b"))

                                                              ;; Clearing gptel-tools removes gptel--tool-names on save
                                                              (setq-local gptel-tools nil)
                                                              (set-buffer-modified-p t)
                                                              (save-buffer)
                                                              (expect (buffer-string) :not :to-match "gptel--tool-names:")

                                                              ;; Re-enable tools and re-save for reopen verification
                                                              (setq-local gptel-tools (list (gptel-get-tool "skl19_tool_a")
                                                                                            (gptel-get-tool "skl19_tool_b")))
                                                              (set-buffer-modified-p t)
                                                              (save-buffer)
                                                              (set-buffer-modified-p nil))

                                                            (kill-buffer owner-buf)
                                                            (setq reopened-md (find-file-noselect md-file))
                                                            (with-current-buffer reopened-md
                                                              (expect gptel--preset :to-equal 'tools-less-persona)
                                                              (expect (mapcar #'gptel-tool-name gptel-tools)
                                                                      :to-equal '("skl19_tool_a" "skl19_tool_b"))
                                                              (set-buffer-modified-p nil))

                                                            ;; 2. Org buffer: preset lacks :tools, user selects tools and saves via save-buffer
                                                            (with-current-buffer (find-file-noselect org-file)
                                                              (setq reopened-org (current-buffer))
                                                              (org-mode)
                                                              (gptel-mode 1)
                                                              (macher-agent-mode 1)
                                                              (macher-agent-refresh-skills-and-tools macher-agent-persistent-context)
                                                              (setq-local gptel--preset 'tools-less-persona)
                                                              (setq-local gptel-system-prompt "Persona prompt without explicit tools.")
                                                              (setq-local gptel-tools (list (gptel-get-tool "skl19_tool_a")))
                                                              (insert "* Chat\nUser org prompt.\n")
                                                              (save-buffer)
                                                              (let ((saved-org (buffer-string)))
                                                                (expect saved-org :to-match ":GPTEL_PRESET: tools-less-persona")
                                                                (expect saved-org :to-match ":GPTEL_TOOLS: skl19_tool_a"))
                                                              (set-buffer-modified-p nil))
                                                            (kill-buffer reopened-org)
                                                            (setq reopened-org (find-file-noselect org-file))
                                                            (with-current-buffer reopened-org
                                                              (let ((debug-on-error nil)
                                                                    (warning-minimum-log-level :error))
                                                                (gptel-mode 1))
                                                              (macher-agent-mode 1)
                                                              (expect gptel--preset :to-equal 'tools-less-persona)
                                                              (expect (mapcar #'gptel-tool-name gptel-tools)
                                                                      :to-equal '("skl19_tool_a"))
                                                              (set-buffer-modified-p nil)))
                                                        (dolist (b (list reopened-md reopened-org))
                                                          (when (buffer-live-p b)
                                                            (with-current-buffer b (set-buffer-modified-p nil))
                                                            (kill-buffer b)))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-20
          ;; --------------------------------------------------------------------------
          (it "SKL-20: clears stale boot-directive and force flag when switching to a skill without :boot-directive"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((tool-a (gptel-make-tool :name "skl20_tool_a" :description "A" :category "misc" :args nil :function #'ignore))
                                                           (tool-b (gptel-make-tool :name "skl20_tool_b" :description "B" :category "misc" :args nil :function #'ignore)))
                                                      (with-current-buffer owner-buf
                                                        (dolist (tl (list tool-a tool-b))
                                                          (macher-agent-register-tool-in-gptel-tables tl "misc"))
                                                        (setq-local gptel--known-presets
                                                                    (list (cons 'skl20-boot-skill
                                                                                (list :system "Skill with boot."
                                                                                      :boot-directive "BOOT: Stale check."
                                                                                      :tools '("skl20_tool_a")))
                                                                          (cons 'skl20-plain-skill
                                                                                (list :system "Plain skill without boot."
                                                                                      :tools '("skl20_tool_b")))
                                                                          (cons 'skl20-blank-boot-skill
                                                                                (list :system "Blank boot skill."
                                                                                      :boot-directive "   "))))
                                                        ;; 1. Activate skill with :boot-directive
                                                        (macher-agent-use-skill 'skl20-boot-skill owner-buf)
                                                        (expect macher-agent-boot-directive :to-equal "BOOT: Stale check.")
                                                        (expect macher-agent-force-boot-directive :to-be t)

                                                        ;; 2. Switch to skill without :boot-directive via macher-agent-use-skill
                                                        (macher-agent-use-skill 'skl20-plain-skill owner-buf)
                                                        (expect macher-agent-boot-directive :to-be nil)
                                                        (expect macher-agent-force-boot-directive :to-be nil)
                                                        (let* ((tx (macher-agent--compile-transmission-payload owner-buf nil '(skl20-plain-skill) nil ctx))
                                                               (compiled (macher-agent-transmission-state-compiled-prompt tx)))
                                                          (expect compiled :not :to-match "BOOT: Stale check\\."))

                                                        ;; 3. Re-activate boot skill, then switch to whitespace-only :boot-directive
                                                        (macher-agent-use-skill 'skl20-boot-skill owner-buf)
                                                        (expect macher-agent-force-boot-directive :to-be t)
                                                        (macher-agent-use-skill 'skl20-blank-boot-skill owner-buf)
                                                        (expect macher-agent-force-boot-directive :to-be nil)

                                                        ;; 4. Switching via use_skill tool also clears stale boot directive
                                                        (macher-agent-test-invoke-tool macher-agent-use-skill-tool "skl20-boot-skill" nil)
                                                        (expect macher-agent-boot-directive :to-equal "BOOT: Stale check.")
                                                        (expect macher-agent-force-boot-directive :to-be t)
                                                        (macher-agent-test-invoke-tool macher-agent-use-skill-tool "skl20-plain-skill" nil)
                                                        (expect macher-agent-boot-directive :to-be nil)
                                                        (expect macher-agent-force-boot-directive :to-be nil)))))

          ;; --------------------------------------------------------------------------
          ;; SKL-21
          ;; --------------------------------------------------------------------------
          (it "SKL-21: preserves buffer-local tools for non-exclusive skills and clears them for exclusive skills"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((existing-tool (gptel-make-tool :name "skl21_existing_tool" :description "Existing" :category "misc" :args nil :function #'ignore))
                                                           (additive-tool (gptel-make-tool :name "skl21_additive_tool" :description "Additive" :category "misc" :args nil :function #'ignore))
                                                           (excl-tool (gptel-make-tool :name "skl21_excl_tool" :description "Exclusive" :category "misc" :args nil :function #'ignore)))
                                                      (with-current-buffer owner-buf
                                                        (dolist (tl (list existing-tool additive-tool excl-tool))
                                                          (macher-agent-register-tool-in-gptel-tables tl "misc"))
                                                        (setq-local gptel--known-presets
                                                                    (list (cons 'skl21-bare-additive
                                                                                (list :system "Bare additive skill."))
                                                                          (cons 'skl21-additive
                                                                                (list :system "Additive skill."
                                                                                      :tools '("skl21_additive_tool")))
                                                                          (cons 'skl21-exclusive
                                                                                (list :system "Exclusive skill."
                                                                                      :exclusive t
                                                                                      :tools '("skl21_excl_tool")))
                                                                          (cons 'skl21-bare-exclusive
                                                                                (list :system "Bare exclusive skill."
                                                                                      :exclusive t))))
                                                        ;; 1. Non-exclusive skill without tools preserves existing tools
                                                        (setq-local gptel-tools (list existing-tool))
                                                        (macher-agent-use-skill 'skl21-bare-additive owner-buf)
                                                        (expect (mapcar #'gptel-tool-name gptel-tools) :to-equal '("skl21_existing_tool"))

                                                        ;; 2. Non-exclusive skill with tools merges additively with existing ones
                                                        (macher-agent-use-skill 'skl21-additive owner-buf)
                                                        (let ((tool-names (mapcar #'gptel-tool-name gptel-tools)))
                                                          (expect (member "skl21_existing_tool" tool-names) :not :to-be nil)
                                                          (expect (member "skl21_additive_tool" tool-names) :not :to-be nil))

                                                        ;; 3. Exclusive skill clears prior tools, retaining only its own
                                                        (macher-agent-use-skill 'skl21-exclusive owner-buf)
                                                        (expect (mapcar #'gptel-tool-name gptel-tools) :to-equal '("skl21_excl_tool"))

                                                        ;; 4. Bare exclusive skill clears tools to nil
                                                        (setq-local gptel-tools (list existing-tool))
                                                        (macher-agent-use-skill 'skl21-bare-exclusive owner-buf)
                                                        (expect gptel-tools :to-be nil)))))

          ;; --------------------------------------------------------------------------
          ;; SKL-22
          ;; --------------------------------------------------------------------------
          (it "SKL-22: does not repeatedly re-run gptel--restore-state for unresolvable tools while still reconciling missing known tools"
              (macher-agent-test-with-git-workspace (root _ctx _owner-buf)
                                                    (let* ((ws-skills-dir (expand-file-name "skills" root))
                                                           (md-file (expand-file-name "missing-tools.md" root))
                                                           (org-file (expand-file-name "missing-tools.org" root))
                                                           (reopened-md nil)
                                                           (reopened-org nil))
                                                      (unwind-protect
                                                          (progn
                                                            (make-directory (expand-file-name "scripts" ws-skills-dir) t)
                                                            (with-temp-file (expand-file-name "scripts/skl22_known_a.el" ws-skills-dir)
                                                              (insert "(gptel-make-tool :name \"skl22_known_a\" :description \"Known A\" :category \"misc\" :args nil :function #'ignore)\n"))
                                                            (with-temp-file (expand-file-name "scripts/skl22_known_b.el" ws-skills-dir)
                                                              (insert "(gptel-make-tool :name \"skl22_known_b\" :description \"Known B\" :category \"misc\" :args nil :function #'ignore)\n"))

                                                            ;; 1. Markdown buffer with one known tool and one unresolvable tool
                                                            (with-temp-file md-file
                                                              (insert "### User prompt\n\n"
                                                                      "<!-- Local Variables: -->\n"
                                                                      "<!-- gptel--tool-names: (\"skl22_known_a\" \"skl22_missing_tool\") -->\n"
                                                                      "<!-- End: -->\n"))
                                                            (let ((debug-on-error nil)
                                                                  (warning-minimum-log-level :error))
                                                              (setq reopened-md (find-file-noselect md-file))
                                                              (with-current-buffer reopened-md
                                                                (gptel-mode 1)
                                                                (macher-agent-mode 1)
                                                                (expect (mapcar #'gptel-tool-name gptel-tools)
                                                                        :to-equal '("skl22_known_a"))
                                                                (expect (macher-agent--needs-preset-reconcile-p) :to-be nil)

                                                                (let ((restore-calls 0)
                                                                      (orig-restore (symbol-function 'gptel--restore-state)))
                                                                  (cl-letf (((symbol-function 'gptel--restore-state)
                                                                             (lambda ()
                                                                               (setq restore-calls (1+ restore-calls))
                                                                               (funcall orig-restore))))
                                                                    (macher-agent-setup-gptel-buffer)
                                                                    (expect restore-calls :to-equal 0)

                                                                    ;; Adding a known tool that is missing from gptel-tools triggers reconciliation
                                                                    (setq-local gptel--tool-names
                                                                                '("skl22_known_a" "skl22_known_b" "skl22_missing_tool"))
                                                                    (expect (macher-agent--needs-preset-reconcile-p) :not :to-be nil)
                                                                    (macher-agent-setup-gptel-buffer)
                                                                    (expect restore-calls :to-equal 1)
                                                                    (expect (mapcar #'gptel-tool-name gptel-tools)
                                                                            :to-equal '("skl22_known_a" "skl22_known_b"))
                                                                    (expect (macher-agent--needs-preset-reconcile-p) :to-be nil)

                                                                    ;; Subsequent setup calls do not re-run restore despite missing tool remaining
                                                                    (macher-agent-setup-gptel-buffer)
                                                                    (expect restore-calls :to-equal 1)))
                                                                (set-buffer-modified-p nil)))

                                                            ;; 2. Org buffer with GPTEL_TOOLS containing a known tool and an unresolvable tool
                                                            (with-temp-file org-file
                                                              (insert ":PROPERTIES:\n"
                                                                      ":GPTEL_TOOLS: skl22_known_a skl22_missing_tool\n"
                                                                      ":END:\n"
                                                                      "* Chat\n"
                                                                      "User org prompt.\n"))
                                                            (let ((debug-on-error nil)
                                                                  (warning-minimum-log-level :error))
                                                              (setq reopened-org (find-file-noselect org-file))
                                                              (with-current-buffer reopened-org
                                                                (gptel-mode 1)
                                                                (macher-agent-mode 1)
                                                                (expect (mapcar #'gptel-tool-name gptel-tools)
                                                                        :to-equal '("skl22_known_a"))
                                                                (expect (macher-agent--needs-preset-reconcile-p) :to-be nil)

                                                                (let ((restore-calls 0)
                                                                      (orig-restore (symbol-function 'gptel--restore-state)))
                                                                  (cl-letf (((symbol-function 'gptel--restore-state)
                                                                             (lambda ()
                                                                               (setq restore-calls (1+ restore-calls))
                                                                               (funcall orig-restore))))
                                                                    (macher-agent-setup-gptel-buffer)
                                                                    (expect restore-calls :to-equal 0)

                                                                    ;; Adding a known tool to GPTEL_TOOLS while missing from gptel-tools reconciles
                                                                    (org-entry-put (point-min) "GPTEL_TOOLS"
                                                                                   "skl22_known_a skl22_known_b skl22_missing_tool")
                                                                    (expect (macher-agent--needs-preset-reconcile-p) :not :to-be nil)
                                                                    (macher-agent-setup-gptel-buffer)
                                                                    (expect restore-calls :to-equal 1)
                                                                    (expect (mapcar #'gptel-tool-name gptel-tools)
                                                                            :to-equal '("skl22_known_a" "skl22_known_b"))
                                                                    (expect (macher-agent--needs-preset-reconcile-p) :to-be nil)

                                                                    ;; Subsequent setup calls do not re-run restore despite missing tool remaining
                                                                    (macher-agent-setup-gptel-buffer)
                                                                    (expect restore-calls :to-equal 1)))
                                                                (set-buffer-modified-p nil))))
                                                        (dolist (b (list reopened-md reopened-org))
                                                          (when (buffer-live-p b)
                                                            (with-current-buffer b (set-buffer-modified-p nil))
                                                            (kill-buffer b)))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-23
          ;; --------------------------------------------------------------------------
          (it "SKL-23: clears macher-agent-preset-meta when deleting exclusive skills or commands via VFS or initialize-skills"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((saved-global-skills macher-agent-global-skills-alist)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (skill-path "skills/skl23-excl-skill/SKILL.md")
                                                           (cmd-path "commands/skl23-excl-cmd.md")
                                                           (skill-content (concat "---\n"
                                                                                  "name: skl23-excl-skill\n"
                                                                                  "description: Exclusive skill\n"
                                                                                  "exclusive: true\n"
                                                                                  "boot-directive: BOOT SKL23\n"
                                                                                  "ptc-primitives:\n"
                                                                                  "  - read_file\n"
                                                                                  "---\n"
                                                                                  "Exclusive skill instructions.\n"))
                                                           (cmd-content (concat "---\n"
                                                                                "name: skl23-excl-cmd\n"
                                                                                "description: Exclusive command\n"
                                                                                "exclusive: true\n"
                                                                                "---\n"
                                                                                "Exclusive command body $1.\n"))
                                                           (ext-skill-dir (expand-file-name "skills/skl23-ext-dir" root))
                                                           (ext-skill-file (expand-file-name "SKILL.md" ext-skill-dir)))
                                                      (unwind-protect
                                                          (with-current-buffer owner-buf
                                                            (setq-local gptel--known-presets
                                                                        (list (cons 'skl23-base-skill
                                                                                    (list :system "Base skill instructions."))))
                                                            (setq-local gptel-directives
                                                                        (list (cons 'skl23-base-skill "Base skill instructions.")))

                                                            ;; 1. Register exclusive skill and command in VFS (triggers mutation-dispatcher via hook)
                                                            (macher-agent-test--stage-ws-file ctx skill-path skill-content)
                                                            (macher-agent-test--stage-ws-file ctx cmd-path cmd-content)

                                                            (expect (get 'skl23-excl-skill 'macher-agent-preset-meta) :not :to-be nil)
                                                            (expect (plist-get (get 'skl23-excl-skill 'macher-agent-preset-meta) :exclusive) :to-be t)
                                                            (expect (plist-get (get 'skl23-excl-skill 'macher-agent-preset-meta) :boot-directive)
                                                                    :to-equal "BOOT SKL23")
                                                            (expect (plist-get (get 'skl23-excl-skill 'macher-agent-preset-meta) :ptc-primitives)
                                                                    :to-equal '(read_file))
                                                            (expect (plist-get (get 'skl23-excl-cmd 'macher-agent-preset-meta) :exclusive) :to-be t)
                                                            (expect (plist-get (get 'skl23-excl-cmd 'macher-agent-preset-meta) :is-command) :to-be t)

                                                            (expect (plist-get (macher-agent-lookup-skill-spec 'skl23-excl-skill ctx owner-buf) :exclusive)
                                                                    :to-be t)
                                                            (expect (plist-get (macher-agent-lookup-skill-spec 'skl23-excl-cmd ctx owner-buf) :exclusive)
                                                                    :to-be t)
                                                            (expect (macher-agent--transformer-resolve-skills
                                                                     '(skl23-base-skill) '(skl23-excl-skill) gptel--known-presets ctx owner-buf)
                                                                    :to-equal '(skl23-excl-skill))
                                                            (expect (macher-agent--transformer-resolve-skills
                                                                     '(skl23-base-skill) '(skl23-excl-cmd) gptel--known-presets ctx owner-buf)
                                                                    :to-equal '(skl23-excl-cmd))

                                                            ;; 2. Delete both via VFS nil content -> clears macher-agent-preset-meta and resolves normally
                                                            (macher-agent-test--stage-ws-file ctx skill-path nil)
                                                            (macher-agent-test--stage-ws-file ctx cmd-path nil)

                                                            (expect (get 'skl23-excl-skill 'macher-agent-preset-meta) :to-be nil)
                                                            (expect (get 'skl23-excl-cmd 'macher-agent-preset-meta) :to-be nil)
                                                            (expect (macher-agent-lookup-skill-spec 'skl23-excl-skill ctx owner-buf) :to-be nil)
                                                            (expect (macher-agent-lookup-skill-spec 'skl23-excl-cmd ctx owner-buf) :to-be nil)
                                                            (expect (macher-agent--transformer-resolve-skills
                                                                     '(skl23-base-skill) '(skl23-excl-skill) gptel--known-presets ctx owner-buf)
                                                                    :to-equal '(skl23-base-skill skl23-excl-skill))
                                                            (expect (macher-agent--transformer-resolve-skills
                                                                     '(skl23-base-skill) '(skl23-excl-cmd) gptel--known-presets ctx owner-buf)
                                                                    :to-equal '(skl23-base-skill skl23-excl-cmd))

                                                            ;; 3. External deletion on disk with frontmatter name != dir name, plus preserving
                                                            ;;    non-overridden macher-agent-global-skills-alist metadata
                                                            (setq macher-agent-global-skills-alist
                                                                  (cons (cons 'skl23-global-excl
                                                                              (list :system "Global exclusive." :exclusive t))
                                                                        saved-global-skills))
                                                            (macher-agent-set-preset-meta 'skl23-global-excl :exclusive t)
                                                            (make-directory ext-skill-dir t)
                                                            (with-temp-file ext-skill-file
                                                              (insert "---\nname: skl23-ext-renamed\ndescription: Ext excl\nexclusive: true\n---\nExt prompt.\n"))
                                                            (macher-agent-refresh-skills-and-tools ctx)

                                                            (expect (plist-get (get 'skl23-ext-renamed 'macher-agent-preset-meta) :exclusive) :to-be t)
                                                            (expect (plist-get (get 'skl23-global-excl 'macher-agent-preset-meta) :exclusive) :to-be t)

                                                            ;; Remove from disk externally without calling mutation-dispatcher, then re-initialize
                                                            (delete-file ext-skill-file)
                                                            (macher-agent-initialize-skills ctx)

                                                            (expect (get 'skl23-ext-renamed 'macher-agent-preset-meta) :to-be nil)
                                                            (expect (macher-agent-lookup-skill-spec 'skl23-ext-renamed ctx owner-buf) :to-be nil)
                                                            (expect (macher-agent--transformer-resolve-skills
                                                                     '(skl23-base-skill) '(skl23-ext-renamed) gptel--known-presets ctx owner-buf)
                                                                    :to-equal '(skl23-base-skill skl23-ext-renamed))
                                                            (expect (plist-get (get 'skl23-global-excl 'macher-agent-preset-meta) :exclusive) :to-be t))
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (cl-remprop 'skl23-excl-skill 'macher-agent-preset-meta)
                                                        (cl-remprop 'skl23-excl-cmd 'macher-agent-preset-meta)
                                                        (cl-remprop 'skl23-ext-renamed 'macher-agent-preset-meta)
                                                        (cl-remprop 'skl23-global-excl 'macher-agent-preset-meta)))))

          ;; --------------------------------------------------------------------------
          ;; SKL-24
          ;; --------------------------------------------------------------------------
          (it "SKL-24: resolves workspace VFS tool overrides ahead of macher-agent-tools-registry without clobbering global registry"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((saved-tools-reg (copy-hash-table macher-agent-tools-registry))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (other-dir (macher-agent-test-create-git-repo))
                                                           (other-ctx (macher-agent-make-context
                                                                       :id "other-ws-ctx"
                                                                       :project-root other-dir
                                                                       :tools (make-hash-table :test 'equal)))
                                                           (global-top (gptel-make-tool
                                                                        :name "skl24_top_override"
                                                                        :description "Global top version"
                                                                        :category "misc"
                                                                        :args nil
                                                                        :function (lambda () "global-top")))
                                                           (global-nested (gptel-make-tool
                                                                           :name "skl24_nested_override"
                                                                           :description "Global nested version"
                                                                           :category "misc"
                                                                           :args nil
                                                                           :function (lambda () "global-nested"))))
                                                      (unwind-protect
                                                          (progn
                                                            (puthash "skl24_top_override" global-top macher-agent-tools-registry)
                                                            (puthash "skl24_nested_override" global-nested macher-agent-tools-registry)

                                                            ;; Stage workspace VFS overrides without disk files and without populating local-reg via hook
                                                            (let ((macher-agent-context-mutated-hook nil))
                                                              (macher-agent-test--stage-ws-file
                                                               ctx "skills/scripts/skl24_top_override.el"
                                                               "(gptel-make-tool :name \"skl24_top_override\" :description \"Workspace top VFS\" :category \"misc\" :args nil :function (lambda () \"ws-top\"))\n")
                                                              (macher-agent-test--stage-ws-file
                                                               ctx "skills/custom-skill/scripts/skl24_nested_override.el"
                                                               "(gptel-make-tool :name \"skl24_nested_override\" :description \"Workspace nested VFS\" :category \"misc\" :args nil :function (lambda () \"ws-nested\"))\n"))

                                                            ;; Even when macher-agent-tools-registry already holds global versions (and even when passed as tools-registry),
                                                            ;; workspace VFS overrides win and are cached only in ctx's local-reg
                                                            (let ((resolved-top (macher-agent-resolve-tool "skl24_top_override" nil nil ctx))
                                                                  (resolved-nested (macher-agent-resolve-tool "skl24_nested_override" macher-agent-tools-registry nil ctx))
                                                                  (local-reg (macher-agent-local-tools-table ctx)))
                                                              (expect (gptel-tool-p resolved-top) :to-be t)
                                                              (expect (gptel-tool-description resolved-top) :to-equal "Workspace top VFS")
                                                              (expect (funcall (gptel-tool-function resolved-top)) :to-equal "ws-top")
                                                              (expect (gethash "skl24_top_override" local-reg) :to-equal resolved-top)

                                                              (expect (gptel-tool-p resolved-nested) :to-be t)
                                                              (expect (gptel-tool-description resolved-nested) :to-equal "Workspace nested VFS")
                                                              (expect (funcall (gptel-tool-function resolved-nested)) :to-equal "ws-nested")
                                                              (expect (gethash "skl24_nested_override" local-reg) :to-equal resolved-nested))

                                                            ;; Global macher-agent-tools-registry remains untouched for other contexts
                                                            (expect (gptel-tool-description (gethash "skl24_top_override" macher-agent-tools-registry))
                                                                    :to-equal "Global top version")
                                                            (expect (gptel-tool-description (gethash "skl24_nested_override" macher-agent-tools-registry))
                                                                    :to-equal "Global nested version")
                                                            (let ((other-top (macher-agent-resolve-tool "skl24_top_override" nil nil other-ctx))
                                                                  (other-nested (macher-agent-resolve-tool "skl24_nested_override" nil nil other-ctx)))
                                                              (expect (gptel-tool-description other-top) :to-equal "Global top version")
                                                              (expect (funcall (gptel-tool-function other-top)) :to-equal "global-top")
                                                              (expect (gptel-tool-description other-nested) :to-equal "Global nested version")
                                                              (expect (funcall (gptel-tool-function other-nested)) :to-equal "global-nested")))
                                                        (setq macher-agent-tools-registry saved-tools-reg)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (when (and other-dir (file-directory-p other-dir))
                                                          (ignore-errors (delete-directory other-dir t)))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-25
          ;; --------------------------------------------------------------------------
          (it "SKL-25: discovers and loads VFS-only skills/, .skills/, commands/, and nested <skill>/scripts/*.el without disk directories"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let ((saved-global-skills macher-agent-global-skills-alist)
                                                          (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                          (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                          (orig-directives (copy-alist (default-value 'gptel-directives))))
                                                      (unwind-protect
                                                          (progn
                                                            ;; Stage files in VFS-only directories without firing mutation hook yet
                                                            (let ((macher-agent-context-mutated-hook nil))
                                                              ;; Note: skl25-skill does NOT list skl25_unlisted_tool in allowed-tools
                                                              (macher-agent-test--stage-ws-file
                                                               ctx "skills/skl25-skill/SKILL.md"
                                                               "---\nname: skl25-skill\ndescription: VFS-only skill\n---\nVFS skill prompt.\n")
                                                              (macher-agent-test--stage-ws-file
                                                               ctx "skills/skl25-skill/scripts/skl25_unlisted_tool.el"
                                                               "(gptel-make-tool :name \"skl25_unlisted_tool\" :description \"Nested VFS tool\" :category \"misc\" :args nil :function (lambda () \"unlisted\"))\n")
                                                              (macher-agent-test--stage-ws-file
                                                               ctx "skills/scripts/skl25_top_tool.el"
                                                               "(gptel-make-tool :name \"skl25_top_tool\" :description \"Top VFS tool\" :category \"misc\" :args nil :function (lambda () \"top\"))\n")
                                                              (macher-agent-test--stage-ws-file
                                                               ctx ".skills/skl25-dot-skill/SKILL.md"
                                                               "---\nname: skl25-dot-skill\ndescription: Dot-skills VFS skill\n---\nDot-skills prompt.\n")
                                                              (macher-agent-test--stage-ws-file
                                                               ctx ".skills/skl25-dot-skill/scripts/skl25_dot_tool.el"
                                                               "(gptel-make-tool :name \"skl25_dot_tool\" :description \"Dot-skills VFS tool\" :category \"misc\" :args nil :function (lambda () \"dot\"))\n")
                                                              (macher-agent-test--stage-ws-file
                                                               ctx "commands/skl25_cmd.md"
                                                               "---\nname: skl25-cmd\ndescription: VFS-only command\n---\nRun VFS command $1.\n"))

                                                            ;; Confirm none of these directories exist on physical disk
                                                            (expect (file-directory-p (expand-file-name "skills" root)) :to-be nil)
                                                            (expect (file-directory-p (expand-file-name ".skills" root)) :to-be nil)
                                                            (expect (file-directory-p (expand-file-name "commands" root)) :to-be nil)

                                                            (with-current-buffer owner-buf
                                                              (macher-agent-initialize-skills ctx)
                                                              (let ((local-reg (macher-agent-local-tools-table ctx)))
                                                                ;; All three VFS-only scripts (including nested <skill>/scripts/*.el) are loaded
                                                                (expect (gptel-tool-p (gethash "skl25_unlisted_tool" local-reg)) :to-be t)
                                                                (expect (gptel-tool-p (gethash "skl25_top_tool" local-reg)) :to-be t)
                                                                (expect (gptel-tool-p (gethash "skl25_dot_tool" local-reg)) :to-be t)
                                                                (expect (gptel-tool-description (gptel-get-tool "skl25_unlisted_tool"))
                                                                        :to-equal "Nested VFS tool")
                                                                (expect (gptel-tool-description (gptel-get-tool "skl25_top_tool"))
                                                                        :to-equal "Top VFS tool")
                                                                (expect (gptel-tool-description (gptel-get-tool "skl25_dot_tool"))
                                                                        :to-equal "Dot-skills VFS tool"))

                                                              ;; Skills from skills/ and .skills/ are registered as presets
                                                              (expect (alist-get 'skl25-skill gptel--known-presets) :not :to-be nil)
                                                              (expect (alist-get 'skl25-dot-skill gptel--known-presets) :not :to-be nil)

                                                              ;; Command from VFS-only commands/ is registered in context-skills with :is-command t
                                                              (let ((cmd-spec (alist-get 'skl25-cmd (macher-agent-context-skills ctx))))
                                                                (expect cmd-spec :not :to-be nil)
                                                                (expect (plist-get cmd-spec :is-command) :to-be t)
                                                                (expect (alist-get 'skl25-cmd gptel--known-presets) :to-be nil))))
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (cl-remprop 'skl25-skill 'macher-agent-preset-meta)
                                                        (cl-remprop 'skl25-dot-skill 'macher-agent-preset-meta)
                                                        (cl-remprop 'skl25-cmd 'macher-agent-preset-meta)))))

          ;; --------------------------------------------------------------------------
          ;; SKL-26
          ;; --------------------------------------------------------------------------
          (it "SKL-26: reloads edited VFS tool scripts into local-reg and gptel--known-tools via mutation-dispatcher and removes on deletion"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let ((orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                          (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                          (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                          (nested-path "skills/skl26-skill/scripts/skl26_hot_tool.el")
                                                          (direct-path "scripts/skl26_direct_tool.el"))
                                                      (unwind-protect
                                                          (progn
                                                            (with-current-buffer owner-buf
                                                              (markdown-mode)
                                                              (gptel-mode 1))

                                                            ;; 1. Create nested VFS script (hook fires macher-agent-mutation-dispatcher)
                                                            (macher-agent-test--stage-ws-file
                                                             ctx nested-path
                                                             "(gptel-make-tool :name \"skl26_hot_tool\" :description \"Hot tool v1\" :category \"misc\" :args nil :function (lambda () \"v1\"))\n")
                                                            (let ((local-reg (macher-agent-local-tools-table ctx)))
                                                              (expect (gptel-tool-description (gethash "skl26_hot_tool" local-reg)) :to-equal "Hot tool v1")
                                                              (expect (funcall (gptel-tool-function (gethash "skl26_hot_tool" local-reg))) :to-equal "v1")
                                                              (with-current-buffer owner-buf
                                                                (expect (gptel-tool-description (gptel-get-tool "skl26_hot_tool")) :to-equal "Hot tool v1")
                                                                (expect (funcall (gptel-tool-function (gptel-get-tool "skl26_hot_tool"))) :to-equal "v1")))

                                                            ;; 2. Edit nested VFS script -> immediately reloads new implementation into local-reg and gptel--known-tools
                                                            (macher-agent-test--stage-ws-file
                                                             ctx nested-path
                                                             "(gptel-make-tool :name \"skl26_hot_tool\" :description \"Hot tool v2\" :category \"misc\" :args nil :function (lambda () \"v2\"))\n")
                                                            (let ((local-reg (macher-agent-local-tools-table ctx)))
                                                              (expect (gptel-tool-description (gethash "skl26_hot_tool" local-reg)) :to-equal "Hot tool v2")
                                                              (expect (funcall (gptel-tool-function (gethash "skl26_hot_tool" local-reg))) :to-equal "v2")
                                                              (with-current-buffer owner-buf
                                                                (expect (gptel-tool-description (gptel-get-tool "skl26_hot_tool")) :to-equal "Hot tool v2")
                                                                (expect (funcall (gptel-tool-function (gptel-get-tool "skl26_hot_tool"))) :to-equal "v2")))

                                                            ;; 3. Create & edit direct scripts/<name>.el (outside skills/) -> reloads cleanly
                                                            (macher-agent-test--stage-ws-file
                                                             ctx direct-path
                                                             "(gptel-make-tool :name \"skl26_direct_tool\" :description \"Direct v1\" :category \"misc\" :args nil :function (lambda () \"d1\"))\n")
                                                            (macher-agent-test--stage-ws-file
                                                             ctx direct-path
                                                             "(gptel-make-tool :name \"skl26_direct_tool\" :description \"Direct v2\" :category \"misc\" :args nil :function (lambda () \"d2\"))\n")
                                                            (let ((local-reg (macher-agent-local-tools-table ctx)))
                                                              (expect (gptel-tool-description (gethash "skl26_direct_tool" local-reg)) :to-equal "Direct v2")
                                                              (with-current-buffer owner-buf
                                                                (expect (gptel-tool-description (gptel-get-tool "skl26_direct_tool")) :to-equal "Direct v2")))

                                                            ;; 4. Delete VFS scripts -> removes from local-reg and gptel--known-tools
                                                            (macher-agent-test--stage-ws-file ctx nested-path nil)
                                                            (macher-agent-test--stage-ws-file ctx direct-path nil)
                                                            (let ((local-reg (macher-agent-local-tools-table ctx)))
                                                              (expect (gethash "skl26_hot_tool" local-reg) :to-be nil)
                                                              (expect (gethash 'skl26_hot_tool local-reg) :to-be nil)
                                                              (expect (gethash "skl26-hot-tool" local-reg) :to-be nil)
                                                              (expect (gethash "skl26_direct_tool" local-reg) :to-be nil)
                                                              (with-current-buffer owner-buf
                                                                (expect (ignore-errors (gptel-get-tool "skl26_hot_tool")) :to-be nil)
                                                                (expect (ignore-errors (gptel-get-tool "skl26_direct_tool")) :to-be nil))))
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (set-default 'gptel--known-tools orig-known-tools)))))

          ;; --------------------------------------------------------------------------
          ;; SKL-27
          ;; --------------------------------------------------------------------------
          (it "SKL-27: reloads VFS-edited tool scripts and updates active FSM :tools and :data mid-turn before DONE"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((saved-global-skills macher-agent-global-skills-alist)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (tool-rel "skills/skl27-skill/scripts/skl27_tool.el")
                                                           (skill-rel "skills/skl27-skill/SKILL.md")
                                                           (tool-abs (expand-file-name tool-rel root))
                                                           (skill-abs (expand-file-name skill-rel root)))
                                                      (unwind-protect
                                                          (progn
                                                            (make-directory (file-name-directory tool-abs) t)
                                                            (with-temp-file tool-abs
                                                              (insert "(gptel-make-tool :name \"skl27_tool\" :description \"SKL27 v1\" :category \"misc\" :args nil :function (lambda () \"v1-result\"))\n"))
                                                            (with-temp-file skill-abs
                                                              (insert "---\nname: skl27-skill\ndescription: SKL27 skill\nallowed-tools:\n  - skl27_tool\n---\nSKL27 persona v1.\n"))

                                                            (with-current-buffer owner-buf
                                                              (markdown-mode)
                                                              (gptel-mode 1)
                                                              (macher-agent-mode 1)
                                                              (macher-agent-refresh-skills-and-tools ctx)
                                                              (gptel--apply-preset
                                                               'skl27-skill
                                                               (lambda (sym val) (set (make-local-variable sym) val)))

                                                              (let* ((backend (or gptel-backend
                                                                                  (bound-and-true-p gptel--openai)
                                                                                  (gptel-make-openai "skl27-openai" :stream nil)))
                                                                     (init-data (list :model "test-model"
                                                                                      :system "SKL27 persona v1."
                                                                                      :tools (gptel--parse-tools backend gptel-tools)
                                                                                      :messages (vector (list :role "user" :content "run"))))
                                                                     (wait-entered 0)
                                                                     (fsm (gptel-make-fsm
                                                                           :state 'TOOL
                                                                           :table (copy-alist gptel-send--transitions)
                                                                           :handlers (list (cons 'TRET (list #'gptel--handle-tool-result))
                                                                                           (cons 'WAIT (list (lambda (_f) (setq wait-entered (1+ wait-entered))))))
                                                                           :info (list :buffer owner-buf
                                                                                       :backend backend
                                                                                       :macher-agent-context ctx
                                                                                       :tools (copy-sequence gptel-tools)
                                                                                       :data init-data
                                                                                       :callback #'ignore
                                                                                       :tool-use (list (list :id "call_1"
                                                                                                             :name "edit_file"
                                                                                                             :result "ok"))
                                                                                       :tool-result (list (list "call_1" "edit_file" "ok"))))))
                                                                (setq-local gptel-backend backend)
                                                                (setq-local macher-agent-active-fsm fsm)

                                                                ;; Mutate tool script in VFS mid-turn while FSM is in TOOL state
                                                                (macher-agent-test--stage-ws-file
                                                                 ctx tool-rel
                                                                 "(gptel-make-tool :name \"skl27_tool\" :description \"SKL27 v2\" :category \"misc\" :args nil :function (lambda () \"v2-result\"))\n")

                                                                ;; Active FSM's :tools and :data are updated before TRET -> WAIT
                                                                (let* ((info (gptel-fsm-info fsm))
                                                                       (fsm-tool (cl-find-if (lambda (tl) (equal (gptel-tool-name tl) "skl27_tool"))
                                                                                             (plist-get info :tools)))
                                                                       (data-tools (plist-get (plist-get info :data) :tools))
                                                                       (data-str (prin1-to-string data-tools)))
                                                                  (expect fsm-tool :not :to-be nil)
                                                                  (expect (gptel-tool-description fsm-tool) :to-equal "SKL27 v2")
                                                                  (expect (funcall (gptel-tool-function fsm-tool)) :to-equal "v2-result")
                                                                  (expect data-str :to-match "SKL27 v2"))

                                                                ;; Advance TOOL -> TRET -> WAIT (before DONE) and execute next tool call
                                                                (gptel--fsm-transition fsm)
                                                                (expect (gptel-fsm-state fsm) :to-equal 'WAIT)
                                                                (expect wait-entered :to-equal 1)
                                                                (let* ((next-call (list :id "call_2" :name "skl27_tool" :args nil))
                                                                       (inf (plist-put (copy-sequence (gptel-fsm-info fsm))
                                                                                       :tool-use (list next-call))))
                                                                  (setf (gptel-fsm-info fsm) inf)
                                                                  (setf (gptel-fsm-state fsm) 'TOOL)
                                                                  (cl-letf (((symbol-function 'gptel--fsm-transition) #'ignore))
                                                                    (gptel--handle-tool-use fsm))
                                                                  (expect (plist-get next-call :result) :to-equal "v2-result")))))
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (cl-remprop 'skl27-skill 'macher-agent-preset-meta)))))

          ;; --------------------------------------------------------------------------
          ;; SKL-29
          ;; --------------------------------------------------------------------------
          (it "SKL-29: refreshes updated SKILL.md and tool scripts per turn across gptel--preset and buffer-local gptel-tools"
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((saved-global-skills macher-agent-global-skills-alist)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (ws-skills-dir (expand-file-name "skills" root))
                                                           (skill-file (expand-file-name "skl29-skill/SKILL.md" ws-skills-dir))
                                                           (bare-file (expand-file-name "skl29-bare/SKILL.md" ws-skills-dir))
                                                           (tool1-file (expand-file-name "skl29-skill/scripts/skl29_tool.el" ws-skills-dir))
                                                           (tool2-file (expand-file-name "skl29-skill/scripts/skl29_tool_two.el" ws-skills-dir))
                                                           (wire-1 (generate-new-buffer "*skl-29-wire-1*"))
                                                           (wire-2 (generate-new-buffer "*skl-29-wire-2*")))
                                                      (unwind-protect
                                                          (progn
                                                            (make-directory (file-name-directory tool1-file) t)
                                                            (make-directory (file-name-directory bare-file) t)
                                                            (with-temp-file tool1-file
                                                              (insert "(gptel-make-tool :name \"skl29_tool\" :description \"Tool v1\" :category \"misc\" :args nil :function (lambda () \"tool-v1\"))\n"))
                                                            (with-temp-file skill-file
                                                              (insert "---\nname: skl29-skill\ndescription: SKL29 skill\nmodel: gpt-4o\nallowed-tools:\n  - skl29_tool\n---\nPersona v1.\n"))
                                                            (with-temp-file bare-file
                                                              (insert "---\nname: skl29-bare\ndescription: SKL29 bare\n---\nBare persona.\n"))

                                                            (with-current-buffer owner-buf
                                                              (markdown-mode)
                                                              (gptel-mode 1)
                                                              (macher-agent-mode 1)
                                                              (macher-agent-refresh-skills-and-tools ctx)

                                                              ;; 1. Apply gptel--preset with macher-agent-presets nil
                                                              (setq-local macher-agent-presets nil)
                                                              (gptel--apply-preset
                                                               'skl29-skill
                                                               (lambda (sym val) (set (make-local-variable sym) val)))
                                                              (expect gptel-model :to-equal 'gpt-4o)
                                                              (expect gptel-system-prompt :to-equal "Persona v1.")

                                                              ;; Edit SKILL.md and tool scripts directly on disk between turns
                                                              (with-temp-file tool1-file
                                                                (insert "(gptel-make-tool :name \"skl29_tool\" :description \"Tool v2\" :category \"misc\" :args nil :function (lambda () \"tool-v2\"))\n"))
                                                              (with-temp-file tool2-file
                                                                (insert "(gptel-make-tool :name \"skl29_tool_two\" :description \"Tool two\" :category \"misc\" :args nil :function (lambda () \"two\"))\n"))
                                                              (with-temp-file skill-file
                                                                (insert "---\nname: skl29-skill\ndescription: SKL29 skill\nmodel: claude-3-5-sonnet\nallowed-tools:\n  - skl29_tool\n  - skl29_tool_two\n---\nPersona v2.\n"))

                                                              (let ((fsm-1 (gptel-make-fsm :info (list :buffer owner-buf :data wire-1))))
                                                                (with-current-buffer wire-1
                                                                  (markdown-mode)
                                                                  (insert "### Next turn prompt\n")
                                                                  (macher-agent-sync-prompt-transformer #'ignore fsm-1))
                                                                (let* ((inf (gptel-fsm-info fsm-1))
                                                                       (tools (plist-get inf :tools))
                                                                       (tl1 (cl-find-if (lambda (tl) (equal (gptel-tool-name tl) "skl29_tool")) tools))
                                                                       (tl2 (cl-find-if (lambda (tl) (equal (gptel-tool-name tl) "skl29_tool_two")) tools)))
                                                                  (expect (plist-get inf :model) :to-equal 'claude-3-5-sonnet)
                                                                  (expect (plist-get inf :system) :to-match "Persona v2\\.")
                                                                  (expect tl1 :not :to-be nil)
                                                                  (expect (gptel-tool-description tl1) :to-equal "Tool v2")
                                                                  (expect (funcall (gptel-tool-function tl1)) :to-equal "tool-v2")
                                                                  (expect tl2 :not :to-be nil)))

                                                              ;; 2. Preset omitting :tools (SKL-19) preserves buffer-local gptel-tools and refreshes tool closure
                                                              (gptel--apply-preset
                                                               'skl29-bare
                                                               (lambda (sym val) (set (make-local-variable sym) val)))
                                                              (setq-local gptel-tools (list (gptel-get-tool "skl29_tool")))
                                                              (let ((macher-agent-context-mutated-hook nil))
                                                                (macher-agent-test--stage-ws-file
                                                                 ctx "skills/skl29-skill/scripts/skl29_tool.el"
                                                                 "(gptel-make-tool :name \"skl29_tool\" :description \"Tool v3\" :category \"misc\" :args nil :function (lambda () \"tool-v3\"))\n"))
                                                              (let ((fsm-2 (gptel-make-fsm :info (list :buffer owner-buf :data wire-2))))
                                                                (with-current-buffer wire-2
                                                                  (markdown-mode)
                                                                  (insert "### Bare preset turn\n")
                                                                  (macher-agent-sync-prompt-transformer #'ignore fsm-2))
                                                                (let* ((tools (plist-get (gptel-fsm-info fsm-2) :tools))
                                                                       (tl1 (cl-find-if (lambda (tl) (equal (gptel-tool-name tl) "skl29_tool")) tools)))
                                                                  (expect (length tools) :to-equal 1)
                                                                  (expect (gptel-tool-description tl1) :to-equal "Tool v3")
                                                                  (expect (funcall (gptel-tool-function tl1)) :to-equal "tool-v3")))

                                                              ;; 3. Explicit local suppression (setq-local gptel-tools nil) keeps tools nil even with gptel--preset
                                                              (setq-local gptel--preset 'skl29-skill)
                                                              (setq-local gptel-tools nil)
                                                              (let* ((st (make-macher-agent-transmission-state
                                                                          :target-buffer owner-buf
                                                                          :context ctx))
                                                                     (hydrated (macher-agent-pipe--hydrate-base-state st)))
                                                                (expect (macher-agent-transmission-state-tools hydrated) :to-be nil))))
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (cl-remprop 'skl29-skill 'macher-agent-preset-meta)
                                                        (cl-remprop 'skl29-bare 'macher-agent-preset-meta)
                                                        (dolist (b (list wire-1 wire-2))
                                                          (when (buffer-live-p b) (kill-buffer b)))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-30
          ;; --------------------------------------------------------------------------
          (it "SKL-30: resolves cross-backend models and propagates :backend per-turn, via VFS edit mid-turn, and via use_skill"
              (macher-agent-test-with-git-workspace (_root ctx owner-buf)
                                                    (let* ((saved-global-skills macher-agent-global-skills-alist)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (gptel--known-backends nil)
                                                           (openai (gptel-make-openai "TestOpenAI" :models '(gpt-4o gpt-4o-mini) :stream nil))
                                                           (anthropic (gptel-make-anthropic "TestAnthropic" :models '(claude-3-5-sonnet claude-3-7-sonnet) :stream nil))
                                                           (gemini (gptel-make-gemini "TestGemini" :models '(gemini-2.0-flash gemini-2.5-pro) :stream nil))
                                                           (wire-1 (generate-new-buffer "*skl-30-wire-1*")))
                                                      (unwind-protect
                                                          (progn
                                                            (macher-agent-test--stage-ws-file
                                                             ctx "skills/skl30-base/SKILL.md"
                                                             "---\nname: skl30-base\ndescription: Base skill\nallowed-tools:\n  - use_skill\n---\nBase orchestrator persona.\n")
                                                            (macher-agent-test--stage-ws-file
                                                             ctx "skills/skl30-anthropic/SKILL.md"
                                                             "---\nname: skl30-anthropic\ndescription: Anthropic model skill\nmodel: claude-3-5-sonnet\n---\nAnthropic persona.\n")
                                                            (macher-agent-test--stage-ws-file
                                                             ctx "skills/skl30-gemini/SKILL.md"
                                                             "---\nname: skl30-gemini\ndescription: Explicit Gemini skill\nbackend: TestGemini\nmodel: gemini-2.5-pro\n---\nGemini persona.\n")

                                                            (with-current-buffer owner-buf
                                                              (markdown-mode)
                                                              (gptel-mode 1)
                                                              (macher-agent-mode 1)
                                                              (macher-agent-refresh-skills-and-tools ctx)

                                                              ;; Explicit backend: in SKILL.md is published into gptel--known-presets
                                                              (let ((gem-preset (alist-get 'skl30-gemini gptel--known-presets)))
                                                                (expect gem-preset :not :to-be nil)
                                                                (expect (plist-get gem-preset :backend) :to-equal "TestGemini")
                                                                (expect (plist-get gem-preset :model) :to-equal 'gemini-2.5-pro))

                                                              ;; 1. Per-turn resolution: model-only cross-backend and inline explicit backend
                                                              (setq-local gptel-backend openai)
                                                              (setq-local gptel-model 'gpt-4o)
                                                              (setq-local gptel--preset 'skl30-anthropic)
                                                              (setq-local macher-agent-presets nil)
                                                              (let ((tx (macher-agent--compile-transmission-payload owner-buf nil nil nil ctx)))
                                                                (expect (macher-agent-transmission-state-model tx) :to-equal 'claude-3-5-sonnet)
                                                                (expect (macher-agent-transmission-state-backend tx) :to-equal anthropic))

                                                              (let ((fsm-1 (gptel-make-fsm :info (list :buffer owner-buf :data wire-1))))
                                                                (with-current-buffer wire-1
                                                                  (markdown-mode)
                                                                  (insert "### @skl30-gemini analyze\n")
                                                                  (macher-agent-sync-prompt-transformer #'ignore fsm-1)
                                                                  (expect gptel-model :to-equal 'gemini-2.5-pro)
                                                                  (expect gptel-backend :to-equal gemini))
                                                                (let ((inf (gptel-fsm-info fsm-1)))
                                                                  (expect (plist-get inf :model) :to-equal 'gemini-2.5-pro)
                                                                  (expect (plist-get inf :backend) :to-equal gemini))
                                                                ;; owner-buf remains isolated from inline wire-1 skill
                                                                (expect gptel-backend :to-equal openai)
                                                                (expect gptel-model :to-equal 'gpt-4o))

                                                              ;; 2. Mid-turn VFS edit of SKILL.md (Anthropic -> Gemini via model: alone)
                                                              (setq-local gptel-backend anthropic)
                                                              (setq-local gptel-model 'claude-3-5-sonnet)
                                                              (setq-local gptel--preset 'skl30-anthropic)
                                                              (setq-local macher-agent-presets nil)
                                                              (let* ((init-data (let ((gptel-backend anthropic)
                                                                                      (gptel-model 'claude-3-5-sonnet)
                                                                                      (gptel-system-prompt "Anthropic persona.")
                                                                                      (gptel-tools gptel-tools)
                                                                                      (gptel-use-tools nil))
                                                                                  (gptel--request-data
                                                                                   anthropic
                                                                                   (list (list :role "user" :content "go")))))
                                                                     (fsm-2 (gptel-make-fsm
                                                                             :state 'TOOL
                                                                             :info (list :buffer owner-buf
                                                                                         :backend anthropic
                                                                                         :model 'claude-3-5-sonnet
                                                                                         :macher-agent-context ctx
                                                                                         :data init-data))))
                                                                (setq-local macher-agent-active-fsm fsm-2)
                                                                (macher-agent-test--stage-ws-file
                                                                 ctx "skills/skl30-anthropic/SKILL.md"
                                                                 "---\nname: skl30-anthropic\ndescription: Switched to Gemini model\nmodel: gemini-2.5-pro\n---\nSwitched Gemini persona.\n")
                                                                (expect gptel-model :to-equal 'gemini-2.5-pro)
                                                                (expect gptel-backend :to-equal gemini)
                                                                (let* ((inf (gptel-fsm-info fsm-2))
                                                                       (data (plist-get inf :data)))
                                                                  (expect (plist-get inf :model) :to-equal 'gemini-2.5-pro)
                                                                  (expect (plist-get inf :backend) :to-equal gemini)
                                                                  (expect (plist-get inf :url) :to-match "gemini-2\\.5-pro:generateContent")
                                                                  (expect (plist-member data :contents) :not :to-be nil)
                                                                  (expect (plist-member data :systemInstruction) :not :to-be nil)
                                                                  (expect (plist-member data :messages) :to-be nil))
                                                                (setq-local macher-agent-active-fsm nil))

                                                              ;; 3. Mid-turn use_skill switch (OpenAI -> Anthropic) and done restoration (Anthropic -> OpenAI)
                                                              (setq-local gptel-backend openai)
                                                              (setq-local gptel-model 'gpt-4o)
                                                              (setq-local macher-agent-presets '(skl30-base))
                                                              (macher-agent-apply-preset '(skl30-base))
                                                              (let* ((oai-data (let ((gptel-backend openai)
                                                                                     (gptel-model 'gpt-4o)
                                                                                     (gptel-system-prompt "Base orchestrator persona.")
                                                                                     (gptel-tools gptel-tools)
                                                                                     (gptel-use-tools t))
                                                                                 (gptel--request-data
                                                                                  openai
                                                                                  (list (list :role "user" :content "build")))))
                                                                     (fsm-3 (gptel-make-fsm
                                                                             :state 'TOOL
                                                                             :info (list :buffer owner-buf
                                                                                         :backend openai
                                                                                         :model 'gpt-4o
                                                                                         :macher-agent-context ctx
                                                                                         :tools (copy-sequence gptel-tools)
                                                                                         :data oai-data))))
                                                                (setq-local macher-agent-active-fsm fsm-3)
                                                                ;; Restore skl30-anthropic back to claude-3-5-sonnet
                                                                (macher-agent-test--stage-ws-file
                                                                 ctx "skills/skl30-anthropic/SKILL.md"
                                                                 "---\nname: skl30-anthropic\ndescription: Anthropic model skill\nmodel: claude-3-5-sonnet\n---\nAnthropic persona.\n")

                                                                (macher-agent-test-invoke-tool macher-agent-use-skill-tool "skl30-anthropic" nil)
                                                                (expect gptel-model :to-equal 'claude-3-5-sonnet)
                                                                (expect gptel-backend :to-equal anthropic)
                                                                (let* ((inf (gptel-fsm-info fsm-3))
                                                                       (data (plist-get inf :data)))
                                                                  (expect (plist-get inf :model) :to-equal 'claude-3-5-sonnet)
                                                                  (expect (plist-get inf :backend) :to-equal anthropic)
                                                                  (expect (plist-get data :model) :to-equal "claude-3-5-sonnet")
                                                                  (expect (plist-get data :system) :to-match "Anthropic persona\\.")
                                                                  (expect (plist-member data :messages) :not :to-be nil)
                                                                  (expect (plist-member data :input) :to-be nil))

                                                                ;; Exit skill via done: true -> restores OpenAI Responses backend, model, and request data
                                                                (macher-agent-test-invoke-tool macher-agent-use-skill-tool "done" t)
                                                                (expect gptel-model :to-equal 'gpt-4o)
                                                                (expect gptel-backend :to-equal openai)
                                                                (let* ((inf (gptel-fsm-info fsm-3))
                                                                       (data (plist-get inf :data)))
                                                                  (expect (plist-get inf :model) :to-equal 'gpt-4o)
                                                                  (expect (plist-get inf :backend) :to-equal openai)
                                                                  (expect (plist-get data :model) :to-equal "gpt-4o")
                                                                  (expect (plist-member data :system) :to-be nil)
                                                                  (expect (plist-member data :messages) :to-be nil)
                                                                  (expect (plist-member data :input) :not :to-be nil)
                                                                  (expect (plist-get data :instructions) :to-match "Base orchestrator persona\\."))
                                                                (setq-local macher-agent-active-fsm nil))))
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (dolist (sym '(skl30-base skl30-anthropic skl30-gemini))
                                                          (cl-remprop sym 'macher-agent-preset-meta))
                                                        (when (buffer-live-p wire-1) (kill-buffer wire-1))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-31
          ;; --------------------------------------------------------------------------
          (it "SKL-31: preserves current backend when gptel--known-backends is nil or current backend supports the model"
              (macher-agent-test-with-git-workspace (_root ctx owner-buf)
                                                    (let* ((saved-global-skills macher-agent-global-skills-alist)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (gptel--known-backends nil)
                                                           ;; `gptel-make-openai' prepends to `gptel--known-backends' via `setf alist-get',
                                                           ;; so register SecondOpenAI first to place FirstOpenAI earlier in the alist.
                                                           (openai-second (gptel-make-openai "SecondOpenAI" :models '(gpt-4o gpt-4o-mini) :stream nil))
                                                           (openai-first (gptel-make-openai "FirstOpenAI" :models '(gpt-4o gpt-4o-mini) :stream nil)))
                                                      (unwind-protect
                                                          (with-current-buffer owner-buf
                                                            (markdown-mode)
                                                            (gptel-mode 1)
                                                            (macher-agent-mode 1)

                                                            ;; 1. When gptel--known-backends is nil, model-only specs preserve current backend
                                                            (let ((gptel--known-backends nil))
                                                              (setq-local gptel-backend openai-second)
                                                              (setq-local gptel-model 'gpt-4o)
                                                              (let* ((base-state (list :model 'gpt-4o
                                                                                       :backend openai-second
                                                                                       :known-presets '((m-only . (:system "M" :model claude-3-5-sonnet)))))
                                                                     (composed (macher-agent-compose-payload base-state '(m-only))))
                                                                (expect (plist-get composed :model) :to-equal 'claude-3-5-sonnet)
                                                                (expect (plist-get composed :backend) :to-equal openai-second)))

                                                            ;; 2. When current backend already supports the requested model, keep current backend
                                                            ;;    even though FirstOpenAI appears earlier in gptel--known-backends
                                                            (expect (cdar (last gptel--known-backends)) :to-equal openai-second)
                                                            (expect (cdar gptel--known-backends) :to-equal openai-first)
                                                            (setq-local gptel-backend openai-second)
                                                            (setq-local gptel-model 'gpt-4o)
                                                            (setq-local gptel--known-presets
                                                                        '((same-provider-model . (:system "Same provider" :model gpt-4o-mini))
                                                                          (invalid-backend . (:system "Bad backend" :backend "MissingProvider"))))
                                                            (macher-agent-apply-preset '(same-provider-model))
                                                            (expect gptel-model :to-equal 'gpt-4o-mini)
                                                            (expect gptel-backend :to-equal openai-second)

                                                            ;; 3. Unresolvable backend: in SKILL.md does not pollute gptel--known-presets or clobber gptel-backend
                                                            (macher-agent-test--stage-ws-file
                                                             ctx "skills/skl31-bad-backend/SKILL.md"
                                                             "---\nname: skl31-bad-backend\ndescription: Unresolvable backend\nbackend: MissingProvider\n---\nFallback prompt.\n")
                                                            (macher-agent-refresh-skills-and-tools ctx)
                                                            (let ((entry (alist-get 'skl31-bad-backend gptel--known-presets)))
                                                              (expect entry :not :to-be nil)
                                                              (expect (plist-member entry :backend) :to-be nil))
                                                            (macher-agent-apply-preset '(skl31-bad-backend))
                                                            (expect gptel-backend :to-equal openai-second))
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (cl-remprop 'skl31-bad-backend 'macher-agent-preset-meta)))))

          ;; --------------------------------------------------------------------------
          ;; SKL-32
          ;; --------------------------------------------------------------------------
          (it "SKL-32: converts tool calls and request data across gptel-openai-responses, gptel-openai, and gptel-anthropic"
              (with-temp-buffer
                (let* ((chat-buf (current-buffer))
                       (gptel--known-backends nil)
                       (oai-resp (gptel-make-openai "RespOpenAI" :models '(gpt-4o) :stream nil))
                       (oai-chat (gptel-make-openai "ChatOpenAI" :host "localhost" :models '(gpt-4o) :stream nil))
                       (anthropic (gptel-make-anthropic "TestAnthropic32" :models '(claude-3-5-sonnet) :stream nil)))
                  (setq-local gptel-track-response t)
                  (gptel--parse-list-and-insert
                   '((prompt . "run tool")
                     (tool . (:id "call_1" :name "use_skill" :args (:skill_name "helper") :result "Activated"))))
                  (let* ((resp-data (let ((gptel-backend oai-resp)
                                          (gptel-model 'gpt-4o)
                                          (gptel-system-prompt "Base persona")
                                          (gptel-use-tools nil))
                                      (goto-char (point-max))
                                      (gptel--request-data oai-resp (gptel--parse-buffer oai-resp nil))))
                         (fsm (gptel-make-fsm :state 'TOOL
                                              :info (list :buffer chat-buf
                                                          :backend oai-resp
                                                          :model 'gpt-4o
                                                          :data resp-data))))
                    ;; Responses API -> Anthropic
                    (macher-agent--sync-fsm-request-data fsm "Anthropic sys" nil 'claude-3-5-sonnet anthropic oai-resp)
                    (let* ((ant-data (plist-get (gptel-fsm-info fsm) :data))
                           (ant-msgs (plist-get ant-data :messages)))
                      (expect (plist-member ant-data :messages) :not :to-be nil)
                      (expect (plist-member ant-data :input) :to-be nil)
                      (expect (plist-get ant-data :system) :to-equal "Anthropic sys")
                      (expect (length ant-msgs) :to-equal 3)
                      (expect (plist-get (aref (plist-get (aref ant-msgs 1) :content) 0) :type) :to-equal "tool_use")
                      (expect (plist-get (aref (plist-get (aref ant-msgs 2) :content) 0) :type) :to-equal "tool_result"))

                    ;; Anthropic -> OpenAI Chat Completions (:messages)
                    (macher-agent--sync-fsm-request-data fsm "Chat sys" nil 'gpt-4o oai-chat anthropic)
                    (let* ((chat-data (plist-get (gptel-fsm-info fsm) :data))
                           (chat-msgs (plist-get chat-data :messages)))
                      (expect (plist-member chat-data :messages) :not :to-be nil)
                      (expect (plist-member chat-data :input) :to-be nil)
                      (expect (plist-member chat-data :system) :to-be nil)
                      (expect (plist-get (aref chat-msgs 0) :role) :to-equal "system")
                      (expect (plist-get (aref chat-msgs 2) :tool_calls) :not :to-be nil)
                      (expect (plist-get (aref chat-msgs 3) :role) :to-equal "tool"))

                    ;; OpenAI Chat Completions -> OpenAI Responses API (:input + :instructions)
                    (macher-agent--sync-fsm-request-data fsm "Resp sys" nil 'gpt-4o oai-resp oai-chat)
                    (let* ((back-data (plist-get (gptel-fsm-info fsm) :data))
                           (back-input (plist-get back-data :input)))
                      (expect (plist-member back-data :input) :not :to-be nil)
                      (expect (plist-member back-data :messages) :to-be nil)
                      (expect (plist-get back-data :instructions) :to-equal "Resp sys")
                      (expect (length back-input) :to-equal 3)
                      (expect (plist-get (aref back-input 1) :type) :to-equal "function_call")
                      (expect (plist-get (aref back-input 2) :type) :to-equal "function_call_output"))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-33
          ;; --------------------------------------------------------------------------
          (it "SKL-33: deleting a workspace skill from a non-local helper buffer does not mutate global gptel--known-presets or gptel-directives"
              (macher-agent-test-with-git-workspace (_root ctx-a owner-a)
                                                    (let* ((saved-global-skills macher-agent-global-skills-alist)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (sentinel-preset (list :system "Global sentinel prompt."))
                                                           (sentinel-dir "Global sentinel prompt.")
                                                           (ws-b-dir (macher-agent-test-create-git-repo))
                                                           (buf-b (generate-new-buffer "*skl33-ws-b*"))
                                                           (ctx-b nil)
                                                           (helper-buf (generate-new-buffer "*skl33-helper*")))
                                                      (unwind-protect
                                                          (progn
                                                            (set-default 'gptel--known-presets
                                                                         (cons (cons 'skl33-shared-name sentinel-preset)
                                                                               orig-known-presets))
                                                            (set-default 'gptel-directives
                                                                         (cons (cons 'skl33-shared-name sentinel-dir)
                                                                               orig-directives))

                                                            ;; Set up Workspace A with a VFS skill sharing the sentinel symbol name
                                                            (with-current-buffer owner-a
                                                              (setq-local gptel--known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                              (setq-local gptel-directives (copy-alist (default-value 'gptel-directives))))
                                                            (macher-agent-test--stage-ws-file
                                                             ctx-a "skills/skl33-shared-name/SKILL.md"
                                                             "---\nname: skl33-shared-name\ndescription: WS A override\n---\nWS A prompt.\n")
                                                            (macher-agent-test--stage-ws-file
                                                             ctx-a "skills/skl33-ws-only/SKILL.md"
                                                             "---\nname: skl33-ws-only\ndescription: WS A only\n---\nWS A only prompt.\n")

                                                            ;; Set up Workspace B buffer with its own persistent context
                                                            (with-current-buffer buf-b
                                                              (setq-local default-directory ws-b-dir)
                                                              (setq ctx-b (macher-agent-make-context
                                                                           :project-root ws-b-dir
                                                                           :origin-buffer buf-b
                                                                           :tools (make-hash-table :test 'equal)))
                                                              (setf (macher-agent-context-owner-buffer ctx-b) buf-b)
                                                              (setq-local macher-agent-persistent-context ctx-b)
                                                              (setq-local gptel--known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                              (setq-local gptel-directives (copy-alist (default-value 'gptel-directives))))

                                                            ;; Delete Workspace A skills from a non-local helper buffer (neither variable is buffer-local)
                                                            (with-current-buffer helper-buf
                                                              (expect (local-variable-p 'gptel--known-presets) :to-be nil)
                                                              (expect (local-variable-p 'gptel-directives) :to-be nil)
                                                              (macher-agent-test--stage-ws-file ctx-a "skills/skl33-shared-name/SKILL.md" nil)
                                                              (macher-agent-mutation-dispatcher "skills/skl33-shared-name/SKILL.md" ctx-a)
                                                              (macher-agent-test--stage-ws-file ctx-a "skills/skl33-ws-only/SKILL.md" nil)
                                                              (macher-agent-initialize-skills ctx-a)
                                                              (expect (local-variable-p 'gptel--known-presets) :to-be nil)
                                                              (expect (local-variable-p 'gptel-directives) :to-be nil))

                                                            ;; Global default gptel--known-presets and gptel-directives remain untouched
                                                            (expect (alist-get 'skl33-shared-name (default-value 'gptel--known-presets))
                                                                    :to-equal sentinel-preset)
                                                            (expect (alist-get 'skl33-shared-name (default-value 'gptel-directives))
                                                                    :to-equal sentinel-dir)
                                                            (expect (alist-get 'skl33-ws-only (default-value 'gptel--known-presets))
                                                                    :to-be nil)
                                                            (expect (alist-get 'skl33-ws-only (default-value 'gptel-directives))
                                                                    :to-be nil)

                                                            ;; Calling initialize-skills for ctx-a while inside Workspace B's buffer does not pollute Workspace B
                                                            (macher-agent-test--stage-ws-file
                                                             ctx-a "skills/skl33-ws-only/SKILL.md"
                                                             "---\nname: skl33-ws-only\ndescription: WS A only\n---\nWS A only prompt.\n")
                                                            (with-current-buffer buf-b
                                                              (macher-agent-initialize-skills ctx-a)
                                                              (expect (alist-get 'skl33-ws-only gptel--known-presets) :to-be nil)
                                                              (expect (alist-get 'skl33-shared-name gptel--known-presets) :to-equal sentinel-preset))

                                                            ;; Workspace A's owner buffer reflects the local skill
                                                            (with-current-buffer owner-a
                                                              (expect (alist-get 'skl33-ws-only gptel--known-presets) :not :to-be nil)))
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (cl-remprop 'skl33-shared-name 'macher-agent-preset-meta)
                                                        (cl-remprop 'skl33-ws-only 'macher-agent-preset-meta)
                                                        (dolist (b (list buf-b helper-buf))
                                                          (when (buffer-live-p b) (kill-buffer b)))
                                                        (when (and ws-b-dir (file-directory-p ws-b-dir))
                                                          (ignore-errors (delete-directory ws-b-dir t)))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-34
          ;; --------------------------------------------------------------------------
          (it "SKL-34: isolates workspace tool script setq macher-agent-*-tool mutations and prefers local-reg in transmission/recompile"
              (macher-agent-test-with-git-workspace (_root ctx-a owner-a)
                                                    (let* ((saved-tools-reg (copy-hash-table macher-agent-tools-registry))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (orig-use-skill (and (boundp 'macher-agent-use-skill-tool)
                                                                                (symbol-value 'macher-agent-use-skill-tool)))
                                                           (orig-submit-task (and (boundp 'macher-agent-submit-task-result-tool)
                                                                                  (symbol-value 'macher-agent-submit-task-result-tool)))
                                                           (ws-b-dir (macher-agent-test-create-git-repo))
                                                           (buf-b (generate-new-buffer "*skl34-ws-b*"))
                                                           (ctx-b nil))
                                                      (unwind-protect
                                                          (progn
                                                            (with-current-buffer buf-b
                                                              (setq-local default-directory ws-b-dir)
                                                              (setq ctx-b (macher-agent-make-context
                                                                           :project-root ws-b-dir
                                                                           :origin-buffer buf-b
                                                                           :tools (make-hash-table :test 'equal)))
                                                              (setf (macher-agent-context-owner-buffer ctx-b) buf-b)
                                                              (setq-local macher-agent-persistent-context ctx-b))

                                                            ;; Stage workspace overrides in Workspace A that execute (setq macher-agent-*-tool ...)
                                                            (let ((macher-agent-context-mutated-hook nil))
                                                              (macher-agent-test--stage-ws-file
                                                               ctx-a "skills/scripts/use_skill.el"
                                                               (concat "(setq macher-agent-use-skill-tool\n"
                                                                       "      (gptel-make-tool\n"
                                                                       "       :name \"use_skill\"\n"
                                                                       "       :description \"WS-A overridden use_skill\"\n"
                                                                       "       :category \"meta\"\n"
                                                                       "       :args nil\n"
                                                                       "       :function (lambda () \"ws-a-use-skill\")))\n"))
                                                              (macher-agent-test--stage-ws-file
                                                               ctx-a "skills/scripts/submit_task_result.el"
                                                               (concat "(setq macher-agent-submit-task-result-tool\n"
                                                                       "      (gptel-make-tool\n"
                                                                       "       :name \"submit_task_result\"\n"
                                                                       "       :description \"WS-A overridden submit_task_result\"\n"
                                                                       "       :category \"meta\"\n"
                                                                       "       :args nil\n"
                                                                       "       :function (lambda () \"ws-a-submit\")))\n"))
                                                              (macher-agent-test--stage-ws-file
                                                               ctx-a "skills/scripts/skl34_custom_tool.el"
                                                               (concat "(setq macher-agent-skl34-custom-tool-tool\n"
                                                                       "      (gptel-make-tool\n"
                                                                       "       :name \"skl34_custom_tool\"\n"
                                                                       "       :description \"WS-A custom tool\"\n"
                                                                       "       :category \"misc\"\n"
                                                                       "       :args nil\n"
                                                                       "       :function (lambda () \"ws-a-custom\")))\n")))

                                                            ;; Resolving in Workspace A populates ctx-a local-reg without clobbering global vars
                                                            (let ((ws-a-us (macher-agent-resolve-tool "use_skill" nil nil ctx-a))
                                                                  (ws-a-sub (macher-agent-resolve-tool "submit_task_result" nil nil ctx-a))
                                                                  (ws-a-cust (macher-agent-resolve-tool "skl34_custom_tool" nil nil ctx-a)))
                                                              (expect (gptel-tool-p ws-a-us) :to-be t)
                                                              (expect (gptel-tool-description ws-a-us) :to-equal "WS-A overridden use_skill")
                                                              (expect (gptel-tool-description ws-a-sub) :to-equal "WS-A overridden submit_task_result")
                                                              (expect (gptel-tool-description ws-a-cust) :to-equal "WS-A custom tool")
                                                              (expect (gethash "use_skill" (macher-agent-local-tools-table ctx-a)) :to-equal ws-a-us)
                                                              (expect (gethash "submit_task_result" (macher-agent-local-tools-table ctx-a)) :to-equal ws-a-sub)
                                                              (expect (gethash "skl34_custom_tool" (macher-agent-local-tools-table ctx-a)) :to-equal ws-a-cust))

                                                            ;; Global tool variables remain untouched
                                                            (expect (symbol-value 'macher-agent-use-skill-tool) :to-be orig-use-skill)
                                                            (expect (symbol-value 'macher-agent-submit-task-result-tool) :to-be orig-submit-task)
                                                            (expect (boundp 'macher-agent-skl34-custom-tool-tool) :to-be nil)

                                                            ;; Workspace B resolves the pristine global tools, not Workspace A's overrides
                                                            (let ((ws-b-us (macher-agent-resolve-tool "use_skill" nil nil ctx-b))
                                                                  (ws-b-sub (macher-agent-resolve-tool "submit_task_result" nil nil ctx-b)))
                                                              (expect (gptel-tool-description ws-b-us) :not :to-equal "WS-A overridden use_skill")
                                                              (expect (gptel-tool-description ws-b-sub) :not :to-equal "WS-A overridden submit_task_result"))

                                                            ;; Pipeline steps and mid-turn recompile prefer ctx-a local-reg over global symbol-value
                                                            (with-current-buffer owner-a
                                                              (setq-local macher-agent-cached-presets-set t)
                                                              (setq-local macher-agent-current-task-id "skl34-task")
                                                              (setq-local gptel-tools nil)
                                                              (let* ((st (make-macher-agent-transmission-state
                                                                          :target-buffer owner-a
                                                                          :context ctx-a
                                                                          :tools nil))
                                                                     (_ (macher-agent-pipe--auto-retain-use-skill st))
                                                                     (_ (macher-agent-pipe--auto-enable-task-tool st))
                                                                     (st-tools (macher-agent-transmission-state-tools st))
                                                                     (st-us (cl-find-if (lambda (tl) (equal (gptel-tool-name tl) "use_skill")) st-tools))
                                                                     (st-sub (cl-find-if (lambda (tl) (equal (gptel-tool-name tl) "submit_task_result")) st-tools)))
                                                                (expect (gptel-tool-description st-us) :to-equal "WS-A overridden use_skill")
                                                                (expect (gptel-tool-description st-sub) :to-equal "WS-A overridden submit_task_result"))

                                                              (macher-agent-recompile-and-sync-buffer-turn owner-a ctx-a)
                                                              (let ((recompiled-us (cl-find-if (lambda (tl) (equal (gptel-tool-name tl) "use_skill")) gptel-tools)))
                                                                (expect recompiled-us :not :to-be nil)
                                                                (expect (gptel-tool-description recompiled-us) :to-equal "WS-A overridden use_skill")))

                                                            ;; Workspace B's pipeline steps resolve the global tools
                                                            (with-current-buffer buf-b
                                                              (setq-local macher-agent-cached-presets-set t)
                                                              (setq-local macher-agent-current-task-id "skl34-task-b")
                                                              (setq-local gptel-tools nil)
                                                              (let* ((st-b (make-macher-agent-transmission-state
                                                                            :target-buffer buf-b
                                                                            :context ctx-b
                                                                            :tools nil))
                                                                     (_ (macher-agent-pipe--auto-retain-use-skill st-b))
                                                                     (_ (macher-agent-pipe--auto-enable-task-tool st-b))
                                                                     (b-tools (macher-agent-transmission-state-tools st-b))
                                                                     (b-us (cl-find-if (lambda (tl) (equal (gptel-tool-name tl) "use_skill")) b-tools))
                                                                     (b-sub (cl-find-if (lambda (tl) (equal (gptel-tool-name tl) "submit_task_result")) b-tools)))
                                                                (expect (gptel-tool-description b-us) :not :to-equal "WS-A overridden use_skill")
                                                                (expect (gptel-tool-description b-sub) :not :to-equal "WS-A overridden submit_task_result"))))
                                                        (setq macher-agent-tools-registry saved-tools-reg)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (when orig-use-skill
                                                          (setq macher-agent-use-skill-tool orig-use-skill))
                                                        (when orig-submit-task
                                                          (setq macher-agent-submit-task-result-tool orig-submit-task))
                                                        (when (boundp 'macher-agent-skl34-custom-tool-tool)
                                                          (makunbound 'macher-agent-skl34-custom-tool-tool))
                                                        (when (buffer-live-p buf-b) (kill-buffer buf-b))
                                                        (when (and ws-b-dir (file-directory-p ws-b-dir))
                                                          (ignore-errors (delete-directory ws-b-dir t)))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-35
          ;; --------------------------------------------------------------------------
          (it "SKL-35: VFS-only exclusive skill in Workspace A does not bleed :exclusive or :is-command into Workspace B"
              (macher-agent-test-with-git-workspace (_root ctx-a owner-a)
                                                    (let* ((saved-global-skills macher-agent-global-skills-alist)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (ws-b-dir (macher-agent-test-create-git-repo))
                                                           (buf-b (generate-new-buffer "*skl35-ws-b*"))
                                                           (wire-b (generate-new-buffer "*skl35-wire-b*"))
                                                           (ctx-b nil)
                                                           (b-base-tool (gptel-make-tool
                                                                         :name "skl35_b_base"
                                                                         :description "B base tool"
                                                                         :category "misc"
                                                                         :args nil
                                                                         :function #'ignore))
                                                           (b-inline-tool (gptel-make-tool
                                                                           :name "skl35-shared"
                                                                           :description "B inline tool sharing skill name"
                                                                           :category "misc"
                                                                           :args nil
                                                                           :function #'ignore)))
                                                      (unwind-protect
                                                          (progn
                                                            ;; 1. Workspace A registers a VFS-only :exclusive t (and :is-command/:ptc-primitives) skill
                                                            (macher-agent-test--stage-ws-file
                                                             ctx-a "skills/skl35-shared/SKILL.md"
                                                             (concat "---\n"
                                                                     "name: skl35-shared\n"
                                                                     "description: WS A exclusive skill\n"
                                                                     "exclusive: true\n"
                                                                     "ptc-primitives:\n"
                                                                     "  - read_file\n"
                                                                     "---\n"
                                                                     "WS A exclusive prompt.\n"))
                                                            (with-current-buffer owner-a
                                                              (macher-agent-refresh-skills-and-tools ctx-a))
                                                            (expect (plist-get (get 'skl35-shared 'macher-agent-preset-meta) :exclusive) :to-be t)

                                                            ;; 2. Workspace B has NO skl35-shared skill; instead skl35-shared is an inline tool
                                                            (with-current-buffer buf-b
                                                              (setq-local default-directory ws-b-dir)
                                                              (setq ctx-b (macher-agent-make-context
                                                                           :project-root ws-b-dir
                                                                           :origin-buffer buf-b
                                                                           :tools (make-hash-table :test 'equal)))
                                                              (setf (macher-agent-context-owner-buffer ctx-b) buf-b)
                                                              (setq-local macher-agent-persistent-context ctx-b)
                                                              (setq-local gptel--known-presets
                                                                          (list (cons 'skl35-b-base-skill
                                                                                      (list :system "Workspace B base skill."
                                                                                            :tools '("skl35_b_base")))))
                                                              (setq-local gptel-directives
                                                                          (list (cons 'skl35-b-base-skill "Workspace B base skill.")))
                                                              (setq-local gptel--known-tools
                                                                          (list (cons "misc" (list (cons "skl35_b_base" b-base-tool)
                                                                                                   (cons "skl35-shared" b-inline-tool)))))
                                                              (setq-local macher-agent-presets '(skl35-b-base-skill))
                                                              (macher-agent-apply-preset '(skl35-b-base-skill))

                                                              ;; Direct helper checks: skl35-shared is not treated as :exclusive or :is-command in Workspace B
                                                              (expect (macher-agent--transformer-resolve-skills
                                                                       '(skl35-b-base-skill) '(skl35-shared) gptel--known-presets ctx-b buf-b)
                                                                      :to-equal '(skl35-b-base-skill skl35-shared))
                                                              (expect (macher-agent--transformer-detect-redirect
                                                                       t '(skl35-shared) gptel--known-presets ctx-b)
                                                                      :to-be nil)
                                                              (let ((tx (macher-agent--compile-transmission-payload
                                                                         buf-b '(skl35-b-base-skill) '(skl35-b-base-skill skl35-shared) nil ctx-b)))
                                                                (expect (macher-agent-transmission-state-compiled-prompt tx)
                                                                        :to-match "Workspace B base skill\\.")
                                                                (expect (mapcar #'gptel-tool-name (macher-agent-transmission-state-tools tx))
                                                                        :to-equal '("skl35_b_base" "skl35-shared"))))

                                                            ;; 3. Full prompt transformer in Workspace B preserves base skill + adds inline tool
                                                            (let ((fsm-b (gptel-make-fsm :info (list :buffer buf-b :data wire-b))))
                                                              (with-current-buffer wire-b
                                                                (markdown-mode)
                                                                (insert "### @skl35-shared inspect this\n")
                                                                (macher-agent-sync-prompt-transformer #'ignore fsm-b)
                                                                (let ((inf (gptel-fsm-info fsm-b)))
                                                                  (expect (plist-get inf :system) :to-match "Workspace B base skill\\.")
                                                                  (expect (plist-get inf :system) :not :to-match "WS A exclusive prompt")
                                                                  (expect (mapcar #'gptel-tool-name (plist-get inf :tools))
                                                                          :to-equal '("skl35_b_base" "skl35-shared"))))))
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (cl-remprop 'skl35-shared 'macher-agent-preset-meta)
                                                        (dolist (b (list buf-b wire-b))
                                                          (when (buffer-live-p b) (kill-buffer b)))
                                                        (when (and ws-b-dir (file-directory-p ws-b-dir))
                                                          (ignore-errors (delete-directory ws-b-dir t)))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-36
          ;; --------------------------------------------------------------------------
          (it "SKL-36: clears :stream on streaming->non-streaming Gemini switch and handles :toolConfig vs :tools/:tool_choice"
              (require 'gptel-gemini)
              (require 'gptel-bedrock)
              (let* ((gptel--known-backends nil)
                     (gptel-stream t)
                     (gptel-use-curl t)
                     (tool-a (gptel-make-tool
                              :name "skl36_alpha"
                              :description "Alpha tool"
                              :category "misc"
                              :args nil
                              :function #'ignore))
                     (tool-b (gptel-make-tool
                              :name "skl36_beta"
                              :description "Beta tool"
                              :category "misc"
                              :args nil
                              :function #'ignore))
                     (anthropic (gptel-make-anthropic "TestAnthropic36" :models '(claude-3-5-sonnet) :stream t))
                     (gemini (gptel-make-gemini "TestGemini36" :models '(gemini-2.5-flash) :stream nil))
                     (oai-chat (gptel-make-openai "TestChat36" :host "localhost" :models '(gpt-4o) :stream t))
                     (bedrock (gptel-make-bedrock "TestBedrock36"
                                :region "us-east-1"
                                :aws-bearer-token "test-token"
                                :models '(claude-3-5-sonnet-20241022)
                                :stream nil))
                     (ant-data (list :model "claude-3-5-sonnet"
                                     :system "Stream sys"
                                     :stream t
                                     :messages (vector (list :role "user" :content "hello"))))
                     (fsm (gptel-make-fsm :state 'TOOL
                                          :info (list :backend anthropic
                                                      :model 'claude-3-5-sonnet
                                                      :stream t
                                                      :data ant-data))))
                ;; 1. Streaming Anthropic -> non-streaming Gemini with gptel-use-tools = 'force
                (let ((gptel-use-tools 'force))
                  (macher-agent--sync-fsm-request-data fsm "Gemini sys" (list tool-a) 'gemini-2.5-flash gemini anthropic))
                (let* ((info (gptel-fsm-info fsm))
                       (gem-data (plist-get info :data)))
                  (expect (plist-get info :stream) :to-be nil)
                  (expect (plist-member info :stream) :to-be nil)
                  (expect (plist-get gem-data :toolConfig) :to-equal '(:functionCallingConfig (:mode "ANY")))
                  (expect (plist-get gem-data :tools) :not :to-be nil)
                  (expect (plist-get (plist-get gem-data :toolConfig) :tools) :to-be nil))

                ;; 2. Same-backend Gemini sync preserves :toolConfig (:functionCallingConfig (:mode "ANY")) separately from :tools
                (macher-agent--sync-fsm-request-data fsm "Gemini sys 2" (list tool-a tool-b) 'gemini-2.5-flash gemini gemini)
                (let ((gem-data (plist-get (gptel-fsm-info fsm) :data)))
                  (expect (plist-get gem-data :toolConfig) :to-equal '(:functionCallingConfig (:mode "ANY")))
                  (expect (plist-get (plist-get gem-data :toolConfig) :tools) :to-be nil)
                  (expect (length (plist-get (aref (plist-get gem-data :tools) 0) :functionDeclarations)) :to-equal 2))

                ;; 3. Clearing tools on Gemini removes :tools and :toolConfig
                (macher-agent--sync-fsm-request-data fsm "Gemini sys 3" nil 'gemini-2.5-flash gemini gemini)
                (let ((gem-data (plist-get (gptel-fsm-info fsm) :data)))
                  (expect (plist-member gem-data :tools) :to-be nil)
                  (expect (plist-member gem-data :toolConfig) :to-be nil))

                ;; 4. Non-streaming Gemini -> streaming OpenAI restores :stream t, and empty tools clears :tool_choice
                (macher-agent--sync-fsm-request-data fsm "Chat sys" (list tool-a) 'gpt-4o oai-chat gemini)
                (expect (plist-get (gptel-fsm-info fsm) :stream) :to-be t)
                (let ((chat-fsm (gptel-make-fsm
                                 :state 'TOOL
                                 :info (list :backend oai-chat
                                             :model 'gpt-4o
                                             :stream t
                                             :data (list :model "gpt-4o"
                                                         :messages (vector (list :role "user" :content "hi"))
                                                         :tools [(:type "function")]
                                                         :parallel_tool_calls t
                                                         :tool_choice "auto")))))
                  (macher-agent--sync-fsm-request-data chat-fsm "Chat sys" nil 'gpt-4o oai-chat oai-chat)
                  (let ((cdata (plist-get (gptel-fsm-info chat-fsm) :data)))
                    (expect (plist-member cdata :tools) :to-be nil)
                    (expect (plist-member cdata :parallel_tool_calls) :to-be nil)
                    (expect (plist-member cdata :tool_choice) :to-be nil)))

                ;; 5. Bedrock starting with 0 tools populates :toolConfig (:toolChoice (:auto ()) :tools ...) when tools added
                (let* ((br-data (list :messages (vector (list :role "user" :content [(:text "hi")]))
                                      :inferenceConfig (list :maxTokens 500)
                                      :system [(:text "Bedrock sys")]))
                       (br-fsm (gptel-make-fsm :state 'TOOL
                                               :info (list :backend bedrock
                                                           :model 'claude-3-5-sonnet-20241022
                                                           :data br-data))))
                  (macher-agent--sync-fsm-request-data br-fsm "Bedrock sys" (list tool-a) 'claude-3-5-sonnet-20241022 bedrock bedrock)
                  (let* ((updated (plist-get (gptel-fsm-info br-fsm) :data))
                         (tc (plist-get updated :toolConfig)))
                    (expect (plist-member updated :tools) :to-be nil)
                    (expect (plist-member (plist-get tc :toolChoice) :auto) :not :to-be nil)
                    (expect (length (plist-get tc :tools)) :to-equal 1))
                  (macher-agent--sync-fsm-request-data br-fsm "Bedrock sys" nil 'claude-3-5-sonnet-20241022 bedrock bedrock)
                  (expect (plist-member (plist-get (gptel-fsm-info br-fsm) :data) :toolConfig) :to-be nil))))

          ;; --------------------------------------------------------------------------
          ;; SKL-37
          ;; --------------------------------------------------------------------------
          (it "SKL-37: backfills missing :id on Gemini :tool-use when switching to Anthropic/OpenAI and correlates tool names for Gemini"
              (require 'gptel-gemini)
              (let* ((gptel--known-backends nil)
                     (gemini (gptel-make-gemini "TestGemini37" :models '(gemini-2.5-flash) :stream nil))
                     (anthropic (gptel-make-anthropic "TestAnthropic37" :models '(claude-3-5-sonnet) :stream nil))
                     (oai-resp (gptel-make-openai "TestResp37" :models '(gpt-4o) :stream nil))
                     (oai-chat (gptel-make-openai "TestChat37" :host "localhost" :models '(gpt-4o) :stream nil)))
                ;; 1. Gemini -> Anthropic backfills :id to call_read_file_2 after prior Gemini call in buffer
                (with-temp-buffer
                  (setq-local gptel-track-response t)
                  (gptel--parse-list-and-insert
                   '((prompt . "read two files")
                     (tool . (:id nil :name "read_file" :args (:path "a.el") :result "first"))))
                  (let ((fsm-ant (gptel-make-fsm
                                  :state 'TOOL
                                  :info (list :buffer (current-buffer)
                                              :backend gemini
                                              :model 'gemini-2.5-flash
                                              :data (list :contents (vector))
                                              :tool-use (list (list :name "read_file" :args '(:path "b.el") :result "second"))))))
                    (macher-agent--sync-fsm-request-data fsm-ant "Ant sys" nil 'claude-3-5-sonnet anthropic gemini)
                    (let* ((info (gptel-fsm-info fsm-ant))
                           (tool-use (plist-get info :tool-use))
                           (ant-msgs (plist-get (plist-get info :data) :messages))
                           (last-call-blk (aref (plist-get (aref ant-msgs (1- (length ant-msgs))) :content) 0))
                           (parsed-res (gptel--parse-tool-results anthropic tool-use))
                           (res-blk (aref (plist-get parsed-res :content) 0)))
                      (expect (plist-get (car tool-use) :id) :to-equal "call_read_file_2")
                      (expect (plist-get last-call-blk :id) :to-equal "call_read_file_2")
                      (expect (plist-get res-blk :tool_use_id) :to-equal "call_read_file_2"))))

                ;; 2. Gemini -> OpenAI Responses backfills :id matching :call_id
                (with-temp-buffer
                  (setq-local gptel-track-response t)
                  (gptel--parse-list-and-insert '((prompt . "lookup")))
                  (let ((fsm-resp (gptel-make-fsm
                                   :state 'TOOL
                                   :info (list :buffer (current-buffer)
                                               :backend gemini
                                               :model 'gemini-2.5-flash
                                               :data (list :contents (vector))
                                               :tool-use (list (list :name "lookup_docs" :args '(:q "fsm") :result "docs"))))))
                    (macher-agent--sync-fsm-request-data fsm-resp "Resp sys" nil 'gpt-4o oai-resp gemini)
                    (let* ((info (gptel-fsm-info fsm-resp))
                           (tool-use (plist-get info :tool-use))
                           (parsed-res (gptel--parse-tool-results oai-resp tool-use)))
                      (expect (plist-get (car tool-use) :id) :to-equal "call_lookup_docs")
                      (expect (plist-get (car parsed-res) :call_id) :to-equal "call_lookup_docs"))))

                ;; 3-5. Switching from OpenAI Responses / Anthropic / OpenAI Chat to Gemini correlates :functionResponse :name
                (dolist (case-spec `((,oai-resp "fc_42" "lookup_docs")
                                     (,anthropic "toolu_99" "search_code")
                                     (,oai-chat "call_chat_7" "grep_files")))
                  (pcase-let ((`(,src-backend ,call-id ,tool-name) case-spec))
                    (with-temp-buffer
                      (setq-local gptel-track-response t)
                      (gptel--parse-list-and-insert
                       `((prompt . "lookup")
                         (tool . (:id ,call-id :name ,tool-name :args (:q "x") :result "matched"))))
                      (let ((fsm-gem (gptel-make-fsm
                                      :state 'TOOL
                                      :info (list :buffer (current-buffer)
                                                  :backend src-backend
                                                  :data (list :messages (vector))))))
                        (macher-agent--sync-fsm-request-data fsm-gem "Gem sys" nil 'gemini-2.5-flash gemini src-backend)
                        (let* ((contents (plist-get (plist-get (gptel-fsm-info fsm-gem) :data) :contents))
                               (resp-part (aref (plist-get (aref contents 2) :parts) 0))
                               (fn-resp (plist-get resp-part :functionResponse)))
                          (expect (plist-get fn-resp :name) :to-equal tool-name))))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-38
          ;; --------------------------------------------------------------------------
          (it "SKL-38: resolves 0-arg backend URL lambdas and refreshes functionp URLs on same-backend model change"
              (require 'gptel-gemini)
              (require 'gptel-bedrock)
              (let* ((gptel--known-backends nil)
                     (zero-arg-backend (gptel-make-openai
                                           "ZeroArgURL38"
                                         :models '(gpt-4o)
                                         :stream nil))
                     (gemini (gptel-make-gemini
                                 "ModelURLGemini38"
                               :models '(gemini-2.5-flash gemini-2.5-pro)
                               :stream nil))
                     (bedrock (gptel-make-bedrock
                                  "ModelURLBedrock38"
                                :region "us-east-1"
                                :aws-bearer-token "test-token"
                                :models '(claude-3-5-sonnet-20241022 claude-sonnet-4-5-20250929)
                                :stream nil)))
                (setf (gptel-backend-url zero-arg-backend)
                      (lambda () "https://zero-arg.example/v1/chat"))
                ;; 1. 0-arg URL lambda resolves cleanly
                (expect (macher-agent--resolve-backend-url zero-arg-backend 'gpt-4o (list :model 'gpt-4o))
                        :to-equal "https://zero-arg.example/v1/chat")

                ;; 2. Same-backend model change on Gemini refreshes :url even when :url was absent on info
                (let ((gem-fsm (gptel-make-fsm
                                :state 'TOOL
                                :info (list :backend gemini
                                            :model 'gemini-2.5-flash
                                            :data (list :contents (vector (list :role "user" :parts [(:text "hi")])))))))
                  (macher-agent--sync-fsm-request-data gem-fsm "Sys" nil 'gemini-2.5-pro gemini gemini)
                  (expect (plist-get (gptel-fsm-info gem-fsm) :url) :to-match "gemini-2\\.5-pro:generateContent"))

                ;; 3. Same-backend model change on Bedrock refreshes model-encoded :url
                (let ((br-fsm (gptel-make-fsm
                               :state 'TOOL
                               :info (list :backend bedrock
                                           :model 'claude-3-5-sonnet-20241022
                                           :data (list :messages (vector (list :role "user" :content [(:text "hi")])))))))
                  (macher-agent--sync-fsm-request-data br-fsm "Sys" nil 'claude-sonnet-4-5-20250929 bedrock bedrock)
                  (expect (plist-get (gptel-fsm-info br-fsm) :url)
                          :to-match "/model/anthropic\\.claude-sonnet-4-5-20250929-v1:0/converse"))))

          ;; --------------------------------------------------------------------------
          ;; SKL-39
          ;; --------------------------------------------------------------------------
          (it "SKL-39: restores global script fallback on VFS override deletion and prunes frontmatter-renamed/deleted skills across all live context buffers"
              (macher-agent-test-with-git-workspace (_root ctx owner-buf)
                                                    (let* ((saved-tools-reg (copy-hash-table macher-agent-tools-registry))
                                                           (saved-global-skills macher-agent-global-skills-alist)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (buf-2 (generate-new-buffer "*skl-39-live-2*"))
                                                           (fallback-tool (gptel-make-tool
                                                                           :name "skl39_override_tool"
                                                                           :description "Global fallback v0"
                                                                           :category "misc"
                                                                           :args nil
                                                                           :function (lambda () "global-v0")))
                                                           (script-path "scripts/skl39_override_tool.el")
                                                           (skill-path "skills/skl39-dir/SKILL.md"))
                                                      (unwind-protect
                                                          (progn
                                                            (puthash "skl39_override_tool" fallback-tool macher-agent-tools-registry)
                                                            (with-current-buffer owner-buf
                                                              (markdown-mode)
                                                              (gptel-mode 1)
                                                              (macher-agent-register-tool-in-gptel-tables fallback-tool "misc")
                                                              (setq-local gptel-tools (list fallback-tool)))
                                                            (with-current-buffer buf-2
                                                              (markdown-mode)
                                                              (gptel-mode 1)
                                                              (setq-local macher-agent-persistent-context ctx)
                                                              (macher-agent-register-tool-in-gptel-tables fallback-tool "misc")
                                                              (setq-local gptel-tools (list fallback-tool)))

                                                            ;; 1. Stage VFS script override -> updates local-reg, gptel--known-tools, and gptel-tools in both buffers
                                                            (macher-agent-test--stage-ws-file
                                                             ctx script-path
                                                             "(gptel-make-tool :name \"skl39_override_tool\" :description \"VFS override v1\" :category \"misc\" :args nil :function (lambda () \"vfs-v1\"))\n")
                                                            (dolist (b (list owner-buf buf-2))
                                                              (with-current-buffer b
                                                                (let ((tl (gptel-get-tool "skl39_override_tool")))
                                                                  (expect (gptel-tool-description tl) :to-equal "VFS override v1")
                                                                  (expect (funcall (gptel-tool-function tl)) :to-equal "vfs-v1"))
                                                                (expect (gptel-tool-description (car gptel-tools)) :to-equal "VFS override v1")))

                                                            ;; Delete VFS override -> restores global fallback into local-reg, gptel--known-tools, and gptel-tools
                                                            (macher-agent-test--stage-ws-file ctx script-path nil)
                                                            (let ((resolved (macher-agent-resolve-tool "skl39_override_tool" nil nil ctx)))
                                                              (expect (gptel-tool-p resolved) :to-be t)
                                                              (expect (gptel-tool-description resolved) :to-equal "Global fallback v0")
                                                              (expect (funcall (gptel-tool-function resolved)) :to-equal "global-v0"))
                                                            (dolist (b (list owner-buf buf-2))
                                                              (with-current-buffer b
                                                                (let ((tl (ignore-errors (gptel-get-tool "skl39_override_tool"))))
                                                                  (expect (gptel-tool-p tl) :to-be t)
                                                                  (expect (gptel-tool-description tl) :to-equal "Global fallback v0")
                                                                  (expect (funcall (gptel-tool-function tl)) :to-equal "global-v0"))
                                                                (expect (length gptel-tools) :to-equal 1)
                                                                (expect (gptel-tool-description (car gptel-tools)) :to-equal "Global fallback v0")))

                                                            ;; 2. Create VFS SKILL.md whose frontmatter name: differs from directory name
                                                            (macher-agent-test--stage-ws-file
                                                             ctx skill-path
                                                             "---\nname: skl39-frontmatter-v1\ndescription: Frontmatter v1\n---\nPrompt v1.\n")
                                                            (dolist (b (list owner-buf buf-2))
                                                              (with-current-buffer b
                                                                (expect (alist-get 'skl39-frontmatter-v1 gptel--known-presets) :not :to-be nil)
                                                                (expect (alist-get 'skl39-frontmatter-v1 gptel-directives) :to-equal "Prompt v1.")))

                                                            ;; Rename frontmatter name: in-place -> old symbol pruned from BOTH buffers, new symbol added
                                                            (macher-agent-test--stage-ws-file
                                                             ctx skill-path
                                                             "---\nname: skl39-frontmatter-v2\ndescription: Frontmatter v2\n---\nPrompt v2.\n")
                                                            (dolist (b (list owner-buf buf-2))
                                                              (with-current-buffer b
                                                                (expect (alist-get 'skl39-frontmatter-v1 gptel--known-presets) :to-be nil)
                                                                (expect (alist-get 'skl39-frontmatter-v1 gptel-directives) :to-be nil)
                                                                (expect (alist-get 'skl39-frontmatter-v2 gptel--known-presets) :not :to-be nil)
                                                                (expect (alist-get 'skl39-frontmatter-v2 gptel-directives) :to-equal "Prompt v2.")))

                                                            ;; Delete VFS SKILL.md -> frontmatter-v2 symbol pruned from BOTH live buffers
                                                            (macher-agent-test--stage-ws-file ctx skill-path nil)
                                                            (dolist (b (list owner-buf buf-2))
                                                              (with-current-buffer b
                                                                (expect (alist-get 'skl39-frontmatter-v2 gptel--known-presets) :to-be nil)
                                                                (expect (alist-get 'skl39-frontmatter-v2 gptel-directives) :to-be nil))))
                                                        (setq macher-agent-tools-registry saved-tools-reg)
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (cl-remprop 'skl39-frontmatter-v1 'macher-agent-preset-meta)
                                                        (cl-remprop 'skl39-frontmatter-v2 'macher-agent-preset-meta)
                                                        (cl-remprop 'skl39-dir 'macher-agent-preset-meta)
                                                        (when (buffer-live-p buf-2) (kill-buffer buf-2))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-40
          ;; --------------------------------------------------------------------------
          (it "SKL-40: use_skill done: true restores cached baseline boot-directive without clobbering from orig preset"
              (macher-agent-test-with-git-workspace (_root ctx owner-buf)
                                                    (unwind-protect
                                                        (with-current-buffer owner-buf
                                                          (setq-local gptel--known-presets
                                                                      (list (cons 'skl40-orig
                                                                                  (list :system "Orig system."))
                                                                            (cons 'skl40-target
                                                                                  (list :system "Target system."))))
                                                          (macher-agent-set-preset-meta 'skl40-orig :boot-directive "ORIG PRESET BOOT")
                                                          (macher-agent-set-preset-meta 'skl40-target :boot-directive "TARGET PRESET BOOT")

                                                          ;; 1. Baseline has custom boot-directive distinct from orig's :boot-directive
                                                          (setq-local macher-agent-presets '(skl40-orig))
                                                          (setq-local gptel-system-prompt "Orig system.")
                                                          (setq-local macher-agent-boot-directive "CUSTOM BASELINE BOOT")

                                                          (macher-agent-test-invoke-tool macher-agent-use-skill-tool "skl40-target" nil)
                                                          (expect macher-agent-boot-directive :to-equal "TARGET PRESET BOOT")
                                                          (expect macher-agent-cached-boot-directive :to-equal "CUSTOM BASELINE BOOT")

                                                          (macher-agent-test-invoke-tool macher-agent-use-skill-tool "done" t)
                                                          (expect macher-agent-boot-directive :to-equal "CUSTOM BASELINE BOOT")
                                                          (expect (local-variable-p 'macher-agent-cached-boot-directive owner-buf) :to-be nil)
                                                          (expect (local-variable-p 'macher-agent-cached-boot-directive-set owner-buf) :to-be nil)

                                                          ;; 2. Baseline had nil boot-directive while orig preset carries :boot-directive
                                                          (setq-local macher-agent-presets '(skl40-orig))
                                                          (setq-local macher-agent-boot-directive nil)
                                                          (macher-agent-test-invoke-tool macher-agent-use-skill-tool "skl40-target" nil)
                                                          (expect macher-agent-boot-directive :to-equal "TARGET PRESET BOOT")
                                                          (macher-agent-test-invoke-tool macher-agent-use-skill-tool "done" t)
                                                          (expect macher-agent-boot-directive :to-be nil))
                                                      (cl-remprop 'skl40-orig 'macher-agent-preset-meta)
                                                      (cl-remprop 'skl40-target 'macher-agent-preset-meta))))

          ;; --------------------------------------------------------------------------
          ;; SKL-41
          ;; --------------------------------------------------------------------------
          (it "SKL-41: Org-mode unresolvable or command GPTEL_PRESET does not loop gptel--restore-state"
              (macher-agent-test-with-git-workspace (root _ctx _owner-buf)
                                                    (let* ((orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (unknown-org (expand-file-name "unknown-preset.org" root))
                                                           (cmd-org (expand-file-name "cmd-preset.org" root))
                                                           (valid-org (expand-file-name "valid-preset.org" root))
                                                           (buf-unknown nil)
                                                           (buf-cmd nil)
                                                           (buf-valid nil))
                                                      (unwind-protect
                                                          (progn
                                                            (gptel-make-preset 'skl41-valid-preset
                                                              :description "Valid preset"
                                                              :system "Valid SKL41 system.")
                                                            (gptel-make-preset 'skl41-cmd-preset
                                                              :description "Command preset"
                                                              :system "Cmd SKL41 body $1.")
                                                            (macher-agent-set-preset-meta 'skl41-cmd-preset :is-command t)

                                                            ;; 1. Unknown/deleted GPTEL_PRESET in Org buffer does not trigger reconcile loop
                                                            (with-temp-file unknown-org
                                                              (insert ":PROPERTIES:\n"
                                                                      ":GPTEL_PRESET: skl41-deleted-preset\n"
                                                                      ":END:\n"
                                                                      "* Chat\nHello\n"))
                                                            (let ((debug-on-error nil)
                                                                  (warning-minimum-log-level :error))
                                                              (setq buf-unknown (find-file-noselect unknown-org))
                                                              (with-current-buffer buf-unknown
                                                                (gptel-mode 1)
                                                                (macher-agent-mode 1)
                                                                (expect gptel--preset :to-be nil)
                                                                (expect (macher-agent--needs-preset-reconcile-p) :to-be nil)
                                                                (let ((restore-calls 0)
                                                                      (orig-restore (symbol-function 'gptel--restore-state)))
                                                                  (cl-letf (((symbol-function 'gptel--restore-state)
                                                                             (lambda ()
                                                                               (setq restore-calls (1+ restore-calls))
                                                                               (funcall orig-restore))))
                                                                    (macher-agent-setup-gptel-buffer)
                                                                    (macher-agent-setup-gptel-buffer)
                                                                    (expect restore-calls :to-equal 0)))
                                                                (set-buffer-modified-p nil)))

                                                            ;; 2. Command GPTEL_PRESET in Org buffer also does not trigger reconcile loop
                                                            (with-temp-file cmd-org
                                                              (insert ":PROPERTIES:\n"
                                                                      ":GPTEL_PRESET: skl41-cmd-preset\n"
                                                                      ":END:\n"
                                                                      "* Chat\nHello\n"))
                                                            (let ((debug-on-error nil)
                                                                  (warning-minimum-log-level :error))
                                                              (setq buf-cmd (find-file-noselect cmd-org))
                                                              (with-current-buffer buf-cmd
                                                                (gptel-mode 1)
                                                                (macher-agent-mode 1)
                                                                (expect (macher-agent--needs-preset-reconcile-p) :to-be nil)
                                                                (set-buffer-modified-p nil)))

                                                            ;; 3. Resolvable non-command GPTEL_PRESET still reconciles when gptel--preset is nil
                                                            (with-temp-file valid-org
                                                              (insert ":PROPERTIES:\n"
                                                                      ":GPTEL_PRESET: skl41-valid-preset\n"
                                                                      ":END:\n"
                                                                      "* Chat\nHello\n"))
                                                            (let ((debug-on-error nil)
                                                                  (warning-minimum-log-level :error))
                                                              (setq buf-valid (find-file-noselect valid-org))
                                                              (with-current-buffer buf-valid
                                                                (gptel-mode 1)
                                                                (macher-agent-mode 1)
                                                                (setq-local gptel--preset nil)
                                                                (expect (macher-agent--needs-preset-reconcile-p) :not :to-be nil)
                                                                (macher-agent-setup-gptel-buffer)
                                                                (expect gptel--preset :to-equal 'skl41-valid-preset)
                                                                (expect (macher-agent--needs-preset-reconcile-p) :to-be nil)
                                                                (set-buffer-modified-p nil))))
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (cl-remprop 'skl41-valid-preset 'macher-agent-preset-meta)
                                                        (cl-remprop 'skl41-cmd-preset 'macher-agent-preset-meta)
                                                        (dolist (b (list buf-unknown buf-cmd buf-valid))
                                                          (when (buffer-live-p b)
                                                            (with-current-buffer b (set-buffer-modified-p nil))
                                                            (kill-buffer b)))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-43
          ;; --------------------------------------------------------------------------
          (it "SKL-43: composes declarative :system modifiers and parent presets without ### Skill: :description or base-prompt duplication"
              (macher-agent-test-with-git-workspace (_root _ctx owner-buf '(("src/lib.el" . "(provide 'lib)\n")))
                                                    (let* ((orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (read-tool (gptel-make-tool :name "skl43_read" :description "Read" :category "perception" :args nil :function #'ignore))
                                                           (search-tool (gptel-make-tool :name "skl43_search" :description "Search" :category "perception" :args '((:name "regexp" :type "string") (:name "path" :type "string" :optional t)) :function #'ignore))
                                                           (wire-1 (generate-new-buffer " *skl43-wire-1*"))
                                                           (wire-2 (generate-new-buffer " *skl43-wire-2*")))
                                                      (unwind-protect
                                                          (progn
                                                            ;; 1. Unit check: :append, :prepend, and anonymous :parents plists
                                                            (let* ((known
                                                                    (list (cons 'mod-preset
                                                                                (list :description "Modifier preset"
                                                                                      :parents (list '(:description "Anon parent"
                                                                                                                    :system (:append "\n[APPENDED_CTX]")))
                                                                                      :system '(:prepend "[PREPENDED]\n")))
                                                                          (cons 'named-skill
                                                                                (list :description "Named skill"
                                                                                      :system "Follow strict style."))))
                                                                   (composed (macher-agent-compose-payload
                                                                              (list :known-presets known
                                                                                    :system "Base rules.")
                                                                              '(mod-preset named-skill)))
                                                                   (sys (plist-get composed :system)))
                                                              (expect sys :not :to-match "### Skill: :description")
                                                              (expect sys :to-match "\\`\\[PREPENDED\\]\nBase rules\\.\n\\[APPENDED_CTX\\]\n---\n### Skill: named-skill\nFollow strict style\\.\n\\'")
                                                              (expect (cl-count "Base rules." (split-string sys "\n") :test #'string=)
                                                                      :to-equal 1))

                                                            ;; 2. End-to-end @skl43-ro inline and gptel--preset with :parents system modifier
                                                            (with-current-buffer owner-buf
                                                              (markdown-mode)
                                                              (gptel-mode 1)
                                                              (macher-agent-mode 1)
                                                              (macher-agent-register-tool-in-gptel-tables read-tool "perception")
                                                              (macher-agent-register-tool-in-gptel-tables search-tool "perception")
                                                              (setq-local gptel-system-prompt "* Use British English\n* Keep responses concise")
                                                              (setq-local gptel--known-presets
                                                                          (append (list (cons 'skl43-base
                                                                                              (list :description "Base project context"
                                                                                                    :system '(:append "\n<current-project>\nsrc/lib.el\n</current-project>")))
                                                                                        (cons 'skl43-ro
                                                                                              (list :description "Read-only preset"
                                                                                                    :parents '(skl43-base)
                                                                                                    :tools '("skl43_read" "skl43_search")))
                                                                                        (cons 'extra-reviewer
                                                                                              (list :description "Reviewer"
                                                                                                    :system "Check edge cases.")))
                                                                                  (copy-alist (default-value 'gptel--known-presets))))
                                                              (let ((fsm-1 (gptel-make-fsm :info (list :buffer owner-buf :data wire-1))))
                                                                (with-current-buffer wire-1
                                                                  (insert "### @skl43-ro @extra-reviewer audit files")
                                                                  (macher-agent-sync-prompt-transformer nil fsm-1))
                                                                (let ((compiled (plist-get (gptel-fsm-info fsm-1) :system)))
                                                                  (expect compiled :not :to-match "### Skill: :description")
                                                                  (expect compiled :to-match "<current-project>")
                                                                  (expect compiled :to-match "src/lib\\.el")
                                                                  (expect compiled :to-match "### Skill: extra-reviewer\nCheck edge cases\\.")
                                                                  (expect (cl-count "* Use British English" (split-string compiled "\n") :test #'string=)
                                                                          :to-equal 1)))

                                                              ;; Buffer-level preset `skl43-ro' also expands parent system modifiers cleanly
                                                              (gptel--apply-preset 'skl43-ro (lambda (sym val) (set (make-local-variable sym) val)))
                                                              (let ((fsm-2 (gptel-make-fsm :info (list :buffer owner-buf :data wire-2))))
                                                                (with-current-buffer wire-2
                                                                  (insert "### inspect repo")
                                                                  (macher-agent-sync-prompt-transformer nil fsm-2))
                                                                (let ((compiled-2 (plist-get (gptel-fsm-info fsm-2) :system))
                                                                      (tools-2 (plist-get (gptel-fsm-info fsm-2) :tools)))
                                                                  (expect compiled-2 :not :to-match "### Skill: :description")
                                                                  (expect compiled-2 :to-match "<current-project>")
                                                                  (expect (cl-count "* Use British English" (split-string compiled-2 "\n") :test #'string=)
                                                                          :to-equal 1)
                                                                  (let ((siw (cl-find "skl43_search" tools-2 :key #'gptel-tool-name :test #'equal)))
                                                                    (expect siw :not :to-be nil)
                                                                    (expect (gptel-tool-category siw) :to-equal "perception")
                                                                    (expect (mapcar (lambda (a) (plist-get a :name)) (gptel-tool-args siw))
                                                                            :to-equal '("regexp" "path")))))))
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (dolist (b (list wire-1 wire-2))
                                                          (when (buffer-live-p b) (kill-buffer b)))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-44
          ;; --------------------------------------------------------------------------
          (it "SKL-44: resolves inline @skl44-ro and @skl44-rust in restored sessions without historical @preset clobbering or Local Variables leakage"
              (macher-agent-test-with-git-workspace (root _ctx _owner-buf '(("src/lib.rs" . "pub fn add(a: i32, b: i32) -> i32 { a + b }\n")))
                                                    (let* ((orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (gptel--known-backends (copy-alist (bound-and-true-p gptel--known-backends)))
                                                           (backend (gptel-make-openai "RestoredTestBackend"
                                                                      :host "localhost"
                                                                      :endpoint "/v1/chat/completions"
                                                                      :models '(gpt-4o gpt-4o-mini)
                                                                      :stream nil))
                                                           (base-tool (gptel-make-tool
                                                                       :name "skl44_base_tool"
                                                                       :description "Baseline saved tool"
                                                                       :category "execution"
                                                                       :args nil
                                                                       :function #'ignore))
                                                           (read-tool (gptel-make-tool
                                                                       :name "skl44_read_tool"
                                                                       :description "Read tool"
                                                                       :category "perception"
                                                                       :args nil
                                                                       :function #'ignore))
                                                           (write-tool (gptel-make-tool
                                                                        :name "skl44_write_tool"
                                                                        :description "Write tool"
                                                                        :category "execution"
                                                                        :args nil
                                                                        :function #'ignore))
                                                           (search-tool (gptel-make-tool
                                                                         :name "skl44_search_tool"
                                                                         :description "Search tool"
                                                                         :category "perception"
                                                                         :args nil
                                                                         :function #'ignore))
                                                           (rust-tool (gptel-make-tool
                                                                       :name "skl44_cargo_check"
                                                                       :description "Cargo check"
                                                                       :category "execution"
                                                                       :args nil
                                                                       :function #'ignore))
                                                           (chat-file (expand-file-name "restored-inline.md" root))
                                                           (reopened nil))
                                                      (unwind-protect
                                                          (progn
                                                            (with-temp-buffer
                                                              (setq-local gptel--known-tools (default-value 'gptel--known-tools))
                                                              (dolist (tl (list base-tool read-tool write-tool search-tool rust-tool))
                                                                (macher-agent-register-tool-in-gptel-tables tl (gptel-tool-category tl)))
                                                              (set-default 'gptel--known-tools (copy-tree gptel--known-tools)))
                                                            (gptel-make-preset 'skl44-default
                                                              :description "Default style preset"
                                                              :system "* Use British English")
                                                            (gptel-make-preset 'skl44-ro
                                                              :description "Read-only skill"
                                                              :system "Read only."
                                                              :tools '("skl44_read_tool" "skl44_search_tool"))
                                                            (gptel-make-preset 'skl44-rust
                                                              :description "Rust coding skill"
                                                              :system "Write idiomatic Rust."
                                                              :tools '("skl44_read_tool"
                                                                       "skl44_write_tool"
                                                                       "skl44_search_tool"
                                                                       "skl44_cargo_check"))

                                                            ;; Write saved chat with unpropped historical @skl44-ro text + Local Variables
                                                            (with-temp-file chat-file
                                                              (let* ((unpropped-history
                                                                      (concat "Historical pasted context mentioning @skl44-rust and @skl44-ro.\n\n"
                                                                              "### @skl44-ro earlier unpropped turn\n\n"
                                                                              "Unpropped reply.\n\n"
                                                                              "### Recent turn\n\n"))
                                                                     (resp-beg (1+ (length unpropped-history)))
                                                                     (resp-text "Tracked assistant response.")
                                                                     (resp-end (+ resp-beg (length resp-text)))
                                                                     (lv-start (+ resp-end 2)))
                                                                (insert unpropped-history resp-text "\n\n"
                                                                        "<!-- Local " "Variables: -->\n"
                                                                        "<!-- gptel--preset: skl44-default -->\n"
                                                                        "<!-- gptel-model: gpt-4o-mini -->\n"
                                                                        "<!-- gptel--backend-name: \"RestoredTestBackend\" -->\n"
                                                                        (format "<!-- gptel--bounds: ((response (%d %d)) (ignore (%d %d))) -->\n"
                                                                                resp-beg resp-end lv-start (+ lv-start 120))
                                                                        "<!-- gptel--tool-names: (\"skl44_base_tool\") -->\n"
                                                                        "<!-- " "End: -->\n")))

                                                            (let ((enable-local-variables :all)
                                                                  (warning-minimum-log-level :error))
                                                              (setq reopened (find-file-noselect chat-file))
                                                              (with-current-buffer reopened
                                                                ;; Simulate gptel-mode running raw restore before macher-agent-mode
                                                                (gptel-mode 1)
                                                                (macher-agent-mode 1)
                                                                (expect gptel--preset :to-equal 'skl44-default)
                                                                (expect gptel-model :to-equal 'gpt-4o-mini)
                                                                (expect (mapcar #'gptel-tool-name gptel-tools) :to-equal '("skl44_base_tool"))

                                                                ;; 1. Typing right at `<!-- Local Variables: -->' does not inherit `ignore' stickiness,
                                                                ;;    and full gptel-request :dry-run t from point-max preserves @skl44-rust write tools
                                                                ;;    without historical @skl44-ro clobbering or footer leakage into :data
                                                                (save-excursion
                                                                  (goto-char (point-min))
                                                                  (search-forward (concat "<!-- Local " "Variables:"))
                                                                  (goto-char (match-beginning 0))
                                                                  (insert "### @skl44-rust implement feature\n\n")
                                                                  (put-text-property (point) (point-max) 'gptel 'ignore))
                                                                (goto-char (point-max))
                                                                (let* ((fsm-rust (gptel-request nil
                                                                                   :dry-run t
                                                                                   :stream nil
                                                                                   :transforms gptel-prompt-transform-functions))
                                                                       (info-rust (gptel-fsm-info fsm-rust))
                                                                       (tool-names (mapcar #'gptel-tool-name (plist-get info-rust :tools)))
                                                                       (msgs (append (plist-get (plist-get info-rust :data) :messages) nil))
                                                                       (last-user (car (last msgs))))
                                                                  (expect (member "skl44_base_tool" tool-names) :not :to-be nil)
                                                                  (expect (member "skl44_write_tool" tool-names) :not :to-be nil)
                                                                  (expect (member "skl44_cargo_check" tool-names) :not :to-be nil)
                                                                  (expect (plist-get last-user :content) :to-equal "implement feature")
                                                                  (expect (plist-get last-user :content) :not :to-match (concat "Local " "Variables:")))

                                                                ;; Main buffer tools remain strictly baseline
                                                                (expect (mapcar #'gptel-tool-name gptel-tools) :to-equal '("skl44_base_tool"))

                                                                ;; 2. Replace prompt with inline @skl44-ro: read-only tools
                                                                (save-excursion
                                                                  (goto-char (point-min))
                                                                  (when (search-forward "### @skl44-rust implement feature\n\n" nil t)
                                                                    (replace-match "### @skl44-ro inspect files\n\n" t t)))
                                                                (goto-char (point-max))
                                                                (let* ((fsm-ro (gptel-request nil
                                                                                 :dry-run t
                                                                                 :stream nil
                                                                                 :transforms gptel-prompt-transform-functions))
                                                                       (ro-tools (plist-get (gptel-fsm-info fsm-ro) :tools))
                                                                       (ro-names (mapcar #'gptel-tool-name ro-tools))
                                                                       (siw (cl-find "skl44_search_tool" ro-tools :key #'gptel-tool-name :test #'equal)))
                                                                  (expect (member "skl44_base_tool" ro-names) :not :to-be nil)
                                                                  (expect (member "skl44_read_tool" ro-names) :not :to-be nil)
                                                                  (expect (member "skl44_write_tool" ro-names) :to-be nil)
                                                                  (expect siw :not :to-be nil)
                                                                  (expect (gptel-tool-category siw) :to-equal "perception"))

                                                                ;; 3. Plain follow-up turn without inline tag reverts strictly to baseline saved tools
                                                                (save-excursion
                                                                  (goto-char (point-min))
                                                                  (when (search-forward "### @skl44-ro inspect files\n\n" nil t)
                                                                    (replace-match "### plain follow-up question\n\n" t t)))
                                                                (goto-char (point-max))
                                                                (let* ((fsm-plain (gptel-request nil
                                                                                    :dry-run t
                                                                                    :stream nil
                                                                                    :transforms gptel-prompt-transform-functions))
                                                                       (plain-names (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm-plain) :tools))))
                                                                  (expect plain-names :to-equal '("skl44_base_tool")))
                                                                (set-buffer-modified-p nil))))
                                                        (ignore backend)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (when (buffer-live-p reopened)
                                                          (with-current-buffer reopened (set-buffer-modified-p nil))
                                                          (kill-buffer reopened))))))

          ;; --------------------------------------------------------------------------
          ;; SKL-45
          ;; --------------------------------------------------------------------------
          (it "SKL-45: passes server-side allowed-tools to API when disable-local-tools is true without local tool leakage"
              (macher-agent-test-with-git-workspace (_root ctx owner-buf)
                                                    (let* ((saved-global-skills macher-agent-global-skills-alist)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (gptel--known-backends nil)
                                                           (openai (gptel-make-openai "SKL45OpenAI" :models '(gpt-4o) :stream nil))
                                                           (gemini (gptel-make-gemini "SKL45Gemini" :models '(gemini-flash-latest) :stream nil))
                                                           (local-tool (gptel-make-tool
                                                                        :name "skl45_local_tool"
                                                                        :description "Local workspace tool"
                                                                        :category "workspace"
                                                                        :args nil
                                                                        :function (lambda () "local-ok"))))
                                                      (unwind-protect
                                                          (progn
                                                            (macher-agent-test--stage-ws-file
                                                             ctx "skills/scripts/skl45_local_tool.el"
                                                             "(gptel-make-tool :name \"skl45_local_tool\" :description \"Local workspace tool\" :category \"workspace\" :args nil :function (lambda () \"local-ok\"))\n")
                                                            (macher-agent-test--stage-ws-file
                                                             ctx "skills/google-search/SKILL.md"
                                                             (concat "---\n"
                                                                     "name: google-search\n"
                                                                     "description: Grounds responses in live Google Search results via Gemini without local workspace tools.\n"
                                                                     "model: gemini-flash-latest\n"
                                                                     "exclusive: true\n"
                                                                     "disable-local-tools: true\n"
                                                                     "allowed-tools: [google_search]\n"
                                                                     "---\n"
                                                                     "Ground answers in live search results.\n"))

                                                            (with-current-buffer owner-buf
                                                              (markdown-mode)
                                                              (gptel-mode 1)
                                                              (macher-agent-mode 1)
                                                              (macher-agent-register-tool-in-gptel-tables local-tool "workspace")
                                                              (macher-agent-refresh-skills-and-tools ctx)

                                                              ;; 1. Registered preset has API :request-params, nil local :tools, and no extension meta leakage
                                                              (let ((preset-entry (alist-get 'google-search gptel--known-presets))
                                                                    (full-spec (macher-agent-lookup-skill-spec 'google-search ctx owner-buf)))
                                                                (expect preset-entry :not :to-be nil)
                                                                (expect (plist-get preset-entry :request-params)
                                                                        :to-equal '(:tools [(:google_search nil)]))
                                                                (expect (plist-get preset-entry :tools) :to-be nil)
                                                                (expect (plist-get preset-entry :use-tools) :to-be nil)
                                                                (expect (plist-member preset-entry :exclusive) :to-be nil)
                                                                (expect (plist-member preset-entry :disable-local-tools) :to-be nil)
                                                                (expect (plist-member preset-entry :server-tools) :to-be nil)
                                                                (expect (plist-get full-spec :disable-local-tools) :to-be t)
                                                                (expect (plist-get full-spec :server-tools) :to-equal '("google_search"))
                                                                (expect (plist-get full-spec :tools) :to-be nil)
                                                                (expect (ignore-errors (gptel-get-tool "google_search")) :to-be nil)
                                                                (expect (json-serialize (plist-get preset-entry :request-params)
                                                                                        :null-object :null
                                                                                        :false-object :json-false)
                                                                        :to-equal "{\"tools\":[{\"google_search\":{}}]}"))

                                                              ;; 2. Inline @google-search encodes Gemini server tool, flattens wire tool history, and isolates owner-buf
                                                              (setq-local gptel-backend openai)
                                                              (setq-local gptel-model 'gpt-4o)
                                                              (setq-local gptel-tools (list local-tool macher-agent-use-skill-tool))
                                                              (erase-buffer)
                                                              (insert "### Earlier turn\n\n")
                                                              (let ((tbeg (point)))
                                                                (insert "```tool (skl45_local_tool)\nlocal-ok\n```\n\n")
                                                                (put-text-property tbeg (point) 'gptel 'ignore)
                                                                (put-text-property tbeg (point) 'gptel-tool
                                                                                   (list :name "skl45_local_tool" :args nil :result "local-ok")))
                                                              (let ((rbeg (point)))
                                                                (insert "Earlier assistant reply.\n\n")
                                                                (put-text-property rbeg (point) 'gptel 'response))
                                                              (insert "### @google-search latest match result\n\n")
                                                              (goto-char (point-max))
                                                              (let* ((fsm (gptel-request nil
                                                                            :dry-run t
                                                                            :stream nil
                                                                            :transforms gptel-prompt-transform-functions))
                                                                     (inf (gptel-fsm-info fsm))
                                                                     (data (plist-get inf :data)))
                                                                (expect (plist-get inf :backend) :to-equal gemini)
                                                                (expect (plist-get inf :model) :to-equal 'gemini-flash-latest)
                                                                (expect (plist-get inf :tools) :to-be nil)
                                                                (expect (plist-get inf :disable-local-tools) :to-be t)
                                                                (expect (plist-get data :tools) :to-equal [(:google_search nil)])
                                                                (expect (plist-get data :toolConfig) :to-be nil)
                                                                (let ((serialized (json-serialize data :null-object :null :false-object :json-false)))
                                                                  (expect serialized :to-match "\"google_search\":{}")
                                                                  (expect serialized :not :to-match "functionDeclarations")
                                                                  (expect serialized :not :to-match "functionCall")
                                                                  (expect serialized :not :to-match "functionResponse")))
                                                              ;; Main buffer remains untouched
                                                              (expect gptel-backend :to-equal openai)
                                                              (expect gptel-model :to-equal 'gpt-4o)
                                                              (expect (local-variable-p 'gptel--request-params owner-buf) :to-be nil)
                                                              (expect macher-agent-disable-local-tools :to-be nil)
                                                              (expect (length gptel-tools) :to-equal 2)

                                                              ;; 3. Mid-turn use_skill rejects disable-local-tools skills
                                                              (expect (macher-agent-test-invoke-tool macher-agent-use-skill-tool "google-search" nil)
                                                                      :to-match "^ERROR:.*disables local tools")

                                                              ;; 4. Buffer-level activation + VFS hot-edits ([google_search, url_context] -> [] -> local tool)
                                                              (macher-agent-use-skill 'google-search owner-buf)
                                                              (expect gptel--request-params :to-equal '(:tools [(:google_search nil)]))
                                                              (expect macher-agent-disable-local-tools :to-be t)
                                                              (expect gptel-tools :to-be nil)

                                                              (let ((live-fsm (gptel-make-fsm
                                                                               :state 'TOOL
                                                                               :info (list :buffer owner-buf
                                                                                           :backend gemini
                                                                                           :model 'gemini-flash-latest
                                                                                           :macher-agent-context ctx
                                                                                           :data (list :contents [(:role "user" :parts [(:text "hi")])])))))
                                                                (setq-local macher-agent-active-fsm live-fsm)
                                                                (macher-agent-test--stage-ws-file
                                                                 ctx "skills/google-search/SKILL.md"
                                                                 (concat "---\n"
                                                                         "name: google-search\n"
                                                                         "description: Multi server tools\n"
                                                                         "model: gemini-flash-latest\n"
                                                                         "exclusive: true\n"
                                                                         "disable-local-tools: true\n"
                                                                         "allowed-tools: [google_search, url_context]\n"
                                                                         "---\n"
                                                                         "Use search and URL context.\n"))
                                                                (expect gptel--request-params
                                                                        :to-equal '(:tools [(:google_search nil) (:url_context nil)]))
                                                                (expect (plist-get (plist-get (gptel-fsm-info live-fsm) :data) :tools)
                                                                        :to-equal [(:google_search nil) (:url_context nil)])

                                                                (macher-agent-test--stage-ws-file
                                                                 ctx "skills/google-search/SKILL.md"
                                                                 (concat "---\n"
                                                                         "name: google-search\n"
                                                                         "description: Pure toolless\n"
                                                                         "model: gemini-flash-latest\n"
                                                                         "exclusive: true\n"
                                                                         "disable-local-tools: true\n"
                                                                         "allowed-tools: []\n"
                                                                         "---\n"
                                                                         "Pure toolless persona.\n"))
                                                                (expect (local-variable-p 'gptel--request-params owner-buf) :to-be nil)
                                                                (expect macher-agent-disable-local-tools :to-be t)
                                                                (expect (plist-member (plist-get (gptel-fsm-info live-fsm) :data) :tools) :to-be nil)
                                                                (setq-local macher-agent-active-fsm nil))

                                                              ;; Between-turn edit removing disable-local-tools restores local tool resolution on next turn
                                                              (macher-agent-use-skill 'google-search owner-buf)
                                                              (setq-local macher-agent-presets nil)
                                                              (macher-agent-test--stage-ws-file
                                                               ctx "skills/google-search/SKILL.md"
                                                               (concat "---\n"
                                                                       "name: google-search\n"
                                                                       "description: Switched back to local tool\n"
                                                                       "model: gpt-4o\n"
                                                                       "exclusive: true\n"
                                                                       "allowed-tools: [skl45_local_tool]\n"
                                                                       "---\n"
                                                                       "Local tool persona.\n"))
                                                              (let ((tx (macher-agent--compile-transmission-payload
                                                                         owner-buf nil nil nil ctx)))
                                                                (expect (macher-agent-transmission-state-disable-local-tools tx) :to-be nil)
                                                                (expect (macher-agent-transmission-state-request-params tx) :to-be nil)
                                                                (expect (mapcar #'gptel-tool-name (macher-agent-transmission-state-tools tx))
                                                                        :to-equal '("skl45_local_tool")))))
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (cl-remprop 'google-search 'macher-agent-preset-meta)))))

          ;; --------------------------------------------------------------------------
          ;; SKL-46
          ;; --------------------------------------------------------------------------
          (it "SKL-46: preserves inline @skl46-inline tools across mid-turn SKILL.md writes and sanitises blocked nil tool-spec results in TRET"
              (macher-agent-test-with-git-workspace (_root ctx owner-buf)
                                                    (let* ((orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (base-tool (gptel-make-tool
                                                                       :name "skl46_base_tool"
                                                                       :description "Baseline tool"
                                                                       :category "workspace"
                                                                       :include t
                                                                       :args nil
                                                                       :function (lambda () "ok")))
                                                           (read-tool (gptel-make-tool
                                                                       :name "skl46_read_tool"
                                                                       :description "Read tool"
                                                                       :category "workspace"
                                                                       :args nil
                                                                       :function (lambda () "read")))
                                                           (edit-tool (gptel-make-tool
                                                                       :name "skl46_edit_tool"
                                                                       :description "Edit tool"
                                                                       :category "workspace"
                                                                       :args nil
                                                                       :function (lambda () "edit")))
                                                           (write-tool (gptel-make-tool
                                                                        :name "skl46_write_tool"
                                                                        :description "Write tool"
                                                                        :category "workspace"
                                                                        :args '((:name "path" :type "string") (:name "content" :type "string"))
                                                                        :function (lambda (path content)
                                                                                    (macher-agent-test--stage-ws-file ctx path content)
                                                                                    "wrote"))))
                                                      (unwind-protect
                                                          (with-current-buffer owner-buf
                                                            (markdown-mode)
                                                            (gptel-mode 1)
                                                            (macher-agent-mode 1)
                                                            (setq-local gptel--known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                            (setq-local gptel--known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                            (dolist (tl (list base-tool read-tool edit-tool write-tool))
                                                              (macher-agent-register-tool-in-gptel-tables tl "workspace"))
                                                            (setf (alist-get 'skl46-inline gptel--known-presets)
                                                                  '(:system "Inline skill" :tools ("skl46_read_tool" "skl46_edit_tool" "skl46_write_tool")))
                                                            (setq-local gptel-tools (list base-tool))
                                                            (setq-local macher-agent-presets nil)
                                                            (macher-agent-initialize-skills ctx)

                                                            ;; 1. Start an inline @skl46-inline turn via dry-run FSM
                                                            (erase-buffer)
                                                            (insert "### @skl46-inline create a workspace skill\n\n")
                                                            (goto-char (point-max))
                                                            (let* ((fsm (gptel-request nil
                                                                          :dry-run t
                                                                          :stream nil
                                                                          :transforms gptel-prompt-transform-functions))
                                                                   (initial-tools (plist-get (gptel-fsm-info fsm) :tools))
                                                                   (found-write (cl-find "skl46_write_tool" initial-tools
                                                                                         :key #'gptel-tool-name :test #'equal)))
                                                              (expect found-write :not :to-be nil)
                                                              (setq-local macher-agent-active-fsm fsm)

                                                              ;; Invoke write tool on a managed SKILL.md path mid-turn
                                                              (funcall (gptel-tool-function found-write)
                                                                       "skills/skl46-hot/SKILL.md"
                                                                       "---\nname: skl46-hot\ndescription: Hot skill\n---\nHot body.\n")

                                                              ;; FSM retains inline @skl46-inline tools while main buffer stays isolated
                                                              (let ((after-names (mapcar #'gptel-tool-name (plist-get (gptel-fsm-info fsm) :tools))))
                                                                (expect (member "skl46_write_tool" after-names) :not :to-be nil)
                                                                (expect (member "skl46_edit_tool" after-names) :not :to-be nil)
                                                                (expect (member "skl46_read_tool" after-names) :not :to-be nil))
                                                              (expect (mapcar #'gptel-tool-name gptel-tools) :to-equal '("skl46_base_tool"))
                                                              (setq-local macher-agent-active-fsm nil))

                                                            ;; 2. Out-of-scope tool blocked in TPRE -> TRET displays error cleanly without wrong-type-argument
                                                            (erase-buffer)
                                                            (insert "### Test blocked tool display\n")
                                                            (let* ((pos-marker (copy-marker (point-max)))
                                                                   (tool-call (list :id "call_skl46_blocked"
                                                                                    :name "unauthorised_write_tool"
                                                                                    :args '(:path "secret.txt")))
                                                                   (block-fsm (gptel-make-fsm
                                                                               :state 'TPRE
                                                                               :handlers (copy-alist gptel-send--handlers)
                                                                               :table (copy-alist gptel-send--transitions)
                                                                               :info (list :buffer owner-buf
                                                                                           :position pos-marker
                                                                                           :callback #'gptel-curl--stream-insert-response
                                                                                           :backend gptel-backend
                                                                                           :model gptel-model
                                                                                           :data (list :messages [(:role "user" :content "hi")])
                                                                                           :tools (list base-tool)
                                                                                           :tool-use (list tool-call)))))
                                                              (setq-local gptel-include-tool-results 'auto)
                                                              (setq-local macher-agent-active-fsm block-fsm)
                                                              (macher-agent-fsm-hijack-transform block-fsm)
                                                              (cl-letf (((symbol-function 'gptel--fsm-transition) #'ignore)
                                                                        ((symbol-function 'gptel--inject-prompt) #'ignore)
                                                                        ((symbol-function 'gptel--parse-tool-results) (lambda (&rest _) nil)))
                                                                (gptel--handle-pre-tool block-fsm)
                                                                ;; Upstream gptel pushed (nil (:path "secret.txt") "<tool_call_error>...")
                                                                (expect (car (car (plist-get (gptel-fsm-info block-fsm) :tool-result))) :to-be nil)
                                                                ;; Running TRET sanitiser + gptel--handle-tool-result under 'auto must not throw
                                                                (macher-agent-sanitize-fsm-tool-results block-fsm)
                                                                (gptel--handle-tool-result block-fsm)
                                                                ;; Also verify full display body under gptel-include-tool-results = t
                                                                (setq-local gptel-include-tool-results t)
                                                                (gptel--handle-tool-result block-fsm))
                                                              (expect (buffer-string) :to-match "unauthorised_write_tool")
                                                              (expect (buffer-string) :to-match "<tool_call_error>")
                                                              (setq-local macher-agent-active-fsm nil)))
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (set-default 'gptel-directives orig-directives)))))

          ;; --------------------------------------------------------------------------
          ;; SKL-47
          ;; --------------------------------------------------------------------------
          (it "SKL-47: honours gptel-menu preset switches over stale macher-agent-presets, google-search state, and exclusive toolless skills"
              (require 'gptel-gemini)
              (macher-agent-test-with-git-workspace (root ctx owner-buf)
                                                    (let* ((saved-global-skills macher-agent-global-skills-alist)
                                                           (orig-known-presets (copy-alist (default-value 'gptel--known-presets)))
                                                           (orig-known-tools (copy-tree (default-value 'gptel--known-tools)))
                                                           (orig-directives (copy-alist (default-value 'gptel-directives)))
                                                           (gptel--known-backends nil)
                                                           (openai (gptel-make-openai "Skl47OpenAI" :models '(gpt-4o gpt-4o-mini) :stream nil))
                                                           (gemini (gptel-make-gemini "Skl47Gemini" :models '(gemini-flash-latest) :stream nil))
                                                           (tool-coder (gptel-make-tool
                                                                        :name "skl47_write"
                                                                        :description "Write tool"
                                                                        :category "workspace"
                                                                        :args nil
                                                                        :function (lambda () "wrote")))
                                                           (tool-reader (gptel-make-tool
                                                                         :name "skl47_read"
                                                                         :description "Read tool"
                                                                         :category "workspace"
                                                                         :args nil
                                                                         :function (lambda () "read")))
                                                           (ws-skills-dir (expand-file-name "skills" root)))
                                                      (unwind-protect
                                                          (progn
                                                            (make-directory (expand-file-name "skl47-coder" ws-skills-dir) t)
                                                            (make-directory (expand-file-name "skl47-reader" ws-skills-dir) t)
                                                            (make-directory (expand-file-name "skl47-excl-empty" ws-skills-dir) t)
                                                            (make-directory (expand-file-name "skl47-explicit-empty" ws-skills-dir) t)
                                                            (make-directory (expand-file-name "skl47-additive" ws-skills-dir) t)
                                                            (make-directory (expand-file-name "skl47-search" ws-skills-dir) t)
                                                            (with-temp-file (expand-file-name "skl47-coder/SKILL.md" ws-skills-dir)
                                                              (insert "---\n"
                                                                      "name: skl47-coder\n"
                                                                      "description: Coder skill\n"
                                                                      "model: gpt-4o\n"
                                                                      "exclusive: true\n"
                                                                      "boot-directive: BOOT CODER\n"
                                                                      "allowed-tools:\n"
                                                                      "  - skl47_write\n"
                                                                      "ptc-primitives:\n"
                                                                      "  - skl47_write\n"
                                                                      "---\n"
                                                                      "Coder system prompt.\n"))
                                                            (with-temp-file (expand-file-name "skl47-reader/SKILL.md" ws-skills-dir)
                                                              (insert "---\n"
                                                                      "name: skl47-reader\n"
                                                                      "description: Reader skill\n"
                                                                      "model: gpt-4o-mini\n"
                                                                      "allowed-tools:\n"
                                                                      "  - skl47_read\n"
                                                                      "---\n"
                                                                      "Reader system prompt.\n"))
                                                            (with-temp-file (expand-file-name "skl47-excl-empty/SKILL.md" ws-skills-dir)
                                                              (insert "---\n"
                                                                      "name: skl47-excl-empty\n"
                                                                      "description: Exclusive toolless\n"
                                                                      "exclusive: true\n"
                                                                      "---\n"
                                                                      "Exclusive toolless prompt.\n"))
                                                            (with-temp-file (expand-file-name "skl47-explicit-empty/SKILL.md" ws-skills-dir)
                                                              (insert "---\n"
                                                                      "name: skl47-explicit-empty\n"
                                                                      "description: Explicit empty tools\n"
                                                                      "allowed-tools: []\n"
                                                                      "---\n"
                                                                      "Explicit empty prompt.\n"))
                                                            (with-temp-file (expand-file-name "skl47-additive/SKILL.md" ws-skills-dir)
                                                              (insert "---\n"
                                                                      "name: skl47-additive\n"
                                                                      "description: Additive no-tools skill\n"
                                                                      "---\n"
                                                                      "Additive prompt.\n"))
                                                            (with-temp-file (expand-file-name "skl47-search/SKILL.md" ws-skills-dir)
                                                              (insert "---\n"
                                                                      "name: skl47-search\n"
                                                                      "description: Server search\n"
                                                                      "model: gemini-flash-latest\n"
                                                                      "exclusive: true\n"
                                                                      "disable-local-tools: true\n"
                                                                      "allowed-tools:\n"
                                                                      "  - google_search\n"
                                                                      "---\n"
                                                                      "Search prompt.\n"))

                                                            (with-current-buffer owner-buf
                                                              (markdown-mode)
                                                              (gptel-mode 1)
                                                              (macher-agent-mode 1)
                                                              (setq-local gptel-backend openai)
                                                              (setq-local gptel-model 'gpt-4o)
                                                              (macher-agent-register-tool-in-gptel-tables tool-coder "workspace")
                                                              (macher-agent-register-tool-in-gptel-tables tool-reader "workspace")
                                                              (macher-agent-refresh-skills-and-tools ctx)

                                                              ;; Known presets: exclusive/empty-tools get :tools nil, additive omits :tools, none emit :use-tools t
                                                              (let ((excl-entry (alist-get 'skl47-excl-empty gptel--known-presets))
                                                                    (empty-entry (alist-get 'skl47-explicit-empty gptel--known-presets))
                                                                    (add-entry (alist-get 'skl47-additive gptel--known-presets))
                                                                    (reader-entry (alist-get 'skl47-reader gptel--known-presets)))
                                                                (expect (plist-member excl-entry :tools) :not :to-be nil)
                                                                (expect (plist-get excl-entry :tools) :to-be nil)
                                                                (expect (plist-member empty-entry :tools) :not :to-be nil)
                                                                (expect (plist-get empty-entry :tools) :to-be nil)
                                                                (expect (plist-member add-entry :tools) :to-be nil)
                                                                (expect (plist-member reader-entry :use-tools) :to-be nil)
                                                                (expect (plist-member excl-entry :use-tools) :to-be nil))

                                                              ;; 1. Activate skl47-coder via macher-agent-use-skill
                                                              (macher-agent-use-skill 'skl47-coder owner-buf)
                                                              (setq-local gptel--request-params '(:top_p 0.85))
                                                              (setq-local macher-agent-preset-request-params '(:top_p 0.85))
                                                              (expect macher-agent-presets :to-equal '(skl47-coder))

                                                              ;; Switch via gptel-menu (gptel--apply-preset) to non-exclusive skl47-reader
                                                              (gptel--apply-preset
                                                               'skl47-reader
                                                               (lambda (sym val) (set (make-local-variable sym) val)))
                                                              (insert "### Read only please\n")
                                                              (let* ((fsm (gptel-request nil
                                                                            :dry-run t
                                                                            :stream nil
                                                                            :transforms gptel-prompt-transform-functions))
                                                                     (inf (gptel-fsm-info fsm))
                                                                     (tool-names (mapcar #'gptel-tool-name (plist-get inf :tools))))
                                                                (expect (plist-get inf :model) :to-equal 'gpt-4o-mini)
                                                                (expect (plist-get inf :system) :to-equal "Reader system prompt.")
                                                                (expect (plist-get inf :system) :not :to-match "BOOT CODER")
                                                                (expect (plist-get inf :request-params) :to-be nil)
                                                                (expect tool-names :to-equal '("skl47_read"))
                                                                (expect macher-agent-presets :to-be nil)
                                                                (expect macher-agent-boot-directive :to-be nil)
                                                                (expect macher-agent-force-boot-directive :to-be nil)
                                                                (expect (local-variable-p 'gptel--request-params owner-buf) :to-be nil))

                                                              ;; 2. Switch from skl47-search (disable-local-tools) to skl47-reader via gptel-menu with macher-agent-presets nil
                                                              (erase-buffer)
                                                              (gptel--apply-preset
                                                               'skl47-search
                                                               (lambda (sym val) (set (make-local-variable sym) val)))
                                                              (expect gptel-use-tools :to-be nil)
                                                              (expect gptel--request-params :to-equal '(:tools [(:google_search nil)]))
                                                              (gptel--apply-preset
                                                               'skl47-reader
                                                               (lambda (sym val) (set (make-local-variable sym) val)))
                                                              (insert "### Back to local tools\n")
                                                              (let* ((fsm-back (gptel-request nil
                                                                                 :dry-run t
                                                                                 :stream nil
                                                                                 :transforms gptel-prompt-transform-functions))
                                                                     (inf-back (gptel-fsm-info fsm-back))
                                                                     (data-back (plist-get inf-back :data)))
                                                                (expect (plist-get inf-back :model) :to-equal 'gpt-4o-mini)
                                                                (expect (plist-get inf-back :request-params) :to-be nil)
                                                                (expect (mapcar #'gptel-tool-name (plist-get inf-back :tools))
                                                                        :to-equal '("skl47_read"))
                                                                (expect (plist-member data-back :tools) :not :to-be nil)
                                                                (expect (json-serialize data-back :null-object :null :false-object :json-false)
                                                                        :not :to-match "google_search"))

                                                              ;; 3. Manual gptel-use-tools = nil without stale server-tool params keeps tools suppressed; 'force stays 'force
                                                              (setq-local gptel--request-params nil)
                                                              (setq-local gptel-use-tools nil)
                                                              (let* ((fsm-sup (gptel-request nil
                                                                                :dry-run t
                                                                                :stream nil
                                                                                :transforms gptel-prompt-transform-functions))
                                                                     (data-sup (plist-get (gptel-fsm-info fsm-sup) :data)))
                                                                (expect (plist-get data-sup :tools) :to-be nil))
                                                              (setq-local gptel-use-tools 'force)
                                                              (gptel--apply-preset
                                                               'skl47-reader
                                                               (lambda (sym val) (set (make-local-variable sym) val)))
                                                              (expect gptel-use-tools :to-equal 'force)

                                                              ;; 4. Switching to exclusive toolless clears gptel-tools
                                                              (gptel--apply-preset
                                                               'skl47-excl-empty
                                                               (lambda (sym val) (set (make-local-variable sym) val)))
                                                              (expect gptel-tools :to-be nil)

                                                              ;; Non-exclusive preset without tools preserves buffer-local state
                                                              (gptel--apply-preset
                                                               'skl47-additive
                                                               (lambda (sym val) (set (make-local-variable sym) val)))
                                                              (expect gptel--preset :to-equal 'skl47-additive)

                                                              ;; 5. macher-agent-sync-preset-before-save clears stale macher-agent-presets when gptel--preset matches prompt
                                                              (setq-local macher-agent-presets '(skl47-coder))
                                                              (macher-agent-apply-preset '(skl47-reader))
                                                              (setq-local macher-agent-presets '(skl47-coder))
                                                              (macher-agent-sync-preset-before-save)
                                                              (expect gptel--preset :to-equal 'skl47-reader)
                                                              (expect macher-agent-presets :to-be nil)

                                                              ;; 6. Diverging from disable-local-tools preset restores gptel-use-tools across Turn 1 and Turn 2,
                                                              ;;    and nil preset-rp does not kill explicit buffer-local gptel--request-params = nil
                                                              (setq-local gptel-use-tools (default-value 'gptel-use-tools))
                                                              (macher-agent-use-skill 'skl47-search owner-buf)
                                                              (setq-local gptel-use-tools nil)
                                                              (gptel--apply-preset
                                                               'skl47-reader
                                                               (lambda (sym val) (set (make-local-variable sym) val)))
                                                              (erase-buffer)
                                                              (insert "### Turn 1 after search divergence\n")
                                                              (let* ((fsm-d1 (gptel-request nil
                                                                               :dry-run t
                                                                               :stream nil
                                                                               :transforms gptel-prompt-transform-functions))
                                                                     (data-d1 (plist-get (gptel-fsm-info fsm-d1) :data)))
                                                                (expect gptel-use-tools :to-equal (default-value 'gptel-use-tools))
                                                                (expect (plist-get data-d1 :tools) :not :to-be nil))
                                                              (erase-buffer)
                                                              (insert "### Turn 2 after search divergence\n")
                                                              (let* ((fsm-d2 (gptel-request nil
                                                                               :dry-run t
                                                                               :stream nil
                                                                               :transforms gptel-prompt-transform-functions))
                                                                     (inf-d2 (gptel-fsm-info fsm-d2))
                                                                     (data-d2 (plist-get inf-d2 :data)))
                                                                (expect (mapcar #'gptel-tool-name (plist-get inf-d2 :tools))
                                                                        :to-equal '("skl47_read"))
                                                                (expect (plist-get data-d2 :tools) :not :to-be nil))
                                                              (let ((orig-default-rp (default-value 'gptel--request-params)))
                                                                (unwind-protect
                                                                    (progn
                                                                      (set-default 'gptel--request-params '(:top_p 0.5))
                                                                      (setq-local macher-agent-presets '(skl47-coder))
                                                                      (setq-local macher-agent-preset-request-params nil)
                                                                      (setq-local gptel--request-params nil)
                                                                      (setq-local gptel--preset 'skl47-reader)
                                                                      (macher-agent--reconcile-diverged-presets owner-buf ctx)
                                                                      (expect (local-variable-p 'gptel--request-params owner-buf) :to-be t)
                                                                      (expect gptel--request-params :to-be nil))
                                                                  (set-default 'gptel--request-params orig-default-rp)))))
                                                        (ignore gemini)
                                                        (setq macher-agent-global-skills-alist saved-global-skills)
                                                        (set-default 'gptel--known-presets orig-known-presets)
                                                        (set-default 'gptel--known-tools orig-known-tools)
                                                        (set-default 'gptel-directives orig-directives)
                                                        (dolist (s '(skl47-coder skl47-reader skl47-excl-empty skl47-explicit-empty skl47-additive skl47-search))

  ;; --------------------------------------------------------------------------
                                                          (cl-remprop s 'macher-agent-preset-meta))))))

  ;; SKL-48: Dirty-checked skill init skips unchanged turns & hot-reloads edits
  ;; --------------------------------------------------------------------------
  (it "SKL-48: skips redundant skill reload when clean and immediately reloads on-the-fly disk or VFS edits"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (let* ((ext-dir (make-temp-file "skl48-ext-" t))
             (ext-skill-dir (expand-file-name "ext-hot" ext-dir))
             (ext-scripts-dir (expand-file-name "scripts" ext-dir))
             (ws-skill-dir (expand-file-name "skills/ws-hot" root))
             (ws-cmd-dir (expand-file-name "commands" root))
             (saved-skill-dirs macher-agent-skill-directories))
        (unwind-protect
            (progn
              (make-directory ext-skill-dir t)
              (make-directory ext-scripts-dir t)
              (make-directory ws-skill-dir t)
              (make-directory ws-cmd-dir t)
              (with-temp-file (expand-file-name "SKILL.md" ws-skill-dir)
                (insert "---\nname: ws-hot\n---\nWS_BODY_V1\n"))
              (with-temp-file (expand-file-name "SKILL.md" ext-skill-dir)
                (insert "---\nname: ext-hot\nallowed-tools: [skl48_ext_tool]\n---\nEXT_BODY_V1\n"))
              (with-temp-file (expand-file-name "skl48_ext_tool.el" ext-scripts-dir)
                (insert "(setq macher-agent-skl48-ext-tool (gptel-make-tool :name \"skl48_ext_tool\" :description \"v1\" :category \"meta\" :args nil :function (lambda () \"TOOL_V1\")))\n"))
              (setq macher-agent-skill-directories (list ext-dir))
              (with-current-buffer owner-buf
                (macher-agent-refresh-skills-and-tools ctx)
                ;; 1. Steady state: unchanged sources skip full reload
                (expect (macher-agent-initialize-skills-if-stale ctx) :to-be nil)
                (expect (plist-get (alist-get 'ws-hot (macher-agent-context-skills ctx)) :system)
                        :to-match "WS_BODY_V1")
                (expect (funcall (gptel-tool-function (gptel-get-tool "skl48_ext_tool")))
                        :to-equal "TOOL_V1")
                ;; 2. Edit workspace SKILL.md on disk -> immediate reload
                (with-temp-file (expand-file-name "SKILL.md" ws-skill-dir)
                  (insert "---\nname: ws-hot\n---\nWS_BODY_V2_EDITED\n"))
                (expect (macher-agent-initialize-skills-if-stale ctx) :to-be t)
                (expect (plist-get (alist-get 'ws-hot (macher-agent-context-skills ctx)) :system)
                        :to-match "WS_BODY_V2_EDITED")
                (expect (macher-agent-initialize-skills-if-stale ctx) :to-be nil)
                ;; 3. Create new workspace command on disk -> immediate reload
                (with-temp-file (expand-file-name "hot-cmd.md" ws-cmd-dir)
                  (insert "---\nname: hot-cmd\n---\nCMD_HOT_V1\n"))
                (expect (macher-agent-initialize-skills-if-stale ctx) :to-be t)
                (expect (alist-get 'hot-cmd (macher-agent-context-skills ctx)) :not :to-be nil)
                ;; 4. Edit external user skill script on disk -> immediate reload & new tool fn
                (with-temp-file (expand-file-name "skl48_ext_tool.el" ext-scripts-dir)
                  (insert "(setq macher-agent-skl48-ext-tool (gptel-make-tool :name \"skl48_ext_tool\" :description \"v2\" :category \"meta\" :args nil :function (lambda () \"TOOL_V2_HOT\")))\n"))
                (expect (macher-agent-initialize-skills-if-stale ctx) :to-be t)
                (expect (funcall (gptel-tool-function (gptel-get-tool "skl48_ext_tool")))
                        :to-equal "TOOL_V2_HOT")
                (expect (macher-agent-initialize-skills-if-stale ctx) :to-be nil)))
          (setq macher-agent-skill-directories saved-skill-dirs)
          (when (file-directory-p ext-dir)
            (delete-directory ext-dir t))))))


  ;; --------------------------------------------------------------------------
  ;; SKL-49: Externalise oversized gptel--bounds & recover legacy split footers
  ;; --------------------------------------------------------------------------
  (it "SKL-49: keeps Local Variables under Emacs 3000-byte window on save and recovers legacy split footers on load"
    (macher-agent-test-with-git-workspace (root _ctx _owner-buf)
      (let* ((chat-file (expand-file-name "long-chat.md" root))
             (small-file (expand-file-name "small-chat.md" root))
             (legacy-file (expand-file-name "legacy-split.md" root))
             (big-bounds
              (list (cons 'response
                          (cl-loop for i from 1 to 400
                                   collect (list (* i 10) (+ (* i 10) 5))))))
             (big-bounds-str (prin1-to-string big-bounds))
             (saved-hack-hook (copy-sequence hack-local-variables-hook))
             (hook-saw-model nil)
             (observer-fn (lambda ()
                            (when (local-variable-p 'gptel-model)
                              (setq hook-saw-model gptel-model)))))
        (expect (> (length big-bounds-str) 3500) :to-be t)
        (unwind-protect
            (progn
              (macher-agent-install)
              (add-hook 'hack-local-variables-hook observer-fn 0)
              ;; 1. Save with oversized gptel--bounds + missing-preset-tools -> single compact Local Variables block
              (let ((buf (find-file-noselect chat-file)))
                (unwind-protect
                    (with-current-buffer buf
                      (erase-buffer)
                      (insert (make-string 5000 ?a) "\n\n")
                      (add-text-properties 10 15 '(gptel response))
                      (setq-local comment-start "<!-- ")
                      (setq-local comment-end " -->")
                      (setq-local gptel-mode t)
                      (setq-local gptel-model 'gemini-4-neon)
                      (setq-local gptel--backend-name "Gemini")
                      (setq-local gptel--preset 'default)
                      (setq-local gptel-tools
                                  (list (gptel-make-tool :name "skl49_tool" :description "t"
                                                         :category "test" :args nil :function #'ignore)))
                      (cl-letf (((symbol-function 'gptel-get-preset)
                                 (lambda (_sym) '(:description "no tools"))))
                        (macher-agent-prepare-local-vars-before-save)
                        (let ((print-escape-newlines t))
                          (add-file-local-variable 'gptel--preset 'default)
                          (add-file-local-variable 'gptel-model 'gemini-4-neon)
                          (add-file-local-variable 'gptel--backend-name "Gemini")
                          (add-file-local-variable 'gptel--bounds big-bounds))
                        (macher-agent-externalize-oversized-gptel-bounds)
                        (macher-agent-persist-missing-preset-tools))
                      (save-buffer)
                      (let* ((txt (buffer-string))
                             (lv-count 0)
                             (lv-pos nil))
                        (with-temp-buffer
                          (insert txt)
                          (goto-char (point-min))
                          (while (search-forward "<!-- Local Variables: -->" nil t)
                            (setq lv-count (1+ lv-count)
                                  lv-pos (match-beginning 0))))
                        (expect lv-count :to-equal 1)
                        (expect (< (- (length txt) lv-pos) 500) :to-be t)
                        (expect txt :to-match "<!-- macher-agent-gptel-bounds: ")
                        (expect txt :to-match "gptel--tool-names: (\"skl49_tool\")")))
                  (when (buffer-live-p buf) (kill-buffer buf))))
              ;; 2. Reopen saved file: native hack-local-variables + depth -50 restore recovers bounds & model before depth 0 hook
              (setq hook-saw-model nil)
              (let ((buf2 (find-file-noselect chat-file)))
                (unwind-protect
                    (with-current-buffer buf2
                      (let ((enable-local-variables :all))
                        (hack-local-variables))
                      (expect hook-saw-model :to-equal 'gemini-4-neon)
                      (expect (local-variable-p 'gptel-model) :to-be t)
                      (expect gptel-model :to-equal 'gemini-4-neon)
                      (expect gptel--tool-names :to-equal '("skl49_tool"))
                      (expect gptel--bounds :to-equal big-bounds))
                  (when (buffer-live-p buf2) (kill-buffer buf2))))
              ;; 3. Legacy split/oversized file (> 3000 bytes from EOF): recovers all vars before depth 0 hook & coalesces on save
              (with-temp-file legacy-file
                (insert (make-string 5000 ?b) "\n\n"
                        "<!-- Local Variables: -->\n"
                        "<!-- gptel--preset: default -->\n"
                        "<!-- gptel-model: gemini-4-neon -->\n"
                        "<!-- gptel--backend-name: \"Gemini\" -->\n"
                        "<!-- gptel--bounds: " big-bounds-str " -->\n"
                        "<!-- End: -->\n\n"
                        "<!-- Local Variables: -->\n"
                        "<!-- gptel--tool-names: (\"skl49_tool\") -->\n"
                        "<!-- End: -->\n"))
              (setq hook-saw-model nil)
              (let ((buf3 (find-file-noselect legacy-file)))
                (unwind-protect
                    (with-current-buffer buf3
                      (let ((enable-local-variables :all))
                        (hack-local-variables))
                      (expect hook-saw-model :to-equal 'gemini-4-neon)
                      (expect gptel-model :to-equal 'gemini-4-neon)
                      (expect gptel--backend-name :to-equal "Gemini")
                      (expect gptel--tool-names :to-equal '("skl49_tool"))
                      (expect gptel--bounds :to-equal big-bounds)
                      ;; Re-save coalesces split blocks into one compact footer
                      (setq-local gptel-mode t)
                      (macher-agent-prepare-local-vars-before-save)
                      (let ((print-escape-newlines t))
                        (add-file-local-variable 'gptel--bounds big-bounds))
                      (macher-agent-externalize-oversized-gptel-bounds)
                      (let ((lv-count 0))
                        (save-excursion
                          (goto-char (point-min))
                          (while (search-forward "<!-- Local Variables: -->" nil t)
                            (setq lv-count (1+ lv-count))))
                        (expect lv-count :to-equal 1)))
                  (when (buffer-live-p buf3) (kill-buffer buf3))))
              ;; 4. Small chat (< 2000 bytes): keeps standard gptel--bounds inside Local Variables
              (let ((buf4 (find-file-noselect small-file)))
                (unwind-protect
                    (with-current-buffer buf4
                      (erase-buffer)
                      (insert "hello\n\n")
                      (setq-local comment-start "<!-- ")
                      (setq-local comment-end " -->")
                      (setq-local gptel-mode t)
                      (let ((print-escape-newlines t))
                        (add-file-local-variable 'gptel-model 'gemini-4-neon)
                        (add-file-local-variable 'gptel--bounds '((response (1 5)))))
                      (macher-agent-externalize-oversized-gptel-bounds)
                      (expect (buffer-string) :not :to-match "macher-agent-gptel-bounds:")
                      (expect (buffer-string) :to-match "gptel--bounds: ((response (1 5)))"))
                  (when (buffer-live-p buf4) (kill-buffer buf4)))))
          (setq hack-local-variables-hook saved-hack-hook)))))
)

(provide 'test-skills)
;;; test-skills.el ends here
