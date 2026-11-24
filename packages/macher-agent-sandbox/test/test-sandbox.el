;;; test-sandbox.el --- Sandbox skill lifecycle, override isolation, and boundaries -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Standalone Buttercup specs for `macher-agent-sandbox':
;; - SKL-42 workspace `ptc-function' override isolation
;; - `:ptc-primitives' preset composition, `:exclusive' / `:disable-local-tools' clearing,
;;   `macher-agent-apply-payload-hook', `macher-agent-preset-cleared-hook',
;;   and `use_skill' baseline capture/restore
;; - Pure `gptel-tool' loading & execution of `ptc_execution.el' and 5 orchestration tools
;; - Install/uninstall idempotency and custom `macher-agent-evaluate-tool-forms-function' preservation
;; - Static package boundary verification

;;; Code:

(unless (featurep 'macher-agent-sandbox-test-setup)
  (load (expand-file-name "macher-agent-sandbox-test-setup.el"
                          (file-name-directory (or load-file-name buffer-file-name)))
        nil t))

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-helpers)
(require 'macher-agent-sandbox)

(describe "Macher-Agent Sandbox lifecycle, skills, and boundaries"

  (before-each
    (macher-agent-test-reset-env)
    (macher-agent-sandbox-install))

  ;; --------------------------------------------------------------------------
  ;; SKL-42: Workspace ptc-function override isolation
  ;; --------------------------------------------------------------------------
  (it "SKL-42: isolates workspace ptc-function overrides via macher-agent-evaluate-tool-forms-function without mutating global symbol plists"
    (macher-agent-test-with-git-workspace
        (root ctx owner-buf
              '(("skills/scripts/ws_override_demo.el" .
                 "(setq macher-agent-ws-override-demo-tool
                        (gptel-make-tool
                         :name \"ws_override_demo\"
                         :description \"Workspace override demo\"
                         :args '((:name \"input\" :type \"string\"))
                         :function (lambda (input) (format \"WS-FN:%s\" input))))
                  (put 'macher-agent-ws-override-demo-tool 'ptc-function
                       (lambda (input &optional _ctx) (format \"WS-PTC:%s\" input)))")
                ("skills/scripts/ws_pure_override.el" .
                 "(setq macher-agent-ws-pure-override-tool
                        (gptel-make-tool
                         :name \"ws_pure_override\"
                         :description \"Workspace pure override without ptc-function\"
                         :args '((:name \"input\" :type \"string\"))
                         :function (lambda (input) (format \"WS-PURE:%s\" input))))")))
      (let ((global-demo-fn (lambda (input &optional _ctx) (format "GLOBAL-DEMO:%s" input)))
            (global-pure-fn (lambda (input &optional _ctx) (format "GLOBAL-PURE:%s" input))))
        (unwind-protect
            (progn
              (put 'macher-agent-ws-override-demo-tool 'ptc-function global-demo-fn)
              (put 'macher-agent-ws-pure-override-tool 'ptc-function global-pure-fn)

              ;; 1. Workspace override WITH ptc-function: global plist untouched, PTC runs workspace fn
              (let ((ws-tool (macher-agent-resolve-tool "ws_override_demo" nil nil ctx))
                    (ptc-res nil)
                    (ptc-err nil))
                (expect (macher-agent-tool-valid-p ws-tool) :to-be t)
                (expect (get 'macher-agent-ws-override-demo-tool 'ptc-function) :to-be global-demo-fn)
                (expect (functionp (gethash ws-tool macher-agent-sandbox-workspace-tool-overrides)) :to-be t)
                (with-current-buffer owner-buf
                  (setq-local macher-agent-sandbox-active-ptc-primitives '(ws_override_demo))
                  (macher-agent-sandbox-execute-ptc-script
                   "(ws-override-demo \"alpha\")" ctx owner-buf
                   (lambda (v) (setq ptc-res v))
                   (lambda (e) (setq ptc-err e))))
                (expect ptc-err :to-be nil)
                (expect ptc-res :to-equal "WS-PTC:alpha")
                ;; Global symbol plist still intact after PTC run
                (expect (funcall (get 'macher-agent-ws-override-demo-tool 'ptc-function) "beta")
                        :to-equal "GLOBAL-DEMO:beta"))

              ;; 2. Workspace override WITHOUT ptc-function: recorded as :none, falls back to workspace :function
              (let ((ws-pure (macher-agent-resolve-tool "ws_pure_override" nil nil ctx))
                    (pure-res nil)
                    (pure-err nil))
                (expect (macher-agent-tool-valid-p ws-pure) :to-be t)
                (expect (get 'macher-agent-ws-pure-override-tool 'ptc-function) :to-be global-pure-fn)
                (expect (gethash ws-pure macher-agent-sandbox-workspace-tool-overrides) :to-equal :none)
                (with-current-buffer owner-buf
                  (setq-local macher-agent-sandbox-active-ptc-primitives '(ws_pure_override))
                  (macher-agent-sandbox-execute-ptc-script
                   "(ws-pure-override \"gamma\")" ctx owner-buf
                   (lambda (v) (setq pure-res v))
                   (lambda (e) (setq pure-err e))))
                (expect pure-err :to-be nil)
                (expect pure-res :to-equal "WS-PURE:gamma")))
          (cl-remprop 'macher-agent-ws-override-demo-tool 'ptc-function)
          (cl-remprop 'macher-agent-ws-pure-override-tool 'ptc-function)))))

  ;; --------------------------------------------------------------------------
  ;; Preset composition, exclusive/disable-local-tools clearing, apply-payload,
  ;; preset-cleared, and use_skill baseline capture/restore
  ;; --------------------------------------------------------------------------
  (it "SBX-LIFE-01: composes :ptc-primitives, clears on :exclusive/:disable-local-tools, and preserves baseline across use_skill"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (with-current-buffer owner-buf
        (unwind-protect
            (progn
              (gptel-make-preset 'sbx-ptc-skill
                :description "PTC Skill"
                :system "PTC Skill"
                :tools '("spawn_subagent"))
              (macher-agent-set-preset-meta 'sbx-ptc-skill :ptc-primitives '(spawn_subagent))
              (gptel-make-preset 'sbx-plain-excl
                :description "Exclusive Plain Skill"
                :system "Exclusive Plain Skill"
                :tools '("read_tool_schema"))
              (macher-agent-set-preset-meta 'sbx-plain-excl :exclusive t)
              (gptel-make-preset 'sbx-toolless
                :description "Toolless Skill"
                :system "Toolless Skill")
              (macher-agent-set-preset-meta 'sbx-toolless :disable-local-tools t)

              ;; 1. Applying PTC skill injects valid ptc_execution tool and populates buffer/ctx primitives via hook
              (let ((payload (macher-agent-apply-preset '(sbx-ptc-skill))))
                (expect (plist-get payload :ptc-primitives) :to-equal '(spawn_subagent))
                (expect (cl-some (lambda (tl)
                                   (and (macher-agent-tool-valid-p tl)
                                        (equal (gptel-tool-name tl) "ptc_execution")))
                                 (plist-get payload :tools))
                        :to-be-truthy)
                (expect macher-agent-sandbox-active-ptc-primitives :to-equal '(spawn_subagent))
                (expect (plist-get (macher-agent-sandbox-get-state ctx) :ptc-primitives)
                        :to-equal '(spawn_subagent)))

              ;; 2. Exclusive preset clears :ptc-primitives and omits ptc_execution
              (let ((excl-payload (macher-agent-compose-payload
                                   (list :tools gptel-tools
                                         :ptc-primitives '(spawn_subagent))
                                   '(sbx-plain-excl))))
                (expect (plist-get excl-payload :ptc-primitives) :to-be nil)
                (expect (cl-some (lambda (tl)
                                   (equal (macher-agent-canonical-tool-name tl) "ptc_execution"))
                                 (plist-get excl-payload :tools))
                        :to-be nil))

              ;; 3. :disable-local-tools clears :ptc-primitives and :tools locally via apply-payload hook
              (let ((tl-payload (macher-agent-apply-preset '(sbx-toolless))))
                (expect (plist-get tl-payload :ptc-primitives) :to-be nil)
                (expect (plist-get tl-payload :tools) :to-be nil)
                (expect macher-agent-sandbox-active-ptc-primitives :to-be nil)
                (expect (plist-get (macher-agent-sandbox-get-state ctx) :ptc-primitives) :to-be nil))

              ;; 4. Re-apply PTC skill, then fire macher-agent-preset-cleared-hook -> clears both buffer & ctx
              (macher-agent-apply-preset '(sbx-ptc-skill))
              (expect macher-agent-sandbox-active-ptc-primitives :to-equal '(spawn_subagent))
              (run-hook-with-args 'macher-agent-preset-cleared-hook owner-buf ctx)
              (expect macher-agent-sandbox-active-ptc-primitives :to-be nil)
              (expect (plist-get (macher-agent-sandbox-get-state ctx) :ptc-primitives) :to-be nil)

              ;; 5. use_skill baseline capture & restore preserves buffer-local PTC primitives even when macher-agent-presets is nil
              (setq-local macher-agent-presets nil)
              (setq-local macher-agent-sandbox-active-ptc-primitives '(custom_baseline_prim))
              (let* ((use-skill-tool (macher-agent-resolve-tool "use_skill" nil nil ctx))
                     (fsm (gptel-make-fsm :info (list :macher-agent-context ctx :buffer owner-buf)))
                     (macher-agent-active-fsm fsm))
                (expect (macher-agent-tool-valid-p use-skill-tool) :to-be t)
                (cl-letf (((symbol-function 'macher-agent-get-active-fsm) (lambda () fsm)))
                  ;; Switch to exclusive non-PTC skill
                  (let ((activate-out (macher-agent-test-invoke-tool use-skill-tool "sbx-plain-excl" nil)))
                    (expect activate-out :to-match "Activated skill 'sbx-plain-excl'")
                    (expect macher-agent-sandbox-active-ptc-primitives :to-be nil))
                  ;; Restore with done=t (orig presets is nil, so restoration comes strictly from baseline-restore hook)
                  (let ((restore-out (macher-agent-test-invoke-tool use-skill-tool nil t)))
                    (expect restore-out :to-match "Restored original presets")
                    (expect macher-agent-sandbox-active-ptc-primitives :to-equal '(custom_baseline_prim)))))

              ;; 6. Transmission respects :exclusive t on active gptel preset via macher-agent-current-gptel-preset
              (macher-agent-apply-preset '(sbx-plain-excl))
              (expect (macher-agent-current-gptel-preset owner-buf) :to-equal 'sbx-plain-excl)
              (setq-local macher-agent-sandbox-active-ptc-primitives '(spawn_subagent))
              (let* ((tx-state (make-macher-agent-transmission-state
                                :target-buffer owner-buf
                                :context ctx
                                :tools (list (macher-agent-resolve-tool "spawn_subagent" nil nil ctx))))
                     (out-state (macher-agent-sandbox-append-ptc-to-transmission tx-state)))
                (expect (plist-get (plist-get (macher-agent-transmission-state-plugins out-state) :sandbox)
                                   :ptc-primitives)
                        :to-be nil)))
          (dolist (sym '(sbx-ptc-skill sbx-plain-excl sbx-toolless))
            (cl-remprop sym 'macher-agent-preset-meta))))))

  ;; --------------------------------------------------------------------------
  ;; Cross-buffer & parent-to-child :sandbox state isolation
  ;; --------------------------------------------------------------------------
  (it "SBX-ISO-01: isolates :sandbox state across cloned child contexts, sibling buffers, and transient prompt buffers"
    (macher-agent-test-with-git-workspace (root parent-ctx parent-buf)
      (let ((child-buf (generate-new-buffer "*test-sbx-iso-child*"))
            (sib-a (generate-new-buffer "*test-sbx-iso-sib-a*"))
            (sib-b (generate-new-buffer "*test-sbx-iso-sib-b*")))
        (unwind-protect
            (progn
              (gptel-make-preset 'sbx-iso-a
                :description "Sibling A PTC"
                :system "Sibling A"
                :tools '("spawn_subagent"))
              (macher-agent-set-preset-meta 'sbx-iso-a :ptc-primitives '(spawn_subagent))
              (gptel-make-preset 'sbx-iso-b
                :description "Sibling B PTC"
                :system "Sibling B"
                :tools '("send_message"))
              (macher-agent-set-preset-meta 'sbx-iso-b :ptc-primitives '(send_message))

              ;; 1. Parent PTC primitives must NOT bleed into cloned non-PTC child context/buffer
              (with-current-buffer parent-buf
                (macher-agent-apply-preset '(sbx-iso-a))
                (expect (plist-get (macher-agent-sandbox-get-state parent-ctx (buffer-name parent-buf))
                                   :ptc-primitives)
                        :to-equal '(spawn_subagent)))
              (let ((child-ctx (macher-agent-clone-context parent-ctx)))
                (setf (macher-agent-context-owner-buffer child-ctx) child-buf)
                (expect (macher-agent-sandbox-get-state child-ctx) :to-be nil)
                (expect (macher-agent-sandbox-get-state child-ctx (buffer-name child-buf)) :to-be nil)
                (with-current-buffer child-buf
                  (setq-local macher-agent-persistent-context child-ctx)
                  (let* ((tx-state (make-macher-agent-transmission-state
                                    :target-buffer child-buf
                                    :context child-ctx
                                    :tools (list (macher-agent-resolve-tool "spawn_subagent" nil nil child-ctx))))
                         (out-state (macher-agent-sandbox-append-ptc-to-transmission tx-state)))
                    (expect (plist-get (plist-get (macher-agent-transmission-state-plugins out-state) :sandbox)
                                       :ptc-primitives)
                            :to-be nil)))
                ;; Parent state remains intact
                (expect (plist-get (macher-agent-sandbox-get-state parent-ctx (buffer-name parent-buf))
                                   :ptc-primitives)
                        :to-equal '(spawn_subagent)))

              ;; 2. Sibling buffers sharing a workspace context do not clobber each other's :ptc-primitives
              (let ((shared-ctx (macher-agent-make-context
                                 :id "ctx-sbx-iso-shared"
                                 :project-root root
                                 :origin-buffer sib-a)))
                (setf (macher-agent-context-owner-buffer shared-ctx) sib-a)
                (with-current-buffer sib-a
                  (setq-local macher-agent-persistent-context shared-ctx)
                  (macher-agent-apply-preset '(sbx-iso-a)))
                (with-current-buffer sib-b
                  (setq-local macher-agent-persistent-context shared-ctx)
                  (macher-agent-apply-preset '(sbx-iso-b)))
                (expect (plist-get (macher-agent-sandbox-get-state shared-ctx (buffer-name sib-a))
                                   :ptc-primitives)
                        :to-equal '(spawn_subagent))
                (expect (plist-get (macher-agent-sandbox-get-state shared-ctx (buffer-name sib-b))
                                   :ptc-primitives)
                        :to-equal '(send_message))
                ;; Clearing presets on sib-b must NOT wipe sib-a's cached primitives
                (run-hook-with-args 'macher-agent-preset-cleared-hook sib-b shared-ctx)
                (expect (plist-get (macher-agent-sandbox-get-state shared-ctx (buffer-name sib-b))
                                   :ptc-primitives)
                        :to-be nil)
                (expect (plist-get (macher-agent-sandbox-get-state shared-ctx (buffer-name sib-a))
                                   :ptc-primitives)
                        :to-equal '(spawn_subagent))

                ;; 3. Transient prompt-copy buffer applying payload must NOT overwrite owner buffer's state
                (let ((fsm (gptel-make-fsm :info (list :buffer sib-a :macher-agent-context shared-ctx))))
                  (cl-letf (((symbol-function 'macher-agent-get-active-fsm) (lambda () fsm)))
                    (with-temp-buffer
                      (setq-local macher-agent-persistent-context shared-ctx)
                      (macher-agent-apply-payload-locally (list :disable-local-tools t))
                      (macher-agent-apply-payload-locally (list :ptc-primitives '(transient_only))))))
                (expect (plist-get (macher-agent-sandbox-get-state shared-ctx (buffer-name sib-a))
                                   :ptc-primitives)
                        :to-equal '(spawn_subagent)))

              ;; 4. Backward compatibility with direct pre-populated plist in :sandbox
              (let ((legacy-ctx (macher-agent-make-context :id "ctx-sbx-iso-legacy" :origin-buffer parent-buf)))
                (macher-agent-sandbox-set-state legacy-ctx '(:ptc-primitives (legacy_prim)))
                (expect (plist-get (macher-agent-sandbox-get-state legacy-ctx) :ptc-primitives)
                        :to-equal '(legacy_prim))
                (expect (plist-get (macher-agent-sandbox-get-state legacy-ctx (buffer-name parent-buf))
                                   :ptc-primitives)
                        :to-equal '(legacy_prim))))
          (dolist (sym '(sbx-iso-a sbx-iso-b))
            (cl-remprop sym 'macher-agent-preset-meta))
          (when (buffer-live-p child-buf) (kill-buffer child-buf))
          (when (buffer-live-p sib-a) (kill-buffer sib-a))
          (when (buffer-live-p sib-b) (kill-buffer sib-b))))))

  ;; --------------------------------------------------------------------------
  ;; Transmission resolution: active gptel preset, :parents, redirected-skill,
  ;; and primitive/docstring hardening
  ;; --------------------------------------------------------------------------
  (it "SBX-TX-01: resolves active preset, :parents inheritance, and redirected-skill in transmission and hardens primitives"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (with-current-buffer owner-buf
        (unwind-protect
            (progn
              (gptel-make-preset 'sbx-tx-parent
                :description "Parent PTC"
                :system "Parent PTC"
                :tools '("spawn_subagent"))
              (macher-agent-set-preset-meta 'sbx-tx-parent :ptc-primitives '(spawn_subagent))
              (gptel-make-preset 'sbx-tx-child
                :description "Child inheriting parent PTC"
                :parents '(sbx-tx-parent)
                :system "Child PTC"
                :tools '("send_message"))
              (macher-agent-set-preset-meta 'sbx-tx-child :ptc-primitives '(send_message))
              (gptel-make-preset 'sbx-tx-excl-child
                :description "Exclusive child inheriting parent PTC"
                :parents '(sbx-tx-parent)
                :system "Exclusive Child PTC")
              (macher-agent-set-preset-meta 'sbx-tx-excl-child :exclusive t)
              (gptel-make-preset 'sbx-tx-redir
                :description "Redirected PTC skill"
                :parents '(sbx-tx-parent)
                :system "Redirected PTC")
              (macher-agent-set-preset-meta 'sbx-tx-redir :ptc-primitives '(wait_for_message))
              (gptel-make-preset 'sbx-tx-toolless
                :description "Active toolless preset"
                :system "Toolless")
              (macher-agent-set-preset-meta 'sbx-tx-toolless :disable-local-tools t)

              ;; 1. Active gptel preset :ptc-primitives extracted when :skills and :known-presets are nil
              (macher-agent-apply-preset '(sbx-tx-parent))
              (setq-local macher-agent-sandbox-active-ptc-primitives nil)
              (macher-agent-sandbox-set-state ctx nil (buffer-name owner-buf))
              (expect (macher-agent-current-gptel-preset owner-buf) :to-equal 'sbx-tx-parent)
              (let* ((tx-state (make-macher-agent-transmission-state
                                :target-buffer owner-buf
                                :context ctx
                                :skills nil
                                :known-presets nil
                                :tools (list (macher-agent-resolve-tool "spawn_subagent" nil nil ctx))))
                     (out-state (macher-agent-sandbox-append-ptc-to-transmission tx-state)))
                (expect (plist-get (plist-get (macher-agent-transmission-state-plugins out-state) :sandbox)
                                   :ptc-primitives)
                        :to-equal '(spawn_subagent)))

              ;; 2. Active gptel preset with :disable-local-tools clears stale buf/ctx prims
              (macher-agent-apply-preset '(sbx-tx-toolless))
              (setq-local macher-agent-sandbox-active-ptc-primitives '(spawn_subagent))
              (let* ((tx-state (make-macher-agent-transmission-state
                                :target-buffer owner-buf
                                :context ctx
                                :skills nil
                                :known-presets nil
                                :tools (list (macher-agent-resolve-tool "spawn_subagent" nil nil ctx))))
                     (out-state (macher-agent-sandbox-append-ptc-to-transmission tx-state)))
                (expect (plist-get (plist-get (macher-agent-transmission-state-plugins out-state) :sandbox)
                                   :ptc-primitives)
                        :to-be nil))

              ;; 3. :parents inheritance via :skills (with :known-presets nil) and via :exclusive child
              (setq-local macher-agent-sandbox-active-ptc-primitives '(stale_prim))
              (let* ((tx-child (make-macher-agent-transmission-state
                                :target-buffer owner-buf
                                :context ctx
                                :skills '(sbx-tx-child)
                                :known-presets nil
                                :tools (list (macher-agent-resolve-tool "spawn_subagent" nil nil ctx)
                                             (macher-agent-resolve-tool "send_message" nil nil ctx))))
                     (out-child (macher-agent-sandbox-append-ptc-to-transmission tx-child))
                     (prims-child (plist-get (plist-get (macher-agent-transmission-state-plugins out-child) :sandbox)
                                             :ptc-primitives)))
                (expect (memq 'spawn_subagent prims-child) :to-be-truthy)
                (expect (memq 'send_message prims-child) :to-be-truthy))

              (setq-local macher-agent-sandbox-active-ptc-primitives '(stale_prim))
              (let* ((tx-excl (make-macher-agent-transmission-state
                               :target-buffer owner-buf
                               :context ctx
                               :skills '(sbx-tx-excl-child)
                               :known-presets nil
                               :tools (list (macher-agent-resolve-tool "spawn_subagent" nil nil ctx))))
                     (out-excl (macher-agent-sandbox-append-ptc-to-transmission tx-excl))
                     (prims-excl (plist-get (plist-get (macher-agent-transmission-state-plugins out-excl) :sandbox)
                                            :ptc-primitives)))
                (expect prims-excl :to-equal '(spawn_subagent)))

              ;; 4. redirected-skill resolution (inheriting :parents, adding own :ptc-primitives, and exclusive clearing)
              (setq-local macher-agent-sandbox-active-ptc-primitives nil)
              (macher-agent-sandbox-set-state ctx nil (buffer-name owner-buf))
              (let* ((tx-redir (make-macher-agent-transmission-state
                                :target-buffer owner-buf
                                :context ctx
                                :skills nil
                                :known-presets nil
                                :redirected-skill 'sbx-tx-redir
                                :tools (list (macher-agent-resolve-tool "spawn_subagent" nil nil ctx)
                                             (macher-agent-resolve-tool "wait_for_message" nil nil ctx))))
                     (out-redir (macher-agent-sandbox-append-ptc-to-transmission tx-redir))
                     (prims-redir (plist-get (plist-get (macher-agent-transmission-state-plugins out-redir) :sandbox)
                                             :ptc-primitives)))
                (expect (memq 'spawn_subagent prims-redir) :to-be-truthy)
                (expect (memq 'wait_for_message prims-redir) :to-be-truthy))
              (setq-local macher-agent-sandbox-active-ptc-primitives '(spawn_subagent))
              (let* ((tx-redir-excl (make-macher-agent-transmission-state
                                     :target-buffer owner-buf
                                     :context ctx
                                     :skills nil
                                     :known-presets nil
                                     :redirected-skill '(:exclusive t :system "Plain redirect")
                                     :tools (list (macher-agent-resolve-tool "spawn_subagent" nil nil ctx))))
                     (out-redir-excl (macher-agent-sandbox-append-ptc-to-transmission tx-redir-excl)))
                (expect (plist-get (plist-get (macher-agent-transmission-state-plugins out-redir-excl) :sandbox)
                                   :ptc-primitives)
                        :to-be nil))

              ;; 5. Fallback primitive check excludes ptc_execution and meta tools when active is nil
              (let ((macher-agent-sandbox-active-ptc-primitives nil)
                    (gptel-tools (list (macher-agent-resolve-tool "ptc_execution" nil nil ctx)
                                       (macher-agent-resolve-tool "use_skill" nil nil ctx)
                                       (macher-agent-resolve-tool "spawn_subagent" nil nil ctx))))
                (expect (macher-agent-sandbox--ptc-primitive-p 'ptc_execution) :to-be nil)
                (expect (macher-agent-sandbox--ptc-primitive-p 'ptc-execution) :to-be nil)
                (expect (macher-agent-sandbox--ptc-primitive-p 'use_skill) :to-be nil)
                (expect (macher-agent-sandbox--ptc-primitive-p 'use-skill) :to-be nil)
                (expect (macher-agent-sandbox--ptc-primitive-p 'spawn_subagent) :to-be t))

              ;; 6. execute_subagents docstring advertises (list string) return type
              (let* ((exec-tool (macher-agent-resolve-tool "execute_subagents" nil nil ctx))
                     (doc (macher-agent-sandbox--format-tool-lisp-docstring exec-tool)))
                (expect doc :to-match "(list string)")
                (expect doc :to-match "(list \"\")")))
          (dolist (sym '(sbx-tx-parent sbx-tx-child sbx-tx-excl-child sbx-tx-redir sbx-tx-toolless))
            (cl-remprop sym 'macher-agent-preset-meta))))))

  ;; --------------------------------------------------------------------------
  ;; Pure ptc_execution.el & 5 orchestration tool scripts loading & execution
  ;; --------------------------------------------------------------------------
  (it "SBX-TOOLS-01: loads pure ptc_execution.el (no ptc-function) and all 5 orchestration tools from sandbox skills dir"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (clrhash macher-agent-tools-registry)
      (cl-remprop 'macher-agent-ptc-execution-tool 'ptc-function)
      (cl-remprop 'ptc_execution 'ptc-function)
      (makunbound 'macher-agent-ptc-execution-tool)

      ;; 1. ptc_execution.el loads as pure gptel-tool with NO ptc-function property
      (let ((ptc-tool (macher-agent-resolve-tool "ptc_execution" nil nil ctx)))
        (expect (macher-agent-tool-valid-p ptc-tool) :to-be t)
        (expect (gptel-tool-name ptc-tool) :to-equal "ptc_execution")
        (expect (get 'macher-agent-ptc-execution-tool 'ptc-function) :to-be nil)
        (expect (get 'ptc_execution 'ptc-function) :to-be nil)

        ;; Direct invocation via gptel-tool :function
        (with-current-buffer owner-buf
          (let ((fsm (gptel-make-fsm :info (list :buffer owner-buf :macher-agent-context ctx)))
                (out-num nil)
                (out-list nil))
            (cl-letf (((symbol-function 'macher-agent-get-active-fsm) (lambda () fsm)))
              (funcall (gptel-tool-function ptc-tool) (lambda (r) (setq out-num r)) "(+ 20 22)")
              (funcall (gptel-tool-function ptc-tool) (lambda (r) (setq out-list r)) "(list \"alpha\" \"beta\")"))
            (expect out-num :to-equal "42")
            (expect out-list :to-equal "alpha\n\nbeta"))))

      ;; 2. All 5 orchestration tools load cleanly with ptc-function properties
      (dolist (entry '(("delegate_tasks_to_subagents" . macher-agent-delegate-tasks-to-subagents-tool)
                       ("execute_subagents" . macher-agent-execute-subagents-tool)
                       ("spawn_subagent" . macher-agent-spawn-subagent-tool)
                       ("send_message" . macher-agent-send-message-tool)
                       ("wait_for_message" . macher-agent-wait-for-message-tool)))
        (let ((tl (macher-agent-resolve-tool (car entry) nil nil ctx)))
          (expect (macher-agent-tool-valid-p tl) :to-be t)
          (expect (gptel-tool-name tl) :to-equal (car entry))
          (expect (functionp (get (cdr entry) 'ptc-function)) :to-be t)))

      ;; 3. Execute spawn_subagent + send_message + wait_for_message inside PTC
      (with-current-buffer owner-buf
        (let ((spawn-res nil)
              (spawn-err nil))
          (setq-local gptel-tools (list (macher-agent-resolve-tool "spawn_subagent" nil nil ctx)))
          (setq-local macher-agent-sandbox-active-ptc-primitives '(spawn_subagent send_message wait_for_message))
          (macher-agent-sandbox-execute-ptc-script
           "(let* ((bname (spawn-subagent \"sbx-peer\"))) bname)"
           ctx owner-buf
           (lambda (r) (setq spawn-res r))
           (lambda (e) (setq spawn-err e)))
          (expect spawn-err :to-be nil)
          (expect (stringp spawn-res) :to-be t)
          (expect (buffer-live-p (get-buffer spawn-res)) :to-be t)

          ;; Queue a message into the spawned peer and verify wait_for_message reads it inside PTC
          (let ((peer-buf (get-buffer spawn-res))
                (wait-res nil)
                (wait-err nil))
            (with-current-buffer peer-buf
              (setq-local macher-agent-message-queue '("hello from peer"))
              (setq-local gptel-tools (list (macher-agent-resolve-tool "wait_for_message" nil nil ctx)))
              (setq-local macher-agent-sandbox-active-ptc-primitives '(wait_for_message))
              (macher-agent-sandbox-execute-ptc-script
               "(wait-for-message)"
               (macher-agent-resolve-context peer-buf)
               peer-buf
               (lambda (r) (setq wait-res r))
               (lambda (e) (setq wait-err e))))
            (expect wait-err :to-be nil)
            (expect wait-res :to-match "hello from peer"))))))

  ;; --------------------------------------------------------------------------
  ;; Install/uninstall idempotency & custom evaluator preservation
  ;; --------------------------------------------------------------------------
  (it "SBX-LIFE-02: installs/uninstalls idempotently and preserves custom macher-agent-evaluate-tool-forms-function"
    (let ((skills-dir (file-name-as-directory
                       (expand-file-name (macher-agent-sandbox-skills-directory))))
          (custom-eval (lambda (forms name &optional ws)
                         (macher-agent-evaluate-trusted-tool-ast forms name ws)))
          (baseline-buf (generate-new-buffer "*test-sbx-life-02-baseline*")))
      (unwind-protect
          (progn
            ;; Idempotent double install
            (macher-agent-sandbox-install)
            (macher-agent-sandbox-install)
            (expect macher-agent-evaluate-tool-forms-function :to-be #'macher-agent-sandbox-evaluate-tool-forms)
            (expect (cl-count skills-dir macher-agent-plugin-skill-directories :test #'equal)
                    :to-equal 1)
            (expect (cl-count #'macher-agent-sandbox-append-ptc-to-transmission
                              (macher-agent-get-pipeline-steps 'transmission))
                    :to-equal 1)
            (expect (cl-count #'macher-agent-sandbox--inject-preset-ptc-tool
                              (macher-agent-get-pipeline-steps 'preset-composition))
                    :to-equal 1)
            (expect (cl-count #'macher-agent-sandbox--clone-context-step
                              (macher-agent-get-pipeline-steps 'context-clone))
                    :to-equal 1)
            (expect (length (cl-remove-if-not
                             (lambda (f) (eq f #'macher-agent-sandbox--on-apply-payload))
                             macher-agent-apply-payload-hook))
                    :to-equal 1)

            ;; Populate cached baseline locals to verify uninstall clears them across buffers
            (with-current-buffer baseline-buf
              (setq-local macher-agent-sandbox--cached-baseline-set t)
              (setq-local macher-agent-sandbox--cached-baseline-ptc-primitives '(spawn_subagent)))

            ;; Uninstall restores default evaluator and cleans up hooks/pipeline/skills dir/baselines
            (macher-agent-sandbox-uninstall)
            (expect macher-agent-evaluate-tool-forms-function :to-be #'macher-agent-evaluate-trusted-tool-ast)
            (expect (cl-count skills-dir macher-agent-plugin-skill-directories :test #'equal)
                    :to-equal 0)
            (expect (memq #'macher-agent-sandbox-append-ptc-to-transmission
                          (macher-agent-get-pipeline-steps 'transmission))
                    :to-be nil)
            (expect (memq #'macher-agent-sandbox--inject-preset-ptc-tool
                          (macher-agent-get-pipeline-steps 'preset-composition))
                    :to-be nil)
            (expect (memq #'macher-agent-sandbox--clone-context-step
                          (macher-agent-get-pipeline-steps 'context-clone))
                    :to-be nil)
            (expect (memq #'macher-agent-sandbox--on-apply-payload macher-agent-apply-payload-hook)
                    :to-be nil)
            (expect (memq #'macher-agent-sandbox--on-preset-cleared macher-agent-preset-cleared-hook)
                    :to-be nil)
            (expect (memq #'macher-agent-sandbox--on-skill-baseline-capture macher-agent-skill-baseline-capture-hook)
                    :to-be nil)
            (expect (memq #'macher-agent-sandbox--on-skill-baseline-restore macher-agent-skill-baseline-restore-hook)
                    :to-be nil)
            (expect (local-variable-p 'macher-agent-sandbox--cached-baseline-set baseline-buf) :to-be nil)
            (expect (local-variable-p 'macher-agent-sandbox--cached-baseline-ptc-primitives baseline-buf) :to-be nil)

            ;; Custom evaluator is NOT clobbered by uninstall
            (setq macher-agent-evaluate-tool-forms-function custom-eval)
            (macher-agent-sandbox-uninstall)
            (expect macher-agent-evaluate-tool-forms-function :to-be custom-eval))
        (when (buffer-live-p baseline-buf)
          (kill-buffer baseline-buf))
        (setq macher-agent-evaluate-tool-forms-function #'macher-agent-evaluate-trusted-tool-ast)
        (macher-agent-sandbox-install))))

  ;; --------------------------------------------------------------------------
  ;; Subprocess byte-compilation verification for sandbox module & scripts
  ;; --------------------------------------------------------------------------
  (it "SBX-COMPILE-01: byte-compiles macher-agent-sandbox and all bundled skill scripts in clean subprocess with zero warnings"
    (let* ((sbx-lib (locate-library "macher-agent-sandbox"))
           (sbx-file (if (string-suffix-p ".elc" sbx-lib)
                         (concat (file-name-sans-extension sbx-lib) ".el")
                       sbx-lib))
           (sbx-root (file-name-directory sbx-file))
           (core-root (file-name-directory (locate-library "macher-agent")))
           (gptel-dir (file-name-directory (locate-library "gptel")))
           (scripts-dir (expand-file-name "skills/scripts" sbx-root))
           (skill-scripts (when (file-directory-p scripts-dir)
                            (directory-files scripts-dir t "\\.el\\'")))
           (all-files (cons sbx-file skill-scripts))
           (emacs-bin (expand-file-name invocation-name invocation-directory))
           (out-buf (generate-new-buffer " *sbx-compile-out*")))
      (expect (file-exists-p sbx-file) :to-be t)
      (expect (length skill-scripts) :to-be-greater-than 0)
      (unwind-protect
          (dolist (file all-files)
            (with-current-buffer out-buf (erase-buffer))
            (let* ((is-pkg-mod (string= file sbx-file))
                   (eval-code
                    (if is-pkg-mod
                        "(setq byte-compile-error-on-warn t byte-compile-warnings '(not obsolete docstrings))"
                      (format "(progn (setq byte-compile-error-on-warn t byte-compile-warnings '(not obsolete docstrings)) (dolist (feat '(cl-lib subr-x gptel macher-agent-core macher-agent-presets macher-agent-gptel macher-agent-tools macher-agent-orchestration macher-agent-api macher-agent macher-agent-sandbox)) (require feat nil t)) (let ((sdir %S)) (when (file-directory-p sdir) (dolist (f (directory-files sdir t \"\\\\.el\\\\'\")) (let* ((b (file-name-sans-extension (file-name-nondirectory f))) (h (intern (format \"macher-agent-%%s-tool\" (replace-regexp-in-string \"_\" \"-\" b)))) (u (intern (format \"macher-agent-%%s-tool\" (replace-regexp-in-string \"-\" \"_\" b))))) (eval `(defvar ,h nil)) (eval `(defvar ,u nil)))))))"
                              scripts-dir)))
                   (args (list "-Q" "--batch"
                               "-L" (expand-file-name sbx-root)
                               "-L" (expand-file-name core-root)
                               "-L" (expand-file-name gptel-dir)
                               "-L" (expand-file-name scripts-dir)
                               "--eval" eval-code
                               "-f" "batch-byte-compile"
                               file))
                   (exit-code (apply #'call-process emacs-bin nil out-buf nil args))
                   (output (with-current-buffer out-buf (buffer-string))))
              (let ((elc (concat (file-name-sans-extension file) ".elc")))
                (when (file-exists-p elc)
                  (delete-file elc)))
              (when (/= exit-code 0)
                (error "SBX-COMPILE-01 FAILED ON %s (exit %s): %s" file exit-code output))
              (expect exit-code :to-equal 0)
              (expect output :not :to-match "Warning:")
              (expect output :not :to-match "free variable")
              (expect output :not :to-match "unresolved call")))
        (when (buffer-live-p out-buf)
          (kill-buffer out-buf)))))

  ;; --------------------------------------------------------------------------
  ;; Static package boundary verification
  ;; --------------------------------------------------------------------------
  (it "SBX-BOUND-01: enforces zero private core/gptel or sibling package references in sandbox module and scripts"
    (let* ((sbx-lib (locate-library "macher-agent-sandbox"))
           (sbx-file (if (string-suffix-p ".elc" sbx-lib)
                         (concat (file-name-sans-extension sbx-lib) ".el")
                       sbx-lib))
           (sbx-dir (file-name-directory sbx-file))
           (scripts-dir (expand-file-name "skills/scripts" sbx-dir)))
      (expect (file-exists-p sbx-file) :to-be t)
      (with-temp-buffer
        (insert-file-contents sbx-file)
        (let ((content (buffer-string)))
          (expect content :not :to-match "\\bmacher-agent--")
          (expect content :not :to-match "\\bgptel--")
          (expect content :not :to-match "macher-agent-wire")
          (expect content :not :to-match "macher-agent-zero-mem")
          (expect content :not :to-match "macher-agent-vfs")))
      (expect (file-directory-p scripts-dir) :to-be t)
      (dolist (script (directory-files scripts-dir t "\\.el\\'"))
        (with-temp-buffer
          (insert-file-contents script)
          (let ((content (buffer-string)))
            (expect content :not :to-match "\\bmacher-agent--")
            (expect content :not :to-match "\\bmacher-agent-sandbox--")
            (expect content :not :to-match "\\bgptel--")
            (expect content :not :to-match "macher-agent-wire")
            (expect content :not :to-match "macher-agent-zero-mem")
            (expect content :not :to-match "macher-agent-vfs")
            (when (string= (file-name-nondirectory script) "ptc_execution.el")
              (expect content :not :to-match "ptc-function"))))))))

(provide 'test-sandbox)
;;; test-sandbox.el ends here
