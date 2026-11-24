;;; use_skill.el --- Dynamic skill switching tool -*- lexical-binding: t; -*-

(setq macher-agent-use-skill-tool
      (gptel-make-tool
       :name "use_skill"
       :description "Switch to a specialized skill/preset for subsequent agent operations, or restore original presets when done."
       :category "meta"
       :include nil
       :args '((:name "skill_name" :type "string"
                      :description "The name of the skill or preset to activate, or 'done' to restore original presets."
                      :optional t)
               (:name "done" :type "boolean"
                      :description "Optional boolean flag to indicate completion and restore original presets."
                      :optional t))
       :async t
       :function (macher-agent-with-presentation-context (skill-name &optional done)
                   (let* ((fsm (macher-agent-get-active-fsm))
                          (target-buf (or (when (macher-agent-valid-context-p context)
                                            (or (let ((ob (macher-agent-context-owner-buffer context)))
                                                  (when (and (bufferp ob) (buffer-live-p ob)) ob))
                                                (let ((plugins (macher-agent-context-plugins context)))
                                                  (when (macher-agent-plist-p plugins)
                                                    (or (plist-get plugins :buffer)
                                                        (plist-get plugins :origin-buffer)
                                                        (plist-get plugins :originator-buffer))))))
                                          (when fsm
                                            (or (ignore-errors (plist-get (gptel-fsm-info fsm) :buffer))
                                                (when (fboundp 'gptel-fsm-buffer)
                                                  (ignore-errors (funcall (symbol-function 'gptel-fsm-buffer) fsm)))))
                                          (current-buffer)))
                          (ctx (or (when (macher-agent-valid-context-p context) context)
                                   (macher-agent-resolve-context target-buf)))
                          (effective-name (or skill-name ""))
                          (is-done (or (and done (not (eq done :json-false)) (not (equal done "false")))
                                       (and (stringp effective-name)
                                            (string-equal (string-trim (downcase effective-name)) "done"))
                                       (eq effective-name 'done)))
                          (res
                           (with-current-buffer target-buf
                             (if is-done
                                 (let* ((had-cache (or (bound-and-true-p macher-agent-cached-presets-set)
                                                       (local-variable-p 'macher-agent-cached-presets target-buf)))
                                        (baseline (bound-and-true-p macher-agent-cached-skill-baseline))
                                        (orig (when had-cache
                                                (buffer-local-value 'macher-agent-cached-presets target-buf)))
                                        (orig-gptel-preset (plist-get baseline :gptel-preset))
                                        (restored-label (or orig (and orig-gptel-preset (list orig-gptel-preset)))))
                                   (when had-cache
                                     (setq-local macher-agent-presets orig)
                                     (setq-local gptel-tools (copy-sequence (plist-get baseline :tools))))
                                   (when (plist-member baseline :gptel-preset)
                                     (setq-local gptel--preset orig-gptel-preset))
                                   (when (plist-member baseline :system-prompt)
                                     (setq-local gptel-system-prompt (plist-get baseline :system-prompt)))
                                   (when (plist-member baseline :model)
                                     (setq-local gptel-model (plist-get baseline :model)))
                                   (when (plist-member baseline :backend)
                                     (setq-local gptel-backend (plist-get baseline :backend)))
                                   (when (plist-member baseline :request-params)
                                     (let ((rp (plist-get baseline :request-params)))
                                       (if rp
                                           (setq-local gptel--request-params rp)
                                         (kill-local-variable 'gptel--request-params))))
                                   (when (plist-member baseline :preset-request-params)
                                     (setq-local macher-agent-preset-request-params
                                                 (plist-get baseline :preset-request-params)))
                                   (when (plist-member baseline :disable-local-tools)
                                     (setq-local macher-agent-disable-local-tools
                                                 (plist-get baseline :disable-local-tools)))
                                   (macher-agent-clear-cached-presets target-buf)
                                   (kill-local-variable 'macher-agent-cached-presets-set)
                                   (kill-local-variable 'macher-agent-cached-skill-baseline)
                                   (when orig
                                     (macher-agent-apply-preset orig))
                                   (when (or had-cache baseline)
                                     (run-hook-with-args 'macher-agent-skill-baseline-restore-hook target-buf))
                                   (setq-local macher-agent-force-boot-directive nil)
                                   (when (bound-and-true-p macher-agent-cached-boot-directive-set)
                                     (setq-local macher-agent-boot-directive macher-agent-cached-boot-directive)
                                     (kill-local-variable 'macher-agent-cached-boot-directive)
                                     (kill-local-variable 'macher-agent-cached-boot-directive-set))
                                   (when (fboundp 'gptel--update-header-line)
                                     (gptel--update-header-line))
                                   (let* ((recompiled (when (functionp macher-agent-mid-turn-recompile-function)
                                                        (funcall macher-agent-mid-turn-recompile-function target-buf ctx)))
                                          (active-names (when recompiled
                                                          (mapcar #'macher-agent-canonical-tool-name
                                                                  (macher-agent-transmission-state-tools recompiled)))))
                                     (if restored-label
                                         (format "Restored original presets: %S%s"
                                                 restored-label
                                                 (if active-names
                                                     (format " (active tools: %s)" (string-join active-names ", "))
                                                   ""))
                                       "Restored original presets.")))
                               (let* ((raw-skill (cond
                                                  ((and (listp effective-name) effective-name)
                                                   (delq nil (mapcar #'macher-agent-normalise-preset-name effective-name)))
                                                  ((symbolp effective-name)
                                                   (list (macher-agent-normalise-preset-name effective-name)))
                                                  ((stringp effective-name)
                                                   (let ((trimmed (string-trim effective-name)))
                                                     (unless (string-empty-p trimmed)
                                                       (list (macher-agent-normalise-preset-name trimmed)))))
                                                  (t nil)))
                                      (skill-sym (car raw-skill)))
                                 (unless skill-sym
                                   (error "skill_name must be provided to activate a skill, or set done to true"))
                                 (let ((spec (macher-agent-validate-activatable-skill skill-sym ctx target-buf)))
                                   (when (plist-get spec :disable-local-tools)
                                     (error "Skill '%s' disables local tools and cannot be activated mid-turn via `use_skill'; delegate via `delegate_tasks_to_subagents' with `:presets [\"%s\"]' instead"
                                            skill-sym skill-sym))
                                   (unless (or (bound-and-true-p macher-agent-cached-presets-set)
                                               (local-variable-p 'macher-agent-cached-presets target-buf))
                                     (setq-local macher-agent-cached-presets (copy-sequence macher-agent-presets))
                                     (setq-local macher-agent-cached-presets-set t)
                                     (setq-local macher-agent-cached-skill-baseline
                                                 (list :gptel-preset (bound-and-true-p gptel--preset)
                                                       :system-prompt gptel-system-prompt
                                                       :model gptel-model
                                                       :backend (bound-and-true-p gptel-backend)
                                                       :request-params (when (local-variable-p 'gptel--request-params target-buf)
                                                                         (buffer-local-value 'gptel--request-params target-buf))
                                                       :preset-request-params (bound-and-true-p macher-agent-preset-request-params)
                                                       :disable-local-tools (bound-and-true-p macher-agent-disable-local-tools)
                                                       :tools (copy-sequence gptel-tools)))
                                     (run-hook-with-args 'macher-agent-skill-baseline-capture-hook target-buf))
                                   (unless (bound-and-true-p macher-agent-cached-boot-directive-set)
                                     (setq-local macher-agent-cached-boot-directive (bound-and-true-p macher-agent-boot-directive))
                                     (setq-local macher-agent-cached-boot-directive-set t))
                                   (setq-local gptel-tools nil)
                                   (run-hook-with-args 'macher-agent-preset-cleared-hook target-buf ctx)
                                   (setq-local macher-agent-presets raw-skill)
                                   (let ((payload (macher-agent-apply-preset raw-skill)))
                                     (macher-agent-set-active-boot-directive
                                      (if (plist-member payload :boot-directive)
                                          (plist-get payload :boot-directive)
                                        (plist-get spec :boot-directive))))
                                   (when (fboundp 'gptel--update-header-line)
                                     (gptel--update-header-line))
                                   (let* ((recompiled (when (functionp macher-agent-mid-turn-recompile-function)
                                                        (funcall macher-agent-mid-turn-recompile-function target-buf ctx)))
                                          (active-names (when recompiled
                                                          (mapcar #'macher-agent-canonical-tool-name
                                                                  (macher-agent-transmission-state-tools recompiled))))
                                          (sys-body (or (plist-get spec :system) ""))
                                          (bd-body (or (plist-get spec :boot-directive) "")))
                                     (concat
                                      (format "Activated skill '%s'. Presets updated." skill-sym)
                                      (if active-names
                                          (format "\nActive tools: %s" (string-join active-names ", "))
                                        "")
                                      (if (not (string-empty-p (string-trim bd-body)))
                                          (format "\nBoot directive: %s" (string-trim bd-body))
                                        "")
                                      (if (not (string-empty-p (string-trim sys-body)))
                                          (format "\n\n### Skill: %s\n%s" skill-sym (string-trim sys-body))
                                        "")))))))))
                     (format "SUCCESS: %s" res)))))
