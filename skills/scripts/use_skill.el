;;; use_skill.el --- Dynamic skill switching tool -*- lexical-binding: t; -*-

(setq macher-agent-use-skill-tool
      (gptel-make-tool
       :name "use_skill"
       :description "Switch to a specialized skill/preset for subsequent agent operations, or restore original presets when done."
       :category "meta"
       :args '((:name "skill_name" :type "string"
                      :description "The name of the skill or preset to activate, or 'done' to restore original presets."
                      :optional t)
               (:name "done" :type "boolean"
                      :description "Optional boolean flag to indicate completion and restore original presets."
                      :optional t))
       :async t
       :function (macher-agent-with-presentation-context (skill-name &optional done)
                   (let* ((native-fn (get 'macher-agent-use-skill-tool 'ptc-function))
                          (res (funcall native-fn (or skill-name "") done context)))
                     (format "SUCCESS: %s" res)))))

(put 'macher-agent-use-skill-tool 'ptc-function
     (lambda (skill-name &optional done context)
       (let* ((fsm (macher-agent-get-active-fsm))
              (target-buf (or (when (macher-agent-valid-context-p context)
                                (or (let ((ob (macher-agent-context-owner-buffer context)))
                                      (when (and (bufferp ob) (buffer-live-p ob)) ob))
                                    (let ((plugins (macher-agent-context-plugins context)))
                                      (when (macher-agent--plist-p plugins)
                                        (or (plist-get plugins :buffer)
                                            (plist-get plugins :origin-buffer)
                                            (plist-get plugins :originator-buffer))))))
                              (when fsm
                                (or (ignore-errors (plist-get (gptel-fsm-info fsm) :buffer))
                                    (when (fboundp 'gptel-fsm-buffer)
                                      (ignore-errors (gptel-fsm-buffer fsm)))))
                              (current-buffer)))
              (ctx (or (when (macher-agent-valid-context-p context) context)
                       (macher-agent-resolve-context target-buf)))
              (is-done (or (and done (not (eq done :json-false)) (not (equal done "false")))
                           (and (stringp skill-name)
                                (string-equal (string-trim (downcase skill-name)) "done"))
                           (eq skill-name 'done))))
         (with-current-buffer target-buf
           (if is-done
               (let* ((had-cache (or (bound-and-true-p macher-agent--cached-presets-set)
                                     (local-variable-p 'macher-agent--cached-presets target-buf)))
                      (baseline (bound-and-true-p macher-agent--cached-skill-baseline))
                      (orig (when had-cache
                              (buffer-local-value 'macher-agent--cached-presets target-buf)))
                      (orig-gptel-preset (plist-get baseline :gptel-preset))
                      (restored-label (or orig (and orig-gptel-preset (list orig-gptel-preset)))))
                 (when had-cache
                   (setq-local macher-agent-presets orig)
                   (setq-local gptel-tools (copy-sequence (plist-get baseline :tools)))
                   (setq-local macher-agent--active-ptc-primitives
                               (copy-sequence (plist-get baseline :ptc-primitives))))
                 (when (plist-member baseline :gptel-preset)
                   (setq-local gptel--preset orig-gptel-preset))
                 (when (plist-member baseline :system-prompt)
                   (setq-local gptel-system-prompt (plist-get baseline :system-prompt)))
                 (when (plist-member baseline :model)
                   (setq-local gptel-model (plist-get baseline :model)))
                 (when (plist-member baseline :backend)
                   (setq-local gptel-backend (plist-get baseline :backend)))
                 (macher-agent-clear-cached-presets target-buf)
                 (kill-local-variable 'macher-agent--cached-presets-set)
                 (kill-local-variable 'macher-agent--cached-skill-baseline)
                 (when orig
                   (macher-agent--apply-preset orig))
                 (setq-local macher-agent--force-boot-directive nil)
                 (when (bound-and-true-p macher-agent--cached-boot-directive-set)
                   (setq-local macher-agent--boot-directive macher-agent--cached-boot-directive)
                   (kill-local-variable 'macher-agent--cached-boot-directive)
                   (kill-local-variable 'macher-agent--cached-boot-directive-set))
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
                                ((and (listp skill-name) skill-name)
                                 (delq nil (mapcar #'macher-normalise-preset-name skill-name)))
                                ((symbolp skill-name)
                                 (list (macher-normalise-preset-name skill-name)))
                                ((stringp skill-name)
                                 (let ((trimmed (string-trim skill-name)))
                                    (unless (string-empty-p trimmed)
                                      (list (macher-normalise-preset-name trimmed)))))
                                (t nil)))
                    (skill-sym (car raw-skill)))
               (unless skill-sym
                 (error "ERROR: skill_name must be provided to activate a skill, or set done to true"))
               (let ((spec (macher-agent--validate-activatable-skill skill-sym ctx target-buf)))
                 (unless (or (bound-and-true-p macher-agent--cached-presets-set)
                             (local-variable-p 'macher-agent--cached-presets target-buf))
                   (setq-local macher-agent--cached-presets (copy-sequence macher-agent-presets))
                   (setq-local macher-agent--cached-presets-set t)
                   (setq-local macher-agent--cached-skill-baseline
                               (list :gptel-preset (bound-and-true-p gptel--preset)
                                     :system-prompt gptel-system-prompt
                                     :model gptel-model
                                     :backend (bound-and-true-p gptel-backend)
                                     :tools (copy-sequence gptel-tools)
                                     :ptc-primitives
                                     (copy-sequence (bound-and-true-p macher-agent--active-ptc-primitives)))))
                 (unless (bound-and-true-p macher-agent--cached-boot-directive-set)
                   (setq-local macher-agent--cached-boot-directive (bound-and-true-p macher-agent--boot-directive))
                   (setq-local macher-agent--cached-boot-directive-set t))
                 (setq-local gptel-tools nil)
                 (setq-local macher-agent--active-ptc-primitives nil)
                 (setq-local macher-agent-presets raw-skill)
                 (let ((payload (macher-agent--apply-preset raw-skill)))
                   (macher-agent--set-active-boot-directive
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
                      ""))))))))))

(put 'use_skill 'ptc-function (get 'macher-agent-use-skill-tool 'ptc-function))
(put 'use-skill 'ptc-function (get 'macher-agent-use-skill-tool 'ptc-function))
