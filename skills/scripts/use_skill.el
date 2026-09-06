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
              (target-buf (or (when fsm
                                (or (ignore-errors (plist-get (gptel-fsm-info fsm) :buffer))
                                    (when (fboundp 'gptel-fsm-buffer)
                                      (ignore-errors (gptel-fsm-buffer fsm)))))
                              (when (macher-agent-valid-context-p context)
                                (let ((plugins (macher-agent-context-plugins context)))
                                  (when (macher-agent--plist-p plugins)
                                    (or (plist-get plugins :buffer)
                                        (plist-get plugins :origin-buffer)
                                        (plist-get plugins :originator-buffer)))))
                              (current-buffer)))
              (is-done (or (and done (not (eq done :json-false)) (not (equal done "false")))
                           (and (stringp skill-name)
                                (string-equal (string-trim (downcase skill-name)) "done"))
                           (eq skill-name 'done))))
         (with-current-buffer target-buf
           (if is-done
               (let ((orig (when (local-variable-p 'macher-agent--cached-presets target-buf)
                             (buffer-local-value 'macher-agent--cached-presets target-buf))))
                 (when (local-variable-p 'macher-agent--cached-presets target-buf)
                   (setq-local macher-agent-presets orig))
                 (macher-agent-clear-cached-presets target-buf)
                 (if orig
                     (format "Restored original presets: %S" orig)
                   "Restored original presets."))
             (let* ((raw-skill (cond
                                ((listp skill-name) skill-name)
                                ((symbolp skill-name) (list skill-name))
                                ((stringp skill-name)
                                 (let ((trimmed (string-trim skill-name)))
                                   (unless (string-empty-p trimmed)
                                     (list (intern trimmed)))))
                                (t nil))))
               (unless raw-skill
                 (error "ERROR: skill_name must be provided to activate a skill, or set done to true"))
               (unless (local-variable-p 'macher-agent--cached-presets target-buf)
                 (setq-local macher-agent--cached-presets (copy-sequence macher-agent-presets)))
               (setq-local macher-agent-presets raw-skill)
               (format "Activated skill '%s'. Presets updated." (car raw-skill))))))))

(put 'use_skill 'ptc-function (get 'macher-agent-use-skill-tool 'ptc-function))
(put 'use-skill 'ptc-function (get 'macher-agent-use-skill-tool 'ptc-function))
