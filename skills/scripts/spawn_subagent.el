;;; spawn_subagent.el --- Spawn subagent tool -*- lexical-binding: t; -*-

(setq macher-agent-spawn-subagent-tool
      (gptel-make-tool
       :name "spawn_subagent"
       :description "Spawn a new sub-agent buffer to handle delegated work."
       :category "collaboration"
       :args '((:name "name" :type "string")
               (:name "presets" :type "array" :items (:type "string")
                      :description "Array of presets to apply"
                      :optional t))
       :async t
       :function (macher-agent-with-presentation-context (name presets)
                   (let* ((native-fn (get 'macher-agent-spawn-subagent-tool 'ptc-function))
                          (res (funcall native-fn name presets context)))
                     (format "SUCCESS: Sub-agent created. The EXACT buffer name to use is '%s'." res)))))

(put 'macher-agent-spawn-subagent-tool 'ptc-function
     (lambda (name &optional presets context)
       (let* ((root (when (macher-agent-valid-context-p context)
                      (macher-agent-context-project-root context)))
              (buf (macher-agent-add-subagent name presets (current-buffer) root context)))
         (when (bufferp buf)
           (buffer-name buf)))))
