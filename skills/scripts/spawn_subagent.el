(macher-agent-make-tool macher-agent-spawn-subagent-tool
			"Spawn a new sub-agent buffer to handle delegated work."
			:category "orchestrate"
			:args '((:name "name" :type string)
				(:name "presets" :type array :items (:type string) :description "Array of \
SKILL.md presets to apply" :optional t))
			:command-fn (lambda (payload context root)
				      (let* ((name (plist-get payload :name))
					     (preset-list (append (plist-get payload :presets) nil))
					     (buf (macher-agent-add-subagent name root nil context preset-list)))
					(make-macher-agent-lisp-result-response
					 :payload (format "SUCCESS: Sub-agent created. The EXACT buffer name to \
use is '%s'." (buffer-name buf))
					 :ptc-payload (buffer-name buf)))))
