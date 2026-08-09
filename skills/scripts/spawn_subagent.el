(macher-agent-make-tool
    macher-agent-spawn-subagent-tool
    "Spawn a new sub-agent buffer to handle delegated work."
  :category "collaboration"
  :args
  '((
     :name "name" :type string)
    (
     :name "presets"
     :type array
     :items (:type string) :description "Array of SKILL.md presets to apply" :optional t))
  :command-fn
  (lambda (payload context root)
    (let* ((name (plist-get payload :name))
           (preset-list (append (plist-get payload :presets) nil))
           (buf (macher-agent-add-subagent name preset-list nil root context)))
      (when (bufferp buf)
        (buffer-name buf))))
  :success-fn
  (lambda (buf-name-string _payload)
    (format "SUCCESS: Sub-agent created. The EXACT buffer name to use is '%s'."
            buf-name-string)))
