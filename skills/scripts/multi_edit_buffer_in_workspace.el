(macher-agent-make-tool macher-agent-multi-edit-buffer-in-workspace-tool
    "Apply 1-to-many replacements to one scoped buffer in your workspace sequentially. \
All edits use exact text matching (whitespace, newlines, indentation). Use actual content - \
NO line numbers.\n\nEdits apply in array order. If ANY edit fails, ALL changes are rolled \
back."
  :category "workspace"
  :args '((:name "buffer_name" :type string :description "The name of the target buffer")
          (:name "edits" :type array :description "Array of edit operations to apply in \
sequence"
                 :items (:type object
                               :properties (:old_text (:type string :description "Exact \
text \
to find and replace.")
                                                      :new_text (:type string
                                                                       :description "Text \
to replace the old_text with")
                                                      :replace_all (:type boolean
                                                                          :description "If \
true, replace all occurrences."))
                               :required ["old_text" "new_text"])))
  :command-fn (lambda (payload context _root)
                (let* ((buffer_name (plist-get payload :buffer_name))
                       (edits (plist-get payload :edits))
                       (actual-name (macher-agent--resolve-buffer-name buffer_name))
                       (content
                        (let ((contents
                               (cl-find actual-name
                                        (macher-agent--get-context-contents context)
                                        :key #'macher-agent-vfs-entry-path :test #'equal)))
                          (if contents
                              (macher-agent-vfs-entry-curr contents)
                            (error "Buffer '%s' not found in workspace" actual-name)))))
                  (cl-loop for edit in (append edits nil) do
                           (let* ((old-text (plist-get edit :old_text))
                                  (new-text (plist-get edit :new_text))
                                  (replace-all (plist-get edit :replace_all))
                                  (replace-all-bool
                                   (and replace-all (not (eq replace-all :json-false)))))
                             (unless (and old-text new-text)
                               (error "Each edit must contain old_text and new_text \
properties"))
                             (setq content
                                   (macher-agent--edit-string-fast content old-text 
                                                                   new-text replace-all-bool))))
                  (when context
                    (macher-agent--update-context-file context actual-name content))
                  (make-macher-agent-lisp-result-response :payload (format "SUCCESS: \
Virtual multi-edit recorded for buffer '%s'. A patch will be generated at the end of the \
turn." actual-name)))))
