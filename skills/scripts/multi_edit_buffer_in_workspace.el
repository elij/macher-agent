;;; multi_edit_buffer_in_workspace.el --- Multi edit buffer in workspace -*- lexical-binding: t; -*-

(setq macher-agent-multi-edit-buffer-in-workspace-tool
      (gptel-make-tool
       :name "multi_edit_buffer_in_workspace"
       :description "Apply 1-to-many replacements to one scoped buffer in your workspace sequentially. All edits use exact text matching (whitespace, newlines, indentation). Use actual content - NO line numbers.\n\nEdits apply in array order. If ANY edit fails, ALL changes are rolled back."
       :category "execution"
       :args '((:name "buffer_name" :type "string" :description "The name of the target buffer")
               (:name "edits" :type "array" :description "Array of edit operations to apply in sequence"
                      :items (:type "object"
                                    :properties (:old_text (:type "string" :description "Exact text to find and replace.")
                                                           :new_text (:type "string" :description "Text to replace the old_text with")
                                                           :replace_all (:type "boolean" :description "If true, replace all occurrences."))
                                    :required ["old_text" "new_text"])))
       :async t
       :function (macher-agent-with-presentation-context (buffer-name edits)
                   (let* ((native-fn (get 'macher-agent-multi-edit-buffer-in-workspace-tool 'ptc-function))
                          (res (funcall native-fn buffer-name edits context)))
                     (format "SUCCESS: Virtual multi-edit recorded for buffer '%s'. A patch will be generated at the end of the turn."
                             (cdr (assoc 'buffer res)))))))

(put 'macher-agent-multi-edit-buffer-in-workspace-tool 'ptc-function
     (lambda (buffer-name edits context)
       (let* ((actual-name (macher-agent--resolve-buffer-name buffer-name))
              (norm-key (macher-agent--normalize-path-key actual-name context))
              (contents (when (macher-agent-valid-context-p context)
                          (macher-agent--get-context-contents context)))
              (entry (or (cl-find norm-key contents :key #'macher-agent-vfs-entry-path :test #'equal)
                         (cl-find actual-name contents :key #'macher-agent-vfs-entry-path :test #'equal)))
              (content (cond
                        (entry (macher-agent-vfs-entry-curr entry))
                        ((macher-agent--read-content-from-disk-or-buffer actual-name))
                        (t (error "Buffer '%s' not found in workspace" actual-name))))
              (edits-list (append edits nil)))
         (dolist (edit edits-list)
           (let* ((old-text (plist-get edit :old_text))
                  (new-text (plist-get edit :new_text))
                  (replace-all (plist-get edit :replace_all))
                  (replace-all-bool (and replace-all (not (eq replace-all :json-false)))))
             (unless (and old-text new-text)
               (error "Each edit must contain old_text and new_text properties"))
             (setq content (macher-agent--edit-string-fast content old-text new-text replace-all-bool))))
         (when (macher-agent-valid-context-p context)
           (macher-agent--update-context-file context norm-key content))
         `((status . "success") (buffer . ,actual-name) (edits-applied . ,(length edits-list))))))
