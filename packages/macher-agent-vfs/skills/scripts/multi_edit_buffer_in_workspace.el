;;; multi_edit_buffer_in_workspace.el --- Multi edit buffer in workspace -*- lexical-binding: t; -*-

(setq macher-agent-multi-edit-buffer-in-workspace-tool
      (gptel-make-tool
       :name "multi_edit_buffer_in_workspace"
       :description "Apply 1-to-many replacements to one scoped buffer in your workspace sequentially."
       :category "execution"
       :args '((:name "buffer_name" :type "string" :description "The name of the target buffer")
               (:name "edits" :type "array" :description "Array of edit operations"
                      :items (:type "object"
                                    :properties (:old_text (:type "string")
                                                           :new_text (:type "string")
                                                           :replace_all (:type "boolean"))
                                    :required ["old_text" "new_text"])))
       :async t
       :function (macher-agent-with-presentation-context (buffer-name edits)
                   (let* ((native-fn (get 'macher-agent-multi-edit-buffer-in-workspace-tool 'ptc-function))
                          (res (funcall native-fn buffer-name edits context)))
                     (format "SUCCESS: Virtual multi-edit recorded for buffer '%s'." (cdr (assoc 'buffer res)))))))

(put 'macher-agent-multi-edit-buffer-in-workspace-tool 'ptc-function
     (lambda (buffer-name edits &optional context)
       (unless (and (fboundp 'macher-agent-vfs-ensure-access)
                    (fboundp 'macher-agent-vfs-normalize-path-key)
                    (fboundp 'macher-agent-vfs-read-context-file)
                    (fboundp 'macher-agent-vfs-edit-string-fast)
                    (fboundp 'macher-agent-vfs-update-context-file))
         (error "Macher-Agent VFS plugin (`macher-agent-vfs') is not loaded"))
       (let* ((fsm (macher-agent-get-active-fsm))
              (ctx (or (when context (macher-agent-resolve-context context))
                       (and fsm (macher-agent-gptel-context-from-fsm fsm))
                       (macher-agent-resolve-context (current-buffer))
                       (macher-agent-resolve-context nil)))
              (actual-name (macher-agent-resolve-buffer-name buffer-name))
              (live-buf (get-buffer actual-name))
              (target-path (or (and live-buf (buffer-file-name live-buf)) actual-name))
              (_ (when (macher-agent-valid-context-p ctx)
                   (funcall (symbol-function 'macher-agent-vfs-ensure-access) ctx actual-name)))
              (norm-key (if (macher-agent-valid-context-p ctx)
                            (funcall (symbol-function 'macher-agent-vfs-normalize-path-key)
                                     target-path ctx)
                          target-path))
              (content (if (macher-agent-valid-context-p ctx)
                           (funcall (symbol-function 'macher-agent-vfs-read-context-file)
                                    ctx target-path)
                         (or (funcall (symbol-function 'macher-agent-vfs-read-content-from-disk-or-buffer)
                                      target-path)
                             (error "Buffer '%s' not found in workspace" actual-name))))
              (edits-list (append edits nil)))
         (dolist (edit edits-list)
           (let* ((old-text (plist-get edit :old_text))
                  (new-text (plist-get edit :new_text))
                  (replace-all (plist-get edit :replace_all))
                  (replace-all-bool (and replace-all (not (eq replace-all :json-false)))))
             (unless (and old-text new-text)
               (error "Each edit must contain old_text and new_text properties"))
             (setq content (funcall (symbol-function 'macher-agent-vfs-edit-string-fast)
                                    old-text new-text content replace-all-bool))))
         (when (macher-agent-valid-context-p ctx)
           (funcall (symbol-function 'macher-agent-vfs-update-context-file) ctx norm-key content))
         `((status . "success") (buffer . ,actual-name) (edits-applied . ,(length edits-list))))))
