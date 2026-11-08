;;; read_buffer_in_workspace.el --- Read buffer in workspace tool -*- lexical-binding: t; -*-

(setq macher-agent-read-buffer-in-workspace-tool
      (gptel-make-tool
       :name "read_buffer_in_workspace"
       :description "Read the contents of a scoped buffer."
       :category "perception"
       :args '((:name "buffer_name" :type "string" :description "The name of the buffer to read")
               (:name "offset" :type "number" :optional t :description "Line number to start reading from (1-based)")
               (:name "limit" :type "number" :optional t :description "Number of lines to read")
               (:name "show_line_numbers" :type "boolean" :optional t :description "Include line numbers in output"))
       :async t
       :function (macher-agent-with-presentation-context (buffer-name offset limit show-line-numbers)
                                                         (let ((native-fn (get 'macher-agent-read-buffer-in-workspace-tool 'ptc-function)))
                                                           (funcall native-fn buffer-name offset limit show-line-numbers context)))))

(put 'macher-agent-read-buffer-in-workspace-tool 'ptc-function
     (lambda (buffer-name &optional offset limit show-line-numbers context)
       (let* ((ctx (or (macher-agent-resolve-context context)
                       (macher-agent-resolve-context (current-buffer))
                       (macher-agent-resolve-context nil)))
              (actual-name (macher-agent--resolve-buffer-name buffer-name))
              (parsed-offset (if (numberp offset) (round offset) 1))
              (parsed-limit (if (numberp limit) (round limit) -1))
              (show-nums (and show-line-numbers (not (eq show-line-numbers :json-false)))))
         (when (macher-agent-valid-context-p ctx)
           (macher-agent--ensure-access ctx actual-name))
         (let* ((norm-key (if (macher-agent-valid-context-p ctx)
                              (macher-agent--normalize-path-key actual-name ctx)
                            actual-name))
                (vfs-ht (when (macher-agent-valid-context-p ctx)
                          (macher-agent-workspace-vfs-buffers ctx)))
                (raw (and (hash-table-p vfs-ht)
                          (or (gethash norm-key vfs-ht) (gethash actual-name vfs-ht))))
                (entry (when (macher-agent-vfs-entry-p raw) raw))
                (content
                 (cond
                  (entry
                   (when (macher-agent-valid-context-p ctx)
                     (let ((ctx-root (or (macher-agent-context-project-root ctx) default-directory)))
                       (macher-agent--sync-context-entry entry ctx-root ctx)))
                   (macher-agent-vfs-entry-curr entry))
                  ((stringp raw) raw)
                  (t
                   (or (macher-agent--read-content-from-disk-or-buffer actual-name) "")))))
           (macher-agent--read-string parsed-offset parsed-limit content show-nums)))))
