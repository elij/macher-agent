;;; read_buffer_in_workspace.el --- Read buffer in workspace tool -*- lexical-binding: t; -*-

(setq macher-agent-read-buffer-in-workspace-tool
      (gptel-make-tool
       :name "read_buffer_in_workspace"
       :description "Read the contents of a scoped buffer (that is, a buffer in your allowed list)."
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
       (let* ((actual-name (macher-agent--resolve-buffer-name buffer-name))
              (parsed-offset (when (numberp offset) (round offset)))
              (parsed-limit (when (numberp limit) (round limit)))
              (show-nums (and show-line-numbers (not (eq show-line-numbers :json-false)))))
         (when (macher-agent-valid-context-p context)
           (macher-agent--ensure-access context actual-name))
         (let* ((norm-key (if (macher-agent-valid-context-p context)
                              (macher-agent--normalize-path-key actual-name context)
                            actual-name))
                (contents (when (macher-agent-valid-context-p context)
                            (macher-agent--get-context-contents context)))
                (entry (or (cl-find norm-key contents :key #'macher-agent-vfs-entry-path :test #'equal)
                           (cl-find actual-name contents :key #'macher-agent-vfs-entry-path :test #'equal)))
                (content
                 (if entry
                     (progn
                       (when (macher-agent-valid-context-p context)
                         (let ((mtime-ht (ignore-errors (macher-agent-workspace-mtime-tracker context)))
                               (ctx-root (macher-agent-context-project-root context)))
                           (macher-agent--sync-context-entry entry ctx-root mtime-ht)))
                       (macher-agent-vfs-entry-curr entry))
                   (let ((raw-str (or (macher-agent--read-content-from-disk-or-buffer actual-name) "")))
                     (when (macher-agent-valid-context-p context)
                       (let ((old-contents (macher-agent--get-context-contents context)))
                         (unless (or (cl-find norm-key old-contents :key #'macher-agent-vfs-entry-path :test #'equal)
                                     (cl-find actual-name old-contents :key #'macher-agent-vfs-entry-path :test #'equal))
                           (macher-agent--set-context-contents
                            context
                            (cons (macher-agent-vfs-make-entry norm-key raw-str raw-str)
                                  old-contents)))))
                     raw-str))))
           (macher-agent--read-string content parsed-offset parsed-limit show-nums)))))
