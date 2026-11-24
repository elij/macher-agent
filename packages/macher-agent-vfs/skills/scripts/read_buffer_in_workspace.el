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
       (unless (and (fboundp 'macher-agent-vfs-ensure-access)
                    (fboundp 'macher-agent-vfs-read-context-file)
                    (fboundp 'macher-agent-vfs-read-string))
         (error "Macher-Agent VFS plugin (`macher-agent-vfs') is not loaded"))
       (let* ((fsm (macher-agent-get-active-fsm))
              (ctx (or (when context (macher-agent-resolve-context context))
                       (and fsm (macher-agent-gptel-context-from-fsm fsm))
                       (macher-agent-resolve-context (current-buffer))
                       (macher-agent-resolve-context nil)))
              (actual-name (macher-agent-resolve-buffer-name buffer-name))
              (live-buf (get-buffer actual-name))
              (target-path (or (and live-buf (buffer-file-name live-buf)) actual-name))
              (parsed-offset (if (numberp offset) (round offset) 1))
              (parsed-limit (if (numberp limit) (round limit) 0))
              (show-nums (and show-line-numbers (not (eq show-line-numbers :json-false)))))
         (when (macher-agent-valid-context-p ctx)
           (funcall (symbol-function 'macher-agent-vfs-ensure-access) ctx actual-name))
         (let ((content
                (if (macher-agent-valid-context-p ctx)
                    (funcall (symbol-function 'macher-agent-vfs-read-context-file)
                             ctx target-path)
                  (or (funcall (symbol-function 'macher-agent-vfs-read-content-from-disk-or-buffer)
                               target-path)
                      ""))))
           (funcall (symbol-function 'macher-agent-vfs-read-string)
                    parsed-offset parsed-limit content show-nums)))))
