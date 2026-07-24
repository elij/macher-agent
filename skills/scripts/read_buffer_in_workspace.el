(macher-agent-make-tool macher-agent-read-buffer-in-workspace-tool
    "Read the contents of a scoped buffer (ie a buffer in your allowed list)."
  :category "ro"
  :args '((:name "buffer_name" :type string :description "The name of the buffer to read")
          (:name "offset" :type number :optional t :description "Line number to start reading from (1-based)")
          (:name "limit" :type number :optional t :description "Number of lines to read")
          (:name "show_line_numbers" :type boolean :optional t :description "Include line numbers in output"))
  :command-fn (lambda (payload context _root)
                (let* ((buffer_name (plist-get payload :buffer_name))
                       (offset (plist-get payload :offset))
                       (limit (plist-get payload :limit))
                       (show_line_numbers (and (plist-get payload :show_line_numbers) 
                                               (not (eq
                                                     (plist-get payload :show_line_numbers)
                                                     :json-false))))
                       (actual-name (macher-agent--resolve-buffer-name buffer_name))
                       (parsed-offset (when (numberp offset) (round offset)))
                       (parsed-limit (when (numberp limit) (round limit))))
                  (macher-agent--ensure-access context actual-name)
                  (let* ((norm-key (macher-agent--normalize-path-key actual-name context))
                         (contents (macher-agent--get-context-contents context))
                         (entry (or (cl-find norm-key contents :key #'car :test #'equal)
                                    (cl-find actual-name contents :key #'car :test #'equal)))
                         (content (if entry (macher-agent-vfs-entry-curr entry)
                                    (let ((raw-str (cond
                                                    ((get-buffer actual-name)
                                                     (with-current-buffer (get-buffer actual-name)
                                                       (buffer-substring-no-properties (point-min) (point-max))))
                                                    ((and (not (file-name-absolute-p actual-name))
                                                          (not (string-prefix-p "~" actual-name))
                                                          (file-exists-p actual-name))
                                                     (with-temp-buffer
                                                       (insert-file-contents actual-name)
                                                       (buffer-string)))
                                                    (t ""))))
                                      (when context
                                        (let ((old-contents (macher-agent--get-context-contents context)))
                                          (unless (or (cl-find norm-key old-contents :key #'car :test #'equal)
                                                      (cl-find actual-name old-contents :key #'car :test #'equal))
                                            (macher-agent--set-context-contents
                                             context
                                             (cons (macher-agent-vfs-make-entry norm-key raw-str raw-str)
                                                   old-contents)))))
                                      raw-str))))
                    (make-macher-agent-lisp-result-response
                     :payload (macher--read-string
                               content parsed-offset parsed-limit show_line_numbers))))))
