(macher-agent-make-tool macher-agent-read-buffer-in-workspace-tool
    "Read the contents of a scoped buffer (ie a buffer in your allowed list)."
  :category "ro"
  :args '((:name "buffer_name" :type string :description "The name of the buffer to read")
          (:name "offset" :type number :optional t :description "Line number to start \
reading from (1-based)")
          (:name "limit" :type number :optional t :description "Number of lines to read")
          (:name "show_line_numbers" :type boolean :optional t :description "Include line \
numbers in output"))
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
                  (let* ((contents
                          (cl-find actual-name (macher-agent--get-context-contents context)
                                   :key #'macher-agent-vfs-entry-path :test #'equal))
                         (content (if contents (macher-agent-vfs-entry-curr contents)
                                    (with-current-buffer (get-buffer actual-name)
                                      (buffer-substring-no-properties
                                       (point-min) (point-max))))))
                    (make-macher-agent-lisp-result-response
                     :payload (macher--read-string
                               content parsed-offset parsed-limit show_line_numbers))))))
