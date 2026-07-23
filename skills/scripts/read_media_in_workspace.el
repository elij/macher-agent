(macher-agent-make-tool macher-agent-read-media-in-workspace-tool
    "Read a media file (for example, an image) from the workspace into \
the agent's visual context."
  :category "ro"
  :args '((:name "media_path" :type string :description "The path to the media file \
relative to the workspace root."))
  :command-fn (lambda (payload context root)
                (let* ((media_path (plist-get payload :media_path))
                       (workspace
                        (when context
                          (macher-agent--get-context-workspace context)))
                       (workspace-root
                        (when context
                          (macher-context-workspace-root context)))
                       (actual-name (if (fboundp 'macher-agent--resolve-buffer-name)
                                        (macher-agent--resolve-buffer-name media_path)
                                      media_path))
                       (abs-path (if workspace-root
                                     (expand-file-name actual-name workspace-root)
                                   (expand-file-name actual-name)))
                       (vfs-contents
                        (when context
                          (macher-agent--get-context-contents context)))
                       (in-vfs
                        (cl-find actual-name vfs-contents :key #'macher-agent-vfs-entry-path :test #'equal)))

                  (unless (and (boundp 'gptel-track-media) gptel-track-media)
                    (error "gptel media send option is off (gptel-track-media is nil)"))

                  (unless (or in-vfs (file-exists-p abs-path))
                    (error "Cannot read media. The file does not exist in VFS or on disk \
at: %s" abs-path))

                  (let* ((mime (mailcap-file-name-to-mime-type abs-path))
                         (info (when (bound-and-true-p macher-agent--active-fsm)
                                 (macher-agent--extract-fsm-info macher-agent--active-fsm))))
                    (unless mime
                      (error "Could not determine MIME type for media: %s" abs-path))

                    (let ((base64-str
                           (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (if in-vfs
                                 (insert (or (macher-agent-vfs-entry-curr in-vfs) ""))
                               (insert-file-contents-literally abs-path))
                             (base64-encode-region (point-min) (point-max) t)
                             (buffer-string))))
                      (when context
                        (let ((pending
                               (macher-agent--get-context-data context :pending-media)))
                          (setq pending (append pending (list (list base64-str :mime mime))))
                          (macher-agent--set-context-data context :pending-media pending)
                          (when info
                            (unless (plist-get info :macher-agent-context)
                              (setf (gptel-fsm-info macher-agent--active-fsm)
                                    (plist-put info :macher-agent-context context)))))))

                    (make-macher-agent-lisp-result-response
                     :payload (format "SUCCESS: Media '%s' has been successfully read and \
attached to this response. You may now analyse it immediately." actual-name))))))
