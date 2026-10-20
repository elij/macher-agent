;;; read_media_in_workspace.el --- Read media in workspace tool -*- lexical-binding: t; -*-

(setq macher-agent-read-media-in-workspace-tool
      (gptel-make-tool
       :name "read_media_in_workspace"
       :description "Read a media file from workspace into visual context."
       :category "perception"
       :args '((:name "media_path" :type "string" :description "Path to media file"))
       :async t
       :function (macher-agent-with-presentation-context (media-path)
                                                         (let* ((native-fn (get 'macher-agent-read-media-in-workspace-tool 'ptc-function))
                                                                (res (funcall native-fn media-path context)))
                                                           (format "SUCCESS: Media '%s' has been successfully read." (cdr (assoc 'media_path res)))))))

(put 'macher-agent-read-media-in-workspace-tool 'ptc-function
     (lambda (media-path context)
       (let* ((workspace-root (when (macher-agent-valid-context-p context)
                                (macher-agent-context-project-root context)))
              (actual-name (macher-agent--resolve-buffer-name media-path))
              (abs-path (if workspace-root (expand-file-name actual-name workspace-root) (expand-file-name actual-name)))
              (vfs-contents (when (macher-agent-valid-context-p context) (macher-agent--get-context-contents context)))
              (in-vfs (cl-find actual-name vfs-contents :key #'macher-agent-vfs-entry-path :test #'equal)))
         (unless (and (boundp 'gptel-track-media) gptel-track-media)
           (error "gptel media send option is off (gptel-track-media is nil)"))
         (unless (or in-vfs (file-exists-p abs-path))
           (error "Cannot read media. File does not exist at: %s" abs-path))
         (let ((mime (mailcap-file-name-to-mime-type abs-path)))
           (unless mime (error "Could not determine MIME type for media: %s" abs-path))
           (let* ((base64-str
                   (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (if in-vfs (insert (or (macher-agent-vfs-entry-curr in-vfs) ""))
                       (insert-file-contents-literally abs-path))
                     (base64-encode-region (point-min) (point-max) t)
                     (buffer-string)))
                  (media-spec (cons base64-str (list :mime mime))))
             (when (macher-agent-valid-context-p context)
               (setf (macher-agent-context-media-queue context) media-spec)
               (setf (macher-agent-context-plugins context)
                     (plist-put (copy-sequence (macher-agent-context-plugins context)) :pending-media media-spec)))
             `((status . "success") (media_path . ,actual-name) (mime . ,mime)))))))
