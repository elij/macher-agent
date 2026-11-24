;;; read_media_in_workspace.el --- Read media in workspace tool -*- lexical-binding: t; -*-

(require 'mailcap)

(setq macher-agent-read-media-in-workspace-tool
      (gptel-make-tool
       :name "read_media_in_workspace"
       :description "Read a media file from workspace into visual context."
       :category "perception"
       :args '((:name "media_path" :type "string" :description "Path to media file"))
       :async t
       :function (macher-agent-with-presentation-context (media-path)
                   (unless (and (fboundp 'macher-agent-vfs-ensure-access)
                                (fboundp 'macher-agent-vfs-resolve-safe-path)
                                (fboundp 'macher-agent-vfs-read-context-file))
                     (error "Macher-Agent VFS plugin (`macher-agent-vfs') is not loaded"))
                   (let* ((ctx (or (when (macher-agent-valid-context-p context) context)
                                   (macher-agent-resolve-context (current-buffer))))
                          (workspace-root (when (macher-agent-valid-context-p ctx)
                                            (macher-agent-context-project-root ctx)))
                          (actual-name (macher-agent-resolve-buffer-name media-path)))
                     (unless (and (macher-agent-valid-context-p ctx)
                                  (stringp workspace-root)
                                  (not (string-empty-p workspace-root)))
                       (error "SECURITY ERROR: Valid workspace context required to read media"))
                     (funcall (symbol-function 'macher-agent-vfs-ensure-access) ctx actual-name)
                     (let ((abs-path (funcall (symbol-function 'macher-agent-vfs-resolve-safe-path)
                                              actual-name workspace-root)))
                       (unless (bound-and-true-p gptel-track-media)
                         (error "gptel media send option is off (gptel-track-media is nil)"))
                       (let* ((media-content (funcall (symbol-function 'macher-agent-vfs-read-context-file)
                                                      ctx actual-name))
                              (mime (mailcap-file-name-to-mime-type abs-path)))
                         (unless mime
                           (error "Could not determine MIME type for media: %s" abs-path))
                         (let* ((base64-str
                                 (with-temp-buffer
                                   (set-buffer-multibyte nil)
                                   (if (multibyte-string-p media-content)
                                       (insert (encode-coding-string media-content 'utf-8))
                                     (insert media-content))
                                   (base64-encode-region (point-min) (point-max) t)
                                   (buffer-string)))
                                (media-spec (cons base64-str (list :mime mime))))
                           (setf (macher-agent-context-media-queue ctx) media-spec)
                           (setf (macher-agent-context-plugins ctx)
                                 (plist-put (copy-sequence (macher-agent-context-plugins ctx))
                                            :pending-media media-spec))
                           (format "SUCCESS: Media '%s' has been successfully read." actual-name))))))))
