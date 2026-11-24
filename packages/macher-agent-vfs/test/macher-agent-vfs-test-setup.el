;;; macher-agent-vfs-test-setup.el --- Test Setup Adapter for macher-agent-vfs -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Standalone Buttercup test setup for `macher-agent-vfs'.

;;; Code:

(setq load-prefer-newer t)

(let* ((this-file (or load-file-name buffer-file-name))
       (this-dir (if this-file
                     (file-name-directory (expand-file-name this-file))
                   (expand-file-name default-directory)))
       (vfs-dir (or (locate-dominating-file this-dir "macher-agent-vfs.el")
                    (locate-dominating-file default-directory "macher-agent-vfs.el")
                    (expand-file-name ".." this-dir)))
       (core-dir (expand-file-name "../macher-agent" vfs-dir))
       (sbx-dir (expand-file-name "../macher-agent-sandbox" vfs-dir))
       (push-front
        (lambda (dir)
          (when (and dir (file-directory-p dir))
            (let ((norm (file-name-as-directory (expand-file-name dir))))
              (setq load-path
                    (cons norm
                          (delete norm
                                  (delete (directory-file-name norm) load-path)))))))))
  (dolist (rel '("../../gptel" "gptel"))
    (funcall push-front (expand-file-name rel vfs-dir)))
  (dolist (rel '("../../macher" "macher"))
    (funcall push-front (expand-file-name rel vfs-dir)))
  (when (file-directory-p core-dir)
    (funcall push-front core-dir)
    (funcall push-front (expand-file-name "test" core-dir))
    (funcall push-front (expand-file-name "skills/scripts" core-dir)))
  (when (file-directory-p sbx-dir)
    (funcall push-front sbx-dir)
    (funcall push-front (expand-file-name "skills/scripts" sbx-dir)))
  (when (and vfs-dir (file-directory-p vfs-dir))
    (funcall push-front vfs-dir)
    (funcall push-front (expand-file-name "test" vfs-dir))
    (funcall push-front (expand-file-name "skills/scripts" vfs-dir))))

(require 'macher-agent-test-helpers)
(require 'macher-agent-sandbox)
(require 'macher-agent-vfs)
(macher-agent-install)

;; Load VFS bundled skill scripts so their tool definitions are registered
(let ((scripts-dir (expand-file-name "scripts" (macher-agent-vfs-skills-directory))))
  (when (file-directory-p scripts-dir)
    (dolist (script (directory-files scripts-dir t "\\.el\\'"))
      (load script nil t))))

(provide 'macher-agent-vfs-test-setup)
;;; macher-agent-vfs-test-setup.el ends here
