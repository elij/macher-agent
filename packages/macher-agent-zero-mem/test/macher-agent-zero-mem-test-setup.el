;;; macher-agent-zero-mem-test-setup.el --- Test Setup Adapter for macher-agent-zero-mem -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Standalone Buttercup test setup for `macher-agent-zero-mem'.

;;; Code:

(setq load-prefer-newer t)

(let* ((this-file (or load-file-name buffer-file-name))
       (this-dir (if this-file
                     (file-name-directory (expand-file-name this-file))
                   (expand-file-name default-directory)))
       (zm-dir (or (locate-dominating-file this-dir "macher-agent-zero-mem.el")
                   (locate-dominating-file default-directory "macher-agent-zero-mem.el")
                   (expand-file-name ".." this-dir)))
       (core-dir (expand-file-name "../macher-agent" zm-dir))
       (push-front
        (lambda (dir)
          (when (and dir (file-directory-p dir))
            (let ((norm (file-name-as-directory (expand-file-name dir))))
              (setq load-path
                    (cons norm
                          (delete norm
                                  (delete (directory-file-name norm) load-path)))))))))
  (dolist (rel '("../../gptel" "gptel"))
    (funcall push-front (expand-file-name rel zm-dir)))
  (when (file-directory-p core-dir)
    (funcall push-front core-dir)
    (funcall push-front (expand-file-name "test" core-dir))
    (funcall push-front (expand-file-name "skills/scripts" core-dir)))
  (when (and zm-dir (file-directory-p zm-dir))
    (funcall push-front zm-dir)
    (funcall push-front (expand-file-name "test" zm-dir))
    (funcall push-front (expand-file-name "skills/scripts" zm-dir))))

(require 'macher-agent-test-helpers)
(require 'macher-agent-zero-mem)
(macher-agent-install)

(provide 'macher-agent-zero-mem-test-setup)
;;; macher-agent-zero-mem-test-setup.el ends here
