;;; macher-agent-wire-test-setup.el --- Test Setup Adapter for macher-agent-wire -*- lexical-binding: t; no-byte-compile: t; -*-

(setq load-prefer-newer t)

(let* ((this-file (or load-file-name buffer-file-name))
       (this-dir (if this-file
                     (file-name-directory (expand-file-name this-file))
                   (expand-file-name default-directory)))
       (wire-dir (or (locate-dominating-file this-dir "macher-agent-wire.el")
                     (locate-dominating-file default-directory "macher-agent-wire.el")
                     (expand-file-name ".." this-dir)))
       (core-dir (expand-file-name "../macher-agent" wire-dir))
       (push-front
        (lambda (dir)
          (when (and dir (file-directory-p dir))
            (let ((norm (file-name-as-directory (expand-file-name dir))))
              (setq load-path
                    (cons norm
                          (delete norm
                                  (delete (directory-file-name norm) load-path)))))))))
  (dolist (rel '("../../gptel" "gptel"))
    (funcall push-front (expand-file-name rel wire-dir)))
  (when (file-directory-p core-dir)
    (funcall push-front core-dir)
    (funcall push-front (expand-file-name "test" core-dir))
    (funcall push-front (expand-file-name "skills/scripts" core-dir)))
  (when (and wire-dir (file-directory-p wire-dir))
    (funcall push-front wire-dir)
    (funcall push-front (expand-file-name "test" wire-dir))))

(require 'macher-agent-test-helpers)
(require 'macher-agent-wire)
(macher-agent-install)

(provide 'macher-agent-wire-test-setup)
;;; macher-agent-wire-test-setup.el ends here
