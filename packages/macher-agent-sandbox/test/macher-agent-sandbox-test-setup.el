;;; macher-agent-sandbox-test-setup.el --- Test Setup Adapter for macher-agent-sandbox -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Standalone Buttercup test setup for `macher-agent-sandbox'.

;;; Code:

(setq load-prefer-newer t)

(let* ((this-file (or load-file-name buffer-file-name))
       (this-dir (if this-file
                     (file-name-directory (expand-file-name this-file))
                   (expand-file-name default-directory)))
       (sbx-dir (or (locate-dominating-file this-dir "macher-agent-sandbox.el")
                    (locate-dominating-file default-directory "macher-agent-sandbox.el")
                    (expand-file-name ".." this-dir)))
       (core-dir (expand-file-name "../macher-agent" sbx-dir))
       (push-front
        (lambda (dir)
          (when (and dir (file-directory-p dir))
            (let ((norm (file-name-as-directory (expand-file-name dir))))
              (setq load-path
                    (cons norm
                          (delete norm
                                  (delete (directory-file-name norm) load-path)))))))))
  (dolist (rel '("../../gptel" "gptel"))
    (funcall push-front (expand-file-name rel sbx-dir)))
  (when (file-directory-p core-dir)
    (funcall push-front core-dir)
    (funcall push-front (expand-file-name "test" core-dir))
    (funcall push-front (expand-file-name "skills/scripts" core-dir)))
  (when (and sbx-dir (file-directory-p sbx-dir))
    (funcall push-front sbx-dir)
    (funcall push-front (expand-file-name "test" sbx-dir))
    (funcall push-front (expand-file-name "skills/scripts" sbx-dir))))

(require 'macher-agent-test-helpers)
(require 'macher-agent-sandbox)
(macher-agent-install)

(provide 'macher-agent-sandbox-test-setup)
;;; macher-agent-sandbox-test-setup.el ends here
