;;; macher-agent-test-setup.el --- Test Setup Adapter -*- lexical-binding: t; no-byte-compile: t; -*-

(let* ((this-file (or load-file-name buffer-file-name))
       (this-dir (if this-file
                     (file-name-directory (expand-file-name this-file))
                   (expand-file-name default-directory)))
       (pkg-dir (or (locate-dominating-file this-dir "macher-agent.el")
                    (locate-dominating-file default-directory "macher-agent.el")
                    (expand-file-name ".." this-dir))))
  (when (and pkg-dir (file-directory-p pkg-dir))
    (add-to-list 'load-path (file-name-as-directory (expand-file-name pkg-dir)))
    (dolist (rel '("gptel" "../../gptel"))
      (let ((gptel-dir (expand-file-name rel pkg-dir)))
        (when (file-directory-p gptel-dir)
          (add-to-list 'load-path (file-name-as-directory gptel-dir)))))
    (let ((skills-dir (expand-file-name "skills/scripts" pkg-dir)))
      (when (file-directory-p skills-dir)
        (add-to-list 'load-path (file-name-as-directory skills-dir))))
    (add-to-list 'load-path (file-name-as-directory (expand-file-name "test" pkg-dir)))))

(require 'macher-agent-test-helpers)

(provide 'macher-agent-test-setup)
;;; macher-agent-test-setup.el ends here
