;;; macher-agent-skills-test-setup.el --- Test setup for macher-agent-skills -*- lexical-binding: t; -*-

(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (pkg-dir (expand-file-name ".." this-dir))
       (core-dir (expand-file-name "../macher-agent" pkg-dir))
       (core-test-dir (expand-file-name "test" core-dir))
       (repo-root (expand-file-name "../.." pkg-dir))
       (gptel-dir (expand-file-name "gptel" repo-root)))
  (when (file-directory-p gptel-dir)
    (add-to-list 'load-path gptel-dir))
  (add-to-list 'load-path core-dir)
  (add-to-list 'load-path core-test-dir)
  (add-to-list 'load-path pkg-dir)
  (add-to-list 'load-path this-dir))

(require 'buttercup)
(require 'macher-agent-test-helpers)
(require 'macher-agent)
(require 'macher-agent-skills)

(provide 'macher-agent-skills-test-setup)
;;; macher-agent-skills-test-setup.el ends here
