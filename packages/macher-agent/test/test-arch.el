;;; test-arch.el --- Module architecture and static invariants -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Specification 1: Module architecture and static invariants
;; ARCH-01, ARCH-02, ARCH-03, ARCH-04, ARCH-05, ARCH-06

;;; Code:

(require 'cl-lib)
(require 'lisp-mnt)
(require 'buttercup)
(require 'macher-agent-test-helpers)

(describe "1. Module architecture and static invariants"

  (before-each
    (macher-agent-test-reset-env))

  ;; --------------------------------------------------------------------------
  ;; ARCH-01
  ;; --------------------------------------------------------------------------
  (it "ARCH-01: byte-compiles all package modules and skill scripts in clean subprocess with zero warnings"
    (let* ((root (file-name-directory (locate-library "macher-agent")))
           (gptel-dir (file-name-directory (locate-library "gptel")))
           (emacs-bin (expand-file-name invocation-name invocation-directory))
           (package-modules
            (directory-files root t "^macher-agent\\(-[a-z]+\\)*\\.el$"))
           (scripts-dir (expand-file-name "skills/scripts" root))
           (skill-scripts
            (when (file-directory-p scripts-dir)
              (directory-files scripts-dir t "\\.el$")))
           (all-files (append package-modules skill-scripts))
           (out-buf (generate-new-buffer " *arch01-compile-out*")))
      (unwind-protect
          (dolist (file all-files)
            (with-current-buffer out-buf (erase-buffer))
            (let* ((is-pkg-mod (member file package-modules))
                   (eval-code
                    (if is-pkg-mod
                        "(setq byte-compile-error-on-warn t byte-compile-warnings '(not obsolete docstrings))"
                      (format "(progn (setq byte-compile-error-on-warn t byte-compile-warnings '(not obsolete docstrings)) (dolist (feat '(gptel macher-agent-core macher-agent-presets macher-agent-gptel macher-agent-tools macher-agent-orchestration macher-agent-api macher-agent transient)) (require feat nil t)) (let ((sdir %S)) (when (file-directory-p sdir) (dolist (f (directory-files sdir t \"\\\\.el$\")) (let* ((b (file-name-sans-extension (file-name-nondirectory f))) (h (intern (format \"macher-agent-%%s-tool\" (replace-regexp-in-string \"_\" \"-\" b)))) (u (intern (format \"macher-agent-%%s-tool\" (replace-regexp-in-string \"-\" \"_\" b))))) (eval `(defvar ,h nil)) (eval `(defvar ,u nil)))))))"
                              (expand-file-name "skills/scripts" root))))
                   (args (list "-Q" "--batch"
                               "-L" (expand-file-name root)
                               "-L" (expand-file-name gptel-dir)
                               "-L" (expand-file-name "skills/scripts" root)
                               "--eval" eval-code
                               "-f" "batch-byte-compile"
                               file))
                   (exit-code (apply #'call-process emacs-bin nil out-buf nil args))
                   (output (with-current-buffer out-buf (buffer-string))))
              (let ((elc (concat (file-name-sans-extension file) ".elc")))
                (when (file-exists-p elc)
                  (delete-file elc)))
              (when (/= exit-code 0)
                (error "ARCH-01 FAILED ON %s (exit %s): %s" file exit-code output))
              (expect exit-code :to-equal 0)
              (expect output :not :to-match "Warning:")
              (expect output :not :to-match "free variable")
              (expect output :not :to-match "unresolved call")))
        (when (buffer-live-p out-buf)
          (kill-buffer out-buf)))))

  ;; --------------------------------------------------------------------------
  ;; ARCH-02
  ;; --------------------------------------------------------------------------
  (it "ARCH-02: verifies core Directed Acyclic Graph without circular, plugin, or macher requires"
    (let* ((root (file-name-directory (locate-library "macher-agent")))
           (tier-0 '(macher-agent-core))
           (tier-1 '(macher-agent-presets macher-agent-tools macher-agent-gptel))
           (tier-2 '(macher-agent-orchestration macher-agent-api macher-agent))
           (forbidden-requires '(macher macher-agent-wire macher-agent-vfs macher-agent-sandbox macher-agent-zero-mem macher-agent-skills macher-agent-macher))
           (all-modules (append tier-0 tier-1 tier-2))
           (require-graph (make-hash-table :test 'eq)))

      ;; Extract require graph from ASTs of all core package modules
      (dolist (mod all-modules)
        (let* ((file (expand-file-name (format "%s.el" mod) root))
               (reqs nil))
          (expect (file-exists-p file) :to-be t)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (condition-case nil
                (while t
                  (let ((form (read (current-buffer))))
                    (when (and (consp form) (eq (car form) 'require))
                      (let ((feat (cadr form)))
                        (when (and (consp feat) (eq (car feat) 'quote))
                          (setq feat (cadr feat)))
                        (expect (memq feat forbidden-requires) :to-be nil)
                        (when (memq feat all-modules)
                          (push feat reqs))))))
              (end-of-file nil)))
          (puthash mod (delete-dups reqs) require-graph)))

      ;; Tier 0 must not require Tiers 1 or 2; Tier 1 must not require Tier 2
      (dolist (r (gethash 'macher-agent-core require-graph))
        (expect (memq r (append tier-1 tier-2)) :to-be nil))
      (dolist (mod tier-1)
        (dolist (r (gethash mod require-graph))
          (expect (memq r tier-2) :to-be nil)))

      ;; Assert: Require graph is a Directed Acyclic Graph (no cycles)
      (let ((visited (make-hash-table :test 'eq))
            (rec-stack (make-hash-table :test 'eq))
            (has-cycle nil))
        (cl-labels ((dfs (node)
                      (puthash node t visited)
                      (puthash node t rec-stack)
                      (dolist (neighbor (gethash node require-graph nil))
                        (cond
                         ((gethash neighbor rec-stack)
                          (setq has-cycle t))
                         ((not (gethash neighbor visited))
                          (dfs neighbor))))
                      (puthash node nil rec-stack)))
          (dolist (node all-modules)
            (unless (gethash node visited)
              (dfs node)))
          (expect has-cycle :to-be nil)))))

  ;; --------------------------------------------------------------------------
  ;; ARCH-03
  ;; --------------------------------------------------------------------------
  (it "ARCH-03: verifies zero internal declare-function forms targeting macher-agent symbols"
    (let* ((root (file-name-directory (locate-library "macher-agent")))
           (packages-dir (expand-file-name ".." root))
           (pkg-dirs
            (if (string= (file-name-nondirectory (directory-file-name packages-dir)) "packages")
                (cl-remove-if-not
                 #'file-directory-p
                 (mapcar (lambda (d) (expand-file-name d packages-dir))
                         '("macher-agent" "macher-agent-wire" "macher-agent-zero-mem"
                           "macher-agent-sandbox" "macher-agent-vfs" "macher-agent-skills")))
              (list root)))
           (files
            (apply #'append
                   (mapcar (lambda (d)
                             (directory-files d t "^macher-agent\\(-[a-z]+\\)*\\.el$"))
                           pkg-dirs)))
           (internal-declares nil))
      (dolist (file files)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (while t
                (let ((form (read (current-buffer))))
                  (when (and (consp form) (eq (car form) 'declare-function))
                    (let ((fn (cadr form)))
                      (when (and (consp fn) (eq (car fn) 'quote))
                        (setq fn (cadr fn)))
                      (when (and (symbolp fn)
                                 (string-prefix-p "macher-agent-" (symbol-name fn)))
                        (push (list file fn form) internal-declares))))))
            (end-of-file nil))))
      (expect (length internal-declares) :to-equal 0)))

  ;; --------------------------------------------------------------------------
  ;; ARCH-04
  ;; --------------------------------------------------------------------------
  (it "ARCH-04: registers pipeline steps idempotently and unregisters cleanly without dangling functions"
    (clrhash macher-agent-pipeline-registry)
    (let* ((test-step (lambda (st) st))
           (installer (lambda ()
                        (macher-agent-register-pipeline-step 'transmission test-step 42)))
           (macher-agent-install-hook (list installer)))
      (macher-agent-install)
      (macher-agent-install)
      (macher-agent-install)

      ;; Verify all pipeline steps are registered without duplicate entries
      (maphash
       (lambda (_pipeline entries)
         (let ((steps (mapcar (lambda (e) (or (plist-get e :step) (plist-get e :fn))) entries)))
           (expect (length steps) :to-equal (length (delete-dups (copy-sequence steps))))))
       macher-agent-pipeline-registry)

      ;; Core transmission step and plugin-hook step are both present
      (expect (member #'macher-agent-pipe--hydrate-base-state
                      (macher-agent-get-pipeline-steps 'transmission))
              :to-be-truthy)
      (expect (member test-step
                      (macher-agent-get-pipeline-steps 'transmission))
              :to-be-truthy)

      ;; Unregister test step cleanly without dangling references
      (macher-agent-unregister-pipeline-step 'transmission test-step)
      (expect (member test-step
                      (macher-agent-get-pipeline-steps 'transmission))
              :to-be nil))
    ;; Restore pipeline state for subsequent tests
    (macher-agent-install))

  ;; --------------------------------------------------------------------------
  ;; ARCH-05
  ;; --------------------------------------------------------------------------
  (it "ARCH-05: verifies zero private macher-agent-- leaks in sibling packages and pure core modules/scripts"
    (let* ((root (file-name-directory (locate-library "macher-agent")))
           (core-files (directory-files root t "^macher-agent\\(-[a-z]+\\)*\\.el\\'"))
           (scripts-dir (expand-file-name "skills/scripts" root))
           (wire-file (expand-file-name "../macher-agent-wire/macher-agent-wire.el" root))
           (zm-file (expand-file-name "../macher-agent-zero-mem/macher-agent-zero-mem.el" root))
           (zm-scripts-dir (expand-file-name "../macher-agent-zero-mem/skills/scripts" root))
           (sbx-file (expand-file-name "../macher-agent-sandbox/macher-agent-sandbox.el" root))
           (sbx-scripts-dir (expand-file-name "../macher-agent-sandbox/skills/scripts" root))
           (vfs-file (expand-file-name "../macher-agent-vfs/macher-agent-vfs.el" root))
           (vfs-macher-file (expand-file-name "../macher-agent-vfs/macher-agent-macher.el" root))
           (vfs-scripts-dir (expand-file-name "../macher-agent-vfs/skills/scripts" root))
           (skills-file (expand-file-name "../macher-agent-skills/macher-agent-skills.el" root))
           (skills-scripts-dir (expand-file-name "../macher-agent-skills/skills/scripts" root)))
      ;; Core package modules must not reference ptc-function, active-ptc, or sibling private symbols
      (dolist (file core-files)
        (with-temp-buffer
          (insert-file-contents file)
          (let ((content (buffer-string)))
            (expect content :not :to-match "ptc-function")
            (expect content :not :to-match "active-ptc")
            (expect content :not :to-match "macher-agent-wire--")
            (expect content :not :to-match "macher-agent-zero-mem--")
            (expect content :not :to-match "macher-agent-sandbox--")
            (expect content :not :to-match "macher-agent-vfs--")
            (expect content :not :to-match "macher-agent-macher--"))))
      ;; Core meta scripts must not register ptc-function properties or touch private symbols
      (when (file-directory-p scripts-dir)
        (dolist (script (directory-files scripts-dir t "\\.el\\'"))
          (with-temp-buffer
            (insert-file-contents script)
            (let ((content (buffer-string)))
              (expect content :not :to-match "ptc-function")
              (expect content :not :to-match "active-ptc")
              (expect content :not :to-match "\\bmacher-agent--")
              (expect content :not :to-match "macher-agent-wire--")
              (expect content :not :to-match "macher-agent-zero-mem--")
              (expect content :not :to-match "macher-agent-sandbox--")
              (expect content :not :to-match "macher-agent-vfs--")
              (expect content :not :to-match "macher-agent-macher--")))))
      ;; Sibling macher-agent-wire package must call ONLY public macher-agent-* symbols
      (when (file-exists-p wire-file)
        (with-temp-buffer
          (insert-file-contents wire-file)
          (let ((content (buffer-string)))
            (expect content :not :to-match "\\bmacher-agent--")
            (expect content :not :to-match "macher-agent-zero-mem")
            (expect content :not :to-match "macher-agent-sandbox")
            (expect content :not :to-match "macher-agent-vfs")
            (expect content :not :to-match "macher-agent-macher"))))
      ;; Sibling macher-agent-zero-mem package must stay isolated from private/wire/sandbox/vfs symbols
      (when (file-exists-p zm-file)
        (with-temp-buffer
          (insert-file-contents zm-file)
          (let* ((content (buffer-string))
                 (stripped-wire-hooks
                  (replace-regexp-in-string
                   "macher-agent-wire-\\(?:pre-truncate\\|invalidated\\)-hook"
                   "" content)))
            (expect content :not :to-match "\\bmacher-agent--")
            (expect content :not :to-match "\\bgptel--")
            (expect content :not :to-match "ptc-function")
            (expect content :not :to-match "active-ptc")
            (expect content :not :to-match "macher-agent-sandbox")
            (expect content :not :to-match "macher-agent-vfs")
            (expect content :not :to-match "macher-agent-macher")
            (expect stripped-wire-hooks :not :to-match "macher-agent-wire"))))
      (when (file-directory-p zm-scripts-dir)
        (dolist (script (directory-files zm-scripts-dir t "\\.el\\'"))
          (with-temp-buffer
            (insert-file-contents script)
            (let ((content (buffer-string)))
              (expect content :not :to-match "\\bmacher-agent--")
              (expect content :not :to-match "\\bmacher-agent-zero-mem--")
              (expect content :not :to-match "\\bgptel--")
              (expect content :not :to-match "macher-agent-wire")
              (expect content :not :to-match "ptc-function")
              (expect content :not :to-match "active-ptc")
              (expect content :not :to-match "macher-agent-sandbox")
              (expect content :not :to-match "macher-agent-vfs")
              (expect content :not :to-match "macher-agent-macher")))))
      ;; Sibling macher-agent-sandbox package must stay isolated from private/wire/zero-mem/vfs symbols
      (when (file-exists-p sbx-file)
        (with-temp-buffer
          (insert-file-contents sbx-file)
          (let ((content (buffer-string)))
            (expect content :not :to-match "\\bmacher-agent--")
            (expect content :not :to-match "\\bgptel--")
            (expect content :not :to-match "macher-agent-wire")
            (expect content :not :to-match "macher-agent-zero-mem")
            (expect content :not :to-match "macher-agent-vfs")
            (expect content :not :to-match "macher-agent-macher"))))
      (when (file-directory-p sbx-scripts-dir)
        (dolist (script (directory-files sbx-scripts-dir t "\\.el\\'"))
          (with-temp-buffer
            (insert-file-contents script)
            (let ((content (buffer-string)))
              (expect content :not :to-match "\\bmacher-agent--")
              (expect content :not :to-match "\\bmacher-agent-sandbox--")
              (expect content :not :to-match "\\bgptel--")
              (expect content :not :to-match "macher-agent-wire")
              (expect content :not :to-match "macher-agent-zero-mem")
              (expect content :not :to-match "macher-agent-vfs")
              (expect content :not :to-match "macher-agent-macher")
              (when (string= (file-name-nondirectory script) "ptc_execution.el")
                (expect content :not :to-match "ptc-function"))))))
      ;; Sibling macher-agent-vfs package must stay isolated from private/wire/zero-mem symbols
      (dolist (vfile (list vfs-file vfs-macher-file))
        (when (file-exists-p vfile)
          (with-temp-buffer
            (insert-file-contents vfile)
            (let ((content (buffer-string)))
              (expect content :not :to-match "\\bmacher-agent--")
              (expect content :not :to-match "\\bmacher-agent-sandbox--")
              (expect content :not :to-match "\\bgptel--")
              (expect content :not :to-match "macher-agent-wire")
              (expect content :not :to-match "macher-agent-zero-mem")
              (when (string= (file-name-nondirectory vfile) "macher-agent-vfs.el")
                (expect content :not :to-match "\\bmacher-agent-macher--"))
              (when (string= (file-name-nondirectory vfile) "macher-agent-macher.el")
                (expect content :not :to-match "\\bmacher-agent-vfs--"))))))
      (when (file-directory-p vfs-scripts-dir)
        (dolist (script (directory-files vfs-scripts-dir t "\\.el\\'"))
          (with-temp-buffer
            (insert-file-contents script)
            (let ((content (buffer-string)))
              (expect content :not :to-match "\\bmacher-agent--")
              (expect content :not :to-match "\\bmacher-agent-sandbox--")
              (expect content :not :to-match "\\bmacher-agent-vfs--")
              (expect content :not :to-match "\\bmacher-agent-macher--")
              (expect content :not :to-match "\\bgptel--")
              (expect content :not :to-match "macher-agent-wire")
              (expect content :not :to-match "macher-agent-zero-mem")
              (when (string= (file-name-nondirectory script) "read_media_in_workspace.el")
                (expect content :not :to-match "ptc-function")))))
      ;; Sibling macher-agent-skills package must stay isolated from private/sandbox/vfs/zero-mem symbols
      (when (file-exists-p skills-file)
        (with-temp-buffer
          (insert-file-contents skills-file)
          (let ((content (buffer-string)))
            (expect content :not :to-match "\\bmacher-agent--")
            (expect content :not :to-match "\\bgptel--")
            (expect content :not :to-match "macher-agent-wire")
            (expect content :not :to-match "macher-agent-zero-mem")
            (expect content :not :to-match "macher-agent-sandbox")
            (expect content :not :to-match "macher-agent-vfs")
            (expect content :not :to-match "macher-agent-macher"))))
      (when (file-directory-p skills-scripts-dir)
        (dolist (script (directory-files skills-scripts-dir t "\\.el\\'"))
          (with-temp-buffer
            (insert-file-contents script)
            (let ((content (buffer-string)))
              (expect content :not :to-match "\\bmacher-agent--")
              (expect content :not :to-match "\\bmacher-agent-skills--")
              (expect content :not :to-match "ptc-function")
              (expect content :not :to-match "macher-agent-zero-mem")
              (expect content :not :to-match "macher-agent-sandbox")
              (expect content :not :to-match "macher-agent-vfs")
              (expect content :not :to-match "macher-agent-macher"))))))))

  ;; --------------------------------------------------------------------------
  ;; ARCH-06
  ;; --------------------------------------------------------------------------
  (it "ARCH-06: verifies zero stray root .el files and strict 5-package ELPA Package-Requires and require DAG"
    (let* ((core-root (file-name-directory (locate-library "macher-agent")))
           (packages-dir (expand-file-name ".." core-root))
           (repo-root (expand-file-name "../.." core-root)))
      (when (and (string= (file-name-nondirectory (directory-file-name packages-dir)) "packages")
                 (file-directory-p repo-root))
        ;; 1. Zero stray .el files at repository root
        (expect (directory-files repo-root nil "\\.el\\'") :to-be nil)

        ;; 2. Verify all 5 packages' Package-Requires and require DAG
        (let* ((pkg-specs
                '((macher-agent
                   :dir "macher-agent"
                   :main "macher-agent.el"
                   :required-deps (emacs gptel)
                   :forbidden-deps (macher macher-agent-wire macher-agent-zero-mem
                                           macher-agent-sandbox macher-agent-vfs
                                           macher-agent-macher))
                  (macher-agent-wire
                   :dir "macher-agent-wire"
                   :main "macher-agent-wire.el"
                   :required-deps (emacs gptel macher-agent)
                   :forbidden-deps (macher macher-agent-zero-mem macher-agent-sandbox
                                           macher-agent-vfs macher-agent-macher))
                  (macher-agent-zero-mem
                   :dir "macher-agent-zero-mem"
                   :main "macher-agent-zero-mem.el"
                   :required-deps (emacs gptel macher-agent)
                   :forbidden-deps (macher macher-agent-wire macher-agent-sandbox
                                           macher-agent-vfs macher-agent-macher))
                  (macher-agent-sandbox
                   :dir "macher-agent-sandbox"
                   :main "macher-agent-sandbox.el"
                   :required-deps (emacs gptel macher-agent)
                   :forbidden-deps (macher macher-agent-wire macher-agent-zero-mem
                                           macher-agent-vfs macher-agent-macher))
                  (macher-agent-vfs
                   :dir "macher-agent-vfs"
                   :main "macher-agent-vfs.el"
                   :required-deps (emacs gptel macher macher-agent macher-agent-sandbox)
                   :forbidden-deps (macher-agent-wire macher-agent-zero-mem macher-agent-skills))
                  (macher-agent-skills
                   :dir "macher-agent-skills"
                   :main "macher-agent-skills.el"
                   :required-deps (emacs gptel macher-agent)
                   :forbidden-deps (macher macher-agent-wire macher-agent-zero-mem
                                           macher-agent-sandbox macher-agent-vfs
                                           macher-agent-macher))))
               (all-pkgs (mapcar #'car pkg-specs))
               (pkg-graph (make-hash-table :test 'eq)))
          (cl-labels ((collect-requires (form acc)
                        (cond
                         ((and (consp form) (eq (car form) 'require)
                               (consp (cdr form)))
                          (let ((feat (cadr form)))
                            (when (and (consp feat) (eq (car feat) 'quote)
                                       (consp (cdr feat)))
                              (setq feat (cadr feat)))
                            (if (symbolp feat) (cons feat acc) acc)))
                         ((and (consp form) (eq (car form) 'quote))
                          acc)
                         ((consp form)
                          (let ((cur form))
                            (while (consp cur)
                              (setq acc (collect-requires (car cur) acc))
                              (setq cur (cdr cur)))
                            (if cur (collect-requires cur acc) acc)))
                         ((vectorp form)
                          (dotimes (i (length form))
                            (setq acc (collect-requires (aref form i) acc)))
                          acc)
                         (t acc))))
            (dolist (spec pkg-specs)
              (let* ((pkg-sym (car spec))
                     (plist (cdr spec))
                     (pdir (expand-file-name (plist-get plist :dir) packages-dir))
                     (main-file (expand-file-name (plist-get plist :main) pdir))
                     (required-deps (plist-get plist :required-deps))
                     (forbidden-deps (plist-get plist :forbidden-deps))
                     (el-files (append (directory-files pdir t "\\.el\\'")
                                       (let ((sdir (expand-file-name "skills/scripts" pdir)))
                                         (when (file-directory-p sdir)
                                           (directory-files sdir t "\\.el\\'")))))
                     (pkg-edges nil))
                ;; Main file exists and declares valid ELPA Package-Requires
                (expect (file-exists-p main-file) :to-be t)
                (with-temp-buffer
                  (insert-file-contents main-file)
                  (let* ((raw-req (lm-header "package-requires"))
                         (parsed (and (stringp raw-req) (car (read-from-string raw-req)))))
                    (expect (stringp raw-req) :to-be t)
                    (expect (listp parsed) :to-be t)
                    (expect (> (length parsed) 0) :to-be t)
                    (dolist (entry parsed)
                      (expect (consp entry) :to-be t)
                      (expect (symbolp (car entry)) :to-be t)
                      (expect (stringp (cadr entry)) :to-be t)
                      (expect (memq (car entry) forbidden-deps) :to-be nil)
                      (when (and (memq (car entry) all-pkgs)
                                 (not (eq (car entry) pkg-sym)))
                        (push (car entry) pkg-edges)))
                    (dolist (dep required-deps)
                      (expect (assq dep parsed) :to-be-truthy))))

                ;; Scan every .el file and skill script for forbidden or cyclic require forms
                (dolist (file el-files)
                  (let ((reqs nil))
                    (with-temp-buffer
                      (insert-file-contents file)
                      (goto-char (point-min))
                      (condition-case nil
                          (while t
                            (setq reqs (collect-requires (read (current-buffer)) reqs)))
                        (end-of-file nil)))
                    (dolist (feat reqs)
                      (expect (memq feat forbidden-deps) :to-be nil)
                      (when (and (memq feat all-pkgs)
                                 (not (eq feat pkg-sym)))
                        (push feat pkg-edges)))))
                (puthash pkg-sym (delete-dups pkg-edges) pkg-graph))))

          ;; 3. Confirm 5-package dependency graph is strictly acyclic
          (let ((visited (make-hash-table :test 'eq))
                (rec-stack (make-hash-table :test 'eq))
                (has-cycle nil))
            (cl-labels ((dfs (node)
                          (puthash node t visited)
                          (puthash node t rec-stack)
                          (dolist (neighbor (gethash node pkg-graph nil))
                            (cond
                             ((gethash neighbor rec-stack)
                              (setq has-cycle t))
                             ((not (gethash neighbor visited))
                              (dfs neighbor))))
                          (puthash node nil rec-stack)))
              (dolist (node all-pkgs)
                (unless (gethash node visited)
                  (dfs node)))
              (expect has-cycle :to-be nil))))))))

(provide 'test-arch)
;;; test-arch.el ends here
