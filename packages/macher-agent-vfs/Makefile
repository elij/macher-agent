.PHONY: all compile checkdoc package-lint lint test format fmt clean

EMACS ?= emacs

EL_FILES := macher-agent-macher.el macher-agent-vfs.el

define SETUP_DEPS
(progn \
  (setq load-prefer-newer t) \
  (require 'cl-lib) \
  (require 'package) \
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")) \
  (let ((local-gptel (file-directory-p (expand-file-name "../../gptel" default-directory))) \
        (local-macher (file-directory-p (expand-file-name "../../macher" default-directory))) \
        (register-local-pkg \
         (lambda (file) \
           (when (file-exists-p file) \
             (let ((desc (with-temp-buffer \
                           (insert-file-contents file) \
                           (ignore-errors (package-buffer-info))))) \
               (when desc \
                 (setf (package-desc-dir desc) (file-name-directory file)) \
                 (setq package-alist \
                       (cons (cons (package-desc-name desc) (list desc)) \
                             (assq-delete-all (package-desc-name desc) package-alist))))))))) \
    (setq package-load-list \
          (append \
           (when local-gptel '((gptel nil))) \
           (when local-macher '((macher nil))) \
           '((macher-agent nil) \
             (macher-agent-wire nil) \
             (macher-agent-vfs nil) \
             (macher-agent-sandbox nil) \
             (macher-agent-zero-mem nil) \
             all))) \
    (package-initialize) \
    (setq load-path \
          (delq nil \
                (mapcar (lambda (dir) \
                          (unless (and (stringp dir) \
                                       (or (string-match-p "/elpa/macher-agent\\(?:-\\(?:wire\\|vfs\\|sandbox\\|zero-mem\\)\\)?-[0-9]" dir) \
                                           (and local-gptel \
                                                (string-match-p "/elpa/gptel-[0-9]" dir)) \
                                           (and local-macher \
                                                (string-match-p "/elpa/macher-[0-9]" dir)))) \
                            dir)) \
                        load-path))) \
    (dolist (pkg '(gptel macher package-lint)) \
      (unless (or (package-installed-p pkg) \
                  (and (eq pkg 'gptel) local-gptel) \
                  (and (eq pkg 'macher) local-macher)) \
        (unless package-archive-contents (package-refresh-contents)) \
        (package-install pkg))) \
    (when local-gptel \
      (let ((gdir (expand-file-name "../../gptel" default-directory))) \
        (push gdir load-path) \
        (funcall register-local-pkg (expand-file-name "gptel.el" gdir)))) \
    (when local-macher \
      (let ((mdir (expand-file-name "../../macher" default-directory))) \
        (push mdir load-path) \
        (funcall register-local-pkg (expand-file-name "macher.el" mdir)))) \
    (let ((core-dir (expand-file-name "../macher-agent" default-directory))) \
      (when (file-directory-p core-dir) \
        (push core-dir load-path) \
        (funcall register-local-pkg (expand-file-name "macher-agent.el" core-dir)))) \
    (let ((sbx-dir (expand-file-name "../macher-agent-sandbox" default-directory))) \
      (when (file-directory-p sbx-dir) \
        (push sbx-dir load-path) \
        (funcall register-local-pkg (expand-file-name "macher-agent-sandbox.el" sbx-dir))))) \
  (push (expand-file-name default-directory) load-path))
endef
export SETUP_DEPS

define FORMAT_ELISP
(progn \
  (dolist (file command-line-args-left) \
    (with-current-buffer (find-file-noselect file) \
      (setq-local indent-tabs-mode nil) \
      (indent-region (point-min) (point-max)) \
      (delete-trailing-whitespace) \
      (save-buffer) \
      (message "Formatted %s" file))))
endef
export FORMAT_ELISP

define CHECKDOC_ELISP
(progn \
  (require 'checkdoc) \
  (let ((errors 0)) \
    (dolist (file command-line-args-left) \
      (let* ((diag-name "*checkdoc-output*") \
             (diag-buf (get-buffer-create diag-name))) \
        (with-current-buffer diag-buf \
          (let ((inhibit-read-only t)) \
            (erase-buffer))) \
        (with-current-buffer (find-file-noselect file) \
          (let ((checkdoc-autofix-flag nil) \
                (checkdoc-diagnostic-buffer diag-name)) \
            (checkdoc-current-buffer t))) \
        (with-current-buffer diag-buf \
          (goto-char (point-min)) \
          (let ((has-issues nil)) \
            (while (not (eobp)) \
              (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))) \
                (when (and (not (string-match-p "^\\s-*$$" line)) \
                           (not (string-match-p "^\\*\\*\\*" line))) \
                  (unless has-issues \
                    (setq has-issues t) \
                    (setq errors (1+ errors)) \
                    (princ (format "\n--- checkdoc issues in %s ---\n" file))) \
                  (princ (format "  %s\n" (string-trim line))))) \
              (forward-line 1)))))) \
    (when (> errors 0) \
      (message "checkdoc failed with issues across %d file(s)" errors) \
      (kill-emacs 1))))
endef
export CHECKDOC_ELISP

define PACKAGE_LINT_ELISP
(progn \
  (require 'package-lint) \
  (let ((errors 0)) \
    (dolist (file command-line-args-left) \
      (with-current-buffer (find-file-noselect file) \
        (setq-local package-lint-main-file (expand-file-name file default-directory)) \
        (let ((lint-errors (package-lint-buffer))) \
          (when lint-errors \
            (message "\n--- package-lint issues in %s ---" file) \
            (dolist (err lint-errors) \
              (message "  Line %d: %s" (car err) (nth 3 err))) \
            (setq errors (+ errors (length lint-errors))))))) \
    (when (> errors 0) \
      (message "package-lint failed with %d issue(s)" errors) \
      (kill-emacs 1))))
endef
export PACKAGE_LINT_ELISP

all: lint

format:
	@echo "==> Auto-formatting macher-agent-vfs source files..."
	$(EMACS) -Q --batch \
		-L . \
		--eval "$$FORMAT_ELISP" \
		$(EL_FILES)

fmt: format

compile:
	@echo "==> Byte-compiling macher-agent-vfs source files..."
	$(EMACS) -Q --batch \
		-L . \
		--eval "$$SETUP_DEPS" \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(EL_FILES)

checkdoc:
	@echo "==> Running checkdoc on macher-agent-vfs..."
	$(EMACS) -Q --batch \
		-L . \
		--eval "$$SETUP_DEPS" \
		--eval "$$CHECKDOC_ELISP" \
		$(EL_FILES)

package-lint:
	@echo "==> Running package-lint on macher-agent-vfs..."
	$(EMACS) -Q --batch \
		-L . \
		--eval "$$SETUP_DEPS" \
		--eval "$$PACKAGE_LINT_ELISP" \
		$(EL_FILES)

test:
	@echo "==> Running standalone Buttercup tests for macher-agent-vfs..."
	$(EMACS) -Q --batch \
		-L . \
		-L test \
		-L skills/scripts \
		-L ../../macher \
		-L ../macher-agent \
		-L ../macher-agent/test \
		-L ../macher-agent/skills/scripts \
		-L ../macher-agent-sandbox \
		-L ../macher-agent-sandbox/skills/scripts \
		--eval "$$SETUP_DEPS" \
		--eval "(progn (push (expand-file-name \"../macher-agent/skills/scripts\" default-directory) load-path) (push (expand-file-name \"../macher-agent/test\" default-directory) load-path) (push (expand-file-name \"../macher-agent\" default-directory) load-path) (push (expand-file-name \"../macher-agent-sandbox/skills/scripts\" default-directory) load-path) (push (expand-file-name \"../macher-agent-sandbox\" default-directory) load-path) (push (expand-file-name \"skills/scripts\" default-directory) load-path) (push (expand-file-name \"test\" default-directory) load-path) (push (expand-file-name default-directory) load-path))" \
		--eval "(unless (require 'buttercup nil t) (unless package-archive-contents (package-refresh-contents)) (package-install 'buttercup) (require 'buttercup))" \
		-l test/macher-agent-vfs-test-setup.el \
		-f buttercup-run-discover test

lint: compile checkdoc package-lint

clean:
	@echo "==> Removing generated .elc files in macher-agent-vfs..."
	find . -name "*.elc" -delete
