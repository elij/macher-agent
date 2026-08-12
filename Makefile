.PHONY: all compile checkdoc package-lint lint format fmt clean

EMACS ?= emacs

# Find source files while ignoring hidden files, tests, package descriptors, and generated autoloads
EL_FILES := $(shell find . -maxdepth 2 -name "*.el" \
	! -path "./bench/*" \
	! -name "*test.el" \
	! -name ".*" \
	! -name "*-pkg.el" \
	! -name "*-autoloads.el")

# Elisp snippet to initialise package.el and install required dependencies safely
define SETUP_DEPS
(progn \
  (require 'package) \
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")) \
  (package-initialize) \
  (dolist (pkg '(gptel macher package-lint)) \
    (unless (package-installed-p pkg) \
      (unless package-archive-contents (package-refresh-contents)) \
      (package-install pkg))))
endef
export SETUP_DEPS

# Elisp snippet to auto-format files
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

# Elisp snippet to run checkdoc and print every issue line cleanly (FILE:LINE: MSG)
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

# Elisp snippet to run package-lint
define PACKAGE_LINT_ELISP
(progn \
  (require 'package-lint) \
  (let ((errors 0)) \
    (dolist (file command-line-args-left) \
      (with-current-buffer (find-file-noselect file) \
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
	@echo "==> Auto-formatting Elisp source files..."
	$(EMACS) -Q --batch \
		-L . \
		--eval "$$FORMAT_ELISP" \
		$(EL_FILES)

fmt: format

compile:
	@echo "==> Resolving dependencies and byte-compiling source files..."
	$(EMACS) -Q --batch \
		-L . \
		--eval "$$SETUP_DEPS" \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(EL_FILES)

checkdoc:
	@echo "==> Running checkdoc..."
	$(EMACS) -Q --batch \
		-L . \
		--eval "$$SETUP_DEPS" \
		--eval "$$CHECKDOC_ELISP" \
		$(EL_FILES)

package-lint:
	@echo "==> Running package-lint..."
	$(EMACS) -Q --batch \
		-L . \
		--eval "$$SETUP_DEPS" \
		--eval "$$PACKAGE_LINT_ELISP" \
		$(EL_FILES)

lint: compile checkdoc package-lint

clean:
	@echo "==> Removing generated .elc files..."
	find . -name "*.elc" -delete
