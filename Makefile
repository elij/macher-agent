.PHONY: all compile checkdoc package-lint lint format fmt clean

EMACS ?= emacs

# Find source files while ignoring hidden files, tests, package descriptors, and generated autoloads
EL_FILES := $(shell find . -maxdepth 2 -name "*.el" \
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
      (let ((diag-buf (get-buffer-create checkdoc-diagnostic-buffer))) \
        (with-current-buffer diag-buf (erase-buffer))) \
      (with-current-buffer (find-file-noselect file) \
        (let ((checkdoc-autofix-flag nil)) \
          (checkdoc-current-buffer t) \
          (when-let* ((b (get-buffer checkdoc-diagnostic-buffer))) \
            (with-current-buffer b \
              (goto-char (point-min)) \
              (let ((file-errors nil)) \
                (while (not (eobp)) \
                  (let ((line (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))) \
                    (when (and (not (string-empty-p line)) \
                               (not (string-prefix-p "*** " line))) \
                      (push line file-errors))) \
                  (forward-line 1)) \
                (when file-errors \
                  (setq errors (1+ errors)) \
                  (message "\n--- checkdoc issues in %s ---" file) \
                  (dolist (err (nreverse file-errors)) \
                    (message "  %s" err))))))))) \
    (when (> errors 0) \
      (error "checkdoc failed with issues across %d file(s)" errors))))
endef
export CHECKDOC_ELISP

# Elisp snippet to run package-lint
define PACKAGE_LINT_ELISP
(progn \
  (require 'package-lint) \
  (let ((errors 0)) \
    (dolist (file command-line-args-left) \
      (let ((lint-errors (package-lint-file file))) \
        (when lint-errors \
          (message "package-lint issues in %s:" file) \
          (dolist (err lint-errors) \
            (message "  Line %d: %s" (car err) (nth 2 err))) \
          (setq errors (+ errors (length lint-errors)))))) \
    (when (> errors 0) \
      (error "package-lint failed"))))
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
