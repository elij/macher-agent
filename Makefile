.PHONY: all compile checkdoc package-lint lint test integration-test format fmt clean

EMACS ?= emacs

# All 6 monorepo packages in topological DAG order
PACKAGES := packages/macher-agent \
	packages/macher-agent-wire \
	packages/macher-agent-zero-mem \
	packages/macher-agent-sandbox \
	packages/macher-agent-vfs \
	packages/macher-agent-skills

define INTEGRATION_SETUP
(progn \
  (setq load-prefer-newer t) \
  (require 'cl-lib) \
  (require 'package) \
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")) \
  (let ((local-gptel (file-directory-p (expand-file-name "gptel" default-directory))) \
        (local-macher (file-directory-p (expand-file-name "macher" default-directory))) \
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
             (macher-agent-skills nil) \
             all))) \
    (package-initialize) \
    (setq load-path \
          (delq nil \
                (mapcar (lambda (dir) \
                          (unless (and (stringp dir) \
                                       (or (string-match-p "/elpa/macher-agent\\(?:-\\(?:wire\\|vfs\\|sandbox\\|zero-mem\\|skills\\)\\)?-[0-9]" dir) \
                                           (and local-gptel \
                                                (string-match-p "/elpa/gptel-[0-9]" dir)) \
                                           (and local-macher \
                                                (string-match-p "/elpa/macher-[0-9]" dir)))) \
                            dir)) \
                        load-path))) \
    (dolist (pkg '(gptel macher buttercup)) \
      (unless (or (package-installed-p pkg) \
                  (and (eq pkg 'gptel) local-gptel) \
                  (and (eq pkg 'macher) local-macher)) \
        (unless package-archive-contents (package-refresh-contents)) \
        (package-install pkg))) \
    (when local-gptel \
      (let ((gdir (expand-file-name "gptel" default-directory))) \
        (push gdir load-path) \
        (funcall register-local-pkg (expand-file-name "gptel.el" gdir)))) \
    (when local-macher \
      (let ((mdir (expand-file-name "macher" default-directory))) \
        (push mdir load-path) \
        (funcall register-local-pkg (expand-file-name "macher.el" mdir)))) \
    (dolist (spec '(("packages/macher-agent" . "macher-agent.el") \
                    ("packages/macher-agent-wire" . "macher-agent-wire.el") \
                    ("packages/macher-agent-zero-mem" . "macher-agent-zero-mem.el") \
                    ("packages/macher-agent-sandbox" . "macher-agent-sandbox.el") \
                    ("packages/macher-agent-vfs" . "macher-agent-vfs.el") \
                    ("packages/macher-agent-skills" . "macher-agent-skills.el"))) \
      (let ((pdir (expand-file-name (car spec) default-directory))) \
        (when (file-directory-p pdir) \
          (push pdir load-path) \
          (funcall register-local-pkg (expand-file-name (cdr spec) pdir))))) \
    (let ((core-test (expand-file-name "packages/macher-agent/test" default-directory)) \
          (int-test (expand-file-name "test" default-directory))) \
      (when (file-directory-p core-test) \
        (push core-test load-path)) \
      (when (file-directory-p int-test) \
        (push int-test load-path)) \
    (push (expand-file-name default-directory) load-path))) \
  (require 'gptel) \
  (require 'gptel-openai nil t) \
  (require 'gptel-anthropic nil t) \
  (require 'gptel-gemini nil t) \
  (require 'buttercup))
endef
export INTEGRATION_SETUP

all: lint

format:
	@for pkg in $(PACKAGES); do \
		echo "==> [$$pkg] format"; \
		$(MAKE) -C $$pkg format || exit 1; \
	done

fmt: format

compile:
	@for pkg in $(PACKAGES); do \
		echo "==> [$$pkg] compile"; \
		$(MAKE) -C $$pkg compile || exit 1; \
	done

checkdoc:
	@for pkg in $(PACKAGES); do \
		echo "==> [$$pkg] checkdoc"; \
		$(MAKE) -C $$pkg checkdoc || exit 1; \
	done

package-lint:
	@for pkg in $(PACKAGES); do \
		echo "==> [$$pkg] package-lint"; \
		$(MAKE) -C $$pkg package-lint || exit 1; \
	done

lint:
	@for pkg in $(PACKAGES); do \
		echo "==> [$$pkg] lint"; \
		$(MAKE) -C $$pkg lint || exit 1; \
	done

integration-test:
	@echo "==> Running 6-package monorepo integration test suite..."
	$(EMACS) -Q --batch \
		-L . \
		--eval "$$INTEGRATION_SETUP" \
		-f buttercup-run-discover test

test:
	@for pkg in $(PACKAGES); do \
		echo "==> [$$pkg] test"; \
		$(MAKE) -C $$pkg test || exit 1; \
	done
	@$(MAKE) integration-test

clean:
	@for pkg in $(PACKAGES); do \
		echo "==> [$$pkg] clean"; \
		$(MAKE) -C $$pkg clean || exit 1; \
	done
	@if [ -d test ]; then find test -name "*.elc" -delete; fi
