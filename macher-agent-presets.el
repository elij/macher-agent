;;; macher-agent-presets.el --- Skill facade for directives -*- lexical-binding: t; -*-

;;; Commentary:

;; Skill facade for directives and presets in Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'macher-agent-core)

(defcustom macher-agent-skill-directories nil
  "Specify user-defined directories to scan for skill definition files.

This variable holds a list of directory path strings that are scanned for
SKILL.md files during skill initialisation.

Return a list of directory paths or nil if unset.

Side effects: None."
  :type '(repeat string)
  :group 'macher-agent)

(defvar macher-agent--bundled-skills-dir
  (expand-file-name "skills" (file-name-directory (or load-file-name buffer-file-name)))
  "Store internal directory path to bundled skills.

This variable holds the absolute path string to the package's bundled
skills directory.

Return the absolute file path string.

Side effects: None.")

;;

(defun macher-agent--parse-frontmatter-list-item (line)
  "Extract list item string from LINE when matching frontmatter syntax.

LINE is the text line string to inspect.

Return the trimmed item string, or nil if LINE is not a list item.

Side effects: None."
  (when (string-match "^[ \t]*-[ \t]+\"?\\([^\"]+\\)\"?" line)
    (string-trim (match-string 1 line))))

(defun macher-agent--parse-frontmatter-kv-pair (key raw-val ht)
  "Parse frontmatter bounded by non-empty RAW-VAL strings."
  (cl-check-type raw-val string)
  (cl-assert (not (string-empty-p raw-val)))
  (cond
   ((string-match "^\\[\\(.*\\)\\]$" raw-val)
    (let* ((inner (match-string 1 raw-val))
           (items (split-string inner "[, \t\n\r\"]+" t)))
      (puthash key items ht)
      nil))
   (t
    (let ((clean-val (replace-regexp-in-string "\\`['\"]\\|['\"]\\'" "" raw-val)))
      (cond
       ((member (downcase clean-val) '("true" "yes" "1")) (puthash key t ht))
       ((member (downcase clean-val) '("false" "no" "0")) (puthash key nil ht))
       (t (puthash key clean-val ht))))
    nil)))

(defun macher-agent--parse-frontmatter (text)
  "Parse a YAML frontmatter TEXT block into a hash table.

TEXT is the YAML frontmatter string block to parse.

Return a hash table mapping property keys to values.

Side effects: None."
  (let ((ht (make-hash-table :test 'equal))
        (lines (split-string text "\n"))
        (current-list-key nil)
        (current-list-val nil))
    (dolist (line lines)
      (let ((item (and current-list-key (macher-agent--parse-frontmatter-list-item line))))
        (if item
            (push item current-list-val)
          (when (string-match "^\\([A-Za-z0-9_-]+\\):[ \t]*\\(.*\\)$" line)
            (when current-list-key
              (puthash current-list-key (nreverse current-list-val) ht)
              (setq current-list-key nil
                    current-list-val nil))
            (let* ((key (match-string 1 line))
                   (raw-val (string-trim (match-string 2 line))))
              (if (string-empty-p raw-val)
                  (setq current-list-key key
                        current-list-val nil)
                (macher-agent--parse-frontmatter-kv-pair key raw-val ht)))))))

    (when current-list-key
      (puthash current-list-key (nreverse current-list-val) ht))
    ht))

(defun macher-agent-parse-skill-file (filepath context)
  "Parse explicitly mapped FILEPATH string strictly bound to CONTEXT struct."
  (cl-check-type filepath string)
  (cl-check-type context macher-agent-context)
  (unless (file-exists-p filepath)
    (error "Skill file not found: %s" filepath))
  (let* ((workspace-root (macher-agent-context-project-root context))
         (abs-file (expand-file-name filepath))
         (rel-to-workspace (when (and workspace-root abs-file)
                             (file-relative-name abs-file workspace-root)))
         (visiting-buf (find-buffer-visiting abs-file))
         (visiting-buf-name (when visiting-buf (buffer-name visiting-buf)))
         (candidates (delq nil (list filepath abs-file rel-to-workspace visiting-buf-name)))
         (vfs-content (when (fboundp 'macher-agent--resolve-skill-vfs-content)
                        (macher-agent--resolve-skill-vfs-content candidates context))))
    (with-temp-buffer
      (let ((org-inhibit-startup t))
        (setq default-directory (file-name-directory abs-file))
        (if vfs-content
            (insert vfs-content)
          (insert-file-contents abs-file))
        (goto-char (point-min))
        (org-mode)
        (when (fboundp 'org-macro-initialize-templates)
          (org-macro-initialize-templates))
        (when (fboundp 'org-macro-replace-all)
          (let ((templates (bound-and-true-p org-macro-templates)))
            (when (boundp 'macher-agent-skill-macros)
              (setq templates (append macher-agent-skill-macros templates)))
            (org-macro-replace-all templates)))
        (setq-local buffer-file-name abs-file)
        (unwind-protect
            (macher-agent--extract-skill-frontmatter-and-body)
          (set-buffer-modified-p nil))))))

(defun macher-agent--resolve-skill-vfs-content (candidates context)
  "Resolve virtual file system content for CANDIDATES in CONTEXT.

CANDIDATES is a list of candidate path strings.
CONTEXT is the active context structure.

Return the resolved content string, or nil if unresolved.

Side effects: None."
  (cl-check-type context macher-agent-context)
  (when context
    (or
     (let ((contents (macher-agent--get-context-contents context)))
       (cl-loop for cand in candidates
                thereis (when-let* ((entry (cl-find cand contents :key #'macher-agent-vfs-entry-path :test #'equal)))
                          (macher-agent-vfs-entry-curr entry))))
     (when-let* ((vfs-buffers (when (fboundp 'macher-agent-workspace-vfs-buffers)
                                (macher-agent-workspace-vfs-buffers context))))
       (cl-loop for cand in candidates
                thereis (gethash cand vfs-buffers)))
     (cl-loop for cand in candidates
              thereis (when (fboundp 'macher-agent--read-context-file)
                        (condition-case nil
                            (macher-agent--read-context-file context cand)
                          (error nil)))))))

(defun macher-agent--extract-skill-frontmatter-and-body ()
  "Extract frontmatter metadata and body string from the current buffer.

Expects point to be at buffer start.

Return a property list containing parsed skill properties.

Side effects: Moves point and initialises Org macro templates."
  (goto-char (point-min))
  (let ((fm-hash (make-hash-table :test 'equal))
        body name has-tools)

    (when-let* ((_ (re-search-forward "^---\n" nil t))
                (start (point))
                (_ (re-search-forward "^---\n" nil t))
                (frontmatter (buffer-substring-no-properties start
                                                             (match-beginning 0))))
      (setq fm-hash (macher-agent--parse-frontmatter frontmatter)))

    (setq body (string-trim (buffer-substring-no-properties (point) (point-max))))
    (setq name (gethash "name" fm-hash))

    (setq has-tools (not (eq (gethash "allowed-tools" fm-hash 'missing) 'missing)))

    (list :name name
          :name-sym (when name (intern name))
          :description (gethash "description" fm-hash)
          :model (gethash "model" fm-hash)
          :boot-directive (gethash "boot-directive" fm-hash)
          :has-tools has-tools
          :allowed-tools (gethash "allowed-tools" fm-hash)
          :exclusive (gethash "exclusive" fm-hash)
          :ptc-primitives (when-let* ((raw (gethash "ptc-primitives" fm-hash)))
                            (if (listp raw)
                                (mapcar (lambda (p) (if (symbolp p) p (intern p))) raw)
                              raw))
          :body body)))

(defun macher-agent--eval-forms-sequence (forms)
  "Evaluate FORMS sequentially and return evaluation result of last form.

FORMS is a list of Lisp expression forms to evaluate in order.

Return the value returned by the final form in FORMS.

Side effects: Executes Lisp evaluation for each form in FORMS."
  (let ((res nil))
    (dolist (form forms res)
      (setq res (eval form t)))))

(defun macher-agent--parse-and-validate-tool-ast (content tool-name)
  "Parse CONTENT string and validate its AST forms for safe tool execution.

CONTENT is the tool script code string to parse.
TOOL-NAME is the string name of the tool.

Return a list of validated trusted forms, or nil if parsing fails.

Side effects: Emits security warning messages on invalid forms."
  (condition-case err
      (macher-agent--parse-safe-forms content
                                      (lambda (f)
                                        (if (macher-agent--secure-ast-p f)
                                            t
                                          (message "Macher-Agent SECURITY WARNING: \
Skipped tool '%s' because it attempts to evaluate \
top-level definition: %s"
                                                   tool-name (car f))
                                          nil)))
    (error
     (message "Macher-Agent: Failed to parse tool %s - %s" tool-name err)
     nil)))

(defun macher-agent--eval-form-and-extract-tool (form)
  "Evaluate FORM and extract a valid tool structure if produced.

FORM is the Lisp expression form to evaluate.

Return the extracted gptel tool struct, or nil if not produced.

Side effects: Evaluates Lisp form."
  (let ((res (eval form t)))
    (cond
     ((macher-tool-valid-p res) res)
     ((and (symbolp res) (boundp res) (macher-tool-valid-p (symbol-value res)))
      (symbol-value res))
     (t nil))))

(defun macher-agent--evaluate-trusted-tool-ast (forms tool-name)
  "Evaluate trusted FORMS and return the captured tool structure.

FORMS is a list of validated tool Lisp expression forms.
TOOL-NAME is the string name of the tool.

Return the evaluated gptel tool struct, or nil if evaluation fails.

Side effects: Evaluates Lisp expressions."
  (let ((captured-tool nil))
    (condition-case err
        (dolist (form forms)
          (when-let* ((tool (macher-agent--eval-form-and-extract-tool form)))
            (setq captured-tool tool)))
      (error
       (message "Macher-Agent: Failed to evaluate tool %s - %s" tool-name err)
       (setq captured-tool nil)))
    captured-tool))

;;

(defun macher-agent--read-first-existing-file (paths)
  "Read content string from the first existing file path in PATHS.

PATHS is a list of file path strings to check in order.

Return the string content of the first readable file found, or nil.

Side effects: Reads file content from physical disk."
  (catch 'found
    (dolist (path paths)
      (when (and path (file-exists-p path))
        (with-temp-buffer
          (insert-file-contents path)
          (throw 'found (buffer-string)))))
    nil))

(defun macher-agent--is-managed-path-p (path)
  "Verify specific PATH string against managed signatures."
  (cl-check-type path string)
  (or (string-match-p "SKILL\\.md$" path)
      (string-match-p "skills/.*\\.el$" path)
      (string-match-p "scripts/.*\\.el$" path)))

(defun macher-agent--mutation-dispatcher (&rest args)
  "Dispatch context mutation events to appropriate invalidation handlers.

ARGS represents the list of event arguments passed by the hook.

Return nil.

Side effects: Removes cached tools or re-initialises skills on change."
  (let* ((path (cl-find-if #'stringp args))
         (context (or
                   (cl-find-if
                    (lambda (x) (and x (not (stringp x))
                                     (macher-agent-valid-context-p x))) args)
                   (bound-and-true-p macher-agent--persistent-context)))
         (workspace (when context (macher-agent-context-workspace context))))

    (when (and path (string-match "scripts/\\([^/]+\\)\\.el$" path))
      (let* ((tool-name (match-string 1 path))
             (registry (if workspace
                           (macher-agent-workspace-tools-registry context)
                         macher-agent-tools-registry))
             (canonical-name (macher-agent-canonical-tool-name tool-name)))
        (when registry
          (remhash canonical-name registry)
          (remhash tool-name registry)
          (remhash (intern tool-name) registry))))

    (when (and context workspace
               (or (null path)
                   (string-match-p "skills/" path)
                   (string-match-p "SKILL\\.md$" path)))
      (macher-agent-initialize-skills context))

    (when (and path (macher-agent--is-managed-path-p path))
      nil)))

(defun macher-agent--locate-tool-source
    (tool-name context dir-context script-paths _workspace)
  "Locate tool source content for TOOL-NAME across VFS and disk candidates.

TOOL-NAME is the string name of the tool.
CONTEXT is the active context structure.
DIR-CONTEXT is the active directory context string.
SCRIPT-PATHS is a list of candidate script file path strings.
_WORKSPACE is the unused active workspace structure.

Return the tool source content string, or nil if not found.

Side effects: Reads VFS content or physical disk files."
  (if-let* ((context context)
            (vfs-content
             (let* ((vfs-path (when dir-context
                                (expand-file-name (format "scripts/%s.el" tool-name)
                                                  dir-context)))
                    (workspace-root (macher-agent-context-workspace-root context))
                    (rel-to-workspace (when (and workspace-root vfs-path)
                                        (file-relative-name vfs-path workspace-root)))
                    (rel-to-dir (when (and dir-context vfs-path)
                                  (file-relative-name vfs-path dir-context)))
                    (visiting-buf (when vfs-path (find-buffer-visiting vfs-path)))
                    (visiting-buf-name (when visiting-buf (buffer-name visiting-buf)))
                    (std-name (format "scripts/%s.el" tool-name))
                    (base-name (format "%s.el" tool-name))
                    (candidates (delq nil
                                      (list vfs-path rel-to-workspace
                                            rel-to-dir visiting-buf-name std-name base-name)))
                    (found-content nil))
               (cl-loop for cand in candidates
                        until found-content
                        do (setq found-content
                                 (condition-case nil
                                     (macher-agent--read-context-file context cand)
                                   (error nil))))
               found-content)))
      vfs-content
    (macher-agent--read-first-existing-file script-paths)))

(defun macher-agent--load-tool-from-source (canonical-name context dir-context script-paths workspace)
  "Load tool structure from source code given CANONICAL-NAME.

CANONICAL-NAME is the canonical string name of the tool.
CONTEXT is the active context structure.
DIR-CONTEXT is the directory context string.
SCRIPT-PATHS is a list of candidate script path strings.
WORKSPACE is the active workspace structure.

Return the evaluated tool struct, or nil if loading fails.

Side effects: Parses, evaluates, and loads tool code."
  (when canonical-name
    (when-let*
        ((content
          (macher-agent--locate-tool-source canonical-name context dir-context script-paths workspace))
         (forms (macher-agent--parse-and-validate-tool-ast content canonical-name)))
      (macher-agent--evaluate-trusted-tool-ast forms canonical-name))))

(defun macher-agent--resolve-fallback-tool (tool-name canonical-name)
  "Resolve TOOL-NAME strictly as an interned symbol."
  (cl-check-type tool-name symbol)
  (let ((sym-val (when (boundp tool-name) (symbol-value tool-name))))
    (if (and sym-val (macher-tool-valid-p sym-val))
        sym-val
      (or (when (fboundp 'gptel-get-tool)
            (ignore-errors (gptel-get-tool tool-name)))
          (when (and canonical-name (fboundp 'gptel-get-tool))
            (ignore-errors (gptel-get-tool canonical-name)))))))

(defun macher-agent-resolve-tool (tool-name tools-registry &optional dir-context context)
  "Retrieve tool explicitly bound to string TOOL-NAME."
  (cl-check-type tool-name string)
  (let* ((registry
          (or tools-registry
              (when context
                (when-let* ((ws (macher-agent-context-workspace context)))
                  (macher-agent-workspace-tools-registry context)))
              macher-agent-tools-registry))
         (canonical-name (macher-agent-canonical-tool-name tool-name))
         (cached (and canonical-name (gethash canonical-name registry))))
    (if cached
        cached
      (let* ((workspace (when context (macher-agent-context-workspace context)))
             (script-paths
              (when canonical-name
                (delq nil (list
                           (when dir-context
                             (expand-file-name (format "scripts/%s.el" canonical-name)
                                               dir-context))
                           (expand-file-name (format "scripts/%s.el" canonical-name)
                                             (bound-and-true-p macher-agent--bundled-skills-dir))))))
             (loaded-tool
              (or (macher-agent--load-tool-from-source
                   canonical-name context dir-context script-paths workspace)
                  (macher-agent--resolve-fallback-tool (intern tool-name) canonical-name))))
        (when loaded-tool
          (macher-agent--cache-tool loaded-tool registry))
        (or loaded-tool tool-name)))))

(defun macher-agent--load-scripts-from-dir (skills-dir context)
  "Load script tools from the scripts subdirectory of SKILLS-DIR.

SKILLS-DIR is the base skills directory path string.
CONTEXT is the active `macher-agent-context' struct.

Return nil.
Side effects: Loads and caches script tools in workspace or global registry."
  (cl-check-type skills-dir string)
  (cl-check-type context macher-agent-context)
  (let* ((scripts-dir (expand-file-name "scripts" skills-dir))
         (registry (macher-agent-workspace-tools-registry context))
         (script-files (when (file-directory-p scripts-dir)
                         (directory-files scripts-dir t "\\.el$"))))

    (when-let* ((contents (macher-agent--get-context-contents context))
                (ws-root (macher-agent-context-workspace-root context)))
      (dolist (entry contents)
        (when-let* ((path (macher-agent-vfs-entry-path entry))
                    ((stringp path))
                    (abs-path (if (and ws-root (not (file-name-absolute-p path)))
                                  (expand-file-name path ws-root)
                                path))
                    ((string-prefix-p scripts-dir abs-path))
                    ((string-match-p "\\.el$" abs-path)))
          (push abs-path script-files))))

    (dolist (script (delete-dups script-files))
      (ignore (macher-agent-resolve-tool (file-name-base script) registry skills-dir context)))))

;; registrations

(defun macher-agent--tool-category-for-skill (skill-file)
  "Determine tool category string from SKILL-FILE location.

SKILL-FILE is an absolute file path string.

Return the category name string.
Side effects: None."
  (cl-check-type skill-file string)
  (let* ((parent-dir (file-name-directory skill-file))
         (dir-name (file-name-nondirectory (directory-file-name parent-dir))))
    (if (member dir-name '("scripts" "." ""))
        "macher-agent"
      dir-name)))

(defun macher-agent--register-tool-in-gptel-tables (tool category)
  "Register TOOL struct into buffer-local `gptel--known-tools' under CATEGORY.

TOOL is a `gptel-tool' struct.
CATEGORY is a non-empty string identifier.

Return nil.
Side effects: Mutates `gptel--known-tools' association list."
  (cl-check-type category string)
  (when (and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
    (unless (local-variable-p 'gptel--known-tools)
      (make-local-variable 'gptel--known-tools))
    (let* ((cat-name (if (string-empty-p category) "macher-agent" category))
           (tool-name (gptel-tool-name tool)))
      (when (fboundp 'set-gptel-tool-category)
        (unless (gptel-tool-category tool)
          (set-gptel-tool-category tool cat-name)))
      (setf (alist-get tool-name
                       (alist-get cat-name gptel--known-tools nil nil #'equal)
                       nil nil #'equal)
            tool))))

(defun macher-agent--sync-gptel-known-presets (skills-alist)
  "Publish SKILLS-ALIST into buffer-local `gptel--known-presets' and `gptel--known-tools'.

SKILLS-ALIST is an association list of (SYMBOL . PLIST).

Return nil.
Side effects: Mutates `gptel--known-presets', `gptel--known-tools',
and `gptel-directives'."
  (cl-check-type skills-alist list)
  (unless (boundp 'gptel--known-presets)
    (setq gptel--known-presets nil))
  (dolist (entry skills-alist)
    (let* ((sym (car entry))
           (spec (cdr entry))
           (tools (plist-get spec :tools))
           (resolved-tools
            (cl-loop for item in (ensure-list tools)
                     for tool = (cond
                                 ((and (fboundp 'gptel-tool-p) (gptel-tool-p item)) item)
                                 ((stringp item) (or (ignore-errors (gptel-get-tool item)) item))
                                 ((symbolp item) (or (ignore-errors (gptel-get-tool (symbol-name item))) item))
                                 (t item))
                     when tool collect tool)))
      (dolist (tool resolved-tools)
        (when (and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
          (macher-agent--register-tool-in-gptel-tables tool (or (gptel-tool-category tool) "macher"))))
      (let ((preset-plist
             (list :description (plist-get spec :description)
                   :system (plist-get spec :system)
                   :model (plist-get spec :model)
                   :boot-directive (plist-get spec :boot-directive)
                   :ptc-primitives (plist-get spec :ptc-primitives)
                   :exclusive (plist-get spec :exclusive)
                   :tools (when resolved-tools (list :append resolved-tools)))))
        (setf (alist-get sym gptel--known-presets) preset-plist)
        (when-let* ((sys (plist-get spec :system)))
          (setf (alist-get sym gptel-directives) sys)))))
  (when (fboundp 'gptel--setup-directive-menu)
    (gptel--setup-directive-menu 'gptel-system-prompt "Agent Profile")))

(defun macher-agent--register-parsed-skill (parsed skill-file context)
  "Register PARSED skill metadata from SKILL-FILE directly into CONTEXT.

PARSED is the property list of skill attributes.
SKILL-FILE is the file path string of the skill definition.
CONTEXT is the active `macher-agent-context' struct.

Return nil.
Side effects: Updates skill association list and resolves skill tools."
  (cl-check-type parsed list)
  (cl-check-type skill-file string)
  (cl-check-type context macher-agent-context)
  (let ((sym (plist-get parsed :name-sym))
        (body (plist-get parsed :body)))
    (when (and sym body)
      (let* ((desc (plist-get parsed :description))
             (model (plist-get parsed :model))
             (boot-directive (plist-get parsed :boot-directive))
             (tool-names (plist-get parsed :allowed-tools))
             (exclusive (plist-get parsed :exclusive))
             (ptc-primitives (plist-get parsed :ptc-primitives))
             (has-tools (plist-get parsed :has-tools))
             (skill-base-dir (file-name-directory skill-file))
             (registry (macher-agent-workspace-tools-registry context))
             (resolved-tools
              (when tool-names
                (delq nil (mapcar (lambda (tname)
                                    (macher-agent-resolve-tool tname registry skill-base-dir context))
                                  tool-names))))
             (alist (macher-agent-context-skills context)))
        (setf (alist-get sym alist)
              (list :system body
                    :description desc
                    :model (when model (intern model))
                    :boot-directive boot-directive
                    :has-tools has-tools
                    :tools resolved-tools
                    :exclusive exclusive
                    :ptc-primitives ptc-primitives))
        (setf (macher-agent-context-skills context) alist)))))

(defun macher-agent--load-skill-from-path (path context)
  "Load a skill definition from PATH into CONTEXT and register its metadata.

PATH is the skill directory or SKILL.md file path string.
CONTEXT is the active `macher-agent-context' struct.

Return nil.
Side effects: Reads skill file and updates skills association list."
  (cl-check-type path string)
  (cl-check-type context macher-agent-context)
  (let ((skill-file (cond
                     ((and (file-directory-p path)
                           (file-exists-p (expand-file-name "SKILL.md" path)))
                      (expand-file-name "SKILL.md" path))
                     ((and (file-regular-p path)
                           (string-match-p "\\.md$" path))
                      path)
                     (t nil))))
    (when skill-file
      (let ((parsed (macher-agent-parse-skill-file skill-file context)))
        (when parsed
          (macher-agent--register-parsed-skill parsed skill-file context))))))

(defun macher-agent--try-load-skill-from-path (path context)
  "Attempt to load skill from PATH catching and logging any errors.

PATH is the skill directory or file path string.
CONTEXT is the active `macher-agent-context' struct.

Return nil.
Side effects: Loads skill metadata or emits warning message on error."
  (cl-check-type path string)
  (cl-check-type context macher-agent-context)
  (condition-case err
      (macher-agent--load-skill-from-path path context)
    (error
     (message "Error loading path %s: %S" path err))))

(defun macher-agent-api-register-skills-in-directory (skills-dir context)
  "Scan SKILLS-DIR directory for skill definition files and load them into CONTEXT.

SKILLS-DIR is the directory path string to scan.
CONTEXT is the active `macher-agent-context' struct.

Return nil.
Side effects: Loads script files and registers skill metadata."
  (cl-check-type skills-dir string)
  (cl-check-type context macher-agent-context)
  (let* ((expanded-dir (file-name-as-directory (expand-file-name skills-dir)))
         (skills-subdir (file-name-as-directory (expand-file-name "skills" expanded-dir)))
         (target-dir (if (file-directory-p skills-subdir) skills-subdir expanded-dir)))
    (when (file-directory-p target-dir)
      (macher-agent--load-scripts-from-dir target-dir context)
      (let ((candidate-files (directory-files target-dir t macher-agent--file-scan-regex)))
        (when-let* ((contents (macher-agent--get-context-contents context))
                    (ws-root (macher-agent-context-workspace-root context)))
          (dolist (entry contents)
            (when-let* ((path (macher-agent-vfs-entry-path entry))
                        ((stringp path))
                        (abs-path (if (and ws-root (not (file-name-absolute-p path)))
                                      (expand-file-name path ws-root)
                                    path))
                        ((string-prefix-p target-dir abs-path))
                        ((string-match-p "SKILL\\.md$" abs-path)))
              (push abs-path candidate-files))))
        (dolist (path (delete-dups candidate-files))
          (macher-agent--try-load-skill-from-path path context))))))

(defun macher-agent-initialize-skills (&optional context)
  "Load, register, and synchronise skills.

If CONTEXT is provided, it must be a typed `macher-agent-context' struct,
and skills are loaded strictly into that workspace instance.
If CONTEXT is nil, the function acts as a global bootstrapper, loading
skills into a temporary sandbox and promoting them to global defaults."
  (interactive)
  (if (null context)
      (with-temp-buffer
        (unless (local-variable-p 'gptel--known-presets)
          (make-local-variable 'gptel--known-presets))
        (unless (local-variable-p 'gptel--known-tools)
          (make-local-variable 'gptel--known-tools))
        (unless (local-variable-p 'gptel-directives)
          (make-local-variable 'gptel-directives))

        (macher-agent-initialize-skills (macher-agent--make-context))

        (setq-default gptel--known-tools (copy-tree gptel--known-tools))
        (setq-default gptel--known-presets (copy-alist gptel--known-presets))
        (setq-default gptel-directives (copy-alist gptel-directives)))

    (cl-check-type context macher-agent-context)
    (let* ((ws-root (macher-agent-context-workspace-root context))
           (bundled (or (bound-and-true-p macher-agent--bundled-skills-dir)
                        (bound-and-true-p macher-agent-bundled-skills-directory))))
      (when (and bundled (file-directory-p bundled))
        (macher-agent-api-register-skills-in-directory bundled context))
      (when (and ws-root (file-directory-p ws-root))
        (let ((skills-dir (expand-file-name "skills" ws-root))
              (dot-skills-dir (expand-file-name ".skills" ws-root)))
          (if (or (file-directory-p skills-dir) (file-directory-p dot-skills-dir))
              (progn
                (when (file-directory-p skills-dir)
                  (macher-agent-api-register-skills-in-directory skills-dir context))
                (when (file-directory-p dot-skills-dir)
                  (macher-agent-api-register-skills-in-directory dot-skills-dir context)))
            (macher-agent-api-register-skills-in-directory ws-root context))))
      (when (boundp 'macher-agent-skill-directories)
        (dolist (dir macher-agent-skill-directories)
          (when (and dir (file-directory-p dir))
            (macher-agent-api-register-skills-in-directory dir context))))
      (let ((merged (macher-agent-workspace-skills-alist context)))
        (macher-agent--sync-gptel-known-presets merged)))))

(defun macher-agent-refresh-skills-and-tools (context)
  "Reset buffer-local GPTel tables to defaults, then reload skills and tools for CONTEXT.

CONTEXT is a typed `macher-agent-context' struct.

Return nil.
Side effects: Reinitialises buffer-local `gptel--known-tools',
`gptel--known-presets', `gptel-directives', and context skills."
  (cl-check-type context macher-agent-context)
  (unless (local-variable-p 'gptel--known-presets)
    (make-local-variable 'gptel--known-presets))
  (unless (local-variable-p 'gptel--known-tools)
    (make-local-variable 'gptel--known-tools))
  (unless (local-variable-p 'gptel-directives)
    (make-local-variable 'gptel-directives))

  (setq-local gptel--known-tools (copy-tree (default-value 'gptel--known-tools)))
  (setq-local gptel--known-presets (copy-alist (default-value 'gptel--known-presets)))
  (setq-local gptel-directives (copy-alist (default-value 'gptel-directives)))

  (setf (macher-agent-context-skills context) nil)
  (macher-agent-initialize-skills context))

(defun macher-agent--secure-ast-p (form)
  "Verify list FORM contains no unsafe definitions."
  (cl-check-type form list)
  (cond
   ((null form) t)
   ((memq (car form) '(defun cl-defun defvar defcustom defmacro defalias)) nil)
   (t (and (if (listp (car form)) (macher-agent--secure-ast-p (car form)) t)
           (if (listp (cdr form)) (macher-agent--secure-ast-p (cdr form)) t)))))

(defun macher-agent--read-and-validate-form (buf &optional validation-cb)
  "Read a single form from BUF and optionally validate it.

BUF is the buffer to read from.
VALIDATION-CB is an optional predicate function.

Return the validated read Lisp form.

Side effects: Advances point in BUF; signals an error if validation fails."
  (let ((form (read buf)))
    (when (and validation-cb (not (funcall validation-cb form)))
      (error "Validation failed for form: %S" form))
    form))

(defun macher-agent--parse-safe-forms (content &optional validation-cb)
  "Parse CONTENT string into a list of validated Lisp forms.

CONTENT is the code string block to parse.
VALIDATION-CB is an optional predicate function.

Return a list of parsed Lisp forms."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case nil
          (while t
            (push (macher-agent--read-and-validate-form (current-buffer) validation-cb) forms))
        (end-of-file nil))
      (nreverse forms))))

;; tool normalisation

(defun macher-agent--tool-matches-name-p (tool normalised-target)
  "Verify TOOL struct matches NORMALISED-TARGET string."
  (cl-check-type tool gptel-tool)
  (cl-check-type normalised-target string)
  (let ((t-name (macher-agent-canonical-tool-name tool)))
    (equal (replace-regexp-in-string "-" "_" t-name) normalised-target)))

(defun macher-agent--find-tool-in-known-tools (normalised-target)
  "Search gptel known tools for a tool structure matching NORMALISED-TARGET.

NORMALISED-TARGET is target tool name string formatted with underscores.

Return matching gptel tool struct, or nil if not found.

Side effects: None."
  (when (boundp 'gptel--known-tools)
    (cl-loop
     for category-alist in gptel--known-tools
     thereis (cl-loop for (_ . tool) in (cdr category-alist)
                      thereis (when (macher-agent--tool-matches-name-p tool normalised-target)
                                tool)))))

(defun macher-agent--find-native-tool (tool-name)
  "Find native tool strictly using string TOOL-NAME."
  (cl-check-type tool-name string)
  (let ((normalised-target (replace-regexp-in-string "-" "_" tool-name)))
    (or (cl-loop for t_ in (append (default-value 'gptel-tools) (bound-and-true-p gptel-tools))
                 thereis (when (and (macher-tool-valid-p t_)
                                    (macher-agent--tool-matches-name-p t_ normalised-target))
                           t_))
        (macher-agent--find-tool-in-known-tools normalised-target)
        (when (fboundp 'gptel-get-tool)
          (or (ignore-errors (gptel-get-tool tool-name))
              (ignore-errors (gptel-get-tool normalised-target)))))))

(defun macher-agent--select-resolved-tool (resolved item)
  "Select strictly validated RESOLVED struct."
  (cl-check-type resolved gptel-tool)
  (list resolved))

(defun macher-agent--resolve-tool-in-env (item)
  "Resolve tool ITEM using active workspace registry and environment context.

ITEM is the tool identifier string, symbol, or structure to resolve.

Return a list of resolved gptel tool structs, or nil.

Side effects: May load script files from VFS or physical disk."
  (let* ((ctx (bound-and-true-p macher-agent--persistent-context))
         (ws (when ctx (macher-agent-context-workspace ctx)))
         (registry
          (if ws (macher-agent-workspace-tools-registry ctx) macher-agent-tools-registry))
         (skills-dir (when ctx (macher-agent-context-root ctx)))
         (resolved (macher-agent-resolve-tool item registry skills-dir ctx)))
    (macher-agent--select-resolved-tool resolved item)))

(cl-defgeneric macher-agent-resolve-item (item)
  "Resolve tool representation ITEM into a list of gptel tool structs.

ITEM is the tool symbol, string, structure, or list to resolve.

Return a list of resolved gptel tool structures, or nil.

Side effects: May resolve tools from environment registries or evaluation.")

(cl-defmethod macher-agent-resolve-item ((item cl-structure-object))
  "Resolve tool ITEM when it is a structure object.

ITEM is the structure object to resolve.

Return a list containing ITEM if valid, or nil.

Side effects: None."
  (when (macher-tool-valid-p item) (list item)))

(cl-defmethod macher-agent-resolve-item ((item record))
  "Resolve tool ITEM when it is a record object.

ITEM is the record to resolve.

Return a list containing ITEM if valid, or nil.

Side effects: None."
  (when (macher-tool-valid-p item) (list item)))

(cl-defmethod macher-agent-resolve-item ((item string))
  "Resolve tool ITEM when it is a string name.

ITEM is the string tool name to resolve.

Return a list of resolved gptel tool structs, or nil.

Side effects: May read tool scripts from disk or VFS."
  (macher-agent--resolve-tool-in-env item))

(cl-defmethod macher-agent-resolve-item ((item symbol))
  "Resolve tool ITEM when it is a symbol.

ITEM is the symbol tool identifier or function to resolve.

Return a list of resolved gptel tool structs, or nil.

Side effects: May invoke function bound to ITEM or resolve from context."
  (if (fboundp item)
      (let ((res (funcall item)))
        (if res
            (macher-agent-resolve-item res)
          (macher-agent--resolve-tool-in-env item)))
    (macher-agent--resolve-tool-in-env item)))

(cl-defmethod macher-agent-resolve-item ((item list))
  "Resolve tool ITEM when it is a list of tool representations.

ITEM is the list of tool representations to resolve.

Return a flattened list of resolved gptel tool structs, or nil.

Side effects: Resolves each element in ITEM sequentially."
  (cond
   ((null item) nil)
   ((and (consp item) (keywordp (car item)))
    (when (or (plist-member item :function)
              (plist-member item :name)
              (plist-member item :args))
      (list (apply #'gptel-make-tool item))))
   ((and (eq (car item) 'quote) (cdr item) (null (cddr item)))
    (macher-agent-resolve-item (cadr item)))
   (t
    (cl-mapcan #'macher-agent-resolve-item item))))

(cl-defmethod macher-agent-resolve-item ((item t))
  "Fallback method for resolving tool ITEM."
  (cond
   ((macher-tool-valid-p item)
    (list item))
   (t nil)))

(defun macher-agent--deduplicate-tool (tool seen)
  "Deduplicate explicitly validated TOOL struct."
  (cl-check-type tool gptel-tool)
  (let* ((raw-name (macher-agent-canonical-tool-name tool))
         (canon-name (replace-regexp-in-string "-" "_" (substring-no-properties (format "%s" raw-name)))))
    (when (not (gethash canon-name seen))
      (puthash canon-name t seen)
      tool)))

(defun macher-agent-normalize-tools (tools)
  "Normalise, resolve, and deduplicate a mixed list of TOOLS.

TOOLS is a list of tool representations including names, symbols, structs,
functions, or nested lists.

Return a list of unique resolved gptel tool structures."
  (when tools
    (let ((flat-resolved (delq nil (macher-agent-resolve-item tools)))
          (seen (make-hash-table :test 'equal))
          (unique nil))
      (dolist (tool flat-resolved)
        (when-let* ((deduped (macher-agent--deduplicate-tool tool seen)))
          (push deduped unique)))
      (nreverse unique))))

(defun macher-agent-resolve-to-struct (t-item)
  "Convert tool item T-ITEM into a single gptel tool structure.

T-ITEM is the tool representation to convert.

Return the first resolved gptel tool structure, or nil if unresolved.

Side effects: Resolves tools via environment context."
  (car (macher-agent-normalize-tools t-item)))

(defun macher-agent--cache-tool (tool registry)
  "Cache explicitly validated TOOL struct into REGISTRY."
  (cl-check-type tool gptel-tool)
  (let ((canonical-name (macher-agent-canonical-tool-name tool)))
    (puthash canonical-name tool registry)))

(provide 'macher-agent-presets)
;;; macher-agent-presets.el ends here
