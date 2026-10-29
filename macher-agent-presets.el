;;; macher-agent-presets.el --- Skill facade for directives -*- lexical-binding: t; -*-

;;; Commentary:
;; Skill facade for directives and presets in Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'org)
(eval-and-compile
  (let* ((root (file-name-directory (or load-file-name buffer-file-name default-directory)))
         (gptel-dir (expand-file-name "gptel" root)))
    (when (file-directory-p gptel-dir)
      (add-to-list 'load-path gptel-dir))))
(require 'gptel)
(require 'macher-agent-core)

(declare-function set-gptel-tool-category "gptel" (tool category))
(declare-function gptel--setup-directive-menu "gptel-transient" (sym &optional prompt))

(defvar gptel--known-presets)
(defvar gptel--known-tools)
(defvar gptel-directives)
(defvar gptel-system-prompt)
(defvar gptel-model)
(defvar gptel-backend)
(defvar gptel-temperature)
(defvar gptel-max-tokens)
(defvar gptel-tools)

(defcustom macher-agent-skill-directories nil
  "Specify user-defined directories to scan for skill definition files."
  :type '(repeat string)
  :group 'macher-agent)

(defvar-local macher-agent--cached-boot-directive nil
  "Cached boot directive for use_skill restoration.")
(put 'macher-agent--cached-boot-directive 'permanent-local t)

(defvar-local macher-agent--cached-boot-directive-set nil
  "Flag indicating whether `macher-agent--cached-boot-directive' was saved.")
(put 'macher-agent--cached-boot-directive-set 'permanent-local t)

(defvar macher-agent--bundled-skills-dir
  (expand-file-name "skills" (file-name-directory (or load-file-name buffer-file-name)))
  "Store internal directory path to bundled skills.")

(defun macher-agent--local-tools-table (context)
  "Return the persistent local tools hash table for CONTEXT, creating it if needed."
  (cl-check-type context macher-agent-context)
  (let ((table (macher-agent-context-tools context)))
    (if (hash-table-p table)
        table
      (let ((new-table (make-hash-table :test 'equal)))
        (setf (macher-agent-context-tools context) new-table)
        new-table))))

(defun macher-agent--parse-frontmatter-list-item (line)
  "Extract list item string from LINE when matching frontmatter syntax."
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
  "Parse a YAML frontmatter TEXT block into a hash table."
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

(defun macher-agent--resolve-skill-vfs-content (candidates context)
  "Resolve workspace storage content for CANDIDATES in CONTEXT."
  (cl-check-type context macher-agent-context)
  (when (functionp macher-agent-workspace-read-file-function)
    (or (cl-loop for cand in candidates
                 thereis (funcall macher-agent-workspace-read-file-function context cand t))
        (cl-loop for cand in candidates
                 thereis (funcall macher-agent-workspace-read-file-function context cand nil)))))

(defun macher-agent--extract-skill-frontmatter-and-body ()
  "Extract frontmatter metadata and body string from the current buffer."
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
          :system body)))

(defun macher-agent-parse-skill-file (filepath context)
  "Parse FILEPATH string strictly bound to CONTEXT struct."
  (cl-check-type filepath string)
  (cl-check-type context macher-agent-context)
  (let* ((workspace-root (macher-agent-context-project-root context))
         (abs-file (expand-file-name filepath))
         (rel-to-workspace (when (and workspace-root abs-file)
                             (file-relative-name abs-file workspace-root)))
         (visiting-buf (find-buffer-visiting abs-file))
         (visiting-buf-name (when visiting-buf (buffer-name visiting-buf)))
         (candidates (delq nil (list filepath abs-file rel-to-workspace visiting-buf-name)))
         (vfs-content (macher-agent--resolve-skill-vfs-content candidates context)))
    (unless (or (file-exists-p filepath) vfs-content)
      (error "Skill file not found on disk or in VFS: %s" filepath))
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

(defun macher-agent--secure-ast-p (form)
  "Verify list FORM contains no unsafe definitions."
  (cl-check-type form list)
  (cond
   ((null form) t)
   ((memq (car form) '(defun cl-defun defvar defcustom defmacro defalias fset eval advice-add setq-default)) nil)
   (t (and (if (listp (car form)) (macher-agent--secure-ast-p (car form)) t)
           (if (listp (cdr form)) (macher-agent--secure-ast-p (cdr form)) t)))))

(defun macher-agent--read-and-validate-form (buf &optional validation-cb)
  "Read a single form from BUF and optionally validate it."
  (let ((form (read buf)))
    (when (and validation-cb (not (funcall validation-cb form)))
      (error "Validation failed for form: %S" form))
    form))

(defun macher-agent--parse-safe-forms (content &optional validation-cb)
  "Parse CONTENT string into a list of validated Lisp forms."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case nil
          (while t
            (push (macher-agent--read-and-validate-form (current-buffer) validation-cb) forms))
        (end-of-file nil))
      (nreverse forms))))

(defun macher-agent--parse-and-validate-tool-ast (content tool-name)
  "Parse CONTENT string and validate its AST forms for safe tool execution."
  (condition-case err
      (macher-agent--parse-safe-forms content
                                      (lambda (f)
                                        (if (macher-agent--secure-ast-p f)
                                            t
                                          (message "Macher-Agent SECURITY WARNING: Skipped tool '%s' because it attempts to evaluate top-level definition: %s"
                                                   tool-name (car f))
                                          nil)))
    (error
     (message "Macher-Agent: Failed to parse tool %s - %s" tool-name err)
     nil)))

(defun macher-agent--eval-form-and-extract-tool (form)
  "Evaluate FORM and extract a valid tool structure if produced."
  (let ((res (eval form t)))
    (cond
     ((macher-tool-valid-p res) res)
     ((and (symbolp res) (boundp res) (macher-tool-valid-p (symbol-value res)))
      (symbol-value res))
     (t nil))))

(defun macher-agent--evaluate-trusted-tool-ast (forms tool-name)
  "Evaluate trusted FORMS and return the captured tool structure."
  (let ((captured-tool nil))
    (condition-case err
        (dolist (form forms)
          (when-let* ((tool (macher-agent--eval-form-and-extract-tool form)))
            (setq captured-tool tool)))
      (error
       (message "Macher-Agent: Failed to evaluate tool %s - %s" tool-name err)
       (setq captured-tool nil)))
    captured-tool))

(defun macher-agent--read-first-existing-file (paths)
  "Read content string from the first existing file path in PATHS."
  (catch 'found
    (dolist (path paths)
      (when (and path (file-exists-p path))
        (with-temp-buffer
          (insert-file-contents path)
          (throw 'found (buffer-string)))))
    nil))

(defun macher-agent--read-vfs-or-disk (context path)
  "Read content for PATH from CONTEXT workspace or disk, or nil if absent."
  (or (when (and (functionp macher-agent-workspace-read-file-function) context path)
        (funcall macher-agent-workspace-read-file-function context path))
      (when (and path (file-exists-p path) (not (file-directory-p path)))
        (ignore-errors
          (with-temp-buffer
            (insert-file-contents path)
            (buffer-string))))))

(defun macher-agent--skill-name-from-path (path)
  "Extract skill or command symbol from PATH for deletion."
  (when (stringp path)
    (let ((nondir (file-name-nondirectory path)))
      (cond
       ((string-equal nondir "SKILL.md")
        (let ((parent-dir (file-name-nondirectory (directory-file-name (file-name-directory path)))))
          (unless (string-empty-p parent-dir)
            (intern parent-dir))))
       ((and (string-match-p "\\(?:\\`\\|/\\)commands/" path)
             (string-match-p "\\.md\\'" path))
        (intern (file-name-sans-extension nondir)))
       ((and (string-match-p "\\(?:\\`\\|/\\)\\.?skills/" path)
             (string-match-p "\\.md\\'" path))
        (intern (file-name-sans-extension nondir)))
       (t nil)))))

(defun macher-agent--is-managed-path-p (path)
  "Verify specific PATH string against managed skill, command, or script signatures."
  (and (stringp path)
       (or (string-match-p "SKILL\\.md$" path)
           (string-match-p "/skills/" path)
           (string-match-p "/\\.skills/" path)
           (string-match-p "/commands/" path)
           (string-match-p "\\(?:\\`\\|/\\)scripts/\\([^/]+\\)\\.el\\'" path))))

(defun macher-agent--mutation-dispatcher (&rest args)
  "Dispatch context mutation events to workspace-local invalidation handlers."
  (let* ((path (cl-find-if #'stringp args))
         (context (or (cl-find-if #'macher-agent-valid-context-p args)
                      (macher-agent-resolve-context))))
    (when (and context (or (null path) (macher-agent--is-managed-path-p path)))
      (when (and path (string-match "\\(?:\\`\\|/\\)scripts/\\([^/]+\\)\\.el\\'" path))
        (let* ((tool-name (match-string 1 path))
               (canonical-name (macher-agent-canonical-tool-name tool-name))
               (local-reg (macher-agent--local-tools-table context)))
          (remhash canonical-name local-reg)
          (remhash tool-name local-reg)
          (remhash (intern tool-name) local-reg)))
      (when (and path (null (macher-agent--read-vfs-or-disk context path)))
        (when-let* ((sym (macher-agent--skill-name-from-path path)))
          (setf (macher-agent-context-skills context)
                (assq-delete-all sym (macher-agent-context-skills context)))
          (when (boundp 'gptel--known-presets)
            (setq gptel--known-presets (assq-delete-all sym gptel--known-presets)))
          (when (boundp 'gptel-directives)
            (setq gptel-directives (assq-delete-all sym gptel-directives)))))
      (when (or (null path)
                (string-match-p "/skills/" path)
                (string-match-p "/\\.skills/" path)
                (string-match-p "/commands/" path)
                (string-match-p "SKILL\\.md$" path))
        (macher-agent-initialize-skills context)))))

(defun macher-agent--locate-tool-source (tool-name context dir-context script-paths _workspace)
  "Locate tool source content for TOOL-NAME across VFS and disk candidates."
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
                                            rel-to-dir visiting-buf-name std-name base-name))))
               (macher-agent--resolve-skill-vfs-content candidates context))))
      vfs-content
    (macher-agent--read-first-existing-file script-paths)))

(defun macher-agent--load-tool-from-source (canonical-name context dir-context script-paths workspace)
  "Load tool structure from source code given CANONICAL-NAME."
  (when canonical-name
    (when-let* ((content (macher-agent--locate-tool-source canonical-name context dir-context script-paths workspace))
                (forms (macher-agent--parse-and-validate-tool-ast content canonical-name)))
      (macher-agent--evaluate-trusted-tool-ast forms canonical-name))))

(defun macher-agent--resolve-fallback-tool (tool-sym canonical-name)
  "Resolve TOOL-SYM or CANONICAL-NAME from loaded variables or gptel tables."
  (cl-check-type tool-sym symbol)
  (let* ((hyphen-name (replace-regexp-in-string "_" "-" (symbol-name tool-sym)))
         (tool-var (intern-soft (format "macher-agent-%s-tool" hyphen-name)))
         (sym-val (cond
                   ((and (boundp tool-sym) (macher-tool-valid-p (symbol-value tool-sym)))
                    (symbol-value tool-sym))
                   ((and tool-var (boundp tool-var) (macher-tool-valid-p (symbol-value tool-var)))
                    (symbol-value tool-var))
                   (t nil))))
    (or sym-val
        (when (fboundp 'gptel-get-tool)
          (or (ignore-errors (gptel-get-tool tool-sym))
              (when canonical-name
                (ignore-errors (gptel-get-tool canonical-name))))))))

(defun macher-agent--cache-tool (tool registry)
  "Cache validated TOOL struct into REGISTRY."
  (cl-check-type tool gptel-tool)
  (when (hash-table-p registry)
    (let ((canonical-name (macher-agent-canonical-tool-name tool)))
      (puthash canonical-name tool registry))))

(defun macher-agent-resolve-tool (tool-name tools-registry &optional dir-context context)
  "Retrieve tool bound to string TOOL-NAME, caching into workspace-local registry."
  (cl-check-type tool-name string)
  (let* ((local-reg (when (macher-agent-valid-context-p context)
                      (macher-agent--local-tools-table context)))
         (lookup-reg (or tools-registry local-reg macher-agent-tools-registry))
         (canonical-name (macher-agent-canonical-tool-name tool-name))
         (cached (and canonical-name
                      (or (and (hash-table-p local-reg) (gethash canonical-name local-reg))
                          (and (hash-table-p lookup-reg) (gethash canonical-name lookup-reg))
                          (and (hash-table-p macher-agent-tools-registry)
                               (gethash canonical-name macher-agent-tools-registry))))))
    (if cached
        cached
      (let* ((workspace (when context (macher-agent-context-workspace context)))
             (script-paths
              (when canonical-name
                (delq nil (list
                           (when dir-context
                             (expand-file-name (format "scripts/%s.el" canonical-name)
                                               dir-context))
                           (when (bound-and-true-p macher-agent--bundled-skills-dir)
                             (expand-file-name (format "scripts/%s.el" canonical-name)
                                               macher-agent--bundled-skills-dir))))))
             (loaded-tool
              (or (macher-agent--load-tool-from-source
                   canonical-name context dir-context script-paths workspace)
                  (macher-agent--resolve-fallback-tool (intern tool-name) canonical-name))))
        (when (macher-tool-valid-p loaded-tool)
          (if (hash-table-p local-reg)
              (macher-agent--cache-tool loaded-tool local-reg)
            (macher-agent--cache-tool loaded-tool (or tools-registry macher-agent-tools-registry))))
        (or loaded-tool tool-name)))))

(defun macher-agent--load-scripts-from-dir (skills-dir context)
  "Load script tools from the scripts subdirectory of SKILLS-DIR into CONTEXT."
  (cl-check-type skills-dir string)
  (cl-check-type context macher-agent-context)
  (let* ((scripts-dir (expand-file-name "scripts" skills-dir))
         (registry (macher-agent--local-tools-table context))
         (script-files (when (file-directory-p scripts-dir)
                         (directory-files scripts-dir t "\\.el$"))))
    (when-let* ((tracked (and (functionp macher-agent-workspace-list-files-function)
                              (funcall macher-agent-workspace-list-files-function context)))
                (ws-root (macher-agent-context-workspace-root context)))
      (dolist (path tracked)
        (when-let* (((stringp path))
                    (abs-path (if (and ws-root (not (file-name-absolute-p path)))
                                  (expand-file-name path ws-root)
                                path))
                    ((string-prefix-p scripts-dir abs-path))
                    ((string-match-p "\\.el$" abs-path)))
          (push abs-path script-files))))
    (dolist (script (delete-dups script-files))
      (ignore (macher-agent-resolve-tool (file-name-base script) registry skills-dir context)))))

(defun macher-agent--register-tool-in-gptel-tables (tool category)
  "Register TOOL struct into buffer-local `gptel--known-tools' under CATEGORY."
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
  "Publish SKILLS-ALIST into buffer-local `gptel--known-presets' and `gptel--known-tools'."
  (cl-check-type skills-alist list)
  (unless (boundp 'gptel--known-presets)
    (setq gptel--known-presets nil))
  (unless (boundp 'gptel-directives)
    (setq gptel-directives nil))
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
      (let* ((sys-text (plist-get spec :system))
             (preset-plist (list :system sys-text)))
        (when-let* ((desc (plist-get spec :description)))
          (setq preset-plist (plist-put preset-plist :description desc)))
        (when-let* ((mod (plist-get spec :model)))
          (setq preset-plist (plist-put preset-plist :model mod)))
        (when-let* ((boot (plist-get spec :boot-directive)))
          (setq preset-plist (plist-put preset-plist :boot-directive boot)))
        (when-let* ((ptc (plist-get spec :ptc-primitives)))
          (setq preset-plist (plist-put preset-plist :ptc-primitives ptc)))
        (when-let* ((excl (plist-get spec :exclusive)))
          (setq preset-plist (plist-put preset-plist :exclusive excl)))
        (when-let* ((cmd (plist-get spec :is-command)))
          (setq preset-plist (plist-put preset-plist :is-command cmd)))
        (when resolved-tools
          (setq preset-plist (plist-put preset-plist :tools (list :append resolved-tools))))
        (setf (alist-get sym gptel--known-presets) preset-plist)
        (when sys-text
          (setf (alist-get sym gptel-directives) sys-text)))))
  (when (fboundp 'gptel--setup-directive-menu)
    (gptel--setup-directive-menu 'gptel-system-prompt "Agent Profile")))

(defun macher-agent--register-parsed-skill (parsed skill-file context)
  "Register PARSED skill metadata from SKILL-FILE directly into CONTEXT."
  (cl-check-type parsed list)
  (cl-check-type skill-file string)
  (cl-check-type context macher-agent-context)
  (when (string-match-p "/commands/" (expand-file-name skill-file))
    (setq parsed (plist-put parsed :is-command t)))
  (let ((sym (plist-get parsed :name-sym))
        (body (plist-get parsed :system)))
    (when (and sym body)
      (let* ((desc (plist-get parsed :description))
             (model (plist-get parsed :model))
             (boot-directive (plist-get parsed :boot-directive))
             (tool-names (plist-get parsed :allowed-tools))
             (exclusive (plist-get parsed :exclusive))
             (ptc-primitives (plist-get parsed :ptc-primitives))
             (has-tools (plist-get parsed :has-tools))
             (is-command (plist-get parsed :is-command))
             (skill-base-dir (file-name-directory skill-file))
             (registry (macher-agent--local-tools-table context))
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
                    :ptc-primitives ptc-primitives
                    :is-command is-command))
        (setf (macher-agent-context-skills context) alist)))))

(defun macher-agent--load-skill-from-path (path context)
  "Load a skill definition from PATH into CONTEXT and register its metadata."
  (cl-check-type path string)
  (cl-check-type context macher-agent-context)
  (let* ((in-vfs (and (functionp macher-agent-workspace-read-file-function)
                      (funcall macher-agent-workspace-read-file-function context path)))
         (skill-file (cond
                      ((and (file-directory-p path)
                            (file-exists-p (expand-file-name "SKILL.md" path)))
                       (expand-file-name "SKILL.md" path))
                      ((and (file-regular-p path)
                            (string-match-p "\\.md$" path))
                       path)
                      ((and in-vfs
                            (string-match-p "\\.md$" path))
                       path)
                      (t nil))))
    (when skill-file
      (let ((parsed (macher-agent-parse-skill-file skill-file context)))
        (when parsed
          (macher-agent--register-parsed-skill parsed skill-file context))))))

(defun macher-agent--try-load-skill-from-path (path context)
  "Attempt to load skill from PATH into CONTEXT catching and logging any errors."
  (cl-check-type path string)
  (cl-check-type context macher-agent-context)
  (condition-case err
      (macher-agent--load-skill-from-path path context)
    (error
     (message "Error loading path %s: %S" path err))))

(defun macher-agent-api-register-skills-in-directory (skills-dir context)
  "Scan SKILLS-DIR directory for skill definition files and load them into CONTEXT."
  (cl-check-type skills-dir string)
  (cl-check-type context macher-agent-context)
  (let* ((expanded-dir (file-name-as-directory (expand-file-name skills-dir)))
         (skills-subdir (file-name-as-directory (expand-file-name "skills" expanded-dir)))
         (target-dir (if (file-directory-p skills-subdir) skills-subdir expanded-dir)))
    (when (file-directory-p target-dir)
      (macher-agent--load-scripts-from-dir target-dir context)
      (let ((candidate-files (directory-files target-dir t macher-agent--file-scan-regex)))
        (when-let* ((tracked (and (functionp macher-agent-workspace-list-files-function)
                                  (funcall macher-agent-workspace-list-files-function context)))
                    (ws-root (macher-agent-context-workspace-root context)))
          (dolist (path tracked)
            (when-let* (((stringp path))
                        (abs-path (if (and ws-root (not (file-name-absolute-p path)))
                                      (expand-file-name path ws-root)
                                    path))
                        ((string-prefix-p target-dir abs-path))
                        ((string-match-p "SKILL\\.md$" abs-path)))
              (push abs-path candidate-files))))
        (dolist (path (delete-dups candidate-files))
          (macher-agent--try-load-skill-from-path path context))))))

(defun macher-agent-initialize-commands (context)
  "Load flat markdown command files from commands directory into CONTEXT."
  (cl-check-type context macher-agent-context)
  (when-let* ((root (or (macher-agent-context-project-root context)
                        (macher-agent-context-workspace-root context)))
              (cmds-dir (expand-file-name "commands" root)))
    (when (file-directory-p cmds-dir)
      (dolist (file (directory-files cmds-dir t "\\.md$"))
        (macher-agent--load-skill-from-path file context)))
    (when-let* ((tracked (and (functionp macher-agent-workspace-list-files-function)
                              (funcall macher-agent-workspace-list-files-function context))))
      (dolist (path tracked)
        (when-let* (((stringp path))
                    (abs-path (if (file-name-absolute-p path)
                                  path
                                (expand-file-name path root)))
                    ((string-prefix-p cmds-dir abs-path))
                    ((string-match-p "\\.md$" abs-path)))
          (macher-agent--load-skill-from-path abs-path context)))))
  (when (boundp 'macher-agent-skill-directories)
    (dolist (dir macher-agent-skill-directories)
      (when (and dir (file-directory-p dir))
        (let ((cmds-sibling (expand-file-name "../commands" dir))
              (cmds-child (expand-file-name "commands" dir)))
          (dolist (target-dir (list cmds-sibling cmds-child))
            (when (file-directory-p target-dir)
              (dolist (file (directory-files target-dir t "\\.md$"))
                (macher-agent--load-skill-from-path file context)))))))))

(defun macher-agent-initialize-skills (&optional context)
  "Load, register, and synchronise skills into CONTEXT or global defaults."
  (interactive)
  (if (null context)
      (with-temp-buffer
        (unless (local-variable-p 'gptel--known-presets)
          (make-local-variable 'gptel--known-presets))
        (unless (local-variable-p 'gptel--known-tools)
          (make-local-variable 'gptel--known-tools))
        (unless (local-variable-p 'gptel-directives)
          (make-local-variable 'gptel-directives))
        (let ((bootstrap-ctx (macher-agent--make-context :tools macher-agent-tools-registry)))
          (macher-agent-initialize-skills bootstrap-ctx)
          (setq macher-agent-global-skills-alist (macher-agent-context-skills bootstrap-ctx)))
        (setq-default gptel--known-tools (copy-tree gptel--known-tools))
        (setq-default gptel--known-presets (copy-alist gptel--known-presets))
        (setq-default gptel-directives (copy-alist gptel-directives)))
    (cl-check-type context macher-agent-context)
    (setf (macher-agent-context-skills context) nil)
    (let* ((ws-root (or (macher-agent-context-project-root context)
                        (macher-agent-context-workspace-root context)))
           (bundled (bound-and-true-p macher-agent--bundled-skills-dir)))
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
      (macher-agent-initialize-commands context)
      (let ((merged (macher-agent-workspace-skills-alist context)))
        (macher-agent--sync-gptel-known-presets merged)))))

(defun macher-agent-refresh-skills-and-tools (context)
  "Reset buffer-local GPTel tables to defaults and reload skills and tools for CONTEXT."
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

;;; Tool Normalisation

(defun macher-agent--select-resolved-tool (resolved _item)
  "Return a single-element list containing RESOLVED when it is a valid tool struct."
  (when (macher-tool-valid-p resolved)
    (list resolved)))

(defun macher-agent--resolve-tool-in-env (item)
  "Resolve tool ITEM using active workspace registry and environment context."
  (let* ((item-str (cond ((stringp item) item)
                         ((symbolp item) (symbol-name item))
                         (t (format "%s" item))))
         (ctx (macher-agent-resolve-context))
         (registry (if (macher-agent-valid-context-p ctx)
                       (macher-agent--local-tools-table ctx)
                     macher-agent-tools-registry))
         (skills-dir (when (macher-agent-valid-context-p ctx)
                       (macher-agent-context-project-root ctx)))
         (resolved (macher-agent-resolve-tool item-str registry skills-dir ctx)))
    (macher-agent--select-resolved-tool resolved item)))

(cl-defgeneric macher-agent-resolve-item (item)
  "Resolve tool representation ITEM into a list of gptel tool structs.")

(cl-defmethod macher-agent-resolve-item ((item cl-structure-object))
  "Resolve tool ITEM when it is a structure object."
  (when (macher-tool-valid-p item) (list item)))

(cl-defmethod macher-agent-resolve-item ((item record))
  "Resolve tool ITEM when it is a record object."
  (when (macher-tool-valid-p item) (list item)))

(cl-defmethod macher-agent-resolve-item ((item string))
  "Resolve tool ITEM when it is a string name."
  (macher-agent--resolve-tool-in-env item))

(cl-defmethod macher-agent-resolve-item ((item symbol))
  "Resolve tool ITEM when it is a symbol."
  (if (fboundp item)
      (let ((res (funcall item)))
        (if res
            (macher-agent-resolve-item res)
          (macher-agent--resolve-tool-in-env (symbol-name item))))
    (macher-agent--resolve-tool-in-env (symbol-name item))))

(cl-defmethod macher-agent-resolve-item ((item list))
  "Resolve tool ITEM when it is a list of tool representations."
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
  (when (macher-tool-valid-p item)
    (list item)))

(defun macher-agent--deduplicate-tool (tool seen)
  "Deduplicate validated TOOL struct against SEEN hash table."
  (cl-check-type tool gptel-tool)
  (let* ((raw-name (macher-agent-canonical-tool-name tool))
         (canon-name (replace-regexp-in-string "-" "_" (substring-no-properties (format "%s" raw-name)))))
    (when (not (gethash canon-name seen))
      (puthash canon-name t seen)
      tool)))

(defun macher-agent-normalize-tools (tools)
  "Normalise, resolve, and deduplicate a mixed list of TOOLS."
  (when tools
    (let ((flat-resolved (delq nil (macher-agent-resolve-item tools)))
          (seen (make-hash-table :test 'equal))
          (unique nil))
      (dolist (tool flat-resolved)
        (when-let* ((deduped (macher-agent--deduplicate-tool tool seen)))
          (push deduped unique)))
      (nreverse unique))))

(defun macher-agent-resolve-to-struct (t-item)
  "Convert tool item T-ITEM into a single gptel tool structure."
  (car (macher-agent-normalize-tools t-item)))

;;; Preset and Payload Composition

(defun macher-agent--compose-merge-system-prompt (current-sys sym sys-spec)
  "Merge SYS-SPEC into CURRENT-SYS for skill SYM."
  (let ((normalized-sym (cond
                         ((symbolp sym) (symbol-name sym))
                         ((stringp sym) sym)
                         ((consp sym)
                          (or (plist-get sym :name)
                              (when (symbolp (car sym)) (symbol-name (car sym)))
                              "preset"))
                         (t "preset"))))
    (when (and (consp sys-spec) (eq (car sys-spec) :function) (cadr sys-spec))
      (setq sys-spec (cadr sys-spec)))
    (unless (or (stringp sys-spec)
                (functionp sys-spec)
                (and (consp sys-spec) (not (keywordp (car sys-spec))))
                (null sys-spec))
      (signal 'wrong-type-argument `((or string function cons null) ,sys-spec)))
    (let* ((cur (cond
                 ((stringp current-sys) current-sys)
                 ((listp current-sys) (string-join (delq nil current-sys) "\n---\n"))
                 (t "")))
           (spec-str
            (cond
             ((functionp sys-spec)
              (let ((res (funcall sys-spec cur)))
                (if (stringp res) res (format "%s" res))))
             ((consp sys-spec)
              (string-join (delq nil (mapcar (lambda (x) (if (stringp x) x (format "%s" x))) sys-spec)) "\n"))
             ((stringp sys-spec) sys-spec)
             (t "")))
           (clean-spec (string-trim spec-str)))
      (if (or (string-empty-p clean-spec)
              (string-match-p (regexp-quote clean-spec) cur))
          cur
        (concat cur
                (if (string-empty-p cur) "" "\n---\n")
                (format "### Skill: %s\n%s\n" normalized-sym clean-spec))))))

(defun macher-agent--compose-merge-tools (current-tools tools-spec)
  "Merge TOOLS-SPEC into CURRENT-TOOLS without polluting global registries."
  (let* ((effective-spec
          (if (and current-tools
                   (not (and (consp tools-spec) (keywordp (car tools-spec)))))
              (list :append (if (listp tools-spec) tools-spec (list tools-spec)))
            tools-spec))
         (merged-tools (gptel--modify-value current-tools effective-spec))
         (make-anon-tool (lambda (plist)
                           (if (fboundp 'gptel--make-tool)
                               (apply #'gptel--make-tool plist)
                             (apply #'gptel-make-tool plist)))))
    (cond
     ((null merged-tools) nil)
     ((and (fboundp 'gptel-tool-p) (gptel-tool-p merged-tools))
      (list merged-tools))
     ((and (consp merged-tools) (keywordp (car merged-tools)))
      (list (funcall make-anon-tool merged-tools)))
     ((and (listp merged-tools)
           (not (and (consp merged-tools) (keywordp (car merged-tools))))
           (let ((single (ignore-errors (gptel-get-tool merged-tools))))
             (and single (fboundp 'gptel-tool-p) (gptel-tool-p single))))
      (let ((single (ignore-errors (gptel-get-tool merged-tools))))
        (if single (list single) nil)))
     ((listp merged-tools)
      (cl-loop for t-obj in merged-tools
               append (let* ((res (if (and (fboundp 'gptel-tool-p) (gptel-tool-p t-obj))
                                      t-obj
                                    (ignore-errors (gptel-get-tool t-obj)))))
                        (cond
                         ((and (fboundp 'gptel-tool-p) (gptel-tool-p res))
                          (list res))
                         ((and (consp t-obj) (keywordp (car t-obj)))
                          (list (funcall make-anon-tool t-obj)))
                         ((and (listp res) (not (and (fboundp 'gptel-tool-p) (gptel-tool-p res))))
                          res)
                         (t
                          (list (or res t-obj)))))))
     (t
      (list (if (and (fboundp 'gptel-tool-p) (gptel-tool-p merged-tools))
                merged-tools
              (or (ignore-errors (gptel-get-tool merged-tools)) merged-tools)))))))

(defun macher-agent--resolve-preset-or-tool (sym known)
  "Resolve SYM against KNOWN presets and tool registries."
  (when sym
    (cond
     ((and (fboundp 'gptel-tool-p) (gptel-tool-p sym))
      (cons 'tool sym))
     ((and (consp sym) (keywordp (car sym)))
      (if (or (plist-member sym :function) (plist-member sym :args))
          (cons 'tool sym)
        (cons 'preset sym)))
     (t
      (let* ((str (if (symbolp sym) (symbol-name sym) (format "%s" sym)))
             (clean-str (replace-regexp-in-string "^@+" "" str))
             (clean-sym (intern clean-str)))
        (if-let*
            ((spec (or (alist-get clean-sym known)
                       (when-let* ((ctx (macher-agent-resolve-context)))
                         (ignore-errors (alist-get clean-sym (macher-agent-workspace-skills-alist ctx))))
                       (alist-get clean-sym (bound-and-true-p macher-agent-global-skills-alist)))))
            (cons 'preset spec)
          (when-let*
              ((tool
                (when (fboundp 'gptel-get-tool)
                  (or (ignore-errors (gptel-get-tool clean-str))
                      (ignore-errors (gptel-get-tool (replace-regexp-in-string "-" "_" clean-str)))
                      (ignore-errors (gptel-get-tool (replace-regexp-in-string "_" "-" clean-str)))))))
            (cons 'tool tool))))))))

(defvar macher-agent-preset-pipeline-functions
  '(macher-agent-preset-pipe--exclusive
    macher-agent-preset-pipe--system
    macher-agent-preset-pipe--tools
    macher-agent-preset-pipe--boot
    macher-agent-preset-pipe--parameters)
  "Pipeline functions for preset payload composition.")

(defun macher-agent-preset-pipe--exclusive (state item)
  "Apply the `:exclusive' preset modifier in ITEM to STATE."
  (pcase item
    ((or `(preset ,_sym ,spec)
         `(exclusive-reset ,_sym ,spec))
     (if (plist-get spec :exclusive)
         (let ((st (copy-sequence state)))
           (setq st (plist-put st :system nil))
           (setq st (plist-put st :tools nil))
           (setq st (plist-put st :boot-directive nil))
           (setq st (plist-put st :ptc-primitives nil))
           st)
       state))
    (_ state)))

(defun macher-agent-preset-pipe--system (state item)
  "Merge the system prompt specification in ITEM into STATE."
  (pcase item
    (`(preset ,sym ,spec)
     (if-let* ((sys-spec (or (plist-get spec :system)
                             (plist-get spec :system-message))))
         (plist-put state :system
                    (macher-agent--compose-merge-system-prompt
                     (plist-get state :system) sym sys-spec))
       state))
    (_ state)))

(defun macher-agent-preset-pipe--tools (state item)
  "Merge allowed tools specification or standalone tool in ITEM into STATE."
  (pcase item
    (`(preset ,_sym ,spec)
     (if-let* ((tools-spec (or (plist-get spec :tools)
                               (plist-get spec :allowed-tools))))
         (plist-put state :tools
                    (macher-agent--compose-merge-tools
                     (plist-get state :tools) tools-spec))
       state))
    (`(tool ,tool-obj)
     (let* ((canonical-tool
             (cond
              ((and (fboundp 'gptel-tool-p) (gptel-tool-p tool-obj)) tool-obj)
              ((stringp tool-obj) (ignore-errors (gptel-get-tool tool-obj)))
              ((symbolp tool-obj) (ignore-errors (gptel-get-tool (symbol-name tool-obj))))
              ((consp tool-obj) (ignore-errors (gptel-get-tool (plist-get tool-obj :name))))
              (t nil))))
       (if canonical-tool
           (plist-put state :tools
                      (macher-agent--compose-merge-tools
                       (plist-get state :tools)
                       (list :append canonical-tool)))
         state)))
    (_ state)))

(defun macher-agent-preset-pipe--boot (state item)
  "Merge boot directive specification in ITEM into STATE."
  (pcase item
    (`(preset ,_sym ,spec)
     (if-let* ((bd (plist-get spec :boot-directive)))
         (plist-put state :boot-directive bd)
       state))
    (_ state)))

(defun macher-agent-preset-pipe--parameters (state item)
  "Merge model parameters in ITEM into STATE."
  (pcase item
    (`(preset ,_sym ,spec)
     (let ((st state))
       (dolist (key '(:model :backend :temperature :max-tokens))
         (when-let* ((val (plist-get spec key)))
           (setq st (plist-put st key val))))
       st))
    (_ state)))

(defun macher-agent--flatten-preset-dependencies (inline-presets known)
  "Pre-flatten INLINE-PRESETS and their parent preset dependencies using KNOWN."
  (let ((result nil)
        (visited nil))
    (cl-labels
        ((flatten-item (sym)
           (cond
            ((and (symbolp sym) (member sym visited))
             nil)
            (t
             (when (symbolp sym)
               (push sym visited))
             (pcase (macher-agent--resolve-preset-or-tool sym known)
               (`(preset . ,spec)
                (let* ((parents (plist-get spec :parents))
                       (exclusive (plist-get spec :exclusive)))
                  (if (and exclusive parents)
                      (progn
                        (push (list 'exclusive-reset sym (list :exclusive t)) result)
                        (dolist (parent (if (listp parents) parents (list parents)))
                          (flatten-item parent))
                        (let ((clean-spec (copy-sequence spec)))
                          (setq clean-spec (plist-put clean-spec :exclusive nil))
                          (push (list 'preset sym clean-spec) result)))
                    (when parents
                      (dolist (parent (if (listp parents) parents (list parents)))
                        (flatten-item parent)))
                    (push (list 'preset sym spec) result))))
               (`(tool . ,tool)
                (push (list 'tool tool) result))
               (_
                (display-warning 'macher-agent
                                 (format "Unknown or unresolvable preset/tool: %s" sym)
                                 :warning)))))))
      (dolist (sym inline-presets)
        (flatten-item sym))
      (nreverse result))))

(defun macher-agent-compose-payload (base-state inline-presets)
  "Compose a transmission payload by merging BASE-STATE with INLINE-PRESETS."
  (let* ((state (copy-sequence base-state))
         (known (plist-get base-state :known-presets))
         (flattened (macher-agent--flatten-preset-dependencies inline-presets known))
         (static-steps macher-agent-preset-pipeline-functions)
         (dynamic-steps (macher-agent-get-pipeline-steps 'preset-composition)))
    (setq state
          (cl-reduce
           (lambda (st item)
             (cl-reduce (lambda (s pipe-fn)
                          (if (functionp pipe-fn)
                              (funcall pipe-fn s item)
                            s))
                        static-steps
                        :initial-value st))
           flattened
           :initial-value state))
    (when dynamic-steps
      (setq state (plist-put state :resolved-items flattened))
      (setq state
            (seq-reduce (lambda (st dyn-fn)
                          (if (functionp dyn-fn)
                              (funcall dyn-fn st)
                            st))
                        dynamic-steps
                        state)))
    (setq state (plist-put state :tools (macher-agent-normalize-tools (plist-get state :tools))))
    state))

(defun macher-agent--apply-payload-locally (payload)
  "Apply a composed PAYLOAD to the current buffer variables."
  (when payload
    (when (plist-member payload :system)
      (setq-local gptel-system-prompt (plist-get payload :system)))
    (when (plist-member payload :model)
      (setq-local gptel-model (plist-get payload :model)))
    (when (plist-member payload :backend)
      (setq-local gptel-backend (plist-get payload :backend)))
    (when (plist-member payload :temperature)
      (setq-local gptel-temperature (plist-get payload :temperature)))
    (when (plist-member payload :max-tokens)
      (setq-local gptel-max-tokens (plist-get payload :max-tokens)))
    (when (plist-member payload :tools)
      (setq-local gptel-tools (plist-get payload :tools)))
    (when (plist-member payload :boot-directive)
      (setq-local macher-agent--boot-directive (plist-get payload :boot-directive)))))

(defun macher-agent--extract-first-preset-symbol (resolved-presets)
  "Extract lead symbol explicitly from RESOLVED-PRESETS list."
  (cl-check-type resolved-presets list)
  (let ((first (car resolved-presets)))
    (cond
     ((symbolp first) first)
     ((stringp first) (intern first))
     (t nil))))

(defun macher-agent--apply-preset (preset-or-presets)
  "Apply validated PRESET-OR-PRESETS list to buffer-local variables."
  (let* ((presets (if (listp preset-or-presets) preset-or-presets (list preset-or-presets)))
         (base-state (list :model gptel-model
                           :backend (bound-and-true-p gptel-backend)
                           :system (bound-and-true-p gptel-system-prompt)
                           :temperature (bound-and-true-p gptel-temperature)
                           :max-tokens (bound-and-true-p gptel-max-tokens)
                           :tools gptel-tools
                           :known-presets (bound-and-true-p gptel--known-presets)))
         (payload (macher-agent-compose-payload base-state presets)))
    (when presets
      (let* ((primary-sym (macher-agent--extract-first-preset-symbol presets))
             (known (plist-get base-state :known-presets))
             (spec (when primary-sym
                     (or (alist-get primary-sym known)
                         (when-let* ((ctx (macher-agent-resolve-context)))
                           (alist-get primary-sym (macher-agent-workspace-skills-alist ctx)))
                         (alist-get primary-sym (bound-and-true-p macher-agent-global-skills-alist)))))
             (raw-sys (or (plist-get spec :system) (plist-get spec :system-message)))
             (boot-dir (plist-get spec :boot-directive)))
        (when (stringp raw-sys)
          (setq payload (plist-put payload :system raw-sys)))
        (when boot-dir
          (setq payload (plist-put payload :boot-directive boot-dir)))))
    (macher-agent--apply-payload-locally payload)))

(defun macher-agent-use-skill (skill &optional buf)
  "Attach specific SKILL to live BUF object."
  (interactive "sSkill: ")
  (let ((target-buf (or buf (current-buffer)))
        (skill-list (ensure-list skill)))
    (cl-check-type target-buf buffer)
    (when (buffer-live-p target-buf)
      (with-current-buffer target-buf
        (setq-local macher-agent-presets skill-list)
        (macher-agent--apply-preset skill-list)))))

(provide 'macher-agent-presets)
;;; macher-agent-presets.el ends here
