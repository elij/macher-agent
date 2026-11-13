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
(declare-function gptel--parse-directive "gptel-request" (directive &optional raw))

(defvar gptel--preset)
(defvar gptel--known-presets)
(defvar gptel--known-tools)
(defvar gptel-directives)
(defvar gptel-system-prompt)
(defvar gptel-model)
(defvar gptel-backend)
(defvar gptel--known-backends)
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

(defvar-local macher-agent--cached-presets-set nil
  "Flag indicating whether `macher-agent--cached-presets' was saved.")
(put 'macher-agent--cached-presets-set 'permanent-local t)

(defvar-local macher-agent--cached-skill-baseline nil
  "Baseline buffer settings plist saved prior to `use_skill' activation.")
(put 'macher-agent--cached-skill-baseline 'permanent-local t)

(defun macher-agent--has-cached-skill-state-p (&optional buf)
  "Return non-nil when BUF holds cached baseline state from `use_skill'."
  (let ((target (or buf (current-buffer))))
    (when (and (bufferp target) (buffer-live-p target))
      (or (buffer-local-value 'macher-agent--cached-presets-set target)
          (local-variable-p 'macher-agent--cached-presets target)
          (buffer-local-value 'macher-agent--cached-boot-directive-set target)
          (buffer-local-value 'macher-agent--cached-skill-baseline target)))))

(defvar macher-agent-mid-turn-recompile-function nil
  "Hook called with (TARGET-BUF &optional CONTEXT) to sync FSM mid-turn.")

(defvar macher-agent--workspace-tool-overrides
  (make-hash-table :test 'eq :weakness 'key)
  "Weak-key table mapping workspace tools to local `ptc-function' values.")

(defvar macher-agent--bundled-skills-dir
  (let ((base-file (or load-file-name
                       buffer-file-name
                       (locate-library "macher-agent-presets")
                       (locate-library "macher-agent"))))
    (expand-file-name "skills"
                      (if base-file
                          (file-name-directory base-file)
                        default-directory)))
  "Store internal directory path to bundled skills.")

(defun macher-agent--resolve-bundled-skills-dir ()
  "Return live bundled skills directory, relocating if cached path expired."
  (if (and (bound-and-true-p macher-agent--bundled-skills-dir)
           (file-directory-p macher-agent--bundled-skills-dir))
      macher-agent--bundled-skills-dir
    (when-let* ((lib (or (locate-library "macher-agent-presets")
                         (locate-library "macher-agent")))
                (dir (expand-file-name "skills" (file-name-directory lib)))
                ((file-directory-p dir)))
      (setq macher-agent--bundled-skills-dir dir))))

(defconst macher-agent--preset-meta-keys
  '(:exclusive :is-command :boot-directive :ptc-primitives)
  "Preset keys managed by `macher-agent' outside `gptel--known-presets'.")

(defun macher-agent-set-preset-meta (sym &rest meta)
  "Attach `macher-agent' preset META plist to preset symbol SYM."
  (cl-check-type sym symbol)
  (put sym 'macher-agent-preset-meta meta)
  sym)

(defun macher-agent--merge-preset-meta (sym &rest specs)
  "Merge preset SPECS and symbol-property metadata for SYM into one plist."
  (let ((sym-meta (and (symbolp sym) (get sym 'macher-agent-preset-meta)))
        (result nil))
    (dolist (spec (append specs (list sym-meta)))
      (when (and (listp spec) (keywordp (car spec)))
        (let ((tail spec))
          (while (consp tail)
            (let ((k (pop tail))
                  (v (pop tail)))
              (unless (plist-member result k)
                (setq result (plist-put result k v))))))))
    result))

(defun macher-agent--local-tools-table (context)
  "Return persistent local tools table for CONTEXT, creating it if needed."
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
  "Parse frontmatter KEY and non-empty RAW-VAL into hash table HT."
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
          :backend (gethash "backend" fm-hash)
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
  "Read a single form from BUF and optionally run VALIDATION-CB."
  (let ((form (read buf)))
    (when (and validation-cb (not (funcall validation-cb form)))
      (error "Validation failed for form: %S" form))
    form))

(defun macher-agent--parse-safe-forms (content &optional validation-cb)
  "Parse CONTENT into Lisp forms, checking each with VALIDATION-CB."
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
  "Parse CONTENT and validate its AST forms for TOOL-NAME."
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
  "Evaluate trusted FORMS for TOOL-NAME and return the captured tool."
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
  (let* ((ws-root (when (macher-agent-valid-context-p context)
                    (or (macher-agent-context-project-root context)
                        (macher-agent-context-workspace-root context))))
         (in-ws (and ws-root path
                     (or (not (file-name-absolute-p path))
                         (let ((canon-ws (file-name-as-directory
                                          (file-truename (expand-file-name ws-root))))
                               (canon-path (file-truename (expand-file-name path))))
                           (string-prefix-p canon-ws canon-path))))))
    (if (and in-ws (functionp macher-agent-workspace-read-file-function) context path)
        (funcall macher-agent-workspace-read-file-function context path)
      (let ((disk-path (if (and path ws-root (not (file-name-absolute-p path)))
                           (expand-file-name path ws-root)
                         path)))
        (when (and disk-path (file-exists-p disk-path) (not (file-directory-p disk-path)))
          (ignore-errors
            (with-temp-buffer
              (insert-file-contents disk-path)
              (buffer-string))))))))

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
  "Verify PATH against managed skill, command, or script signatures."
  (and (stringp path)
       (or (string-match-p "SKILL\\.md$" path)
           (string-match-p "\\(?:\\`\\|/\\)skills/" path)
           (string-match-p "\\(?:\\`\\|/\\)\\.skills/" path)
           (string-match-p "\\(?:\\`\\|/\\)commands/" path)
           (string-match-p "\\(?:\\`\\|/\\)scripts/\\([^/]+\\)\\.el\\'" path))))

(defun macher-agent--context-workspace-root (context)
  "Return canonical workspace root directory for valid CONTEXT, or nil."
  (when (macher-agent-valid-context-p context)
    (when-let* ((raw (or (macher-agent-context-project-root context)
                         (macher-agent-context-workspace-root context))))
      (file-name-as-directory (file-truename (expand-file-name raw))))))

(defun macher-agent--path-in-workspace-p (path ws-root)
  "Return non-nil when PATH lies inside WS-ROOT."
  (when (and (stringp path) (stringp ws-root))
    (if (not (file-name-absolute-p path))
        t
      (let ((canon-ws (file-name-as-directory
                       (file-truename (expand-file-name ws-root))))
            (canon-path (file-truename (expand-file-name path))))
        (or (string-prefix-p canon-ws canon-path)
            (string-equal (directory-file-name canon-ws)
                          (directory-file-name canon-path)))))))

(defun macher-agent--tool-name-variants (tool-name)
  "Return deduplicated list of string name variants for TOOL-NAME."
  (when-let* ((canon (macher-agent-canonical-tool-name tool-name)))
    (let ((under (replace-regexp-in-string "-" "_" canon))
          (hyphen (replace-regexp-in-string "_" "-" canon)))
      (delete-dups (list canon under hyphen)))))

(defun macher-agent--lookup-tool-in-table (variants table)
  "Return first valid tool matching any key in VARIANTS from TABLE."
  (when (hash-table-p table)
    (cl-loop for key in variants
             for val = (or (gethash key table)
                           (gethash (intern key) table))
             when (macher-tool-valid-p val)
             return val)))

(defun macher-agent--context-live-buffers (context)
  "Return live buffers for CONTEXT, or compatible `current-buffer' if none."
  (let ((bufs nil)
        (owner (macher-agent-context-owner-buffer context))
        (origin (macher-agent-context-origin-buffer context)))
    (dolist (b (list owner origin))
      (when (and (bufferp b) (buffer-live-p b))
        (cl-pushnew b bufs :test #'eq)))
    (dolist (b (buffer-list))
      (when (and (buffer-live-p b)
                 (eq (buffer-local-value 'macher-agent--persistent-context b) context))
        (cl-pushnew b bufs :test #'eq)))
    (let ((cur (current-buffer)))
      (when (and (buffer-live-p cur)
                 (or (and (local-variable-p 'macher-agent--persistent-context cur)
                          (eq (buffer-local-value 'macher-agent--persistent-context cur) context))
                     (and (null bufs)
                          (or (not (local-variable-p 'macher-agent--persistent-context cur))
                              (null (buffer-local-value 'macher-agent--persistent-context cur))))))
        (cl-pushnew cur bufs :test #'eq)))
    (nreverse bufs)))

(defun macher-agent--unregister-tool-from-gptel-tables (variants)
  "Remove tools matching VARIANTS from buffer-local `gptel--known-tools'."
  (when (boundp 'gptel--known-tools)
    (setq-local gptel--known-tools
                (mapcar (lambda (cat-entry)
                          (if (consp (cdr cat-entry))
                              (cons (car cat-entry)
                                    (cl-remove-if (lambda (tool-entry)
                                                    (member (car-safe tool-entry) variants))
                                                  (cdr cat-entry)))
                            cat-entry))
                        gptel--known-tools))))

(defun macher-agent--collect-workspace-script-candidates (variants context &optional dir-context)
  "Collect workspace script paths for VARIANTS in CONTEXT and DIR-CONTEXT."
  (when-let* ((ws-root (macher-agent--context-workspace-root context)))
    (let ((candidates nil)
          (tracked (and (functionp macher-agent-workspace-list-files-function)
                        (funcall macher-agent-workspace-list-files-function context))))
      (when (and dir-context (macher-agent--path-in-workspace-p dir-context ws-root))
        (let ((norm-dir (file-name-as-directory (file-truename (expand-file-name dir-context)))))
          (dolist (v variants)
            (push (expand-file-name (format "scripts/%s.el" v) norm-dir) candidates))))
      (dolist (v variants)
        (push (expand-file-name (format "skills/scripts/%s.el" v) ws-root) candidates)
        (push (expand-file-name (format ".skills/scripts/%s.el" v) ws-root) candidates))
      (when tracked
        (let ((vfs-top nil)
              (vfs-dot-top nil)
              (vfs-nested nil)
              (vfs-dot-nested nil)
              (vfs-root nil))
          (dolist (rel tracked)
            (when (stringp rel)
              (dolist (v variants)
                (let ((fname (regexp-quote (format "%s.el" v))))
                  (cond
                   ((string-match-p (format "\\`skills/scripts/%s\\'" fname) rel)
                    (push (expand-file-name rel ws-root) vfs-top))
                   ((string-match-p (format "\\`\\.skills/scripts/%s\\'" fname) rel)
                    (push (expand-file-name rel ws-root) vfs-dot-top))
                   ((string-match-p (format "\\`skills/[^/]+/scripts/%s\\'" fname) rel)
                    (push (expand-file-name rel ws-root) vfs-nested))
                   ((string-match-p (format "\\`\\.skills/[^/]+/scripts/%s\\'" fname) rel)
                    (push (expand-file-name rel ws-root) vfs-dot-nested))
                   ((string-match-p (format "\\`scripts/%s\\'" fname) rel)
                    (push (expand-file-name rel ws-root) vfs-root)))))))
          (dolist (group (list vfs-top vfs-dot-top vfs-nested vfs-dot-nested vfs-root))
            (dolist (p (sort group #'string<))
              (push p candidates)))))
      (dolist (sub-name '("skills" ".skills"))
        (let ((base (expand-file-name sub-name ws-root)))
          (when (file-directory-p base)
            (dolist (sub (sort (directory-files base t macher-agent--file-scan-regex) #'string<))
              (when (file-directory-p sub)
                (dolist (v variants)
                  (push (expand-file-name (format "scripts/%s.el" v) sub) candidates)))))))
      (dolist (v variants)
        (push (expand-file-name (format "scripts/%s.el" v) ws-root) candidates))
      (delete-dups (nreverse candidates)))))

(defun macher-agent--collect-external-script-candidates (variants &optional dir-context)
  "Collect non-workspace script paths for VARIANTS and DIR-CONTEXT."
  (let ((candidates nil)
        (search-dirs (delq nil
                           (append
                            (when dir-context (list dir-context))
                            (when (boundp 'macher-agent-skill-directories)
                              macher-agent-skill-directories)
                            (when-let* ((bdir (macher-agent--resolve-bundled-skills-dir)))
                              (list bdir))))))
    (dolist (dir (delete-dups (mapcar #'expand-file-name search-dirs)))
      (dolist (v variants)
        (push (expand-file-name (format "scripts/%s.el" v) dir) candidates))
      (when (file-directory-p dir)
        (dolist (sub (sort (directory-files dir t macher-agent--file-scan-regex) #'string<))
          (when (file-directory-p sub)
            (dolist (v variants)
              (push (expand-file-name (format "scripts/%s.el" v) sub) candidates))))))
    (delete-dups (nreverse candidates))))

(defun macher-agent--prune-skill-from-buffers (sym buffers)
  "Remove preset metadata and local table entries for SYM across BUFFERS."
  (when (symbolp sym)
    (cl-remprop sym 'macher-agent-preset-meta)
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (local-variable-p 'gptel--known-presets buf)
            (setq-local gptel--known-presets
                        (cl-remove sym gptel--known-presets :key #'car-safe :test #'eq)))
          (when (local-variable-p 'gptel-directives buf)
            (setq-local gptel-directives
                        (cl-remove sym gptel-directives :key #'car-safe :test #'eq))))))))

(defun macher-agent--mutation-dispatcher (&rest args)
  "Dispatch context mutation ARGS to workspace-local invalidation handlers."
  (let* ((path (cl-find-if #'stringp args))
         (context (or (cl-find-if #'macher-agent-valid-context-p args)
                      (macher-agent-resolve-context))))
    (when (and context (or (null path) (macher-agent--is-managed-path-p path)))
      (let* ((ws-root (macher-agent--context-workspace-root context))
             (is-script (and path
                             (string-match "\\(?:\\`\\|/\\)scripts/\\([^/]+\\)\\.el\\'" path)))
             (script-tool-name (when is-script (match-string 1 path)))
             (script-exists (when is-script
                              (macher-agent--read-vfs-or-disk context path)))
             (target-bufs (macher-agent--context-live-buffers context)))
        (when is-script
          (let* ((variants (macher-agent--tool-name-variants script-tool-name))
                 (local-reg (macher-agent--local-tools-table context))
                 (abs-path (if (and ws-root (not (file-name-absolute-p path)))
                               (expand-file-name path ws-root)
                             (expand-file-name path)))
                 (script-dir-context
                  (directory-file-name
                   (file-name-directory
                    (directory-file-name (file-name-directory abs-path))))))
            (dolist (variant variants)
              (remhash variant local-reg)
              (remhash (intern variant) local-reg))
            (if script-exists
                (let ((reloaded (macher-agent-resolve-tool
                                 script-tool-name local-reg script-dir-context context)))
                  (when (macher-tool-valid-p reloaded)
                    (let ((cat (or (gptel-tool-category reloaded) "macher-agent"))
                          (reloaded-name (gptel-tool-name reloaded)))
                      (dolist (buf target-bufs)
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (macher-agent--unregister-tool-from-gptel-tables
                             (delete-dups (cons reloaded-name variants)))
                            (macher-agent--register-tool-in-gptel-tables reloaded cat)
                            (when (and (local-variable-p 'gptel-tools buf)
                                       (listp gptel-tools))
                              (setq-local gptel-tools
                                          (mapcar (lambda (tl)
                                                    (if (and (macher-tool-valid-p tl)
                                                             (member (gptel-tool-name tl)
                                                                     (cons reloaded-name variants)))
                                                        reloaded
                                                      tl))
                                                  gptel-tools)))))))))
              (let ((bufs-with-tool nil))
                (dolist (buf target-bufs)
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (macher-agent--unregister-tool-from-gptel-tables variants)
                      (when (and (local-variable-p 'gptel-tools buf)
                                 (listp gptel-tools))
                        (when (cl-some (lambda (tl)
                                         (and (macher-tool-valid-p tl)
                                              (member (gptel-tool-name tl) variants)))
                                       gptel-tools)
                          (push buf bufs-with-tool))
                        (setq-local gptel-tools
                                    (cl-remove-if (lambda (tl)
                                                    (and (macher-tool-valid-p tl)
                                                         (member (gptel-tool-name tl) variants)))
                                                  gptel-tools))))))
                (when-let* ((fallback (with-temp-buffer
                                        (macher-agent-resolve-tool
                                         script-tool-name nil nil context)))
                            ((macher-tool-valid-p fallback)))
                  (let ((cat (or (gptel-tool-category fallback) "macher-agent"))
                        (fb-name (gptel-tool-name fallback)))
                    (dolist (buf target-bufs)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (macher-agent--register-tool-in-gptel-tables fallback cat)
                          (when (and (memq buf bufs-with-tool)
                                     (local-variable-p 'gptel-tools buf)
                                     (listp gptel-tools)
                                     (not (cl-some (lambda (tl)
                                                     (and (macher-tool-valid-p tl)
                                                          (equal (gptel-tool-name tl) fb-name)))
                                                   gptel-tools)))
                            (setq-local gptel-tools
                                        (append gptel-tools (list fallback)))))))))))))
        (when (and path (null (macher-agent--read-vfs-or-disk context path)))
          (when-let* ((sym (macher-agent--skill-name-from-path path)))
            (setf (macher-agent-context-skills context)
                  (assq-delete-all sym (macher-agent-context-skills context)))
            (macher-agent--prune-skill-from-buffers sym target-bufs)))
        (when (or (null path)
                  (string-match-p "\\(?:\\`\\|/\\)\\.?skills/" path)
                  (string-match-p "\\(?:\\`\\|/\\)commands/" path)
                  (string-match-p "SKILL\\.md$" path))
          (macher-agent-initialize-skills context))
        (when (functionp macher-agent-mid-turn-recompile-function)
          (dolist (buf target-bufs)
            (when (and (buffer-live-p buf)
                       (buffer-local-value 'macher-agent--active-fsm buf))
              (funcall macher-agent-mid-turn-recompile-function buf context))))))))

(defun macher-agent--locate-tool-source (tool-name context dir-context script-paths _workspace &optional workspace-only-p)
  "Locate source content for TOOL-NAME across SCRIPT-PATHS.
Searches VFS and disk using CONTEXT and DIR-CONTEXT.  When
WORKSPACE-ONLY-P is non-nil, restrict the fallback lookup to paths
inside the workspace root of CONTEXT."
  (or (when script-paths
        (catch 'found
          (dolist (cand script-paths)
            (when cand
              (let ((content (if (macher-agent-valid-context-p context)
                                 (macher-agent--read-vfs-or-disk context cand)
                               (macher-agent--read-first-existing-file (list cand)))))
                (when content
                  (throw 'found content)))))
          nil))
      (let ((ws-root (macher-agent--context-workspace-root context)))
        (when (and (macher-agent-valid-context-p context)
                   dir-context
                   (or (not workspace-only-p)
                       (and ws-root
                            (macher-agent--path-in-workspace-p dir-context ws-root))))
          (let* ((vfs-path (expand-file-name (format "scripts/%s.el" tool-name) dir-context))
                 (workspace-root (macher-agent-context-workspace-root context))
                 (rel-to-workspace (when workspace-root
                                     (file-relative-name vfs-path workspace-root)))
                 (rel-to-dir (file-relative-name vfs-path dir-context))
                 (visiting-buf (find-buffer-visiting vfs-path))
                 (visiting-buf-name (when visiting-buf (buffer-name visiting-buf)))
                 (candidates (delq nil
                                   (list vfs-path rel-to-workspace
                                         rel-to-dir visiting-buf-name))))
            (macher-agent--resolve-skill-vfs-content candidates context))))))

(defun macher-agent--collect-assigned-symbols (forms)
  "Collect symbols assigned or given `put' properties across FORMS.
Returns `(VAR-SYMS . PROP-SYMS)'."
  (let ((syms nil)
        (prop-syms nil))
    (cl-labels
        ((unquote-sym (node)
           (when (and (consp node) (eq (car node) 'quote) (symbolp (cadr node)))
             (cadr node)))
         (walk (node)
           (when (consp node)
             (pcase (car node)
               ((or 'setq 'setq-local 'setf)
                (let ((tail (cdr node)))
                  (while (consp tail)
                    (let ((s (car tail)))
                      (cond
                       ((and (symbolp s)
                             (not (keywordp s))
                             (not (memq s '(nil t))))
                        (cl-pushnew s syms :test #'eq))
                       ((and (consp s) (eq (car s) 'get))
                        (when-let* ((ps (unquote-sym (cadr s))))
                          (cl-pushnew ps prop-syms :test #'eq)))))
                    (setq tail (cddr tail)))))
               ('put
                (when-let* ((ps (unquote-sym (cadr node))))
                  (cl-pushnew ps prop-syms :test #'eq)))
               ((or 'defvar 'defconst 'defcustom)
                (let ((s (cadr node)))
                  (when (and (symbolp s)
                             (not (keywordp s))
                             (not (memq s '(nil t))))
                    (cl-pushnew s syms :test #'eq)))))
             (walk (car node))
             (when (consp (cdr node))
               (walk (cdr node))))))
      (dolist (f forms)
        (walk f)))
    (cons syms prop-syms)))

(defun macher-agent--load-tool-from-source (canonical-name context dir-context script-paths workspace &optional workspace-override-p)
  "Load tool structure from source for CANONICAL-NAME.
Resolves source in CONTEXT, DIR-CONTEXT, SCRIPT-PATHS, and WORKSPACE.
When WORKSPACE-OVERRIDE-P is non-nil, isolate symbol `ptc-function'
and tool variable bindings and record the tool struct in
`macher-agent--workspace-tool-overrides'."
  (when canonical-name
    (when-let* ((content (macher-agent--locate-tool-source
                          canonical-name context dir-context script-paths workspace
                          workspace-override-p))
                (forms (macher-agent--parse-and-validate-tool-ast content canonical-name)))
      (let* ((ws-root (macher-agent--context-workspace-root context))
             (bdir (macher-agent--resolve-bundled-skills-dir))
             (isolate-gptel (or workspace-override-p
                                ws-root
                                (and dir-context
                                     (not (and bdir (macher-agent--path-in-workspace-p dir-context bdir))))))
             (gptel--known-tools (if isolate-gptel
                                     (and (boundp 'gptel--known-tools)
                                          (copy-tree gptel--known-tools))
                                   (bound-and-true-p gptel--known-tools)))
             (under-name (replace-regexp-in-string "-" "_" canonical-name))
             (hyphen-name (replace-regexp-in-string "_" "-" canonical-name))
             (scanned (when workspace-override-p
                        (macher-agent--collect-assigned-symbols forms)))
             (extra-syms
              (when workspace-override-p
                (delq nil
                      (mapcar #'intern-soft
                              (delete-dups
                               (list (format "macher-agent-%s-tool" hyphen-name)
                                     (format "macher-agent-%s-tool" under-name)
                                     canonical-name
                                     hyphen-name
                                     under-name))))))
             (assigned-syms
              (when workspace-override-p
                (delete-dups (append (car scanned) extra-syms))))
             (prop-syms
              (when workspace-override-p
                (delete-dups (append (cdr scanned) extra-syms))))
             (prop-snapshots
              (cl-loop for sym in prop-syms
                       when (and sym (not (memq sym '(nil t))))
                       append (cl-loop for prop in '(ptc-function command-fn)
                                       collect (list (cons sym prop)
                                                     (and (plist-member (symbol-plist sym) prop) t)
                                                     (get sym prop)))))
             (var-snapshots
              (mapcar (lambda (sym)
                        (list sym
                              (default-boundp sym)
                              (when (default-boundp sym) (default-value sym))
                              (local-variable-p sym)
                              (when (local-variable-p sym) (symbol-value sym))))
                      assigned-syms))
             (captured-ptc-alist nil)
             (captured-ptc-fn nil)
             (tool
              (unwind-protect
                  (prog1 (macher-agent--evaluate-trusted-tool-ast forms canonical-name)
                    (when workspace-override-p
                      (dolist (psnap prop-snapshots)
                        (pcase-let* ((`(,key ,had-prop ,old-val) psnap)
                                     (sym (car key))
                                     (prop (cdr key))
                                     (now-has (and (plist-member (symbol-plist sym) prop) t))
                                     (new-val (get sym prop)))
                          (when (or (not (eq had-prop now-has))
                                    (not (eq old-val new-val)))
                            (push (cons key new-val) captured-ptc-alist)
                            (when new-val
                              (setq captured-ptc-fn new-val)))))
                      (setq captured-ptc-alist (nreverse captured-ptc-alist))))
                (dolist (psnap prop-snapshots)
                  (pcase-let ((`((,sym . ,prop) ,had-prop ,old-val) psnap))
                    (if had-prop
                        (put sym prop old-val)
                      (cl-remprop sym prop))))
                (dolist (snap var-snapshots)
                  (pcase-let ((`(,sym ,had-default ,old-default ,had-local ,old-local) snap))
                    (if had-local
                        (set (make-local-variable sym) old-local)
                      (when (local-variable-p sym)
                        (kill-local-variable sym)))
                    (if had-default
                        (set-default sym old-default)
                      (when (default-boundp sym)
                        (makunbound sym))))))))
        (when (and workspace-override-p (macher-tool-valid-p tool))
          (when captured-ptc-alist
            (let ((orig-tool-fn (gptel-tool-function tool)))
              (when (functionp orig-tool-fn)
                (setf (gptel-tool-function tool)
                      (lambda (&rest fn-args)
                        (let ((saved nil))
                          (unwind-protect
                              (progn
                                (dolist (cell captured-ptc-alist)
                                  (let ((sym (caar cell))
                                        (prop (cdar cell)))
                                    (push (list sym prop
                                                (plist-member (symbol-plist sym) prop)
                                                (get sym prop))
                                          saved)
                                    (put sym prop (cdr cell))))
                                (apply orig-tool-fn fn-args))
                            (dolist (s saved)
                              (pcase-let ((`(,sym ,prop ,had-prop ,old-val) s))
                                (if had-prop
                                    (put sym prop old-val)
                                  (cl-remprop sym prop)))))))))))
          (let ((primary-ptc-fn
                 (or (cl-loop for sym in extra-syms
                              for entry = (assoc (cons sym 'ptc-function) captured-ptc-alist)
                              when (and entry (functionp (cdr entry)))
                              return (cdr entry))
                     captured-ptc-fn)))
            (puthash tool (or primary-ptc-fn :no-ptc-fn)
                     macher-agent--workspace-tool-overrides)))
        tool))))

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
  "Resolve tool for TOOL-NAME using TOOLS-REGISTRY, DIR-CONTEXT, and CONTEXT.
Caches resolved tools in the workspace-local registry."
  (cl-check-type tool-name string)
  (let* ((valid-ctx (macher-agent-valid-context-p context))
         (local-reg (when valid-ctx
                      (macher-agent--local-tools-table context)))
         (ws-root (macher-agent--context-workspace-root context))
         (variants (macher-agent--tool-name-variants tool-name))
         (canonical-name (car variants))
         (dir-is-ws-p (and ws-root dir-context
                           (macher-agent--path-in-workspace-p dir-context ws-root)))
         (workspace (when valid-ctx (macher-agent-context-workspace context)))
         (ws-cached
          (and canonical-name
               (or (when (and (hash-table-p local-reg)
                              (not (eq local-reg macher-agent-tools-registry)))
                     (macher-agent--lookup-tool-in-table variants local-reg))
                   (when (and (hash-table-p tools-registry)
                              (not (eq tools-registry macher-agent-tools-registry)))
                     (macher-agent--lookup-tool-in-table variants tools-registry))))))
    (or ws-cached
        (let* ((ws-script-paths
                (when (and canonical-name ws-root)
                  (macher-agent--collect-workspace-script-candidates
                   variants context dir-context)))
               (ws-loaded
                (when ws-script-paths
                  (macher-agent--load-tool-from-source
                   canonical-name context dir-context ws-script-paths workspace t))))
          (when (macher-tool-valid-p ws-loaded)
            (if (hash-table-p local-reg)
                (macher-agent--cache-tool ws-loaded local-reg)
              (when (and (hash-table-p tools-registry)
                         (not (eq tools-registry macher-agent-tools-registry)))
                (macher-agent--cache-tool ws-loaded tools-registry)))
            ws-loaded))
        (and canonical-name
             (or (when (hash-table-p tools-registry)
                   (macher-agent--lookup-tool-in-table variants tools-registry))
                 (when (hash-table-p macher-agent-tools-registry)
                   (macher-agent--lookup-tool-in-table variants macher-agent-tools-registry))))
        (let* ((ext-dir (unless dir-is-ws-p dir-context))
               (ext-script-paths
                (when canonical-name
                  (macher-agent--collect-external-script-candidates variants ext-dir)))
               (loaded-tool
                (or (when ext-script-paths
                      (macher-agent--load-tool-from-source
                       canonical-name context dir-context ext-script-paths workspace))
                    (macher-agent--resolve-fallback-tool (intern tool-name) canonical-name))))
          (when (macher-tool-valid-p loaded-tool)
            (cond
             ((hash-table-p local-reg)
              (macher-agent--cache-tool loaded-tool local-reg))
             ((hash-table-p tools-registry)
              (macher-agent--cache-tool loaded-tool tools-registry))
             ((null dir-is-ws-p)
              (macher-agent--cache-tool loaded-tool macher-agent-tools-registry))))
          (or loaded-tool tool-name)))))

(defun macher-agent--load-script-files (script-files in-ws registry context)
  "Resolve and register SCRIPT-FILES into REGISTRY and CONTEXT.
When IN-WS is non-nil, read and refresh workspace overrides."
  (dolist (script (delete-dups (mapcar (lambda (s) (file-truename (expand-file-name s))) script-files)))
    (when (or (not in-ws)
              (macher-agent--read-vfs-or-disk context script))
      (let* ((base-name (file-name-base script))
             (script-parent-dir
              (directory-file-name
               (file-name-directory
                (directory-file-name (file-name-directory script))))))
        (when (and in-ws (not (eq registry macher-agent-tools-registry)))
          (dolist (v (macher-agent--tool-name-variants base-name))
            (remhash v registry)
            (remhash (intern v) registry)))
        (when-let* ((tool (macher-agent-resolve-tool base-name registry script-parent-dir context))
                    ((macher-tool-valid-p tool)))
          (macher-agent--register-tool-in-gptel-tables
           tool (or (gptel-tool-category tool) "macher-agent")))))))

(defun macher-agent--load-scripts-from-dir (skills-dir context)
  "Load script tools from SKILLS-DIR (`scripts/' and `*/scripts/') into CONTEXT."
  (cl-check-type skills-dir string)
  (cl-check-type context macher-agent-context)
  (let* ((expanded-dir (file-name-as-directory (file-truename (expand-file-name skills-dir))))
         (scripts-dir (expand-file-name "scripts" expanded-dir))
         (registry (macher-agent--local-tools-table context))
         (ws-root (macher-agent--context-workspace-root context))
         (in-ws (and ws-root (macher-agent--path-in-workspace-p expanded-dir ws-root)))
         (script-files (when (file-directory-p scripts-dir)
                         (directory-files scripts-dir t "\\.el\\'"))))
    (when (file-directory-p expanded-dir)
      (dolist (sub (directory-files expanded-dir t macher-agent--file-scan-regex))
        (when (file-directory-p sub)
          (let ((sub-scripts (expand-file-name "scripts" sub)))
            (when (file-directory-p sub-scripts)
              (setq script-files
                    (append script-files (directory-files sub-scripts t "\\.el\\'"))))))))
    (when-let* ((tracked (and (functionp macher-agent-workspace-list-files-function)
                              (funcall macher-agent-workspace-list-files-function context)))
                (ws-root ws-root))
      (dolist (path tracked)
        (when-let* (((stringp path))
                    (abs-path (file-truename (expand-file-name path ws-root)))
                    ((string-prefix-p expanded-dir abs-path))
                    (rel (file-relative-name abs-path expanded-dir))
                    ((string-match-p "\\`\\(?:[^/]+/\\)?scripts/[^/]+\\.el\\'" rel)))
          (push abs-path script-files))))
    (macher-agent--load-script-files script-files in-ws registry context)))

(defun macher-agent--register-tool-in-gptel-tables (tool category)
  "Register TOOL struct into buffer-local `gptel--known-tools' under CATEGORY."
  (cl-check-type category string)
  (when (and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
    (unless (local-variable-p 'gptel--known-tools)
      (setq-local gptel--known-tools (copy-tree (bound-and-true-p gptel--known-tools))))
    (let* ((cat-name (if (string-empty-p category) "macher-agent" category))
           (tool-name (gptel-tool-name tool)))
      (when (fboundp 'set-gptel-tool-category)
        (unless (gptel-tool-category tool)
          (set-gptel-tool-category tool cat-name)))
      (setf (alist-get tool-name
                       (alist-get cat-name gptel--known-tools nil nil #'equal)
                       nil nil #'equal)
            tool))))

(defun macher-agent--find-model-in-backend (backend model-str)
  "Search BACKEND for a model matching MODEL-STR."
  (when (and backend model-str (fboundp 'gptel-backend-p) (gptel-backend-p backend))
    (let ((target (if (symbolp model-str) (symbol-name model-str) (format "%s" model-str)))
          (models (gptel-backend-models backend))
          result)
      (dolist (m models)
        (unless result
          (let* ((m-raw (if (consp m) (car m) m))
                 (m-str (if (symbolp m-raw) (symbol-name m-raw) (format "%s" m-raw))))
            (when (equal m-str target)
              (setq result (cons backend m-raw))))))
      result)))

(defun macher-agent-resolve-backend-and-model (model-name)
  "Find first backend and model format matching MODEL-NAME."
  (when (and model-name (boundp 'gptel--known-backends))
    (let ((model-str (if (symbolp model-name) (symbol-name model-name) (format "%s" model-name)))
          result)
      (dolist (item gptel--known-backends)
        (unless result
          (setq result (macher-agent--find-model-in-backend (cdr item) model-str))))
      result)))

(defun macher-agent--resolve-backend (backend-spec)
  "Resolve BACKEND-SPEC to a valid `gptel-backend' struct, or nil."
  (cond
   ((null backend-spec) nil)
   ((and (fboundp 'gptel-backend-p) (gptel-backend-p backend-spec))
    backend-spec)
   ((and (stringp backend-spec)
         (not (string-empty-p (string-trim backend-spec)))
         (boundp 'gptel--known-backends))
    (alist-get (string-trim backend-spec) gptel--known-backends nil nil #'equal))
   ((and (symbolp backend-spec)
         (not (eq backend-spec t))
         (boundp 'gptel--known-backends))
    (alist-get (symbol-name backend-spec) gptel--known-backends nil nil #'equal))
   (t nil)))

(defun macher-agent--resolve-model-and-backend (cur-backend cur-model new-backend-spec new-model-spec)
  "Resolve `(backend . model)' from buffer and spec parameters.
Uses CUR-BACKEND and CUR-MODEL as baseline, applying NEW-BACKEND-SPEC
and NEW-MODEL-SPEC when present."
  (let* ((resolved-cur-backend (macher-agent--resolve-backend cur-backend))
         (explicit-backend (macher-agent--resolve-backend new-backend-spec)))
    (cond
     (explicit-backend
      (let* ((resolved-model
              (cond
               (new-model-spec
                (let ((match (macher-agent--find-model-in-backend explicit-backend new-model-spec)))
                  (if (and match (not (symbolp new-model-spec)))
                      (cdr match)
                    new-model-spec)))
               ((and (gptel-backend-models explicit-backend)
                     (or (null cur-model)
                         (not (macher-agent--find-model-in-backend explicit-backend cur-model))))
                (let ((first-m (car (gptel-backend-models explicit-backend))))
                  (if (consp first-m) (car first-m) first-m)))
               (t cur-model))))
        (cons explicit-backend resolved-model)))
     (new-model-spec
      (let* ((cur-supports (and resolved-cur-backend
                                (macher-agent--find-model-in-backend resolved-cur-backend new-model-spec)))
             (found (unless cur-supports
                      (macher-agent-resolve-backend-and-model new-model-spec)))
             (resolved-backend (or (car found) resolved-cur-backend cur-backend))
             (resolved-model (if (symbolp new-model-spec)
                                 new-model-spec
                               (or (cdr cur-supports) (cdr found) new-model-spec))))
        (cons resolved-backend resolved-model)))
     (t
      (cons (or resolved-cur-backend cur-backend) cur-model)))))

(defun macher-agent--sync-gptel-known-presets (skills-alist)
  "Publish SKILLS-ALIST into buffer-local `gptel--known-presets'.
Also registers resolved tools in `gptel--known-tools'."
  (cl-check-type skills-alist list)
  (unless (local-variable-p 'gptel--known-presets)
    (setq-local gptel--known-presets
                (copy-alist (bound-and-true-p gptel--known-presets))))
  (unless (local-variable-p 'gptel-directives)
    (setq-local gptel-directives
                (copy-alist (bound-and-true-p gptel-directives))))
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
             (is-cmd (plist-get spec :is-command))
             (meta-plist nil))
        (dolist (k macher-agent--preset-meta-keys)
          (when (plist-member spec k)
            (when-let* ((val (plist-get spec k)))
              (setq meta-plist (plist-put meta-plist k val)))))
        (when (symbolp sym)
          (if meta-plist
              (put sym 'macher-agent-preset-meta meta-plist)
            (when (or (cl-some (lambda (k) (plist-member spec k))
                               macher-agent--preset-meta-keys)
                      (null (get sym 'macher-agent-preset-meta)))
              (cl-remprop sym 'macher-agent-preset-meta))))
        (unless is-cmd
          (let ((preset-plist (list :system sys-text)))
            (when-let* ((desc (plist-get spec :description)))
              (setq preset-plist (plist-put preset-plist :description desc)))
            (when-let* ((mod (plist-get spec :model)))
              (setq preset-plist (plist-put preset-plist :model mod)))
            (when-let* ((raw-backend (plist-get spec :backend))
                        (resolved-backend (macher-agent--resolve-backend raw-backend)))
              (let ((bname (and (fboundp 'gptel-backend-name)
                                (gptel-backend-name resolved-backend))))
                (setq preset-plist
                      (plist-put preset-plist :backend
                                 (if (and bname
                                          (boundp 'gptel--known-backends)
                                          (eq (alist-get bname gptel--known-backends nil nil #'equal)
                                              resolved-backend))
                                     bname
                                   resolved-backend)))))
            (let ((tool-names
                   (cl-loop for tool in resolved-tools
                            for tname = (cond
                                         ((and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
                                          (gptel-tool-name tool))
                                         ((and (stringp tool)
                                               (fboundp 'gptel-get-tool)
                                               (ignore-errors (gptel-get-tool tool)))
                                          tool)
                                         (t nil))
                            when (and tname (not (member tname acc)))
                            collect tname into acc
                            finally return acc)))
              (when tool-names
                (setq preset-plist (plist-put preset-plist :tools tool-names))))
            (setf (alist-get sym gptel--known-presets) preset-plist)
            (when sys-text
              (setf (alist-get sym gptel-directives) sys-text)))))))
  (when (fboundp 'gptel--setup-directive-menu)
    (gptel--setup-directive-menu 'gptel-system-prompt "Agent Profile")))

(defun macher-agent--register-parsed-skill (parsed skill-file context)
  "Register PARSED skill metadata from SKILL-FILE directly into CONTEXT."
  (cl-check-type parsed list)
  (cl-check-type skill-file string)
  (cl-check-type context macher-agent-context)
  (when (or (string-match-p "\\(?:\\`\\|/\\)commands/" skill-file)
            (string-match-p "/commands/" (expand-file-name skill-file)))
    (setq parsed (plist-put parsed :is-command t)))
  (let ((sym (plist-get parsed :name-sym))
        (body (plist-get parsed :system)))
    (when (and sym body)
      (let* ((desc (plist-get parsed :description))
             (model (plist-get parsed :model))
             (backend (plist-get parsed :backend))
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
                    :backend backend
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
         (ws-root (macher-agent--context-workspace-root context))
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
    (when (and skill-file
               (or (not ws-root)
                   (not (macher-agent--path-in-workspace-p skill-file ws-root))
                   (macher-agent--read-vfs-or-disk context skill-file)))
      (let ((parsed (macher-agent-parse-skill-file skill-file context)))
        (when parsed
          (macher-agent--register-parsed-skill parsed skill-file context))))))

(defun macher-agent--try-load-skill-from-path (path context)
  "Attempt to load skill from PATH into CONTEXT and log any errors."
  (cl-check-type path string)
  (cl-check-type context macher-agent-context)
  (condition-case err
      (macher-agent--load-skill-from-path path context)
    (error
     (message "Error loading path %s: %S" path err))))

(defun macher-agent-api-register-skills-in-directory (skills-dir context)
  "Scan SKILLS-DIR for skill definition files and load them into CONTEXT."
  (cl-check-type skills-dir string)
  (cl-check-type context macher-agent-context)
  (let* ((expanded-dir (file-name-as-directory (file-truename (expand-file-name skills-dir))))
         (skills-subdir (file-name-as-directory (expand-file-name "skills" expanded-dir)))
         (ws-root (macher-agent--context-workspace-root context))
         (tracked (and (functionp macher-agent-workspace-list-files-function)
                       (funcall macher-agent-workspace-list-files-function context)))
         (has-vfs-skills-subdir
          (and ws-root tracked
               (cl-some (lambda (p)
                          (and (stringp p)
                               (string-prefix-p
                                skills-subdir
                                (file-truename (expand-file-name p ws-root)))))
                        tracked)))
         (target-dir (if (or (file-directory-p skills-subdir) has-vfs-skills-subdir)
                         skills-subdir
                       expanded-dir))
         (has-vfs-target
          (and ws-root tracked
               (cl-some (lambda (p)
                          (and (stringp p)
                               (string-prefix-p
                                target-dir
                                (file-truename (expand-file-name p ws-root)))))
                        tracked))))
    (when (or (file-directory-p target-dir) has-vfs-target)
      (macher-agent--load-scripts-from-dir target-dir context)
      (let ((candidate-files (when (file-directory-p target-dir)
                               (directory-files target-dir t macher-agent--file-scan-regex))))
        (when (and ws-root tracked)
          (dolist (path tracked)
            (when-let* (((stringp path))
                        (abs-path (file-truename (expand-file-name path ws-root)))
                        ((string-prefix-p target-dir abs-path))
                        (rel (file-relative-name abs-path target-dir))
                        ((or (string-match-p "\\(?:\\`\\|/\\)SKILL\\.md\\'" abs-path)
                             (string-match-p "\\`[^/]+\\.md\\'" rel))))
              (push abs-path candidate-files))))
        (dolist (path (delete-dups candidate-files))
          (macher-agent--try-load-skill-from-path path context))))))

(defun macher-agent-initialize-commands (context)
  "Load flat markdown command files from commands directory into CONTEXT."
  (cl-check-type context macher-agent-context)
  (when-let* ((root (macher-agent--context-workspace-root context))
              (cmds-dir (file-name-as-directory (expand-file-name "commands" root))))
    (let ((cmd-files (when (file-directory-p cmds-dir)
                       (directory-files cmds-dir t "\\.md\\'"))))
      (when-let* ((tracked (and (functionp macher-agent-workspace-list-files-function)
                                (funcall macher-agent-workspace-list-files-function context))))
        (dolist (path tracked)
          (when-let* (((stringp path))
                      (abs-path (file-truename (expand-file-name path root)))
                      ((string-prefix-p cmds-dir abs-path))
                      ((string-match-p "\\.md\\'" abs-path)))
            (push abs-path cmd-files))))
      (dolist (file (delete-dups cmd-files))
        (macher-agent--load-skill-from-path file context))))
  (when (boundp 'macher-agent-skill-directories)
    (dolist (dir macher-agent-skill-directories)
      (when (and dir (file-directory-p dir))
        (let ((cmds-sibling (expand-file-name "../commands" dir))
              (cmds-child (expand-file-name "commands" dir)))
          (dolist (target-dir (list cmds-sibling cmds-child))
            (when (file-directory-p target-dir)
              (dolist (file (directory-files target-dir t "\\.md$"))
                (macher-agent--load-skill-from-path file context)))))))))

(defun macher-agent--initialize-skills-for-context (context)
  "Load, register, and synchronise skills into CONTEXT without mutating globals."
  (cl-check-type context macher-agent-context)
  (let ((prev-skills (macher-agent-context-skills context))
        (local-reg (macher-agent--local-tools-table context))
        (stale-ws-keys nil)
        (stale-ws-names nil))
    (when (hash-table-p local-reg)
      (maphash (lambda (k v)
                 (when (and (macher-tool-valid-p v)
                            (gethash v macher-agent--workspace-tool-overrides))
                   (push k stale-ws-keys)
                   (push (gptel-tool-name v) stale-ws-names)))
               local-reg)
      (dolist (k stale-ws-keys)
        (remhash k local-reg)))
    (dolist (entry prev-skills)
      (when-let* ((sym (car-safe entry))
                  ((symbolp sym)))
        (cl-remprop sym 'macher-agent-preset-meta)))
    (setf (macher-agent-context-skills context) nil)
    (let* ((ws-root (macher-agent--context-workspace-root context))
           (bundled (macher-agent--resolve-bundled-skills-dir)))
      (when (and bundled (file-directory-p bundled))
        (macher-agent-api-register-skills-in-directory bundled context))
      (when ws-root
        (let* ((tracked (and (functionp macher-agent-workspace-list-files-function)
                             (funcall macher-agent-workspace-list-files-function context)))
               (skills-dir (file-name-as-directory (expand-file-name "skills" ws-root)))
               (dot-skills-dir (file-name-as-directory (expand-file-name ".skills" ws-root)))
               (scripts-dir (file-name-as-directory (expand-file-name "scripts" ws-root)))
               (has-vfs-under
                (lambda (dir)
                  (cl-some (lambda (p)
                             (and (stringp p)
                                  (string-prefix-p
                                   dir
                                   (file-truename (expand-file-name p ws-root)))))
                           tracked)))
               (has-skills (or (file-directory-p skills-dir) (funcall has-vfs-under skills-dir)))
               (has-dot-skills (or (file-directory-p dot-skills-dir) (funcall has-vfs-under dot-skills-dir)))
               (has-root-scripts (or (file-directory-p scripts-dir) (funcall has-vfs-under scripts-dir))))
          (if (or has-skills has-dot-skills)
              (progn
                (when has-root-scripts
                  (let ((root-scripts (when (file-directory-p scripts-dir)
                                        (directory-files scripts-dir t "\\.el\\'"))))
                    (dolist (p tracked)
                      (when-let* (((stringp p))
                                  (abs-p (file-truename (expand-file-name p ws-root)))
                                  ((string-prefix-p scripts-dir abs-p))
                                  ((string-match-p "\\.el\\'" abs-p)))
                        (push abs-p root-scripts)))
                    (macher-agent--load-script-files
                     root-scripts t local-reg context)))
                (when has-skills
                  (macher-agent-api-register-skills-in-directory skills-dir context))
                (when has-dot-skills
                  (macher-agent-api-register-skills-in-directory dot-skills-dir context)))
            (when (or (file-directory-p ws-root) tracked)
              (macher-agent-api-register-skills-in-directory ws-root context)))))
      (when (boundp 'macher-agent-skill-directories)
        (dolist (dir macher-agent-skill-directories)
          (when (and dir (file-directory-p dir))
            (macher-agent-api-register-skills-in-directory dir context))))
      (macher-agent-initialize-commands context)
      (dolist (sname (delete-dups stale-ws-names))
        (let ((variants (macher-agent--tool-name-variants sname)))
          (unless (macher-agent--lookup-tool-in-table variants local-reg)
            (macher-agent--unregister-tool-from-gptel-tables (cons sname variants))
            (when-let* ((fallback (macher-agent--lookup-tool-in-table
                                   variants macher-agent-tools-registry)))
              (macher-agent--register-tool-in-gptel-tables
               fallback (or (gptel-tool-category fallback) "macher-agent"))))))
      (let ((merged (macher-agent-workspace-skills-alist context))
            (live-bufs (macher-agent--context-live-buffers context)))
        (dolist (entry prev-skills)
          (when-let* ((sym (car-safe entry))
                      ((symbolp sym))
                      ((not (assq sym merged))))
            (macher-agent--prune-skill-from-buffers sym (cons (current-buffer) live-bufs))))
        (macher-agent--sync-gptel-known-presets merged)
        (dolist (buf live-bufs)
          (when (and (buffer-live-p buf)
                     (not (eq buf (current-buffer)))
                     (or (local-variable-p 'gptel--known-presets buf)
                         (local-variable-p 'gptel-directives buf)
                         (buffer-local-value 'macher-agent--active-fsm buf)
                         (and (boundp 'gptel-mode)
                              (buffer-local-value 'gptel-mode buf))
                         (and (local-variable-p 'macher-agent--persistent-context buf)
                              (eq (buffer-local-value 'macher-agent--persistent-context buf)
                                  context))))
            (with-current-buffer buf
              (macher-agent--sync-gptel-known-presets merged))))))))

(defun macher-agent-initialize-skills (&optional context)
  "Load, register, and synchronise skills into CONTEXT or global defaults."
  (interactive)
  (if (null context)
      (with-temp-buffer
        (setq-local gptel--known-presets
                    (copy-alist (default-value 'gptel--known-presets)))
        (setq-local gptel--known-tools
                    (copy-tree (default-value 'gptel--known-tools)))
        (setq-local gptel-directives
                    (copy-alist (default-value 'gptel-directives)))
        (dolist (entry (bound-and-true-p macher-agent-global-skills-alist))
          (when-let* ((sym (car-safe entry))
                      ((symbolp sym)))
            (cl-remprop sym 'macher-agent-preset-meta)))
        (let ((bootstrap-ctx (macher-agent--make-context :tools macher-agent-tools-registry)))
          (macher-agent-initialize-skills bootstrap-ctx)
          (setq macher-agent-global-skills-alist (macher-agent-context-skills bootstrap-ctx)))
        (setq-default gptel--known-tools (copy-tree gptel--known-tools))
        (setq-default gptel--known-presets (copy-alist gptel--known-presets))
        (setq-default gptel-directives (copy-alist gptel-directives)))
    (cl-check-type context macher-agent-context)
    (let* ((cur (current-buffer))
           (cur-ctx (and (local-variable-p 'macher-agent--persistent-context cur)
                         (buffer-local-value 'macher-agent--persistent-context cur)))
           (owner (macher-agent-context-owner-buffer context))
           (origin (macher-agent-context-origin-buffer context))
           (target-buf (cl-find-if #'buffer-live-p (list owner origin)))
           (compatible-cur-p
            (or (eq cur-ctx context)
                (eq cur owner)
                (eq cur origin)
                (and (null cur-ctx) (null target-buf)))))
      (if compatible-cur-p
          (macher-agent--initialize-skills-for-context context)
        (if target-buf
            (with-current-buffer target-buf
              (macher-agent--initialize-skills-for-context context))
          (with-temp-buffer
            (macher-agent--initialize-skills-for-context context)))))))

(defun macher-agent-refresh-skills-and-tools (context)
  "Reset buffer-local gptel tables and reload skills and tools for CONTEXT."
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
  (macher-agent-initialize-skills context))

;;; Tool Normalisation

(defun macher-agent--select-resolved-tool (resolved _item)
  "Return a single-element list of RESOLVED when it is a valid tool struct."
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
  "Resolve tool ITEM using the fallback method."
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

(defun macher-agent--lookup-skill-spec (sym &optional context target-buf known strict-known)
  "Look up skill specification plist for SYM across TARGET-BUF, CONTEXT, and KNOWN.
When STRICT-KNOWN is non-nil, restrict preset table lookup to KNOWN."
  (if (and (consp sym) (keywordp (car sym)))
      sym
    (let* ((clean-sym (macher-normalise-preset-name sym))
           (buf (or target-buf (current-buffer)))
           (ctx (or (and (macher-agent-valid-context-p context) context)
                    (macher-agent-resolve-context buf))))
      (when clean-sym
        (let ((known-spec
               (if strict-known
                   (alist-get clean-sym known)
                 (or (and known (alist-get clean-sym known))
                     (if (and (bufferp buf) (buffer-live-p buf)
                              (local-variable-p 'gptel--known-presets buf))
                         (or (alist-get clean-sym (buffer-local-value 'gptel--known-presets buf))
                             (and (default-boundp 'gptel--known-presets)
                                  (alist-get clean-sym (default-value 'gptel--known-presets))))
                       (or (alist-get clean-sym (bound-and-true-p gptel--known-presets))
                           (when (fboundp 'gptel-get-preset)
                             (ignore-errors (gptel-get-preset clean-sym))))))))
              (skill-spec
               (or (and (macher-agent-valid-context-p ctx)
                        (ignore-errors
                          (alist-get clean-sym (macher-agent-workspace-skills-alist ctx))))
                   (alist-get clean-sym (bound-and-true-p macher-agent-global-skills-alist)))))
          (when (or known-spec skill-spec)
            (macher-agent--merge-preset-meta clean-sym known-spec skill-spec)))))))

(defun macher-agent--validate-activatable-skill (sym &optional context target-buf)
  "Return skill spec for SYM in CONTEXT and TARGET-BUF, signaling if invalid."
  (let ((spec (macher-agent--lookup-skill-spec sym context target-buf)))
    (unless spec
      (error "Skill '%s' not found in known skills" sym))
    (when (plist-get spec :is-command)
      (error "Command '%s' is not an activatable skill" sym))
    spec))

(defun macher-agent--set-active-boot-directive (boot-directive)
  "Set buffer-local boot directive and one-turn force flag from BOOT-DIRECTIVE."
  (setq-local macher-agent--boot-directive boot-directive)
  (setq-local macher-agent--force-boot-directive
              (and (stringp boot-directive)
                   (not (string-empty-p (string-trim boot-directive)))
                   t)))

(defun macher-agent--primary-preset-symbol (presets &optional context target-buf single-lead-only)
  "Return primary or last `:exclusive' preset symbol in PRESETS.
Inspects CONTEXT and TARGET-BUF; when SINGLE-LEAD-ONLY is non-nil,
only defaults to the first preset when PRESETS has length 1."
  (when presets
    (let ((lead (when (or (not single-lead-only) (= (length presets) 1))
                  (macher-normalise-preset-name (car presets)))))
      (dolist (p presets lead)
        (when-let* ((psym (macher-normalise-preset-name p))
                    (pspec (macher-agent--lookup-skill-spec psym context target-buf))
                    ((plist-get pspec :exclusive)))
          (setq lead psym))))))

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
             (clean-str (replace-regexp-in-string "^@+" "" str)))
        (if-let* ((spec (macher-agent--lookup-skill-spec clean-str nil nil known)))
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
                       (list :append (list canonical-tool))))
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
  "Merge model and backend parameters in ITEM into STATE."
  (pcase item
    (`(preset ,_sym ,spec)
     (let* ((st state)
            (cur-backend (plist-get st :backend))
            (cur-model (plist-get st :model))
            (new-model (plist-get spec :model))
            (new-backend (plist-get spec :backend)))
       (when (or new-model new-backend)
         (pcase-let ((`(,resolved-backend . ,resolved-model)
                      (macher-agent--resolve-model-and-backend
                       cur-backend cur-model new-backend new-model)))
           (when (or resolved-backend (plist-member st :backend))
             (setq st (plist-put st :backend resolved-backend)))
           (when (or resolved-model (plist-member st :model))
             (setq st (plist-put st :model resolved-model)))))
       (dolist (key '(:temperature :max-tokens))
         (when-let* ((val (plist-get spec key)))
           (setq st (plist-put st key val))))
       st))
    (_ state)))

(defun macher-agent--flatten-preset-dependencies (inline-presets known)
  "Flatten INLINE-PRESETS and parent dependencies using KNOWN."
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
    (when-let* ((backend-val (plist-get payload :backend))
                (resolved-backend (macher-agent--resolve-backend backend-val)))
      (setq-local gptel-backend resolved-backend))
    (when (plist-member payload :temperature)
      (setq-local gptel-temperature (plist-get payload :temperature)))
    (when (plist-member payload :max-tokens)
      (setq-local gptel-max-tokens (plist-get payload :max-tokens)))
    (when (plist-member payload :tools)
      (setq-local gptel-tools (plist-get payload :tools)))
    (when (plist-member payload :ptc-primitives)
      (setq-local macher-agent--active-ptc-primitives (plist-get payload :ptc-primitives)))
    (when (plist-member payload :boot-directive)
      (setq-local macher-agent--boot-directive (plist-get payload :boot-directive)))))

(defun macher-agent--preset-matches-system-p (spec parsed-sys &optional sym)
  "Return non-nil when preset SPEC matches PARSED-SYS for optional SYM."
  (let ((full-spec (if (and sym spec) (macher-agent--merge-preset-meta sym spec) spec)))
    (when (and (listp full-spec)
               (not (plist-get full-spec :is-command))
               (stringp parsed-sys)
               (not (string-empty-p parsed-sys)))
      (let ((sys (or (plist-get full-spec :system)
                     (plist-get full-spec :system-message))))
        (cond
         ((stringp sys)
          (equal sys parsed-sys))
         ((and sys (symbolp sys) (not (functionp sys)))
          (equal (alist-get sys (bound-and-true-p gptel-directives)) parsed-sys))
         ((and (consp sys) (not (keywordp (car sys))))
          (equal (car-safe (gptel--parse-directive sys)) parsed-sys))
         (t nil))))))

(defun macher-agent--sync-preset-before-save ()
  "Synchronise `gptel--preset' with the active skill prompt prior to saving."
  (let* ((parsed-sys (car-safe (gptel--parse-directive gptel-system-prompt)))
         (cur-sym (bound-and-true-p gptel--preset))
         (cur-spec (and cur-sym
                        (fboundp 'gptel-get-preset)
                        (ignore-errors (gptel-get-preset cur-sym)))))
    (unless (and cur-spec (macher-agent--preset-matches-system-p cur-spec parsed-sys cur-sym))
      (let* ((lead-sym (car-safe (bound-and-true-p macher-agent-presets)))
             (lead-spec (and (symbolp lead-sym)
                             (fboundp 'gptel-get-preset)
                             (ignore-errors (gptel-get-preset lead-sym)))))
        (cond
         ((and lead-spec (macher-agent--preset-matches-system-p lead-spec parsed-sys lead-sym))
          (setq-local gptel--preset lead-sym))
         ((and (stringp parsed-sys)
               (not (string-empty-p parsed-sys))
               (bound-and-true-p gptel--known-presets))
          (when-let* ((match (cl-find-if
                              (lambda (entry)
                                (and (symbolp (car entry))
                                     (macher-agent--preset-matches-system-p
                                      (cdr entry) parsed-sys (car entry))))
                              gptel--known-presets)))
            (setq-local gptel--preset (car match)))))))))

(defun macher-agent--persist-missing-preset-tools ()
  "Persist active `gptel-tools' on save when `gptel--preset' omits `:tools'.
Upstream `gptel--preset-mismatch-value' evaluates to nil whenever a preset
spec lacks `:tools', causing `gptel--save-state' to drop active tools."
  (when (and (bound-and-true-p gptel-mode)
             (bound-and-true-p gptel--preset)
             (bound-and-true-p gptel-tools))
    (let ((preset-spec (and (fboundp 'gptel-get-preset)
                            (ignore-errors (gptel-get-preset gptel--preset)))))
      (when (and preset-spec
                 (null (plist-get preset-spec :tools)))
        (let ((tool-names
               (cl-loop for tl in gptel-tools
                        when (and (fboundp 'gptel-tool-p) (gptel-tool-p tl))
                        collect (gptel-tool-name tl))))
          (when tool-names
            (if (derived-mode-p 'org-mode)
                (org-with-wide-buffer
                 (org-entry-put (point-min) "GPTEL_TOOLS"
                                (string-join tool-names " ")))
              (let ((print-escape-newlines t))
                (save-excursion
                  (save-restriction
                    (add-file-local-variable 'gptel--tool-names tool-names)))))))))))

(defun macher-agent--apply-preset (preset-or-presets)
  "Apply validated PRESET-OR-PRESETS list to buffer-local variables."
  (let* ((presets (if (listp preset-or-presets) preset-or-presets (list preset-or-presets)))
         (base-state (list :model gptel-model
                           :backend (bound-and-true-p gptel-backend)
                           :system (bound-and-true-p gptel-system-prompt)
                           :temperature (bound-and-true-p gptel-temperature)
                           :max-tokens (bound-and-true-p gptel-max-tokens)
                           :tools gptel-tools
                           :ptc-primitives (bound-and-true-p macher-agent--active-ptc-primitives)
                           :known-presets (bound-and-true-p gptel--known-presets)))
         (payload (macher-agent-compose-payload base-state presets)))
    (when presets
      (let* ((known (plist-get base-state :known-presets))
             (primary-sym (macher-agent--primary-preset-symbol presets))
             (spec (when primary-sym
                     (macher-agent--lookup-skill-spec primary-sym)))
             (raw-sys (or (plist-get spec :system) (plist-get spec :system-message)))
             (boot-dir (plist-get spec :boot-directive)))
        (when (and primary-sym
                   (not (plist-get spec :is-command))
                   (or (alist-get primary-sym known)
                       (and (fboundp 'gptel-get-preset)
                            (ignore-errors (gptel-get-preset primary-sym)))))
          (setq-local gptel--preset primary-sym))
        (when (stringp raw-sys)
          (setq payload (plist-put payload :system raw-sys)))
        (when (and boot-dir (not (plist-member payload :boot-directive)))
          (setq payload (plist-put payload :boot-directive boot-dir)))))
    (macher-agent--apply-payload-locally payload)
    payload))

(defun macher-agent-use-skill (skill &optional buf)
  "Attach SKILL to live BUF, validating that it is a non-command skill."
  (interactive "sSkill: ")
  (let* ((target-buf (or buf (current-buffer)))
         (raw-list (ensure-list skill))
         (skill-list (delq nil (mapcar #'macher-normalise-preset-name raw-list))))
    (cl-check-type target-buf buffer)
    (when (buffer-live-p target-buf)
      (with-current-buffer target-buf
        (let ((ctx (macher-agent-resolve-context target-buf))
              (has-exclusive nil))
          (dolist (sym skill-list)
            (let ((spec (macher-agent--validate-activatable-skill sym ctx target-buf)))
              (when (plist-get spec :exclusive)
                (setq has-exclusive t))))
          (when has-exclusive
            (setq-local gptel-tools nil)
            (setq-local macher-agent--active-ptc-primitives nil))
          (setq-local macher-agent-presets skill-list)
          (let ((payload (macher-agent--apply-preset skill-list)))
            (macher-agent--set-active-boot-directive
             (plist-get payload :boot-directive)))
          (when (functionp macher-agent-mid-turn-recompile-function)
            (funcall macher-agent-mid-turn-recompile-function target-buf ctx)))))))

(provide 'macher-agent-presets)
;;; macher-agent-presets.el ends here
