;;; macher-agent-api.el --- Public API for macher-agent -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'org-macro)
(require 'macher-agent-vfs-client)
(require 'macher-agent-macher-bridge)
(require 'macher-agent-orchestration)
(require 'macher-agent-gptel-tools)
(require 'macher-agent-sandbox)

(defcustom macher-agent-skill-directories nil
  "List of user-defined directories to scan for SKILL.md files."
  :type '(repeat string)
  :group 'macher-agent)

(defvar macher-agent--bundled-skills-dir
  (expand-file-name "skills" (file-name-directory (or load-file-name buffer-file-name)))
  "Internal path to bundled skills.")

(defvar macher-agent-tools-registry (make-hash-table :test 'equal)
  "Global registry for all loaded macher-agent tools.")

(defvar macher-agent-global-skills-alist nil
  "Global registry for all loaded macher-agent skills metadata (presets/tools).")

(defvar-local macher-agent-parent-buffer nil
  "Stores the name of the buffer this chat branched from.")

(defvar-local macher-agent--active-skill-sym nil
  "Symbol representing the currently active skill preset in this buffer.")

(defmacro macher-agent-with-project-root (&rest body)
  "Execute BODY with `default-directory` strictly bound to the absolute project root."
  `(let ((default-directory (file-name-as-directory (macher-agent-root))))
     ,@body))

(defun macher-agent-workspace-resolve-path (path)
  "Resolve PATH into an absolute buffer name.

PATH is the relative or absolute path string.

Return the resolved absolute buffer name string."
  (macher-agent--resolve-buffer-name path))

(defun macher-agent-context-read (context file)
  "Read FILE from CONTEXT.

CONTEXT is the active context structure.
FILE is the relative file path string.

Return the file content string, or nil."
  (macher-agent--read-context-file context file))

(defun macher-agent-context-update (context file content)
  "Update FILE in CONTEXT with CONTENT.

CONTEXT is the active context structure.
FILE is the relative file path string.
CONTENT is the new string content.

Return nil."
  (macher-agent--update-context-file context file content))

(defun macher-agent-scope-add-file (buffer-name context)
  "Add BUFFER-NAME to the authorised scope in CONTEXT.

BUFFER-NAME is the name of the buffer (string).
CONTEXT is the active context structure.

Return nil."
  (macher-agent--add-buffer-to-scope-headless buffer-name context))

(defun macher-agent-prepare-instructions (buf instructions preset)
  "Prepare and inject sub-agent INSTRUCTIONS into BUF with PRESET.

BUF is the target sub-agent buffer.
INSTRUCTIONS represents the directive string.
PRESET is the preset symbol or string representing requested skills.

Return nil."
  (macher-agent--prepare-subagent-instructions buf instructions preset))

(defun macher-agent-submit-task-result (result)
  "Submit the final RESULT for the current agent task.

RESULT is the final synthesised answer string.

Return nil."
  (setq-local macher-agent--final-result result)
  (when (boundp 'macher-agent--parent-callback)
    (funcall macher-agent--parent-callback
             (make-macher-agent-lisp-result-response
              :status 'success
              :data result
              :buffer-name (buffer-name)))
    (makunbound 'macher-agent--parent-callback)))

(defun macher-agent-ui-show (&optional buf)
  "Show the user interface for BUF.

BUF is the optional buffer to display.

Return nil."
  (macher-agent--show-ui buf))

(defun macher-agent-workspace-root (workspace)
  "Retrieve the project root directory of WORKSPACE.

WORKSPACE is the workspace object.

Return the project root path string, or nil."
  (if (and (fboundp 'macher-agent-workspace-p) (macher-agent-workspace-p workspace))
      (macher-agent-workspace-project-root workspace)
    (macher--workspace-root workspace)))

(defun macher-context-workspace-root (context)
  "Navigate the CONTEXT struct to retrieve the root directory.

CONTEXT is the active context structure.

Return the project root path string, or nil."
  (let ((workspace (when context (macher-agent--get-context-workspace context))))
    (when workspace (macher-agent-root workspace))))

(defun macher-normalise-preset-name (preset)
  "Remove leading character symbols and convert PRESET to a uniform symbol.

PRESET is the preset string or symbol.

Return the normalised preset symbol, or nil."
  (when (and preset (or (symbolp preset) (stringp preset)))
    (let* ((raw-str (if (symbolp preset) (symbol-name preset) preset))
           (clean-str (replace-regexp-in-string "^@+" "" raw-str)))
      (intern clean-str))))

(defun macher-tool-valid-p (tool)
  "Check if TOOL is a valid struct.

TOOL is the tool object to verify.

Return non-nil if valid, otherwise nil."
  (and tool (fboundp 'gptel-tool-p) (gptel-tool-p tool)))

(defsubst macher-agent-canonical-tool-name (tool)
  "Strictly extract and coerce TOOL into a string name.
Handles gptel structs, symbols, plists, and raw strings.

TOOL is the tool representation to convert.

Return the canonical tool name string, or nil."
  (and tool
       (let ((raw-name
              (cond
               ((stringp tool) tool)
               ((and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
                (gptel-tool-name tool))
               ((symbolp tool) (symbol-name tool))
               ((and (listp tool) (plist-get tool :name))
                (plist-get tool :name))
               ((and (listp tool) (plist-get tool :function))
                (let ((fn (plist-get tool :function)))
                  (if (listp fn) (plist-get fn :name) fn)))
               (t (format "%s" tool)))))
         (if (symbolp raw-name) (symbol-name raw-name) raw-name))))

(defun macher-agent--cache-tool (tool registry)
  "Cache the TOOL in REGISTRY using its canonical string name.

TOOL is the tool object to cache.
REGISTRY is the hash-table representing the registry.

Return nil."
  (when (macher-tool-valid-p tool)
    (let ((canonical-name (macher-agent-canonical-tool-name tool)))
      (when canonical-name
        (puthash canonical-name tool registry)))))

(defun macher-agent-force-review ()
  "Manually trigger the diff review screen for any pending virtual edits.

Return nil."
  (interactive)
  (let ((context (macher-agent-resolve-context))
        (fsm (macher-agent--get-fsm-latest)))
    (if (not (and context (macher-agent--get-context-dirty-p context)))
        (message "No pending edits to review.")
      (macher--build-patch context fsm)
      (message "SUCCESS: Patch review screen(s) generated for pending edits."))))

(defun macher-agent--parse-frontmatter (text)
  "Parse a YAML frontmatter TEXT block into a hash table.

TEXT is the YAML frontmatter string block.

Return a hash table of key-value pairs."
  (let ((ht (make-hash-table :test 'equal))
        (lines (split-string text "\n"))
        (current-list-key nil)
        (current-list-val nil))
    (dolist (line lines)
      (cond
       ((and current-list-key (string-match "^[ \t]*-[ \t]+\"?\\([^\"]+\\)\"?" line))
        (push (string-trim (match-string 1 line)) current-list-val))

       ((string-match "^\\([A-Za-z0-9_-]+\\):[ \t]*\\(.*\\)$" line)
        (when current-list-key
          (puthash current-list-key (nreverse current-list-val) ht)
          (setq current-list-key nil
                current-list-val nil))

        (let* ((key (match-string 1 line))
               (raw-val (string-trim (match-string 2 line))))
          (cond
           ((string-match "^\\[\\(.*\\)\\]$" raw-val)
            (let* ((inner (match-string 1 raw-val))
                   (items (split-string inner "[, \t\n\r\"]+" t)))
              (puthash key items ht)))

           ((string-empty-p raw-val)
            (setq current-list-key key
                  current-list-val nil))

           (t
            (let ((clean-val (replace-regexp-in-string "\\`['\"]\\|['\"]\\'" "" raw-val)))
              (cond
               ((member (downcase clean-val) '("true" "yes" "1")) (puthash key t ht))
               ((member (downcase clean-val) '("false" "no" "0")) (puthash key nil ht))
               (t (puthash key clean-val ht))))))))))

    (when current-list-key
      (puthash current-list-key (nreverse current-list-val) ht))
    ht))

(defun macher-agent-parse-skill-file (filepath)
  "Parse a SKILL.md file at FILEPATH extracting frontmatter and body.

FILEPATH is the path to the skill file (string).

Return a property list containing skill properties."
  (with-temp-buffer
    (let* ((org-inhibit-startup t)
           (abs-file (expand-file-name filepath)))
      (setq default-directory (file-name-directory abs-file))
      (insert-file-contents abs-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (org-mode)
      (setq-local org-element-use-cache nil)
      (org-macro-initialize-templates)
      (org-macro-replace-all org-macro-templates)
      (goto-char (point-min))

      (let ((fm-hash (make-hash-table :test 'equal))
            body name has-tools)

        (when-let* (((re-search-forward "^---\n" nil t))
                    (start (point))
                    ((re-search-forward "^---\n" nil t))
                    (frontmatter (buffer-substring-no-properties start (match-beginning 0))))
          (setq fm-hash (macher-agent--parse-frontmatter frontmatter)))

        (setq body (string-trim (buffer-substring-no-properties (point) (point-max))))
        (setq name (gethash "name" fm-hash))

        (setq has-tools (not (eq (gethash "allowed-tools" fm-hash 'missing) 'missing)))

        (list :name name
              :name-sym (when name (intern name))
              :description (gethash "description" fm-hash)
              :model (gethash "model" fm-hash)
              :has-tools has-tools
              :allowed-tools (gethash "allowed-tools" fm-hash)
              :exclusive (gethash "exclusive" fm-hash)
              :body body)))))

(defun macher-agent--secure-ast-p (form)
  "Return t if FORM is a secure AST node (no top-level definitions).

FORM is the expression to inspect.

Return t if secure, otherwise nil."
  (not (and (consp form)
            (memq (car form) '(defun cl-defun defvar defcustom defmacro)))))

(defun macher-agent--parse-safe-forms (content &optional validation-cb)
  "Parse CONTENT and return a list of trusted forms.
If VALIDATION-CB is provided, it is called on each form; if it returns nil, an error is signaled.

CONTENT is the string block to parse.
VALIDATION-CB is an optional validation function.

Return a list of parsed forms."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when validation-cb
                (unless (funcall validation-cb form)
                  (error "Validation failed for form: %S" form)))
              (push form forms)))
        (end-of-file nil))
      (nreverse forms))))

(defun macher-agent--evaluate-and-cache-tool (content tool-name registry)
  "Evaluate string CONTENT and cache it in REGISTRY under TOOL-NAME.

CONTENT is the string representation of the tool code.
TOOL-NAME is the string name of the tool.
REGISTRY is the hash-table representing the tools registry.

Return the cached tool object, or TOOL-NAME if evaluation fails."
  (let ((tool nil))
    (condition-case err
        (let* ((forms (macher-agent--parse-safe-forms content
                                                      (lambda (f)
                                                        (if (macher-agent--secure-ast-p f)
                                                            t
                                                          (error "SECURITY WARNING: Tool attempts to evaluate top-level definition: %s" (car f))))))
               (val (progn
                      (let ((res nil))
                        (dolist (form forms res)
                          (setq res (eval form t)))))))
          (setq tool (if (and (symbolp val) (boundp val))
                         (symbol-value val)
                       val)))
      (error
       (message "Macher-Agent: Failed to load tool %s - %s" tool-name err)))
    (when tool
      (macher-agent--cache-tool tool registry))
    tool))

(defun macher-agent--read-and-cache-from-disk (tool-name script-paths registry)
  "Load and cache a tool from physical disk using SCRIPT-PATHS.

TOOL-NAME is the string name of the tool.
SCRIPT-PATHS is a list of candidate files on the physical disk.
REGISTRY is the hash-table representing the tools registry.

Return the cached tool object, or TOOL-NAME if physical loading fails."
  (let ((disk-content nil))
    (catch 'found-disk
      (dolist (path script-paths)
        (when (file-exists-p path)
          (with-temp-buffer
            (insert-file-contents path)
            (setq disk-content (buffer-string))
            (throw 'found-disk t)))))
    (if disk-content
        (macher-agent--evaluate-and-cache-tool disk-content tool-name registry)
      tool-name)))

(defun macher-agent--is-managed-path-p (path)
  "Evaluate if PATH resides within managed skill or script directories.

PATH is the string file path to inspect.

Return non-nil if managed, otherwise nil."
  (and path
       (stringp path)
       (or (string-match-p "SKILL\\.md$" path)
           (string-match-p "skills/.*\\.el$" path)
           (string-match-p "scripts/.*\\.el$" path))))

(defun macher-agent--mutation-dispatcher (&optional path &rest _)
  "Route file system events to appropriate invalidation or reload handlers.

PATH is the optional mutated file path string.
_ represents unused extra hook arguments.

Return nil."
  (when (macher-agent--is-managed-path-p path)
    (let* ((workspace (when (bound-and-true-p macher-agent--persistent-context)
                        (macher-agent--get-context-workspace macher-agent--persistent-context))))

      (when (string-match "scripts/\\([^/]+\\)\\.el$" path)
        (let* ((tool-name (match-string 1 path))
               (registry (if workspace
                             (macher-agent-workspace-tools-registry workspace)
                           macher-agent-tools-registry))
               (canonical-name (macher-agent-canonical-tool-name tool-name)))
          (remhash canonical-name registry)
          (remhash tool-name registry)
          (remhash (intern tool-name) registry)))

      (when (and macher-agent--persistent-context workspace)
        (let ((skills-dir (expand-file-name "skills" (macher-agent-root workspace))))
          (when (and skills-dir (file-directory-p skills-dir))
            (macher-agent-initialize-skills macher-agent--persistent-context skills-dir)))))))

(add-hook 'macher-agent-context-mutated-hook #'macher-agent--mutation-dispatcher)

(defun macher-agent--locate-tool-source (tool-name context dir-context script-paths workspace)
  "Locate the tool source content for TOOL-NAME.

TOOL-NAME is the string name of the tool.
CONTEXT is the active context structure.
DIR-CONTEXT is the active directory context string.
SCRIPT-PATHS is a list of script candidate paths.
WORKSPACE is the active workspace struct.

Return the tool source content string, or nil."
  (let ((vfs-content (when context
                       (let* ((vfs-path (when dir-context (expand-file-name (format "scripts/%s.el" tool-name) dir-context)))
                              (workspace-root (macher-context-workspace-root context))
                              (rel-to-workspace (when (and workspace-root vfs-path)
                                                  (file-relative-name vfs-path workspace-root)))
                              (rel-to-dir (when (and dir-context vfs-path)
                                            (file-relative-name vfs-path dir-context)))
                              (visiting-buf (when vfs-path (find-buffer-visiting vfs-path)))
                              (visiting-buf-name (when visiting-buf (buffer-name visiting-buf)))
                              (std-name (format "scripts/%s.el" tool-name))
                              (base-name (format "%s.el" tool-name))
                              (candidates (delq nil (list vfs-path rel-to-workspace rel-to-dir visiting-buf-name std-name base-name)))
                              (found-content nil))
                         (cl-loop for cand in candidates
                                  until found-content
                                  do (setq found-content (ignore-errors (macher-agent--read-context-file context cand))))
                         found-content))))
    (if vfs-content
        vfs-content
      (let ((disk-content nil))
        (catch 'found
          (dolist (path script-paths)
            (when (file-exists-p path)
              (with-temp-buffer
                (insert-file-contents path)
                (setq disk-content (buffer-string))
                (throw 'found t)))))
        disk-content))))

(defun macher-agent--parse-and-validate-tool-ast (content tool-name)
  "Parse CONTENT and validate the AST. Returns a list of trusted forms.

CONTENT is the tool script content string.
TOOL-NAME is the string name of the tool.

Return a list of validated trusted forms, or nil."
  (condition-case err
      (macher-agent--parse-safe-forms content
                                      (lambda (f)
                                        (if (macher-agent--secure-ast-p f)
                                            t
                                          (message "Macher-Agent SECURITY WARNING: Skipped tool '%s' because it attempts to evaluate top-level definition: %s" tool-name (car f))
                                          nil)))
    (error
     (message "Macher-Agent: Failed to parse tool %s - %s" tool-name err)
     nil)))

(defun macher-agent--evaluate-trusted-tool-ast (forms tool-name)
  "Evaluate trusted FORMS and return the captured tool struct.

FORMS is the list of validated tool Lisp expressions.
TOOL-NAME is the string name of the tool.

Return the evaluated tool struct, or nil."
  (let ((captured-tool nil))
    (condition-case err
        (dolist (form forms)
          (let ((res (eval form t)))
            (cond
             ((macher-tool-valid-p res)
              (setq captured-tool res))
             ((and (symbolp res) (boundp res)
                   (macher-tool-valid-p (symbol-value res)))
              (setq captured-tool (symbol-value res))))))
      (error
       (message "Macher-Agent: Failed to evaluate tool %s - %s" tool-name err)
       (setq captured-tool nil)))
    captured-tool))

(defun macher-agent-resolve-tool (tool-name context dir-context)
  "Retrieve TOOL-NAME from workspace registry or load from VFS/disk, deferring native tools.

TOOL-NAME is the name of the tool (string or symbol).
CONTEXT is the active context structure.
DIR-CONTEXT is the active directory context string.

Return the resolved tool object, or TOOL-NAME if unresolved."
  (let* ((workspace (when context (macher-agent--get-context-workspace context)))
         (registry (if workspace (macher-agent-workspace-tools-registry workspace) macher-agent-tools-registry))
         (canonical-name (macher-agent-canonical-tool-name tool-name))
         (cached (and canonical-name (gethash canonical-name registry)))
         (script-paths (when canonical-name
                         (delq nil (list
                                    (when dir-context (expand-file-name (format "scripts/%s.el" canonical-name) dir-context))
                                    (expand-file-name (format "scripts/%s.el" canonical-name) (or (bound-and-true-p macher-agent--bundled-skills-dir) macher-agent-bundled-skills-directory))))))
         (loaded-tool nil))

    (if cached
        (setq loaded-tool cached)
      (when canonical-name
        (let ((content-to-eval (macher-agent--locate-tool-source canonical-name context dir-context script-paths workspace)))
          (when content-to-eval
            (let ((trusted-forms (macher-agent--parse-and-validate-tool-ast content-to-eval canonical-name)))
              (when trusted-forms
                (setq loaded-tool (macher-agent--evaluate-trusted-tool-ast trusted-forms canonical-name))))))))

    (unless loaded-tool
      (let* ((tool-sym (if (symbolp tool-name) tool-name (intern-soft tool-name)))
             (sym-val (when (and tool-sym (boundp tool-sym)) (symbol-value tool-sym))))
        (if (and sym-val (macher-tool-valid-p sym-val))
            (setq loaded-tool sym-val)
          (when canonical-name
            (setq loaded-tool
                  (ignore-errors
                    (gptel-get-tool canonical-name)))))))

    (when loaded-tool
      (macher-agent--cache-tool loaded-tool registry))

    (or loaded-tool tool-name)))

(defun macher-agent--load-scripts-from-dir (skills-dir context)
  "Load script tools from the scripts subdirectory of SKILLS-DIR.

SKILLS-DIR is the path to the skills directory (string).
CONTEXT is the active context structure.

Return nil."
  (let ((scripts-dir (expand-file-name "scripts" skills-dir)))
    (when (file-directory-p scripts-dir)
      (dolist (script (directory-files scripts-dir t "\\.el$"))
        (let* ((base (file-name-base script))
               (tool (macher-agent-resolve-tool base context skills-dir)))
          (ignore tool))))))

(defun macher-agent--load-skill-from-path (path &optional context)
  "Load a skill from PATH and register it natively with gptel.

PATH is the path to the skill directory or file (string).
CONTEXT is the optional active context structure.

Return nil."
  (let ((skill-file (cond
                     ((and (file-directory-p path)
                           (file-exists-p (expand-file-name "SKILL.md" path)))
                      (expand-file-name "SKILL.md" path))
                     ((and (file-regular-p path)
                           (string-match-p "\\.md$" path))
                      path)
                     (t nil))))
    (when skill-file
      (let* ((parsed (macher-agent-parse-skill-file skill-file))
             (sym (plist-get parsed :name-sym))
             (body (plist-get parsed :body))
             (desc (plist-get parsed :description))
             (model (plist-get parsed :model))
             (tool-names (plist-get parsed :allowed-tools))
             (exclusive (plist-get parsed :exclusive))
             (workspace (when context (macher-agent--get-context-workspace context)))
             (skill-base-dir (file-name-directory skill-file)))

        (when (and sym body)
          (let* ((alist (if workspace
                            (macher-agent-workspace-skills-alist workspace)
                          macher-agent-global-skills-alist))
                 (resolved-tools (when tool-names
                                   (delq nil (mapcar (lambda (tname)
                                                       (macher-agent-resolve-tool tname context skill-base-dir))
                                                     tool-names)))))
            (setf (alist-get sym alist)
                  (list :system body :description desc :model (when model (intern model)) :tools resolved-tools :exclusive exclusive))
            (if workspace
                (setf (macher-agent-workspace-skills-alist workspace) alist)
              (setq macher-agent-global-skills-alist alist))))))))

(defun macher-agent-api-register-skills-in-directory (skills-dir &optional context)
  "Scan SKILLS-DIR for SKILL.md files and load them.

SKILLS-DIR is the path to the directory (string).
CONTEXT is the optional active context structure.

Return nil."
  (let* ((expanded-dir (file-name-as-directory (expand-file-name skills-dir)))
         (target-dir (if (file-directory-p (expand-file-name "skills" expanded-dir))
                         (file-name-as-directory (expand-file-name "skills" expanded-dir))
                       expanded-dir)))
    (when (and target-dir (file-directory-p target-dir))
      (macher-agent--load-scripts-from-dir target-dir context)
      (let ((files (directory-files target-dir t "^[^.]")))
        (dolist (path files)
          (condition-case err
              (macher-agent--load-skill-from-path path context)
            (error
             (message "Error loading path %s: %S" path err))))))))

(defun macher-agent--get-system-message-name (sys-msg)
  "Reverse lookup SYS-MSG to find its short name in local or global skills.

SYS-MSG is the system message prompt string.

Return the short name string, or nil if not found."
  (when (and sys-msg (stringp sys-msg) (not (string-empty-p sys-msg)))
    (let* ((ctx (ignore-errors (macher-agent-resolve-context)))
           (ws (when ctx (macher-agent--get-context-workspace ctx)))
           (ws-skills (when ws (macher-agent-workspace-skills-alist ws)))
           (global-skills macher-agent-global-skills-alist))
      (or
       (cl-loop for (sym . meta) in ws-skills
                if (equal (or (plist-get meta :system) "") sys-msg)
                return (symbol-name sym))
       (cl-loop for (sym . meta) in global-skills
                if (equal (or (plist-get meta :system) "") sys-msg)
                return (symbol-name sym))
       (cl-loop for (sym . msg) in (bound-and-true-p gptel-directives)
                for prompt = (if (stringp msg) msg (plist-get msg :system))
                if (equal prompt sys-msg)
                return (symbol-name sym))))))

(defun macher-agent-initialize-skills (&optional context dir)
  "Initialise agent skills from all registered directories.

CONTEXT is the optional active context structure.
DIR is the optional directory path string to load from.

Return nil."
  (interactive)
  (let ((directories (delq nil (append
                                (list dir macher-agent--bundled-skills-dir)
                                (if (listp macher-agent-skill-directories)
                                    macher-agent-skill-directories
                                  (list macher-agent-skill-directories))))))

    (cl-loop for d in (delete-dups directories)
             do (when (file-directory-p d)
                  (macher-agent-api-register-skills-in-directory d context)))

    (let* ((workspace (when context (macher-agent--get-context-workspace context)))
           (ws-skills (when workspace (macher-agent-workspace-skills-alist workspace)))
           (global-skills macher-agent-global-skills-alist)
           (merged-skills (cl-remove-duplicates (append ws-skills global-skills)
                                                :key #'car
                                                :test #'eq
                                                :from-end t)))

      (unless (local-variable-p 'gptel-directives)
        (setq-local gptel-directives (copy-tree (default-value 'gptel-directives))))

      (unless (local-variable-p 'gptel--known-presets)
        (setq-local gptel--known-presets (copy-tree (default-value 'gptel--known-presets))))

      (cl-loop for (sym . meta) in merged-skills
               for system-prompt = (plist-get meta :system)
               for desc = (plist-get meta :description)
               for model = (plist-get meta :model)
               for tools = (plist-get meta :tools)
               for exclusive = (plist-get meta :exclusive)
               for tool-names = (mapcar #'macher-agent-canonical-tool-name tools)
               do (when system-prompt
                    (setf (alist-get sym gptel-directives) system-prompt)
                    (let ((preset-spec (list :description (or desc (format "Agent Profile: %s" sym))
                                             :system system-prompt)))
                      (when exclusive (setq preset-spec (plist-put preset-spec :exclusive t)))
                      (when model (setq preset-spec (plist-put preset-spec :model model)))
                      (when tool-names
                        (setq preset-spec (plist-put preset-spec :tools
                                                     (if exclusive tool-names `(:append ,tool-names)))))
                      (setf (alist-get sym gptel--known-presets) preset-spec))))))

  (when (fboundp 'gptel--setup-directive-menu)
    (gptel--setup-directive-menu 'gptel-system-prompt "Agent Profile")))

(defun macher-agent--find-native-tool (tool-name)
  "Find a gptel-tool registered natively via `gptel-make-tool`.

TOOL-NAME is the name of the tool (string or symbol).

Return the gptel-tool struct if found, otherwise nil."
  (let* ((t-str (if (symbolp tool-name) (symbol-name tool-name) tool-name))
         (normalized-target (replace-regexp-in-string "-" "_" t-str))
         (found nil))

    (dolist (t_ (append (default-value 'gptel-tools) (bound-and-true-p gptel-tools)))
      (when (and (not found) (macher-tool-valid-p t_))
        (let ((t-name (macher-agent-canonical-tool-name t_)))
          (when (and t-name (equal (replace-regexp-in-string "-" "_" t-name) normalized-target))
            (setq found t_)))))

    (unless found
      (when (boundp 'gptel--known-tools)
        (cl-loop for category-alist in gptel--known-tools
                 until found
                 do (let ((tools-alist (cdr category-alist)))
                      (cl-loop for (name . tool) in tools-alist
                               until found
                               do (when (macher-tool-valid-p tool)
                                    (let ((t-name (macher-agent-canonical-tool-name tool)))
                                      (when (and t-name (equal (replace-regexp-in-string "-" "_" t-name) normalized-target))
                                        (setq found tool)))))))))
    found))

(defun macher-agent--resolve-tool-in-env (item)
  "Resolve a tool ITEM using the current workspace environment.

ITEM is the tool identifier (string, symbol, or struct).

Return a list of resolved gptel-tool structs, or nil."
  (let* ((ctx (ignore-errors (macher-agent-resolve-context)))
         (skills-dir (when ctx (macher-agent-context-root ctx)))
         (resolved (macher-agent-resolve-tool item ctx skills-dir)))
    (if (and resolved (macher-tool-valid-p resolved))
        (list resolved)
      (let ((native (macher-agent--find-native-tool item)))
        (if native
            (list native)
          (and resolved (list resolved)))))))

(cl-defgeneric macher-agent-resolve-item (item)
  "Resolve an ITEM of any type into a list of gptel-tool structs."
  nil)

(cl-defmethod macher-agent-resolve-item ((item cl-structure-object))
  (when (macher-tool-valid-p item) (list item)))

(cl-defmethod macher-agent-resolve-item ((item string))
  (macher-agent--resolve-tool-in-env item))

(cl-defmethod macher-agent-resolve-item ((item symbol))
  (if (fboundp item)
      (let ((res (ignore-errors (funcall item))))
        (if res
            (macher-agent-resolve-item res)
          (macher-agent--resolve-tool-in-env item)))
    (macher-agent--resolve-tool-in-env item)))

(cl-defmethod macher-agent-resolve-item ((item list))
  (cl-mapcan #'macher-agent-resolve-item item))

(defun macher-agent-normalize-tools (tools)
  "Normalise, resolve, and deduplicate a mixed list of TOOLS.
Accepts a list of tool representations (names, symbols, structs, functions, or nested lists),
recursively flattens them, resolves them to `gptel-tool' structs via `macher-agent-resolve-tool'
or native fallback, filters out nil, and deduplicates by name using a hash table.

TOOLS is the list of tool representations.

Return a list of unique resolved gptel-tool structures."
  (let ((flat-resolved (delq nil (macher-agent-resolve-item tools)))
        (seen (make-hash-table :test 'equal))
        (unique nil))
    (dolist (tool flat-resolved)
      (let* ((raw-name (macher-agent-canonical-tool-name tool))
             (canon-name (if (stringp raw-name) (substring-no-properties raw-name) raw-name)))
        (when (and canon-name (not (gethash canon-name seen)))
          (puthash canon-name t seen)
          (push tool unique))))
    (nreverse unique)))

(defun macher-agent-resolve-to-struct (t-item)
  "Convert a tool item into a gptel-tool struct.

T-ITEM is the tool item to convert.

Return the resolved gptel-tool structure, or nil."
  (car (macher-agent-normalize-tools t-item)))

(defun macher-agent--wrap-tool-for-project-root (tool)
  "Clone and wrap the tool so it ALWAYS runs in the project root.

TOOL is the gptel-tool struct to wrap.

Return the wrapped gptel-tool structure."
  (if (macher-tool-valid-p tool)
      (let* ((orig-fn (gptel-tool-function tool))
             (tool-name-str (macher-agent-canonical-tool-name tool))
             (tool-sym (when tool-name-str (intern tool-name-str)))
             (cached-wrapped-fn (when tool-sym (get tool-sym 'macher-agent-wrapped-fn))))

        (if (and cached-wrapped-fn (eq orig-fn cached-wrapped-fn))
            tool

          (let ((new-tool (copy-sequence tool)))
            (setf (gptel-tool-function new-tool)
                  (lambda (&rest args)
                    (macher-agent-with-project-root
                     (apply orig-fn args))))

            (when tool-sym
              (put tool-sym 'macher-agent-wrapped-fn (gptel-tool-function new-tool)))
            new-tool)))
    tool))

(defvar macher-agent--allow-gptel-restore)

(declare-function macher-agent-setup-gptel-buffer "macher-agent-gptel-bridge")

(defun macher-agent-gptel-mode-setup ()
  "Initialise macher-agent defaults for gptel buffers inside a workspace.
If outside a workspace, allows standard gptel execution and emits a message."
  (let ((is-workspace (and default-directory
                           (or (and (fboundp 'project-current) (project-current nil default-directory))
                               (and (fboundp 'vc-root-dir) (vc-root-dir))))))
    (if is-workspace
        (progn
          (make-local-variable 'gptel--preset)
          (make-local-variable 'gptel-tools)
          (make-local-variable 'gptel-model)
          (make-local-variable 'gptel-backend)
          (make-local-variable 'gptel-system-prompt)
          (make-local-variable 'gptel-temperature)
          (make-local-variable 'gptel-max-tokens)
          (make-local-variable 'gptel--tool-names)
          (make-local-variable 'gptel--backend-name)

          (setq-local gptel--set-buffer-locally t)

          (setq-local gptel-tools nil)

          (unless (macher-agent-subagent-p)
            (macher-agent--init-workspace-state
             (file-name-as-directory (macher-agent-root default-directory))))

          (when (fboundp 'gptel--restore-state)
            (let ((macher-agent--allow-gptel-restore t))
              (gptel--restore-state))))

      (message "Macher-Agent: Not in a recognised project workspace. Running standard gptel."))))

(defun macher-agent-force-enable ()
  "Force Macher Agent to treat the current directory as a workspace root."
  (interactive)
  (unless (macher-agent-subagent-p)
    (macher-agent--init-workspace-state
     (file-name-as-directory (expand-file-name default-directory))))
  (macher-agent-setup-gptel-buffer)
  (message "Macher-Agent manually forced on for: %s" default-directory))

(add-hook 'gptel-mode-hook #'macher-agent-gptel-mode-setup)

(defun macher-agent-branch-chat (new-name)
  "Clone the current chat, establishing lineage and inheriting all agent state.

NEW-NAME is the name of the new branch buffer (string).

Return nil."
  (interactive "sNew branch name: ")
  (let* ((parent-buf (current-buffer))
         (parent-name (buffer-name parent-buf))
         (parent-mode major-mode)
         (content (buffer-string))
         (active-backend gptel-backend)
         (active-model gptel-model)
         (active-sys gptel-system-prompt)
         (active-skill (bound-and-true-p macher-agent--active-skill-sym)))

    (with-current-buffer (generate-new-buffer new-name)
      (funcall parent-mode)
      (gptel-mode)
      (insert content)

      (setq-local macher-agent-parent-buffer parent-name)

      (when active-backend (setq-local gptel-backend active-backend))
      (when active-model (setq-local gptel-model active-model))
      (when active-sys (setq-local gptel-system-prompt active-sys))
      (when active-skill (setq-local macher-agent--active-skill-sym active-skill))

      (switch-to-buffer (current-buffer)))))

(defun macher-agent-sandbox-run (expression extra-operations)
  "Execute EXPRESSION in a sandboxed environment with EXTRA-OPERATIONS.

EXPRESSION is the Lisp form to evaluate.
EXTRA-OPERATIONS is a list of allowed host function symbols.

Return the evaluated result."
  (declare (ftype (function (t list) t)))
  (macher-agent--sandbox-run expression extra-operations))

(provide 'macher-agent-api)
