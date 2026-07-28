;;; macher-agent-api.el --- Public API for macher-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Public API implementation for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-macro)
(require 'macher-agent-vfs-client)
(require 'macher-agent-macher-bridge)
(require 'macher-agent-orchestration)
(require 'macher-agent-gptel-tools)
(require 'macher-agent-sandbox)

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

(defvar macher-agent-tools-registry (make-hash-table :test 'equal)
  "Store global hash table for all loaded agent tools.

This variable maps tool names to tool structures across sessions when
workspace-specific registries are not active.

Return a hash table mapping canonical tool names to tool structures.

Side effects: None.")

(defvar macher-agent-global-skills-alist nil
  "Store global association list for all loaded agent skills metadata.

This association list maps skill symbols to property lists containing
system prompts, tool lists, descriptions, and other metadata.

Return an association list of skill metadata or nil.

Side effects: None.")

(defvar-local macher-agent-parent-buffer nil
  "Store name of parent chat buffer from which current chat branched.

This buffer-local variable retains the string name of the originating parent
buffer when a chat is branched.

Return the parent buffer name string or nil.

Side effects: None.")

(defvar-local macher-agent--active-skill-sym nil
  "Store symbol representing the currently active skill preset in buffer.

This buffer-local variable holds the active skill symbol bound to the current
chat buffer.

Return the active skill symbol or nil.

Side effects: None.")

(defmacro macher-agent-with-project-root (&rest body)
  "Execute BODY with default directory bound to the project root.

BODY forms are evaluated sequentially with `default-directory` set to
the resolved project root directory.

Return the result of evaluating the last form in BODY.

Side effects: Temporarily rebinds `default-directory` during execution."
  `(let ((default-directory (file-name-as-directory (macher-agent-root))))
     ,@body))

(defun macher-agent-workspace-resolve-path (path)
  "Resolve PATH into an absolute buffer name string.

PATH is the relative or absolute path string to resolve.

Return the resolved absolute buffer name string.

Side effects: None."
  (macher-agent--resolve-buffer-name path))

(defun macher-agent-context-read (context file)
  "Read content of FILE from CONTEXT.

CONTEXT is the active context structure.
FILE is the relative file path string.

Return the file content string, or nil if not found.

Side effects: None."
  (macher-agent--read-context-file context file))

(defun macher-agent-context-update (context file content)
  "Update FILE in CONTEXT with new CONTENT string.

CONTEXT is the active context structure.
FILE is the relative file path string.
CONTENT is the new text content string.

Return nil.

Side effects: Mutates CONTEXT in place to store updated file content."
  (macher-agent--update-context-file context file content))

(defun macher-agent-scope-add-file (buffer-name context)
  "Add BUFFER-NAME to the authorised scope in CONTEXT.

BUFFER-NAME is the name of the buffer string to authorise.
CONTEXT is the active context structure.

Return nil.

Side effects: Mutates CONTEXT scope list."
  (macher-agent--add-buffer-to-scope-headless buffer-name context))

(defun macher-agent-prepare-instructions (buf instructions preset)
  "Prepare and inject sub-agent INSTRUCTIONS into BUF using PRESET.

BUF is the target sub-agent buffer.
INSTRUCTIONS is the directive text string.
PRESET is the preset symbol or string representing requested skills.

Return nil.

Side effects: Modifies sub-agent buffer local state and directives."
  (macher-agent--prepare-subagent-instructions buf instructions preset))

(defun macher-agent-submit-task-result (result)
  "Submit the final RESULT for the current agent task.

RESULT is the final synthesised answer string.

Return nil.

Side effects: Sets buffer-local result variable and calls parent callback."
  (setq-local macher-agent--final-result result)
  (when (boundp 'macher-agent--parent-callback)
    (funcall macher-agent--parent-callback
             (make-macher-agent-lisp-result-response
              :status 'success
              :data result
              :buffer-name (buffer-name)))
    (makunbound 'macher-agent--parent-callback)))

(defun macher-agent-ui-show (&optional buf)
  "Display the user interface for buffer BUF.

BUF is the optional buffer to display, defaulting to current buffer.

Return nil.

Side effects: Opens or focuses user interface window."
  (macher-agent--show-ui buf))

(defun macher-agent-workspace-root (workspace)
  "Retrieve the project root directory path of WORKSPACE.

WORKSPACE is the target workspace structure or object.

Return the project root path string, or nil if unresolved.

Side effects: None."
  (if (macher-agent-workspace-p workspace)
      (macher-agent-workspace-project-root workspace)
    (macher--workspace-root workspace)))

(defun macher-context-workspace-root (context)
  "Retrieve the project root directory from CONTEXT structure.

CONTEXT is the active context structure.

Return the project root path string, or nil if unresolved.

Side effects: None."
  (when-let* ((workspace (when context (macher-agent--get-context-workspace context))))
    (macher-agent-workspace-project-root workspace)))

(defun macher-normalise-preset-name (preset)
  "Convert PRESET to a uniform symbol without leading character symbols.

PRESET is the preset string or symbol to normalise.

Return the normalised preset symbol, or nil if PRESET is invalid.

Side effects: None."
  (when (and preset (or (symbolp preset) (stringp preset)))
    (let* ((raw-str (if (symbolp preset) (symbol-name preset) preset))
           (clean-str (replace-regexp-in-string "^@+" "" raw-str)))
      (intern clean-str))))

(defun macher-tool-valid-p (tool)
  "Validate whether TOOL is a valid gptel tool structure.

TOOL is the object to inspect.

Return non-nil if TOOL is a valid gptel tool, otherwise nil.

Side effects: None."
  (and tool (fboundp 'gptel-tool-p) (gptel-tool-p tool)))

(defun macher-agent--extract-raw-tool-name (tool)
  "Extract raw name representation from TOOL structure or list.

TOOL is a string, symbol, plist, or gptel tool structure.

Return the raw tool name representation or TOOL unchanged if unrecognised.

Side effects: None."
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
   (t tool)))

(defsubst macher-agent-canonical-tool-name (tool)
  "Extract and coerce TOOL into a canonical string name.

TOOL is a gptel tool struct, symbol, plist, or raw string to convert.

Return the canonical tool name string, or nil if TOOL is nil.

Side effects: None."
  (when tool
    (let ((raw-name (macher-agent--extract-raw-tool-name tool)))
      (format "%s" (if (symbolp raw-name) (symbol-name raw-name) raw-name)))))

(defun macher-agent--cache-tool (tool registry)
  "Cache TOOL in REGISTRY using its canonical string name.

TOOL is the tool structure to cache.
REGISTRY is the hash table representing the tool registry.

Return nil.

Side effects: Inserts canonical tool mapping into REGISTRY."
  (when (macher-tool-valid-p tool)
    (when-let* ((canonical-name (macher-agent-canonical-tool-name tool)))
      (puthash canonical-name tool registry))))

(defun macher-agent-force-review ()
  "Trigger the diff review screen for pending virtual edits manually.

Inspects active context and FSM state and generates patch review buffers
when uncommitted virtual edits are detected.

Return nil.

Side effects: Generates diff patch buffers and displays review interface."
  (interactive)
  (let ((context (macher-agent-resolve-context))
        (fsm (macher-agent--get-fsm-latest)))
    (cond
     ((not (and context fsm))
      (message "No active context or FSM found."))
     ((not (macher-agent--get-context-dirty-p context))
      (message "No pending edits to review."))
     (t
      (macher--build-patch context fsm)
      (message "SUCCESS: Patch review screen(s) generated for pending edits.")))))

(defun macher-agent--parse-frontmatter-list-item (line)
  "Extract list item string from LINE when matching frontmatter syntax.

LINE is the text line string to inspect.

Return the trimmed item string, or nil if LINE is not a list item.

Side effects: None."
  (when (string-match "^[ \t]*-[ \t]+\"?\\([^\"]+\\)\"?" line)
    (string-trim (match-string 1 line))))

(defun macher-agent--parse-frontmatter-kv-pair (key raw-val ht)
  "Parse key-value pair KEY and RAW-VAL into hash table HT.

KEY is the property key string.
RAW-VAL is the raw value string to parse.
HT is the target hash table storing key-value pairs.

Return the symbol `multiline-list` if RAW-VAL starts a multiline list,
or nil after storing the parsed value in HT.

Side effects: Inserts parsed key-value pair into HT."
  (cond
   ((string-match "^\\[\\(.*\\)\\]$" raw-val)
    (let* ((inner (match-string 1 raw-val))
           (items (split-string inner "[, \t\n\r\"]+" t)))
      (puthash key items ht)
      nil))
   ((string-empty-p raw-val)
    'multiline-list)
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
                   (raw-val (string-trim (match-string 2 line)))
                   (status (macher-agent--parse-frontmatter-kv-pair key raw-val ht)))
              (when (eq status 'multiline-list)
                (setq current-list-key key
                      current-list-val nil)))))))

    (when current-list-key
      (puthash current-list-key (nreverse current-list-val) ht))
    ht))

(defun macher-agent--resolve-skill-vfs-content (candidates context)
  "Resolve virtual file system content for CANDIDATES in CONTEXT.

CANDIDATES is a list of candidate path strings.
CONTEXT is the active context structure.

Return the resolved content string, or nil if unresolved.

Side effects: None."
  (when context
    (or
     (let ((contents (macher-agent--get-context-contents context)))
       (cl-loop for cand in candidates
                thereis (when-let* ((entry (cl-find cand contents :key #'car :test #'equal)))
                          (if (consp (cdr entry))
                              (cddr entry)
                            (cdr entry)))))
     (when-let* ((ws (macher-agent--get-context-workspace context))
                 (vfs-buffers (macher-agent-workspace-vfs-buffers ws)))
       (cl-loop for cand in candidates
                thereis (gethash cand vfs-buffers)))
     (cl-loop for cand in candidates
              thereis (ignore-errors
                        (macher-agent--read-context-file context cand))))))

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
          :has-tools has-tools
          :allowed-tools (gethash "allowed-tools" fm-hash)
          :exclusive (gethash "exclusive" fm-hash)
          :ptc-primitives (gethash "ptc-primitives" fm-hash)
          :body body)))

(defun macher-agent-parse-skill-file (filepath &optional context)
  "Parse a skill definition file at FILEPATH extracting metadata and body.

Prioritises virtual file system contents before reading from disk.
FILEPATH is the skill file path string.
CONTEXT is the optional active context structure.

Return a property list containing skill properties.

Side effects: Reads file or VFS buffer into a temporary buffer."
  (let* ((resolved-ctx (or context (ignore-errors (macher-agent-resolve-context))))
         (workspace-root (when resolved-ctx (macher-context-workspace-root resolved-ctx)))
         (abs-file (expand-file-name filepath))
         (rel-to-workspace (when (and workspace-root abs-file)
                             (file-relative-name abs-file workspace-root)))
         (visiting-buf (find-buffer-visiting abs-file))
         (visiting-buf-name (when visiting-buf (buffer-name visiting-buf)))
         (candidates (delq nil (list filepath abs-file rel-to-workspace visiting-buf-name)))
         (vfs-content (macher-agent--resolve-skill-vfs-content candidates resolved-ctx)))
    (with-temp-buffer
      (let ((org-inhibit-startup t))
        (setq default-directory (file-name-directory abs-file))
        (if vfs-content
            (insert vfs-content)
          (insert-file-contents abs-file))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (org-mode)
        (setq-local org-element-use-cache nil)
        (let ((buffer-file-name abs-file))
          (org-macro-initialize-templates)
          (org-macro-replace-all org-macro-templates))
        (macher-agent--extract-skill-frontmatter-and-body)))))

(defun macher-agent--secure-ast-p (form)
  "Verify whether FORM is a secure abstract syntax tree node.

FORM is the Lisp expression form to inspect for top-level definitions.

Return t if FORM contains no unsafe top-level definitions, otherwise nil.

Side effects: None."
  (not (and (consp form)
            (memq (car form) '(defun cl-defun defvar defcustom defmacro)))))

(defun macher-agent--read-and-validate-form (buf &optional validation-cb)
  "Read a form from BUF and validate it using VALIDATION-CB.

BUF is the source buffer to read from.
VALIDATION-CB is an optional validation callback function called on the form.

Return the validated read Lisp form.

Side effects: Advances point in BUF; signals an error if validation fails."
  (let ((form (read buf)))
    (when (and validation-cb (not (funcall validation-cb form)))
      (error "Validation failed for form: %S" form))
    form))

(defun macher-agent--parse-safe-forms (content &optional validation-cb)
  "Parse CONTENT string into a list of validated Lisp forms.

CONTENT is the code string block to parse.
VALIDATION-CB is an optional predicate function called on each form; if it
returns nil, an error is signalled.

Return a list of parsed Lisp forms.

Side effects: Creates temporary buffer during parsing."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((forms nil))
      (condition-case nil
          (while t
            (push (macher-agent--read-and-validate-form (current-buffer) validation-cb)
                  forms))
        (end-of-file nil))
      (nreverse forms))))

(defun macher-agent--eval-forms-sequence (forms)
  "Evaluate FORMS sequentially and return evaluation result of last form.

FORMS is a list of Lisp expression forms to evaluate in order.

Return the value returned by the final form in FORMS.

Side effects: Executes Lisp evaluation for each form in FORMS."
  (let ((res nil))
    (dolist (form forms res)
      (setq res (eval form t)))))

(defun macher-agent--evaluate-and-cache-tool (content tool-name registry)
  "Evaluate CONTENT string and cache resulting tool structure in REGISTRY.

CONTENT is the string representation of the tool code.
TOOL-NAME is the string name of the tool.
REGISTRY is the hash table representing the tool registry.

Return the cached tool object, or TOOL-NAME string if evaluation fails.

Side effects: Evaluates Lisp code and modifies REGISTRY."
  (let ((tool nil))
    (condition-case err
        (let* ((forms (macher-agent--parse-safe-forms
                       content
                       (lambda (f)
                         (if (macher-agent--secure-ast-p f)
                             t
                           (error "SECURITY WARNING: Tool attempts to evaluate top-level definition: %s" (car f))))))
               (val (macher-agent--eval-forms-sequence forms)))
          (setq tool (if (and (symbolp val) (boundp val))
                         (symbol-value val)
                       val)))
      (error
       (message "Macher-Agent: Failed to load tool %s - %s" tool-name err)))
    (when tool
      (macher-agent--cache-tool tool registry))
    tool))

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

(defun macher-agent--read-and-cache-from-disk (tool-name script-paths registry)
  "Load and cache a tool from physical disk using candidate SCRIPT-PATHS.

TOOL-NAME is the string name of the tool.
SCRIPT-PATHS is a list of candidate file path strings on physical disk.
REGISTRY is the hash table representing the tool registry.

Return the cached tool object, or TOOL-NAME string if loading fails.

Side effects: Reads disk files and inserts tool entry into REGISTRY."
  (let ((disk-content (macher-agent--read-first-existing-file script-paths)))
    (if disk-content
        (macher-agent--evaluate-and-cache-tool disk-content tool-name registry)
      tool-name)))

(defun macher-agent--is-managed-path-p (path)
  "Test whether PATH resides within managed skill or script directories.

PATH is the file path string to inspect.

Return non-nil if PATH matches managed skill or script patterns, else nil.

Side effects: None."
  (and path
       (stringp path)
       (or (string-match-p "SKILL\\.md$" path)
           (string-match-p "skills/.*\\.el$" path)
           (string-match-p "scripts/.*\\.el$" path))))

(defun macher-agent--mutation-dispatcher (&rest args)
  "Dispatch context mutation events to appropriate invalidation handlers.

ARGS represents the list of event arguments passed by the hook.

Return nil.

Side effects: Removes cached tools or re-initialises skills on change."
  (let* ((path (cl-find-if #'stringp args))
         (context (or
                   (cl-find-if
                    (lambda (x) (and x (not (stringp x))
                                     (macher-context-p x))) args)
                   (bound-and-true-p macher-agent--persistent-context)))
         (workspace (when context (macher-agent--get-context-workspace context))))

    (when (and path (string-match "scripts/\\([^/]+\\)\\.el$" path))
      (let* ((tool-name (match-string 1 path))
             (registry (if workspace
                           (macher-agent-workspace-tools-registry workspace)
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
      (let ((skills-dir (expand-file-name "skills"
                                          (macher-agent-workspace-project-root workspace))))
        (macher-agent-initialize-skills context skills-dir)))

    (when (and path (macher-agent--is-managed-path-p path))
      nil)))

(add-hook 'macher-agent-context-mutated-hook #'macher-agent--mutation-dispatcher)

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
                    (workspace-root (macher-context-workspace-root context))
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
                                 (ignore-errors (macher-agent--read-context-file
                                                 context cand))))
               found-content)))
      vfs-content
    (macher-agent--read-first-existing-file script-paths)))

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
    (when-let* ((content (macher-agent--locate-tool-source canonical-name context dir-context script-paths workspace))
                (forms (macher-agent--parse-and-validate-tool-ast content canonical-name)))
      (macher-agent--evaluate-trusted-tool-ast forms canonical-name))))

(defun macher-agent--resolve-fallback-tool (tool-name canonical-name)
  "Resolve TOOL-NAME via bound symbol value or gptel fallback registry.

TOOL-NAME is the string or symbol representation of the tool.
CANONICAL-NAME is the canonical tool name string.

Return the resolved tool struct, or nil if unresolved.

Side effects: None."
  (let* ((tool-sym (if (symbolp tool-name) tool-name (intern-soft tool-name)))
         (sym-val (when (and tool-sym (boundp tool-sym)) (symbol-value tool-sym))))
    (if (and sym-val (macher-tool-valid-p sym-val))
        sym-val
      (when canonical-name
        (ignore-errors (gptel-get-tool canonical-name))))))

(defun macher-agent-resolve-tool (tool-name tools-registry &optional dir-context context)
  "Retrieve TOOL-NAME from TOOLS-REGISTRY or load from VFS or physical disk.

TOOL-NAME is the tool identifier string or symbol.
TOOLS-REGISTRY is the hash table tool registry or nil to resolve via context.
DIR-CONTEXT is the active directory context string.
CONTEXT is the active context structure.

Return the resolved gptel tool struct, or TOOL-NAME if unresolved.

Side effects: Caches newly loaded tools in TOOLS-REGISTRY."
  (let* ((registry (or tools-registry
                       (when context
                         (when-let* ((ws (macher-agent--get-context-workspace context)))
                           (macher-agent-workspace-tools-registry ws)))
                       macher-agent-tools-registry))
         (canonical-name (macher-agent-canonical-tool-name tool-name))
         (cached (and canonical-name (gethash canonical-name registry))))
    (if cached
        cached
      (let* ((workspace (when context (macher-agent--get-context-workspace context)))
             (script-paths (when canonical-name
                             (delq nil (list
                                        (when dir-context
                                          (expand-file-name (format "scripts/%s.el" canonical-name)
                                                            dir-context))
                                        (expand-file-name (format "scripts/%s.el" canonical-name)
                                                          (bound-and-true-p macher-agent--bundled-skills-dir))))))
             (loaded-tool (or (macher-agent--load-tool-from-source canonical-name context dir-context script-paths workspace)
                              (macher-agent--resolve-fallback-tool tool-name canonical-name))))
        (when loaded-tool
          (macher-agent--cache-tool loaded-tool registry))
        (or loaded-tool tool-name)))))

(defun macher-agent--load-scripts-from-dir (skills-dir context)
  "Load script tools from the scripts subdirectory of SKILLS-DIR.

SKILLS-DIR is the base skills directory path string.
CONTEXT is the active context structure.

Return nil.

Side effects: Loads and caches script tools in workspace or global registry."
  (when-let* ((scripts-dir (expand-file-name "scripts" skills-dir))
              ((file-directory-p scripts-dir)))
    (let* ((workspace (when context (macher-agent--get-context-workspace context)))
           (registry (if workspace
                         (macher-agent-workspace-tools-registry workspace)
                       macher-agent-tools-registry)))
      (dolist (script (directory-files scripts-dir t "\\.el$"))
        (let* ((base (file-name-base script))
               (tool (macher-agent-resolve-tool base registry skills-dir context)))
          (ignore tool))))))

(defun macher-agent--register-parsed-skill (parsed skill-file context)
  "Register PARSED skill metadata from SKILL-FILE into CONTEXT or global registry.

PARSED is the property list of skill attributes.
SKILL-FILE is the file path string of the skill definition.
CONTEXT is the active context structure.

Return nil.

Side effects: Updates skill association list and resolves skill tools."
  (let ((sym (plist-get parsed :name-sym))
        (body (plist-get parsed :body)))
    (when (and sym body)
      (let* ((desc (plist-get parsed :description))
             (model (plist-get parsed :model))
             (tool-names (plist-get parsed :allowed-tools))
             (exclusive (plist-get parsed :exclusive))
             (ptc-primitives (plist-get parsed :ptc-primitives))
             (workspace (when context (macher-agent--get-context-workspace context)))
             (skill-base-dir (file-name-directory skill-file))
             (alist (if workspace
                        (macher-agent-workspace-skills-alist workspace)
                      macher-agent-global-skills-alist))
             (registry (if workspace
                           (macher-agent-workspace-tools-registry workspace)
                         macher-agent-tools-registry))
             (resolved-tools (when tool-names
                               (delq nil (mapcar (lambda (tname)
                                                   (macher-agent-resolve-tool tname registry skill-base-dir context))
                                                 tool-names)))))
        (setf (alist-get sym alist)
              (list :system body :description desc :model (when model (intern model)) :tools resolved-tools :exclusive exclusive :ptc-primitives ptc-primitives))
        (if workspace
            (setf (macher-agent-workspace-skills-alist workspace) alist)
          (setq macher-agent-global-skills-alist alist))))))

(defun macher-agent--load-skill-from-path (path &optional context)
  "Load a skill definition from PATH and register its metadata.

PATH is the skill directory or SKILL.md file path string.
CONTEXT is the optional active context structure.

Return nil.

Side effects: Reads skill file and updates skills association list."
  (let ((skill-file (cond
                     ((and (file-directory-p path)
                           (file-exists-p (expand-file-name "SKILL.md" path)))
                      (expand-file-name "SKILL.md" path))
                     ((and (file-regular-p path)
                           (string-match-p "\\.md$" path))
                      path)
                     ((string-match-p "SKILL\\.md$" path)
                      path)
                     (t nil))))
    (when skill-file
      (let ((parsed (macher-agent-parse-skill-file skill-file context)))
        (macher-agent--register-parsed-skill parsed skill-file context)))))

(defun macher-agent--try-load-skill-from-path (path &optional context)
  "Attempt to load skill from PATH catching and logging any errors.

PATH is the skill directory or file path string.
CONTEXT is the optional active context structure.

Return nil.

Side effects: Loads skill metadata or emits warning message on error."
  (condition-case err
      (macher-agent--load-skill-from-path path context)
    (error
     (message "Error loading path %s: %S" path err))))

(defun macher-agent-api-register-skills-in-directory (skills-dir &optional context)
  "Scan SKILLS-DIR directory for skill definition files and load them.

SKILLS-DIR is the directory path string to scan.
CONTEXT is the optional active context structure.

Return nil.

Side effects: Loads script files and registers skill metadata."
  (let* ((expanded-dir (file-name-as-directory (expand-file-name skills-dir)))
         (target-dir (if (file-directory-p (expand-file-name "skills" expanded-dir))
                         (file-name-as-directory (expand-file-name "skills" expanded-dir))
                       expanded-dir)))
    (when (and target-dir (file-directory-p target-dir))
      (macher-agent--load-scripts-from-dir target-dir context)
      (let ((files (directory-files target-dir t "^[^.]")))
        (dolist (path files)
          (macher-agent--try-load-skill-from-path path context))))))

(defun macher-agent--get-system-message-name (sys-msg)
  "Find skill name matching SYS-MSG system message prompt in registered skills.

SYS-MSG is the system message prompt text string to match.

Return the short skill name string, or nil if not found.

Side effects: None."
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
  "Initialise agent skills from all registered directories and contexts.

CONTEXT is the optional active context structure.
DIR is the optional directory path string to load from.

Return nil.

Side effects: Registers skills, configures directives, and sets up menus."
  (interactive)
  (let ((directories (delq nil (append
                                (list dir macher-agent--bundled-skills-dir)
                                (if (listp macher-agent-skill-directories)
                                    macher-agent-skill-directories
                                  (list macher-agent-skill-directories))))))

    (cl-loop for d in (delete-dups directories)
             do (when (file-directory-p d)
                  (macher-agent-api-register-skills-in-directory d context)))

    (when context
      (let ((contents (macher-agent--get-context-contents context)))
        (dolist (entry contents)
          (let ((path (car entry)))
            (when (and path (string-match-p "SKILL\.md$" path))
              (macher-agent--load-skill-from-path path context))))))

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
               for ptc-primitives = (plist-get meta :ptc-primitives)
               for tool-names = (mapcar #'macher-agent-canonical-tool-name tools)
               do (when system-prompt
                    (setf (alist-get sym gptel-directives) system-prompt)
                    (let ((preset-spec (list :description (or desc (format "Agent Profile: %s" sym))
                                             :system system-prompt)))
                      (when exclusive
                        (setq preset-spec (plist-put preset-spec :exclusive t)))
                      (when model (setq preset-spec (plist-put preset-spec :model model)))
                      (when ptc-primitives (setq preset-spec (plist-put preset-spec :ptc-primitives ptc-primitives)))
                      (when tool-names
                        (setq preset-spec (plist-put preset-spec :tools
                                                     (if exclusive
                                                         tool-names `(:append ,tool-names)))))
                      (setf (alist-get sym gptel--known-presets) preset-spec))))))

  (when (fboundp 'gptel--setup-directive-menu)
    (gptel--setup-directive-menu 'gptel-system-prompt "Agent Profile")))

(defun macher-agent--tool-matches-name-p (tool normalised-target)
  "Verify whether TOOL is valid and matches NORMALISED-TARGET name string.

TOOL is the tool struct or object to inspect.
NORMALISED-TARGET is target tool name string formatted with underscores.

Return non-nil if TOOL matches NORMALISED-TARGET, otherwise nil.

Side effects: None."
  (and (macher-tool-valid-p tool)
       (when-let* ((t-name (macher-agent-canonical-tool-name tool)))
         (equal (replace-regexp-in-string "-" "_" t-name) normalised-target))))

(defun macher-agent--find-tool-in-known-tools (normalised-target)
  "Search gptel known tools for a tool structure matching NORMALISED-TARGET.

NORMALISED-TARGET is target tool name string formatted with underscores.

Return matching gptel tool struct, or nil if not found.

Side effects: None."
  (when (boundp 'gptel--known-tools)
    (cl-loop for category-alist in gptel--known-tools
             thereis (cl-loop for (_ . tool) in (cdr category-alist)
                              thereis (when (macher-agent--tool-matches-name-p tool normalised-target)
                                        tool)))))

(defun macher-agent--find-native-tool (tool-name)
  "Find a gptel tool registered natively via gptel tool registration.

TOOL-NAME is the string or symbol tool identifier.

Return matching gptel tool struct, or nil if not found.

Side effects: None."
  (let* ((t-str (if (symbolp tool-name) (symbol-name tool-name) tool-name))
         (normalised-target (replace-regexp-in-string "-" "_" t-str)))
    (or (cl-loop for t_ in (append (default-value 'gptel-tools) (bound-and-true-p gptel-tools))
                 thereis (when (macher-agent--tool-matches-name-p t_ normalised-target)
                           t_))
        (macher-agent--find-tool-in-known-tools normalised-target))))

(defun macher-agent--select-resolved-tool (resolved item)
  "Select resolved tool list from RESOLVED tool object or fallback for ITEM.

RESOLVED is the primary resolved tool candidate or list.
ITEM is the original tool identifier string, symbol, or structure.

Return a list containing resolved gptel tool struct, or nil.

Side effects: None."
  (cond
   ((and resolved (macher-tool-valid-p resolved))
    (list resolved))
   ((when-let* ((native (macher-agent--find-native-tool item)))
      (list native)))
   ((macher-tool-valid-p resolved)
    (list resolved))
   (t nil)))

(defun macher-agent--resolve-tool-in-env (item)
  "Resolve tool ITEM using active workspace registry and environment context.

ITEM is the tool identifier string, symbol, or structure to resolve.

Return a list of resolved gptel tool structs, or nil.

Side effects: May load script files from VFS or physical disk."
  (let* ((ctx (ignore-errors (macher-agent-resolve-context)))
         (ws (when ctx (macher-agent--get-context-workspace ctx)))
         (registry (if ws (macher-agent-workspace-tools-registry ws) macher-agent-tools-registry))
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
      (let ((res (ignore-errors (funcall item))))
        (if res
            (macher-agent-resolve-item res)
          (macher-agent--resolve-tool-in-env item)))
    (macher-agent--resolve-tool-in-env item)))

(cl-defmethod macher-agent-resolve-item ((item list))
  "Resolve tool ITEM when it is a list of tool representations.

ITEM is the list of tool representations to resolve.

Return a flattened list of resolved gptel tool structs, or nil.

Side effects: Resolves each element in ITEM sequentially."
  (cl-mapcan #'macher-agent-resolve-item item))

(defun macher-agent--deduplicate-tool (tool seen)
  "Return TOOL if valid and not previously present in SEEN hash table.

TOOL is the tool struct to check.
SEEN is the hash table tracking seen canonical tool names.

Return TOOL if unique, or nil if TOOL is invalid or already present in SEEN.

Side effects: Records canonical tool name in SEEN hash table."
  (when (macher-tool-valid-p tool)
    (let* ((raw-name (macher-agent-canonical-tool-name tool))
           (canon-name (if (stringp raw-name) (substring-no-properties raw-name) raw-name)))
      (when (and canon-name (not (gethash canon-name seen)))
        (puthash canon-name t seen)
        tool))))

(defun macher-agent-normalize-tools (tools)
  "Normalise, resolve, and deduplicate a mixed list of TOOLS.

TOOLS is a list of tool representations including names, symbols, structs,
functions, or nested lists.

Return a list of unique resolved gptel tool structures.

Side effects: Resolves tools via environment registries and updates hash table."
  (let ((flat-resolved (delq nil (macher-agent-resolve-item tools)))
        (seen (make-hash-table :test 'equal))
        (unique nil))
    (dolist (tool flat-resolved)
      (when-let* ((deduped (macher-agent--deduplicate-tool tool seen)))
        (push deduped unique)))
    (nreverse unique)))

(defun macher-agent-resolve-to-struct (t-item)
  "Convert tool item T-ITEM into a single gptel tool structure.

T-ITEM is the tool representation to convert.

Return the first resolved gptel tool structure, or nil if unresolved.

Side effects: Resolves tools via environment context."
  (car (macher-agent-normalize-tools t-item)))

(defun macher-agent--create-project-root-wrapper (tool orig-fn tool-sym)
  "Create a copy of TOOL wrapping ORIG-FN in project root execution context.

TOOL is the original gptel tool struct.
ORIG-FN is the original function of the tool.
TOOL-SYM is the symbol associated with the tool name.

Return a new gptel tool struct wrapped to execute in project root.

Side effects: Attaches `macher-agent-wrapped-fn` property to TOOL-SYM."
  (let ((new-tool (copy-sequence tool)))
    (setf (gptel-tool-function new-tool)
          (lambda (&rest args)
            (macher-agent-with-project-root
             (apply orig-fn args))))
    (when tool-sym
      (put tool-sym 'macher-agent-wrapped-fn (gptel-tool-function new-tool)))
    new-tool))

(defun macher-agent--wrap-tool-for-project-root (tool)
  "Clone and wrap TOOL to execute function calls within project root.

TOOL is the gptel tool struct to wrap.

Return the wrapped gptel tool structure, or TOOL unchanged if invalid.

Side effects: Creates wrapped function closure and caches property."
  (if (not (macher-tool-valid-p tool))
      tool
    (let* ((orig-fn (gptel-tool-function tool))
           (tool-name-str (macher-agent-canonical-tool-name tool))
           (tool-sym (when tool-name-str (intern tool-name-str)))
           (cached-wrapped-fn (when tool-sym (get tool-sym 'macher-agent-wrapped-fn))))
      (if (and cached-wrapped-fn (eq orig-fn cached-wrapped-fn))
          tool
        (macher-agent--create-project-root-wrapper tool orig-fn tool-sym)))))

(defvar macher-agent--allow-gptel-restore nil
  "Control whether gptel session state restoration is permitted.

When non-nil, allows gptel state restore operations during initialisation.

Return non-nil if state restoration is allowed, otherwise nil.

Side effects: None.")

(declare-function macher-agent-setup-gptel-buffer "macher-agent-gptel-bridge")

(defun macher-agent-gptel-mode-setup ()
  "Initialise agent defaults for gptel buffers inside a workspace.

When operating outside a project workspace, permits standard gptel execution
and logs an informational message.

Return nil.

Side effects: Binds buffer-local gptel variables and restores session state."
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
  "Force Macher Agent to treat current directory as a workspace root.

Return nil.

Side effects: Initialises workspace state and sets up gptel buffer defaults."
  (interactive)
  (unless (macher-agent-subagent-p)
    (macher-agent--init-workspace-state
     (file-name-as-directory (expand-file-name default-directory))))
  (macher-agent-setup-gptel-buffer)
  (message "Macher-Agent manually forced on for: %s" default-directory))

(defun macher-agent-branch-chat (new-name)
  "Clone current chat buffer establishing lineage and inheriting agent state.

NEW-NAME is the name string for the new branch buffer.

Return nil.

Side effects: Creates new chat buffer and sets buffer-local agent variables."
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
  "Execute Lisp EXPRESSION in a sandboxed environment with EXTRA-OPERATIONS.

EXPRESSION is the Lisp expression form to evaluate inside sandbox.
EXTRA-OPERATIONS is a list of host function symbols allowed in sandbox.

Return the result of evaluating EXPRESSION.

Side effects: Evaluates sandboxed Lisp expression."
  (declare (ftype (function (t list) t)))
  (macher-agent-sandbox--run expression extra-operations))

(provide 'macher-agent-api)
;;; macher-agent-api.el ends here