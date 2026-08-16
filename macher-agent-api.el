;;; macher-agent-api.el --- Public API for macher-agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Public API implementation for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-macro)
(require 'macher-agent-core)
(require 'macher-agent-sandbox)
(require 'macher-agent-vfs)
(require 'macher-agent-presets)
(require 'macher-agent-macher)
(require 'macher-agent-gptel)
(require 'macher-agent-orchestration)
(require 'macher-agent-tools)

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
  (when-let* ((buf (get-buffer buffer-name))
              (content (with-current-buffer buf
                         (buffer-substring-no-properties (point-min) (point-max)))))
    (macher-agent-context-update context buffer-name content)))

(defun macher-agent-add-buffer-to-scope (buffer)
  "Interactively add a BUFFER to the agent's authorised virtual file system scope.

BUFFER is the name of the buffer to authorise.

Return nil.

Side effects: Mutates the active agent context and displays a message."
  (interactive "bAdd buffer to agent scope: ")
  (let* ((buf-name (if (bufferp buffer) (buffer-name buffer) buffer))
         (ctx (ignore-errors (macher-agent-resolve-context))))
    (if (not ctx)
        (error "ERROR: No active Macher Agent context found")
      (macher-agent-scope-add-file buf-name ctx)
      (message "SUCCESS: Added '%s' to the agent's authorised scope." buf-name))))

(defun macher-agent-prepare-instructions (buf instructions preset)
  "Prepare and inject sub-agent INSTRUCTIONS into BUF using PRESET.

BUF is the target sub-agent buffer.
INSTRUCTIONS is the directive text string.
PRESET is the preset symbol or string representing requested skills.

Return target buffer object.

Side effects: Modifies sub-agent buffer local state and directives."
  (when-let* ((target-buf (if (bufferp buf) buf (get-buffer buf))))
    (with-current-buffer target-buf
      (when preset
        (let ((preset-list (cond ((listp preset) preset)
                                 ((vectorp preset) (append preset nil))
                                 (t (list preset)))))
          (setq-local macher-agent-presets preset-list)
          (when (fboundp 'macher-agent--apply-preset)
            (macher-agent--apply-preset preset-list))))
      (when (fboundp 'macher-agent-sync-prompt-transformer)
        (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t))
      (when (fboundp 'macher-agent--enforce-tool-scope)
        (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t))
      (when (and (stringp instructions) (not (string-empty-p instructions)))
        (erase-buffer)
        (insert instructions)))
    target-buf))

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

(defun macher-agent-log-tool-intent (context type target args)
  "Log tool execution intent entry to CONTEXT audit log.

CONTEXT is the active context structure.
TYPE is the invocation type string, such as gptel-tool or ptc.
TARGET is the tool identifier name string or PTC primitive symbol.
ARGS is the list of parameters passed to the tool.

Return the updated audit log list.

Side effects: Mutates CONTEXT audit log property list."
  (when context
    (let* ((log (macher-agent--get-context-data context :audit-log))
           (preset-sym (bound-and-true-p macher-agent--active-skill-sym))
           (preset-str (if preset-sym
                           (if (symbolp preset-sym)
                               (symbol-name preset-sym)
                             (format "%s" preset-sym))
                         "unknown"))
           (entry `((timestamp . ,(format-time-string "%Y-%m-%dT%H:%M:%S"))
                    (buffer . ,(buffer-name))
                    (preset . ,preset-str)
                    (type . ,type)
                    (target . ,target)
                    (args . ,args))))
      (macher-agent--set-context-data context :audit-log (append log (list entry))))))

(defun macher-agent--log-gptel-pre-tool (tool &optional _fsm &rest args)
  "Log GPTEL pre-tool call intent to context audit log.

TOOL is the tool structure or property list.
_FSM is the optional state machine object.
ARGS represents additional arguments passed to the tool call.

Return nil.

Side effects: Appends tool call entry to active context audit log."
  (let ((context (ignore-errors (macher-agent-resolve-context)))
        (tool-name (macher-agent-canonical-tool-name tool))
        (tool-args (if (and (listp tool) (plist-member tool :args))
                       (plist-get tool :args)
                     args)))
    (macher-agent-log-tool-intent context "gptel-tool" tool-name tool-args)))

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
     (t
      (when (fboundp 'macher-agent--auto-sync-context)
        (macher-agent--auto-sync-context context))

      (if (not (macher-agent--context-has-changes-p context))
          (message "No pending edits to review.")
        (macher--build-patch context fsm)
        (message "SUCCESS: Patch review screen(s) generated for pending edits."))))))

(defvar macher-agent--allow-gptel-restore nil
  "Control whether gptel session state restoration is permitted.

When non-nil, allows gptel state restore operations during initialisation.

Return non-nil if state restoration is allowed, otherwise nil.

Side effects: None.")

(defun macher-agent-sandbox-run (expression extra-operations)
  "Execute Lisp EXPRESSION in a sandboxed environment with EXTRA-OPERATIONS.

EXPRESSION is the Lisp expression form to evaluate inside sandbox.
EXTRA-OPERATIONS is a list of host function symbols allowed in sandbox.

Return the result of evaluating EXPRESSION.

Side effects: Evaluates sandboxed Lisp expression."
  (declare (ftype (function (t list) t)))
  (let ((macher-agent-sandbox--primitives (make-hash-table :test 'eq))
        (macher-agent-sandbox--functions (make-hash-table :test 'eq))
        (macher-agent-sandbox--globals (make-hash-table :test 'eq)))
    (macher-agent-sandbox--init extra-operations)
    (let ((context (ignore-errors (macher-agent-resolve-context)))
          (iterator (macher-agent-sandbox--eval-iter (macroexpand-all expression) nil))
          (yield-val nil)
          (next-yield nil))
      (condition-case err
          (while t
            (setq next-yield (iter-next iterator yield-val))
            (when (consp next-yield)
              (let ((target (or (plist-get next-yield :target)
                                (plist-get next-yield :name)
                                (car next-yield)))
                    (args (plist-get next-yield :args)))
                (macher-agent-log-tool-intent context "ptc" target args)))
            (setq yield-val next-yield))
        (iter-end-of-sequence (cdr err))))))

(defun macher-agent-apply-patch ()
  "Apply the active patch from `diff-mode' context back to physical files.

Return nil.
Side effects: Invokes external process (`git` or `patch`) to apply patch."
  (interactive)
  (unless (derived-mode-p 'diff-mode) (user-error "Not in a patch/diff buffer"))
  (let* ((patch-content (buffer-substring-no-properties (point-min) (point-max)))
         (ctx (ignore-errors (macher-agent-resolve-context)))
         (ws (or (when ctx (macher-agent--get-context-workspace ctx))
                 (macher-agent--get-active-workspace)))
         (root (if ws
                   (macher-agent-workspace-project-root ws)
                 (or (locate-dominating-file default-directory ".git") default-directory)))
         (default-directory (file-name-as-directory (expand-file-name root)))
         (use-git (locate-dominating-file default-directory ".git"))
         (cmd (if use-git "git" "patch"))
         (args (if use-git '("apply" "-p1" "-") '("-p1"))))
    (with-temp-buffer
      (insert patch-content)
      (let
          ((exit-code
            (apply #'call-process-region
                   (point-min) (point-max) cmd nil "*macher-patch-out*" nil args)))
        (if (= exit-code 0)
            (progn
              (message "SUCCESS: Patch applied safely via %s from %s" cmd default-directory)
              (when (get-buffer "*macher-patch-out*")
                (kill-buffer "*macher-patch-out*")))
          (pop-to-buffer "*macher-patch-out*")
          (message "ERROR: Failed to apply patch safely."))))))

(defun macher-agent-insert-patch ()
  "Insert the proposed patch content into the current buffer.

Return nil.
Side effects: Inserts diff string into the current buffer."
  (interactive)
  (if-let* ((patch-buf (macher-agent-trigger-patch))
            (is-live (buffer-live-p patch-buf))
            (content
             (with-current-buffer
                 patch-buf (buffer-substring-no-properties (point-min) (point-max))))
            ((not (string-empty-p content))))
      (insert "\nHere is your proposed patch:\n```diff\n" content "\n```\n")
    (message "No patch available for current workspace.")))

(provide 'macher-agent-api)
;;; macher-agent-api.el ends here
