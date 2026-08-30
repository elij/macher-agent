;;; macher-agent-macher.el --- Bridge to Macher Core  -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridge implementation for Macher Agent to interact with Macher Core.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'macher-agent-core)

(defvar gptel-directives)
(defvar macher-workspace-types-alist)
(defvar macher-workspace-functions)
(defvar macher--workspace)

(declare-function gptel-fsm-info "gptel" (fsm))
(declare-function gptel-fsm-p "gptel" (obj))
(declare-function project-root "project" (project))
(declare-function macher--workspace-hash "macher" (workspace &optional len))
(declare-function macher--workspace-name "macher" (workspace))
(declare-function macher--build-patch "macher" (context fsm))
(declare-function macher--make-context "macher" (&rest args))
(declare-function make-macher-context "macher" (&rest args))
(declare-function macher-context-p "macher" (obj))
(declare-function macher-context-workspace "macher" (ctx))
(declare-function macher-context-contents "macher" (ctx))
(declare-function macher-context-dirty-p "macher" (ctx))
(declare-function macher-context-shadow-buffers "macher" (ctx))
(declare-function macher-context-data "macher" (ctx))
(declare-function macher-context-prompt "macher" (ctx))
(declare-function macher-patch-buffer "macher" (workspace))

(defun macher-agent--unwrap-workspace (ws)
  "Unwrap WS if wrapped in an agent tag cell or context structure.

Strip the leading agent/project tag from WS if it is structured as a
tagged cons cell, or extract project root from `macher-agent-context`.

Return the unwrapped workspace structure or WS unchanged.

Side effects: None."
  (cond
   ((macher-agent-context-p ws)
    (or (macher-agent-context-project-root ws)
        (macher-agent-context-root ws)))
   ((and (consp ws) (memq (car ws) '(agent project)))
    (cdr ws))
   (t ws)))

(defun macher-agent-macher-safe-workspace-hash (workspace &optional length &rest _args)
  "Compute a safe MD5 hash for WORKSPACE without recursive traversal.

Calculate a deterministic MD5 hash string for WORKSPACE to prevent
recursion depth limit failures during workspace hashing.  If LENGTH
is provided and positive, truncate the hash string to
(min (length str) length).
_ARGS accommodates additional arguments supplied by advice wrappers.

Return the MD5 hash string.

Side effects: None."
  (let* ((unwrapped (macher-agent--unwrap-workspace workspace))
         (path (cond
                ((macher-agent-context-p workspace)
                 (or (macher-agent-context-project-root workspace)
                     (macher-agent-context-root workspace)))
                ((macher-agent-context-p unwrapped)
                 (or (macher-agent-context-project-root unwrapped)
                     (macher-agent-context-root unwrapped)))
                ((and (recordp unwrapped) (eq (type-of unwrapped) 'macher-agent-workspace))
                 (macher-agent-workspace-project-root unwrapped))
                ((and (consp unwrapped) (memq (car unwrapped) '(project agent)))
                 (cdr unwrapped))
                ((stringp unwrapped)
                 unwrapped)
                (t (format "%s" unwrapped))))
         (canonical (and (stringp path) (expand-file-name path)))
         (str (md5 (or canonical path "unknown-workspace"))))
    (if (and (numberp length) (> length 0))
        (substring str 0 (min (length str) length))
      str)))

(defun macher-agent-macher-workspace-hash (workspace &optional length)
  "Generate or safely compute a unique hash for WORKSPACE of LENGTH.

Delegates to `macher--workspace-hash' or
`macher-agent-macher-safe-workspace-hash'.

Return the truncated hash string of LENGTH characters (or default 16).
Side effects: None."
  (let* ((len (or length 16))
         (str (or (when (fboundp 'macher--workspace-hash)
                    (let ((unwrapped (macher-agent--unwrap-workspace workspace)))
                      (condition-case nil
                          (let ((res (macher--workspace-hash (if (consp workspace) workspace unwrapped) len)))
                            (if (and (stringp res) (numberp len) (> len 0))
                                (substring res 0 (min (length res) len))
                              res))
                        (error nil))))
                  (macher-agent-macher-safe-workspace-hash workspace len)
                  "0000")))
    (if (and (stringp str) (numberp len) (> len 0))
        (substring str 0 (min (length str) len))
      str)))

(defun macher-agent-macher-workspace-name (workspace)
  "Retrieve display name for WORKSPACE via Macher core or fallback logic.

WORKSPACE is the workspace object, context structure, or cons cell.

Return the resolved workspace name string.
Side effects: None."
  (if (null workspace)
      "workspace"
    (let* ((root (cond
                  ((macher-agent-context-p workspace)
                   (or (macher-agent-context-project-root workspace)
                       (macher-agent-context-root workspace)))
                  (t nil)))
           (unwrapped (if root root (macher-agent--unwrap-workspace workspace))))
      (or (when (and (fboundp 'macher--workspace-name) (consp unwrapped))
            (condition-case nil
                (macher--workspace-name unwrapped)
              (error nil)))
          (when (and (consp unwrapped) (stringp (cdr unwrapped)))
            (file-name-nondirectory (directory-file-name (cdr unwrapped))))
          (when (stringp unwrapped)
            (file-name-nondirectory (directory-file-name unwrapped)))
          (when workspace
            (condition-case nil
                (let ((name (macher-agent--get-name workspace)))
                  (if (string-prefix-p "Agent: " name)
                      (substring name 7)
                    name))
              (error nil)))
          "workspace"))))

(defun macher-agent-macher-build-patch (&optional arg1 arg2 arg3 &rest _rest)
  "Bridge upstream `macher--build-patch' with flexible arities.

Accepts:
- (CTX FSM) where CTX is a `macher-agent-context` (or upstream context)
- (PROJECT-ROOT FILES FSM)
- (CTX FSM FILES)

Ephemerally instantiates upstream `macher-context` via
`macher--make-context` with :workspace `(cons \\='agent PROJECT-ROOT)`
and :contents FILES solely at the moment `macher--build-patch' is called.

Return the result of upstream patch generation.
Side effects: Delegates patch building to Macher core."
  (let* ((is-native-ctx (macher-agent-valid-context-p arg1))
         (is-upstream-ctx (and (not is-native-ctx) (fboundp 'macher-context-p) (macher-context-p arg1)))
         (project-root
          (cond
           (is-native-ctx
            (or (macher-agent-context-project-root arg1)
                (macher-agent-context-root arg1)))
           (is-upstream-ctx
            (when (fboundp 'macher-context-workspace)
              (let ((ws (macher-context-workspace arg1)))
                (if (consp ws) (cdr ws) ws))))
           ((and (consp arg1) (memq (car arg1) '(agent project)))
            (cdr arg1))
           ((stringp arg1) arg1)
           (t (macher-agent-workspace-project-root arg1))))
         (vfs-files
          (cond
           ((and (not is-native-ctx) (not is-upstream-ctx) arg3) arg2)
           (arg3 arg3)
           (is-native-ctx (macher-agent--get-context-contents arg1))
           (is-upstream-ctx (when (fboundp 'macher-context-contents)
                              (macher-context-contents arg1)))
           (t nil)))
         (fsm
          (cond
           ((and (not is-native-ctx) (not is-upstream-ctx) arg3) arg3)
           (t arg2)))
         (prompt
          (or (when is-native-ctx
                (or (macher-agent-context-prompt arg1)
                    (let ((plugins (macher-agent-context-plugins arg1)))
                      (when (macher-agent--plist-p plugins)
                        (plist-get plugins :prompt)))))
              (when is-upstream-ctx
                (when (fboundp 'macher-context-prompt)
                  (macher-context-prompt arg1)))
              (when (and fsm (fboundp 'gptel-fsm-info) (fboundp 'gptel-fsm-p) (gptel-fsm-p fsm))
                (plist-get (gptel-fsm-info fsm) :prompt))
              (when (and (listp fsm) (plist-member fsm :prompt))
                (plist-get fsm :prompt))))
         (raw-root (if (consp project-root) (cdr project-root) project-root))
         (canonical-root (and raw-root (file-truename (expand-file-name raw-root))))

         (relative-vfs-files
          (mapcar (lambda (entry)
                    (let* ((path (macher-agent-vfs-entry-path entry))
                           (orig (macher-agent-vfs-entry-orig entry))
                           (curr (macher-agent-vfs-entry-curr entry))
                           (rel-path (if (and (stringp path) canonical-root)
                                         (file-relative-name (file-truename (expand-file-name path canonical-root))
                                                             (file-name-as-directory canonical-root))
                                       path)))
                      (cons rel-path (cons orig curr))))
                  vfs-files)))

    (let ((default-directory (if canonical-root
                                 (file-name-as-directory canonical-root)
                               default-directory)))
      (if is-upstream-ctx
          (when (fboundp 'macher--build-patch)
            (or (funcall 'macher--build-patch arg1 fsm)
                (macher-agent-macher-patch-buffer arg1)))
        (let ((ephemeral-ctx
               (cond
                ((fboundp 'macher--make-context)
                 (funcall 'macher--make-context
                          :workspace (cons 'agent (or canonical-root project-root))
                          :contents relative-vfs-files
                          :prompt prompt))
                ((fboundp 'make-macher-context)
                 (funcall 'make-macher-context
                          :workspace (cons 'agent (or canonical-root project-root))
                          :contents relative-vfs-files
                          :prompt prompt))
                (t nil))))
          (when (fboundp 'macher--build-patch)
            (or (funcall 'macher--build-patch ephemeral-ctx fsm)
                (macher-agent-macher-patch-buffer (or ephemeral-ctx (cons 'agent (or canonical-root project-root)))))))))))

(defun macher-agent-macher-patch-buffer (&optional workspace)
  "Retrieve the active patch buffer for WORKSPACE from Macher core.

Return the buffer object if found, or nil."
  (when (fboundp 'macher-patch-buffer)
    (let ((ws (cond
               ((and (fboundp 'macher-context-p) (funcall 'macher-context-p workspace))
                (if (fboundp 'macher-context-workspace)
                    (funcall 'macher-context-workspace workspace)
                  workspace))
               ((macher-agent-context-p workspace)
                (if-let* ((root (or (macher-agent-context-project-root workspace)
                                    (macher-agent-context-root workspace))))
                    (cons 'project (expand-file-name (if (consp root) (cdr root) root)))
                  nil))
               (t workspace))))
      (macher-patch-buffer ws))))

(defun macher-agent-macher-install ()
  "Install Macher Core integration, upstream alias, and workspace hooks.

Configures `macher--workspace-hash' alias to use safe workspace hashing,
registers base agent workspace handlers in `macher-workspace-types-alist',
and registers `macher-agent-workspace-agent' in `macher-workspace-functions'.

Side effects: Modifies global Macher tables, hooks, and symbol definitions."
  (defalias 'macher--workspace-hash #'macher-agent-macher-safe-workspace-hash)
  (when (boundp 'macher-workspace-types-alist)
    (let* ((existing (alist-get 'agent macher-workspace-types-alist))
           (merged (append existing '(:get-root macher-agent-workspace-project-root :get-name macher-agent--get-name))))
      (unless (plist-get merged :get-files)
        (setq merged (plist-put merged :get-files 'macher-agent--collect-raw-files)))
      (setf (alist-get 'agent macher-workspace-types-alist) merged)))
  (when (boundp 'macher-workspace-functions)
    (add-hook 'macher-workspace-functions #'macher-agent-workspace-agent))
  (with-eval-after-load 'macher
    (defalias 'macher--workspace-hash #'macher-agent-macher-safe-workspace-hash)
    (when (boundp 'macher-workspace-types-alist)
      (let* ((existing (alist-get 'agent macher-workspace-types-alist))
             (merged (append existing '(:get-root macher-agent-workspace-project-root :get-name macher-agent--get-name))))
        (unless (plist-get merged :get-files)
          (setq merged (plist-put merged :get-files 'macher-agent--collect-raw-files)))
        (setf (alist-get 'agent macher-workspace-types-alist) merged)))
    (when (boundp 'macher-workspace-functions)
      (add-hook 'macher-workspace-functions #'macher-agent-workspace-agent))))

(cl-defun macher-agent--make-vfs-context (&key workspace contents prompt)
  "Create a context struct with WORKSPACE, CONTENTS, and PROMPT.

Return the newly created `macher-agent-context` or `macher-context` struct.

Side effects: Dynamically binds `macher-agent--persistent-context' to nil
during construction."
  (let* ((root (cond
                ((stringp workspace) (expand-file-name workspace))
                ((and (consp workspace) (memq (car workspace) '(project agent)))
                 (if (stringp (cdr workspace)) (expand-file-name (cdr workspace)) (macher-agent-workspace-project-root workspace)))
                ((macher-agent-workspace-p workspace)
                 (macher-agent-workspace-project-root workspace))
                (t (macher-agent-workspace-project-root workspace))))
         (vfs-state (list :contents (or contents nil) :dirty-p nil))
         (plugins (list :vfs vfs-state))
         (ctx (make-macher-agent-context :project-root root :plugins plugins)))
    (when (and ctx prompt)
      (setf (macher-agent-context-prompt ctx) prompt))

    (when (and (macher-agent-context-p ctx)
               (or (fboundp 'macher--make-context) (fboundp 'make-macher-context)))
      (let ((proxy (let ((macher-agent--persistent-context nil))
                     (if (fboundp 'macher--make-context)
                         (funcall 'macher--make-context
                                  :workspace (or workspace (cons 'agent root))
                                  :contents contents
                                  :prompt prompt)
                       (funcall 'make-macher-context
                                :workspace (or workspace (cons 'agent root))
                                :contents contents
                                :prompt prompt)))))
        (setf (macher-agent-context-plugins ctx)
              (plist-put (copy-sequence (macher-agent-context-plugins ctx)) :upstream-proxy proxy))))
    ctx))

(defun macher-agent--get-fsm-latest ()
  "Get the active finite-state machine (FSM) if bound.

Delegates to `macher-agent-get-active-fsm' to locate and return the active
finite-state machine instance.

Return the active FSM structure, or nil if none is bound.

Side effects: None."
  (macher-agent-get-active-fsm))

(defun macher-agent--inject-context-into-fsm-info (agent-ctx &optional fsm)
  "Inject AGENT-CTX into FSM info property list safely.

Store AGENT-CTX in the info plist of finite-state machine FSM
under key `:macher-agent-context'.
FSM defaults to active `gptel--fsm'.

Return non-nil if injected successfully, or nil otherwise.

Side effects: Mutates the info property list of FSM."
  (when-let* ((fsm-obj (macher-agent-get-active-fsm fsm)))
    (let* ((info (macher-agent--extract-fsm-info fsm-obj))
           (new-info (if info (copy-sequence info) nil)))
      (setq new-info (plist-put new-info :macher-agent-context agent-ctx))
      (macher-agent--set-fsm-info fsm-obj new-info))
    t))

(defun macher-agent--get-active-workspace ()
  "Retrieve the globally active workspace.

Return the unwrapped workspace structure, or nil if unbound."
  (let ((ws (bound-and-true-p macher--workspace)))
    (macher-agent--unwrap-workspace ws)))

(defun macher-agent-trigger-patch (&optional ctx)
  "Retrieve the underlying patch buffer for CTX.

Resolve the patch buffer associated with the workspace of CTX,
falling back to the globally active workspace if CTX is nil.

Return the live patch buffer, or nil."
  (let* ((ws (if ctx
                 (macher-agent-context-workspace ctx)
               (macher-agent--get-active-workspace)))
         (patch-buf (and (fboundp 'macher-patch-buffer)
                         (macher-patch-buffer ws))))
    patch-buf))

(defun macher-agent--resolve-context-lazy-init ()
  "Attempt lazy initialisation of context for current directory.

Return non-nil if initialisation succeeded, or signals an error if not allowed.
Side effects: May initialise workspace state for current directory."
  (unless (bound-and-true-p macher-agent--allow-lazy-init)
    (error "Macher-Agent: Not in a recognised project workspace; running standard gptel"))
  (save-excursion
    (when-let* ((current-root (macher-agent-root default-directory)))
      (macher-agent--init-workspace-state current-root)
      (macher-agent-resolve-context (current-buffer)))))

(defun macher-agent--register-active-workspace-root (root context)
  "Register CONTEXT for ROOT in `macher-agent-active-workspaces` hash-table.

ROOT is the project root directory path string.
CONTEXT is the active context structure to register.

Return CONTEXT.
Side effects: Modifies `macher-agent-active-workspaces` hash-table."
  (cl-assert (stringp root) nil "ROOT must be a string, got: %S" root)
  (cl-assert (or (macher-agent-context-p context) (macher-agent-valid-context-p context))
             nil "CONTEXT must be a valid context, got: %S" context)
  (when (and root (boundp 'macher-agent-active-workspaces) (hash-table-p macher-agent-active-workspaces))
    (let ((expanded (expand-file-name root)))
      (puthash expanded context macher-agent-active-workspaces)
      (puthash (file-name-as-directory expanded) context macher-agent-active-workspaces)
      (puthash (directory-file-name expanded) context macher-agent-active-workspaces)))
  context)

(defmacro macher-agent--with-unresolved-ctx-pipe (state &rest body)
  "Execute BODY unless STATE is already resolved."
  (declare (indent 1) (debug t))
  (let ((st-sym (make-symbol "state-val")))
    `(let ((,st-sym ,state))
       (if (and (macher-agent--plist-p ,st-sym) (plist-get ,st-sym :resolved))
           ,st-sym
         ,@body))))

(defun macher-agent-ctx-pipe--explicit (state)
  "Context pipeline step 1: Resolve context explicitly from STATE input.

If `:resolved' in STATE is nil and `:input' in STATE satisfies
`macher-agent-valid-context-p', set `:resolved' to that context structure.

STATE is the context resolution state plist (:input ... :resolved ...).

Return the updated STATE plist.
Side effects: None."
  (macher-agent--with-unresolved-ctx-pipe state
                                          (let ((input (when (macher-agent--plist-p state) (plist-get state :input))))
                                            (if (or (macher-agent-context-p input)
                                                    (macher-agent-valid-context-p input))
                                                (plist-put state :resolved input)
                                              state))))

(defun macher-agent-ctx-pipe--buffer (state)
  "Context pipeline step: Resolve context from explicit buffer in STATE input.

If `:resolved' in STATE is nil and `:input' in STATE is a live buffer,
extract its buffer-local `macher-agent--persistent-context'.

STATE is the context resolution state plist (:input ... :resolved ...).

Return the updated STATE plist.
Side effects: None."
  (macher-agent--with-unresolved-ctx-pipe state
                                          (let ((input (when (macher-agent--plist-p state) (plist-get state :input))))
                                            (if (and (bufferp input) (buffer-live-p input))
                                                (if-let* ((ctx (buffer-local-value 'macher-agent--persistent-context input))
                                                          ((macher-agent-valid-context-p ctx)))
                                                    (plist-put state :resolved ctx)
                                                  state)
                                              state))))

(defun macher-agent-ctx-pipe--fsm (state)
  "Context pipeline step 4: Resolve context from FSM input in STATE.

If `:resolved' in STATE is nil and `:input' in STATE is a finite-state machine
\(FSM), extract and set `:resolved' to its active context structure.

STATE is the context resolution state plist (:input ... :resolved ...).

Return the updated STATE plist.
Side effects: None."
  (macher-agent--with-unresolved-ctx-pipe state
                                          (let ((input (when (macher-agent--plist-p state) (plist-get state :input))))
                                            (if (and input (or (and (fboundp 'gptel-fsm-p) (gptel-fsm-p input))
                                                               (and (recordp input) (fboundp 'gptel-fsm-info))
                                                               (symbolp input)))
                                                (if-let* ((ctx (macher-agent--extract-fsm-context input)))
                                                    (plist-put state :resolved ctx)
                                                  state)
                                              state))))

(defun macher-agent-ctx-pipe--lazy-init (state)
  "Context pipeline step: Resolve context via lazy initialisation in STATE.

If `:resolved' in STATE is nil, attempt lazy workspace initialisation.

STATE is the context resolution state plist (:input ... :resolved ...).

Return the updated STATE plist.
Side effects: May initialise workspace state for current directory."
  (macher-agent--with-unresolved-ctx-pipe state
                                          (if-let* ((ctx (condition-case nil
                                                             (macher-agent--resolve-context-lazy-init)
                                                           (error nil)))
                                                    ((macher-agent-valid-context-p ctx)))
                                              (let ((target-buf (if (bufferp (plist-get state :input))
                                                                    (plist-get state :input)
                                                                  (current-buffer))))
                                                (when (buffer-live-p target-buf)
                                                  (with-current-buffer target-buf
                                                    (unless (bound-and-true-p macher-agent--persistent-context)
                                                      (setq-local macher-agent--persistent-context ctx))))
                                                (plist-put state :resolved ctx))
                                            state)))

(defun macher-agent-context-resolution-install ()
  "Install context resolution pipeline steps."
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--explicit 10)
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--buffer 15)
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-resolve-from-transit-payload 15)
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--fsm 40)
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--lazy-init 80))

(cl-defun make-macher-agent-workspace (&key project-root &allow-other-keys)
  "Construct a standard workspace cons cell `(project . PROJECT-ROOT)`.

PROJECT-ROOT is the root directory path.

Return a workspace cons cell."
  (cons 'project (and project-root (expand-file-name project-root))))

(defun macher-agent-workspace-p (ws)
  "Return non-nil if WS is a valid workspace identifier.

WS is the object to check.

Return non-nil if valid, otherwise nil."
  (or (and (consp ws) (eq (car ws) 'project))
      (and (consp ws) (eq (car ws) 'agent))
      (and (consp ws) (consp (car ws)) (memq (caar ws) '(project agent)))
      (stringp ws)))

(defun macher-agent-context-lookup (ws-or-id &optional _root)
  "Deterministically resolve a context from WS-OR-ID without ambient side effects.

WS-OR-ID may be a context, workspace structure, or buffer.

Return the resolved context structure, or nil if not found.
Side effects: None."
  (cond
   ((null ws-or-id) nil)
   ((or (macher-agent-context-p ws-or-id) (macher-agent-valid-context-p ws-or-id))
    ws-or-id)
   ((bufferp ws-or-id)
    (when (buffer-live-p ws-or-id)
      (let ((ctx (buffer-local-value 'macher-agent--persistent-context ws-or-id)))
        (when (or (macher-agent-context-p ctx) (macher-agent-valid-context-p ctx))
          ctx))))
   ((and (boundp 'macher-agent-active-workspaces)
         (hash-table-p macher-agent-active-workspaces))
    (or (when-let* ((ws-id (macher-agent-workspace-project-root ws-or-id)))
          (let ((exp (expand-file-name ws-id))
                (true-exp (file-truename (expand-file-name ws-id))))
            (or (gethash exp macher-agent-active-workspaces)
                (gethash (file-name-as-directory exp) macher-agent-active-workspaces)
                (gethash (directory-file-name exp) macher-agent-active-workspaces)
                (gethash true-exp macher-agent-active-workspaces)
                (gethash (file-name-as-directory true-exp) macher-agent-active-workspaces)
                (gethash (directory-file-name true-exp) macher-agent-active-workspaces))))
        (macher-agent-resolve-context ws-or-id)))
   (t
    (macher-agent-resolve-context ws-or-id))))

(defun macher-agent--workspace-get-hash-table (ws-or-ctx key &optional default)
  "Retrieve or initialise hash-table under KEY for WS-OR-CTX deterministically.

WS-OR-CTX is a workspace object or context structure.
KEY is the lookup key.
DEFAULT is the default value if unresolved."
  (let ((ctx (or (when (macher-agent-valid-context-p ws-or-ctx) ws-or-ctx)
                 (macher-agent-context-lookup ws-or-ctx)
                 (macher-agent-resolve-context ws-or-ctx))))
    (if ctx
        (let ((plugins (macher-agent-context-plugins ctx)))
          (or (when (macher-agent--plist-p plugins)
                (plist-get plugins key))
              (let ((ht (make-hash-table :test 'equal)))
                (setf (macher-agent-context-plugins ctx)
                      (plist-put (copy-sequence plugins) key ht))
                ht)))
      (or default (make-hash-table :test 'equal)))))

(defun macher-agent-workspace-vfs-buffers (ws-or-ctx)
  "Retrieve the VFS buffers hash-table for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a hash-table.
Side effects: Initialises `:vfs-buffers` in WS-OR-CTX if not present."
  (macher-agent--workspace-get-hash-table ws-or-ctx :vfs-buffers))

(defun macher-agent-workspace-mtime-tracker (ws-or-ctx)
  "Retrieve the mtime tracker hash-table for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a hash-table.
Side effects: Initialises `:mtime-tracker` in WS-OR-CTX if not present."
  (macher-agent--workspace-get-hash-table ws-or-ctx :mtime-tracker))

(defun macher-agent-workspace-tools-registry (ws-or-ctx)
  "Retrieve the tools registry hash-table for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a hash-table."
  (let ((ctx (or (when (macher-agent-valid-context-p ws-or-ctx) ws-or-ctx)
                 (macher-agent-resolve-context ws-or-ctx)
                 (macher-agent-context-lookup ws-or-ctx))))
    (if ctx
        (let ((tools (macher-agent-context-tools ctx)))
          (if (hash-table-p tools)
              tools
            (let ((plugins (macher-agent-context-plugins ctx)))
              (or (when (and (macher-agent--plist-p plugins)
                             (hash-table-p (plist-get plugins :tools-registry)))
                    (plist-get plugins :tools-registry))
                  (let ((ht (make-hash-table :test 'equal)))
                    (setf (macher-agent-context-tools ctx) ht)
                    ht)))))
      macher-agent-tools-registry)))

(defun macher-agent-workspace-skills-alist (ws-or-ctx)
  "Retrieve the skills alist for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return an alist."
  (let ((ctx (or (when (macher-agent-valid-context-p ws-or-ctx) ws-or-ctx)
                 (macher-agent-resolve-context ws-or-ctx)
                 (macher-agent-context-lookup ws-or-ctx))))
    (if ctx
        (or (macher-agent-context-skills ctx)
            (let ((plugins (macher-agent-context-plugins ctx)))
              (when (macher-agent--plist-p plugins)
                (plist-get plugins :skills-alist))))
      macher-agent-global-skills-alist)))

(defun macher-agent-workspace-agent ()
  "Identify if the current buffer is a workspace and return the workspace.
Return the workspace struct, or nil."
  (when (bound-and-true-p macher-agent--is-workspace)
    (bound-and-true-p macher--workspace)))

(defun macher-agent-workspace-active-subagents (ws-or-ctx)
  "Retrieve the active subagents list for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a list of subagent entries."
  (when-let* ((ctx (or (when (macher-agent-valid-context-p ws-or-ctx) ws-or-ctx)
                       (macher-agent-resolve-context ws-or-ctx)
                       (macher-agent-context-lookup ws-or-ctx))))
    (or (macher-agent-context-subagents ctx)
        (let ((plugins (macher-agent-context-plugins ctx)))
          (when (macher-agent--plist-p plugins)
            (plist-get plugins :active-subagents))))))

(defun macher-agent--set-workspace-skills-alist (ws-or-ctx val)
  "Set the skills alist for WS-OR-CTX to VAL without mutating ambient globals.

WS-OR-CTX is a workspace object or context structure.
VAL is the skills alist to set.

Return VAL.
Side effects: Modifies the skills alist for WS-OR-CTX if resolvable, or global
if WS-OR-CTX is nil."
  (if-let* ((ctx (or (when (macher-agent-valid-context-p ws-or-ctx) ws-or-ctx)
                     (macher-agent-resolve-context ws-or-ctx)
                     (macher-agent-context-lookup ws-or-ctx))))
      (setf (macher-agent-context-skills ctx) val)
    (when (null ws-or-ctx)
      (setq macher-agent-global-skills-alist val)))
  val)

(gv-define-setter macher-agent-workspace-skills-alist (val ws-or-ctx)
  `(macher-agent--set-workspace-skills-alist ,ws-or-ctx ,val))

(defun macher-agent--set-workspace-tools-registry (ws-or-ctx val)
  "Set tools registry for WS-OR-CTX to VAL without mutating ambient globals.

WS-OR-CTX is a workspace object or context structure.
VAL is the tools registry hash-table to set.

Return VAL.
Side effects: Modifies tools registry for WS-OR-CTX if resolvable, or global
if WS-OR-CTX is nil."
  (if-let* ((ctx (or (when (macher-agent-valid-context-p ws-or-ctx) ws-or-ctx)
                     (macher-agent-resolve-context ws-or-ctx)
                     (macher-agent-context-lookup ws-or-ctx))))
      (setf (macher-agent-context-tools ctx) val)
    (when (null ws-or-ctx)
      (setq macher-agent-tools-registry val)))
  val)

(gv-define-setter macher-agent-workspace-tools-registry (val ws-or-ctx)
  `(macher-agent--set-workspace-tools-registry ,ws-or-ctx ,val))

(defun macher-agent--set-workspace-active-subagents (ws-or-ctx val)
  "Set the active subagents list for WS-OR-CTX to VAL.

WS-OR-CTX is a workspace object or context structure.
VAL is the list of active subagents to set.

Return VAL.
Side effects: Modifies the active subagents list for WS-OR-CTX."
  (when-let* ((ctx (or (when (macher-agent-valid-context-p ws-or-ctx) ws-or-ctx)
                       (macher-agent-resolve-context ws-or-ctx)
                       (macher-agent-context-lookup ws-or-ctx))))
    (setf (macher-agent-context-subagents ctx) val))
  val)

(gv-define-setter macher-agent-workspace-active-subagents (val ws-or-ctx)
  `(macher-agent--set-workspace-active-subagents ,ws-or-ctx ,val))

(defun macher-agent--copy-context-hash-tables (data)
  "Copy hash tables in DATA plist for context cloning.

DATA is the property list containing context data.

Return a copied property list with cloned hash-tables."
  (when data
    (let ((res (copy-sequence data)))
      (when-let* ((vfs-ht (plist-get res :vfs-buffers)))
        (setq res (plist-put res :vfs-buffers (copy-hash-table vfs-ht))))
      (when-let* ((mtime-ht (plist-get res :mtime-tracker)))
        (setq res (plist-put res :mtime-tracker (copy-hash-table mtime-ht))))
      res)))

(defun macher-agent--clone-context (ctx)
  "Deep-copy and clone CTX.

CTX is the context structure.

Return the newly cloned context structure, or nil."
  (cl-assert (or (macher-agent-context-p ctx) (and (fboundp 'macher-context-p) (macher-context-p ctx))) nil "CTX must be a valid context, got: %S" ctx)
  (cond
   ((macher-agent-context-p ctx)
    (let* ((new-ctx (macher-agent--copy-context ctx))
           (orig-plugins (macher-agent-context-plugins ctx))
           (new-plugins (macher-agent--copy-context-hash-tables orig-plugins)))
      (setf (macher-agent-context-plugins new-ctx) (copy-tree new-plugins))
      (when-let* ((tools (macher-agent-context-tools ctx)))
        (setf (macher-agent-context-tools new-ctx)
              (if (hash-table-p tools) (copy-hash-table tools) (copy-tree tools))))
      (when-let* ((skills (macher-agent-context-skills ctx)))
        (setf (macher-agent-context-skills new-ctx) (copy-tree skills)))
      (when-let* ((subs (macher-agent-context-subagents ctx)))
        (setf (macher-agent-context-subagents new-ctx) (copy-tree subs)))
      new-ctx))
   (t
    (let* ((orig-data (when (fboundp 'macher-context-data) (macher-context-data ctx)))
           (new-data (macher-agent--copy-context-hash-tables orig-data))
           (prompt (when (fboundp 'macher-context-prompt) (macher-context-prompt ctx)))
           (ws (when (fboundp 'macher-context-workspace) (macher-context-workspace ctx)))
           (contents (when (fboundp 'macher-context-contents) (macher-context-contents ctx)))
           (new-ctx (macher-agent--make-vfs-context
                     :workspace ws
                     :contents (copy-tree contents)
                     :prompt prompt)))
      (when prompt
        (setf (macher-agent-context-prompt new-ctx) prompt))
      (when new-data
        (setf (macher-agent-context-plugins new-ctx)
              (append new-data (macher-agent-context-plugins new-ctx))))
      (when (and (fboundp 'macher-context-dirty-p) (macher-context-dirty-p ctx))
        (macher-agent--set-context-dirty-p new-ctx t))
      new-ctx))))

(defun macher-agent--inject-context-state (context &optional directives)
  "Inject the active CONTEXT and optional DIRECTIVES into the buffer.

CONTEXT is the active context structure.
DIRECTIVES is the optional directives alist.

Return nil.
Side effects: Sets buffer-local variables
`macher-agent--persistent-context` and `gptel-directives`."
  (when context
    (setq-local macher-agent--persistent-context context)
    (when directives
      (if (boundp 'gptel-directives)
          (setq gptel-directives directives)
        (setq-local gptel-directives directives)))))

(defun macher-agent-apply-patch ()
  "Apply the active patch from `diff-mode' context back to physical files.

Return nil.
Side effects: Invokes external process (`git` or `patch`) to apply patch."
  (interactive)
  (unless (derived-mode-p 'diff-mode) (user-error "Not in a patch/diff buffer"))
  (let* ((patch-content (buffer-substring-no-properties (point-min) (point-max)))
         (ctx (or (bound-and-true-p macher-agent--persistent-context)
                  (ignore-errors (macher-agent-resolve-context (current-buffer)))
                  (ignore-errors (macher-agent-resolve-context))))
         (root (if ctx
                   (macher-agent-context-root ctx)
                 (or (locate-dominating-file default-directory ".git") default-directory)))
         (default-directory (file-name-as-directory (expand-file-name root)))
         (use-git (locate-dominating-file default-directory ".git"))
         (cmd (if use-git "git" "patch"))
         (args (if use-git '("apply" "-p1" "-") '("-p1"))))
    (with-temp-buffer
      (insert patch-content)
      (let ((exit-code (apply #'call-process-region (point-min) (point-max) cmd nil "*macher-patch-out*" nil args)))
        (if (= exit-code 0)
            (progn
              (message "SUCCESS: Patch applied safely via %s from %s" cmd default-directory)
              (when (get-buffer "*macher-patch-out*")
                (kill-buffer "*macher-patch-out*")))
          (pop-to-buffer "*macher-patch-out*")
          (message "ERROR: Failed to apply patch safely. Check *macher-patch-out* for details."))))))

(provide 'macher-agent-macher)
;;; macher-agent-macher.el ends here
