;;; macher-agent-macher.el --- Bridge to Macher Core  -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridge implementation for Macher Agent to interact with Macher Core.

;;; Code:

(require 'macher)
(require 'cl-lib)
(require 'subr-x)
(require 'macher-agent-core)

(defvar macher-agent--task-registry)

(defun macher-agent--unwrap-workspace (ws)
  "Unwrap WS if wrapped in an agent tag cell.

Strip the leading agent tag from WS if it is structured as a tagged cons cell.

Return the unwrapped workspace structure or WS unchanged.

Side effects: None."
  (if (and (consp ws) (eq (car ws) 'agent))
      (cdr ws)
    ws))

(defun macher-agent--safe-workspace-hash (workspace &rest _args)
  "Compute a safe MD5 hash for WORKSPACE without recursive traversal.

Calculate a deterministic MD5 hash string for WORKSPACE to prevent recursion
depth limit failures during workspace hashing.  _ARGS accommodates additional
arguments supplied by advice wrappers.

Return the MD5 hash string.

Side effects: None."
  (let* ((unwrapped (macher-agent--unwrap-workspace workspace))
         (path (cond
                ((and (recordp unwrapped) (eq (type-of unwrapped) 'macher-agent-workspace))
                 (macher-agent-workspace-project-root unwrapped))
                ((and (consp unwrapped) (eq (car unwrapped) 'project))
                 (cdr unwrapped))
                (t (format "%s" unwrapped)))))
    (md5 (or path "unknown-workspace"))))

(cl-defun macher-agent--make-vfs-context (&key workspace contents prompt)
  "Create a native `macher-context' struct using `macher--make-context'.

Construct a new context structure with WORKSPACE, CONTENTS, and
PROMPT.

Return the newly created `macher-context' struct.

Side effects: Dynamically binds
`macher-agent--persistent-context' to nil during
construction."
  (let ((macher-agent--persistent-context nil))
    (macher--make-context :workspace workspace :contents contents :prompt prompt)))

(defun macher-agent--create-and-tag-vfs-context (workspace contents prompt)
  "Create a VFS context for WORKSPACE with CONTENTS and PROMPT, tagged dirty.

Construct a VFS context structure for WORKSPACE containing CONTENTS and PROMPT,
storing prompt metadata and marking the context as dirty.

Return the created `macher-context' struct.

Side effects: Mutates prompt and dirty state on the created context structure."
  (let ((ctx (macher-agent--make-vfs-context :workspace workspace
                                             :contents contents
                                             :prompt prompt)))
    (when prompt
      (macher-agent--set-context-prompt ctx prompt)
      (macher-agent--set-context-data ctx :prompt prompt))
    (macher-agent--set-context-dirty-p ctx t)
    ctx))

(defun macher-agent--get-context-workspace (ctx)
  "Retrieve the tagged workspace structure from context CTX."
  (cond
   ((macher-agent-valid-context-p ctx)
    (macher-context-workspace ctx))
   ((and (consp ctx) (memq (car ctx) '(agent project)))
    ctx)
   ((and ctx (macher-agent-workspace-p ctx))
    ctx)
   (t nil)))

(defmacro macher-agent--def-context-accessor (name accessor &optional setter-name docstring)
  "Define a safe accessor and optional setter for a Macher context struct.

Generate a getter function NAME and optional setter function
SETTER-NAME for field ACCESSOR on a `macher-context' structure.

Return expanded macro defun form.
Side effects: Defines new function symbols in Emacs runtime."
  (let* ((getter-doc (format "Safely access `%s' on CTX.\n\n%s\n\nReturn field value, or nil.\n\nSide effects: None."
                             accessor (or docstring (format "Safely access `%s' on CTX." accessor))))
         (setter-doc (format "Safely set `%s' on CTX.\n\nMutate field `%s' on context CTX with VAL.\n\nReturn new value VAL, or nil.\n\nSide effects: Mutates CTX structure." accessor accessor))
         (getter-body `(and (macher-agent-valid-context-p ctx) (fboundp ',accessor) (,accessor ctx)))
         (setter-body `(when (and (macher-agent-valid-context-p ctx) (fboundp ',accessor))
                         (ignore-errors (with-no-warnings (setf (,accessor ctx) val)))))
         (getter-form (list 'defun name '(ctx) getter-doc getter-body)))
    (if setter-name
        (list 'progn
              getter-form
              (list 'defun setter-name '(ctx val) setter-doc setter-body))
      getter-form)))

(macher-agent--def-context-accessor
 macher-agent--get-context-contents macher-context-contents macher-agent--set-context-contents)
(macher-agent--def-context-accessor
 macher-agent--get-context-dirty-p macher-context-dirty-p macher-agent--set-context-dirty-p)
(macher-agent--def-context-accessor
 macher-agent--get-context-shadow-buffers
 macher-context-shadow-buffers
 macher-agent--set-context-shadow-buffers "Safely retrieve shadow buffers if defined upstream.")

(defun macher-agent--get-fsm-latest ()
  "Get the active finite-state machine (FSM) if bound.

Locate and return the active finite-state machine instance
from package global variables `macher--fsm-latest',
`gptel--fsm', or `gptel--fsm-last'.

Return the active FSM structure, or nil if none is bound.

Side effects: None."
  (macher-agent-get-active-fsm))

(defun macher-agent--inject-context-into-fsm-info (agent-ctx &optional fsm)
  "Inject AGENT-CTX into FSM info property list safely.

Store AGENT-CTX in the info plist of finite-state machine FSM
under keys `:macher--context' and `:macher-agent-context'.
FSM defaults to active `gptel--fsm'.

Return non-nil if injected successfully, or nil otherwise.

Side effects: Mutates the info property list of FSM."
  (when-let* ((fsm-obj (macher-agent-get-active-fsm fsm)))
    (let* ((info (gptel-fsm-info fsm-obj))
           (new-info (if info (copy-sequence info) nil)))
      (setq new-info (plist-put new-info :macher--context agent-ctx))
      (setq new-info (plist-put new-info :macher-agent-context agent-ctx))
      (setf (gptel-fsm-info fsm-obj) new-info))
    t))

(defmacro macher-agent--with-protected-context-contents (ctx &rest body)
  "Execute BODY preserving the VFS contents of CTX.

Capture a copy of the VFS contents of context CTX prior to
executing BODY and restore the original contents via
`unwind-protect' upon completion or non-local exit.

Return the result of evaluating BODY.

Side effects: Restores original VFS contents on CTX after
BODY executes."
  (declare (indent 1) (debug t))
  (let ((ctx-sym (gensym "ctx"))
        (protected-sym (gensym "protected")))
    `(let*
         ((,ctx-sym ,ctx)
          (,protected-sym
           (and ,ctx-sym
                (copy-sequence (macher-agent--get-context-contents ,ctx-sym)))))
       (unwind-protect
           (progn ,@body)
         (when (and ,ctx-sym ,protected-sym)
           (macher-agent--set-context-contents ,ctx-sym ,protected-sym))))))

(defun macher-agent--context-p (ctx)
  "Determine whether CTX is a valid `macher-context' struct.

Return non-nil if CTX is a context struct, or nil otherwise."
  (and ctx
       (or (and (fboundp 'macher-context-p) (macher-context-p ctx))
           (and (recordp ctx)
                (memq (type-of ctx) '(macher-context
                                      cl-struct-macher-context
                                      macher-agent-task-context
                                      cl-struct-macher-agent-task-context)))
           (and (recordp ctx)
                (> (length ctx) 0)
                (memq (aref ctx 0) '(cl-struct-macher-context
                                     cl-struct-macher-agent-task-context)))
           (and (vectorp ctx)
                (> (length ctx) 0)
                (memq (aref ctx 0) '(cl-struct-macher-context
                                     cl-struct-macher-agent-task-context)))
           (macher-agent-task-context-p ctx))))

(macher-agent--def-context-accessor
 macher-agent--get-context-raw-data macher-context-data macher-agent--set-context-raw-data
 "Get or set the raw native data property list from CTX.")

(defun macher-agent--get-context-data (ctx key &optional default)
  "Retrieve KEY from the native data slot of CTX.

CTX is the context structure.
KEY is the lookup key symbol.
DEFAULT is the value returned if KEY is not present.

Return the value or DEFAULT."
  (if (macher-agent-valid-context-p ctx)
      (let* ((data (macher-context-data ctx))
             (val (macher-agent--extract-prop data key)))
        (if (eq val 'macher-missing) default val))
    default))

(defun macher-agent--set-context-data (ctx key val)
  "Set KEY to VAL in the native data slot of CTX.

CTX is the context structure.
KEY is the key symbol.
VAL is the value to store.

Return VAL.
Side effects: Modifies native data slot of CTX."
  (when (macher-agent-valid-context-p ctx)
    (let ((data (macher-context-data ctx)))
      (cond
       ((hash-table-p data)
        (puthash key val data))
       ((and (consp data) (consp (car data)))
        (let ((cell (assq key data)))
          (if cell
              (setcdr cell val)
            (setf (macher-context-data ctx) (cons (cons key val) data)))))
       (t
        (setf (macher-context-data ctx)
              (plist-put (if (macher-agent--plist-p data) data nil) key val))))))
  val)

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
                 (macher-agent--get-context-workspace ctx)
               (macher-agent--get-active-workspace)))
         (patch-buf (and (fboundp 'macher-patch-buffer)
                         (macher-patch-buffer ws))))
    patch-buf))

(defun macher-agent--resolve-context-lazy-init ()
  "Attempt lazy initialisation of context for current directory.

Return non-nil if initialisation succeeded, or signals an error if not allowed.
Side effects: May initialise workspace state for current directory."
  (unless macher-agent--allow-lazy-init
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
  (when root
    (let ((expanded (expand-file-name root)))
      (puthash expanded context macher-agent-active-workspaces)
      (puthash (file-name-as-directory expanded) context macher-agent-active-workspaces)
      (puthash (directory-file-name expanded) context macher-agent-active-workspaces))))

(defun macher-agent--get-expanded-root (state)
  "Ensure STATE contains an `:expanded-root' property.

Calculates expanded project root path from `:input' or `default-directory' if not already
present or if nil in STATE.  Validates string paths against project boundaries.

STATE is the context resolution state plist.

Return STATE with `:expanded-root' set to the expanded path string or nil.
Side effects: None."
  (if (and (macher-agent--plist-p state) (plist-member state :expanded-root))
      state
    (let* ((input (when (macher-agent--plist-p state) (plist-get state :input)))
           (ws-id (macher-agent-extract-workspace-id input))
           (raw-root (cond
                      ((stringp ws-id) ws-id)
                      ((and (consp ws-id) (eq (car ws-id) 'project)) (cdr ws-id))
                      ((and (consp ws-id) (eq (car ws-id) 'agent)) (cdr ws-id))
                      ((macher-agent-workspace-p ws-id)
                       (macher-agent-workspace-project-root ws-id))
                      ((and (consp ws-id) (stringp (cdr ws-id))) (cdr ws-id))
                      (t (macher-agent-root default-directory))))
           (expanded (and raw-root (expand-file-name raw-root)))
           (valid-root (when expanded
                         (if (stringp ws-id)
                             (let* ((proj-bound (file-name-as-directory (expand-file-name (macher-agent-root default-directory))))
                                    (exp-dir (file-name-as-directory expanded)))
                               (if (string-prefix-p proj-bound exp-dir)
                                   expanded
                                 nil))
                           expanded))))
      (if (macher-agent--plist-p state)
          (plist-put state :expanded-root valid-root)
        (list :input input :resolved nil :expanded-root valid-root)))))

(defun macher-agent-ctx-pipe--explicit (state)
  "Context pipeline step 1: Resolve context explicitly from STATE input.

If `:resolved' in STATE is nil and `:input' in STATE satisfies
`macher-agent-valid-context-p', set `:resolved' to that context structure.

STATE is the context resolution state plist (:input ... :resolved
... :expanded-root ...).

Return the updated STATE plist.
Side effects: None."
  (if (and (macher-agent--plist-p state) (plist-get state :resolved))
      state
    (let ((input (when (macher-agent--plist-p state) (plist-get state :input))))
      (if (macher-agent-valid-context-p input)
          (plist-put state :resolved input)
        state))))

(defun macher-agent-ctx-pipe--buffer (state)
  "Context pipeline step: Resolve context from explicit buffer in STATE input.

If `:resolved' in STATE is nil and `:input' in STATE is a live buffer,
extract its buffer-local `macher-agent--persistent-context'.

STATE is the context resolution state plist (:input ... :resolved ... :expanded-root ...).

Return the updated STATE plist.
Side effects: None."
  (if (and (macher-agent--plist-p state) (plist-get state :resolved))
      state
    (let ((input (when (macher-agent--plist-p state) (plist-get state :input))))
      (if (and (bufferp input) (buffer-live-p input))
          (if-let* ((ctx (buffer-local-value 'macher-agent--persistent-context input))
                    ((macher-agent-valid-context-p ctx)))
              (plist-put state :resolved ctx)
            state)
        state))))

(defun macher-agent-ctx-pipe--workspace-id (state)
  "Context pipeline step 2.5: Resolve context from workspace-id fallback."
  (if (and (macher-agent--plist-p state) (plist-get state :resolved))
      state
    (let* ((input (when (macher-agent--plist-p state) (plist-get state :input)))
           (ws-id (macher-agent-extract-workspace-id input)))
      (if ws-id
          (let* ((root-path (cond
                             ((stringp ws-id) ws-id)
                             ((and (consp ws-id) (memq (car ws-id) '(project agent))) (cdr ws-id))
                             ((macher-agent-workspace-p ws-id)
                              (macher-agent-workspace-project-root ws-id))
                             (t default-directory)))
                 (expanded (and root-path (expand-file-name root-path)))
                 (existing-ctx (when (and expanded (boundp 'macher-agent-active-workspaces))
                                 (or (gethash expanded macher-agent-active-workspaces)
                                     (gethash (file-name-as-directory expanded) macher-agent-active-workspaces)
                                     (gethash (directory-file-name expanded) macher-agent-active-workspaces)))))
            (if existing-ctx
                (plist-put state :resolved existing-ctx)
              (let* ((ws (cond
                          ((and (consp ws-id) (memq (car ws-id) '(agent project))) ws-id)
                          (t (cons 'agent root-path))))
                     (ctx (when (fboundp 'macher--make-context)
                            (macher--make-context :workspace ws :contents nil))))
                (when ctx
                  (when (boundp 'macher-agent-active-workspaces)
                    (puthash expanded ctx macher-agent-active-workspaces)
                    (puthash (file-name-as-directory expanded) ctx macher-agent-active-workspaces)
                    (puthash (directory-file-name expanded) ctx macher-agent-active-workspaces))
                  (plist-put state :resolved ctx)))))
        state))))

(defun macher-agent-ctx-pipe--fsm (state)
  "Context pipeline step 4: Resolve context from FSM input in STATE.

If `:resolved' in STATE is nil and `:input' in STATE is a finite-state machine
\(FSM), extract and set `:resolved' to its active context structure.

STATE is the context resolution state plist (:input ... :resolved
... :expanded-root ...).

Return the updated STATE plist.
Side effects: None."
  (if (and (macher-agent--plist-p state) (plist-get state :resolved))
      state
    (let ((input (when (macher-agent--plist-p state) (plist-get state :input))))
      (if-let* ((ctx (and input (macher-agent--extract-fsm-context input))))
          (plist-put state :resolved ctx)
        state))))

(defun macher-agent--input-specifies-workspace-p (input)
  "Return non-nil if INPUT explicitly specifies a workspace or root."
  (not (null (macher-agent-extract-workspace-id input))))

(defun macher-agent-ctx-pipe--subagent (state)
  "Context pipeline step: Resolve subagent or persistent context in STATE.

If `:resolved' in STATE is nil and buffer-local `macher-agent--persistent-context'
is bound, set `:resolved' to it if persistent context matches the active workspace root,
or if current buffer is a subagent or orchestrator with bound persistent context.

STATE is the context resolution state plist \(:input ... :resolved ...
:expanded-root ...\).

Return the updated STATE plist.
Side effects: May populate `:expanded-root' in STATE."
  (if (and (macher-agent--plist-p state) (plist-get state :resolved))
      state
    (let* ((st (macher-agent--get-expanded-root state))
           (input (when (macher-agent--plist-p st) (plist-get st :input)))
           (active-root-expanded (when (macher-agent--plist-p st) (plist-get st :expanded-root)))
           (fsm-buf (when-let* ((fsm (macher-agent-get-active-fsm))
                                ((fboundp 'gptel-fsm-info))
                                (info (ignore-errors (gptel-fsm-info fsm)))
                                ((macher-agent--plist-p info)))
                      (plist-get info :buffer)))
           (target-buf (cond
                        ((and (bufferp input) (buffer-live-p input)) input)
                        ((and fsm-buf (buffer-live-p fsm-buf)) fsm-buf)
                        (t (current-buffer))))
           (pers-ctx (and (buffer-live-p target-buf)
                          (buffer-local-value 'macher-agent--persistent-context target-buf))))
      (if (and pers-ctx (macher-agent-valid-context-p pers-ctx))
          (let ((pers-matches (macher-agent--match-persistent-context pers-ctx active-root-expanded))
                (foreign-ws (and (macher-agent--input-specifies-workspace-p input)
                                 (not (macher-agent--match-persistent-context pers-ctx active-root-expanded)))))
            (if (or pers-matches
                    (buffer-local-value 'macher-agent--is-subagent target-buf)
                    (bound-and-true-p macher-agent--is-subagent)
                    (not foreign-ws))
                (plist-put st :resolved pers-ctx)
              st))
        st))))

(defun macher-agent-ctx-pipe--canonical (state)
  "Context pipeline step 6: Resolve canonical context from workspace in STATE.

If `:resolved' in STATE is nil, look up active workspace context in
`macher-agent-active-workspaces' using the expanded project root.
If current buffer is a subagent, clone the canonical context to provide
an isolated clone.

STATE is the context resolution state plist \(:input ... :resolved ...
:expanded-root ...\).

Return the updated STATE plist.
Side effects: None."
  (if (and (macher-agent--plist-p state) (plist-get state :resolved))
      state
    (let* ((st (macher-agent--get-expanded-root state))
           (active-root-expanded (when (macher-agent--plist-p st) (plist-get st :expanded-root))))
      (if-let*
          ((canonical-ctx
            (when active-root-expanded
              (or (gethash active-root-expanded macher-agent-active-workspaces)
                  (gethash
                   (file-name-as-directory active-root-expanded)
                   macher-agent-active-workspaces)
                  (gethash
                   (directory-file-name active-root-expanded) macher-agent-active-workspaces)
                  (macher-agent--find-active-workspace-in-ancestors active-root-expanded)))))
          (plist-put st :resolved (if (and (boundp 'macher-agent--is-subagent) macher-agent--is-subagent)
                                      (macher-agent--clone-context canonical-ctx)
                                    canonical-ctx))
        st))))

(defun macher-agent-ctx-pipe--fsm-fallback (state)
  "Context pipeline step 7: Resolve context from latest FSM fallback in STATE.

If `:resolved' in STATE is nil, attempt to extract context from
`macher-agent--get-fsm-latest'.

STATE is the context resolution state plist \(:input ... :resolved ...
:expanded-root ...\).

Return the updated STATE plist.
Side effects: None."
  (if (and (macher-agent--plist-p state) (plist-get state :resolved))
      state
    (if-let* ((fsm (macher-agent--get-fsm-latest))
              (ctx (macher-agent--extract-fsm-context fsm)))
        (plist-put state :resolved ctx)
      state)))

(defun macher-agent-ctx-pipe--lazy-init (state)
  "Context pipeline step 8: Resolve context via lazy initialisation in STATE.

If `:resolved' in STATE is nil, attempt lazy workspace initialisation.

STATE is the context resolution state plist \(:input ... :resolved ...
:expanded-root ...\).

Return the updated STATE plist.
Side effects: May initialise workspace state for current directory."
  (if (and (macher-agent--plist-p state) (plist-get state :resolved))
      state
    (if-let* ((ctx (ignore-errors (macher-agent--resolve-context-lazy-init)))
              ((macher-agent-valid-context-p ctx)))
        (plist-put state :resolved ctx)
      state)))

(defun macher-agent-context-resolution-install ()
  "Install context resolution pipeline steps."
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--explicit 10)
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--buffer 15)
  (when (fboundp 'macher-agent-resolve-from-transit-payload)
    (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-resolve-from-transit-payload 15))
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--subagent 22)
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--workspace-id 25)
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--fsm 40)
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--canonical 60)
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--fsm-fallback 70)
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

(defun macher-agent-workspace-project-root (ws)
  "Retrieve the project root from workspace WS.

WS is the workspace cons cell, string, or struct.

Return the project root path string."
  (cond
   ((and (fboundp 'project-root) (ignore-errors (project-root ws)))
    (expand-file-name (project-root ws)))

   ((and (consp ws) (eq (car ws) 'project)) (expand-file-name (cdr ws)))
   ((and (consp ws) (eq (car ws) 'agent) (stringp (cdr ws))) (expand-file-name (cdr ws)))
   ((and (consp ws) (consp (car ws)) (eq (caar ws) 'project))
    (expand-file-name (cdar ws)))
   ((and (consp ws) (consp (car ws)) (eq (caar ws) 'agent) (stringp (cdar ws)))
    (expand-file-name (cdar ws)))
   ((stringp ws) (expand-file-name ws))
   ((and (consp ws) (stringp (cdr ws))) (expand-file-name (cdr ws)))
   (t nil)))

(defun macher-agent-workspace-vfs-buffers (ws-or-ctx)
  "Retrieve the VFS buffers hash-table for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a hash-table.
Side effects: Initialises `:vfs-buffers` in WS-OR-CTX if not present."
  (let ((ctx (macher-agent-resolve-context ws-or-ctx)))
    (if ctx
        (or (macher-agent--get-context-data ctx :vfs-buffers)
            (let ((ht (make-hash-table :test 'equal)))
              (macher-agent--set-context-data ctx :vfs-buffers ht)
              ht))
      (make-hash-table :test 'equal))))

(defun macher-agent-workspace-mtime-tracker (ws-or-ctx)
  "Retrieve the mtime tracker hash-table for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a hash-table.
Side effects: Initialises `:mtime-tracker` in WS-OR-CTX if not present."
  (let ((ctx (macher-agent-resolve-context ws-or-ctx)))
    (if ctx
        (or (macher-agent--get-context-data ctx :mtime-tracker)
            (let ((ht (make-hash-table :test 'equal)))
              (macher-agent--set-context-data ctx :mtime-tracker ht)
              ht))
      (make-hash-table :test 'equal))))

(defun macher-agent-workspace-tools-registry (ws-or-ctx)
  "Retrieve the tools registry hash-table for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a hash-table.
Side effects: Initialises `:tools-registry` in WS-OR-CTX if not present."
  (let ((ctx (macher-agent-resolve-context ws-or-ctx)))
    (if ctx
        (or (macher-agent--get-context-data ctx :tools-registry)
            (let ((ht (make-hash-table :test 'equal)))
              (macher-agent--set-context-data ctx :tools-registry ht)
              ht))
      macher-agent-tools-registry)))

(defun macher-agent-workspace-skills-alist (ws-or-ctx)
  "Retrieve the skills alist for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return an alist."
  (let ((ctx (macher-agent-resolve-context ws-or-ctx)))
    (if ctx
        (macher-agent--get-context-data ctx :skills-alist)
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
  (when-let* ((ctx (macher-agent-resolve-context ws-or-ctx)))
    (macher-agent--get-context-data ctx :active-subagents)))

(defun macher-agent--set-workspace-skills-alist (ws-or-ctx val)
  "Set the skills alist for WS-OR-CTX to VAL.

WS-OR-CTX is a workspace object or context structure.
VAL is the skills alist to set.

Return VAL.
Side effects: Modifies the skills alist for WS-OR-CTX or global skills alist."
  (if-let* ((ctx (macher-agent-resolve-context ws-or-ctx)))
      (macher-agent--set-context-data ctx :skills-alist val)
    (setq macher-agent-global-skills-alist val))
  val)

(gv-define-setter macher-agent-workspace-skills-alist (val ws-or-ctx)
  `(macher-agent--set-workspace-skills-alist ,ws-or-ctx ,val))

(defun macher-agent--set-workspace-tools-registry (ws-or-ctx val)
  "Set the tools registry for WS-OR-CTX to VAL.

WS-OR-CTX is a workspace object or context structure.
VAL is the tools registry hash-table to set.

Return VAL.
Side effects: Modifies the tools registry for WS-OR-CTX or global registry."
  (let ((ctx (macher-agent-resolve-context ws-or-ctx)))
    (if ctx
        (macher-agent--set-context-data ctx :tools-registry val)
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
  (when-let* ((ctx (macher-agent-resolve-context ws-or-ctx)))
    (macher-agent--set-context-data ctx :active-subagents val))
  val)

(gv-define-setter macher-agent-workspace-active-subagents (val ws-or-ctx)
  `(macher-agent--set-workspace-active-subagents ,ws-or-ctx ,val))

(defun macher-agent--match-persistent-context (pers-ctx expanded-root)
  "Check if PERS-CTX root matches EXPANDED-ROOT.

PERS-CTX is the persistent context structure.
EXPANDED-ROOT is the expanded project root directory path string.

Return PERS-CTX if matching, otherwise nil."
  (when (and pers-ctx expanded-root)
    (let ((pers-root (ignore-errors (expand-file-name (macher-agent-context-root pers-ctx)))))
      (when (and pers-root
                 (or (string= pers-root expanded-root)
                     (string= (file-name-as-directory pers-root) (file-name-as-directory expanded-root))
                     (string= (directory-file-name pers-root) (directory-file-name expanded-root))
                     (string= pers-root (file-name-as-directory expanded-root))
                     (string= (file-name-as-directory pers-root) expanded-root)
                     (and (file-directory-p expanded-root)
                          (string-prefix-p (file-name-as-directory pers-root)
                                           (file-name-as-directory expanded-root)))))
        pers-ctx))))

(defun macher-agent--find-active-workspace-in-ancestors (expanded-root)
  "Find active workspace context by walking up directory tree from EXPANDED-ROOT.

EXPANDED-ROOT is the expanded project root directory path string.

Return the matching workspace context structure, or nil."
  (when expanded-root
    (cl-loop for dir = (file-name-as-directory expanded-root)
             then (let ((p (file-name-directory (directory-file-name dir))))
                    (if (equal p dir) nil p))
             while dir
             thereis (or (gethash (file-name-as-directory dir) macher-agent-active-workspaces)
                         (gethash (directory-file-name dir) macher-agent-active-workspaces)
                         (gethash dir macher-agent-active-workspaces)))))

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
  (when ctx
    (let*
        ((orig-data (when (macher-agent-valid-context-p ctx)
                      (macher-agent--get-context-raw-data ctx)))
         (new-data (macher-agent--copy-context-hash-tables orig-data))
         (prompt (macher-agent--get-context-prompt ctx))
         (new-ctx (macher-agent--make-vfs-context
                   :workspace (macher-agent--get-context-workspace ctx)
                   :contents (copy-tree (macher-agent--get-context-contents ctx))
                   :prompt prompt)))
      (when prompt
        (macher-agent--set-context-prompt new-ctx prompt)
        (macher-agent--set-context-data new-ctx :prompt prompt))
      (when new-data
        (macher-agent--set-context-raw-data new-ctx new-data)
        (when prompt
          (macher-agent--set-context-data new-ctx :prompt prompt)))
      (when (macher-agent--get-context-dirty-p ctx)
        (macher-agent--set-context-dirty-p new-ctx t))
      new-ctx)))

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

(defun macher-agent--get-root (workspace)
  "Get the project root path of WORKSPACE.

WORKSPACE is the active workspace structure.

Return the project root path string."
  (macher-agent-workspace-project-root workspace))

(defun macher-agent--get-name (workspace)
  "Get a display name for WORKSPACE.

WORKSPACE is the active workspace structure.

Return the formatted name string."
  (concat "Agent: "
          (file-name-nondirectory
           (directory-file-name (macher-agent-workspace-project-root workspace)))))

(provide 'macher-agent-macher)
;;; macher-agent-macher.el ends here
