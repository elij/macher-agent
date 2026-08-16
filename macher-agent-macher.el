;;; macher-agent-macher.el --- Bridge to Macher Core  -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridge implementation for Macher Agent to interact with Macher Core.

;;; Code:

(require 'macher)
(require 'cl-lib)
(require 'subr-x)
(require 'macher-agent-core)
(require 'macher-agent-vfs)

(defvar macher-agent-tools-registry)
(defvar macher-agent-global-skills-alist)
(defvar macher-agent--task-registry)
(defvar macher-agent-active-workspaces (make-hash-table :test 'equal)
  "Registry mapping expanded project roots to their active persistent contexts.")

(defvar macher-agent-context-pipeline-functions
  '(macher-agent-ctx-pipe--explicit
    macher-agent-ctx-pipe--fsm
    macher-agent-ctx-pipe--subagent
    macher-agent-ctx-pipe--canonical
    macher-agent-ctx-pipe--fsm-fallback
    macher-agent-ctx-pipe--lazy-init)
  "Store list of pipeline functions for resolving active agent context.")

(defvar macher-agent--wrapped-tools-hash (make-hash-table :test 'eq)
  "Track wrapped `gptel-tool' instances in a hash table.

Store `gptel-tool' objects that have already been wrapped
by Macher Agent to prevent duplicate tool wrapping.

Return the hash table instance.

Side effects: Global variable storing hash table state.")

(defvar macher-agent--inhibit-patch-hook nil)

(defvar macher-agent-context-mutated-hook nil
  "Hook run when a VFS context is mutated.")
(defvar macher-agent--allow-lazy-init nil
  "When non-nil, allow lazy initialisation of workspace context.")

(defun macher-agent--get-workspace-root (ws)
  "Resolve the absolute project root of WS.

Retrieve the absolute directory path string representing the project root
from the workspace object WS.

Return the absolute project root path string.

Side effects: None."
  (macher-agent-workspace-project-root ws))

(defun macher-agent--unwrap-workspace (ws)
  "Unwrap WS if wrapped in an agent tag cell.

Strip the leading agent tag from WS if it is structured as a tagged cons cell.

Return the unwrapped workspace structure or WS unchanged.

Side effects: None."
  (if (and (consp ws) (eq (car ws) 'agent))
      (cdr ws)
    ws))

(defun macher-agent--pure-virtual-entry-p (entry)
  "Determine whether VFS ENTRY represents a pure virtual buffer.

Check if ENTRY corresponds to a live buffer without an associated disk file.

Return non-nil if ENTRY is a pure virtual buffer, or nil otherwise.

Side effects: None."
  (let* ((name (car entry))
         (live-buf (get-buffer name)))
    (and live-buf (null (buffer-file-name live-buf)))))

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
                ((and (consp workspace) (eq (car workspace) 'project))
                 (cdr workspace))
                (t (format "%s" workspace)))))
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
      (setf (macher-context-prompt ctx) prompt)
      (macher-agent--set-context-data ctx :prompt prompt))
    (macher-agent--set-context-dirty-p ctx t)
    ctx))

(defun macher-agent--prepare-patch-contexts (context fsm project-root)
  "Partition CONTEXT into isolated virtual and physical patch contexts.

Extract VFS entries from CONTEXT or FSM and split them according to
PROJECT-ROOT into dedicated virtual and physical context objects.

Return a list (VIRTUAL-CONTEXT PHYSICAL-CONTEXT PHYSICAL-CONTENTS).

Side effects: None."
  (let* ((vfs-ctx (or (ignore-errors (macher-agent-resolve-context (or fsm context))) context))
         (raw-contents (or (and context (macher-agent--get-context-contents context))
                           (and vfs-ctx (macher-agent--get-context-contents vfs-ctx))))
         (prompt (or (macher-agent--get-context-prompt context)
                     (macher-agent--get-context-data context :prompt)
                     (and vfs-ctx (or (macher-agent--get-context-prompt vfs-ctx)
                                      (macher-agent--get-context-data vfs-ctx :prompt)))))
         (categorised (macher-agent--partition-vfs-entries raw-contents project-root))
         (virtual-contents (car categorised))
         (physical-contents (cdr categorised))
         (macher-compatible-ws (cons 'project project-root))
         (v-ctx (and virtual-contents
                     (macher-agent--create-and-tag-vfs-context macher-compatible-ws
                                                               virtual-contents
                                                               prompt)))
         (p-ctx (and physical-contents
                     (macher-agent--create-and-tag-vfs-context macher-compatible-ws
                                                               physical-contents
                                                               prompt))))
    (list v-ctx p-ctx physical-contents)))

(defun macher-agent-prepare-upstream-payloads (context)
  "Split CONTEXT into independent, native macher-context structs.

Resolve the project root for the workspace in CONTEXT and construct separate
physical and virtual patch context structures.

Return a cons cell (PHYSICAL-CONTEXT . VIRTUAL-CONTEXT).

Side effects: None."
  (let* ((ws (macher-agent--get-context-workspace context))
         (project-root (macher-agent-workspace-project-root ws))
         (prepared (macher-agent--prepare-patch-contexts context nil project-root))
         (v-ctx (nth 0 prepared))
         (p-ctx (nth 1 prepared)))
    (cons p-ctx v-ctx)))

(defun macher-agent--build-and-rename-patch (ctx fsm-obj patch-type)
  "Build patch for CTX using FSM-OBJ and rename buffer according to PATCH-TYPE.

Construct a patch buffer for context CTX and finite-state machine FSM-OBJ if
CTX contains changes and patch generation is not suppressed.  Rename the
resulting patch buffer using PATCH-TYPE, for example, \"physical\" or \"virtual\".

Return the renamed patch buffer if generated, or nil.

Side effects: Creates or renames patch buffers in the Emacs runtime."
  (let* ((target-buf (when (and fsm-obj (fboundp 'gptel-fsm-info))
                       (ignore-errors (plist-get (gptel-fsm-info fsm-obj) :buffer))))
         (suppress-patch (if (buffer-live-p target-buf)
                             (buffer-local-value 'macher-agent--suppress-patch target-buf)
                           (bound-and-true-p macher-agent--suppress-patch))))
    (when (and (not suppress-patch)
               (macher-agent--context-has-changes-p ctx))
      (macher--build-patch ctx fsm-obj)
      (when-let* ((buf (macher-patch-buffer (macher-context-workspace ctx)))
                  (name (buffer-name buf)))
        (let ((new-name (if (string-match "^\\*macher-patch\\(.*\\)\\*$" name)
                            (concat "*macher-" patch-type "-patch" (match-string 1 name) "*")
                          (concat "*macher-" patch-type "-patch*"))))
          (with-current-buffer buf
            (rename-buffer new-name t)
            (current-buffer)))))))

(defun macher-agent--display-patch-buffers (generated-buffers)
  "Display GENERATED-BUFFERS in split windows if multiple buffers exist.

Arrange window layout to show all patch buffers in GENERATED-BUFFERS when
more than one patch buffer is present.

Return nil.

Side effects: Alters window configuration in the active frame."
  (when (> (length generated-buffers) 1)
    (let ((bufs (nreverse generated-buffers)))
      (delete-other-windows)
      (split-window-vertically)
      (set-window-buffer (selected-window) (car bufs))
      (set-window-buffer (next-window) (cadr bufs)))))

(defun macher-agent-process-request (&optional status context fsm)
  "Process request STATUS for CONTEXT and optional FSM.

Split CONTEXT into physical and virtual components and trigger patch creation
via `macher--build-patch' using FSM state when appropriate.

Return nil.

Side effects: Generates and displays patch buffers and updates CTX prompt."
  (let* ((ctx (cond
               ((and status (macher-context-p status)) status)
               ((and context (macher-context-p context)) context)
               (t (ignore-errors (macher-agent-resolve-context)))))
         (fsm-obj (cond
                   ((and context (not (macher-context-p context))) context)
                   (fsm fsm)
                   (t (macher-agent--get-fsm-latest)))))

    (when (and ctx (null (macher-agent--get-context-prompt ctx)))
      (when-let* ((info (when (and fsm-obj (fboundp 'gptel-fsm-info))
                          (ignore-errors (gptel-fsm-info fsm-obj))))
                  (fsm-prompt (plist-get info :prompt)))
        (setf (macher-context-prompt ctx) fsm-prompt)
        (macher-agent--set-context-data ctx :prompt fsm-prompt)))

    (when ctx
      (when (macher-agent--get-context-dirty-p ctx)
        (let* ((payloads (macher-agent-prepare-upstream-payloads ctx))
               (p-ctx (car payloads))
               (v-ctx (cdr payloads))
               (generated-buffers nil))
          (when-let* ((p-buf (macher-agent--build-and-rename-patch p-ctx fsm-obj "physical")))
            (push p-buf generated-buffers))
          (when-let* ((v-buf (macher-agent--build-and-rename-patch v-ctx fsm-obj "virtual")))
            (push v-buf generated-buffers))
          (when generated-buffers
            (macher-agent--display-patch-buffers generated-buffers)))

        (macher-agent--set-context-dirty-p ctx nil)))))

(defun macher-agent--vfs-entry-modified-p (entry)
  "Determine whether VFS ENTRY contains modifications.

Compare original and current content cells within ENTRY to check for edits.

Return non-nil if ENTRY has been modified, or nil otherwise.

Side effects: None."
  (let ((orig (if (consp (cdr entry)) (cadr entry) nil))
        (curr (if (consp (cdr entry)) (cddr entry) (cdr entry))))
    (not (equal (or orig "") (or curr "")))))

(defun macher-agent--context-has-changes-p (context)
  "Determine whether CONTEXT contains actual VFS modifications.

Inspect all VFS entries within CONTEXT to verify if any entries
have been modified.

Return non-nil if CONTEXT has modified VFS entries, or nil otherwise.

Side effects: None."
  (and context
       (cl-some #'macher-agent--vfs-entry-modified-p
                (macher-agent--get-context-contents context))))

(setq macher-process-request-function #'macher-agent-process-request)

(defun macher-agent--get-context-workspace (ctx)
  "Retrieve the workspace structure from context CTX.

Extract and unwrap the workspace object associated with CTX.

Return the workspace structure, or nil if unavailable.

Side effects: None."
  (cond
   ((and ctx (macher-context-p ctx))
    (macher-agent--unwrap-workspace (macher-context-workspace ctx)))
   ((and (consp ctx) (eq (car ctx) 'agent))
    (cdr ctx))
   ((and ctx (macher-agent-workspace-p ctx))
    ctx)
   (t nil)))

(defmacro macher-agent--def-context-accessor (name accessor &optional setter-name docstring)
  "Define a safe accessor and optional setter for a Macher context struct.

Generate a getter function NAME and optional setter function
SETTER-NAME for field ACCESSOR on a `macher-context' structure,
using DOCSTRING if provided.

Return expanded macro defun form.

Side effects: Defines new function symbols in Emacs runtime."
  (let
      ((getter
        `(defun ,name (ctx)
           ,(format
             "Safely access `%s' on CTX.\n\n%s\n\nReturn field value, or nil.\n\nSide effects: None."
             accessor
             (or docstring (format "Safely access `%s' on CTX." accessor)))
           (and ctx
                (macher-context-p ctx)
                (fboundp ',accessor)
                (,accessor ctx)))))
    (if setter-name
        `(progn
           ,getter
           (defun ,setter-name (ctx val)
             ,(format "Safely set `%s' on CTX.\n\nMutate field `%s' on context CTX with VAL.\n\nReturn \
new value VAL, or nil.\n\nSide effects: Mutates CTX structure." accessor accessor)
             (when (and ctx
                        (macher-context-p ctx)
                        (fboundp ',accessor))
               (ignore-errors (with-no-warnings (setf (,accessor ctx) val))))))
      getter)))

(macher-agent--def-context-accessor
 macher-agent--get-context-contents macher-context-contents macher-agent--set-context-contents)
(macher-agent--def-context-accessor
 macher-agent--get-context-dirty-p macher-context-dirty-p macher-agent--set-context-dirty-p)
(macher-agent--def-context-accessor
 macher-agent--get-context-prompt macher-context-prompt macher-agent--set-context-prompt)
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
  (or (bound-and-true-p macher--fsm-latest)
      (bound-and-true-p gptel--fsm)
      (bound-and-true-p gptel--fsm-last)))

(defun macher-agent--inject-context-into-fsm-info (agent-ctx &optional fsm)
  "Inject AGENT-CTX into FSM info property list.

Store AGENT-CTX in the info plist of finite-state machine FSM
under keys `:macher--context' and `:macher-agent-context'.
FSM defaults to active `gptel--fsm'.

Return non-nil if injected successfully, or nil otherwise.

Side effects: Mutates the info property list of FSM."
  (when-let* ((fsm-obj (or fsm (bound-and-true-p gptel--fsm)))
              (info (gptel-fsm-info fsm-obj)))
    (setf (plist-get info :macher--context) agent-ctx)
    (setf (plist-get info :macher-agent-context) agent-ctx)
    (setf (gptel-fsm-info fsm-obj) (plist-put info :macher--context agent-ctx))
    (setf (gptel-fsm-info fsm-obj) (plist-put info :macher-agent-context agent-ctx))
    t))

(defun macher-agent--wrap-single-tool (tool)
  "Wrap single TOOL to intercept orphaned context.

Modify function slot of `gptel-tool' structure TOOL to
intercept calls, transfer user prompt from orphaned
context to sub-agent persistent context, and inject
context into active FSM.

Return non-nil if TOOL was wrapped, or nil if already
wrapped.

Side effects: Mutates function slot of TOOL and updates
`macher-agent--wrapped-tools-hash'."
  (let ((orig-fn (gptel-tool-function tool)))
    (when (and orig-fn (not (gethash tool macher-agent--wrapped-tools-hash)))
      (setf (gptel-tool-function tool)
            (lambda (orphaned-context callback &rest args)
              (let* ((fsm (or (bound-and-true-p macher-agent--active-fsm)
                              (macher-agent--get-fsm-latest)))
                     (target-buf (or (when (and fsm (fboundp 'gptel-fsm-info))
                                       (ignore-errors (plist-get (gptel-fsm-info fsm) :buffer)))
                                     (current-buffer)))
                     (agent-ctx (or (macher-agent--resolve-context fsm)
                                    (macher-agent--resolve-context target-buf))))
                (if agent-ctx
                    (progn
                      (when (and orphaned-context (macher-context-p orphaned-context))
                        (when-let* ((user-prompt (macher-context-prompt orphaned-context)))
                          (ignore-errors (setf (macher-context-prompt agent-ctx) user-prompt))
                          (macher-agent--set-context-data agent-ctx :prompt user-prompt)))
                      (macher-agent--inject-context-into-fsm-info agent-ctx fsm)
                      (with-current-buffer target-buf
                        (apply orig-fn agent-ctx callback args)))
                  (with-current-buffer target-buf
                    (apply orig-fn orphaned-context callback args))))))
      (puthash tool t macher-agent--wrapped-tools-hash))))

(defun macher-agent--wrap-macher-tools ()
  "Wrap all Macher tools to inject persistent VFS context.

Iterate through tools registered under the \"macher\"
category in `gptel--known-tools' and apply
`macher-agent--wrap-single-tool' to each tool.

Return nil.

Side effects: Mutates tool functions in `gptel--known-tools' and populates
`macher-agent--wrapped-tools-hash'."
  (when-let* ((macher-tools
               (alist-get "macher" (bound-and-true-p gptel--known-tools) nil nil #'equal)))
    (dolist (item macher-tools)
      (let ((tool (if (consp item) (cdr item) item)))
        (macher-agent--wrap-single-tool tool)))))

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

(defun macher-agent--process-completed-fsm-buffer (context buffer fsm)
  "Process completed FSM in BUFFER context.

Evaluate request completion logic within live BUFFER for
finite-state machine FSM, invoking
`macher-process-request-function' with protected persistent context.

Return result of request completion process function, or nil if
buffer is invalid.

Side effects: Switches current buffer to BUFFER and triggers
request completion."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (setq-local macher-agent--pending-instructions-queue nil)
      (when-let*
          ((process-fn (bound-and-true-p macher-process-request-function)))
        (macher-agent--with-protected-context-contents context
          (funcall process-fn 'complete context fsm))))))

(defun macher-agent--context-p (ctx)
  "Determine whether CTX is a valid `macher-context' struct.

Return non-nil if CTX is a context struct, or nil otherwise."
  (and ctx
       (fboundp 'macher-context-p)
       (macher-context-p ctx)))

(macher-agent--def-context-accessor
 macher-agent--get-context-raw-data macher-context-data macher-agent--set-context-raw-data
 "Get or set the raw native data property list from CTX.")

(defun macher-agent--get-context-data (ctx key &optional default)
  "Retrieve KEY from the native data slot of CTX.

CTX is the context structure.
KEY is the lookup key symbol.
DEFAULT is the value returned if KEY is not present.

Return the value or DEFAULT."
  (if (and ctx (macher-agent--context-p ctx))
      (let ((data (macher-context-data ctx)))
        (if (plist-member data key)
            (plist-get data key)
          default))
    default))

(defun macher-agent--set-context-data (ctx key val)
  "Set KEY to VAL in the native data slot of CTX.

CTX is the context structure.
KEY is the key symbol.
VAL is the value to store.

Return VAL.
Side effects: Modifies native data slot of CTX."
  (when (and ctx (macher-agent--context-p ctx))
    (let ((data (macher-context-data ctx)))
      (setf (macher-context-data ctx)
            (plist-put data key val))))
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
      (macher-agent--resolve-context (current-buffer)))))

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

Calculates expanded project root path from `default-directory' if not already
present or if nil in STATE.

STATE is the context resolution state plist.

Return STATE with `:expanded-root' set to the expanded path string or nil.
Side effects: None."
  (if (plist-get state :expanded-root)
      state
    (let* ((root (macher-agent-root default-directory))
           (expanded (and root (expand-file-name root))))
      (plist-put state :expanded-root expanded))))

(defun macher-agent-ctx-pipe--explicit (state)
  "Context pipeline step 1: Resolve context explicitly from STATE input.

If `:resolved' in STATE is nil and `:input' in STATE satisfies
`macher-agent--context-p', set `:resolved' to that context structure.

STATE is the context resolution state plist (:input ... :resolved
... :expanded-root ...).

Return the updated STATE plist.
Side effects: None."
  (if (plist-get state :resolved)
      state
    (let ((input (plist-get state :input)))
      (if (and input (macher-agent--context-p input))
          (plist-put state :resolved input)
        state))))

(defun macher-agent-ctx-pipe--fsm (state)
  "Context pipeline step 2: Resolve context from FSM input in STATE.

If `:resolved' in STATE is nil and `:input' in STATE is a finite-state machine
\(FSM), extract and set `:resolved' to its active context structure.

STATE is the context resolution state plist (:input ... :resolved
... :expanded-root ...).

Return the updated STATE plist.
Side effects: None."
  (if (plist-get state :resolved)
      state
    (let ((input (plist-get state :input)))
      (if-let* ((ctx (and input (macher-agent--extract-fsm-context input))))
          (plist-put state :resolved ctx)
        state))))

(defun macher-agent-ctx-pipe--subagent (state)
  "Context pipeline step 3: Resolve subagent or persistent context in STATE.

If `:resolved' in STATE is nil and buffer-local
`macher-agent--persistent-context'
is bound, set `:resolved' to it if current buffer is a subagent or
if the persistent context matches the active workspace root.

STATE is the context resolution state plist \(:input ... :resolved ...
:expanded-root ...\).

Return the updated STATE plist.
Side effects: May populate `:expanded-root' in STATE."
  (if (plist-get state :resolved)
      state
    (let* ((st (macher-agent--get-expanded-root state))
           (active-root-expanded (plist-get st :expanded-root))
           (pers-ctx (macher-agent--resolve-context (current-buffer))))
      (if pers-ctx
          (let ((pers-matches (macher-agent--match-persistent-context pers-ctx active-root-expanded)))
            (if (or pers-matches (bound-and-true-p macher-agent--is-subagent))
                (plist-put st :resolved pers-ctx)
              st))
        st))))

(defun macher-agent-ctx-pipe--canonical (state)
  "Context pipeline step 4: Resolve canonical context from workspace in STATE.

If `:resolved' in STATE is nil, look up active workspace context in
`macher-agent-active-workspaces' using the expanded project root.

STATE is the context resolution state plist \(:input ... :resolved ...
:expanded-root ...\).

Return the updated STATE plist.
Side effects: May update buffer-local `macher-agent--persistent-context' and
populate `:expanded-root' in STATE."
  (if (plist-get state :resolved)
      state
    (let* ((st (macher-agent--get-expanded-root state))
           (active-root-expanded (plist-get st :expanded-root)))
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
          (let ((final-ctx
                 (if (not (bound-and-true-p macher-agent--is-subagent))
                     (progn
                       (let ((pers-ctx (macher-agent--resolve-context (current-buffer))))
                         (when (not (eq pers-ctx canonical-ctx))
                           (setq-local macher-agent--persistent-context canonical-ctx)))
                       canonical-ctx)
                   (let ((isolated-ctx (macher-agent--clone-context canonical-ctx)))
                     (setq-local macher-agent--persistent-context isolated-ctx)
                     isolated-ctx))))
            (plist-put st :resolved final-ctx))
        st))))

(defun macher-agent-ctx-pipe--fsm-fallback (state)
  "Context pipeline step 5: Resolve context from latest FSM fallback in STATE.

If `:resolved' in STATE is nil, attempt to extract context from
`macher-agent--get-fsm-latest'.

STATE is the context resolution state plist \(:input ... :resolved ...
:expanded-root ...\).

Return the updated STATE plist.
Side effects: None."
  (if (plist-get state :resolved)
      state
    (if-let* ((fsm (macher-agent--get-fsm-latest))
              (ctx (macher-agent--extract-fsm-context fsm)))
        (plist-put state :resolved ctx)
      state)))

(defun macher-agent-ctx-pipe--lazy-init (state)
  "Context pipeline step 6: Resolve context via lazy initialisation in STATE.

If `:resolved' in STATE is nil, attempt lazy workspace initialisation.

STATE is the context resolution state plist \(:input ... :resolved ...
:expanded-root ...\).

Return the updated STATE plist.
Side effects: May initialise workspace state for current directory."
  (if (plist-get state :resolved)
      state
    (if-let* ((ctx (ignore-errors (macher-agent--resolve-context-lazy-init)))
              ((macher-agent--context-p ctx)))
        (plist-put state :resolved ctx)
      state)))

(defun macher-agent-resolve-context (&optional ctx-or-fsm)
  "Resolve the active context from CTX-OR-FSM or state.

Passes state (:input CTX-OR-FSM :resolved nil :expanded-root nil) through
`seq-reduce' over `macher-agent-context-pipeline-functions'.

CTX-OR-FSM is the optional context structure or finite-state machine.

Return the resolved context structure, or signals an error if nil.
Side effects: May register active workspace root and update persistent context."
  (let* ((initial-state (list :input ctx-or-fsm :resolved nil :expanded-root nil))
         (final-state (seq-reduce (lambda (state step-fn)
                                    (funcall step-fn state))
                                  macher-agent-context-pipeline-functions
                                  initial-state))
         (ctx (plist-get final-state :resolved)))
    (unless (and ctx (macher-agent--context-p ctx))
      (error "No active agent session found"))
    (when-let* ((root (ignore-errors (macher-agent-context-root ctx))))
      (unless (gethash (expand-file-name root) macher-agent-active-workspaces)
        (macher-agent--register-active-workspace-root root ctx)))
    ctx))

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
   ((stringp ws) (expand-file-name ws))
   ((consp ws) (expand-file-name (cdr ws)))
   (t nil)))

(defun macher-agent-workspace-vfs-buffers (ws-or-ctx)
  "Retrieve the VFS buffers hash-table for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a hash-table.
Side effects: Initialises `:vfs-buffers` in WS-OR-CTX if not present."
  (let ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
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
  (let ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
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
  (let ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
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
  (let ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
    (if ctx
        (macher-agent--get-context-data ctx :skills-alist)
      macher-agent-global-skills-alist)))

(defun macher-agent-workspace-active-subagents (ws-or-ctx)
  "Retrieve the active subagents list for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a list of subagent entries."
  (when-let* ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
    (macher-agent--get-context-data ctx :active-subagents)))

(defun macher-agent--set-workspace-skills-alist (ws-or-ctx val)
  "Set the skills alist for WS-OR-CTX to VAL.

WS-OR-CTX is a workspace object or context structure.
VAL is the skills alist to set.

Return VAL.
Side effects: Modifies the skills alist for WS-OR-CTX or global skills alist."
  (if-let* ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
      (macher-agent--set-context-data ctx :skills-alist val)
    (setq macher-agent-global-skills-alist val))
  val)

(gv-define-setter macher-agent-workspace-skills-alist (val ws-or-ctx)
  `(macher-agent--set-workspace-skills-alist ,ws-or-ctx ,val))

(defun macher-agent--set-workspace-active-subagents (ws-or-ctx val)
  "Set the active subagents list for WS-OR-CTX to VAL.

WS-OR-CTX is a workspace object or context structure.
VAL is the list of active subagents to set.

Return VAL.
Side effects: Modifies the active subagents list for WS-OR-CTX."
  (when-let* ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
    (macher-agent--set-context-data ctx :active-subagents val))
  val)

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

(defun macher-agent--resolve-context-from-ws (ws-or-ctx)
  "Resolve WS-OR-CTX into a `macher-context` struct if possible.

WS-OR-CTX is a workspace object, cons cell, or context struct.

Return the resolved `macher-context` struct, or nil."
  (if (and ws-or-ctx (macher-agent--context-p ws-or-ctx))
      ws-or-ctx
    (let* ((target-root (macher-agent-workspace-project-root ws-or-ctx))
           (expanded-root (and target-root (expand-file-name target-root)))
           (pers-ctx (macher-agent--resolve-context (current-buffer))))
      (or (macher-agent--match-persistent-context pers-ctx expanded-root)
          (macher-agent--find-active-workspace-in-ancestors expanded-root)
          (ignore-errors (macher-agent-resolve-context))))))

(defun macher-agent--merge-contexts (parent-ctx child-ctx)
  "Merge the VFS contents of CHILD-CTX into PARENT-CTX.

PARENT-CTX is the parent context structure.
CHILD-CTX is the child context structure.

Return PARENT-CTX.
Side effects: Updates PARENT-CTX with VFS entries from CHILD-CTX."
  (let ((child-contents (macher-agent--get-context-contents child-ctx))
        (parent-contents (macher-agent--get-context-contents parent-ctx)))
    (dolist (child-entry child-contents)
      (let* ((path (car child-entry))
             (orig (if (consp (cdr child-entry)) (cadr child-entry) nil))
             (new (if (consp (cdr child-entry)) (cddr child-entry) (cdr child-entry)))
             (norm-path (ignore-errors (macher-agent--normalize-path-key path parent-ctx)))
             (parent-entry
              (or
               (cl-find path parent-contents :key #'car :test #'equal)
               (when norm-path (cl-find norm-path parent-contents :key #'car :test #'equal))
               (cl-find-if
                (lambda (e)
                  (let ((e-path (car e)))
                    (or
                     (and norm-path
                          (equal
                           (ignore-errors
                             (macher-agent--normalize-path-key e-path parent-ctx)) norm-path))
                     (string-suffix-p e-path path)
                     (string-suffix-p path e-path))))
                parent-contents)))
             (p-orig
              (when parent-entry (if (consp (cdr parent-entry)) (cadr parent-entry) nil)))
             (p-curr
              (when parent-entry
                (if (consp (cdr parent-entry)) (cddr parent-entry) (cdr parent-entry))))
             (target-has-edit (and parent-entry (not (equal p-orig p-curr)))))
        (when (or (and (not (equal orig new)) (not target-has-edit))
                  (null parent-entry)
                  (and (not (equal orig new)) (not (macher-agent-subagent-p))))
          (macher-agent--update-context-file parent-ctx path new))))
    parent-ctx))

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
        ((orig-data (when (macher-agent--context-p ctx)
                      (macher-agent--get-context-raw-data ctx)))
         (new-data (copy-sequence orig-data))
         (new-ctx (macher-agent--make-vfs-context
                   :workspace (macher-agent--get-context-workspace ctx)
                   :contents (copy-tree (macher-agent--get-context-contents ctx)))))
      (when (macher-agent--get-context-prompt ctx)
        (macher-agent--set-context-prompt new-ctx (macher-agent--get-context-prompt ctx)))
      (when new-data
        (macher-agent--set-context-raw-data new-ctx new-data))
      (when (macher-agent--get-context-dirty-p ctx)
        (macher-agent--set-context-dirty-p new-ctx t))
      new-ctx)))

(defun macher-agent--update-context-file (context path new-content)
  "Update PATH in CONTEXT with NEW-CONTENT.

CONTEXT is the active context structure.
PATH is the relative file path string.
NEW-CONTENT is the modified content string.

Return nil.
Side effects: Updates CONTEXT entries, sets dirty flag, and persists state."
  (unless context
    (error "VFS Write Error: Context cannot be nil"))
  (let*
      ((norm-path (macher-agent--normalize-path-key path context))
       (contents (macher-agent--get-context-contents context))
       (entry
        (or (cl-find norm-path contents :key #'car :test #'equal)
            (cl-find path contents :key #'car :test #'equal)
            (cl-find-if
             (lambda (e)
               (let ((e-path (car e)))
                 (or (equal (macher-agent--normalize-path-key e-path context) norm-path)
                     (string-suffix-p e-path path)
                     (string-suffix-p path e-path))))
             contents))))
    (if entry
        (if (consp (cdr entry))
            (setcdr (cdr entry) new-content)
          (setcdr entry new-content))
      (let* ((workspace-root (macher-agent-context-root context))
             (orig (macher-agent--get-buffer-content-stateless norm-path workspace-root)))
        (macher-agent--set-context-contents
         context
         (cons (cons norm-path (cons orig new-content)) contents))))
    (puthash norm-path new-content (macher-agent-workspace-vfs-buffers context))
    (puthash path new-content (macher-agent-workspace-vfs-buffers context))
    (macher-agent--set-context-dirty-p context t)
    (macher-agent--persist-vfs-to-hidden-buffer context)
    (run-hook-with-args 'macher-agent-context-mutated-hook norm-path)))

(defun macher-agent--read-context-file (context path)
  "Read PATH from CONTEXT.

Prioritises VFS, then active buffers, then physical disk.
Uniformly applies security and path normalisation checks.

CONTEXT is the active context structure.
PATH is the file path string to read.

Return the file content string."
  (unless context
    (error "VFS Read Error: Context cannot be nil"))
  (let* ((norm-path (macher-agent--normalize-path-key path context))
         (contents (macher-agent--get-context-contents context))
         (workspace-root (macher-agent-context-root context)))
    (macher-agent--ensure-access-stateless contents path)
    (if-let*
        ((virtual-entry
          (or (cl-find norm-path contents :key #'car :test #'equal)
              (cl-find path contents :key #'car :test #'equal)))
         (virtual-content
          (if (consp (cdr virtual-entry)) (cddr virtual-entry) (cdr virtual-entry))))
        virtual-content
      (let
          ((target-path
            (if workspace-root
                (let ((relative-path (if (file-name-absolute-p path)
                                         (file-relative-name path workspace-root)
                                       path)))
                  (macher-agent--resolve-safe-path relative-path workspace-root))
              path)))
        (or (macher-agent--read-content-from-disk-or-buffer target-path)
            (error "ERROR: File/Buffer '%s' does not exist" path))))))

(defun macher-agent--normalize-path-key (path &optional context)
  "Normalise PATH to a canonical key for CONTEXT entries.

PATH is the string path to normalise.
CONTEXT is the optional context structure.

Return the canonical key path string, or PATH if non-string."
  (if (or (null path) (not (stringp path)))
      path
    (let* ((ws-root (and context (macher-agent-context-root context)))
           (buf (get-buffer path))
           (is-pure-buffer (and buf (null (buffer-file-name buf)))))
      (if (and ws-root (not is-pure-buffer) (not (string-prefix-p "*" path)))
          (expand-file-name path ws-root)
        (if (and (string-prefix-p "*" path) (not (string-suffix-p "*" path)))
            (concat path "*")
          path)))))

(defun macher-agent--ensure-access (context path)
  "Ensure PATH is within the explicitly scoped CONTEXT.

CONTEXT is the active context structure.
PATH is the string file path.

Return nil or signals an error."
  (unless context
    (error "VFS Error: Context cannot be nil"))
  (macher-agent--ensure-access-stateless (macher-agent--get-context-contents context) path))

(defun macher-agent--set-workspace-tools-registry (ws-or-ctx val)
  "Set the tools registry for WS-OR-CTX to VAL.

WS-OR-CTX is a workspace object or context structure.
VAL is the tools registry hash-table to set.

Return VAL.
Side effects: Modifies the tools registry for WS-OR-CTX or global registry."
  (let ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
    (if ctx
        (macher-agent--set-context-data ctx :tools-registry val)
      (setq macher-agent-tools-registry val)))
  val)

(gv-define-setter macher-agent-workspace-tools-registry (val ws-or-ctx)
  `(macher-agent--set-workspace-tools-registry ,ws-or-ctx ,val))

(gv-define-setter macher-agent-workspace-active-subagents (val ws-or-ctx)
  `(macher-agent--set-workspace-active-subagents ,ws-or-ctx ,val))

(defun macher-agent-context-root (context)
  "Retrieve the project root directory string from CONTEXT.

CONTEXT is the active context structure.

Return the project root path string."
  (or (when-let* ((ws (when context (macher-agent--get-context-workspace context))))
        (macher-agent-workspace-project-root ws))
      default-directory))

(defun macher-agent-call-with-strict-vfs-pipeline (context body-fn)
  "Execute BODY-FN within a physical sandbox directory populated with CONTEXT.

CONTEXT is the active context structure.
BODY-FN is the function containing pipeline logic.

Return the result of BODY-FN.
Side effects: Creates and cleans up a temporary sandbox directory."
  (let* ((ctx-root (ignore-errors (macher-agent-context-root context)))
         (proj (project-current nil default-directory))
         (proj-root (when proj (expand-file-name (project-root proj))))
         (workspace-root (or ctx-root proj-root default-directory))
         (sandbox-dir (make-temp-file "macher-sandbox-" t))
         (contents (when context (macher-agent--get-context-contents context))))
    (unwind-protect
        (progn
          (macher-agent--vfs-verify-clean-merge workspace-root contents)
          (macher-agent--vfs-sync-baseline workspace-root sandbox-dir)
          (when contents
            (macher-agent--vfs-apply-overlay-stateless contents workspace-root sandbox-dir))
          (let ((default-directory sandbox-dir))
            (funcall body-fn)))
      (ignore-errors
        (delete-directory sandbox-dir t)))))

(defmacro macher-agent-with-strict-vfs-pipeline (context &rest body)
  "Execute BODY with strict VFS pipeline isolation populated with CONTEXT.

CONTEXT is the active agent context.
BODY represents the forms to evaluate in the isolated directory.

Return the result of evaluating BODY."
  `(macher-agent-call-with-strict-vfs-pipeline ,context (lambda () ,@body)))

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

(defun macher-agent-current-context (&optional ctx-or-fsm)
  "Return the active context structure for CTX-OR-FSM.

CTX-OR-FSM is the optional context or finite-state machine.

Return the resolved context structure, or nil."
  (macher-agent-resolve-context ctx-or-fsm))

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

(defun macher-agent--auto-sync-context (ctx &rest _args)
  "Synchronise the active context with the physical disk, unless paused.

CTX is the active context structure.
_ARGS represents unused extra arguments.

Return nil.
Side effects: May update CTX dirty state and persist state if synced."
  (when (and ctx (not macher-agent--pause-auto-sync))
    (let* ((contents (macher-agent--get-context-contents ctx))
           (workspace (or (when ctx (macher-agent--get-context-workspace ctx)) ctx))
           (tracker (when workspace (macher-agent-workspace-mtime-tracker workspace)))
           (res (macher-agent--sync-and-check-dirty-entries contents tracker))
           (synced (car res))
           (is-dirty (cdr res)))

      (unless is-dirty
        (macher-agent--set-context-dirty-p ctx nil))

      (when synced
        (macher-agent--persist-vfs-to-hidden-buffer ctx)
        (run-hooks 'macher-agent-context-mutated-hook)))))

(defun macher-agent--persist-vfs-to-hidden-buffer (ctx)
  "Persist virtual file system state of CTX to a hidden buffer for review.

CTX is the active context structure.

Return nil.
Side effects: Creates or erases and populates the hidden VFS state buffer."
  (let* ((workspace (when ctx (macher-agent--get-context-workspace ctx)))
         (root-dir (if workspace (macher-agent-workspace-project-root workspace) "default"))
         (buf-name (format " *macher-agent-vfs-state-%s*" (md5 (expand-file-name root-dir))))
         (vfs-buf (get-buffer-create buf-name)))
    (with-current-buffer vfs-buf
      (erase-buffer)
      (insert ";;; Macher Agent Virtual File System State\n")
      (insert ";;; This buffer is native and handles large text blocks.\n\n")
      (when ctx
        (macher-agent--write-vfs-entries-to-buffer
         (macher-agent--get-context-contents ctx))))))

(defun macher-agent--split-context (ctx)
  "Split context CTX into virtual and physical context clones.

CTX is the active context structure.

Return a cons cell of cloned contexts (FILE-CONTEXT . BUFFER-CONTEXT)."
  (let* ((file-ctx (macher-agent--clone-context ctx))
         (buf-ctx (macher-agent--clone-context ctx))
         (workspace (when ctx (macher-agent--get-context-workspace ctx)))
         (root (and workspace (macher-agent-workspace-project-root workspace)))
         (contents (when ctx (macher-agent--get-context-contents ctx)))
         (modified-contents
          (cl-remove-if (lambda (e)
                          (let ((orig (if (consp (cdr e)) (cadr e) nil))
                                (curr (if (consp (cdr e)) (cddr e) (cdr e))))
                            (equal orig curr)))
                        contents))
         (partitioned (macher-agent--partition-vfs-entries modified-contents root))
         (buf-contents (car partitioned))
         (file-contents (cdr partitioned)))
    (when file-ctx (macher-agent--set-context-contents file-ctx file-contents))
    (when buf-ctx (macher-agent--set-context-contents buf-ctx buf-contents))
    (cons file-ctx buf-ctx)))

(defun macher-agent--trigger-patch-on-complete (fsm &rest _)
  "Trigger patch generation when FSM transitions to DONE state.

Inspect finite-state machine FSM state upon transition.  If state
is DONE, resolve target buffer and invoke
`macher-agent--process-completed-fsm-buffer'.  Ignores errors when
session in aborted state.

FSM is the finite-state machine object.
_RES contains additional optional arguments.

Return nil.

Side effects: May trigger patch generation and buffer
display upon completion."
  (ignore-errors
    (unless macher-agent--inhibit-patch-hook
      (let ((macher-agent--inhibit-patch-hook t))
        (when (eq (gptel-fsm-state fsm) 'DONE)
          (let* ((info (gptel-fsm-info fsm))
                 (buffer (plist-get info :buffer))
                 (context (macher-agent--resolve-context fsm)))
            (with-current-buffer buffer
              (macher-agent--process-completed-fsm-buffer context buffer fsm))))))))

(provide 'macher-agent-macher)
;;; macher-agent-macher.el ends here
