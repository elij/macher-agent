;;; macher-agent-macher-bridge.el --- Bridge to Macher Core  -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridge implementation for Macher Agent to interact with Macher Core.

;;; Code:

(require 'macher)
(require 'cl-lib)
(require 'subr-x)

(declare-function macher-agent-workspace-project-root "macher-agent-vfs-client")
(declare-function macher-agent-workspace-p "macher-agent-vfs-client")
(declare-function macher-agent-resolve-context "macher-agent-vfs-client")
(declare-function macher-agent--set-context-data "macher-agent-vfs-client" (ctx key val))
(declare-function macher-agent--partition-vfs-entries "macher-agent-vfs-client" (contents &optional root-dir))
(declare-function macher-context-shadow-buffers "macher" (ctx))

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

(defun macher-agent--get-workspace-name (ws)
  "Retrieve a human-readable name for workspace WS.

Extract or derive a display name string for workspace object WS based on its
project root or upstream identifier.

Return the workspace name string, or nil if unavailable.

Side effects: None."
  (let ((unwrapped (macher-agent--unwrap-workspace ws)))
    (cond
     ((macher-agent-workspace-p unwrapped)
      (file-name-nondirectory (directory-file-name (macher-agent-workspace-project-root unwrapped))))
     (t (ignore-errors (macher--workspace-name ws))))))

(defun macher-agent--pure-virtual-entry-p (entry)
  "Determine whether VFS ENTRY represents a pure virtual buffer.

Check if ENTRY corresponds to a live buffer without an associated disk file.

Return non-nil if ENTRY is a pure virtual buffer, or nil otherwise.

Side effects: None."
  (let* ((name (car entry))
         (live-buf (get-buffer name)))
    (and live-buf (null (buffer-file-name live-buf)))))

(defun macher-agent--split-vfs-contents (contents)
  "Split raw VFS CONTENTS into pure virtual and physical lists.

Partition the list of VFS entry structures CONTENTS into pure virtual buffer
entries and physical file entries.

Return a cons cell of (VIRTUAL-CONTENTS . PHYSICAL-CONTENTS).

Side effects: None."
  (let ((virtual-contents nil)
        (physical-contents nil))
    (dolist (entry contents)
      (if (macher-agent--pure-virtual-entry-p entry)
          (push entry virtual-contents)
        (push entry physical-contents)))
    (cons (nreverse virtual-contents) (nreverse physical-contents))))

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

(advice-add 'macher--workspace-hash :override #'macher-agent--safe-workspace-hash)

(defvar-local macher-agent--persistent-context nil
  "Store the buffer-local persistent VFS context structure.

Hold the `macher-context' instance bound to the current buffer across agent turns.

Return the `macher-context' struct, or nil if unset.

Side effects: Buffer-local variable.")

(cl-defun macher-agent--make-vfs-context (&key workspace contents prompt)
  "Create a native `macher-context' struct using `macher--make-context'.

Construct a new context structure with WORKSPACE, CONTENTS, and PROMPT.

Return the newly created `macher-context' struct.

Side effects: Dynamically binds `macher-agent--persistent-context' to nil during construction."
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
  (when (and (not (bound-and-true-p macher-agent--suppress-patch))
             (macher-agent--context-has-changes-p ctx))
    (macher--build-patch ctx fsm-obj)
    (when-let* ((buf (macher-patch-buffer (macher-context-workspace ctx)))
                (name (buffer-name buf)))
      (let ((new-name (if (string-match "^\\*macher-patch\\(.*\\)\\*$" name)
                          (concat "*macher-" patch-type "-patch" (match-string 1 name) "*")
                        (concat "*macher-" patch-type "-patch*"))))
        (with-current-buffer buf
          (rename-buffer new-name t)
          (current-buffer))))))

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
      (let* ((payloads (macher-agent-prepare-upstream-payloads ctx))
             (p-ctx (car payloads))
             (v-ctx (cdr payloads))
             (generated-buffers nil))
        (when-let* ((p-buf (macher-agent--build-and-rename-patch p-ctx fsm-obj "physical")))
          (push p-buf generated-buffers))
        (when-let* ((v-buf (macher-agent--build-and-rename-patch v-ctx fsm-obj "virtual")))
          (push v-buf generated-buffers))
        (when generated-buffers
          (macher-agent--display-patch-buffers generated-buffers))))))

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

Inspect all VFS entries within CONTEXT to verify if any entries have been modified.

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

(defun macher-agent--set-context-workspace (ctx ws)
  "Set the workspace on CTX to WS.

Update the workspace slot of context structure CTX with WS.

Return nil.

Side effects: Mutates the workspace slot of CTX."
  (when (and ctx (macher-context-p ctx))
    (setf (macher-context-workspace ctx) ws)))

(defmacro macher-agent--def-context-accessor (name accessor &optional setter-name docstring)
  "Define a safe accessor and optional setter for a Macher context struct.

Generate a getter function NAME and optional setter function SETTER-NAME for
field ACCESSOR on a `macher-context' structure, using DOCSTRING if provided.

Return expanded macro defun form.

Side effects: Defines new function symbols in Emacs runtime."
  (let ((getter `(defun ,name (ctx)
                   ,(format "Safely access `%s' on CTX.\n\n%s\n\nReturn field value, or nil.\n\nSide effects: None."
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
             ,(format "Safely set `%s' on CTX.\n\nMutate field `%s' on context CTX with VAL.\n\nReturn new value VAL, or nil.\n\nSide effects: Mutates CTX structure." accessor accessor)
             (when (and ctx
                        (macher-context-p ctx)
                        (fboundp ',accessor))
               (ignore-errors (with-no-warnings (setf (,accessor ctx) val))))))
      getter)))

(macher-agent--def-context-accessor macher-agent--get-context-contents macher-context-contents macher-agent--set-context-contents)
(macher-agent--def-context-accessor macher-agent--get-context-dirty-p macher-context-dirty-p macher-agent--set-context-dirty-p)
(macher-agent--def-context-accessor macher-agent--get-context-prompt macher-context-prompt macher-agent--set-context-prompt)
(macher-agent--def-context-accessor macher-agent--get-context-shadow-buffers macher-context-shadow-buffers macher-agent--set-context-shadow-buffers "Safely retrieve shadow buffers if defined upstream.")

(defun macher-agent--get-fsm-latest ()
  "Get the active finite-state machine (FSM) if bound.

Locate and return the active finite-state machine instance from package global
variables `macher--fsm-latest', `gptel--fsm', or `gptel--fsm-last'.

Return the active FSM structure, or nil if none is bound.

Side effects: None."
  (or (bound-and-true-p macher--fsm-latest)
      (bound-and-true-p gptel--fsm)
      (bound-and-true-p gptel--fsm-last)))

(defun macher-agent--get-buffer-persistent-context (&optional buf)
  "Get the buffer-local persistent context for BUF.

Retrieve `macher-agent--persistent-context' from buffer BUF, defaulting BUF to
the target buffer associated with `gptel--fsm' or current buffer.

Return the context structure, or nil if unset or buffer is dead.

Side effects: None."
  (let ((target (or buf
                    (and (boundp 'gptel--fsm) gptel--fsm
                         (plist-get (gptel-fsm-info gptel--fsm) :buffer))
                    (current-buffer))))
    (when (buffer-live-p target)
      (buffer-local-value 'macher-agent--persistent-context target))))

(defun macher-agent--inject-context-into-fsm-info (agent-ctx &optional fsm)
  "Inject AGENT-CTX into FSM info property list.

Store AGENT-CTX in the info plist of finite-state machine FSM under keys
`:macher--context' and `:macher-agent-context'.  FSM defaults to active `gptel--fsm'.

Return non-nil if injected successfully, or nil otherwise.

Side effects: Mutates the info property list of FSM."
  (when-let* ((fsm-obj (or fsm (bound-and-true-p gptel--fsm)))
              (info (gptel-fsm-info fsm-obj)))
    (setf (plist-get info :macher--context) agent-ctx)
    (setf (plist-get info :macher-agent-context) agent-ctx)
    (setf (gptel-fsm-info fsm-obj) (plist-put info :macher--context agent-ctx))
    (setf (gptel-fsm-info fsm-obj) (plist-put info :macher-agent-context agent-ctx))
    t))

(defun macher-agent--inject-context-into-tool (orig-fn callback &rest args)
  "Intercept tool execution to ensure persistent context is active.

Execute ORIG-FN with CALLBACK and ARGS while ensuring the buffer-local
`macher-agent--persistent-context' is injected into active FSM info.

Return the result of invoking ORIG-FN.

Side effects: Injects persistent context into active FSM info property list."
  (let* ((target-buf (or (and (boundp 'gptel--fsm) gptel--fsm
                              (plist-get (gptel-fsm-info gptel--fsm) :buffer))
                         (current-buffer)))
         (agent-ctx (macher-agent--get-buffer-persistent-context target-buf)))
    (if agent-ctx
        (progn
          (macher-agent--inject-context-into-fsm-info agent-ctx)
          (with-current-buffer target-buf
            (apply orig-fn callback args)))
      (apply orig-fn callback args))))

(defvar macher-agent--wrapped-tools-hash (make-hash-table :test 'eq)
  "Track wrapped `gptel-tool' instances in a hash table.

Store `gptel-tool' objects that have already been wrapped by Macher Agent to
prevent duplicate tool wrapping.

Return the hash table instance.

Side effects: Global variable storing hash table state.")

(defun macher-agent--wrap-single-tool (tool)
  "Wrap single TOOL to intercept orphaned context.

Modify function slot of `gptel-tool' structure TOOL to intercept calls, transfer
user prompt from orphaned context to sub-agent persistent context, and inject
context into active FSM.

Return non-nil if TOOL was wrapped, or nil if already wrapped.

Side effects: Mutates function slot of TOOL and updates `macher-agent--wrapped-tools-hash'."
  (let ((orig-fn (gptel-tool-function tool)))
    (when (and orig-fn (not (gethash tool macher-agent--wrapped-tools-hash)))
      (setf (gptel-tool-function tool)
            (lambda (orphaned-context callback &rest args)
              (let* ((target-buf (or (and (boundp 'gptel--fsm) gptel--fsm
                                          (plist-get (gptel-fsm-info gptel--fsm) :buffer))
                                     (current-buffer)))
                     (agent-ctx (macher-agent--get-buffer-persistent-context target-buf)))
                (when agent-ctx
                  (when (and orphaned-context (macher-context-p orphaned-context))
                    (when-let* ((user-prompt (macher-context-prompt orphaned-context)))
                      (ignore-errors (setf (macher-context-prompt agent-ctx) user-prompt))
                      (macher-agent--set-context-data agent-ctx :prompt user-prompt)))

                  (macher-agent--inject-context-into-fsm-info agent-ctx))
                (with-current-buffer target-buf
                  (apply orig-fn (or agent-ctx orphaned-context) callback args)))))
      (puthash tool t macher-agent--wrapped-tools-hash))))

(defun macher-agent--wrap-macher-tools ()
  "Wrap all Macher tools to inject persistent VFS context.

Iterate through tools registered under the \"macher\" category in `gptel--known-tools'
and apply `macher-agent--wrap-single-tool' to each tool.

Return nil.

Side effects: Mutates tool functions in `gptel--known-tools' and populates `macher-agent--wrapped-tools-hash'."
  (when-let* ((macher-tools
               (alist-get "macher" (bound-and-true-p gptel--known-tools) nil nil #'equal)))
    (dolist (item macher-tools)
      (let ((tool (if (consp item) (cdr item) item)))
        (macher-agent--wrap-single-tool tool)))))

(defmacro macher-agent--with-protected-context-contents (ctx &rest body)
  "Execute BODY preserving the VFS contents of CTX.

Capture a copy of the VFS contents of context CTX prior to executing BODY and
restore the original contents via `unwind-protect' upon completion or non-local exit.

Return the result of evaluating BODY.

Side effects: Restores original VFS contents on CTX after BODY executes."
  (declare (indent 1) (debug t))
  (let ((ctx-sym (gensym "ctx"))
        (protected-sym (gensym "protected")))
    `(let* ((,ctx-sym ,ctx)
            (,protected-sym (and ,ctx-sym
                                 (copy-sequence (macher-agent--get-context-contents ,ctx-sym)))))
       (unwind-protect
           (progn ,@body)
         (when (and ,ctx-sym ,protected-sym)
           (macher-agent--set-context-contents ,ctx-sym ,protected-sym))))))

(defun macher-agent--process-completed-fsm-buffer (buffer fsm)
  "Process completed FSM in BUFFER context.

Evaluate request completion logic within live BUFFER for finite-state machine FSM,
invoking `macher-process-request-function' with protected persistent context.

Return result of request completion process function, or nil if buffer is invalid.

Side effects: Switches current buffer to BUFFER and triggers request completion."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (when-let* ((agent-ctx (bound-and-true-p macher-agent--persistent-context))
                  (process-fn (bound-and-true-p macher-process-request-function)))
        (macher-agent--with-protected-context-contents agent-ctx
                                                       (funcall process-fn 'complete agent-ctx fsm))))))

(defvar macher-agent--inhibit-patch-hook nil)

(defun macher-agent--trigger-patch-on-complete (fsm &rest _)
  "rigger patch generation when FSM transitions to DONE state.

Inspect finite-state machine FSM state upon transition.  If state is DONE, resolve
target buffer and invoke `macher-agent--process-completed-fsm-buffer'. Ignores errors when
session in aborted state.

Return nil.
Side effects: May trigger patch generation and buffer display upon completion."
  (ignore-errors
    (unless macher-agent--inhibit-patch-hook
      (let ((macher-agent--inhibit-patch-hook t))
        (when (eq (gptel-fsm-state fsm) 'DONE)
          (let* ((info (gptel-fsm-info fsm))
                 (buffer (plist-get info :buffer)))
            (with-current-buffer buffer
              (macher-agent--process-completed-fsm-buffer buffer fsm))))))))

(advice-add 'gptel--fsm-transition :after #'macher-agent--trigger-patch-on-complete)

(provide 'macher-agent-macher-bridge)
;;; macher-agent-macher-bridge.el ends here
