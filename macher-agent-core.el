;;; macher-agent-core.el --- Core state for Macher Agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Core state variables and functions for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(eval-and-compile
  (require 'gv))

(declare-function gptel-tool-p "gptel" (tool))
(declare-function gptel-tool-name "gptel" (tool))
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-root "project" (project))
(declare-function vc-root-dir "vc-hooks" ())

(cl-defstruct (macher-agent-context
               (:constructor macher-agent--make-context)
               (:copier macher-agent--copy-context))
  "Primary execution context and memory bus for macher-agent."
  (id nil :type (or null string))
  (project-root nil :type (or null string))
  (origin-buffer nil :type (or null buffer))
  (prompt nil :type (or null string))
  (tools nil :type list)
  (skills nil :type list)
  (media-queue nil :type list)
  (subagents nil :type list)
  (plugins nil :type list))

(cl-defstruct (macher-agent-vfs-entry
               (:constructor make-macher-agent-vfs-entry)
               (:copier copy-macher-agent-vfs-entry))
  "Virtual file system entry tuple representing a managed file or buffer."
  (path nil :type (or null string))
  (orig nil :type (or null string))
  (curr nil :type (or null string)))

(cl-defstruct (macher-agent-transit-payload
               (:constructor make-macher-agent-transit-payload)
               (:copier copy-macher-agent-transit-payload))
  "Structured transit payload for agent-to-agent and tool communication."
  schema-version
  transit-type
  type
  task-id
  target-buffer
  target-context
  parent-context
  child-context
  shared-state
  payload
  metadata)

(cl-defun macher-agent-make-a2a-payload (&key (schema-version macher-agent-a2a-schema-version)
                                              (transit-type :root-to-subagent)
                                              type
                                              task-id
                                              target
                                              target-buffer
                                              target-context
                                              parent-context
                                              child-context
                                              shared-state
                                              payload
                                              metadata)
  "Construct a validated `macher-agent-transit-payload' struct."
  (cl-assert (memq transit-type macher-agent-a2a-transit-types)
             nil "Invalid transit-type: %S (expected one of %S)" transit-type macher-agent-a2a-transit-types)
  (make-macher-agent-transit-payload
   :schema-version schema-version
   :transit-type transit-type
   :type type
   :task-id task-id
   :target-buffer (or target-buffer target)
   :target-context target-context
   :parent-context parent-context
   :child-context child-context
   :shared-state shared-state
   :payload payload
   :metadata metadata))

(cl-defstruct (macher-agent-tool-call
               (:constructor make-macher-agent-tool-call)
               (:copier copy-macher-agent-tool-call))
  "Structured tool call execution contract."
  name
  args
  status
  result)

(cl-defstruct (macher-agent-sandbox-state
               (:constructor make-macher-agent-sandbox-state)
               (:copier copy-macher-agent-sandbox-state))
  "State for sandboxed evaluation execution."
  env
  is-star
  interrupt)

(cl-defstruct (macher-agent-task-context
               (:constructor make-macher-agent-task-context)
               (:copier copy-macher-agent-task-context))
  "Represent a task execution context structure.

WORKSPACE is the target workspace instance or path.
TARGET-BUFFER is the target buffer for task execution.
SKILL-SYM is the active skill or preset symbol.
SYSTEM-MESSAGE is the system prompt message string."
  workspace
  target-buffer
  skill-sym
  system-message)

;; Universal constants and global state

(defconst macher-agent--file-scan-regex "^[^.]"
  "Regular expression matching non-hidden file and directory names.")

(defvar directory-files-no-dot-files-re macher-agent--file-scan-regex
  "Safe compatibility alias for non-dot file directory searches.")

(defvar macher-agent--active-ptc-execution nil
  "Indicate whether a Programmatic Tool Calling Lisp script is active.

If non-nil, evaluation of a Programmatic Tool Calling Lisp script
is in progress.

Return non-nil when active, nil otherwise.
Side effects: None.")

(defvar macher-agent-allowed-tools nil
  "Hold custom tool names that receive the active Macher context.

List of custom tool name symbols permitted to access context.

Return list of allowed tool symbol names.
Side effects: None.")

;;; Hooks

(defvar macher-agent-pre-tool-use-hook nil
  "Provide hook run before a tool executes.

Called with parameters (TOOL-NAME-SYM ARGUMENTS-PLIST).

Return hook run function list.
Side effects: None.")

(defvar macher-agent-permission-request-hook nil
  "Provide hook run for interactive approval before tool execution.

Called with parameters (TOOL-NAME-SYM ARGUMENTS-PLIST).

Return hook run function list.
Side effects: None.")

(defvar macher-agent-post-tool-use-hook nil
  "Provide hook run after a tool completes successfully.

Called with parameters (TOOL-NAME-SYM ARGUMENTS-PLIST OUTPUT).

Return hook run function list.
Side effects: None.")

(defvar macher-agent-post-tool-use-failure-hook nil
  "Provide hook run if a tool execution fails.

Called with parameters (TOOL-NAME-SYM ARGUMENTS-PLIST ERROR-DATA).

Return hook run function list.
Side effects: None.")

(defvar macher-agent-active-workspaces (make-hash-table :test 'equal)
  "Registry mapping expanded project roots to their active persistent contexts.")

(defvar macher-agent-context-mutated-hook nil
  "Hook run when a context is mutated.")

(defvar macher-agent--allow-lazy-init nil
  "When non-nil, allow lazy initialisation of workspace context.")

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

(defvar macher-agent-search-backend-function #'macher-agent-search-glob
  "Function variable used to dispatch conversation history searches.

When nil, conversation history search defaults to `macher-agent-search-glob`.
Can be dynamically overridden by memory plugins.

Return function symbol or nil.
Side effects: None.")

(defcustom macher-agent-max-context-chars '((nil . 2000000))
  "Alist mapping gptel model symbols to their maximum allowed context characters."
  :type '(alist :key-type symbol :value-type integer)
  :group 'macher-agent)

;; Buffer-local agent state
(defvar-local macher-agent--routing-stack nil
  "Stack of routing frame property lists for current sub-agent buffer.

Each frame is a property list containing :task-id, :originator-name,
and :suppress-patch.

Return list of routing frame plists, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--active-skill-sym nil
  "Store symbol representing the currently active skill preset in buffer.

This buffer-local variable holds the active skill symbol bound to the current
chat buffer.

Return the active skill symbol or nil.

Side effects: None.")

(defvar-local macher-agent--active-ptc-primitives nil
  "Store active Programmatic Tool Calling primitives for current buffer.

Hold a list of primitive tool symbols or names active in the buffer-local
execution context.

Return list of active primitive symbols or names.
Side effects: Buffer-local variable.")

(defvar-local macher-agent--pending-instructions-queue nil
  "Queue of ephemeral thoughts and instructions to inject on the next turn.

Return list of pending instruction strings, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--persistent-context nil
  "Store the buffer-local persistent context structure.

Hold the `macher-agent-context' instance bound to the current buffer across
agent turns.

Return the `macher-agent-context' struct, or nil if unset.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--current-task-id nil
  "Store task ID for current sub-agent execution.")

(defvar-local macher-agent--is-background nil
  "Flag whether the current buffer is running as a background sub-agent.

Return non-nil if current buffer is a background sub-agent, otherwise nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--is-ephemeral nil
  "Flag whether the current buffer is an ephemeral sub-agent.

Return non-nil if current buffer is an ephemeral sub-agent, otherwise nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--ready-to-reap nil
  "Flag whether the sub-agent buffer is ready for garbage collection.

Non-nil indicates that the sub-agent task has completed and can be reaped.

Return non-nil if sub-agent buffer is ready to be reaped, otherwise nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent-task-finished nil
  "Flag whether the current sub-agent task has finished execution.

Non-nil indicates that the task has reached completion and submitted results.

Return non-nil if current buffer task has finished, otherwise nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--task-result nil
  "Store the task result string or payload for the current agent execution.

Return the result data or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent-presets nil
  "Store list of active preset or skill symbols for current buffer.

Holds the active preset and skill symbols configured for the current buffer.

Return list of active preset or skill symbols, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--cached-presets nil
  "Store cached presets for current buffer prior to skill switching.

Holds original `macher-agent-presets' list before `use_skill' was invoked.

Return list of cached presets or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--suppress-patch nil
  "Flag to suppress interactive UI patch buffer generation.

Non-nil prevents the orchestrator from presenting the user with an
interactive visual patch review buffer upon task completion.

Crucially, this flag DOES NOT govern or inhibit underlying merges.
Sub-agents must always generate and transmit their diff payloads during
an Agent-to-Agent update (unless operating as a fire-and-forget background
task), regardless of this variable's state.

Return non-nil if interactive patch display is suppressed, otherwise nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--boot-directive nil
  "Initial boot directive instruction string for sub-agent execution.

Return the boot directive string or nil.

Side effects: Buffer-local variable.")

;; Permanent local puts

(put 'macher-agent--active-fsm 'permanent-local t)
(put 'macher-agent--pending-instructions-queue 'permanent-local t)
(put 'macher-agent--current-task-id 'permanent-local t)
(put 'macher-agent--is-background 'permanent-local t)
(put 'macher-agent--is-ephemeral 'permanent-local t)
(put 'macher-agent--ready-to-reap 'permanent-local t)
(put 'macher-agent-task-finished 'permanent-local t)
(put 'macher-agent--task-result 'permanent-local t)
(put 'macher-agent-presets 'permanent-local t)
(put 'macher-agent--cached-presets 'permanent-local t)
(put 'macher-agent--routing-stack 'permanent-local t)
(put 'macher-agent--active-ptc-primitives 'permanent-local t)
(put 'macher-agent--suppress-patch 'permanent-local t)
(put 'macher-agent--boot-directive 'permanent-local t)
(put 'macher-agent--persistent-context 'permanent-local t)
(put 'macher-agent-fsm-id 'permanent-local t)

;; Global ownership registry

(defvar macher-agent--a2a-ownership (make-hash-table :test 'equal)
  "Global ownership registry tracking sub-agents associated with an originator.

Keys are originator buffer name strings.
Values are lists of active sub-agent buffer name strings.")


;;

(defcustom macher-agent-display-subagent-fn nil
  "Specify function to display a sub-agent buffer during execution.

BUFFER is the buffer object to display.
If nil, the buffer executes silently in the background.

Return the display function or nil.
Side effects: None."
  :type '(choice (const :tag "Silent Background Execution" nil)
                 function)
  :group 'macher-agent)

;;

(defun macher-agent-get-cached-presets (&optional buffer)
  "Return cached presets for BUFFER, defaulting to `current-buffer'.

BUFFER is the buffer to inspect, defaulting to `current-buffer'.

Return list of cached preset symbols, or nil.
Side effects: None."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (buffer-local-value 'macher-agent--cached-presets buf))))

(defun macher-agent-clear-cached-presets (&optional buffer)
  "Clear cached presets in BUFFER, defaulting to `current-buffer'.

BUFFER is the buffer to clear, defaulting to `current-buffer'.

Return nil.
Side effects: Deletes `macher-agent--cached-presets' buffer-local binding."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (kill-local-variable 'macher-agent--cached-presets))))
  nil)

(defun macher-agent-ready-to-reap-p (&optional buffer)
  "Check whether BUFFER is ready to be reaped.

BUFFER is the optional target buffer, defaulting to the current buffer.

Return non-nil if BUFFER is ready to be reaped, otherwise nil.

Side effects: None."
  (with-current-buffer (or buffer (current-buffer))
    (bound-and-true-p macher-agent--ready-to-reap)))

(defun macher-agent--remove-active-subagent-registries (child-buf &optional _ignored)
  "Clear registries strictly tied to CHILD-BUF object."
  (let ((buf (if (bufferp child-buf) child-buf (if (bufferp _ignored) _ignored (get-buffer child-buf)))))
    (cl-check-type buf buffer)
    (let ((child-name (buffer-name buf)))
      (when (boundp 'macher-agent-active-subagents)
        (setq macher-agent-active-subagents
              (cl-delete-if (lambda (entry) (equal (if (consp entry) (car entry) entry) child-name))
                            macher-agent-active-subagents)))
      (when (and (boundp 'macher-agent--a2a-ownership)
                 (hash-table-p macher-agent--a2a-ownership))
        (maphash
         (lambda (key val)
           (when (listp val)
             (let ((new-val (cl-delete-if (lambda (entry)
                                            (let ((k (if (consp entry) (car entry) entry)))
                                              (equal k child-name)))
                                          val)))
               (if new-val
                   (puthash key new-val macher-agent--a2a-ownership)
                 (remhash key macher-agent--a2a-ownership)))))
         macher-agent--a2a-ownership))
      (when-let* ((ctx (buffer-local-value 'macher-agent--persistent-context buf)))
        (when (fboundp 'macher-agent-workspace-active-subagents)
          (let ((subs (macher-agent-workspace-active-subagents ctx)))
            (macher-agent--set-workspace-active-subagents
             ctx (cl-delete-if (lambda (entry) (equal (if (consp entry) (car entry) entry) child-name)) subs))))))))

(defun macher-agent-ui-show (buf)
  "Display the user interface for buffer BUF.

BUF is the optional buffer to display, defaulting to current buffer.

Return nil.

Side effects: Opens or focuses user interface window, invoking
`macher-agent-display-subagent-fn' when non-nil."
  (when macher-agent-display-subagent-fn
    (funcall macher-agent-display-subagent-fn buf)))

(defun macher-agent--push-routing (task-id originator-name &optional suppress-patch)
  "Push a routing context frame onto `macher-agent--routing-stack'.

TASK-ID is the task identifier string.
ORIGINATOR-NAME is the buffer name string of the originating agent.
SUPPRESS-PATCH is the optional patch suppression flag.

Return the pushed frame plist.

Side effects: Modifies buffer-local `macher-agent--routing-stack'."
  (let ((frame (list :task-id task-id
                     :originator-name originator-name
                     :suppress-patch suppress-patch)))
    (unless (boundp 'macher-agent--routing-stack)
      (setq-local macher-agent--routing-stack nil))
    (push frame macher-agent--routing-stack)
    (setq-local macher-agent--current-task-id task-id)
    (setq-local macher-agent--suppress-patch suppress-patch)
    frame))

(defun macher-agent--pop-routing ()
  "Pop a routing context frame from `macher-agent--routing-stack'.
Restores the previous routing frame's task-id and suppress-patch, or clears them.

Return the popped frame plist, or nil if stack was empty.

Side effects: Modifies buffer-local `macher-agent--routing-stack',
`macher-agent--current-task-id', and `macher-agent--suppress-patch'."
  (when (bound-and-true-p macher-agent--routing-stack)
    (let ((frame (pop macher-agent--routing-stack)))
      (if-let* ((prev (and (bound-and-true-p macher-agent--routing-stack)
                           (car macher-agent--routing-stack))))
          (progn
            (setq-local macher-agent--current-task-id (plist-get prev :task-id))
            (setq-local macher-agent--suppress-patch (plist-get prev :suppress-patch)))
        (setq-local macher-agent--current-task-id nil)
        (setq-local macher-agent--suppress-patch nil))
      frame)))

(defun macher-agent--plist-p (object)
  "Return t if OBJECT is a strictly formatted property list."
  (and (listp object) (plistp object)))

(defun macher-agent-valid-context-p (context)
  "Return t if CONTEXT is a strictly valid context structure."
  (macher-agent-context-p context))

(defun set-macher-agent-context-prompt (ctx val)
  "Set prompt on context CTX to VAL.

CTX is the `macher-agent-context' structure.
VAL is the prompt string.

Return VAL on success, or nil.
Side effects: Sets the prompt slot on CTX."
  (when (macher-agent-context-p ctx)
    (setf (macher-agent-context-prompt ctx) val))
  val)

(defun macher-agent-context-workspace (ctx)
  "Extract the workspace struct from CTX."
  (cl-check-type ctx macher-agent-context)
  (plist-get (macher-agent-context-plugins ctx) :workspace))

(defun macher-agent-context-workspace-root (context)
  "Extract canonical workspace root directory string from CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (let ((root (or (macher-agent-context-project-root context)
                  (let ((ws (macher-agent-context-workspace context)))
                    (cond
                     ((consp ws) (if (stringp (cdr ws)) (cdr ws) (plist-get ws :project-root)))
                     ((stringp ws) ws)
                     (t nil))))))
    (when (and root (stringp root))
      (file-truename (expand-file-name root)))))

(defun macher-agent-extract-workspace-id (input)
  "Extract the unique workspace ID from a pre-normalized INPUT list."
  (cl-check-type input list)
  (plist-get input :workspace-id))

(defvar macher-agent-task-flush-hook nil
  "Hook run when an agent task flushes or completes its execution cycle.

Functions in this hook are invoked to commit interaction memory, synchronise
storage, and flush pending artifacts.")

(defun macher-agent-run-task-flush-hook (context)
  "Invoke the flush hook."
  (dolist (hook-fn (if (consp macher-agent-task-flush-hook)
                       macher-agent-task-flush-hook
                     (list macher-agent-task-flush-hook)))
    (when (functionp hook-fn)
      (funcall hook-fn context))))

(defvar macher-agent-pipeline-registry (make-hash-table :test 'equal)
  "Registry storing pipeline definitions and ordered steps.

Maps pipeline identifier symbols to a list of step specifications
ordered strictly by ascending priority depth.")

(defun macher-agent-register-pipeline-step (pipeline-sym step-fn priority)
  "Register STEP-FN in PIPELINE-SYM queue."
  (cl-check-type pipeline-sym symbol)
  (let* ((current-entries (gethash pipeline-sym macher-agent-pipeline-registry))
         (filtered (cl-remove-if (lambda (entry)
                                   (or (equal (plist-get entry :step) step-fn)
                                       (equal (plist-get entry :fn) step-fn)))
                                 current-entries))
         (new-entry (list :step step-fn :fn step-fn :priority priority))
         (updated (sort (cons new-entry filtered)
                        (lambda (a b)
                          (< (plist-get a :priority) (plist-get b :priority))))))
    (puthash pipeline-sym updated macher-agent-pipeline-registry)
    updated))

(defun macher-agent-unregister-pipeline-step (pipeline-sym step-fn)
  "Unregister STEP-FN from PIPELINE-SYM queue."
  (cl-check-type pipeline-sym symbol)
  (let* ((current-entries (gethash pipeline-sym macher-agent-pipeline-registry))
         (filtered (cl-remove-if (lambda (entry)
                                   (or (equal (plist-get entry :step) step-fn)
                                       (equal (plist-get entry :fn) step-fn)))
                                 current-entries)))
    (puthash pipeline-sym filtered macher-agent-pipeline-registry)
    filtered))

(defun macher-agent-get-pipeline-steps (pipeline-sym)
  "Retrieve sequence of functions for PIPELINE-SYM."
  (cl-check-type pipeline-sym symbol)
  (let ((entries (gethash pipeline-sym macher-agent-pipeline-registry)))
    (mapcar (lambda (entry) (plist-get entry :step)) entries)))

(defun macher-agent-run-pipeline (pipeline-name initial-state)
  "Execute PIPELINE-NAME starting with INITIAL-STATE, returning the final state."
  (let ((state initial-state))
    (dolist (step (macher-agent-get-pipeline-steps pipeline-name))
      (when (functionp step)
        (setq state (funcall step state))))
    state))

(defconst macher-agent-transit-buffer-keys
  '(:target-buffer :parent-buffer :buffer :buffer-name)
  "Authoritative list of keys used to pass buffer references in transit payloads.")

(defconst macher-agent-a2a-schema-version :a2a-v1
  "Current schema version tag for Agent-to-Agent transit messages.")

(defconst macher-agent-a2a-transit-types
  '(:root-to-subagent :subagent-to-parent :peer-to-peer :state-sync)
  "Authoritative list of allowed transit types in A2A messages.")

(defun macher-agent-context-from-buffer (buffer-or-name)
  "Extract the context object strictly from a live buffer or string name."
  (let ((buf (if (bufferp buffer-or-name) buffer-or-name (get-buffer buffer-or-name))))
    (when (and buf (buffer-live-p buf))
      (buffer-local-value 'macher-agent--persistent-context buf))))

(defun macher-agent-context-from-payload (payload)
  "Extract the active `macher-agent-context' from a transit PAYLOAD struct."
  (cl-check-type payload macher-agent-transit-payload)
  (or (macher-agent-transit-payload-target-context payload)
      (macher-agent-transit-payload-parent-context payload)
      (macher-agent-transit-payload-child-context payload)
      (let ((shared (macher-agent-transit-payload-shared-state payload)))
        (and (listp shared)
             (or (plist-get shared :target-context)
                 (plist-get shared :parent-context)
                 (plist-get shared :child-context)
                 (plist-get shared :context))))))

(defun macher-agent-root (path)
  "Return the canonical project root for a specific string PATH."
  (cl-check-type path string)
  (let ((proj (and (fboundp 'project-current) (project-current nil path))))
    (expand-file-name
     (or (and proj (if (fboundp 'project-root) (project-root proj) (cdr proj)))
         (and (fboundp 'vc-root-dir) (let ((default-directory path)) (vc-root-dir)))
         path))))

(defun macher-tool-valid-p (tool)
  "Validate whether TOOL is a valid gptel tool structure.

TOOL is the object to inspect.

Return non-nil if TOOL is a valid gptel tool, otherwise nil.

Side effects: None."
  (and tool (fboundp 'gptel-tool-p) (gptel-tool-p tool)))

(defun macher-agent--extract-raw-tool-name (tool)
  "Extract raw name representation from TOOL structure or list.
This acts as the boundary normaliser for raw YAML tool specifications."
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

(defun macher-normalise-preset-name (preset)
  "Convert PRESET to a uniform symbol without leading character symbols.

PRESET is the preset string, symbol, or list to normalise.

Return the normalised preset symbol, or nil if PRESET is nil.

Side effects: None."
  (when preset
    (let ((str (cond ((stringp preset) preset)
                     ((symbolp preset) (symbol-name preset))
                     ((and (listp preset) (car preset))
                      (if (symbolp (car preset))
                          (symbol-name (car preset))
                        (format "%s" (car preset))))
                     (t (format "%s" preset)))))
      (intern (replace-regexp-in-string "^@+" "" str)))))

(defun macher-agent-core-get-buffer (name-or-buf &optional create)
  "Get or resolve a buffer from NAME-OR-BUF.
NAME-OR-BUF can be a buffer object, a buffer name string, a file path
string, a context struct, or nil.
If NAME-OR-BUF is nil, return the current buffer.
If CREATE is non-nil and the buffer does not exist, create it with
`get-buffer-create'.

Return the buffer object, or nil if not found."
  (cond
   ((null name-or-buf) (current-buffer))
   ((bufferp name-or-buf)
    (if (buffer-live-p name-or-buf)
        name-or-buf
      (when create
        (get-buffer-create (buffer-name name-or-buf)))))
   ((macher-agent-context-p name-or-buf)
    (let ((buf (or (macher-agent-context-origin-buffer name-or-buf)
                   (let ((plugins (macher-agent-context-plugins name-or-buf)))
                     (when (macher-agent--plist-p plugins)
                       (plist-get plugins :buffer))))))
      (when buf
        (macher-agent-core-get-buffer buf create))))
   ((macher-agent-task-context-p name-or-buf)
    (let ((buf (macher-agent-task-context-target-buffer name-or-buf)))
      (when buf
        (macher-agent-core-get-buffer buf create))))
   ((stringp name-or-buf)
    (or (get-buffer name-or-buf)
        (get-file-buffer name-or-buf)
        (get-file-buffer (expand-file-name name-or-buf))
        (when create
          (get-buffer-create name-or-buf))))
   (t nil)))

(defun macher-agent--get-context-root (context)
  "Extract root explicitly from a CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (macher-agent-context-root context))

(defun macher-agent--get-name (workspace)
  "Get a display name for WORKSPACE.
This satisfies the upstream `macher-workspace-types-alist' interface."
  (let* ((root (cond
                ((and (macher-agent-context-p workspace)
                      (macher-agent-context-project-root workspace))
                 (macher-agent-context-project-root workspace))
                ((macher-agent-context-p workspace)
                 (macher-agent-context-root workspace))
                (t (when (fboundp 'macher-agent-workspace-project-root)
                     (condition-case nil
                         (macher-agent-workspace-project-root workspace)
                       (error nil))))))
         (name (when (and root (stringp root) (not (string-empty-p root)))
                 (file-name-nondirectory (directory-file-name root)))))
    (if (and name (not (string-empty-p name)))
        (concat "Agent: " name)
      "Agent: Workspace")))

(defun macher-agent-workspace-project-root (ws)
  "Extract the absolute project root from WS struct."
  (cl-check-type ws macher-agent-context)
  (macher-agent-context-project-root ws))

(defun macher-agent--set-workspace-project-root (workspace path)
  "Update the absolute project root in WORKSPACE struct."
  (cl-check-type workspace macher-agent-context)
  (setf (macher-agent-context-project-root workspace) path))

(gv-define-setter macher-agent-workspace-project-root (val ws)
  `(macher-agent--set-workspace-project-root ,ws ,val))

(defun macher-agent-context-root (context)
  "Extract root explicitly from CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (macher-agent-context-project-root context))

(defun macher-agent-search-glob (orig-buf query &optional ctx-lines)
  "Execute search QUERY against live ORIG-BUF."
  (when (and (bufferp query) (stringp orig-buf))
    (cl-rotatef orig-buf query))
  (cl-check-type orig-buf buffer)
  (let ((ctx-lines (if (and ctx-lines (> ctx-lines 0)) ctx-lines 5))
        (results nil)
        (invalid-re nil))
    (if (not (buffer-live-p orig-buf))
        "Error: Cannot locate original conversation buffer."
      (with-current-buffer orig-buf
        (save-excursion
          (goto-char (point-min))
          (condition-case err
              (let ((continue t))
                (while (and continue (re-search-forward query nil t))
                  (let* ((match-beg (match-beginning 0))
                         (match-end (match-end 0))
                         (start-pt (save-excursion
                                     (goto-char match-beg)
                                     (forward-line (- ctx-lines))
                                     (line-beginning-position)))
                         (end-pt (save-excursion
                                   (goto-char match-beg)
                                   (forward-line ctx-lines)
                                   (line-end-position)))
                         (snippet (buffer-substring-no-properties start-pt end-pt)))
                    (push (format "--- Match near line %d ---\n%s\n"
                                  (line-number-at-pos match-beg) snippet)
                          results)
                    (when (= match-beg match-end)
                      (if (eobp)
                          (setq continue nil)
                        (forward-char 1))))))
            (invalid-regexp
             (setq invalid-re (error-message-string err))))))
      (cond
       (invalid-re
        (format "Error: Invalid regular expression: %s" invalid-re))
       (results
        (string-join (nreverse results) "\n"))
       (t
        (format "No matches found in history for: %s" query))))))

(defun macher-agent-search-dispatch (query &optional target-buf context-lines)
  "Dispatch search for QUERY in TARGET-BUF with CONTEXT-LINES context."
  (let ((buf (or target-buf (current-buffer))))
    (if (not (buffer-live-p buf))
        "Error: Cannot locate original conversation buffer."
      (if macher-agent-search-backend-function
          (funcall macher-agent-search-backend-function buf query context-lines)
        (macher-agent-search-glob buf query context-lines)))))

(defun macher-agent--get-context-contents (ctx)
  "Extract file contents strictly from CTX struct."
  (cl-check-type ctx macher-agent-context)
  (plist-get (macher-agent-vfs--get-state ctx) :contents))

(defun macher-agent--set-context-contents (ctx val)
  "Set file contents strictly on CTX struct."
  (cl-check-type ctx macher-agent-context)
  (cl-check-type val list)
  (let ((state (macher-agent-vfs--get-state ctx)))
    (macher-agent-vfs--set-state ctx (plist-put state :contents val))))

(defun macher-agent--get-context-dirty-p (ctx)
  "Check dirty flag strictly on CTX struct."
  (cl-check-type ctx macher-agent-context)
  (or (plist-get (macher-agent-vfs--get-state ctx) :dirty)
      (plist-get (macher-agent-vfs--get-state ctx) :dirty-p)))

(defun macher-agent--set-context-dirty-p (ctx is-dirty)
  "Set dirty flag strictly on CTX struct."
  (cl-check-type ctx macher-agent-context)
  (let ((state (macher-agent-vfs--get-state ctx)))
    (macher-agent-vfs--set-state ctx (plist-put (plist-put state :dirty is-dirty) :dirty-p is-dirty))))

(provide 'macher-agent-core)
;;; macher-agent-core.el ends here
