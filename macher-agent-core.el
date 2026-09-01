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

(defalias 'make-macher-agent-context #'macher-agent--make-context)

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

(defun macher-agent-ready-to-reap-p (&optional buffer)
  "Check whether BUFFER is ready to be reaped.

BUFFER is the optional target buffer, defaulting to the current buffer.

Return non-nil if BUFFER is ready to be reaped, otherwise nil.

Side effects: None."
  (with-current-buffer (or buffer (current-buffer))
    (bound-and-true-p macher-agent--ready-to-reap)))

(defun macher-agent--remove-active-subagent-registries (child-name child-buf)
  "Remove CHILD-NAME and CHILD-BUF from subagent registries.
Also remove CHILD-NAME from all parent ownership lists in
`macher-agent--a2a-ownership'.

Return nil.
Side effects: Updates `macher-agent-active-subagents', workspace
active subagents, and `macher-agent--a2a-ownership'."
  (when (boundp 'macher-agent-active-subagents)
    (setq macher-agent-active-subagents
          (cl-delete-if (lambda (entry)
                          (let ((k (if (consp entry) (car entry) entry)))
                            (or (and child-name (equal k child-name))
                                (and child-buf (equal k child-buf))
                                (and child-buf (buffer-live-p child-buf) (equal k (buffer-name child-buf))))))
                        macher-agent-active-subagents)))
  (when (and (boundp 'macher-agent--a2a-ownership)
             (hash-table-p macher-agent--a2a-ownership))
    (maphash
     (lambda (key val)
       (when (listp val)
         (let ((new-val (cl-delete-if (lambda (entry)
                                        (let ((k (if (consp entry) (car entry) entry)))
                                          (or (and child-name (equal k child-name))
                                              (and child-buf (equal k child-buf))
                                              (and child-buf (buffer-live-p child-buf) (equal k (buffer-name child-buf))))))
                                      val)))
           (if new-val
               (puthash key new-val macher-agent--a2a-ownership)
             (remhash key macher-agent--a2a-ownership)))))
     macher-agent--a2a-ownership))
  (let* ((ctx (when (and child-buf (buffer-live-p child-buf))
                (buffer-local-value 'macher-agent--persistent-context child-buf)))
         (ws (when ctx
               (macher-agent-context-workspace ctx))))
    (when ws
      (let ((subs (when (fboundp 'macher-agent-workspace-active-subagents)
                    (funcall 'macher-agent-workspace-active-subagents ws))))
        (when (and subs (fboundp 'macher-agent--set-workspace-active-subagents))
          (funcall 'macher-agent--set-workspace-active-subagents
                   ws
                   (cl-delete-if (lambda (entry)
                                   (let ((k (if (consp entry) (car entry) entry)))
                                     (or (and child-name (equal k child-name))
                                         (and child-buf (equal k child-buf))
                                         (and child-buf (buffer-live-p child-buf) (equal k (buffer-name child-buf))))))
                                 subs)))))))

(defun macher-agent-ui-show (&optional buf)
  "Display the user interface for buffer BUF.

BUF is the optional buffer to display, defaulting to current buffer.

Return nil.

Side effects: Opens or focuses user interface window, invoking
`macher-agent-display-subagent-fn' when non-nil."
  (let ((target-buf (or buf (current-buffer))))
    (when macher-agent-display-subagent-fn
      (funcall macher-agent-display-subagent-fn target-buf))))

(defun macher-agent--plist-p (object)
  "Return non-nil if OBJECT is a valid property list.

OBJECT is the value to test.

Return non-nil if OBJECT is a valid plist, otherwise nil."
  (if (fboundp 'plistp)
      (plistp object)
    (and (listp object)
         (let ((lst object)
               (valid t))
           (while (and valid (consp lst))
             (if (not (consp (cdr lst)))
                 (setq valid nil)
               (setq lst (cddr lst))))
           (and valid (null lst))))))

(defun macher-agent-valid-context-p (context)
  "Determine whether CONTEXT is a valid persistent context structure.

CONTEXT is the context object to validate.

Return non-nil if CONTEXT is a `macher-agent-context', or nil otherwise.

Side effects: None."
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
  "Retrieve the tagged workspace structure from context CTX.

CTX is the active `macher-agent-context' structure.

Return cons cell `(project . path)', or nil.
Side effects: None."
  (when (macher-agent-context-p ctx)
    (or (let ((plugins (macher-agent-context-plugins ctx)))
          (when (macher-agent--plist-p plugins)
            (let ((vfs (plist-get plugins :vfs)))
              (or (when (macher-agent--plist-p vfs)
                    (plist-get vfs :workspace))
                  (plist-get plugins :workspace)))))
        (when-let* ((root (macher-agent-context-project-root ctx)))
          (cons 'project (expand-file-name (if (consp root) (cdr root) root)))))))

(defun macher-agent-extract-workspace-id (input)
  "Extract workspace identifier or project root from INPUT enforcing strict keys."
  (cond
   ((stringp input) input)
   ((and (consp input) (eq (car input) 'project)) input)
   ((and (consp input) (eq (car input) 'agent)) input)
   ((and (recordp input) (fboundp 'macher-agent-workspace-p) (funcall 'macher-agent-workspace-p input)) input)
   ((macher-agent--plist-p input)
    (let* ((shared (plist-get input :shared-state))
           (ws-id (plist-get input :workspace-id))
           (ws (plist-get input :workspace))
           (t-ws (plist-get input :target-workspace))
           (proj (plist-get input :project))
           (s-ws-id (when (macher-agent--plist-p shared) (plist-get shared :workspace-id)))
           (s-ws (when (macher-agent--plist-p shared) (plist-get shared :workspace))))
      (or ws-id ws t-ws proj s-ws-id s-ws)))
   (t nil)))

(defvar macher-agent-task-flush-hook nil
  "Hook run when an agent task flushes or completes its execution cycle.

Functions in this hook are invoked to commit interaction memory, synchronise
storage, and flush pending artifacts.")

(defun macher-agent-run-task-flush-hook (&optional context)
  "Safely invoke the flush hook, handling legacy 0-arg and new 1-arg functions."
  (let ((ctx (when (macher-agent-valid-context-p context) context)))
    (dolist (hook-fn (if (consp macher-agent-task-flush-hook)
                         macher-agent-task-flush-hook
                       (list macher-agent-task-flush-hook)))
      (when (functionp hook-fn)
        (condition-case nil
            (if ctx (funcall hook-fn ctx) (funcall hook-fn nil))
          (wrong-number-of-arguments (funcall hook-fn)))))))

(defvar macher-agent-pipeline-registry (make-hash-table :test 'equal)
  "Registry storing pipeline definitions and ordered steps.

Maps pipeline identifier symbols to a list of step specifications
ordered strictly by ascending priority depth.")

(defun macher-agent-register-pipeline-step (pipeline step priority)
  "Register STEP function in PIPELINE at the specified PRIORITY depth.

PIPELINE is the symbol or string identifying the target pipeline.
STEP is the step function symbol or lambda to execute.
PRIORITY is an integer defining the execution priority depth, where
lower integer values indicate earlier execution in the pipeline sequence.

Return the ordered list of registered step entries for PIPELINE.

Side effects: Updates `macher-agent-pipeline-registry`."
  (let* ((sym-key (if (symbolp pipeline) pipeline (intern (format "%s" pipeline))))
         (current-entries (gethash sym-key macher-agent-pipeline-registry))
         (filtered (cl-remove-if (lambda (entry)
                                   (equal (plist-get entry :step) step))
                                 current-entries))
         (new-entry (list :step step :priority priority))
         (updated (sort (cons new-entry filtered)
                        (lambda (a b)
                          (< (plist-get a :priority) (plist-get b :priority))))))
    (puthash sym-key updated macher-agent-pipeline-registry)
    updated))

(defun macher-agent-unregister-pipeline-step (pipeline step)
  "Unregister STEP function from PIPELINE.

PIPELINE is the symbol or string identifying the target pipeline.
STEP is the step function symbol or lambda to remove.

Return the updated list of registered step entries for PIPELINE.

Side effects: Updates `macher-agent-pipeline-registry`."
  (let* ((sym-key (if (symbolp pipeline) pipeline (intern (format "%s" pipeline))))
         (current-entries (gethash sym-key macher-agent-pipeline-registry))
         (filtered (cl-remove-if (lambda (entry)
                                   (equal (plist-get entry :step) step))
                                 current-entries)))
    (puthash sym-key filtered macher-agent-pipeline-registry)
    filtered))

(defun macher-agent-get-pipeline-steps (pipeline)
  "Retrieve the ordered list of step functions for PIPELINE.

PIPELINE is the symbol or string identifying the pipeline.

Return a list of step functions ordered strictly by priority depth.

Side effects: None."
  (let* ((sym-key (if (symbolp pipeline) pipeline (intern (format "%s" pipeline))))
         (entries (gethash sym-key macher-agent-pipeline-registry)))
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
                                              message
                                              metadata)
  "Construct a validated `macher-agent-transit-payload' struct.

SCHEMA-VERSION defaults to `:a2a-v1'.
TRANSIT-TYPE must be one of `macher-agent-a2a-transit-types'.
TARGET-CONTEXT, PARENT-CONTEXT, and CHILD-CONTEXT hold context structures.
SHARED-STATE holds shared task metadata.
PAYLOAD holds the message body or data.

Return the constructed and validated `macher-agent-transit-payload' struct."
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
   :payload (or payload message)
   :metadata metadata))

(defun macher-agent-context-from-buffer (buffer)
  "Extract the persistent `macher-agent-context' from BUFFER.

BUFFER is the buffer object or buffer name string to inspect.

Return the `macher-agent-context' structure stored in BUFFER's
`macher-agent--persistent-context', or nil if BUFFER is invalid,
not live, or context is unset.

Side effects: None."
  (let ((buf (if (bufferp buffer) buffer (and (stringp buffer) (get-buffer buffer)))))
    (when (and buf (buffer-live-p buf))
      (let ((ctx (buffer-local-value 'macher-agent--persistent-context buf)))
        (when (macher-agent-valid-context-p ctx)
          ctx)))))

(defun macher-agent-context-from-payload (payload)
  "Extract the active `macher-agent-context' from PAYLOAD.

PAYLOAD is a `macher-agent-transit-payload' structure or a `macher-agent-context' structure.

Return the resolved `macher-agent-context' structure.
Signals an error if PAYLOAD is invalid or no context can be resolved.

Side effects: None."
  (cond
   ((macher-agent-valid-context-p payload)
    payload)

   ((macher-agent-transit-payload-p payload)
    (or (let ((c (macher-agent-transit-payload-target-context payload)))
          (when (macher-agent-valid-context-p c) c))
        (let ((c (macher-agent-transit-payload-parent-context payload)))
          (when (macher-agent-valid-context-p c) c))
        (let ((c (macher-agent-transit-payload-child-context payload)))
          (when (macher-agent-valid-context-p c) c))
        (let ((shared (macher-agent-transit-payload-shared-state payload)))
          (when (macher-agent--plist-p shared)
            (or (let ((c (plist-get shared :target-context)))
                  (when (macher-agent-valid-context-p c) c))
                (let ((c (plist-get shared :parent-context)))
                  (when (macher-agent-valid-context-p c) c))
                (let ((c (plist-get shared :child-context)))
                  (when (macher-agent-valid-context-p c) c))
                (let ((c (plist-get shared :context)))
                  (when (macher-agent-valid-context-p c) c)))))
        (let* ((b-raw (macher-agent-transit-payload-target-buffer payload))
               (buf (when b-raw (if (bufferp b-raw) b-raw (and (stringp b-raw) (get-buffer b-raw))))))
          (when (and buf (buffer-live-p buf))
            (let ((bctx (buffer-local-value 'macher-agent--persistent-context buf)))
              (when (macher-agent-valid-context-p bctx) bctx))))
        (error "Cannot resolve context from transit payload: %S" payload)))

   (t
    (error "Invalid transit payload: expected context or transit payload struct, got %S" payload))))

(defun macher-agent-resolve-from-transit-payload (payload-or-state)
  "Extract context from PAYLOAD-OR-STATE using transit keys.
Reject invalid payloads with an error signal rather than falling
back to nil silently."
  (cond
   ((and (macher-agent--plist-p payload-or-state)
         (plist-member payload-or-state :resolved)
         (plist-member payload-or-state :input))
    (if (plist-get payload-or-state :resolved)
        payload-or-state
      (let* ((input (plist-get payload-or-state :input))
             (ctx (when input
                    (condition-case nil
                        (macher-agent-context-from-payload input)
                      (error nil)))))
        (if (and ctx (macher-agent-valid-context-p ctx))
            (plist-put payload-or-state :resolved ctx)
          payload-or-state))))
   (t
    (macher-agent-context-from-payload payload-or-state))))

(defun macher-agent-root (&optional path)
  "Resolve absolute project root path from PATH.

PATH is a directory or file path, defaulting to `default-directory`.

Return the absolute project root path string."
  (let* ((dir (if (stringp path) path default-directory))
         (proj (and (fboundp 'project-current) (project-current nil dir))))
    (expand-file-name
     (or (and proj (if (fboundp 'project-root) (project-root proj) (cdr proj)))
         (and (fboundp 'vc-root-dir) (let ((default-directory dir)) (vc-root-dir)))
         dir))))

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
  "Retrieve the project root directory string from CONTEXT.

CONTEXT is the active context structure or workspace.

Return the project root path string."
  (or (cond
       ((null context) nil)
       ((stringp context) (file-truename (expand-file-name context)))
       ((and (consp context) (memq (car context) '(project agent directory)))
        (let ((path (cdr context)))
          (when (stringp path) (file-truename (expand-file-name path)))))
       ((macher-agent-context-p context)
        (or (let ((r (macher-agent-context-project-root context)))
              (when (and r (stringp r))
                (file-truename (expand-file-name r))))
            (when-let* ((ws (macher-agent-context-workspace context)))
              (let ((proj-root (macher-agent-workspace-project-root ws)))
                (when (stringp proj-root) (file-truename (expand-file-name proj-root)))))))
       ((consp context)
        (let ((proj-root (macher-agent-workspace-project-root context)))
          (when (stringp proj-root) (file-truename (expand-file-name proj-root))))))
      (file-truename default-directory)))

(defun macher-agent--get-name (workspace)
  "Get a display name for WORKSPACE.
This satisfies the upstream `macher-workspace-types-alist' interface."
  (let* ((root (cond
                ((and (macher-agent-context-p workspace)
                      (macher-agent-context-project-root workspace))
                 (macher-agent-context-project-root workspace))
                ((macher-agent-context-p workspace)
                 (macher-agent-context-root workspace))
                (t (macher-agent-workspace-project-root workspace))))
         (name (when (and root (stringp root) (not (string-empty-p root)))
                 (file-name-nondirectory (directory-file-name root)))))
    (if (and name (not (string-empty-p name)))
        (concat "Agent: " name)
      "Agent: Workspace")))

(defun macher-agent-workspace-project-root (ws)
  "Retrieve the project root directory string from workspace WS."
  (cond
   ((null ws) nil)
   ((stringp ws) (file-truename (expand-file-name ws)))
   ((macher-agent-context-p ws)
    (or (let ((r (macher-agent-context-project-root ws)))
          (when (and r (stringp r))
            (file-truename (expand-file-name r))))
        (when-let* ((inner-ws (macher-agent-context-workspace ws)))
          (macher-agent-workspace-project-root inner-ws))))
   ((and (consp ws) (memq (car ws) '(project agent directory)))
    (let ((path (cdr ws)))
      (cond
       ((stringp path) (file-truename (expand-file-name path)))
       ((null path) nil)
       ((and (fboundp 'project-root) (consp path))
        (or (condition-case nil
                (let ((r (project-root path)))
                  (and r (file-truename (expand-file-name r))))
              (error nil))
            (macher-agent-workspace-project-root path)))
       (t (macher-agent-workspace-project-root path)))))
   ((and (consp ws) (consp (car ws)) (memq (caar ws) '(project agent directory)))
    (let ((path (cdar ws)))
      (cond
       ((stringp path) (file-truename (expand-file-name path)))
       ((null path) nil)
       ((and (fboundp 'project-root) (consp path))
        (or (condition-case nil
                (let ((r (project-root path)))
                  (and r (file-truename (expand-file-name r))))
              (error nil))
            (macher-agent-workspace-project-root path)))
       (t (macher-agent-workspace-project-root path)))))
   ((and (fboundp 'project-root) (not (and (consp ws) (memq (car ws) '(project agent directory)))))
    (or (condition-case nil
            (let ((r (project-root ws)))
              (and r (file-truename (expand-file-name r))))
          (error nil))
        (condition-case nil
            (and (consp ws)
                 (let ((r (project-root (car ws))))
                   (and r (file-truename (expand-file-name r)))))
          (error nil))
        (condition-case nil
            (and (consp ws) (fboundp 'project-current)
                 (let ((p (project-current nil (car ws))))
                   (and p (let ((r (project-root p)))
                            (and r (file-truename (expand-file-name r)))))))
          (error nil))
        (when (consp ws)
          (or (macher-agent-workspace-project-root (car ws))
              (let ((path (cdr ws)))
                (if (stringp path)
                    (file-truename (expand-file-name path))
                  (macher-agent-workspace-project-root path)))))))
   (t (when (consp ws)
        (or (when (and (fboundp 'project-current) (fboundp 'project-root) (car ws))
              (condition-case nil
                  (let ((p (project-current nil (car ws))))
                    (and p (let ((r (project-root p)))
                             (and r (file-truename (expand-file-name r))))))
                (error nil)))
            (macher-agent-workspace-project-root (car ws))
            (let ((path (cdr ws)))
              (if (stringp path)
                  (file-truename (expand-file-name path))
                (macher-agent-workspace-project-root path))))))))

(defun macher-agent-context-root (context)
  "Safely resolve the project root for CONTEXT across diverse context types."
  (or (when-let* ((ws (when (macher-agent-context-p context)
                        (macher-agent-context-workspace context))))
        (macher-agent-workspace-project-root ws))
      (macher-agent--get-context-root context)))

(defun macher-agent-search-glob (query orig-buf &optional ctx-lines)
  "Search for QUERY in ORIG-BUF matching CTX-LINES."
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
          (funcall macher-agent-search-backend-function query buf context-lines)
        (macher-agent-search-glob query buf context-lines)))))

(defun macher-agent--get-context-contents (ctx)
  "Safely retrieve contents list from CTX, delegating to VFS plugin if active."
  (when (macher-agent-context-p ctx)
    (if (fboundp 'macher-agent-vfs--get-state)
        (let ((state (macher-agent-vfs--get-state ctx)))
          (if state
              (plist-get state :contents)
            (let ((plugins (macher-agent-context-plugins ctx)))
              (when (macher-agent--plist-p plugins)
                (plist-get plugins :contents)))))
      (let ((plugins (macher-agent-context-plugins ctx)))
        (when (macher-agent--plist-p plugins)
          (plist-get plugins :contents))))))

(defun macher-agent--set-context-contents (ctx val)
  "Safely set contents list on CTX to VAL, delegating to VFS plugin if active.
Automatically coerces raw strings, property lists, and legacy formats
into strictly typed structs with relativised paths."
  (when (macher-agent-context-p ctx)
    (let* ((root (or (macher-agent-context-root ctx)
                     default-directory))
           (coerced-val
            (when (listp val)
              (mapcar
               (lambda (item)
                 (cond
                  ((macher-agent-vfs-entry-p item) item)
                  ((stringp item)
                   (make-macher-agent-vfs-entry
                    :path (if (fboundp 'macher-agent-to-relative-path)
                              (macher-agent-to-relative-path item root)
                            item)
                    :orig nil :curr nil))
                  ((and (listp item) (keywordp (car item)))
                   (let* ((raw-path (or (plist-get item :path) (plist-get item :file)))
                          (path (if (and (stringp raw-path) (fboundp 'macher-agent-to-relative-path))
                                    (macher-agent-to-relative-path raw-path root)
                                  raw-path))
                          (orig (plist-get item :orig))
                          (curr (or (plist-get item :curr) (plist-get item :content) (plist-get item :contents))))
                     (make-macher-agent-vfs-entry :path path :orig orig :curr curr)))
                  ((and (consp item) (consp (car item)))
                   (let* ((raw-path (or (cdr (assq 'path item)) (cdr (assq 'file item))))
                          (path (if (and (stringp raw-path) (fboundp 'macher-agent-to-relative-path))
                                    (macher-agent-to-relative-path raw-path root)
                                  raw-path))
                          (orig (cdr (assq 'orig item)))
                          (curr (or (cdr (assq 'curr item)) (cdr (assq 'content item)) (cdr (assq 'contents item)))))
                     (make-macher-agent-vfs-entry :path path :orig orig :curr curr)))
                  ((consp item)
                   (let* ((raw-path (car item))
                          (path (if (and (stringp raw-path) (fboundp 'macher-agent-to-relative-path))
                                    (macher-agent-to-relative-path raw-path root)
                                  raw-path))
                          (rest (cdr item)))
                     (if (consp rest)
                         (make-macher-agent-vfs-entry :path path :orig (car rest) :curr (cdr rest))
                       (make-macher-agent-vfs-entry :path path :orig nil :curr rest))))
                  (t item)))
               val))))
      (if (fboundp 'macher-agent-vfs--get-state)
          (let ((state (or (macher-agent-vfs--get-state ctx) (list :contents nil :dirty-p nil))))
            (macher-agent-vfs--set-state ctx (plist-put state :contents coerced-val)))
        (let* ((plugins (macher-agent-context-plugins ctx))
               (updated (plist-put (copy-sequence plugins) :contents coerced-val)))
          (setf (macher-agent-context-plugins ctx) updated)))
      coerced-val)))

(defun macher-agent--get-context-dirty-p (ctx)
  "Safely retrieve dirty flag from CTX, delegating to VFS plugin if active."
  (when (macher-agent-context-p ctx)
    (if (fboundp 'macher-agent-vfs--get-state)
        (let ((state (macher-agent-vfs--get-state ctx)))
          (if state
              (plist-get state :dirty-p)
            (let ((plugins (macher-agent-context-plugins ctx)))
              (when (macher-agent--plist-p plugins)
                (plist-get plugins :dirty-p)))))
      (let ((plugins (macher-agent-context-plugins ctx)))
        (when (macher-agent--plist-p plugins)
          (plist-get plugins :dirty-p))))))

(defun macher-agent--set-context-dirty-p (ctx val)
  "Safely set dirty flag on CTX to VAL, delegating to VFS plugin if active."
  (when (macher-agent-context-p ctx)
    (if (fboundp 'macher-agent-vfs--get-state)
        (let ((state (or (macher-agent-vfs--get-state ctx) (list :contents nil :dirty-p nil))))
          (macher-agent-vfs--set-state ctx (plist-put state :dirty-p val)))
      (let* ((plugins (macher-agent-context-plugins ctx))
             (updated (plist-put (copy-sequence plugins) :dirty-p val)))
        (setf (macher-agent-context-plugins ctx) updated)))
    val))

(provide 'macher-agent-core)
;;; macher-agent-core.el ends here
