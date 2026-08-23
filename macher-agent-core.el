;;; macher-agent-core.el --- Core state for Macher Agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Core state variables and functions for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)

(declare-function gptel-fsm-p "gptel" (fsm))
(declare-function gptel-fsm-info "gptel" (fsm))
(declare-function gptel-tool-p "gptel" (tool))
(declare-function gptel-tool-name "gptel" (tool))
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-root "project" (project))
(declare-function vc-root-dir "vc-hooks" ())

(cl-defstruct (macher-agent-context
               (:constructor make-macher-agent-context)
               (:predicate macher-agent--context-struct-p))
  "Opaque universal Macher Agent context structure."
  workspace project-root buffer active-fsm parent-buffer task-id dirty-p data extra prompt)

;; Universal constants and global state

(defvar macher-agent-ptc-raw-mode nil
  "Toggle raw Lisp data return for Programmatic Tool Calling execution.

If non-nil, tools bypass string formatting and return raw Lisp data.

Return non-nil when raw mode is active, nil otherwise.
Side effects: None.")

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

(defvar macher-agent--pending-tool-media-alist nil
  "Maintain mapping of buffer objects to pending media items.

Return alist mapping buffers to pending media files or data.
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
  "Hook run when a VFS context is mutated.")

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

(defvar-local macher-agent--final-result nil
  "Store the final result string or data returned by agent execution.")

(defvar-local macher-agent--persistent-context nil
  "Store the buffer-local persistent VFS context structure.

Hold the `macher-context' instance bound to the current buffer across
agent turns.

Return the `macher-context' struct, or nil if unset.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--current-task-id nil
  "Store task ID for current sub-agent execution.")

(defvar-local macher-agent--is-subagent nil
  "Flag whether the current buffer is managed as a sub-agent.

Non-nil indicates that the buffer is an isolated sub-agent buffer.

Return non-nil if current buffer is a sub-agent, otherwise nil.

Side effects: Buffer-local variable.")

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

Crucially, this flag DOES NOT govern or inhibit the underlying Virtual
File System (VFS) merges. Sub-agents must always generate and transmit
their VFS diff payloads during an Agent-to-Agent update (unless operating
as a fire-and-forget background task), regardless of this variable's state.

Return non-nil if interactive patch display is suppressed, otherwise nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--boot-directive nil
  "Initial boot directive instruction string for sub-agent execution.

Return the boot directive string or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent-fsm-id nil
  "Buffer-local identifier bridging user interface buffers to the state machine.

Stores the unique state machine identifier associated with the current
user interface buffer session.

Return the state machine identifier string or symbol, or nil if unassigned.

Side effects: Buffer-local variable.")

;; Permanent local puts

(put 'macher-agent--pending-instructions-queue 'permanent-local t)
(put 'macher-agent--current-task-id 'permanent-local t)
(put 'macher-agent--is-subagent 'permanent-local t)
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

(defcustom macher-agent-hide-subagent-fn nil
  "Specify function to hide a sub-agent buffer after execution finishes.

BUFFER is the buffer object to hide.
If nil, no action is taken when finished.

Return the hide function or nil.
Side effects: None."
  :type '(choice (const :tag "Do Nothing" nil)
                 function)
  :group 'macher-agent)

;;

(defun macher-agent-subagent-p (&optional buffer)
  "Check whether BUFFER is a sub-agent buffer.

BUFFER is the optional target buffer, defaulting to the current buffer.

Return non-nil if BUFFER is a sub-agent, otherwise nil.

Side effects: None."
  (with-current-buffer (or buffer (current-buffer))
    (bound-and-true-p macher-agent--is-subagent)))

(defun macher-agent-ready-to-reap-p (&optional buffer)
  "Check whether BUFFER is ready to be reaped.

BUFFER is the optional target buffer, defaulting to the current buffer.

Return non-nil if BUFFER is ready to be reaped, otherwise nil.

Side effects: None."
  (with-current-buffer (or buffer (current-buffer))
    (bound-and-true-p macher-agent--ready-to-reap)))

(defun macher-agent--remove-active-subagent-registries (child-name child-buf)
  "Remove CHILD-NAME and CHILD-BUF from global and workspace active subagent registries,
and remove CHILD-NAME from all parent ownership lists in `macher-agent--a2a-ownership'.

Return nil.
Side effects: Updates `macher-agent-active-subagents', workspace active subagents,
and `macher-agent--a2a-ownership'."
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
  (let* ((ctx (or (when (and child-buf (buffer-live-p child-buf))
                    (buffer-local-value 'macher-agent--persistent-context child-buf))
                  (ignore-errors (macher-agent-resolve-context))))
         (ws (when ctx
               (ignore-errors (macher-agent--get-context-workspace ctx)))))
    (when ws
      (let ((subs (ignore-errors (macher-agent-workspace-active-subagents ws))))
        (when subs
          (macher-agent--set-workspace-active-subagents
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

(defun macher-agent--extract-prop (obj key)
  "Extract KEY from OBJ agnostically.
OBJ is a hash-table, alist, or plist.
KEY is the property key symbol or string to extract.

Return the property value if found, or symbol `macher-missing' if missing.
Side effects: None."
  (unless (or (hash-table-p obj)
              (and (listp obj) (listp (cdr-safe obj))))
    (setq obj nil))

  (let* ((str (cond
               ((symbolp key) (symbol-name key))
               ((stringp key) key)
               (t (format "%s" key))))
         (clean-str (if (string-prefix-p ":" str)
                        (substring str 1)
                      str))
         (hyphen-str (replace-regexp-in-string "_" "-" clean-str))
         (under-str (replace-regexp-in-string "-" "_" clean-str))
         (candidates (delete-dups
                      (list key
                            (intern (concat ":" under-str))
                            (intern (concat ":" hyphen-str))
                            (intern under-str)
                            (intern hyphen-str)
                            under-str
                            hyphen-str
                            clean-str
                            (intern (concat ":" clean-str))
                            (intern clean-str))))
         (result 'macher-missing))
    (when obj
      (cl-loop for k in candidates
               for val = (condition-case nil
                             (map-elt obj k 'macher-missing)
                           (error 'macher-missing))
               unless (eq val 'macher-missing)
               return (setq result val)))
    result))

(defun macher-agent-get-active-fsm (&optional current-fsm)
  "Resolve the active finite-state machine using CURRENT-FSM or environment variables."
  (or current-fsm
      (bound-and-true-p macher-agent--active-fsm)
      (bound-and-true-p gptel--fsm)
      (bound-and-true-p gptel--fsm-last)
      (bound-and-true-p macher--fsm-latest)))

(defun macher-agent--extract-fsm-info (fsm)
  "Extract gptel info plist safely from FSM struct, object, or plist."
  (cond
   ((null fsm) nil)
   ((and (fboundp 'gptel-fsm-p) (gptel-fsm-p fsm))
    (gptel-fsm-info fsm))
   ((and (recordp fsm) (fboundp 'gptel-fsm-info))
    (ignore-errors (gptel-fsm-info fsm)))
   (t
    (let ((ctx (macher-agent--extract-prop fsm :macher-agent-context))
          (buf (macher-agent--extract-prop fsm :buffer)))
      (when (or (not (eq ctx 'macher-missing)) (not (eq buf 'macher-missing)))
        (list :macher-agent-context (if (eq ctx 'macher-missing) nil ctx)
              :buffer (if (eq buf 'macher-missing) nil buf)))))))

(defun macher-agent-valid-context-p (context)
  "Determine whether CONTEXT is a valid context structure.

CONTEXT is the context object or structure to validate.

Return non-nil if CONTEXT is valid, or nil otherwise.

Side effects: None."
  (and context
       (or (macher-agent--context-struct-p context)
           (and (fboundp 'macher-context-p) (macher-context-p context))
           (and (recordp context)
                (memq (type-of context) '(macher-context
                                          cl-struct-macher-context
                                          macher-agent-context
                                          cl-struct-macher-agent-context
                                          macher-agent-task-context
                                          cl-struct-macher-agent-task-context)))
           (and (recordp context)
                (> (length context) 0)
                (memq (aref context 0) '(cl-struct-macher-context
                                         cl-struct-macher-agent-context
                                         cl-struct-macher-agent-task-context)))
           (and (vectorp context)
                (> (length context) 0)
                (memq (aref context 0) '(cl-struct-macher-context
                                         cl-struct-macher-agent-context
                                         cl-struct-macher-agent-task-context)))
           (macher-agent-task-context-p context))))

(defun macher-agent--get-context-raw-data (ctx)
  "Safely retrieve raw data from CTX.

CTX is the context structure.

Return the data slot value, or nil.
Side effects: None."
  (when (macher-agent-valid-context-p ctx)
    (cond
     ((and (fboundp 'macher-context-p) (macher-context-p ctx) (fboundp 'macher-context-data))
      (ignore-errors (macher-context-data ctx)))
     ((macher-agent--context-struct-p ctx)
      (ignore-errors (macher-agent-context-data ctx)))
     ((macher-agent-task-context-p ctx)
      (ignore-errors (macher-agent-task-context-data ctx)))
     ((fboundp 'macher-context-data)
      (ignore-errors (macher-context-data ctx)))
     (t nil))))

(defun macher-agent--set-context-raw-data (ctx val)
  "Safely set raw data on CTX to VAL.

CTX is the context structure.
VAL is the data value.

Return new value VAL, or nil.
Side effects: Mutates CTX structure."
  (when (macher-agent-valid-context-p ctx)
    (cond
     ((and (fboundp 'macher-context-p) (macher-context-p ctx) (fboundp 'macher-context-data))
      (ignore-errors (with-no-warnings (setf (macher-context-data ctx) val))))
     ((macher-agent--context-struct-p ctx)
      (ignore-errors (with-no-warnings (setf (macher-agent-context-data ctx) val))))
     ((macher-agent-task-context-p ctx)
      (ignore-errors (with-no-warnings (setf (macher-agent-task-context-data ctx) val))))
     ((fboundp 'macher-context-data)
      (ignore-errors (with-no-warnings (setf (macher-context-data ctx) val))))))
  val)

(defun macher-agent--get-context-data (ctx key &optional default)
  "Retrieve KEY from the native data slot of CTX.

CTX is the context structure.
KEY is the lookup key symbol.
DEFAULT is the value returned if KEY is not present.

Return the value or DEFAULT."
  (if (macher-agent-valid-context-p ctx)
      (let ((data (macher-agent--get-context-raw-data ctx)))
        (cond
         ((and (macher-agent--plist-p data) (plist-member data key))
          (plist-get data key))
         ((and (consp data) (consp (car data)))
          (if-let* ((cell (assq key data)))
              (cdr cell)
            default))
         ((hash-table-p data)
          (gethash key data default))
         (t default)))
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
      (if data
          (map-put! data key val)
        (setf (macher-context-data ctx) (list key val)))))
  val)

(defun macher-agent--get-context-prompt (ctx)
  "Safely access prompt on CTX, falling back to data slot `:prompt`.

CTX is the context structure.

Return prompt string, or nil.
Side effects: None."
  (when (macher-agent-valid-context-p ctx)
    (or (and (fboundp 'macher-context-prompt)
             (ignore-errors (macher-context-prompt ctx)))
        (ignore-errors (macher-agent-context-prompt ctx))
        (ignore-errors (macher-agent-task-context-prompt ctx))
        (and (or (recordp ctx) (vectorp ctx))
             (> (length ctx) 3)
             (eq (aref ctx 0) 'cl-struct-macher-context)
             (aref ctx 3))
        (and (or (recordp ctx) (vectorp ctx))
             (> (length ctx) 10)
             (eq (aref ctx 0) 'cl-struct-macher-agent-context)
             (aref ctx 10))
        (macher-agent--get-context-data ctx :prompt))))

(defun macher-agent--set-context-prompt (ctx val)
  "Safely set prompt on CTX, synchronising both direct slot and data slot `:prompt`.

Mutate direct prompt slot and `:data :prompt` on context CTX with VAL.

Return new value VAL, or nil.
Side effects: Mutates CTX structure."
  (when (macher-agent-valid-context-p ctx)
    (cond
     ((and (fboundp 'macher-context-p) (macher-context-p ctx))
      (if (fboundp 'macher-context-prompt)
          (condition-case nil
              (with-no-warnings (setf (macher-context-prompt ctx) val))
            (error (when (and (or (recordp ctx) (vectorp ctx)) (> (length ctx) 3))
                     (aset ctx 3 val))))
        (when (and (or (recordp ctx) (vectorp ctx)) (> (length ctx) 3))
          (aset ctx 3 val))))
     ((macher-agent--context-struct-p ctx)
      (condition-case nil
          (with-no-warnings (setf (macher-agent-context-prompt ctx) val))
        (error (when (and (or (recordp ctx) (vectorp ctx)) (> (length ctx) 10))
                 (aset ctx 10 val)))))
     ((macher-agent-task-context-p ctx)
      (ignore-errors (with-no-warnings (setf (macher-agent-task-context-prompt ctx) val))))
     ((and (or (recordp ctx) (vectorp ctx))
           (> (length ctx) 0)
           (eq (aref ctx 0) 'cl-struct-macher-context))
      (if (fboundp 'macher-context-prompt)
          (condition-case nil
              (with-no-warnings (setf (macher-context-prompt ctx) val))
            (error (when (> (length ctx) 3) (aset ctx 3 val))))
        (when (> (length ctx) 3)
          (aset ctx 3 val))))
     ((and (or (recordp ctx) (vectorp ctx))
           (> (length ctx) 0)
           (eq (aref ctx 0) 'cl-struct-macher-agent-context))
      (condition-case nil
          (with-no-warnings (setf (macher-agent-context-prompt ctx) val))
        (error (when (> (length ctx) 10) (aset ctx 10 val)))))
     (t
      (when (fboundp 'macher-context-prompt)
        (ignore-errors (with-no-warnings (setf (macher-context-prompt ctx) val))))
      (ignore-errors (with-no-warnings (setf (macher-agent-context-prompt ctx) val)))
      (ignore-errors (with-no-warnings (setf (macher-agent-task-context-prompt ctx) val)))
      (when (and (or (recordp ctx) (vectorp ctx))
                 (> (length ctx) 3)
                 (eq (aref ctx 0) 'cl-struct-macher-context))
        (aset ctx 3 val))
      (when (and (or (recordp ctx) (vectorp ctx))
                 (> (length ctx) 10)
                 (eq (aref ctx 0) 'cl-struct-macher-agent-context))
        (aset ctx 10 val))))
    (macher-agent--set-context-data ctx :prompt val))
  val)

(defun macher-agent-extract-workspace-id (input)
  "Extract workspace identifier or project root from INPUT."
  (cond
   ((stringp input) input)
   ((and (consp input) (eq (car input) 'project)) input)
   ((and (consp input) (eq (car input) 'agent)) input)
   ((and (recordp input) (macher-agent-workspace-p input)) input)
   (t
    (let* ((shared (macher-agent--extract-prop input :shared-state))
           (shared-obj (if (eq shared 'macher-missing) nil shared))
           (ws-id (macher-agent--extract-prop input :workspace-id))
           (ws (macher-agent--extract-prop input :workspace))
           (t-ws (macher-agent--extract-prop input :target-workspace))
           (proj (macher-agent--extract-prop input :project))
           (s-ws-id (if shared-obj (macher-agent--extract-prop shared-obj :workspace-id) 'macher-missing))
           (s-ws (if shared-obj (macher-agent--extract-prop shared-obj :workspace) 'macher-missing)))
      (cl-some (lambda (x) (unless (eq x 'macher-missing) x))
               (list ws-id ws t-ws proj s-ws-id s-ws))))))

(defun macher-agent--extract-fsm-context (fsm)
  "Extract active context from FSM."
  (cond
   ((null fsm) nil)
   ((macher-agent-valid-context-p fsm) fsm)
   (t (let* ((info (ignore-errors (macher-agent--extract-fsm-info fsm)))
             (fsm-ctx (when (or (listp info) (hash-table-p info))
                        (cl-some (lambda (k)
                                   (let ((v (macher-agent--extract-prop info k)))
                                     (when (and (not (eq v 'macher-missing))
                                                (macher-agent-valid-context-p v))
                                       v)))
                                 '(:context :macher-agent-context :macher--context :macher-context :target-context :parent-context)))))
        (or fsm-ctx
            (ignore-errors (macher-agent-resolve-context info))
            (macher-agent-resolve-from-transit-payload info))))))

(defun macher-agent--transform-inject-context (async-fn fsm)
  "Inject the originating buffer context into the FSM info list.
This function executes in the detached *gptel-prompt* buffer. It retrieves
the context from the originating buffer stored in the FSM."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let* ((info (gptel-fsm-info fsm))
           (origin-buf (when (macher-agent--plist-p info) (plist-get info :buffer))))
      (when (and origin-buf (buffer-live-p origin-buf))
        (let ((agent-ctx (buffer-local-value 'macher-agent--persistent-context origin-buf)))
          (when agent-ctx
            (let ((fsm-prompt (when (macher-agent--plist-p info) (plist-get info :prompt))))
              (when (and fsm-prompt (null (macher-agent--get-context-prompt agent-ctx)))
                (macher-agent--set-context-prompt agent-ctx fsm-prompt)))
            (when-let* ((ctx-prompt (macher-agent--get-context-prompt agent-ctx)))
              (macher-agent--set-context-prompt agent-ctx ctx-prompt))
            (if (macher-agent--plist-p info)
                (let ((cell (plist-member info :macher-agent-context)))
                  (if cell
                      (setcar (cdr cell) agent-ctx)
                    (nconc info (list :macher-agent-context agent-ctx))))))))))
  (when (functionp async-fn)
    (funcall async-fn)))

(defvar macher-agent-task-flush-hook nil
  "Hook run when an agent task flushes or completes its execution cycle.

Functions in this hook are invoked to commit interaction memory, synchronise
storage, and flush pending artifacts.")

(defvar macher-agent-vfs-flush-hook nil
  "Event hook triggered after the Virtual File System processes modifications.
Functions in this hook receive the active `macher-agent-context' struct.")

(defun macher-agent-run-task-flush-hook (&optional context)
  "Safely invoke the flush hook, handling legacy 0-arg and new 1-arg functions."
  (let ((ctx (or (when (macher-agent-valid-context-p context) context)
                 (bound-and-true-p macher-agent--persistent-context)
                 (ignore-errors (macher-agent-resolve-context)))))
    (dolist (hook-fn (if (consp macher-agent-task-flush-hook)
                         macher-agent-task-flush-hook
                       (list macher-agent-task-flush-hook)))
      (when (functionp hook-fn)
        (condition-case nil
            (if ctx (funcall hook-fn ctx) (funcall hook-fn nil))
          (wrong-number-of-arguments (ignore-errors (funcall hook-fn))))))))

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

(defconst macher-agent-transit-context-keys
  '(:target-context :parent-context :context)
  "Authoritative list of keys used to pass context structures in transit payloads.")

(defconst macher-agent-transit-buffer-keys
  '(:target-buffer :parent-buffer :buffer :buffer-name)
  "Authoritative list of keys used to pass buffer references in transit payloads.")

(defun macher-agent-resolve-from-transit-payload (payload-or-state)
  "Extract context from PAYLOAD-OR-STATE using transit keys."
  (cond
   ((macher-agent-valid-context-p payload-or-state)
    payload-or-state)

   ((and (macher-agent--plist-p payload-or-state)
         (plist-member payload-or-state :resolved)
         (plist-member payload-or-state :input))
    (if (plist-get payload-or-state :resolved)
        payload-or-state
      (let* ((input (plist-get payload-or-state :input))
             (ctx (when input (macher-agent-resolve-from-transit-payload input))))
        (if (and ctx (macher-agent-valid-context-p ctx))
            (plist-put payload-or-state :resolved ctx)
          payload-or-state))))

   ((or (listp payload-or-state) (hash-table-p payload-or-state))
    (or (cl-loop for key in macher-agent-transit-context-keys
                 for val = (macher-agent--extract-prop payload-or-state key)
                 when (and (not (eq val 'macher-missing))
                           (macher-agent-valid-context-p val))
                 return val)
        (let ((shared (macher-agent--extract-prop payload-or-state :shared-state)))
          (when (and (not (eq shared 'macher-missing))
                     (or (hash-table-p shared) (listp shared)))
            (cl-loop for key in macher-agent-transit-context-keys
                     for val = (macher-agent--extract-prop shared key)
                     when (and (not (eq val 'macher-missing))
                               (macher-agent-valid-context-p val))
                     return val)))
        (let* ((b-raw (cl-some (lambda (k)
                                 (let ((v (macher-agent--extract-prop payload-or-state k)))
                                   (unless (eq v 'macher-missing) v)))
                               macher-agent-transit-buffer-keys))
               (buf (when b-raw (if (bufferp b-raw) b-raw (and (stringp b-raw) (get-buffer b-raw))))))
          (when (and buf (buffer-live-p buf))
            (let ((bctx (buffer-local-value 'macher-agent--persistent-context buf)))
              (when (macher-agent-valid-context-p bctx) bctx))))))

   (t nil)))

(defun macher-agent-resolve-context (&optional input)
  "Resolve context from optional INPUT using context-resolution pipeline.

Passes state (:input INPUT :resolved nil) through the registered
pipeline steps for `context-resolution'.

INPUT is the optional input context, payload, FSM, or workspace identifier.

Return the resolved context structure, or nil.

Side effects: None."
  (let ((state (list :input input :resolved nil)))
    (setq state (seq-reduce (lambda (acc step)
                              (if (functionp step)
                                  (funcall step acc)
                                acc))
                            (macher-agent-get-pipeline-steps 'context-resolution)
                            state))
    (when (macher-agent--plist-p state)
      (plist-get state :resolved))))

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
NAME-OR-BUF can be a buffer object, a buffer name string, a file path string, a context struct, or nil.
If NAME-OR-BUF is nil, return the current buffer.
If CREATE is non-nil and the buffer does not exist, create it with `get-buffer-create'.

Return the buffer object, or nil if not found."
  (cond
   ((null name-or-buf) (current-buffer))
   ((bufferp name-or-buf)
    (if (buffer-live-p name-or-buf)
        name-or-buf
      (when create
        (get-buffer-create (buffer-name name-or-buf)))))
   ((macher-agent-valid-context-p name-or-buf)
    (let ((buf (cond
                ((macher-agent--context-struct-p name-or-buf)
                 (macher-agent-context-buffer name-or-buf))
                ((ignore-errors (macher-agent--get-context-data name-or-buf :buffer)))
                (t nil))))
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
  "Retrieve the project root directory string from CONTEXT polymorphically.

CONTEXT is the active context structure or workspace.

Return the project root path string."
  (or (cond
       ((null context) nil)
       ((stringp context) (file-truename (expand-file-name context)))
       ((and (consp context) (memq (car context) '(project agent directory)))
        (let ((path (cdr context)))
          (when (stringp path) (file-truename (expand-file-name path)))))
       ((macher-agent-valid-context-p context)
        (or (when (macher-agent--context-struct-p context)
              (let ((r (macher-agent-context-project-root context)))
                (when (and r (stringp r))
                  (file-truename (expand-file-name r)))))
            (when-let* ((ws (when (fboundp 'macher-agent--get-context-workspace)
                              (macher-agent--get-context-workspace context))))
              (let ((proj-root (if (consp ws)
                                   (cdr ws)
                                 (ignore-errors (macher--workspace-root ws)))))
                (when (stringp proj-root) (file-truename (expand-file-name proj-root)))))))
       (t nil))
      (file-truename default-directory)))

(defun macher-agent--get-root (workspace)
  "Get the project root path of WORKSPACE.
This satisfies the upstream `macher-workspace-types-alist' interface."
  (macher-agent-workspace-project-root workspace))

(defun macher-agent--get-name (workspace)
  "Get a display name for WORKSPACE.
This satisfies the upstream `macher-workspace-types-alist' interface."
  (let ((root (macher-agent-workspace-project-root workspace)))
    (if root
        (concat "Agent: " (file-name-nondirectory (directory-file-name root)))
      "Agent: Workspace")))

(defun macher-agent-workspace-project-root (ws)
  "Retrieve the project root directory string from workspace WS."
  (cond
   ((null ws) nil)
   ((stringp ws) (file-truename (expand-file-name ws)))
   ((and (consp ws) (memq (car ws) '(project agent directory)))
    (let ((path (if (stringp (cdr ws)) (expand-file-name (cdr ws)) (cdr ws))))
      (if (stringp path) (file-truename path) path)))
   ((and (fboundp 'project-root) (ignore-errors (project-root ws)))
    (file-truename (expand-file-name (project-root ws))))
   (t (when (consp ws)
        (let ((path (cdr ws)))
          (if (stringp path) (file-truename (expand-file-name path)) path))))))

(provide 'macher-agent-core)
;;; macher-agent-core.el ends here
