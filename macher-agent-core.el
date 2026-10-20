;;; macher-agent-core.el --- Core state for Macher Agent -*- lexical-binding: t; -*-

;;; Commentary:

;; Core state variables, session structures, and pipeline registries for Macher Agent.

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

(defvar gptel-directives)

(cl-defstruct (macher-agent-session
               (:constructor make-macher-agent-session)
               (:copier copy-macher-agent-session))
  "Execution session state for an agent buffer."
  (owner-buffer nil :type (or null buffer))
  (parent-buffer nil :type (or null buffer))
  (routing-stack nil :type list)
  (message-queue nil :type list)
  (lifecycle :persistent :type symbol)
  (state :idle :type symbol))

(cl-defstruct (macher-agent-context
               (:constructor macher-agent--make-context)
               (:copier macher-agent--copy-context))
  "Primary execution context and memory bus for macher-agent."
  (id nil :type (or null string))
  (project-root nil :type (or null string))
  (origin-buffer nil :type (or null buffer))
  (prompt nil :type (or null string))
  (tools nil :type (or null list hash-table))
  (skills nil :type list)
  (media-queue nil :type (or null list string))
  (subagents nil :type list)
  (plugins nil :type list))

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

(cl-defstruct (macher-agent-task-context
               (:constructor make-macher-agent-task-context)
               (:copier copy-macher-agent-task-context))
  "Represent a task execution context structure."
  workspace
  target-buffer
  skill-sym
  system-message)

(defconst macher-agent--file-scan-regex "^[^.]"
  "Regular expression matching non-hidden file and directory names.")

(defvar directory-files-no-dot-files-re macher-agent--file-scan-regex
  "Safe compatibility alias for non-dot file directory searches.")

(defvar macher-agent--active-ptc-execution nil
  "Indicate whether a Programmatic Tool Calling Lisp script is active.")

(defvar macher-agent-allowed-tools nil
  "Hold custom tool names that receive the active Macher context.")

(defvar macher-agent-active-workspaces (make-hash-table :test 'equal)
  "Registry mapping expanded project roots to their active persistent contexts.")

(defvar macher-agent-active-subagents nil
  "Store active sub-agents and their locked directories as an alist.")

(defvar macher-agent-active-fsm-function nil
  "Function called with optional FSM to return the active FSM instance.")

(defvar macher-agent-fsm-info-function nil
  "Function called with FSM to extract its info plist.")

(defvar macher-agent-workspace-read-file-function nil
  "Function called with (CONTEXT PATH &optional ONLY-STAGED) to read a file.")

(defvar macher-agent-workspace-list-files-function nil
  "Function called with (CONTEXT) returning paths tracked in workspace storage.")

(defvar macher-agent-context-mutated-hook nil
  "Hook run when a context is mutated.")

(defvar macher-agent-context-sync-hook nil
  "Hook run with CONTEXT to synchronise workspace state prior to transmission.")

(defvar macher-agent-peer-sync-hook nil
  "Hook run with (SOURCE-CTX TARGET-CTX) during peer-to-peer message dispatch.")

(defvar macher-agent-actor-reap-hook nil
  "Hook run with ACTOR-IDS list when a sub-agent buffer is reaped.")

(defvar macher-agent-buffer-flush-hook nil
  "Hook run with TARGET-BUF when an agent turn finishes and flushes.")

(defvar macher-agent--allow-lazy-init nil
  "When non-nil, allow lazy initialisation of workspace context.")

(defvar macher-agent-tools-registry (make-hash-table :test 'equal)
  "Store global hash table for all loaded agent tools.")

(defvar macher-agent-global-skills-alist nil
  "Store global association list for all loaded agent skills metadata.")

(defvar macher-agent-search-backend-function #'macher-agent-search-glob
  "Function variable used to dispatch conversation history searches.")

(defcustom macher-agent-max-context-chars '((nil . 2000000))
  "Alist mapping gptel model symbols to their maximum allowed context characters."
  :type '(alist :key-type symbol :value-type integer)
  :group 'macher-agent)

(defcustom macher-agent-max-tool-output-chars 30000
  "Maximum character length for sandbox tool output before middle truncation."
  :type 'integer
  :group 'macher-agent)

(defun macher-agent--ensure-trailing-slash (path)
  "Ensure directory string PATH ends with a trailing slash."
  (file-name-as-directory path))

(defun macher-agent--truncate-string (output &optional max-chars)
  "Truncate OUTPUT when it exceeds MAX-CHARS, preserving head and tail halves."
  (let* ((limit (or max-chars macher-agent-max-tool-output-chars 30000))
         (total (length output)))
    (if (or (<= limit 0) (<= total limit))
        output
      (let* ((half (/ limit 2))
             (omitted (- total limit)))
        (concat (substring output 0 half)
                (format "\n... [truncated %d chars] ...\n" omitted)
                (substring output (- total half)))))))

;;; Dedicated Callback Registries

(defvar macher-agent--pending-callbacks (make-hash-table :test 'equal)
  "Unified callback registry synchronised with task, wake, and VFS tables.")

(defvar macher-agent--task-callbacks (make-hash-table :test 'equal)
  "Registry mapping task-id strings to parent completion callbacks.")

(defvar macher-agent--wake-callbacks (make-hash-table :test 'equal)
  "Registry mapping suspended sub-agent buffer names to wake callbacks.")

(defvar macher-agent--vfs-callbacks (make-hash-table :test 'equal)
  "Registry mapping VFS file paths to lock or mutation callbacks.")

(defun macher-agent-register-task-callback (task-id cb)
  "Register task completion callback CB for TASK-ID."
  (puthash task-id cb macher-agent--task-callbacks)
  (puthash task-id cb macher-agent--pending-callbacks))

(defun macher-agent-get-task-callback (task-id)
  "Lookup task completion callback for TASK-ID."
  (or (gethash task-id macher-agent--task-callbacks)
      (gethash task-id macher-agent--pending-callbacks)))

(defun macher-agent-pop-task-callback (task-id)
  "Remove and return task completion callback for TASK-ID."
  (let ((cb (macher-agent-get-task-callback task-id)))
    (remhash task-id macher-agent--task-callbacks)
    (remhash task-id macher-agent--pending-callbacks)
    cb))

(defun macher-agent-register-wake-callback (buf-name cb)
  "Register wake callback CB for suspended buffer BUF-NAME."
  (puthash buf-name cb macher-agent--wake-callbacks)
  (puthash buf-name cb macher-agent--pending-callbacks))

(defun macher-agent-get-wake-callback (buf-name)
  "Lookup wake callback for suspended buffer BUF-NAME."
  (or (gethash buf-name macher-agent--wake-callbacks)
      (gethash buf-name macher-agent--pending-callbacks)))

(defun macher-agent-pop-wake-callback (buf-name)
  "Remove and return wake callback for suspended buffer BUF-NAME."
  (let ((cb (macher-agent-get-wake-callback buf-name)))
    (remhash buf-name macher-agent--wake-callbacks)
    (remhash buf-name macher-agent--pending-callbacks)
    cb))

;;; Buffer-Local Variables

(defvar-local macher-agent--active-fsm nil
  "Store active finite-state machine (FSM) instance for current buffer.")

(defvar-local macher-agent-fsm-id nil
  "Buffer-local identifier bridging user interface buffers to the state machine.")

(defvar-local macher-agent--routing-stack nil
  "Stack of routing frame property lists for current sub-agent buffer.")

(defvar-local macher-agent--message-queue nil
  "Buffer-local FIFO queue of incoming peer-to-peer messages.")

(defvar-local macher-agent--active-skill-sym nil
  "Store symbol representing the currently active skill preset in buffer.")

(defvar-local macher-agent--active-ptc-primitives nil
  "Store active Programmatic Tool Calling primitives for current buffer.")

(defvar-local macher-agent--persistent-context nil
  "Store the buffer-local persistent context structure.")

(defvar-local macher-agent--current-task-id nil
  "Store active awaited task ID for current sub-agent execution.")

(defvar-local macher-agent--is-ephemeral nil
  "Flag whether the current buffer is an ephemeral sub-agent.")

(defvar-local macher-agent--ready-to-reap nil
  "Flag whether the sub-agent buffer is ready for garbage collection.")

(defvar-local macher-agent-task-finished nil
  "Flag whether the current sub-agent task has finished execution.")

(defvar-local macher-agent--task-result nil
  "Store the task result string or payload for the current agent execution.")

(defvar-local macher-agent-presets nil
  "Store list of active preset or skill symbols for current buffer.")

(defvar-local macher-agent--cached-presets nil
  "Store cached presets for current buffer prior to skill switching.")

(defvar-local macher-agent--suppress-patch nil
  "Flag to suppress interactive UI patch buffer generation.")

(defvar-local macher-agent--boot-directive nil
  "Initial boot directive instruction string for sub-agent execution.")

(put 'macher-agent--active-fsm 'permanent-local t)
(put 'macher-agent--current-task-id 'permanent-local t)
(put 'macher-agent--is-ephemeral 'permanent-local t)
(put 'macher-agent--ready-to-reap 'permanent-local t)
(put 'macher-agent-task-finished 'permanent-local t)
(put 'macher-agent--task-result 'permanent-local t)
(put 'macher-agent-presets 'permanent-local t)
(put 'macher-agent--cached-presets 'permanent-local t)
(put 'macher-agent--routing-stack 'permanent-local t)
(put 'macher-agent--message-queue 'permanent-local t)
(put 'macher-agent--active-ptc-primitives 'permanent-local t)
(put 'macher-agent--suppress-patch 'permanent-local t)
(put 'macher-agent--boot-directive 'permanent-local t)
(put 'macher-agent--persistent-context 'permanent-local t)
(put 'macher-agent-fsm-id 'permanent-local t)

(defvar macher-agent--a2a-ownership (make-hash-table :test 'equal)
  "Global ownership registry tracking sub-agents associated with an originator.")

(defcustom macher-agent-display-subagent-fn nil
  "Specify function to display a sub-agent buffer during execution."
  :type '(choice (const :tag "Silent Background Execution" nil)
                 function)
  :group 'macher-agent)

;;; Session and Context Accessors

(defun macher-agent--plist-p (object)
  "Return t if OBJECT is a strictly formatted property list."
  (and (listp object) (plistp object)))

(defun macher-agent-valid-context-p (context)
  "Return t if CONTEXT is a strictly valid context structure."
  (macher-agent-context-p context))

(defun macher-agent-context-session (ctx)
  "Return the `macher-agent-session' struct for CTX, initialising it if absent."
  (cl-check-type ctx macher-agent-context)
  (let* ((plugins (macher-agent-context-plugins ctx))
         (session (when (macher-agent--plist-p plugins)
                    (plist-get plugins :session))))
    (if (macher-agent-session-p session)
        session
      (let ((new-session (make-macher-agent-session
                          :owner-buffer (macher-agent-context-origin-buffer ctx))))
        (setf (macher-agent-context-plugins ctx)
              (plist-put (copy-sequence plugins) :session new-session))
        new-session))))

(defun macher-agent-context-owner-buffer (ctx)
  "Return the buffer that owns and executes CTX."
  (cl-check-type ctx macher-agent-context)
  (or (macher-agent-session-owner-buffer (macher-agent-context-session ctx))
      (macher-agent-context-origin-buffer ctx)))

(defun macher-agent--set-context-owner-buffer (ctx buf)
  "Set the owner buffer of CTX to BUF."
  (cl-check-type ctx macher-agent-context)
  (setf (macher-agent-session-owner-buffer (macher-agent-context-session ctx)) buf)
  (unless (macher-agent-context-origin-buffer ctx)
    (setf (macher-agent-context-origin-buffer ctx) buf))
  buf)

(gv-define-setter macher-agent-context-owner-buffer (val ctx)
  `(macher-agent--set-context-owner-buffer ,ctx ,val))

(defun macher-agent-context-parent-buffer (ctx)
  "Return the parent orchestrator buffer that spawned CTX, or nil."
  (cl-check-type ctx macher-agent-context)
  (macher-agent-session-parent-buffer (macher-agent-context-session ctx)))

(defun macher-agent--set-context-parent-buffer (ctx buf)
  "Set the parent orchestrator buffer of CTX to BUF."
  (cl-check-type ctx macher-agent-context)
  (setf (macher-agent-session-parent-buffer (macher-agent-context-session ctx)) buf)
  (setf (macher-agent-context-origin-buffer ctx) buf)
  buf)

(gv-define-setter macher-agent-context-parent-buffer (val ctx)
  `(macher-agent--set-context-parent-buffer ,ctx ,val))

(defun macher-agent-context-from-buffer (buffer-or-name)
  "Extract the context object strictly from a live buffer or string name."
  (let ((buf (if (bufferp buffer-or-name) buffer-or-name (and (stringp buffer-or-name) (get-buffer buffer-or-name)))))
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
                 (plist-get shared :context))))
      (when-let* ((b (macher-agent-transit-payload-target-buffer payload)))
        (macher-agent-context-from-buffer b))))

(defun macher-agent-resolve-context (&optional source)
  "Resolve SOURCE into a validated `macher-agent-context' struct."
  (cond
   ((macher-agent-context-p source) source)
   ((macher-agent-transit-payload-p source)
    (macher-agent-context-from-payload source))
   ((and source (recordp source) (functionp macher-agent-fsm-info-function))
    (let* ((info (ignore-errors (funcall macher-agent-fsm-info-function source)))
           (ctx (when (macher-agent--plist-p info)
                  (or (plist-get info :macher-agent-context)
                      (plist-get info :context)))))
      (if (macher-agent-context-p ctx)
          ctx
        (when-let* ((buf (when (macher-agent--plist-p info)
                           (or (plist-get info :origin-buffer)
                               (plist-get info :buffer)))))
          (macher-agent-context-from-buffer buf)))))
   ((bufferp source)
    (macher-agent-context-from-buffer source))
   ((stringp source)
    (or (macher-agent-context-from-buffer source)
        (when (hash-table-p macher-agent-active-workspaces)
          (let ((exp (expand-file-name source)))
            (or (gethash exp macher-agent-active-workspaces)
                (gethash (file-name-as-directory exp) macher-agent-active-workspaces)
                (gethash (directory-file-name exp) macher-agent-active-workspaces))))))
   ((macher-agent--plist-p source)
    (let ((ctx (or (plist-get source :target-context)
                   (plist-get source :parent-context)
                   (plist-get source :macher-agent-context)
                   (plist-get source :context)
                   (plist-get source :child-context))))
      (if (macher-agent-context-p ctx)
          ctx
        (when-let* ((b (or (plist-get source :target-buffer)
                           (plist-get source :parent-buffer)
                           (plist-get source :buffer))))
          (macher-agent-context-from-buffer b)))))
   (t
    (or (when-let* ((fsm (and (functionp macher-agent-active-fsm-function)
                              (funcall macher-agent-active-fsm-function))))
          (when (and (recordp fsm) (not (macher-agent-context-p fsm)))
            (macher-agent-resolve-context fsm)))
        (when (bound-and-true-p macher-agent--persistent-context)
          (and (macher-agent-context-p macher-agent--persistent-context)
               macher-agent--persistent-context))
        (macher-agent-context-from-buffer (current-buffer))
        (when (and default-directory (hash-table-p macher-agent-active-workspaces))
          (let ((root (ignore-errors (macher-agent-root default-directory))))
            (when root
              (or (gethash (expand-file-name root) macher-agent-active-workspaces)
                  (gethash (file-name-as-directory (expand-file-name root)) macher-agent-active-workspaces)
                  (gethash (directory-file-name (expand-file-name root)) macher-agent-active-workspaces)))))))))

(defun macher-agent-get-cached-presets (&optional buffer)
  "Return cached presets for BUFFER, defaulting to `current-buffer'."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (buffer-local-value 'macher-agent--cached-presets buf))))

(defun macher-agent-clear-cached-presets (&optional buffer)
  "Clear cached presets in BUFFER, defaulting to `current-buffer'."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (kill-local-variable 'macher-agent--cached-presets))))
  nil)

(defun macher-agent-ready-to-reap-p (&optional buffer)
  "Check whether BUFFER is ready to be reaped."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (or (bound-and-true-p macher-agent--ready-to-reap)
            (when-let* ((ctx (bound-and-true-p macher-agent--persistent-context))
                        ((macher-agent-context-p ctx)))
              (eq (macher-agent-session-state (macher-agent-context-session ctx)) :reapable)))))))

(defun macher-agent--remove-active-subagent-registries (child-buf &optional fallback-buf)
  "Clear registries strictly tied to CHILD-BUF object."
  (let ((buf (if (bufferp child-buf)
                 child-buf
               (if (bufferp fallback-buf)
                   fallback-buf
                 (and (stringp child-buf) (get-buffer child-buf))))))
    (cl-check-type buf buffer)
    (let ((child-name (buffer-name buf)))
      (when (boundp 'macher-agent-active-subagents)
        (setq macher-agent-active-subagents
              (cl-delete-if (lambda (entry)
                              (equal (if (consp entry) (car entry) entry) child-name))
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
        (when (macher-agent-context-p ctx)
          (setf (macher-agent-context-subagents ctx)
                (cl-delete-if (lambda (entry)
                                (equal (if (consp entry) (car entry) entry) child-name))
                              (macher-agent-context-subagents ctx))))))))

(defun macher-agent-ui-show (buf)
  "Display the user interface for buffer BUF."
  (when macher-agent-display-subagent-fn
    (funcall macher-agent-display-subagent-fn buf)))

(defun macher-agent--push-routing (task-id originator-name &optional suppress-patch)
  "Push a routing context frame onto stack and update session state.
Records TASK-ID, ORIGINATOR-NAME, and optional SUPPRESS-PATCH."
  (let ((frame (list :task-id task-id
                     :originator-name originator-name
                     :suppress-patch suppress-patch)))
    (unless (boundp 'macher-agent--routing-stack)
      (setq-local macher-agent--routing-stack nil))
    (push frame macher-agent--routing-stack)
    (setq-local macher-agent--current-task-id task-id)
    (setq-local macher-agent--suppress-patch suppress-patch)
    (when-let* ((ctx (bound-and-true-p macher-agent--persistent-context))
                ((macher-agent-context-p ctx)))
      (setf (macher-agent-session-routing-stack (macher-agent-context-session ctx))
            macher-agent--routing-stack))
    frame))

(defun macher-agent--pop-routing ()
  "Pop a routing context frame from stack and update session state."
  (when (bound-and-true-p macher-agent--routing-stack)
    (let ((frame (pop macher-agent--routing-stack)))
      (if-let* ((prev (and (bound-and-true-p macher-agent--routing-stack)
                           (car macher-agent--routing-stack))))
          (progn
            (setq-local macher-agent--current-task-id (plist-get prev :task-id))
            (setq-local macher-agent--suppress-patch (plist-get prev :suppress-patch)))
        (setq-local macher-agent--current-task-id nil))
      (when-let* ((ctx (bound-and-true-p macher-agent--persistent-context))
                  ((macher-agent-context-p ctx)))
        (setf (macher-agent-session-routing-stack (macher-agent-context-session ctx))
              macher-agent--routing-stack))
      frame)))

(defun set-macher-agent-context-prompt (ctx val)
  "Set prompt on context CTX to VAL."
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

(defun macher-agent--get-context-root (context)
  "Extract root explicitly from a CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (macher-agent-context-project-root context))

(defvar macher-agent-task-flush-hook nil
  "Hook run when an agent task flushes or completes its execution cycle.")

(defun macher-agent-run-task-flush-hook (context)
  "Invoke the flush hook."
  (dolist (hook-fn (if (consp macher-agent-task-flush-hook)
                       macher-agent-task-flush-hook
                     (list macher-agent-task-flush-hook)))
    (when (functionp hook-fn)
      (funcall hook-fn context))))

(defvar macher-agent-pipeline-registry (make-hash-table :test 'equal)
  "Registry storing pipeline definitions and ordered steps.")

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

(defun macher-agent-root (&optional path)
  "Return the canonical project root for a specific string PATH."
  (let ((target-path (or path default-directory)))
    (cl-check-type target-path string)
    (let ((proj (and (fboundp 'project-current) (project-current nil target-path))))
      (expand-file-name
       (or (and proj (if (fboundp 'project-root) (project-root proj) (cdr proj)))
           (and (fboundp 'vc-root-dir) (let ((default-directory target-path)) (vc-root-dir)))
           target-path)))))

(defun macher-tool-valid-p (tool)
  "Validate whether TOOL is a valid gptel tool structure."
  (and tool (fboundp 'gptel-tool-p) (gptel-tool-p tool)))

(defun macher-agent--extract-raw-tool-name (tool)
  "Extract raw name representation from TOOL structure or list."
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
  "Extract and coerce TOOL into a canonical string name."
  (when tool
    (let ((raw-name (macher-agent--extract-raw-tool-name tool)))
      (format "%s" (if (symbolp raw-name) (symbol-name raw-name) raw-name)))))

(defun macher-normalise-preset-name (preset)
  "Convert PRESET to a uniform symbol without leading character symbols."
  (when preset
    (let ((str (cond ((stringp preset) preset)
                     ((symbolp preset) (symbol-name preset))
                     ((and (listp preset) (car preset))
                      (if (symbolp (car preset))
                          (symbol-name (car preset))
                        (format "%s" (car preset))))
                     (t (format "%s" preset)))))
      (intern (replace-regexp-in-string "^@+" "" str)))))

(defun macher-agent--get-name (workspace)
  "Get a display name for WORKSPACE."
  (let* ((root (when (macher-agent-context-p workspace)
                 (macher-agent-context-project-root workspace)))
         (name (when (and root (stringp root) (not (string-empty-p root)))
                 (file-name-nondirectory (directory-file-name root)))))
    (if (and name (not (string-empty-p name)))
        (concat "Agent: " name)
      "Agent: Workspace")))

(defun macher-agent-search-glob (orig-buf query &optional ctx-lines)
  "Execute search QUERY against live ORIG-BUF."
  (cl-check-type orig-buf buffer)
  (cl-check-type query string)
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

(defun macher-agent-log-tool-intent (context tool args target &optional extra)
  "Log TOOL intent with ARGS, TARGET, and optional EXTRA into CONTEXT audit log."
  (cl-check-type context macher-agent-context)
  (let* ((plugins (macher-agent-context-plugins context))
         (audit (plist-get plugins :audit-log))
         (entry (list :timestamp (current-time-string)
                      :tool tool
                      :args args
                      :target target
                      :extra extra)))
    (setf (macher-agent-context-plugins context)
          (plist-put (copy-sequence plugins) :audit-log (cons entry audit))))
  args)

(defun macher-agent--register-active-workspace-root (root context)
  "Map active ROOT string to CONTEXT struct in global registry."
  (cl-check-type root string)
  (cl-check-type context macher-agent-context)
  (when (and (boundp 'macher-agent-active-workspaces)
             (hash-table-p macher-agent-active-workspaces))
    (let ((expanded (expand-file-name root)))
      (puthash expanded context macher-agent-active-workspaces)
      (puthash (file-name-as-directory expanded) context macher-agent-active-workspaces)
      (puthash (directory-file-name expanded) context macher-agent-active-workspaces)))
  context)

(cl-defun make-macher-agent-workspace (&key project-root &allow-other-keys)
  "Construct a standard workspace cons cell `(project . PROJECT-ROOT)'."
  (cons 'project (and project-root (expand-file-name project-root))))

(defun copy-macher-agent-workspace (ws)
  "Create a copy of workspace WS."
  (if (consp ws) (copy-tree ws) ws))

(defun macher-agent-context-lookup (ws-or-id)
  "Retrieve active context mapped to WS-OR-ID string."
  (cl-check-type ws-or-id string)
  (or (macher-agent-resolve-context ws-or-id)
      (when (and (boundp 'macher-agent-active-workspaces)
                 (hash-table-p macher-agent-active-workspaces))
        (let ((exp (expand-file-name ws-or-id))
              (true-exp (file-truename (expand-file-name ws-or-id))))
          (or (gethash exp macher-agent-active-workspaces)
              (gethash (file-name-as-directory exp) macher-agent-active-workspaces)
              (gethash (directory-file-name exp) macher-agent-active-workspaces)
              (gethash true-exp macher-agent-active-workspaces)
              (gethash (file-name-as-directory true-exp) macher-agent-active-workspaces)
              (gethash (directory-file-name true-exp) macher-agent-active-workspaces))))))

(defun macher-agent--workspace-get-hash-table (ctx table-key)
  "Extract TABLE-KEY hash table from CTX struct, initialising it if absent."
  (cl-check-type ctx macher-agent-context)
  (let ((plugins (macher-agent-context-plugins ctx)))
    (or (when (macher-agent--plist-p plugins)
          (plist-get plugins table-key))
        (let ((ht (make-hash-table :test 'equal)))
          (setf (macher-agent-context-plugins ctx)
                (plist-put (copy-sequence plugins) table-key ht))
          ht))))

(defun macher-agent-workspace-tools-registry (ctx)
  "Extract combined tools registry for CTX."
  (cl-check-type ctx macher-agent-context)
  (let* ((plugins (macher-agent-context-plugins ctx))
         (local-table (or (macher-agent-context-tools ctx)
                          (when (macher-agent--plist-p plugins)
                            (plist-get plugins :tools))))
         (persistent-local
          (if (hash-table-p local-table)
              local-table
            (let ((ht (make-hash-table :test 'equal)))
              (setf (macher-agent-context-tools ctx) ht)
              ht)))
         (global-table macher-agent-tools-registry)
         (merged-table (make-hash-table :test 'equal)))
    (when (hash-table-p global-table)
      (maphash (lambda (k v) (puthash k v merged-table)) global-table))
    (when (hash-table-p persistent-local)
      (maphash (lambda (k v) (puthash k v merged-table)) persistent-local))
    merged-table))

(defun macher-agent--set-workspace-tools-registry (ctx registry)
  "Update tools REGISTRY mapped to CTX struct."
  (cl-check-type registry hash-table)
  (if ctx
      (progn
        (cl-check-type ctx macher-agent-context)
        (setf (macher-agent-context-tools ctx) registry))
    (setq macher-agent-tools-registry registry))
  registry)

(gv-define-setter macher-agent-workspace-tools-registry (val ws-or-ctx)
  `(macher-agent--set-workspace-tools-registry ,ws-or-ctx ,val))

(defun macher-agent-workspace-skills-alist (ctx)
  "Extract skills alist for CTX."
  (cl-check-type ctx macher-agent-context)
  (let ((local-skills (or (macher-agent-context-skills ctx)
                          (let ((plugins (macher-agent-context-plugins ctx)))
                            (when (macher-agent--plist-p plugins)
                              (plist-get plugins :skills)))))
        (global-skills (bound-and-true-p macher-agent-global-skills-alist)))
    (cl-remove-duplicates (append local-skills global-skills)
                          :key #'car :test #'eq)))

(defun macher-agent--set-workspace-skills-alist (ctx alist)
  "Update skills ALIST mapped to CTX struct."
  (cl-check-type alist list)
  (if ctx
      (progn
        (cl-check-type ctx macher-agent-context)
        (setf (macher-agent-context-skills ctx) alist))
    (setq macher-agent-global-skills-alist alist))
  alist)

(gv-define-setter macher-agent-workspace-skills-alist (val ws-or-ctx)
  `(macher-agent--set-workspace-skills-alist ,ws-or-ctx ,val))

(defun macher-agent-workspace-active-subagents (ctx)
  "Extract active subagents list directly via CTX struct."
  (cl-check-type ctx macher-agent-context)
  (or (macher-agent-context-subagents ctx)
      (let ((plugins (macher-agent-context-plugins ctx)))
        (when (macher-agent--plist-p plugins)
          (plist-get plugins :active-subagents)))))

(defun macher-agent--set-workspace-active-subagents (ctx subagents)
  "Update SUBAGENTS list mapped to CTX struct."
  (cl-check-type subagents list)
  (when ctx
    (cl-check-type ctx macher-agent-context)
    (setf (macher-agent-context-subagents ctx) subagents))
  subagents)

(gv-define-setter macher-agent-workspace-active-subagents (val ws-or-ctx)
  `(macher-agent--set-workspace-active-subagents ,ws-or-ctx ,val))

(defun macher-agent--copy-context-hash-tables (data)
  "Copy hash tables and session struct in DATA plist for context cloning."
  (when data
    (let ((res (copy-sequence data)))
      (cl-loop for (k v) on res by #'cddr
               do (cond
                   ((hash-table-p v)
                    (setq res (plist-put res k (copy-hash-table v))))
                   ((macher-agent-session-p v)
                    (setq res (plist-put res k (copy-macher-agent-session v))))))
      res)))

(defun macher-agent--clone-context (ctx)
  "Deep clone CTX struct and run the `context-clone' pipeline."
  (cl-check-type ctx macher-agent-context)
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
    (macher-agent-run-pipeline 'context-clone new-ctx)))

(defun macher-agent--inject-context-state (context &optional directives)
  "Inject active CONTEXT and optional DIRECTIVES into buffer."
  (when context
    (setq-local macher-agent--persistent-context context)
    (when directives
      (if (boundp 'gptel-directives)
          (setq gptel-directives directives)
        (setq-local gptel-directives directives)))))

(provide 'macher-agent-core)
;;; macher-agent-core.el ends here
