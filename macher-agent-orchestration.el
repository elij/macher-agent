;;; macher-agent-orchestration.el --- Orchestration -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive orchestration commands for Macher Agent.  This file provides
;; functions to coordinate and execute tasks.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'generator)
(require 'macher-agent-core)
(require 'macher-agent-presets)
(require 'macher-agent-gptel)

(defvar macher-agent-active-subagents nil
  "Store active sub-agents and their locked directories as an alist.

Each entry is a cell mapping sub-agent buffer instances or identifiers
to their designated sandbox directory paths.

Return an association list mapping sub-agent buffers to sandbox paths, or nil.

Side effects: Global variable storing active sub-agent mappings.")

(defvar macher-agent-submit-task-result-tool nil
  "Store the tool object for task result submission.

Holds the gptel tool object that sub-agents call to submit their final result.

Return the submit task result tool object, or nil.

Side effects: None.")

(defvar macher-agent--task-registry (make-hash-table :test 'equal)
  "Global registry mapping Task IDs to their originating buffer names.
Used to route completed artifacts back to the correct agent's inbox.")

(defun macher-agent--generate-uuid ()
  "Generate a unique identifier string."
  (let ((id (if (fboundp 'org-id-uuid)
                (org-id-uuid)
              (format "%04x%04x-%04x-%04x"
                      (random #xffff) (random #xffff) (random #xffff) (random #xffff)))))
    (if (string-prefix-p "task-" id)
        id
      (format "task-%s" id))))

(defvar macher-agent-a2a-pipeline-functions
  '(macher-agent-a2a-pipe--validate-routing
    macher-agent-a2a-pipe--acquire-target
    macher-agent-a2a-pipe--register-ownership
    macher-agent-a2a-pipe--route-instructions
    macher-agent-a2a-pipe--bind-closure
    macher-agent-a2a-pipe--transmit)
  "Chained reducer pipeline to construct and execute an A2A sub-agent.")

(defun macher-agent-a2a-dispatch (send-message-payloads final-callback &optional parent-ctx-override)
  "Dispatch A2A SEND-MESSAGE-PAYLOADS using a reducer pipeline.

Normalises incoming SEND-MESSAGE-PAYLOADS ensuring each message possesses a valid
task identifier.  Passes each payload through `macher-agent-a2a-pipeline-functions'
and calls FINAL-CALLBACK upon completion.

SEND-MESSAGE-PAYLOADS is a list of outgoing message property lists.
FINAL-CALLBACK is the function invoked with the vector of results.
PARENT-CTX-OVERRIDE is an optional context to use as the parent context.

Return nil.

Side effects: Spawns or signals sub-agent buffers and updates registries."
  (let* ((total (length send-message-payloads))
         (actual-parent-fsm (macher-agent-get-active-fsm))
         (actual-parent-buf (or (when actual-parent-fsm
                                  (ignore-errors (plist-get (gptel-fsm-info actual-parent-fsm) :buffer)))
                                (current-buffer)))
         (parent-ctx (or parent-ctx-override
                         (when (and actual-parent-buf (buffer-live-p actual-parent-buf))
                           (buffer-local-value 'macher-agent--persistent-context actual-parent-buf))
                         (ignore-errors (macher-agent-resolve-context actual-parent-buf))))
         (normalized-payloads
          (mapcar (lambda (msg)
                    (let* ((msg-type (plist-get msg :type))
                           (requested-id (plist-get msg :task-id))
                           (tid (cond
                                 ((eq msg-type 'ARTIFACT_UPDATE)
                                  (or requested-id
                                      (let ((gen (macher-agent--generate-uuid)))
                                        (if (string-prefix-p "task-" gen)
                                            gen
                                          (format "task-%s" gen)))))
                                 ((or (null requested-id)
                                      (and (stringp requested-id) (string-empty-p requested-id))
                                      (gethash requested-id macher-agent--pending-callbacks))
                                  (let ((gen (macher-agent--generate-uuid)))
                                    (if (string-prefix-p "task-" gen)
                                        gen
                                      (format "task-%s" gen))))
                                 (t requested-id)))
                           (meta (plist-get msg :metadata))
                           (m (when meta (copy-sequence meta))))
                      (when m
                        (when (plist-member m :suppress_patch)
                          (setq m (plist-put m :suppress-patch (plist-get m :suppress_patch)))
                          (cl-remf m :suppress_patch)))
                      (list :type msg-type
                            :task-id tid
                            :message (plist-get msg :message)
                            :metadata m)))
                  send-message-payloads))
         (shared-state (list :results (make-hash-table :test 'equal)
                             :total total
                             :final-callback final-callback
                             :parent-buf actual-parent-buf
                             :parent-fsm actual-parent-fsm
                             :parent-ctx parent-ctx
                             :original-payloads normalized-payloads)))
    (if (= total 0)
        (when final-callback (funcall final-callback nil))
      (dolist (msg normalized-payloads)
        (if (eq (plist-get msg :type) 'ARTIFACT_UPDATE)
            (let* ((tid (plist-get msg :task-id))
                   (cb (when tid (gethash tid macher-agent--pending-callbacks))))
              (if cb
                  (progn
                    (remhash tid macher-agent--pending-callbacks)
                    (funcall cb msg))
                (display-warning 'macher-agent
                                 (format "No pending callback found for ARTIFACT_UPDATE with task-id '%s'."
                                         tid)
                                 :warning)))
          (let ((initial-state (list :a2a-msg msg
                                     :shared-state shared-state
                                     :child-buf nil)))
            (seq-reduce (lambda (state pipe-fn)
                          (funcall pipe-fn state))
                        macher-agent-a2a-pipeline-functions
                        initial-state)))))))

(defun macher-agent--init-subagent-state (buf task-id meta &optional parent-ctx-or-suppress parent-ctx)
  "Initialise standard sub-agent variables and context in BUF.

Apply metadata key-value pairs in META dynamically to matching buffer-local
variables in BUF, ensuring agnostic state transport."
  (let ((actual-parent-ctx (if (macher-agent-valid-context-p parent-ctx-or-suppress)
                               parent-ctx-or-suppress
                             parent-ctx)))
    (with-current-buffer buf
      (setq-local macher-agent--is-subagent t)
      (setq-local macher-agent--current-task-id task-id)
      (when meta
        (cl-loop for (k v) on meta by #'cddr
                 do (let* ((k-str (substring (symbol-name k) (if (string-prefix-p ":" (symbol-name k)) 1 0)))
                           (norm-k-str (replace-regexp-in-string "_" "-" k-str))
                           (sym (intern (format "macher-agent--%s" norm-k-str))))
                      (set (make-local-variable sym) v))))
      (when (plist-get meta :background)
        (setq-local macher-agent--ready-to-reap nil))
      (when actual-parent-ctx
        (setq-local macher-agent--persistent-context
                    (if (fboundp 'macher-agent--clone-context)
                        (macher-agent--clone-context actual-parent-ctx)
                      actual-parent-ctx)))
      (unless (plist-get meta :background)
        (macher-agent-ui-show buf)))))

(defun macher-agent-a2a-pipe--validate-routing (state)
  "Validate message routing paths and prevent circular communication loops.

Extract task identifiers and metadata from the input state. Perform full
resolution of the canonical buffer names for the originator and the
intended target. Provide strict interception of requests where an agent
attempts to route a payload to its own buffer, returning an error structure
to halt execution.

STATE is the property list containing the active message payload and
shared execution properties.

Return the updated STATE property list containing derived target names
or an error payload.

Side effects: None."
  (let* ((msg (plist-get state :a2a-msg))
         (meta (plist-get msg :metadata))
         (task-id (or (plist-get msg :task-id)
                      (bound-and-true-p macher-agent--current-task-id)
                      (macher-agent--generate-uuid)))
         (shared (plist-get state :shared-state))
         (parent-buf (plist-get shared :parent-buf)))

    (unless (plist-get msg :task-id)
      (setq msg (plist-put (copy-sequence msg) :task-id task-id))
      (setq state (plist-put state :a2a-msg msg)))

    (let* ((raw-buf (plist-get meta :buffer_name))
           (originator-buf (or (plist-get meta :originator) parent-buf (current-buffer)))
           (buf-name (macher-agent--resolve-buffer-name raw-buf))
           (originator-name (macher-agent--resolve-buffer-name originator-buf)))

      (setq state (plist-put state :target-name buf-name))
      (setq state (plist-put state :originator-name originator-name))

      (if (and buf-name originator-name (equal buf-name originator-name))
          (plist-put state :error-payload
                     (list :status 'error
                           :error (format "ERROR: Agent cannot route a sub-agent payload to its own buffer ('%s')." buf-name)
                           :buffer-name buf-name
                           :task-id task-id))
        state))))

(defun macher-agent-a2a-pipe--acquire-target (state)
  "Acquire the target buffer instance or spawn a new sub-agent buffer.

Evaluate the target buffer name against active Emacs buffers. If a matching
buffer exists, acquire it and prepare its local environment. If the target
does not exist but preset specifications are available, generate a fresh
sub-agent buffer. Delegate local variable initialisation and context
cloning to dedicated helper routines.

STATE is the property list containing the validated message payload and
target buffer names.

Return the updated STATE property list containing the resolved child buffer
object or an error payload.

Side effects: Creates new buffers and mutates local variables within the
target buffer."
  (if (plist-get state :error-payload)
      state
    (let* ((msg (plist-get state :a2a-msg))
           (meta (plist-get msg :metadata))
           (task-id (plist-get msg :task-id))
           (raw-buf (plist-get meta :buffer_name))
           (presets (plist-get meta :presets))
           (buf-name (plist-get state :target-name))
           (shared (plist-get state :shared-state))
           (parent-buf (let ((pb (macher-agent--extract-prop shared :parent-buf)))
                         (and (not (eq pb 'macher-missing)) pb)))
           (parent-ctx (or (ignore-errors (macher-agent-resolve-context shared))
                           (when (and parent-buf (buffer-live-p parent-buf))
                             (ignore-errors (macher-agent-resolve-context parent-buf)))))
           (existing-buf (cond
                          ((and (bufferp raw-buf) (buffer-live-p raw-buf)) raw-buf)
                          ((and buf-name (stringp buf-name)) (get-buffer buf-name))
                          (t nil))))

      (if (and existing-buf (buffer-live-p existing-buf))
          (progn
            (macher-agent--init-subagent-state existing-buf task-id meta parent-ctx)
            (plist-put state :child-buf existing-buf))
        (if (and presets (stringp buf-name) (not (string-empty-p buf-name)))
            (condition-case _err
                (let ((child-buf (macher-agent-add-subagent buf-name presets parent-buf nil parent-ctx)))
                  (macher-agent--init-subagent-state child-buf task-id meta parent-ctx)
                  (plist-put state :child-buf child-buf))
              (error
               (plist-put state :error-payload
                          (list :status 'error
                                :error (format "ERROR: Sub-agent buffer '%s' not found." buf-name)
                                :buffer-name buf-name
                                :task-id task-id))))
          (plist-put state :error-payload
                     (list :status 'error
                           :error (format "ERROR: Sub-agent buffer '%s' not found." buf-name)
                           :buffer-name buf-name
                           :task-id task-id)))))))

(defun macher-agent-a2a-pipe--register-ownership (state)
  "Register hierarchical ownership paths in the global agent registries.

Map the active task identifier to the originating buffer name to ensure
accurate result routing. Register the acquired target buffer as a dependent
child of the originator within the global ownership hash table. Establish a
workspace-scoped ownership key to facilitate safe garbage collection when
the project session concludes.

STATE is the property list containing the acquired target buffer and
routing metadata.

Return the unmodified STATE property list.

Side effects: Modifies the global `macher-agent--task-registry' and
`macher-agent--a2a-ownership' hash tables."
  (if (or (plist-get state :error-payload)
          (null (plist-get state :child-buf))
          (null (plist-get state :originator-name)))
      state
    (let* ((msg (plist-get state :a2a-msg))
           (task-id (plist-get msg :task-id))
           (originator-name (plist-get state :originator-name))
           (child-buf (plist-get state :child-buf))
           (child-name (buffer-name child-buf))
           (shared (plist-get state :shared-state))
           (parent-buf (plist-get shared :parent-buf)))

      (when task-id
        (puthash task-id originator-name macher-agent--task-registry))

      (let* ((ws (or (when (and parent-buf (buffer-live-p parent-buf))
                       (let ((pctx (buffer-local-value 'macher-agent--persistent-context parent-buf)))
                         (when pctx (ignore-errors (macher-agent--get-context-workspace pctx)))))
                     (when (and child-buf (buffer-live-p child-buf))
                       (let ((cctx (buffer-local-value 'macher-agent--persistent-context child-buf)))
                         (when cctx (ignore-errors (macher-agent--get-context-workspace cctx)))))
                     (when (fboundp 'macher-workspace)
                       (ignore-errors (macher-workspace (current-buffer))))))
             (ws-root (when ws
                        (ignore-errors (macher-agent-workspace-project-root ws))))
             (scoped-key (when (and ws-root originator-name)
                           (format "%s::%s" (expand-file-name ws-root) originator-name)))
             (current-list (gethash originator-name macher-agent--a2a-ownership nil)))

        (puthash originator-name
                 (if (member child-name current-list)
                     current-list
                   (cons child-name current-list))
                 macher-agent--a2a-ownership)

        (when scoped-key
          (let ((scoped-list (gethash scoped-key macher-agent--a2a-ownership nil)))
            (puthash scoped-key
                     (if (member child-name scoped-list)
                         scoped-list
                       (cons child-name scoped-list))
                     macher-agent--a2a-ownership))))
      state)))

(defun macher-agent-a2a-pipe--route-instructions (state)
  "Route payload instructions to the target buffer or intercept suspended states.

Extract raw instruction text from the message payload. If the target agent
holds a suspended state awaiting a callback, intercept the delivery and
stage the instructions as a wake message. Otherwise, insert the instruction
text directly into the target buffer to prepare the agent for its text
generation cycle.

STATE is the property list containing the acquired target buffer and
message payload.

Return the updated STATE property list containing staged wake callbacks
if applicable.

Side effects: Mutates the text contents of the target buffer."
  (if (plist-get state :error-payload)
      state
    (let* ((child-buf (plist-get state :child-buf))
           (msg-payload (plist-get (plist-get state :a2a-msg) :message))
           (instructions (if (listp msg-payload) (plist-get msg-payload :instructions) msg-payload))
           (wake-cb (and child-buf (buffer-live-p child-buf)
                         (gethash (buffer-name child-buf) macher-agent--pending-callbacks))))
      (if wake-cb
          (progn
            (remhash (buffer-name child-buf) macher-agent--pending-callbacks)
            (plist-put state :wake-cb wake-cb)
            (plist-put state :wake-msg instructions))
        (when (and child-buf (buffer-live-p child-buf))
          (with-current-buffer child-buf
            (goto-char (point-max))
            (insert (or instructions "") "\n\n"))))
      state)))

(defun macher-agent--aggregate-a2a-results (task-id msg-body results total original-payloads final-callback parent-buf parent-fsm)
  "Aggregate A2A results and trigger final callback when complete.

TASK-ID is the completed task identifier string.
MSG-BODY is the returned message body property list.
RESULTS is the hash table collecting sub-agent results.
TOTAL is the integer count of expected sub-agent tasks.
ORIGINAL-PAYLOADS is the list of initial dispatch payloads.
FINAL-CALLBACK is the function invoked with the aggregated results vector.
PARENT-BUF is the orchestrator parent buffer object.
PARENT-FSM is the active orchestrator state machine instance.

Return nil.

Side effects: Updates RESULTS hash table."
  (puthash task-id msg-body results)
  (when (and (= (hash-table-count results) total)
             (not (gethash '*completed* results)))
    (let ((ordered-results
           (mapcar (lambda (p)
                     (let ((p-tid (or (plist-get p :task-id) task-id)))
                       (gethash p-tid results)))
                   original-payloads)))
      (puthash '*completed* t results)
      (if (and parent-buf (buffer-live-p parent-buf))
          (with-current-buffer parent-buf
            (when final-callback
              (let ((macher-agent--active-fsm parent-fsm)
                    (gptel--fsm-last parent-fsm))
                (funcall final-callback (vconcat ordered-results)))))
        (when final-callback
          (funcall final-callback (vconcat ordered-results)))))))

(defun macher-agent-a2a-pipe--bind-closure (state)
  "Bind the remote procedure closure and manage result aggregation.

Construct a lexical closure designed for asynchronous return trip
management. The closure provides safe application of virtual file system
differences to the orchestrator environment and performs steady aggregation
of incoming payloads.

The `suppress-patch' metadata flag applies strictly to interactive
diffs in the UI. It is not used to gate the VFS merge in this closure.

STATE is the property list containing the active message, shared task
totals, and target buffer.

Return the updated STATE property list containing the compiled callback
closure.

Side effects: Updates the pending callbacks hash table and modifies the
routing stack of the child buffer."
  (let* ((msg (plist-get state :a2a-msg))
         (meta (plist-get msg :metadata))
         (raw-task-id (or (plist-get msg :task-id)
                          (bound-and-true-p macher-agent--current-task-id)
                          (macher-agent--generate-uuid)))
         (task-id (if (gethash raw-task-id macher-agent--pending-callbacks)
                      (macher-agent--generate-uuid)
                    raw-task-id))
         (suppress-patch (plist-get meta :suppress-patch))
         (shared (plist-get state :shared-state))
         (results (plist-get shared :results))
         (parent-buf (plist-get shared :parent-buf))
         (parent-name (or (plist-get state :originator-name)
                          (when (and parent-buf (buffer-live-p parent-buf))
                            (buffer-name parent-buf))))
         (parent-fsm (plist-get shared :parent-fsm))
         (total (plist-get shared :total))
         (final-callback (plist-get shared :final-callback))
         (original-payloads (plist-get shared :original-payloads))
         (wake-cb (plist-get state :wake-cb))
         (err-payload (plist-get state :error-payload)))

    (unless (equal task-id (plist-get msg :task-id))
      (setq msg (plist-put (copy-sequence msg) :task-id task-id))
      (setq state (plist-put state :a2a-msg msg)))

    (if err-payload
        (macher-agent--aggregate-a2a-results task-id err-payload results total original-payloads final-callback parent-buf parent-fsm)
      (let ((child-buf (plist-get state :child-buf)))
        (when (and child-buf (buffer-live-p child-buf))
          (with-current-buffer child-buf
            (unless wake-cb
              (let* ((called nil)
                     (a2a-cb
                      (lambda (artifact-payload)
                        (unless called
                          (setq called t)
                          (when task-id (remhash task-id macher-agent--pending-callbacks))
                          (let* ((msg-body (if (eq (plist-get artifact-payload :type) 'ARTIFACT_UPDATE)
                                               (plist-get artifact-payload :message)
                                             artifact-payload))
                                 (actual-task-id (or (plist-get artifact-payload :task-id)
                                                     (plist-get msg-body :task-id)
                                                     task-id)))
                            (unless (plist-get msg-body :shared-state)
                              (setq msg-body (plist-put (copy-sequence msg-body) :shared-state shared)))

                            (when (and parent-buf (buffer-live-p parent-buf))
                              (with-current-buffer parent-buf
                                (let ((parent-ctx (or (bound-and-true-p macher-agent--persistent-context)
                                                      (ignore-errors (macher-agent-resolve-context parent-buf)))))
                                  (when parent-ctx
                                    (setq-local macher-agent--persistent-context parent-ctx)
                                    (setq msg-body (plist-put (copy-sequence msg-body) :target-context parent-ctx))))
                                (setq msg-body (macher-agent-run-pipeline 'payload-merge msg-body))
                                (when-let* ((merged-ctx (plist-get msg-body :target-context)))
                                  (setq-local macher-agent--persistent-context merged-ctx)
                                  (when parent-fsm
                                    (when (fboundp 'macher-agent--inject-context-into-fsm-info)
                                      (macher-agent--inject-context-into-fsm-info merged-ctx parent-fsm))))))

                            (macher-agent--aggregate-a2a-results actual-task-id msg-body results total original-payloads final-callback parent-buf parent-fsm))))))
                (puthash task-id a2a-cb macher-agent--pending-callbacks)
                (macher-agent--push-routing task-id parent-name suppress-patch)
                (setq state (plist-put state :a2a-cb a2a-cb))))))))
    state))

(defun macher-agent--push-context-to-parent (child-ctx parent-buf)
  "Merge CHILD-CTX into PARENT-BUF within the strict scope of the parent.
This guarantees mutation hooks and skill initialisations update the
parent's registry."
  (when (and child-ctx (buffer-live-p parent-buf))
    (with-current-buffer parent-buf
      (let ((parent-ctx (or (bound-and-true-p macher-agent--persistent-context)
                            (ignore-errors (macher-agent-resolve-context)))))
        (when (and parent-ctx (not (eq parent-ctx child-ctx)))
          (let ((merged-ctx (if (fboundp 'macher-agent--merge-contexts)
                                (macher-agent--merge-contexts parent-ctx child-ctx)
                              child-ctx)))
            (when merged-ctx
              (setq-local macher-agent--persistent-context merged-ctx))))))))

(defun macher-agent-a2a-pipe--transmit (state)
  "Trigger network transmission or execute staged wake closures.

Finalise the dispatch sequence by acting upon the staged state. If the
target agent holds a suspended state, call the wake closure with the
staged instructions. For standard execution paths, bind success and error
handlers to the pending closure, package the execution context, and
command the language model integration layer to begin network transmission.

STATE is the property list containing the prepared execution environment
and callback closures.

Return the STATE property list.

Side effects: Initiates asynchronous network processes and modifies text
generation hooks."
  (unless (plist-get state :error-payload)
    (if-let* ((wake-cb (plist-get state :wake-cb)))
        (funcall wake-cb (plist-get state :wake-msg))
      (when-let* ((child-buf (plist-get state :child-buf)))
        (when (buffer-live-p child-buf)
          (with-current-buffer child-buf
            (let* ((msg (plist-get state :a2a-msg))
                   (task-id (or (plist-get msg :task-id)
                                (bound-and-true-p macher-agent--current-task-id)))
                   (a2a-cb (or (and task-id (gethash task-id macher-agent--pending-callbacks))
                               (plist-get state :a2a-cb)))
                   (target-name (buffer-name child-buf))
                   (macher-agent--active-fsm nil)
                   (callbacks
                    (list :success-cb
                          (lambda (res)
                            (let ((cb (and task-id (gethash task-id macher-agent--pending-callbacks))))
                              (when cb
                                (remhash task-id macher-agent--pending-callbacks)
                                (funcall cb (list :status 'success
                                                  :data res
                                                  :task-id task-id
                                                  :buffer-name target-name)))))
                          :on-success
                          (lambda (res)
                            (let ((cb (and task-id (gethash task-id macher-agent--pending-callbacks))))
                              (when cb
                                (remhash task-id macher-agent--pending-callbacks)
                                (funcall cb (list :status 'success
                                                  :data res
                                                  :task-id task-id
                                                  :buffer-name target-name)))))
                          :error-cb
                          (lambda (err)
                            (let ((cb (or (and task-id (gethash task-id macher-agent--pending-callbacks)) a2a-cb)))
                              (when cb
                                (when task-id (remhash task-id macher-agent--pending-callbacks))
                                (funcall cb (list :status 'error
                                                  :error err
                                                  :task-id task-id
                                                  :buffer-name target-name)))))
                          :on-error
                          (lambda (err)
                            (let ((cb (or (and task-id (gethash task-id macher-agent--pending-callbacks)) a2a-cb)))
                              (when cb
                                (when task-id (remhash task-id macher-agent--pending-callbacks))
                                (funcall cb (list :status 'error
                                                  :error err
                                                  :task-id task-id
                                                  :buffer-name target-name))))))))

              (let ((ctx (make-macher-agent-task-context :target-buffer (current-buffer))))
                (macher-agent-gptel-transmit ctx callbacks))))))))
  state)

(cl-defstruct macher-agent-task-context
  "Represent a task execution context structure.

WORKSPACE is the target workspace instance or path.
TARGET-BUFFER is the target buffer for task execution.
SKILL-SYM is the active skill or preset symbol.
SYSTEM-MESSAGE is the system prompt message string.

Return a new `macher-agent-task-context' struct instance.

Side effects: None."
  workspace
  target-buffer
  skill-sym
  system-message)

(defun macher-agent--resolve-buffer-name (name)
  "Resolve buffer string or buffer object NAME to a buffer name string.

NAME is a buffer object, string buffer name, or file path.

Return the resolved buffer name string, or NAME if string and unmapped.

Side effects: None."
  (cond
   ((bufferp name) (buffer-name name))
   ((stringp name)
    (or (when-let* ((buf (get-buffer name)))
          (buffer-name buf))
        (when-let* ((buf (get-file-buffer name)))
          (buffer-name buf))
        (when-let* ((buf (get-file-buffer (expand-file-name name))))
          (buffer-name buf))
        name))
   (t name)))

(defun macher-agent--reap-buffer (buf)
  "Reap sub-agent BUF by aborting gptel operations and killing it.

BUF is the sub-agent buffer to reap.

Return nil.

Side effects: Aborts pending gptel operations and kills BUF."
  (when (and (buffer-live-p buf)
             (with-current-buffer buf
               (and (macher-agent-subagent-p)
                    (macher-agent-ready-to-reap-p)
                    (or (bound-and-true-p macher-agent-task-finished)
                        (bound-and-true-p macher-agent--ready-to-reap)))))
    (let ((name (buffer-name buf)))
      (macher-agent--remove-active-subagent-registries name buf)
      (with-current-buffer buf
        (set-buffer-modified-p nil)
        (ignore-errors (macher-agent-bridge-abort buf))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(defun macher-agent--compose-merge-system-prompt (current-sys sym sys-spec)
  "Merge SYS-SPEC into CURRENT-SYS for skill SYM.

CURRENT-SYS is the existing system prompt (string, list, or nil).
SYM is the preset symbol.
SYS-SPEC is the system prompt specification (string or modifier).

Return the updated system prompt value.

Side effects: None."
  (if (stringp sys-spec)
      (let* ((cur (if (listp current-sys)
                      (string-join current-sys "\n---\n")
                    (or current-sys "")))
             (trimmed-spec (string-trim sys-spec)))
        (if (and (not (string-empty-p trimmed-spec))
                 (string-match-p (regexp-quote trimmed-spec) cur))
            cur
          (concat cur
                  (if (string-empty-p cur) "" "\n---\n")
                  (format "### Skill: %s\n%s\n" sym sys-spec))))
    (gptel--modify-value current-sys sys-spec)))

(defun macher-agent--resolve-single-tool-object (tool)
  "Resolve TOOL to a `gptel-tool' structure.

If TOOL is already a `gptel-tool' structure (checked via `gptel-tool-p`),
pass it through intact.
If TOOL is a string, symbol, or abstract list, use the existing registry
lookup to convert it to a `gptel-tool' object before assignment.

Return the resolved `gptel-tool' object, or TOOL if unresolved.

Side effects: None."
  (cond
   ((and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
    tool)
   ((and (consp tool) (keywordp (car tool)))
    (apply #'gptel-make-tool tool))
   ((symbolp tool)
    (or (when (and (boundp tool)
                   (fboundp 'gptel-tool-p)
                   (gptel-tool-p (symbol-value tool)))
          (symbol-value tool))
        (ignore-errors (gptel-get-tool (symbol-name tool)))
        (ignore-errors
          (gptel-get-tool (replace-regexp-in-string "-" "_" (symbol-name tool))))
        (let ((res (ignore-errors (macher-agent-resolve-tool tool nil))))
          (if (and (fboundp 'gptel-tool-p) (gptel-tool-p res))
              res
            nil))
        (ignore-errors (macher-agent-resolve-to-struct tool))
        tool))
   ((stringp tool)
    (or (ignore-errors (gptel-get-tool tool))
        (ignore-errors
          (gptel-get-tool (replace-regexp-in-string "-" "_" tool)))
        (ignore-errors
          (gptel-get-tool (replace-regexp-in-string "_" "-" tool)))
        (let ((res (ignore-errors (macher-agent-resolve-tool tool nil))))
          (if (and (fboundp 'gptel-tool-p) (gptel-tool-p res))
              res
            nil))
        (ignore-errors (macher-agent-resolve-to-struct tool))
        tool))
   ((listp tool)
    (or (ignore-errors (gptel-get-tool tool))
        (ignore-errors (macher-agent-resolve-to-struct tool))
        (let ((res (ignore-errors (macher-agent-resolve-tool tool nil))))
          (if (and (fboundp 'gptel-tool-p) (gptel-tool-p res))
              res
            nil))
        tool))
   (t tool)))

(defun macher-agent--compose-merge-tools (current-tools tools-spec)
  "Merge TOOLS-SPEC into CURRENT-TOOLS.

CURRENT-TOOLS is the current list of tools.
TOOLS-SPEC is the tools specification (modifier, list, string, or similar).

Return the updated tools list.

Side effects: None."
  (let ((merged-tools (gptel--modify-value current-tools tools-spec)))
    (cond
     ((null merged-tools) nil)
     ((and (fboundp 'gptel-tool-p) (gptel-tool-p merged-tools))
      (list merged-tools))
     ((and (consp merged-tools) (keywordp (car merged-tools)))
      (list (apply #'gptel-make-tool merged-tools)))
     ((and (listp merged-tools)
           (not (and (consp merged-tools) (keywordp (car merged-tools))))
           (let ((single (ignore-errors (gptel-get-tool merged-tools))))
             (and single (fboundp 'gptel-tool-p) (gptel-tool-p single))))
      (list (gptel-get-tool merged-tools)))
     ((listp merged-tools)
      (cl-loop for t-obj in merged-tools
               append (let ((res (macher-agent--resolve-single-tool-object t-obj)))
                        (if (and (listp res)
                                 (not (and (fboundp 'gptel-tool-p) (gptel-tool-p res))))
                            res
                          (list res)))))
     (t
      (list (macher-agent--resolve-single-tool-object merged-tools))))))

(defun macher-agent--resolve-preset-or-tool (sym known)
  "Resolve SYM against KNOWN presets or gptel tools.

SYM is the preset or tool symbol, string, or gptel-tool structure.
KNOWN is an alist of known preset specifications.

Return a cons cell (TYPE . VALUE) where TYPE is \\'preset or \\'tool, or nil.

Side effects: None."
  (cond
   ((and (fboundp 'gptel-tool-p) (gptel-tool-p sym))
    (cons 'tool sym))
   ((and (consp sym) (keywordp (car sym)))
    (if (or (plist-member sym :function) (plist-member sym :args))
        (cons 'tool sym)
      (cons 'preset sym)))
   (t
    (when-let* ((clean-sym (macher-normalise-preset-name sym)))
      (if-let*
          ((spec (or (alist-get clean-sym known)
                     (when-let* ((ctx (ignore-errors (macher-agent-resolve-context))))
                       (alist-get clean-sym (macher-agent-workspace-skills-alist ctx)))
                     (alist-get clean-sym (bound-and-true-p macher-agent-global-skills-alist)))))
          (cons 'preset spec)
        (when-let*
            ((tool
              (or (ignore-errors (gptel-get-tool (symbol-name clean-sym)))
                  (ignore-errors
                    (gptel-get-tool (replace-regexp-in-string "-" "_" (symbol-name clean-sym)))))))
          (cons 'tool tool)))))))

(defvar macher-agent-preset-pipeline-functions
  '(macher-agent-preset-pipe--exclusive
    macher-agent-preset-pipe--system
    macher-agent-preset-pipe--tools
    macher-agent-preset-pipe--ptc
    macher-agent-preset-pipe--boot
    macher-agent-preset-pipe--parameters)
  "Pipeline functions for preset payload composition.

Each function takes accumulated STATE plist and an ITEM tuple
`(preset SYM SPEC)' or `(tool TOOL)', returning updated STATE.")

(defun macher-agent-preset-pipe--exclusive (state item)
  "Apply the `:exclusive' preset modifier in ITEM to STATE."
  (pcase item
    (`(preset ,_sym ,spec)
     (if (plist-get spec :exclusive)
         (let ((st (copy-sequence state)))
           (setq st (plist-put st :system nil))
           (setq st (plist-put st :tools nil))
           (setq st (plist-put st :ptc-primitives nil))
           (setq st (plist-put st :boot-directive nil))
           st)
       state))
    (_ state)))

(defun macher-agent-preset-pipe--system (state item)
  "Merge the system prompt specification in ITEM into STATE.

STATE is the accumulated payload plist.
ITEM is a resolved item tuple `(preset SYM SPEC)' or `(tool TOOL)'.

Return the updated STATE plist.

Side effects: None."
  (pcase item
    (`(preset ,sym ,spec)
     (if-let* ((sys-spec (or (plist-get spec :system)
                             (plist-get spec :system-message))))
         (plist-put state :system
                    (macher-agent--compose-merge-system-prompt
                     (plist-get state :system) sym sys-spec))
       state))
    (_ state)))

(defun macher-agent-preset-pipe--tools (state item)
  "Merge allowed tools specification or standalone tool in ITEM into STATE.

STATE is the accumulated payload plist.
ITEM is a resolved item tuple `(preset SYM SPEC)' or `(tool TOOL)'.

Return the updated STATE plist.

Side effects: None."
  (pcase item
    (`(preset ,_sym ,spec)
     (if-let* ((tools-spec (or (plist-get spec :tools)
                               (plist-get spec :allowed-tools))))
         (plist-put state :tools
                    (macher-agent--compose-merge-tools
                     (plist-get state :tools) tools-spec))
       state))
    (`(tool ,tool-obj)
     (plist-put state :tools
                (macher-agent--compose-merge-tools
                 (plist-get state :tools)
                 (list :append (if (or (and (consp tool-obj) (keywordp (car tool-obj)))
                                       (not (listp tool-obj))
                                       (and (fboundp 'gptel-tool-p)
                                            (gptel-tool-p tool-obj)))
                                   (list tool-obj)
                                 tool-obj)))))
    (_ state)))

(defun macher-agent-preset-pipe--ptc (state item)
  "Merge PTC primitives specification in ITEM into STATE.

STATE is the accumulated payload plist.
ITEM is a resolved item tuple `(preset SYM SPEC)' or `(tool TOOL)'.

Return the updated STATE plist.

Side effects: None."
  (pcase item
    (`(preset ,_sym ,spec)
     (if-let* ((ptc-spec (plist-get spec :ptc-primitives)))
         (let ((current-ptc (plist-get state :ptc-primitives))
               (ptc-list (if (listp ptc-spec) ptc-spec (list ptc-spec))))
           (plist-put state :ptc-primitives
                      (cl-delete-duplicates (append current-ptc ptc-list)
                                            :test #'equal)))
       state))
    (_ state)))

(defun macher-agent-preset-pipe--boot (state item)
  "Merge boot directive specification in ITEM into STATE.

STATE is the accumulated payload plist.
ITEM is a resolved item tuple `(preset SYM SPEC)' or `(tool TOOL)'.

Return the updated STATE plist.

Side effects: None."
  (pcase item
    (`(preset ,_sym ,spec)
     (if-let* ((bd (plist-get spec :boot-directive)))
         (plist-put state :boot-directive bd)
       state))
    (_ state)))

(defun macher-agent-preset-pipe--parameters (state item)
  "Merge model parameters in ITEM into STATE.

STATE is the accumulated payload plist.
ITEM is a resolved item tuple `(preset SYM SPEC)' or `(tool TOOL)'.

Return the updated STATE plist.

Side effects: None."
  (pcase item
    (`(preset ,_sym ,spec)
     (let ((st state))
       (dolist (key '(:model :backend :temperature :max-tokens))
         (when-let* ((val (plist-get spec key)))
           (setq st (plist-put st key val))))
       st))
    (_ state)))

(defun macher-agent--flatten-preset-dependencies (inline-presets known)
  "Pre-flatten INLINE-PRESETS and their parent preset dependencies.

INLINE-PRESETS is a list of preset symbols, strings, or tools.
KNOWN is an alist of known preset specifications.

Return a list of resolved item tuples `(preset SYM SPEC)' or `(tool TOOL)'.

Side effects: None."
  (let ((result nil)
        (visited nil))
    (cl-labels
        ((flatten-item (sym)
           (cond
            ((and (symbolp sym) (member sym visited))
             nil)
            (t
             (when (symbolp sym)
               (push sym visited))
             (pcase (macher-agent--resolve-preset-or-tool sym known)
               (`(preset . ,spec)
                (when-let* ((parents (plist-get spec :parents)))
                  (dolist (parent (if (listp parents) parents (list parents)))
                    (flatten-item parent)))
                (push (list 'preset sym spec) result))
               (`(tool . ,tool)
                (push (list 'tool tool) result))
               (_
                (when sym
                  (push (list 'tool sym) result))))))))
      (dolist (sym inline-presets)
        (flatten-item sym))
      (nreverse result))))

(defun macher-agent-compose-payload (base-state inline-presets)
  "Compose a transmission payload by merging BASE-STATE with INLINE-PRESETS.

Merges BASE-STATE with the resolved configuration of INLINE-PRESETS
using native gptel modification values.

First, pre-flattens INLINE-PRESETS and any parent preset dependencies.
Then reduces over static preset pipeline functions for each item.
After processing all individual dependencies, passes the aggregated state
through dynamic preset-composition pipeline steps exactly once.
Finally, normalises the composed tools list.

BASE-STATE is the base state property list.
INLINE-PRESETS is the list of inline preset symbols.

Return the unified state property list.

Side effects: None."
  (let* ((state (copy-sequence base-state))
         (known (plist-get base-state :known-presets))
         (flattened (macher-agent--flatten-preset-dependencies inline-presets known))
         (static-steps macher-agent-preset-pipeline-functions)
         (dynamic-steps (macher-agent-get-pipeline-steps 'preset-composition)))
    (setq state
          (cl-reduce
           (lambda (st item)
             (cl-reduce (lambda (s pipe-fn)
                          (funcall pipe-fn s item))
                        static-steps
                        :initial-value st))
           flattened
           :initial-value state))
    (when dynamic-steps
      (setq state
            (seq-reduce (lambda (st dyn-fn)
                          (funcall dyn-fn st))
                        dynamic-steps
                        state)))
    (setq state (plist-put state :tools (macher-agent-normalize-tools (plist-get state :tools))))
    state))

(defun macher-agent--apply-payload-locally (payload)
  "Apply a composed payload to the current buffer variables.

PAYLOAD is the composed state property list.

Return nil.

Side effects: Sets buffer-local values for gptel and PTC variables."
  (when payload
    (when (plist-member payload :system)
      (setq-local gptel-system-prompt (plist-get payload :system)))
    (when (plist-member payload :model)
      (setq-local gptel-model (plist-get payload :model)))
    (when (plist-member payload :backend)
      (setq-local gptel-backend (plist-get payload :backend)))
    (when (plist-member payload :temperature)
      (setq-local gptel-temperature (plist-get payload :temperature)))
    (when (plist-member payload :max-tokens)
      (setq-local gptel-max-tokens (plist-get payload :max-tokens)))
    (when (plist-member payload :tools)
      (setq-local gptel-tools (plist-get payload :tools)))
    (when (plist-member payload :ptc-primitives)
      (setq-local macher-agent--active-ptc-primitives (plist-get payload :ptc-primitives)))
    (when (plist-member payload :boot-directive)
      (setq-local macher-agent--boot-directive (plist-get payload :boot-directive)))))

(defun macher-agent--apply-preset (preset-or-presets)
  "Apply PRESET-OR-PRESETS to current buffer using payload compositor.

PRESET-OR-PRESETS represents the preset symbol, list, or vector.

Return nil.

Side effects: Updates buffer-local gptel and PTC settings."
  (let* ((presets (cond ((listp preset-or-presets) preset-or-presets)
                        ((vectorp preset-or-presets) (append preset-or-presets nil))
                        (t (list preset-or-presets))))
         (base-state (list :model gptel-model
                           :backend (bound-and-true-p gptel-backend)
                           :system (bound-and-true-p gptel-system-prompt)
                           :temperature (bound-and-true-p gptel-temperature)
                           :max-tokens (bound-and-true-p gptel-max-tokens)
                           :tools gptel-tools
                           :known-presets (bound-and-true-p gptel--known-presets)))
         (payload (macher-agent-compose-payload base-state presets)))

    (when presets
      (let* ((primary-sym (macher-agent--extract-first-preset-symbol presets))
             (known (plist-get base-state :known-presets))
             (spec (when primary-sym
                     (or (alist-get primary-sym known)
                         (when-let* ((ctx (ignore-errors (macher-agent-resolve-context))))
                           (alist-get primary-sym (macher-agent-workspace-skills-alist ctx)))
                         (alist-get primary-sym (bound-and-true-p macher-agent-global-skills-alist)))))
             (raw-sys (or (plist-get spec :system) (plist-get spec :system-message)))
             (boot-dir (plist-get spec :boot-directive)))
        (when (stringp raw-sys)
          (setq payload (plist-put payload :system raw-sys)))
        (when boot-dir
          (setq payload (plist-put payload :boot-directive boot-dir)))))

    (macher-agent--apply-payload-locally payload)))

(defun macher-agent-use-skill (skill &optional buf)
  "Apply SKILL preset to BUF or current buffer.

SKILL is a skill symbol, string, or list of skills.
BUF is the target buffer, defaulting to current buffer.

Return nil.

Side effects: Sets buffer-local presets, gptel settings, and
boot-directive."
  (interactive "sSkill: ")
  (let ((target-buf (cond ((bufferp buf) buf)
                          ((stringp buf) (get-buffer buf))
                          (t (current-buffer)))))
    (when (buffer-live-p target-buf)
      (with-current-buffer target-buf
        (let ((presets (cond ((listp skill) skill)
                             ((vectorp skill) (append skill nil))
                             (t (list skill)))))
          (setq-local macher-agent-presets presets)
          (macher-agent--apply-preset presets))))))

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
    (push frame macher-agent--routing-stack)
    (setq-local macher-agent--current-task-id task-id)
    (setq-local macher-agent--suppress-patch suppress-patch)
    frame))

(defun macher-agent--pop-routing ()
  "Pop top routing context frame off `macher-agent--routing-stack'.

Restores local buffer state to match the new top frame of the stack.

Return popped frame plist, or a fallback frame if stack was empty.

Side effects: Modifies `macher-agent--routing-stack' and syncs local state."
  (if macher-agent--routing-stack
      (let ((popped (pop macher-agent--routing-stack))
            (top (car macher-agent--routing-stack)))
        (setq-local macher-agent--current-task-id (plist-get top :task-id))
        (setq-local macher-agent--suppress-patch (plist-get top :suppress-patch))
        popped)
    (list :task-id (bound-and-true-p macher-agent--current-task-id)
          :originator-name nil
          :suppress-patch (bound-and-true-p macher-agent--suppress-patch))))

(defun macher-agent--ptc-primitive-p (sym)
  "Check whether SYM is an active Programmatic Tool Calling primitive.

Matches both direct symbol equivalence and dynamic translation between
hyphens and underscores for SYM.

SYM is the symbol to test.

Return t if SYM is an active primitive, otherwise nil.

Side effects: None."
  (let* ((sym-name (symbol-name sym))
         (norm-sym (replace-regexp-in-string "_" "-" sym-name))

         (fsm-obj (macher-agent-get-active-fsm))
         (info (when fsm-obj (gptel-fsm-info fsm-obj)))

         (active (or (when info (plist-get info :ptc-primitives))
                     (bound-and-true-p macher-agent--active-ptc-primitives)))
         (tools (or (when info (plist-get info :tools))
                    (bound-and-true-p gptel-tools))))

    (and (or (memq sym active)
             (cl-some (lambda (prim)
                        (let* ((prim-name (if (symbolp prim) (symbol-name prim) prim))
                               (norm-prim (replace-regexp-in-string "_" "-" prim-name)))
                          (string= norm-sym norm-prim)))
                      active)
             (and (null active)
                  (cl-some (lambda (tool)
                             (let* ((t-name (if (gptel-tool-p tool)
                                                (gptel-tool-name tool)
                                              (if (stringp tool) tool
                                                (format "%s" tool))))
                                    (norm-tool (replace-regexp-in-string "_" "-" t-name)))
                               (string= norm-sym norm-tool)))
                           tools)))
         t)))

(defun macher-agent--extract-first-preset-symbol (resolved-presets)
  "Extract normalised preset symbol from RESOLVED-PRESETS.

RESOLVED-PRESETS is a string, symbol, list, or vector of presets.

Return the clean symbol, or nil.

Side effects: None."
  (when-let* ((first-preset (cond ((stringp resolved-presets) resolved-presets)
                                  ((symbolp resolved-presets) (symbol-name resolved-presets))
                                  ((and (listp resolved-presets) (car resolved-presets))
                                   (if (symbolp (car resolved-presets))
                                       (symbol-name (car resolved-presets))
                                     (car resolved-presets)))
                                  ((and (vectorp resolved-presets) (> (length resolved-presets) 0))
                                   (if (symbolp (aref resolved-presets 0))
                                       (symbol-name (aref resolved-presets 0))
                                     (aref resolved-presets 0)))
                                  (t nil))))
    (intern (replace-regexp-in-string "^@+" "" first-preset))))

(defvar macher-agent-subagent-pipeline-functions
  '(macher-agent-subagent-pipe--normalize-args
    macher-agent-subagent-pipe--resolve-context
    macher-agent-subagent-pipe--init-buffer
    macher-agent-subagent-pipe--register)
  "Store chained reducer pipeline to construct and initialize a subagent buffer.")

(defun macher-agent-subagent-pipe--normalize-args (state)
  "Pipeline step 1: Normalize overloaded arguments in STATE."
  (let ((presets (plist-get state :presets))
        (parent (plist-get state :parent-buf))
        (dir (plist-get state :dir))
        (ctx (plist-get state :context)))

    (when-let* ((is-path-string (and (stringp presets)
                                     (or (file-directory-p presets)
                                         (string-prefix-p "/" presets)
                                         (string-suffix-p "/" presets)))))
      (unless dir
        (setq dir presets))
      (if-let* ((ctx-is-list (listp ctx))
                (ctx-not-obj (not (and (fboundp 'macher-context-p)
                                       (macher-context-p ctx)))))
          (progn
            (setq presets ctx)
            (setq ctx nil))
        (setq presets nil)))

    (when-let* ((has-fbound (fboundp 'macher-context-p))
                (is-ctx (macher-context-p presets)))
      (setq ctx presets)
      (setq presets nil))

    (when-let* ((has-fbound (fboundp 'macher-context-p))
                (is-ctx (macher-context-p dir)))
      (setq ctx dir)
      (setq dir (if-let* ((is-str (stringp presets))
                          (not-eq (not (equal presets ctx))))
                    presets
                  nil)))

    (when-let* ((has-fbound (fboundp 'macher-context-p))
                (is-ctx (macher-context-p parent)))
      (setq ctx parent)
      (setq parent nil))

    (unless parent
      (setq parent (current-buffer)))

    (plist-put (plist-put (plist-put (plist-put state :presets presets)
                                     :parent-buf parent)
                          :dir dir)
               :context ctx)))

(defun macher-agent-subagent-pipe--resolve-context (state)
  "Pipeline step 2.

Resolve context, clone it, and determine target directory in STATE."
  (let* ((parent (plist-get state :parent-buf))
         (dir (plist-get state :dir))
         (ctx (plist-get state :context))
         (resolved-ctx (or ctx
                           (when-let* ((is-live (buffer-live-p parent)))
                             (buffer-local-value 'macher-agent--persistent-context parent))
                           (ignore-errors (macher-agent-resolve-context dir))))
         (cloned-ctx (when-let* ((r-ctx resolved-ctx))
                       (if (fboundp 'macher-agent--clone-context)
                           (macher-agent--clone-context r-ctx)
                         r-ctx)))
         (target-dir (or dir
                         (when-let* ((c-ctx cloned-ctx))
                           (ignore-errors (macher-agent-context-root c-ctx)))
                         default-directory)))
    (plist-put (plist-put (plist-put state :resolved-ctx resolved-ctx)
                          :cloned-ctx cloned-ctx)
               :target-dir target-dir)))

(defun macher-agent-subagent-pipe--init-buffer (state)
  "Pipeline step 3: Create and initialize subagent buffer in STATE."
  (let* ((name (plist-get state :name))
         (target-dir (plist-get state :target-dir))
         (parent (plist-get state :parent-buf))
         (cloned-ctx (plist-get state :cloned-ctx))
         (presets (plist-get state :presets))
         (buf (get-buffer-create name)))

    (with-current-buffer buf
      (when-let* ((has-markdown (fboundp 'markdown-mode))
                  (needs-markdown (not (derived-mode-p 'markdown-mode))))
        (markdown-mode))

      (setq-local macher-agent--is-subagent t)
      (setq-local macher-agent--is-workspace t)
      (setq-local gptel-stream nil)
      (setq-local default-directory (file-name-as-directory target-dir))

      (when-let* ((has-gptel (fboundp 'gptel-mode))
                  (needs-gptel (not gptel-mode)))
        (gptel-mode 1))

      (when-let* ((is-live (buffer-live-p parent)))
        (setq-local gptel-model (buffer-local-value 'gptel-model parent))
        (setq-local gptel-backend (buffer-local-value 'gptel-backend parent))
        (setq-local gptel-temperature (buffer-local-value 'gptel-temperature parent))
        (setq-local gptel-max-tokens (buffer-local-value 'gptel-max-tokens parent))
        (setq-local
         macher-agent--suppress-patch
         (with-current-buffer parent (bound-and-true-p macher-agent--suppress-patch)))
        (setq-local
         macher-agent--boot-directive
         (with-current-buffer parent (bound-and-true-p macher-agent--boot-directive)))
        (if (boundp 'gptel--known-presets)
            (setq gptel--known-presets (buffer-local-value 'gptel--known-presets parent))
          (setq-local gptel--known-presets (buffer-local-value 'gptel--known-presets parent)))
        (if (boundp 'gptel-directives)
            (setq gptel-directives (buffer-local-value 'gptel-directives parent))
          (setq-local gptel-directives (buffer-local-value 'gptel-directives parent))))

      (when-let* ((c-ctx cloned-ctx))
        (setq-local macher-agent--persistent-context c-ctx))

      (when-let* ((p presets))
        (let ((preset-list (cond ((listp p) p)
                                 ((vectorp p) (append p nil))
                                 (t (list p)))))
          (setq-local macher-agent-presets preset-list)
          (macher-agent--apply-preset preset-list)))

      (add-hook 'gptel-prompt-transform-functions
                #'macher-agent-sync-prompt-transformer nil t)
      (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t)
      (add-hook 'gptel-post-response-functions #'macher-agent-post-response-reaper nil t))

    (plist-put state :target-buf buf)))

(defun macher-agent-subagent-pipe--register (state)
  "Pipeline step 4: Register subagent in tracking lists using STATE."
  (let* ((name (plist-get state :name))
         (target-dir (plist-get state :target-dir))
         (cloned-ctx (plist-get state :cloned-ctx)))

    (when-let* ((is-bound (boundp 'macher-agent-active-subagents)))
      (setq macher-agent-active-subagents
            (cons (cons name target-dir)
                  (cl-delete name macher-agent-active-subagents :key #'car :test #'equal))))

    (when-let* ((c-ctx cloned-ctx)
                (ws (ignore-errors (macher-agent--get-context-workspace c-ctx))))
      (let ((subs (ignore-errors (macher-agent-workspace-active-subagents ws))))
        (macher-agent--set-workspace-active-subagents
         ws
         (cons (cons name target-dir)
               (cl-delete name subs :key #'car :test #'equal)))))
    state))

(defun macher-agent-add-subagent (name &optional presets parent-buf dir context)
  "Interactively or programmatically create sub-agent buffer NAME.

NAME is the target sub-agent buffer name string.
PRESETS is optional preset specification, list, vector, or string.
PARENT-BUF is optional parent orchestrator buffer.
DIR is optional directory path string.
CONTEXT is optional VFS context structure.

Return the created sub-agent buffer object.

Side effects: Creates buffer, updates local state, registers in global
tracking lists."
  (interactive "sSub-agent name: ")
  (let ((initial-state (list :name name
                             :presets presets
                             :parent-buf parent-buf
                             :dir dir
                             :context context)))
    (plist-get (seq-reduce (lambda (state pipe-fn)
                             (funcall pipe-fn state))
                           macher-agent-subagent-pipeline-functions
                           initial-state)
               :target-buf)))

(defun macher-agent--apply-single-virtual-buffer (entry)
  "Apply a single VFS virtual edit ENTRY to a live Emacs buffer.

ENTRY is a VFS context entry structure or cell.

Return non-nil if applied to a live buffer, otherwise nil.

Side effects: Modifies the target live buffer contents."
  (when entry
    (let* ((path (if (fboundp 'macher-agent-vfs-entry-path)
                     (ignore-errors (macher-agent-vfs-entry-path entry))
                   (car-safe entry)))
           (curr (if (fboundp 'macher-agent-vfs-entry-curr)
                     (ignore-errors (macher-agent-vfs-entry-curr entry))
                   (cdr-safe entry)))
           (buf-name (when path (macher-agent--resolve-buffer-name path)))
           (buf (when buf-name (get-buffer buf-name))))
      (when (and buf (buffer-live-p buf) (stringp curr))
        (with-current-buffer buf
          (erase-buffer)
          (insert curr))
        t))))

(defun macher-agent-apply-virtual-buffers (&optional context)
  "Apply pending VFS virtual edits to live Emacs buffers.

CONTEXT is the optional VFS context structure.  If omitted, resolves
the active context.

Return nil.

Side effects: Modifies contents of live buffers matching VFS entries."
  (let* ((ctx (or context (ignore-errors (macher-agent-resolve-context))))
         (contents (when (and ctx (fboundp 'macher-agent--get-context-contents))
                     (ignore-errors (macher-agent--get-context-contents ctx)))))
    (dolist (entry contents)
      (macher-agent--apply-single-virtual-buffer entry))
    (when (fboundp 'macher-agent--auto-sync-context)
      (macher-agent--auto-sync-context ctx))))

(defun macher-agent-submit-task-result (result &optional context)
  "Submit RESULT as the completed artifact for current task."
  (setq-local macher-agent--final-result result)
  (let* ((frame (cond ((bound-and-true-p macher-agent--routing-stack)
                       (macher-agent--pop-routing))
                      (t (list :task-id (bound-and-true-p macher-agent--current-task-id)))))
         (task-id (plist-get frame :task-id))
         (a2a-cb (when task-id (gethash task-id macher-agent--pending-callbacks)))
         (msg-body (list :status 'success :data result :buffer-name (buffer-name))))
    (when context
      (setq msg-body (plist-put msg-body :child-context context)))
    (setq msg-body (macher-agent-run-pipeline 'artifact-compose msg-body))

    (if a2a-cb
        (funcall a2a-cb (list :type 'ARTIFACT_UPDATE :task-id task-id :message msg-body))
      (display-warning 'macher-agent (format "No callback registered for task-id '%s'." task-id) :warning))))

(provide 'macher-agent-orchestration)
;;; macher-agent-orchestration.el ends here
