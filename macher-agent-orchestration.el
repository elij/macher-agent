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
(require 'macher-agent-gptel)
(require 'macher-agent-tools)
(require 'macher-agent-presets)

(defvar gptel--known-presets)
(defvar gptel-directives)

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

(defun macher-agent-a2a--extract-parent-buffer (shared)
  "Extract parent buffer from SHARED state structure enforcing strict key."
  (when shared (plist-get shared :parent-buffer)))

(defun macher-agent-a2a--extract-parent-context (shared &optional parent-buf)
  "Extract parent context enforcing strict property lookups."
  (when shared
    (or (let ((ctx (plist-get shared :parent-context)))
          (when (macher-agent-valid-context-p ctx)
            ctx))
        (let ((p-buf (or parent-buf (macher-agent-a2a--extract-parent-buffer shared))))
          (when (and p-buf (buffer-live-p p-buf))
            (let ((p-ctx (buffer-local-value 'macher-agent--persistent-context p-buf)))
              (when (macher-agent-valid-context-p p-ctx)
                p-ctx)))))))

(defvar macher-agent-a2a-pipeline-functions
  '(macher-agent-a2a-pipe--validate-routing
    macher-agent-a2a-pipe--acquire-target
    macher-agent-a2a-pipe--register-ownership
    macher-agent-a2a-pipe--route-instructions
    macher-agent-a2a-pipe--bind-closure
    macher-agent-a2a-pipe--transmit)
  "Chained reducer pipeline to construct and execute an A2A sub-agent.")

(defun macher-agent-a2a-dispatch (send-message-payloads final-callback &optional parent-ctx-override)
  "Dispatch pre-normalized A2A SEND-MESSAGE-PAYLOADS."
  (when final-callback
    (cl-check-type final-callback function))
  (let* ((parent-ctx (when (macher-agent-valid-context-p parent-ctx-override)
                       parent-ctx-override))
         (total (length send-message-payloads))
         (shared-state (list :results (make-hash-table :test 'equal)
                             :total total
                             :final-callback final-callback
                             :parent-buffer (current-buffer)
                             :parent-context parent-ctx
                             :original-payloads send-message-payloads)))
    (if (= total 0)
        (when final-callback
          (funcall final-callback []))
      (dolist (msg send-message-payloads)
        (unless (macher-agent-transit-payload-task-id msg)
          (error "STRICT CONTRACT VIOLATION: Payload missing required task-id"))
        (if (eq (macher-agent-transit-payload-type msg) 'ARTIFACT_UPDATE)
            (let* ((tid (macher-agent-transit-payload-task-id msg))
                   (cb (gethash tid macher-agent--pending-callbacks)))
              (if (functionp cb)
                  (progn
                    (remhash tid macher-agent--pending-callbacks)
                    (funcall cb msg))
                (display-warning 'macher-agent (format "No pending callback for ARTIFACT_UPDATE with task-id '%s'" tid) :warning)))
          (let ((initial-state (list :a2a-msg msg
                                     :shared-state shared-state
                                     :child-buf nil)))
            (seq-reduce (lambda (state pipe-fn)
                          (if (functionp pipe-fn) (funcall pipe-fn state) state))
                        macher-agent-a2a-pipeline-functions
                        initial-state)))))))

(defun macher-agent--init-subagent-state (buf task-id meta parent-ctx)
  "Initialize BUF state strictly mapped to parent-ctx."
  (cl-check-type parent-ctx macher-agent-context)
  (with-current-buffer buf
    (setq-local macher-agent--current-task-id task-id)
    (when meta
      (cl-loop for (k v) on meta by #'cddr
               do (pcase k
                    (:background (setq-local macher-agent--is-background v))
                    (:ephemeral (setq-local macher-agent--is-ephemeral v))
                    (:suppress-patch (setq-local macher-agent--suppress-patch v))
                    (:presets (setq-local macher-agent-presets
                                          (if (vectorp v) (append v nil) (ensure-list v)))))))
    (when (plist-get meta :background)
      (setq-local macher-agent--ready-to-reap nil))
    (setq-local macher-agent--persistent-context
                (if (fboundp 'macher-agent--clone-context)
                    (macher-agent--clone-context parent-ctx)
                  (macher-agent--copy-context parent-ctx)))
    (unless (plist-get meta :background)
      (macher-agent-ui-show buf))))

(defun macher-agent-a2a-pipe--validate-routing (state)
  "Validate message routing paths and prevent circular communication loops.

Extract task identifiers and metadata from the input state. Perform full
resolution of the canonical buffer names for the originator and the
intended target. Provide strict interception of requests where an agent
attempts to route a payload to its own buffer, returning an error structure
to halt execution. Uses structured pattern matching to validate schema.

STATE is the property list containing the active message payload and
shared execution properties.

Return the updated STATE property list containing derived target names
or an error payload.

Side effects: None."
  (let* ((msg (plist-get state :a2a-msg))
         (task-id (macher-agent-transit-payload-task-id msg))
         (meta (macher-agent-transit-payload-metadata msg))
         (buf-name (macher-agent--resolve-buffer-name (plist-get meta :buffer_name)))
         (shared (plist-get state :shared-state))
         (parent-buf (plist-get shared :parent-buffer))
         (originator-name (macher-agent--resolve-buffer-name parent-buf)))
    (setq state (plist-put state :target-name buf-name))
    (setq state (plist-put state :originator-name originator-name))
    (if (and buf-name originator-name (equal buf-name originator-name))
        (plist-put state :error-payload
                   (list :status 'error :error "Circular routing detected" :task-id task-id))
      state)))

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
           (meta (macher-agent-transit-payload-metadata msg))
           (task-id (macher-agent-transit-payload-task-id msg))
           (raw-buf (when meta (plist-get meta :buffer_name)))
           (presets (when meta (plist-get meta :presets)))
           (buf-name (plist-get state :target-name))
           (shared (plist-get state :shared-state))
           (parent-buf (macher-agent-a2a--extract-parent-buffer shared))
           (parent-ctx (or (macher-agent-transit-payload-parent-context msg)
                           (macher-agent-transit-payload-target-context msg)
                           (macher-agent-a2a--extract-parent-context shared parent-buf)))
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
           (task-id (macher-agent-transit-payload-task-id msg))
           (originator-name (plist-get state :originator-name))
           (child-buf (plist-get state :child-buf))
           (child-name (buffer-name child-buf))
           (shared (plist-get state :shared-state))
           (parent-buf (macher-agent-a2a--extract-parent-buffer shared)))

      (when task-id
        (puthash task-id originator-name macher-agent--task-registry))

      (let* ((ws-root (or (when (and parent-buf (buffer-live-p parent-buf))
                            (let ((pctx (buffer-local-value 'macher-agent--persistent-context parent-buf)))
                              (when (macher-agent-context-p pctx)
                                (or (macher-agent-context-project-root pctx)
                                    (macher-agent-context-root pctx)))))
                          (when (and child-buf (buffer-live-p child-buf))
                            (let ((cctx (buffer-local-value 'macher-agent--persistent-context child-buf)))
                              (when (macher-agent-context-p cctx)
                                (or (macher-agent-context-project-root cctx)
                                    (macher-agent-context-root cctx)))))))
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
  (let* ((msg (plist-get state :a2a-msg))
         (raw-task-id (or (when (macher-agent-transit-payload-p msg)
                            (macher-agent-transit-payload-task-id msg))
                          (when (listp msg) (plist-get msg :task-id))
                          (bound-and-true-p macher-agent--current-task-id)
                          (macher-agent--generate-uuid)))
         (task-id (if (or (null raw-task-id)
                          (and (stringp raw-task-id) (string-empty-p raw-task-id))
                          (gethash raw-task-id macher-agent--pending-callbacks))
                      (let ((gen (macher-agent--generate-uuid)))
                        (unless (string-prefix-p "task-" gen)
                          (setq gen (format "task-%s" gen)))
                        (while (gethash gen macher-agent--pending-callbacks)
                          (setq gen (macher-agent--generate-uuid))
                          (unless (string-prefix-p "task-" gen)
                            (setq gen (format "task-%s" gen))))
                        gen)
                    raw-task-id))
         (shared (plist-get state :shared-state))
         (results (plist-get shared :results))
         (parent-buf (macher-agent-a2a--extract-parent-buffer shared))
         (total (or (plist-get shared :total) 1))
         (final-callback (plist-get shared :final-callback))
         (original-payloads (plist-get shared :original-payloads))
         (child-buf (plist-get state :child-buf))
         (msg-payload (if (macher-agent-transit-payload-p msg)
                          (macher-agent-transit-payload-payload msg)
                        (plist-get msg :payload)))
         (instructions (if (listp msg-payload) (plist-get msg-payload :instructions) msg-payload))
         (meta (if (macher-agent-transit-payload-p msg)
                   (macher-agent-transit-payload-metadata msg)
                 (plist-get msg :metadata)))
         (is-ephem (when meta (plist-get meta :ephemeral)))
         (wake-cb (or (plist-get state :wake-cb)
                      (and child-buf (buffer-live-p child-buf)
                           (gethash (buffer-name child-buf) macher-agent--pending-callbacks))))
         (err-payload (plist-get state :error-payload)))

    (when (macher-agent-transit-payload-p msg)
      (unless (equal task-id (macher-agent-transit-payload-task-id msg))
        (setf (macher-agent-transit-payload-task-id msg) task-id)
        (setq state (plist-put state :a2a-msg msg))))

    (if err-payload
        (macher-agent--aggregate-a2a-results task-id err-payload results total original-payloads final-callback parent-buf)
      (if wake-cb
          (progn
            (when (and child-buf (buffer-live-p child-buf))
              (remhash (buffer-name child-buf) macher-agent--pending-callbacks))
            (setq state (plist-put state :wake-cb wake-cb))
            (setq state (plist-put state :wake-msg instructions)))
        (when (and child-buf (buffer-live-p child-buf))
          (with-current-buffer child-buf
            (when is-ephem (setq-local macher-agent--is-ephemeral t))
            (goto-char (point-max))
            (insert (or instructions "") "\n\n")))))
    state))

(defun macher-agent--aggregate-a2a-results (task-id msg-body results total original-payloads final-callback parent-buf)
  "Aggregate results and trigger FINAL-CALLBACK against live PARENT-BUF."
  (cl-check-type parent-buf buffer)
  (when final-callback
    (cl-check-type final-callback function))
  (puthash task-id msg-body results)
  (when (and (= (hash-table-count results) total)
             (not (gethash '*completed* results)))
    (let ((ordered-results
           (mapcar (lambda (p)
                     (let ((p-tid (or (when (macher-agent-transit-payload-p p)
                                        (macher-agent-transit-payload-task-id p))
                                      task-id)))
                       (gethash p-tid results)))
                   original-payloads)))
      (puthash '*completed* t results)
      (when final-callback
        (with-current-buffer parent-buf
          (funcall final-callback (vconcat ordered-results)))))))

(defun macher-agent-a2a-pipe--bind-closure (state)
  "Bind the remote procedure closure and manage result aggregation.

Construct a lexical closure designed for asynchronous return trip
management. The closure provides safe application of virtual file system
differences to the orchestrator environment and performs steady aggregation
of incoming payloads.

The `suppress-patch' metadata flag applies strictly to interactive
diffs in the UI. It is not used to gate the payload merge in this closure.

STATE is the property list containing the active message, shared task
totals, and target buffer.

Return the updated STATE property list containing the compiled callback
closure.

Side effects: Updates the pending callbacks hash table and modifies the
routing stack of the child buffer."
  (if (or (plist-get state :error-payload)
          (plist-get state :wake-cb))
      state
    (let* ((msg (plist-get state :a2a-msg))
           (raw-task-id (or (when (macher-agent-transit-payload-p msg)
                              (macher-agent-transit-payload-task-id msg))
                            (when (listp msg) (plist-get msg :task-id))
                            (bound-and-true-p macher-agent--current-task-id)
                            (macher-agent--generate-uuid)))
           (task-id (if (or (null raw-task-id)
                            (and (stringp raw-task-id) (string-empty-p raw-task-id))
                            (gethash raw-task-id macher-agent--pending-callbacks))
                        (let ((gen (macher-agent--generate-uuid)))
                          (unless (string-prefix-p "task-" gen)
                            (setq gen (format "task-%s" gen)))
                          (while (gethash gen macher-agent--pending-callbacks)
                            (setq gen (macher-agent--generate-uuid))
                            (unless (string-prefix-p "task-" gen)
                              (setq gen (format "task-%s" gen))))
                          gen)
                      raw-task-id))
           (shared (plist-get state :shared-state))
           (parent-buf (macher-agent-a2a--extract-parent-buffer shared))
           (parent-name (or (plist-get state :originator-name)
                            (when (and parent-buf (buffer-live-p parent-buf))
                              (buffer-name parent-buf))))
           (meta (if (macher-agent-transit-payload-p msg)
                     (macher-agent-transit-payload-metadata msg)
                   (plist-get msg :metadata)))
           (suppress-patch (when meta (plist-get meta :suppress-patch)))
           (is-ephem (when meta (plist-get meta :ephemeral)))
           (child-buf (plist-get state :child-buf))
           (results (plist-get shared :results))
           (total (or (plist-get shared :total) 1))
           (final-callback (plist-get shared :final-callback))
           (original-payloads (plist-get shared :original-payloads))
           (wake-cb (and child-buf (buffer-live-p child-buf)
                         (gethash (buffer-name child-buf) macher-agent--pending-callbacks))))

      (if wake-cb
          (progn
            (remhash (buffer-name child-buf) macher-agent--pending-callbacks)
            (setq state (plist-put state :wake-cb wake-cb))
            (let* ((msg-payload (if (macher-agent-transit-payload-p msg)
                                    (macher-agent-transit-payload-payload msg)
                                  (plist-get msg :payload)))
                   (instructions (if (listp msg-payload) (plist-get msg-payload :instructions) msg-payload)))
              (setq state (plist-put state :wake-msg instructions))))

        (when (macher-agent-transit-payload-p msg)
          (unless (equal task-id (macher-agent-transit-payload-task-id msg))
            (setf (macher-agent-transit-payload-task-id msg) task-id)
            (setq state (plist-put state :a2a-msg msg))))

        (when (and child-buf (buffer-live-p child-buf))
          (with-current-buffer child-buf
            (when is-ephem (setq-local macher-agent--is-ephemeral t))
            (let* ((called nil)
                   (a2a-cb
                    (lambda (artifact-payload)
                      (unless called
                        (setq called t)
                        (remhash task-id macher-agent--pending-callbacks)

                        (let* ((p (when (macher-agent-transit-payload-p artifact-payload) artifact-payload))
                               (msg-body (if (and p (eq (macher-agent-transit-payload-type p) 'ARTIFACT_UPDATE))
                                             (macher-agent-transit-payload-payload p)
                                           artifact-payload))
                               (actual-task-id (or (when p (macher-agent-transit-payload-task-id p)) task-id)))
                          (when (and (macher-agent--plist-p msg-body) (not (plist-get msg-body :shared-state)))
                            (setq msg-body (plist-put (copy-sequence msg-body) :shared-state shared)))

                          (when (and parent-buf (buffer-live-p parent-buf))
                            (with-current-buffer parent-buf
                              (let ((parent-ctx (bound-and-true-p macher-agent--persistent-context)))
                                (when parent-ctx
                                  (setq-local macher-agent--persistent-context parent-ctx)
                                  (if p
                                      (setf (macher-agent-transit-payload-target-context p) parent-ctx)
                                    (when (macher-agent--plist-p artifact-payload)
                                      (setq artifact-payload (plist-put (copy-sequence artifact-payload) :target-context parent-ctx))))))

                              (when (fboundp 'macher-agent-run-pipeline)
                                (setq artifact-payload (macher-agent-run-pipeline 'payload-merge artifact-payload)))

                              (when-let* ((merged-ctx (if (macher-agent-transit-payload-p artifact-payload)
                                                          (macher-agent-transit-payload-target-context artifact-payload)
                                                        (when (macher-agent--plist-p artifact-payload)
                                                          (plist-get artifact-payload :target-context)))))
                                (setq-local macher-agent--persistent-context merged-ctx))))

                          (macher-agent--aggregate-a2a-results actual-task-id msg-body results total original-payloads final-callback parent-buf))))))
              (puthash task-id a2a-cb macher-agent--pending-callbacks)
              (macher-agent--push-routing task-id parent-name suppress-patch)
              (setq state (plist-put state :a2a-cb a2a-cb))))))
      state)))

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
        (when (functionp wake-cb)
          (funcall wake-cb (plist-get state :wake-msg)))
      (when-let* ((child-buf (plist-get state :child-buf)))
        (when (buffer-live-p child-buf)
          (with-current-buffer child-buf
            (setq-local macher-agent--active-fsm nil)
            (setq-local macher-agent-task-finished nil)

            (let* ((msg (plist-get state :a2a-msg))
                   (task-id (or (macher-agent-transit-payload-task-id msg)
                                (bound-and-true-p macher-agent--current-task-id)))
                   (a2a-cb (or (and task-id (gethash task-id macher-agent--pending-callbacks))
                               (plist-get state :a2a-cb)))
                   (target-name (buffer-name child-buf))
                   (callbacks
                    (list :success-cb
                          (lambda (res)
                            (let ((cb (and task-id (gethash task-id macher-agent--pending-callbacks))))
                              (when (and cb (functionp cb))
                                (remhash task-id macher-agent--pending-callbacks)
                                (funcall cb (list :status 'success
                                                  :data res
                                                  :task-id task-id
                                                  :buffer-name target-name)))))
                          :on-success
                          (lambda (res)
                            (let ((cb (and task-id (gethash task-id macher-agent--pending-callbacks))))
                              (when (and cb (functionp cb))
                                (remhash task-id macher-agent--pending-callbacks)
                                (funcall cb (list :status 'success
                                                  :data res
                                                  :task-id task-id
                                                  :buffer-name target-name)))))
                          :error-cb
                          (lambda (err)
                            (let ((cb (or (and task-id (gethash task-id macher-agent--pending-callbacks)) a2a-cb)))
                              (when (and cb (functionp cb))
                                (when task-id (remhash task-id macher-agent--pending-callbacks))
                                (funcall cb (list :status 'error
                                                  :error err
                                                  :task-id task-id
                                                  :buffer-name target-name)))))
                          :on-error
                          (lambda (err)
                            (let ((cb (or (and task-id (gethash task-id macher-agent--pending-callbacks)) a2a-cb)))
                              (when (and cb (functionp cb))
                                (when task-id (remhash task-id macher-agent--pending-callbacks))
                                (funcall cb (list :status 'error
                                                  :error err
                                                  :task-id task-id
                                                  :buffer-name target-name))))))))
              (let ((ctx (make-macher-agent-task-context :target-buffer (current-buffer))))
                (macher-agent-gptel-transmit ctx callbacks))))))))
  state)

(defun macher-agent--resolve-buffer-name (buffer-or-name)
  "Resolve BUFFER-OR-NAME into a canonical buffer name string.
Safely accommodates strings, live buffer objects, and symbols."
  (cond
   ((stringp buffer-or-name) buffer-or-name)
   ((bufferp buffer-or-name) (buffer-name buffer-or-name))
   ((symbolp buffer-or-name) (symbol-name buffer-or-name))
   (t (signal 'wrong-type-argument (list '(or string buffer symbol) buffer-or-name)))))

(defun macher-agent--reap-buffer (buf)
  "Execute reap exclusively on live BUF object."
  (cl-check-type buf buffer)
  (when (and (buffer-live-p buf)
             (with-current-buffer buf
               (and (macher-agent-ready-to-reap-p)
                    (or (bound-and-true-p macher-agent-task-finished)
                        (bound-and-true-p macher-agent--ready-to-reap)))))
    (let ((name (buffer-name buf)))
      (macher-agent--remove-active-subagent-registries buf)
      (with-current-buffer buf
        (set-buffer-modified-p nil)
        (ignore-errors (macher-agent-bridge-abort buf))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(defun macher-agent--compose-merge-system-prompt (current-sys sym sys-spec)
  "Merge SYS-SPEC into CURRENT-SYS for skill SYM.

Accommodates gptel native string prompts, modifier functions, and list formats.
CURRENT-SYS is the accumulator string or nil.
SYM is the skill symbol or string identifier (or a gptel preset spec plist).
SYS-SPEC is a string, conversation list, prompt modifier function, or nil.

Return the merged prompt string.
Side effects: None."
  ;; Safely normalize SYM if it's passed as a plist or spec list instead of a symbol/string
  (let ((normalized-sym (cond
                         ((symbolp sym) (symbol-name sym))
                         ((stringp sym) sym)
                         ((consp sym)
                          (or (plist-get sym :name)
                              (when (symbolp (car sym)) (symbol-name (car sym)))
                              "preset"))
                         (t "preset"))))

    (when (and (consp sys-spec) (eq (car sys-spec) :function) (cadr sys-spec))
      (setq sys-spec (cadr sys-spec)))

    (unless (or (stringp sys-spec)
                (functionp sys-spec)
                (and (consp sys-spec) (not (keywordp (car sys-spec))))
                (null sys-spec))
      (signal 'wrong-type-argument `((or string function cons null) ,sys-spec)))

    (let* ((cur (cond
                 ((stringp current-sys) current-sys)
                 ((listp current-sys) (string-join (delq nil current-sys) "\n---\n"))
                 (t "")))
           (spec-str
            (cond
             ((functionp sys-spec)
              (let ((res (funcall sys-spec cur)))
                (if (stringp res) res (format "%s" res))))
             ((consp sys-spec)
              (string-join (delq nil (mapcar (lambda (x) (if (stringp x) x (format "%s" x))) sys-spec)) "\n"))
             ((stringp sys-spec) sys-spec)
             (t "")))
           (clean-spec (string-trim spec-str)))
      (if (or (string-empty-p clean-spec)
              (string-match-p (regexp-quote clean-spec) cur))
          cur
        (concat cur
                (if (string-empty-p cur) "" "\n---\n")
                (format "### Skill: %s\n%s\n" normalized-sym clean-spec))))))

(defun macher-agent--resolve-single-tool-object (tool)
  "Resolve valid TOOL object directly."
  (cl-check-type tool gptel-tool)
  tool)

(defun macher-agent--compose-merge-tools (current-tools tools-spec)
  "Merge TOOLS-SPEC into CURRENT-TOOLS without polluting global registries."
  (let ((merged-tools (gptel--modify-value current-tools tools-spec))
        (make-anon-tool (lambda (plist)
                          (if (fboundp 'gptel--make-tool)
                              (apply #'gptel--make-tool plist)
                            (apply #'gptel-make-tool plist)))))
    (cond
     ((null merged-tools) nil)
     ((and (fboundp 'gptel-tool-p) (gptel-tool-p merged-tools))
      (list merged-tools))
     ((and (consp merged-tools) (keywordp (car merged-tools)))
      (list (funcall make-anon-tool merged-tools)))
     ((and (listp merged-tools)
           (not (and (consp merged-tools) (keywordp (car merged-tools))))
           (let ((single (ignore-errors (gptel-get-tool merged-tools))))
             (and single (fboundp 'gptel-tool-p) (gptel-tool-p single))))
      (let ((single (ignore-errors (gptel-get-tool merged-tools))))
        (if single (list single) nil)))
     ((listp merged-tools)
      (cl-loop for t-obj in merged-tools
               append (let* ((res (if (and (fboundp 'gptel-tool-p) (gptel-tool-p t-obj))
                                      t-obj
                                    (ignore-errors (gptel-get-tool t-obj)))))
                        (cond
                         ((and (fboundp 'gptel-tool-p) (gptel-tool-p res))
                          (list res))
                         ((and (consp t-obj) (keywordp (car t-obj)))
                          (list (funcall make-anon-tool t-obj)))
                         ((and (listp res) (not (and (fboundp 'gptel-tool-p) (gptel-tool-p res))))
                          res)
                         (t
                          (list (or res t-obj)))))))
     (t
      (list (if (and (fboundp 'gptel-tool-p) (gptel-tool-p merged-tools))
                merged-tools
              (or (ignore-errors (gptel-get-tool merged-tools)) merged-tools)))))))

(defun macher-agent--resolve-preset-or-tool (sym known)
  "Resolve explicitly evaluated SYM struct against known registry.

SYM is a preset symbol, tool object, keyword plist, or nil.
KNOWN is the association list of known presets.

Return a cons cell `(preset . SPEC)' or `(tool . TOOL)', or nil.
Side effects: None."
  (when sym
    (cond
     ((and (fboundp 'gptel-tool-p) (gptel-tool-p sym))
      (cl-check-type sym gptel-tool)
      (cons 'tool sym))
     ((and (consp sym) (keywordp (car sym)))
      (if (or (plist-member sym :function) (plist-member sym :args))
          (cons 'tool sym)
        (cons 'preset sym)))
     (t
      (let* ((str (if (symbolp sym) (symbol-name sym) (format "%s" sym)))
             (clean-str (replace-regexp-in-string "^@+" "" str))
             (clean-sym (intern clean-str)))
        (if-let*
            ((spec (or (alist-get clean-sym known)
                       (when-let* ((ctx (bound-and-true-p macher-agent--persistent-context)))
                         (when (fboundp 'macher-agent-workspace-skills-alist)
                           (ignore-errors (alist-get clean-sym (macher-agent-workspace-skills-alist ctx)))))
                       (alist-get clean-sym (bound-and-true-p macher-agent-global-skills-alist)))))
            (cons 'preset spec)
          (when-let*
              ((tool
                (when (fboundp 'gptel-get-tool)
                  (or (ignore-errors (gptel-get-tool clean-str))
                      (ignore-errors (gptel-get-tool (replace-regexp-in-string "-" "_" clean-str)))
                      (ignore-errors (gptel-get-tool (replace-regexp-in-string "_" "-" clean-str)))))))
            (cons 'tool tool))))))))

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
  "Merge allowed tools specification or standalone tool in ITEM into STATE."
  (pcase item
    (`(preset ,_sym ,spec)
     (if-let* ((tools-spec (or (plist-get spec :tools)
                               (plist-get spec :allowed-tools))))
         (plist-put state :tools
                    (macher-agent--compose-merge-tools
                     (plist-get state :tools) tools-spec))
       state))
    (`(tool ,tool-obj)
     (let* ((canonical-tool
             (cond
              ((and (fboundp 'gptel-tool-p) (gptel-tool-p tool-obj)) tool-obj)
              ((stringp tool-obj) (ignore-errors (gptel-get-tool tool-obj)))
              ((symbolp tool-obj) (ignore-errors (gptel-get-tool (symbol-name tool-obj))))
              ((consp tool-obj) (ignore-errors (gptel-get-tool (plist-get tool-obj :name))))
              (t nil))))
       (if canonical-tool
           (plist-put state :tools
                      (macher-agent--compose-merge-tools
                       (plist-get state :tools)
                       (list :append canonical-tool)))
         state)))
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
  "Pre-flatten INLINE-PRESETS and their parent preset dependencies."
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
                (display-warning 'macher-agent
                                 (format "Unknown or unresolvable preset/tool: %s" sym)
                                 :warning)))))))
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
                          (if (functionp pipe-fn)
                              (funcall pipe-fn s item)
                            s))
                        static-steps
                        :initial-value st))
           flattened
           :initial-value state))
    (when dynamic-steps
      (setq state
            (seq-reduce (lambda (st dyn-fn)
                          (if (functionp dyn-fn)
                              (funcall dyn-fn st)
                            st))
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
  "Apply explicitly validated PRESET-OR-PRESETS list.

PRESET-OR-PRESETS is a preset symbol or list of preset symbols.

Return nil.
Side effects: Applies composed configuration to buffer-local variables."
  (let* ((presets (if (listp preset-or-presets) preset-or-presets (list preset-or-presets)))
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
                         (when-let* ((ctx (bound-and-true-p macher-agent--persistent-context)))
                           (when (fboundp 'macher-agent-workspace-skills-alist)
                             (alist-get primary-sym (macher-agent-workspace-skills-alist ctx))))
                         (alist-get primary-sym (bound-and-true-p macher-agent-global-skills-alist)))))
             (raw-sys (or (plist-get spec :system) (plist-get spec :system-message)))
             (boot-dir (plist-get spec :boot-directive)))
        (when (stringp raw-sys)
          (setq payload (plist-put payload :system raw-sys)))
        (when boot-dir
          (setq payload (plist-put payload :boot-directive boot-dir)))))
    (macher-agent--apply-payload-locally payload)))

(defun macher-agent-use-skill (skill &optional buf)
  "Attach specific SKILL to live BUF object.

SKILL is a skill symbol or list of skill symbols.
BUF is the target buffer, defaulting to `current-buffer'.

Return nil.
Side effects: Sets `macher-agent-presets' and applies preset locally."
  (interactive "sSkill: ")
  (let ((target-buf (or buf (current-buffer)))
        (skill-list (ensure-list skill)))
    (cl-check-type target-buf buffer)
    (when (buffer-live-p target-buf)
      (with-current-buffer target-buf
        (setq-local macher-agent-presets skill-list)
        (macher-agent--apply-preset skill-list)))))

(defun macher-agent--extract-first-preset-symbol (resolved-presets)
  "Extract lead symbol explicitly from RESOLVED-PRESETS list."
  (cl-check-type resolved-presets list)
  (let ((first (car resolved-presets)))
    (cond
     ((symbolp first) first)
     ((stringp first) (intern first))
     (t nil))))

(defvar macher-agent-subagent-pipeline-functions
  '(macher-agent-subagent-pipe--normalize-args
    macher-agent-subagent-pipe--resolve-context
    macher-agent-subagent-pipe--init-buffer
    macher-agent-subagent-pipe--register)
  "Store chained reducer pipeline to construct and initialize a subagent buffer.")

(defun macher-agent-subagent-pipe--normalize-args (state)
  "Pipeline step 1: Normalise overloaded arguments in STATE."
  (let ((presets (plist-get state :presets))
        (parent (plist-get state :parent-buffer))
        (dir (plist-get state :dir))
        (ctx (plist-get state :context)))
    (when-let* ((is-path-string (and (stringp presets)
                                     (or (file-directory-p presets)
                                         (string-prefix-p "/" presets)
                                         (string-suffix-p "/" presets)))))
      (unless dir (setq dir presets))
      (if-let* ((ctx-is-list (listp ctx))
                (ctx-not-obj (not (macher-agent-context-p ctx))))
          (progn (setq presets ctx) (setq ctx nil))
        (setq presets nil)))
    (when (macher-agent-context-p presets)
      (setq ctx presets) (setq presets nil))
    (when (macher-agent-context-p dir)
      (setq ctx dir)
      (setq dir (if-let* ((is-str (stringp presets))
                          (not-eq (not (equal presets ctx))))
                    presets nil)))
    (when (macher-agent-context-p parent)
      (setq ctx parent) (setq parent nil))
    (unless parent (setq parent (current-buffer)))
    (plist-put (plist-put (plist-put (plist-put state :presets presets)
                                     :parent-buffer parent)
                          :dir dir)
               :context ctx)))

(defun macher-agent-subagent-pipe--resolve-context (state)
  "Pipeline step 2.

Resolve context, clone it, and determine target directory in STATE."
  (let* ((parent (plist-get state :parent-buffer))
         (dir (plist-get state :dir))
         (ctx (plist-get state :context))
         (resolved-ctx (or (when (or (macher-agent-context-p ctx) (macher-agent-valid-context-p ctx)) ctx)
                           (when parent (macher-agent-context-from-buffer parent))
                           (when (and dir (boundp 'macher-agent-active-workspaces) (hash-table-p macher-agent-active-workspaces))
                             (gethash (expand-file-name dir) macher-agent-active-workspaces))))
         (cloned-ctx (when-let* ((r-ctx resolved-ctx))
                       (if (fboundp 'macher-agent--clone-context)
                           (macher-agent--clone-context r-ctx)
                         (macher-agent--copy-context r-ctx))))
         (target-dir (or dir
                         (when-let* ((c-ctx cloned-ctx))
                           (or (macher-agent-context-project-root c-ctx)
                               (macher-agent-context-root c-ctx)))
                         default-directory)))
    (plist-put (plist-put (plist-put state :resolved-ctx resolved-ctx)
                          :cloned-ctx cloned-ctx)
               :target-dir target-dir)))

(defun macher-agent-subagent-pipe--init-buffer (state)
  "Pipeline step 3: Create and initialize subagent buffer in STATE."
  (let* ((name (plist-get state :name))
         (target-dir (plist-get state :target-dir))
         (parent (plist-get state :parent-buffer))
         (cloned-ctx (plist-get state :cloned-ctx))
         (presets (plist-get state :presets))
         (buf (get-buffer-create name)))

    (with-current-buffer buf
      (when-let* ((has-markdown (fboundp 'markdown-mode))
                  (needs-markdown (not (derived-mode-p 'markdown-mode))))
        (markdown-mode))

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
        (setq-local gptel--known-presets (buffer-local-value 'gptel--known-presets parent))
        (setq-local gptel-directives (buffer-local-value 'gptel-directives parent)))

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
  "Pipeline step 3: Register subagent using unified tracking."
  (let* ((name (plist-get state :name))
         (target-dir (plist-get state :target-dir))
         (cloned-ctx (plist-get state :cloned-ctx))
         (parent-buf (plist-get state :parent-buffer))
         (parent-ctx (when (and parent-buf (buffer-live-p parent-buf))
                       (buffer-local-value 'macher-agent--persistent-context parent-buf))))
    (macher-agent--register-child-process name target-dir parent-ctx cloned-ctx)
    state))

(defun macher-agent-add-subagent (name &optional presets parent-buf dir context)
  "Interactively or programmatically create sub-agent buffer NAME.

NAME is the target sub-agent buffer name string.
PRESETS is optional preset specification, list, vector, or string.
PARENT-BUF is optional parent orchestrator buffer.
DIR is optional directory path string.
CONTEXT is optional context structure.

Return the created sub-agent buffer object.

Side effects: Creates buffer, updates local state, registers in global
tracking lists."
  (interactive "sSub-agent name: ")
  (let ((initial-state (list :name name
                             :presets presets
                             :parent-buffer parent-buf
                             :dir dir
                             :context context)))
    (plist-get (seq-reduce (lambda (state pipe-fn)
                             (if (functionp pipe-fn)
                                 (funcall pipe-fn state)
                               state))
                           macher-agent-subagent-pipeline-functions
                           initial-state)
               :target-buf)))

(defun macher-agent--register-child-process (name target-dir parent-ctx cloned-ctx)
  "Register child PROCESS bound strictly to specific context structs."
  (cl-check-type cloned-ctx macher-agent-context)
  (cl-check-type parent-ctx macher-agent-context)
  (when (boundp 'macher-agent-active-subagents)
    (setq macher-agent-active-subagents
          (cons (cons name target-dir)
                (cl-delete name macher-agent-active-subagents :key (lambda (e) (if (consp e) (car e) e)) :test #'equal))))
  (setf (macher-agent-context-subagents cloned-ctx)
        (cons (cons name target-dir) (cl-delete name (macher-agent-context-subagents cloned-ctx) :key (lambda (e) (if (consp e) (car e) e)) :test #'equal)))
  (setf (macher-agent-context-subagents parent-ctx)
        (cons (cons name target-dir) (cl-delete name (macher-agent-context-subagents parent-ctx) :key (lambda (e) (if (consp e) (car e) e)) :test #'equal))))

(provide 'macher-agent-orchestration)
;;; macher-agent-orchestration.el ends here
