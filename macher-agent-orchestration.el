;;; macher-agent-orchestration.el --- Orchestration -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive orchestration commands and preset composition for Macher Agent.

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
(defvar gptel--known-tools)
(defvar gptel-directives)

(defvar macher-agent-submit-task-result-tool nil
  "Store the tool object for task result submission.")

(defvar macher-agent--task-registry (make-hash-table :test 'equal)
  "Global registry mapping Task IDs to their originating buffer names.")

(defun macher-agent--generate-uuid ()
  "Generate a unique task identifier string."
  (let ((id (if (fboundp 'org-id-uuid)
                (org-id-uuid)
              (format "%04x%04x-%04x-%04x"
                      (random #xffff) (random #xffff) (random #xffff) (random #xffff)))))
    (if (string-prefix-p "task-" id)
        id
      (format "task-%s" id))))

(defun macher-agent--resolve-unique-task-id (raw-task-id)
  "Ensure RAW-TASK-ID is non-empty and does not collide with active task callbacks."
  (if (or (null raw-task-id)
          (and (stringp raw-task-id) (string-empty-p raw-task-id))
          (macher-agent-get-task-callback raw-task-id))
      (let ((gen (macher-agent--generate-uuid)))
        (while (macher-agent-get-task-callback gen)
          (setq gen (macher-agent--generate-uuid)))
        gen)
    raw-task-id))

(defun macher-agent--resolve-buffer-name (buffer-or-name)
  "Resolve BUFFER-OR-NAME into a canonical buffer name string."
  (cond
   ((stringp buffer-or-name) buffer-or-name)
   ((bufferp buffer-or-name) (buffer-name buffer-or-name))
   ((symbolp buffer-or-name) (symbol-name buffer-or-name))
   (t (signal 'wrong-type-argument (list '(or string buffer symbol) buffer-or-name)))))

(defun macher-agent-a2a--extract-parent-buffer (shared)
  "Extract parent buffer from SHARED state structure."
  (when shared (plist-get shared :parent-buffer)))

(defun macher-agent-a2a--extract-parent-context (shared &optional parent-buf)
  "Extract parent context from SHARED state or PARENT-BUF."
  (when shared
    (or (let ((ctx (plist-get shared :parent-context)))
          (when (macher-agent-valid-context-p ctx)
            ctx))
        (let ((p-buf (or parent-buf (macher-agent-a2a--extract-parent-buffer shared))))
          (macher-agent-resolve-context p-buf)))))

(defvar macher-agent-a2a-pipeline-functions
  '(macher-agent-a2a-pipe--validate-routing
    macher-agent-a2a-pipe--acquire-target
    macher-agent-a2a-pipe--register-ownership
    macher-agent-a2a-pipe--route-instructions
    macher-agent-a2a-pipe--bind-closure
    macher-agent-a2a-pipe--transmit)
  "Chained reducer pipeline to construct and execute an A2A sub-agent.")

(defun macher-agent-a2a-dispatch (send-message-payloads final-callback &optional parent-ctx-override)
  "Dispatch pre-normalised A2A SEND-MESSAGE-PAYLOADS."
  (when final-callback
    (cl-check-type final-callback function))
  (let* ((parent-ctx (if (macher-agent-valid-context-p parent-ctx-override)
                         parent-ctx-override
                       (macher-agent-resolve-context (current-buffer))))
         (payloads-list (if (vectorp send-message-payloads)
                            (append send-message-payloads nil)
                          send-message-payloads))
         (total (length payloads-list))
         (shared-state (list :results (make-hash-table :test 'equal)
                             :total total
                             :final-callback final-callback
                             :parent-buffer (current-buffer)
                             :parent-context parent-ctx
                             :original-payloads payloads-list)))
    (if (= total 0)
        (when final-callback
          (funcall final-callback []))
      (let ((seen-targets nil)
            (duplicate-found nil))
        (dolist (msg payloads-list)
          (let* ((tt (when (macher-agent-transit-payload-p msg)
                       (macher-agent-transit-payload-transit-type msg)))
                 (tp (when (macher-agent-transit-payload-p msg)
                       (macher-agent-transit-payload-type msg)))
                 (is-delegation (or (memq tt '(:task-delegation :subagent-execution :root-to-subagent))
                                    (memq tp '(TASK_DELEGATION SUBAGENT_EXECUTION :task-delegation :subagent-execution))))
                 (meta (when (macher-agent-transit-payload-p msg)
                         (macher-agent-transit-payload-metadata msg)))
                 (raw-target (when (macher-agent-transit-payload-p msg)
                               (or (plist-get meta :buffer_name)
                                   (macher-agent-transit-payload-target-buffer msg))))
                 (target-name (when raw-target (macher-agent--resolve-buffer-name raw-target))))
            (when (and is-delegation target-name)
              (if (member target-name seen-targets)
                  (setq duplicate-found t)
                (push target-name seen-targets)))))
        (if duplicate-found
            (when final-callback
              (funcall final-callback "ERROR: Duplicate target buffer in sub-agent batch."))
          (dolist (msg payloads-list)
            (let ((tid (macher-agent-transit-payload-task-id msg)))
              (unless (and tid (stringp tid) (not (string-empty-p tid)))
                (error "STRICT CONTRACT VIOLATION: Payload missing required task-id"))
              (if (eq (macher-agent-transit-payload-type msg) 'ARTIFACT_UPDATE)
                  (let ((cb (macher-agent-pop-task-callback tid)))
                    (if (functionp cb)
                        (funcall cb msg)
                      (error "STRICT CONTRACT VIOLATION: Unknown or non-waiting task-id '%s'" tid)))
                (let* ((initial-state (list :a2a-msg msg
                                            :shared-state shared-state
                                            :child-buf nil))
                       (final-pipe-state
                        (seq-reduce (lambda (state pipe-fn)
                                      (if (functionp pipe-fn) (funcall pipe-fn state) state))
                                    macher-agent-a2a-pipeline-functions
                                    initial-state))
                       (err-payload (plist-get final-pipe-state :error-payload)))
                  (when err-payload
                    (let ((parent-buf (macher-agent-a2a--extract-parent-buffer shared-state)))
                      (macher-agent--aggregate-a2a-results
                       tid err-payload
                       (plist-get shared-state :results)
                       total
                       (plist-get shared-state :original-payloads)
                       final-callback
                       (or parent-buf (current-buffer))))))))))))))

(defun macher-agent--init-subagent-state (buf _task-id meta parent-ctx &optional skip-clone)
  "Initialise BUF state mapped to PARENT-CTX.
When `:background' is non-nil in META, leaves window focus in the caller."
  (cl-check-type parent-ctx macher-agent-context)
  (with-current-buffer buf
    (setq-local macher-agent--is-ephemeral (plist-get meta :ephemeral))
    (setq-local macher-agent--suppress-patch
                (if (plist-member meta :suppress-patch)
                    (plist-get meta :suppress-patch)
                  t))
    (setq-local macher-agent--ready-to-reap nil)
    (when-let* ((v (plist-get meta :presets)))
      (setq-local macher-agent-presets
                  (if (vectorp v) (append v nil) (ensure-list v))))
    (unless (and skip-clone (macher-agent-valid-context-p macher-agent--persistent-context))
      (let ((cloned (macher-agent--clone-context parent-ctx))
            (p-buf (or (macher-agent-context-owner-buffer parent-ctx)
                       (macher-agent-context-origin-buffer parent-ctx))))
        (when (and p-buf (not (eq p-buf buf)))
          (setf (macher-agent-context-parent-buffer cloned) p-buf))
        (setf (macher-agent-context-owner-buffer cloned) buf)
        (setq-local macher-agent--persistent-context cloned)))
    (when (macher-agent-valid-context-p macher-agent--persistent-context)
      (let ((session (macher-agent-context-session macher-agent--persistent-context)))
        (setf (macher-agent-session-owner-buffer session) buf)
        (setf (macher-agent-session-lifecycle session)
              (if (plist-get meta :ephemeral) :ephemeral :persistent))
        (setf (macher-agent-session-state session) :running)))
    (unless (plist-get meta :background)
      (macher-agent-ui-show buf))))

(defun macher-agent-a2a-pipe--validate-routing (state)
  "Validate message routing paths and prevent circular communication loops."
  (let* ((msg (plist-get state :a2a-msg))
         (task-id (macher-agent-transit-payload-task-id msg))
         (transit-type (macher-agent-transit-payload-transit-type msg))
         (msg-type (macher-agent-transit-payload-type msg))
         (meta (macher-agent-transit-payload-metadata msg))
         (buf-name (macher-agent--resolve-buffer-name
                    (or (plist-get meta :buffer_name)
                        (macher-agent-transit-payload-target-buffer msg))))
         (shared (plist-get state :shared-state))
         (parent-buf (plist-get shared :parent-buffer))
         (originator-name (macher-agent--resolve-buffer-name parent-buf))
         (is-delegation (or (memq transit-type '(:task-delegation :subagent-execution :root-to-subagent))
                            (memq msg-type '(TASK_DELEGATION SUBAGENT_EXECUTION :task-delegation :subagent-execution))))
         (target-buf (when buf-name (get-buffer buf-name))))
    (setq state (plist-put state :target-name buf-name))
    (setq state (plist-put state :originator-name originator-name))
    (cond
     ((and buf-name originator-name (equal buf-name originator-name))
      (plist-put state :error-payload
                 (list :status 'error :error "Circular routing detected" :task-id task-id)))
     ((and is-delegation
           target-buf
           (buffer-live-p target-buf)
           (let* ((ctx (buffer-local-value 'macher-agent--persistent-context target-buf))
                  (session (when (macher-agent-context-p ctx) (macher-agent-context-session ctx)))
                  (sess-state (when session (macher-agent-session-state session)))
                  (active-fsm (buffer-local-value 'macher-agent--active-fsm target-buf))
                  (wake-cb (macher-agent-get-wake-callback buf-name)))
             (and (or (eq sess-state :running) active-fsm)
                  (not wake-cb))))
      (plist-put state :error-payload
                 (list :status 'error
                       :error "ERROR: Target sub-agent buffer is already busy running a task."
                       :task-id task-id)))
     (t state))))

(defun macher-agent-a2a-pipe--acquire-target (state)
  "Acquire the target buffer instance or spawn a new sub-agent buffer."
  (if (plist-get state :error-payload)
      state
    (let* ((msg (plist-get state :a2a-msg))
           (meta (macher-agent-transit-payload-metadata msg))
           (task-id (macher-agent-transit-payload-task-id msg))
           (transit-type (macher-agent-transit-payload-transit-type msg))
           (raw-buf (when meta (plist-get meta :buffer_name)))
           (presets (when meta (plist-get meta :presets)))
           (buf-name (plist-get state :target-name))
           (shared (plist-get state :shared-state))
           (parent-buf (macher-agent-a2a--extract-parent-buffer shared))
           (parent-ctx (or (let ((c (macher-agent-transit-payload-parent-context msg)))
                             (when (macher-agent-valid-context-p c) c))
                           (let ((c (macher-agent-transit-payload-target-context msg)))
                             (when (macher-agent-valid-context-p c) c))
                           (macher-agent-a2a--extract-parent-context shared parent-buf)))
           (existing-buf (cond
                          ((and (bufferp raw-buf) (buffer-live-p raw-buf)) raw-buf)
                          ((and buf-name (stringp buf-name)) (get-buffer buf-name))
                          (t nil))))
      (if (and existing-buf (buffer-live-p existing-buf))
          (let ((has-wake-cb (macher-agent-get-wake-callback (buffer-name existing-buf))))
            (unless (or has-wake-cb (eq transit-type :peer-to-peer))
              (when parent-ctx
                (macher-agent--init-subagent-state existing-buf task-id meta parent-ctx)))
            (when (eq transit-type :peer-to-peer)
              (let ((target-ctx (buffer-local-value 'macher-agent--persistent-context existing-buf)))
                (when (and (macher-agent-valid-context-p parent-ctx)
                           (macher-agent-valid-context-p target-ctx))
                  (run-hook-with-args 'macher-agent-peer-sync-hook parent-ctx target-ctx))))
            (plist-put state :child-buf existing-buf))
        (if (and presets (stringp buf-name) (not (string-empty-p buf-name)))
            (condition-case _err
                (let ((child-buf (macher-agent-add-subagent buf-name presets parent-buf nil parent-ctx)))
                  (when parent-ctx
                    (macher-agent--init-subagent-state child-buf task-id meta parent-ctx t))
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
  "Register hierarchical ownership paths in the global agent registries."
  (if (or (plist-get state :error-payload)
          (null (plist-get state :child-buf))
          (null (plist-get state :originator-name)))
      state
    (let* ((msg (plist-get state :a2a-msg))
           (transit-type (macher-agent-transit-payload-transit-type msg))
           (task-id (macher-agent-transit-payload-task-id msg))
           (originator-name (plist-get state :originator-name))
           (child-buf (plist-get state :child-buf))
           (child-name (buffer-name child-buf))
           (has-wake-cb (macher-agent-get-wake-callback child-name))
           (shared (plist-get state :shared-state))
           (parent-buf (macher-agent-a2a--extract-parent-buffer shared)))
      (unless (or has-wake-cb (eq transit-type :peer-to-peer))
        (when task-id
          (puthash task-id originator-name macher-agent--task-registry))
        (let* ((ws-root (or (when-let* ((pctx (macher-agent-resolve-context parent-buf)))
                              (macher-agent-context-project-root pctx))
                            (when-let* ((cctx (macher-agent-resolve-context child-buf)))
                              (macher-agent-context-project-root cctx))))
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
                       macher-agent--a2a-ownership)))))
      state)))

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

(defun macher-agent-a2a-pipe--route-instructions (state)
  "Route payload instructions to the target buffer or intercept suspended states."
  (let* ((msg (plist-get state :a2a-msg))
         (raw-task-id (or (when (macher-agent-transit-payload-p msg)
                            (macher-agent-transit-payload-task-id msg))
                          (when (listp msg) (plist-get msg :task-id))
                          (bound-and-true-p macher-agent--current-task-id)
                          (macher-agent--generate-uuid)))
         (task-id (macher-agent--resolve-unique-task-id raw-task-id))
         (transit-type (when (macher-agent-transit-payload-p msg)
                         (macher-agent-transit-payload-transit-type msg)))
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
                           (macher-agent-get-wake-callback (buffer-name child-buf)))))
         (err-payload (plist-get state :error-payload)))
    (when (and (macher-agent-transit-payload-p msg)
               (not (equal task-id (macher-agent-transit-payload-task-id msg))))
      (setf (macher-agent-transit-payload-task-id msg) task-id)
      (setq state (plist-put state :a2a-msg msg)))
    (cond
     (err-payload
      (macher-agent--aggregate-a2a-results task-id err-payload results total original-payloads final-callback parent-buf))
     (wake-cb
      (when (and child-buf (buffer-live-p child-buf))
        (macher-agent-pop-wake-callback (buffer-name child-buf)))
      (setq state (plist-put state :wake-cb wake-cb))
      (setq state (plist-put state :wake-msg instructions)))
     ((and (eq transit-type :peer-to-peer)
           child-buf
           (buffer-live-p child-buf))
      (with-current-buffer child-buf
        (setq-local macher-agent--message-queue
                    (append macher-agent--message-queue (list (or instructions ""))))
        (when-let* ((ctx (bound-and-true-p macher-agent--persistent-context))
                    ((macher-agent-context-p ctx)))
          (setf (macher-agent-session-message-queue (macher-agent-context-session ctx))
                macher-agent--message-queue)))
      (setq state (plist-put state :queued-peer-msg t)))
     ((and child-buf (buffer-live-p child-buf))
      (with-current-buffer child-buf
        (when is-ephem (setq-local macher-agent--is-ephemeral t))
        (goto-char (point-max))
        (insert (or instructions "") "\n\n"))))
    state))

(defun macher-agent-a2a-pipe--bind-closure (state)
  "Bind the remote procedure closure and manage result aggregation."
  (if (or (plist-get state :error-payload)
          (plist-get state :wake-cb)
          (plist-get state :queued-peer-msg))
      state
    (let* ((msg (plist-get state :a2a-msg))
           (raw-task-id (or (when (macher-agent-transit-payload-p msg)
                              (macher-agent-transit-payload-task-id msg))
                            (when (listp msg) (plist-get msg :task-id))
                            (bound-and-true-p macher-agent--current-task-id)
                            (macher-agent--generate-uuid)))
           (task-id (macher-agent--resolve-unique-task-id raw-task-id))
           (shared (plist-get state :shared-state))
           (parent-buf (macher-agent-a2a--extract-parent-buffer shared))
           (parent-name (or (plist-get state :originator-name)
                            (when (and parent-buf (buffer-live-p parent-buf))
                              (buffer-name parent-buf))))
           (meta (if (macher-agent-transit-payload-p msg)
                     (macher-agent-transit-payload-metadata msg)
                   (plist-get msg :metadata)))
           (suppress-patch (if (and meta (plist-member meta :suppress-patch))
                               (plist-get meta :suppress-patch)
                             t))
           (is-ephem (when meta (plist-get meta :ephemeral)))
           (is-bg (when meta (plist-get meta :background)))
           (child-buf (plist-get state :child-buf))
           (results (plist-get shared :results))
           (total (or (plist-get shared :total) 1))
           (final-callback (plist-get shared :final-callback))
           (original-payloads (plist-get shared :original-payloads)))
      (when (and (macher-agent-transit-payload-p msg)
                 (not (equal task-id (macher-agent-transit-payload-task-id msg))))
        (setf (macher-agent-transit-payload-task-id msg) task-id)
        (setq state (plist-put state :a2a-msg msg)))
      (when (and child-buf (buffer-live-p child-buf))
        (with-current-buffer child-buf
          (when is-ephem (setq-local macher-agent--is-ephemeral t))
          (let* ((called nil)
                 (a2a-cb
                  (lambda (artifact-payload)
                    (unless called
                      (setq called t)
                      (macher-agent-pop-task-callback task-id)
                      (let* ((p (when (macher-agent-transit-payload-p artifact-payload) artifact-payload))
                             (msg-body (if (and p (eq (macher-agent-transit-payload-type p) 'ARTIFACT_UPDATE))
                                           (macher-agent-transit-payload-payload p)
                                         artifact-payload))
                             (actual-task-id (or (when p (macher-agent-transit-payload-task-id p)) task-id)))
                        (when (and (macher-agent--plist-p msg-body) (not (plist-get msg-body :shared-state)))
                          (setq msg-body (plist-put (copy-sequence msg-body) :shared-state shared)))
                        (when (and parent-buf (buffer-live-p parent-buf))
                          (with-current-buffer parent-buf
                            (let ((parent-ctx (macher-agent-resolve-context parent-buf)))
                              (when parent-ctx
                                (setq-local macher-agent--persistent-context parent-ctx)
                                (if p
                                    (setf (macher-agent-transit-payload-target-context p) parent-ctx)
                                  (when (macher-agent--plist-p artifact-payload)
                                    (setq artifact-payload (plist-put (copy-sequence artifact-payload) :target-context parent-ctx))))))
                            (when (fboundp 'macher-agent-run-pipeline)
                              (setq artifact-payload (macher-agent-run-pipeline 'payload-merge artifact-payload)))
                            (when-let* ((merged-ctx (macher-agent-resolve-context artifact-payload)))
                              (setq-local macher-agent--persistent-context merged-ctx))))
                        (macher-agent--aggregate-a2a-results actual-task-id msg-body results total original-payloads final-callback parent-buf))))))
            (macher-agent-register-task-callback task-id a2a-cb)
            (unless is-bg
              (macher-agent--push-routing task-id parent-name suppress-patch))
            (setq state (plist-put state :a2a-cb a2a-cb)))))
      state)))

(defun macher-agent-a2a-pipe--transmit (state)
  "Trigger network transmission or execute staged wake closures."
  (unless (or (plist-get state :error-payload)
              (plist-get state :queued-peer-msg))
    (if-let* ((wake-cb (plist-get state :wake-cb)))
        (when (functionp wake-cb)
          (let ((child-buf (plist-get state :child-buf))
                (wake-msg (plist-get state :wake-msg)))
            (if (and child-buf (buffer-live-p child-buf))
                (with-current-buffer child-buf
                  (funcall wake-cb wake-msg))
              (funcall wake-cb wake-msg))))
      (when-let* ((child-buf (plist-get state :child-buf)))
        (when (buffer-live-p child-buf)
          (with-current-buffer child-buf
            (setq-local macher-agent--active-fsm nil)
            (setq-local macher-agent-task-finished nil)
            (let* ((msg (plist-get state :a2a-msg))
                   (task-id (or (macher-agent-transit-payload-task-id msg)
                                (bound-and-true-p macher-agent--current-task-id)))
                   (a2a-cb (or (and task-id (macher-agent-get-task-callback task-id))
                               (plist-get state :a2a-cb)))
                   (target-name (buffer-name child-buf))
                   (callbacks
                    (list :on-success
                          (lambda (res)
                            (let ((cb (and task-id (macher-agent-pop-task-callback task-id))))
                              (when (and cb (functionp cb))
                                (funcall cb (list :status 'success
                                                  :data res
                                                  :task-id task-id
                                                  :buffer-name target-name)))))
                          :on-error
                          (lambda (err)
                            (let ((cb (or (and task-id (macher-agent-pop-task-callback task-id)) a2a-cb)))
                              (when (and cb (functionp cb))
                                (funcall cb (list :status 'error
                                                  :error err
                                                  :task-id task-id
                                                  :buffer-name target-name))))))))
              (let ((ctx (make-macher-agent-task-context :target-buffer (current-buffer))))
                (macher-agent-gptel-transmit ctx callbacks))))))))
  state)

(defun macher-agent--mark-ready-to-reap-if-ephemeral (&optional buf)
  "Mark BUF as ready to reap when it is both ephemeral and finished."
  (let ((target (or buf (current-buffer))))
    (when (buffer-live-p target)
      (with-current-buffer target
        (when (and (bound-and-true-p macher-agent--is-ephemeral)
                   (bound-and-true-p macher-agent-task-finished))
          (setq-local macher-agent--ready-to-reap t)
          (when-let* ((ctx (bound-and-true-p macher-agent--persistent-context))
                      ((macher-agent-context-p ctx)))
            (setf (macher-agent-session-state (macher-agent-context-session ctx)) :reapable)))))))

(defun macher-agent--reap-buffer (buf)
  "Execute reap on live BUF object when it is flagged as ready to reap."
  (cl-check-type buf buffer)
  (when (buffer-live-p buf)
    (macher-agent--mark-ready-to-reap-if-ephemeral buf)
    (when (macher-agent-ready-to-reap-p buf)
      (let ((buf-name (buffer-name buf))
            (task-id (buffer-local-value 'macher-agent--current-task-id buf)))
        (run-hook-with-args 'macher-agent-actor-reap-hook
                            (delete-dups (delq nil (list buf-name task-id)))))
      (macher-agent--remove-active-subagent-registries buf)
      (with-current-buffer buf
        (set-buffer-modified-p nil)
        (ignore-errors (macher-agent-bridge-abort buf))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(defvar-local macher-agent--reap-timer nil
  "Timer object tracking scheduled asynchronous buffer reap.")
(put 'macher-agent--reap-timer 'permanent-local t)

(defun macher-agent--schedule-buffer-reap (&optional buf)
  "Schedule BUF to be reaped asynchronously."
  (let ((target-buf (or buf (current-buffer))))
    (if (buffer-live-p target-buf)
        (with-current-buffer target-buf
          (when (timerp macher-agent--reap-timer)
            (cancel-timer macher-agent--reap-timer))
          (let ((timer (run-at-time 0 nil
                                    (lambda ()
                                      (when (buffer-live-p target-buf)
                                        (macher-agent--reap-buffer target-buf))))))
            (setq-local macher-agent--reap-timer timer)
            timer))
      nil)))

(defun macher-agent--get-ownership-keys (parent-ident)
  "Extract ownership tracking keys for string PARENT-IDENT."
  (cl-check-type parent-ident string)
  (let* ((p-buf (get-buffer parent-ident))
         (keys (list parent-ident)))
    (when-let* ((p-buf)
                ((buffer-live-p p-buf))
                (ctx (macher-agent-resolve-context p-buf))
                (p-ws-root (macher-agent-context-project-root ctx))
                (scoped-key (format "%s::%s" (expand-file-name p-ws-root) parent-ident)))
      (push scoped-key keys))
    (when (and (bound-and-true-p macher-agent--a2a-ownership) (hash-table-p macher-agent--a2a-ownership))
      (let ((suffix (format "::%s" parent-ident)))
        (maphash (lambda (k _v)
                   (when (and (stringp k) (string-suffix-p suffix k))
                     (push k keys)))
                 macher-agent--a2a-ownership)))
    (delete-dups (delq nil keys))))

(defun macher-agent--get-children (parent-keys)
  "Retrieve all child agents associated with PARENT-KEYS."
  (let ((children nil))
    (when (and (bound-and-true-p macher-agent--a2a-ownership) (hash-table-p macher-agent--a2a-ownership))
      (dolist (k parent-keys)
        (when-let* ((v (gethash k macher-agent--a2a-ownership)))
          (when (listp v)
            (setq children (append children v))))))
    (delete-dups children)))

(defun macher-agent--reap-child-if-ready (child-ident &optional _force-terminal)
  "Evaluate reap conditions for live CHILD-IDENT buffer."
  (cl-check-type child-ident buffer)
  (when (buffer-live-p child-ident)
    (macher-agent--mark-ready-to-reap-if-ephemeral child-ident)
    (when (macher-agent-ready-to-reap-p child-ident)
      (macher-agent--schedule-buffer-reap child-ident)
      (macher-agent--remove-active-subagent-registries child-ident))))

(defun macher-agent--clean-ownership-registry (parent-keys &optional _force-terminal)
  "Remove reaped children from the ownership registry for PARENT-KEYS."
  (when (and (bound-and-true-p macher-agent--a2a-ownership) (hash-table-p macher-agent--a2a-ownership))
    (dolist (k parent-keys)
      (when-let* ((current-list (gethash k macher-agent--a2a-ownership))
                  ((listp current-list)))
        (let ((remaining
               (cl-remove-if-not
                (lambda (entry)
                  (when-let* ((c-buf (if (bufferp entry) entry (get-buffer entry)))
                              ((buffer-live-p c-buf)))
                    (macher-agent--mark-ready-to-reap-if-ephemeral c-buf)
                    (not (macher-agent-ready-to-reap-p c-buf))))
                current-list)))
          (if remaining
              (puthash k remaining macher-agent--a2a-ownership)
            (remhash k macher-agent--a2a-ownership)))))))

(defun macher-agent-sweep-subagents (originator-name &optional force-terminal)
  "Execute sweep exclusively for string ORIGINATOR-NAME and FORCE-TERMINAL."
  (cl-check-type originator-name string)
  (let ((visited (make-hash-table :test 'equal)))
    (letrec ((sweep-subagent-tree
              (lambda (parent-ident)
                (let* ((parent-keys (macher-agent--get-ownership-keys parent-ident))
                       (children (macher-agent--get-children parent-keys)))
                  (dolist (child children)
                    (let ((child-buf (if (bufferp child) child (get-buffer child))))
                      (when child-buf
                        (let ((child-name (buffer-name child-buf)))
                          (when (and child-name (not (gethash child-name visited)))
                            (puthash child-name t visited)
                            (funcall sweep-subagent-tree child-name)
                            (macher-agent--reap-child-if-ready child-buf force-terminal))))))
                  (macher-agent--clean-ownership-registry parent-keys force-terminal)))))
      (funcall sweep-subagent-tree originator-name))))

(defun macher-agent--flush-sweep-buffer (buf)
  "Sweep reapable child sub-agents owned by BUF upon task flush."
  (when (buffer-live-p buf)
    (macher-agent-sweep-subagents (buffer-name buf) t)))

(add-hook 'macher-agent-buffer-flush-hook #'macher-agent--flush-sweep-buffer)

(defun macher-agent-post-response-reaper (_beg _end)
  "Reap finished ephemeral buffer or sweep sub-agents for orchestrator."
  (let ((cur (current-buffer)))
    (macher-agent--mark-ready-to-reap-if-ephemeral cur)
    (let* ((ready (macher-agent-ready-to-reap-p cur))
           (finished (bound-and-true-p macher-agent-task-finished))
           (matching-fsm (macher-agent-get-active-fsm))
           (info (when matching-fsm (macher-agent--extract-fsm-info matching-fsm)))
           (fsm-buf (when info (plist-get info :buffer)))
           (matching-info (when (and fsm-buf (eq fsm-buf cur)) info))
           (tool-calls (and matching-info (plist-get matching-info :tool-use))))
      (when (and (not finished) (not ready) (not tool-calls))
        (while (or (bound-and-true-p macher-agent--routing-stack)
                   (bound-and-true-p macher-agent--current-task-id))
          (let* ((frame (when (bound-and-true-p macher-agent--routing-stack)
                          (macher-agent--pop-routing)))
                 (task-id (or (plist-get frame :task-id)
                              (bound-and-true-p macher-agent--current-task-id))))
            (setq-local macher-agent--current-task-id nil)
            (when task-id
              (when-let* ((cb (macher-agent-pop-task-callback task-id)))
                (funcall cb (list :status 'error
                                  :error "Sub-agent halted before submitting task result."
                                  :task-id task-id)))))))
      (if ready
          (macher-agent--schedule-buffer-reap cur)
        (unless tool-calls
          (macher-agent-sweep-subagents (buffer-name cur) t))))))

;;; Sub-Agent Buffer Creation Pipeline

(defvar macher-agent-subagent-pipeline-functions
  '(macher-agent-subagent-pipe--normalize-args
    macher-agent-subagent-pipe--resolve-context
    macher-agent-subagent-pipe--init-buffer
    macher-agent-subagent-pipe--register)
  "Store chained reducer pipeline to construct and initialise a subagent buffer.")

(defun macher-agent-subagent-pipe--normalize-args (state)
  "Pipeline step 1: Ensure `:parent-buffer' defaults to `current-buffer'."
  (if (plist-get state :parent-buffer)
      state
    (plist-put state :parent-buffer (current-buffer))))

(defun macher-agent-subagent-pipe--resolve-context (state)
  "Pipeline step 2: Resolve context, clone it, and determine target directory."
  (let* ((parent (plist-get state :parent-buffer))
         (dir (plist-get state :dir))
         (ctx (plist-get state :context))
         (resolved-ctx (or (macher-agent-resolve-context ctx)
                           (macher-agent-resolve-context parent)
                           (macher-agent-resolve-context dir)))
         (cloned-ctx (when resolved-ctx
                       (let ((c (macher-agent--clone-context resolved-ctx)))
                         (when (and (bufferp parent) (buffer-live-p parent))
                           (setf (macher-agent-context-parent-buffer c) parent))
                         c)))
         (target-dir (or dir
                         (when cloned-ctx
                           (macher-agent-context-project-root cloned-ctx))
                         default-directory)))
    (plist-put (plist-put (plist-put state :resolved-ctx resolved-ctx)
                          :cloned-ctx cloned-ctx)
               :target-dir target-dir)))

(defun macher-agent-subagent-pipe--init-buffer (state)
  "Pipeline step 3: Create and initialise subagent buffer in STATE."
  (let* ((name (plist-get state :name))
         (target-dir (plist-get state :target-dir))
         (parent (plist-get state :parent-buffer))
         (cloned-ctx (plist-get state :cloned-ctx))
         (presets (plist-get state :presets))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (when (and (fboundp 'markdown-mode)
                 (not (derived-mode-p 'markdown-mode)))
        (markdown-mode))
      (setq-local macher-agent--is-workspace t)
      (setq-local gptel-stream nil)
      (setq-local default-directory (file-name-as-directory target-dir))
      (when (and (fboundp 'gptel-mode) (not gptel-mode))
        (gptel-mode 1))
      (when (and (bufferp parent) (buffer-live-p parent))
        (setq-local gptel-model (buffer-local-value 'gptel-model parent))
        (setq-local gptel-backend (buffer-local-value 'gptel-backend parent))
        (setq-local gptel-temperature (buffer-local-value 'gptel-temperature parent))
        (setq-local gptel-max-tokens (buffer-local-value 'gptel-max-tokens parent))
        (setq-local macher-agent--suppress-patch
                    (with-current-buffer parent (bound-and-true-p macher-agent--suppress-patch)))
        (setq-local macher-agent--boot-directive
                    (with-current-buffer parent (bound-and-true-p macher-agent--boot-directive)))
        (setq-local gptel--known-presets (buffer-local-value 'gptel--known-presets parent))
        (setq-local gptel--known-tools (buffer-local-value 'gptel--known-tools parent))
        (setq-local gptel-directives (buffer-local-value 'gptel-directives parent)))
      (when cloned-ctx
        (setf (macher-agent-context-owner-buffer cloned-ctx) buf)
        (setq-local macher-agent--persistent-context cloned-ctx))
      (when presets
        (let ((preset-list (cond ((listp presets) presets)
                                 ((vectorp presets) (append presets nil))
                                 (t (list presets)))))
          (setq-local macher-agent-presets preset-list)
          (macher-agent--apply-preset preset-list)))
      (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)
      (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t)
      (add-hook 'gptel-post-response-functions #'macher-agent-post-response-reaper nil t))
    (plist-put state :target-buf buf)))

(defun macher-agent-subagent-pipe--register (state)
  "Pipeline step 4: Register subagent using unified tracking."
  (let* ((name (plist-get state :name))
         (target-dir (plist-get state :target-dir))
         (cloned-ctx (plist-get state :cloned-ctx))
         (parent-ctx (or (plist-get state :resolved-ctx)
                         (macher-agent-resolve-context (plist-get state :parent-buffer)))))
    (when (and parent-ctx cloned-ctx)
      (macher-agent--register-child-process name target-dir parent-ctx cloned-ctx))
    state))

(defun macher-agent-add-subagent (name &optional presets parent-buf dir context)
  "Interactively or programmatically create sub-agent buffer NAME."
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
  "Register child process NAME bound strictly to PARENT-CTX and CLONED-CTX structs."
  (cl-check-type cloned-ctx macher-agent-context)
  (cl-check-type parent-ctx macher-agent-context)
  (when (boundp 'macher-agent-active-subagents)
    (setq macher-agent-active-subagents
          (cons (cons name target-dir)
                (cl-delete name macher-agent-active-subagents :key (lambda (e) (if (consp e) (car e) e)) :test #'equal))))
  (setf (macher-agent-context-subagents cloned-ctx)
        (cons (cons name target-dir)
              (cl-delete name (macher-agent-context-subagents cloned-ctx) :key (lambda (e) (if (consp e) (car e) e)) :test #'equal)))
  (setf (macher-agent-context-subagents parent-ctx)
        (cons (cons name target-dir)
              (cl-delete name (macher-agent-context-subagents parent-ctx) :key (lambda (e) (if (consp e) (car e) e)) :test #'equal))))

(provide 'macher-agent-orchestration)
;;; macher-agent-orchestration.el ends here
