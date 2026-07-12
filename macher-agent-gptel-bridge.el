;;; macher-agent-gptel-bridge.el --- Clean gptel boundary -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'gptel)
(require 'macher)
(require 'macher-agent-vfs-client)

(declare-function macher-agent-resolve-context "macher-agent-vfs-client")
(declare-function macher-agent--auto-sync-context "macher-agent-vfs-client")
(declare-function macher-agent--split-context "macher-agent-vfs-client")
(declare-function macher-agent--build-virtual-patch "macher-agent-vfs-client")
(declare-function macher-agent--reap-buffer "macher-agent-orchestration")
(declare-function macher-agent--reap-buffer "macher-agent--apply-payload-locally")
(declare-function macher-agent-initialize-skills "macher-agent-api")
(declare-function macher-agent-canonical-tool-name "macher-agent-api")

(defvar gptel-system-prompt)

(defun macher-agent-sync-prompt-transformer (async-fn fsm)
  "Synchronise the virtual file system, normalise the active tools list,
and compose skill profiles securely.

This function acts exclusively as a pre-wire transformer. It executes
within the temporary transmission buffer managed by the system.

It parses and strips inline skill tags locally, preventing destructive
side effects to the user's source buffer. It then invokes the
composition engine to merge the base state with the inline presets,
applying the resulting transmission state ephemerally before network dispatch.

The payload is applied to buffer variables first, and the finite state machine 
property list is subsequently updated directly.

When an inline skill is used without a prompt its body becomes a prompt
within the hidden buffer, behaving like a command.

ASYNC-FN is a function to call asynchronously.
FSM is the finite-state machine.

Return the result of ASYNC-FN."
  (let* ((temp-buf (current-buffer))
         (info (when fsm (gptel-fsm-info fsm)))
         (orig-buf (or (when info (plist-get info :buffer)) temp-buf))
         (macher-agent--allow-lazy-init t)
         (ctx (macher-agent-resolve-context fsm)))

    (when (and fsm ctx)
      (setf (gptel-fsm-info fsm)
            (plist-put (gptel-fsm-info fsm) :macher-agent-context ctx)))

    (when ctx
      (with-current-buffer orig-buf
        (macher-agent--auto-sync-context ctx)
        (when (fboundp 'macher-agent-initialize-skills)
          (macher-agent-initialize-skills ctx))))

    (let ((matched-skills nil)
          (inline-preset-used nil)
          (prompt-start (save-excursion
                          (goto-char (or (previous-single-property-change (point-max) 'gptel) (point-min)))
                          (point))))
      
      (save-excursion
        (goto-char prompt-start)
        (while (re-search-forward "@\\([[:alnum:]_-]+\\)" nil t)
          (when (or (= (match-beginning 0) (point-min))
                    (memq (char-before (match-beginning 0)) '(?\s ?\t ?\n ?\r ?>)))
            (setq inline-preset-used t)
            (push (intern (match-string-no-properties 1)) matched-skills)
            (replace-match "")
            (when (looking-at "[ \t]+")
              (replace-match "")))))
      
      (unless matched-skills
        (let ((active-sys (with-current-buffer orig-buf gptel-system-prompt))
              (directives (with-current-buffer orig-buf gptel-directives)))
          (when-let* ((sym (cl-loop for (s . sys) in directives
                                    when (equal sys active-sys) return s)))
            (push sym matched-skills))))

      (let ((redirected-skill nil))
        (when inline-preset-used
          (let ((remaining-text (buffer-substring-no-properties prompt-start (point-max))))
            (unless (string-match-p "[A-Za-z]" remaining-text)
              (setq redirected-skill (pop matched-skills)))))

        (when (fboundp 'macher-agent-compose-payload)
          (let* ((base-state
                  (with-current-buffer orig-buf
                    (list :model gptel-model
                          :system gptel-system-prompt
                          :temperature (bound-and-true-p gptel-temperature)
                          :max-tokens (bound-and-true-p gptel-max-tokens)
                          :tools gptel-tools
                          :known-presets (bound-and-true-p gptel--known-presets))))
                 (remaining-skills (nreverse matched-skills))
                 (all-skills (if redirected-skill 
                                 (append remaining-skills (list redirected-skill)) 
                               remaining-skills))
                 (payload (when all-skills 
                            (macher-agent-compose-payload base-state all-skills))))
            
            (when payload
              (let* ((sys-payload (when remaining-skills 
                                    (macher-agent-compose-payload base-state remaining-skills)))
                     (final-sys-prompt (if sys-payload 
                                           (plist-get sys-payload :system)
                                         (plist-get base-state :system))))
                
                (setq payload (plist-put payload :system final-sys-prompt)))
              
              (macher-agent--apply-payload-locally payload)
              
              (when fsm
                (let ((new-info (copy-sequence (gptel-fsm-info fsm))))
                  (dolist (key '(:system :model :temperature :max-tokens :tools))
                    (when (plist-member payload key)
                      (setq new-info (plist-put new-info key (plist-get payload key)))))
                  (setf (gptel-fsm-info fsm) new-info))))
            
            (when redirected-skill
              (when-let* ((dummy-base (plist-put (copy-sequence base-state) :system ""))
                          (dummy-payload (macher-agent-compose-payload dummy-base (list redirected-skill)))
                          (sys-prompt (plist-get dummy-payload :system))
                          ((stringp sys-prompt))
                          ((not (string-empty-p sys-prompt))))
                (delete-region prompt-start (point-max))
                (insert sys-prompt)
                (when fsm
                  (setf (gptel-fsm-info fsm)
                        (plist-put (gptel-fsm-info fsm) :prompt sys-prompt)))))))))
    
    (when-let* ((fn async-fn)
                ((functionp fn)))
      (funcall fn))))

(defun macher-agent-post-response-reaper (_beg _end)
  "Reap the sub-agent buffer if flagged for disposal.

_BEG is the starting position of the text region (integer).
_END is the ending position of the text region (integer).

Return nil."
  (when (and (macher-agent-subagent-p)
             (macher-agent-ready-to-reap-p))
    (let ((buf (current-buffer)))
      (run-at-time 0 nil (lambda ()
                           (when (and (buffer-live-p buf)
                                      (fboundp 'macher-agent--reap-buffer))
                             (macher-agent--reap-buffer buf)))))))

(defun macher-agent-gptel-transmit (task-context callbacks)
  "Facade to transmit network request, restoring buffer-centric execution.

TASK-CONTEXT is a macher-agent-task-context struct representing the task.
CALLBACKS is a list representing the success and error handlers.

Return nil."
  (let* ((target-buffer (macher-agent-task-context-target-buffer task-context))
         (sys-msg (macher-agent-task-context-system-message task-context))
         (success-cb (plist-get callbacks :on-success))
         (error-cb (plist-get callbacks :on-error)))
    
    (with-current-buffer target-buffer
      (setq-local gptel-system-prompt sys-msg)
      
      (let ((response-hook nil))
        (setq response-hook
              (lambda (_beg _end)
                (let ((res (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
                  
                  (if (and (macher-agent-subagent-p)
                           (not (string-empty-p res)))
                      (message "DEBUG BRIDGE: Stream ended for sub-agent. Deferring to tool execution...")
                    
                    (if (not (string-empty-p res))
                        (when success-cb (funcall success-cb res))
                      (when error-cb (funcall error-cb "Buffer stopped silently or returned empty.")))
                    
                    (remove-hook 'gptel-post-response-functions response-hook t)))))
        
        (add-hook 'gptel-post-response-functions response-hook nil t))

      (goto-char (point-max))
      (gptel-send))))

(defun macher-agent--setup-tools-pre-hook (&rest _args)
  "Sync the VFS and inject the sandbox session before tools are executed.

_ARGS represents the unused arguments passed to the hook.

Return nil."
  (let* ((fsm (or macher-agent--active-fsm
                  (bound-and-true-p macher--fsm-latest)
                  (bound-and-true-p gptel--fsm-last)))
         (info (when fsm (gptel-fsm-info fsm)))
         (ctx (ignore-errors (macher-agent-resolve-context fsm))))
    (when ctx
      (when (fboundp 'macher-agent--auto-sync-context)
        (macher-agent--auto-sync-context ctx))
      (when fsm
        (let ((session (or (plist-get info :macher-agent-session)
                           (let* ((ws (macher-agent--get-context-workspace ctx))
                                  (proj-root (if ws (macher-agent-root ws) default-directory))
                                  (agent-ws (make-macher-agent-workspace :project-root proj-root)))
                             (make-macher-agent-session :id (buffer-name) :workspace agent-ws)))))
          (setf (gptel-fsm-info fsm) (plist-put info :macher-agent-session session)))))))

(defun macher-agent--inject-media-fsm-advice (orig-fun fsm &rest args)
  "Inject pending tool media into the FSM payload right before transitioning to WAIT.

ORIG-FUN is the original transition function.
FSM is the finite-state machine struct.
ARGS represents the arguments passed to ORIG-FUN.

Return the result of ORIG-FUN."
  (let* ((new-state (car args))
         (target-state (or new-state (ignore-errors (gptel--fsm-next fsm)))))
    (when (or (eq target-state 'WAIT) (null target-state))
      (let* ((info (macher-agent--extract-fsm-info fsm))
             (session (plist-get info :macher-agent-session))
             (pending (when session (macher-agent-session-pending-media session))))
        
        (when pending
          (let* ((msg-plist (list :role "user" 
                                  :content "Tool execution complete. Here is the requested visual data:"))
                 (prompts (list msg-plist))
                 (gptel-context pending))
            
            (when (fboundp 'gptel--inject-media)
              (gptel--inject-media (plist-get info :backend) prompts))
            
            (when (fboundp 'gptel--inject-prompt)
              (gptel--inject-prompt (plist-get info :backend) 
                                    (plist-get info :data) 
                                    (car prompts)))
            
            (setf (macher-agent-session-pending-media session) nil))))))
  (apply orig-fun fsm args))

(advice-add 'gptel--fsm-transition :around #'macher-agent--inject-media-fsm-advice)

(defun macher-agent--make-safe-callback (orig-cb)
  "Closure generator: captures orig-cb so it survives async network delays.

ORIG-CB is the original callback function.

Return a function."
  (lambda (response &rest cb-args)
    (apply orig-cb (or response "") cb-args)))

(defvar macher-agent--captured-buffer-tools nil
  "Dynamic snapshot of buffer tools.")

(defun macher-agent--protect-nil-responses (orig-fun response info &optional raw)
  "Prevent nil string crashes at the point of insertion.

ORIG-FUN is the original insertion function.
RESPONSE is the string response, which may be nil.
INFO is the response information.
RAW is an optional boolean flag.

Return the result of ORIG-FUN."
  (funcall orig-fun (or response "") info raw))

(advice-add 'gptel--insert-response :around #'macher-agent--protect-nil-responses)
(advice-add 'gptel-curl--stream-insert-response :around #'macher-agent--protect-nil-responses)

(defvar macher-agent--allow-gptel-restore nil
  "Dynamic variable controlling whether `gptel--restore-state' is allowed to execute.")

(defvar-local macher-agent--is-restored-session nil
  "Flag indicating whether the buffer is currently being restored from a saved state.")

(defun macher-agent--gptel-restore-advice (orig-fun &rest args)
  "Bypass `gptel--restore-state' unless explicitly allowed, tagging the buffer.

ORIG-FUN is the original restoration function.
ARGS represents the arguments passed to ORIG-FUN.

Return the result of ORIG-FUN."
  (when macher-agent--allow-gptel-restore
    (setq-local macher-agent--is-restored-session t)
    (let ((current-root (macher-agent-root nil)))
      (when (and current-root (fboundp 'macher-agent--init-workspace-state))
        (macher-agent--init-workspace-state current-root)))
    (let ((ctx (when (fboundp 'macher-agent-resolve-context)
                 (macher-agent-resolve-context))))
      (when ctx
        (macher-agent-initialize-skills ctx)))
    (apply orig-fun args)))

(advice-add 'gptel--restore-state :around #'macher-agent--gptel-restore-advice)

(defun macher-agent-resolve-backend-and-model (model-name)
  "Find the first backend and model format matching MODEL-NAME.

MODEL-NAME is a string or symbol specifying the model name.

Return a cons cell (BACKEND . MODEL-FORMAT) if found, otherwise nil."
  (when (and model-name (boundp 'gptel--known-backends))
    (let ((model-str (if (symbolp model-name) (symbol-name model-name) model-name))
          result)
      (dolist (item gptel--known-backends)
        (unless result
          (let* ((backend (cdr item))
                 (models (gptel-backend-models backend)))
            (dolist (m models)
              (unless result
                (let* ((m-raw (if (consp m) (car m) m))
                       (m-str (if (symbolp m-raw) (symbol-name m-raw) m-raw)))
                  (when (equal m-str model-str)
                    (setq result (cons backend m-raw)))))))))
      result)))

(defvar macher-agent--active-fsm nil
  "Dynamically bound to the active FSM during tool execution hooks.")

(defun macher-agent--bind-active-fsm-advice (orig-fn fsm &rest args)
  "Capture the current FSM dynamically so tool validators do not rely on lagging state variables.

ORIG-FN is the original function being advised.
FSM is the active finite-state machine.
ARGS represents the arguments passed to ORIG-FN.

Return the result of ORIG-FN."
  (let ((macher-agent--active-fsm fsm))
    (apply orig-fn fsm args)))

(advice-add 'gptel--handle-pre-tool :around #'macher-agent--bind-active-fsm-advice)
(advice-add 'gptel--handle-tool-use :around #'macher-agent--bind-active-fsm-advice)
(advice-add 'gptel--handle-post-tool :around #'macher-agent--bind-active-fsm-advice)

(defun macher-agent--enforce-tool-scope (tool &rest _args)
  "Enforce that TOOL is explicitly within the authorised scope.
If not, block execution to prevent hallucinated or globally-leaked tools.

To robustly validate the tool across execution boundaries (including
asynchronous callbacks and temporary buffer switches), the function
retrieves the active finite state machine (FSM) or fallback references
and compares the incoming tool against the authorised toolset.

It reads the tool names exclusively from the finite state machine snapshot,
ignoring buffer-local variables to avoid race conditions.

TOOL is the tool object or name to be verified.
_ARGS represents unused arguments passed from the hook.

Return a block list if the tool is out of scope, otherwise nil."
  (let* ((canonical-name (macher-agent-canonical-tool-name tool))
         (fsm (or macher-agent--active-fsm
                  (bound-and-true-p macher--fsm-latest)
                  (bound-and-true-p gptel--fsm-last)))
         (info (when fsm (gptel-fsm-info fsm)))
         (fsm-tools (when info (plist-get info :tools)))
         (authorised-names
          (mapcar #'macher-agent-canonical-tool-name fsm-tools)))
    (unless (and canonical-name (member canonical-name authorised-names))
      (list :block (format "ERROR: Tool '%s' is out of scope. It was not provided to this sub-agent." (or canonical-name tool))))))

(defun macher-agent-setup-gptel-buffer ()
  "Set up a gptel buffer with macher-agent capabilities if in an active agent session.

Return nil."
  (let* ((macher-agent--allow-lazy-init nil)
         (ctx (ignore-errors (macher-agent-resolve-context))))
    (when ctx
      (when (bound-and-true-p macher-agent--is-restored-session)
        (setq-local macher-agent-presets nil)
        (setq-local macher-agent--is-restored-session nil))
      (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)
      (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t)
      (add-hook 'gptel-pre-tool-call-functions #'macher-agent--setup-tools-pre-hook nil t))))

(add-hook 'gptel-mode-hook #'macher-agent-setup-gptel-buffer)

(provide 'macher-agent-gptel-bridge)
