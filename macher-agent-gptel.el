;;; macher-agent-gptel.el --- Clean gptel boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; Clean gptel boundary implementation for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'text-property-search)
(require 'macher-agent-core)
(require 'macher-agent-presets)

(defvar gptel--ptc-primitives nil)
(defvar gptel--boot-directive nil)
(defvar gptel--context-dir nil)
(defvar gptel--has-tools nil)

(defvar gptel-system-prompt)
(defvar macher-agent-token-multiplier 4
  "Estimated characters per token used for context truncation.")

(cl-defstruct macher-agent-transmission-state
  target-buffer
  model
  temperature
  max-tokens
  tools
  ptc-primitives
  known-presets
  base-prompt
  directives
  compiled-prompt
  redirect-prompt)

(defcustom macher-agent-prompt-transformers
  '(macher-agent-transformer-deduplicate-tools)
  "List of transformer functions to clean buffer prior to transmission.

Added to `gptel-prompt-transform-functions' in `macher-agent-setup-gptel-buffer'
after `macher-agent-sync-prompt-transformer'.

Return list of prompt transformer function symbols.
Side effects: None."
  :type '(repeat function)
  :group 'macher-agent)

(defcustom macher-agent-max-duplicate-tools 2
  "Maximum number of identical tool calls to retain in context history.

Older duplicates exceeding this integer limit are stripped from payload.

Return maximum allowed duplicate tool calls integer.
Side effects: None."
  :type 'integer
  :group 'macher-agent)

(defun macher-agent--fsm-hijack-transform (callback fsm)
  "Mutate the FSM to inject media, capture prompts, protect callbacks, and trigger patches."
  (let* ((info (gptel-fsm-info fsm))
         (orig-cb (plist-get info :callback))
         (b-prop (macher-agent--extract-prop info :buffer))
         (target-buf (if (eq b-prop 'macher-missing) nil b-prop)))

    (when (and target-buf (buffer-live-p target-buf))
      (with-current-buffer target-buf
        (setq-local macher--fsm-latest fsm)))

    (let* ((prompt-start
            (if (or (bound-and-true-p gptel-mode)
                    (bound-and-true-p gptel-track-response))
                (if (and (> (point-max) (point-min))
                         (get-text-property (1- (point-max)) 'gptel))
                    (point-max)
                  (or (previous-single-property-change (point-max) 'gptel) (point-min)))
              (point-min)))
           (raw-prompt
            (if (fboundp 'gptel--trim-prefixes)
                (gptel--trim-prefixes
                 (buffer-substring-no-properties prompt-start (point-max)))
              (string-trim (buffer-substring-no-properties prompt-start (point-max))))))
      (when (and raw-prompt (not (string-empty-p raw-prompt)))
        (setf (gptel-fsm-info fsm) (plist-put info :prompt raw-prompt))
        (let ((ctx (when (and target-buf (buffer-live-p target-buf))
                     (buffer-local-value 'macher-agent--persistent-context target-buf))))
          (when ctx
            (when (fboundp 'macher-agent--set-context-prompt)
              (macher-agent--set-context-prompt ctx raw-prompt))
            (when (fboundp 'macher-agent--set-context-data)
              (macher-agent--set-context-data ctx :prompt raw-prompt))
            (ignore-errors (setf (macher-context-prompt ctx) raw-prompt))))))

    (when orig-cb
      (setf (gptel-fsm-info fsm)
            (plist-put (gptel-fsm-info fsm) :callback
                       (lambda (response &rest args)
                         (let ((info-arg (car args)))
                           (when (or response (plist-get info-arg :tool-use))
                             (apply orig-cb (or response "") args)))))))

    (let* ((handlers (gptel-fsm-handlers fsm))
           (all-states (delete-dups (append (mapcar #'car handlers) '(WAIT DONE ERRS ABRT))))
           (augmented-handlers
            (cl-loop
             for state in all-states
             for funcs = (alist-get state handlers)
             collect (cons state
                           (cond
                            ((eq state 'WAIT)
                             (cons #'macher-agent--inject-media-fsm-logic funcs))
                            ((memq state '(DONE ERRS ABRT))
                             (append funcs (list #'macher-agent-gptel--trigger-flush)))
                            (t funcs))))))
      (setf (gptel-fsm-handlers fsm) augmented-handlers)))

  (funcall callback))

(defun macher-agent--restore-local-state ()
  "Restore agent variables natively after Emacs parses file-local variables."
  (when (or (local-variable-p 'gptel-model)
            (local-variable-p 'gptel-backend))
    (setq-local macher-agent--is-restored-session t)

    (let ((current-root (macher-agent-root default-directory)))
      (when current-root
        (macher-agent--init-workspace-state current-root)))

    (when (fboundp 'gptel--restore-state)
      (ignore-errors (gptel--restore-state)))

    (let* ((restored-ctx (bound-and-true-p macher-agent--persistent-context))
           (ctx (or restored-ctx (ignore-errors (macher-agent-resolve-context))))
           (current-root (macher-agent-root default-directory)))
      (when ctx
        (macher-agent--register-context-workspace-paths ctx current-root)
        (macher-agent-initialize-skills ctx)))))

(defun macher-agent--get-max-context-chars (&optional buf)
  "Resolve max context characters based on the model in the current buffer."
  (let* ((target-buf (or buf (current-buffer)))
         (model (buffer-local-value 'gptel-model target-buf))
         (model-sym (if (stringp model) (intern model) model)))
    (or (alist-get model-sym macher-agent-max-context-chars)
        (alist-get nil macher-agent-max-context-chars)
        2000000)))

;;; Prompt Transformation and Inline Skill Extraction

(defun macher-agent--extract-inline-skills (prompt-start orig-buf)
  "Extract inline skill tags starting from PROMPT-START in current buffer.

Scan for inline skill tags starting at PROMPT-START and strip matched
tags from current buffer.  ORIG-BUF is the original buffer used to locate
known skills for validation.

Return a cons cell (MATCHED-SKILLS . INLINE-PRESET-USED).
Side effects: Modifies buffer text by stripping matched inline tags."
  (let ((matched-skills nil)
        (inline-preset-used nil)
        (known (with-current-buffer orig-buf (bound-and-true-p gptel--known-presets))))
    (save-excursion
      (goto-char prompt-start)
      (while (re-search-forward "@\\([[:alnum:]_-]+\\)" nil t)
        (when (or (= (match-beginning 0) (point-min))
                  (memq (char-before (match-beginning 0)) '(32 ?\t ?\n ?\r 62)))
          (let ((sym (intern (match-string-no-properties 1))))
            (when (assoc sym known)
              (setq inline-preset-used t)
              (push sym matched-skills)
              (replace-match "")
              (when (looking-at "[ \t]+")
                (replace-match "")))))))
    (cons (nreverse matched-skills) inline-preset-used)))

(defun macher-agent--update-fsm-info-from-transmission-state (fsm state)
  "Update FSM info plist with values from TASK-CONTEXT if FSM is non-nil.

Copy existing info property list from state machine FSM and update
keys present in TASK-CONTEXT.

Return updated info plist, or nil if FSM or TASK-CONTEXT is nil.
Side effects: Mutates info property list of FSM state machine object."
  (when (and fsm state)
    (let ((new-info (copy-sequence (gptel-fsm-info fsm))))
      (setq new-info (plist-put new-info :system (macher-agent-transmission-state-compiled-prompt state)))
      (setq new-info (plist-put new-info :model (macher-agent-transmission-state-model state)))
      (setq new-info (plist-put new-info :temperature (macher-agent-transmission-state-temperature state)))
      (setq new-info (plist-put new-info :max-tokens (macher-agent-transmission-state-max-tokens state)))
      (setq new-info (plist-put new-info :tools (macher-agent-transmission-state-tools state)))
      (setq new-info (plist-put new-info :ptc-primitives (macher-agent-transmission-state-ptc-primitives state)))
      (setf (gptel-fsm-info fsm) new-info))))

(defun macher-agent-transformer-deduplicate-tools (async-fn _fsm)
  "Remove redundant tool usage blocks from context history.

ASYNC-FN is a function to call asynchronously.
_FSM is the finite-state machine object.

Return nil.
Side effects: Stubs duplicate tool usage blocks with JSON omitted notice."
  (when (and macher-agent-max-duplicate-tools
             (> macher-agent-max-duplicate-tools 0))
    (let ((seen-tools (make-hash-table :test 'equal)))
      (save-excursion
        (goto-char (point-max))
        (while (re-search-backward "^[ \t]*\\(```\\|#\\+begin_src \\)tool\\(.*\\)$" nil t)
          (let* ((start (match-beginning 0))
                 (signature (save-match-data (string-trim (match-string 2))))
                 (is-markdown (string= (match-string 1) "```"))
                 (end-regex (if is-markdown "^[ \t]*```$" "^[ \t]*#\\+end_src$"))
                 (count (gethash signature seen-tools 0)))
            (when (or (get-text-property (match-beginning 1) 'gptel)
                      (get-text-property (match-beginning 1) 'gptel-tool)
                      (get-text-property (match-beginning 1) 'gptel-response)
                      (get-text-property (match-beginning 1) 'gptel-prompt)
                      (get-text-property start 'gptel)
                      (get-text-property start 'gptel-tool)
                      (get-text-property start 'gptel-response)
                      (get-text-property start 'gptel-prompt))
              (save-excursion
                (goto-char start)
                (when (re-search-forward end-regex nil t)
                  (let* ((content-start (save-excursion
                                          (goto-char start)
                                          (forward-line 1)
                                          (point)))
                         (content-end (match-beginning 0)))
                    (if (>= count macher-agent-max-duplicate-tools)
                        (when (< content-start content-end)
                          (delete-region content-start content-end)
                          (goto-char content-start)
                          (insert "{\"status\": \"omitted\", \"reason\": \"duplicate\"}\n"))
                      (puthash signature (1+ count) seen-tools)))))))))))
  (when-let* ((fn async-fn)
              ((functionp fn)))
    (funcall fn)))

(defun macher-agent--transformer-sync-context (context orig-buf)
  "Synchronise workspace context in ORIG-BUF buffer using CONTEXT.

Return context object or nil.
Side effects: Updates buffer local state."
  (with-current-buffer orig-buf
    (when context
      (when (fboundp 'macher-agent--auto-sync-context)
        (macher-agent--auto-sync-context context))
      (macher-agent-initialize-skills context))

    (when-let* ((active-sys (or (bound-and-true-p macher-agent-base-system-prompt)
                                (bound-and-true-p gptel-system-prompt)))
                (directives (bound-and-true-p gptel-directives))
                (ui-fallback-sym (cl-loop for (s . sys) in directives
                                          when (equal sys active-sys) return s)))
      (let ((existing (bound-and-true-p macher-agent-presets)))
        (unless (memq ui-fallback-sym existing)
          (setq-local macher-agent-presets (list ui-fallback-sym)))))

    context))

(defun macher-agent--transformer-sync-ui-presets (orig-buf)
  "Synchronise UI fallback presets with buffer local presets in ORIG-BUF.

Detect UI preset selection changes in original buffer ORIG-BUF and update
`macher-agent-presets' buffer-locally when fallback preset symbol matches.

Return nil.
Side effects: May set `macher-agent-presets' buffer-locally in ORIG-BUF."
  (with-current-buffer orig-buf
    (when-let* ((active-sys (or (bound-and-true-p macher-agent-base-system-prompt)
                                (bound-and-true-p gptel-system-prompt)))
                (directives (bound-and-true-p gptel-directives))
                (ui-fallback-sym (cl-loop for (s . sys) in directives
                                          when (equal sys active-sys) return s)))
      (let ((existing (bound-and-true-p macher-agent-presets)))
        (unless (memq ui-fallback-sym existing)
          (setq-local macher-agent-presets (list ui-fallback-sym)))))))

(defun macher-agent--transformer-resolve-skills (buffer-presets inline-skills known)
  "Resolve transmission skills from BUFFER-PRESETS and INLINE-SKILLS using KNOWN.

Filter combined presets list based on exclusive preset rules specified in KNOWN
preset definitions list.

Return ordered list of resolved skill symbols.
Side effects: None."
  (let ((combined (delete-dups (append buffer-presets inline-skills nil)))
        (acc nil)
        (exclusive-found nil))
    (dolist (p combined (nreverse acc))
      (if-let* ((spec (alist-get p known))
                (excl (plist-get spec :exclusive)))
          (progn
            (setq acc (list p))
            (setq exclusive-found t))
        (unless exclusive-found
          (push p acc))))))

(defun macher-agent--transformer-detect-redirect
    (inline-preset-used prompt-start inline-skills)
  "Detect whether INLINE-PRESET-USED should redirect to a command prompt.

Check if remaining prompt text starting from PROMPT-START contains no
alphanumeric characters, that is, whitespace or empty.  INLINE-SKILLS is
the list of inline skill symbols detected.

Return redirected skill symbol if redirect condition is met, otherwise nil.
Side effects: None."
  (when-let* ((_ inline-preset-used)
              (remaining (buffer-substring-no-properties prompt-start (point-max)))
              ((not (string-match-p "[A-Za-z0-9]" remaining)))
              (skill (car inline-skills)))
    skill))

(defun macher-agent-transmission-install ()
  "Register core transmission pipeline steps in strict execution order."
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--hydrate-base-state 10)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--apply-skills-and-presets 20)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--extract-redirect 30)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--inject-dynamic-context-tools 40)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--process-hidden-blocks 50)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--init-core-directives 60)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--append-boot-directive 70)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--drain-thought-queue 90)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--compile-directives 100))

(defun macher-agent-pipe--hydrate-base-state (state orig-buf _presets _skills _redirected-skill)
  "Extract pristine local variables from ORIG-BUF into STATE."
  (with-current-buffer orig-buf
    (setf (macher-agent-transmission-state-model state) gptel-model)
    (setf (macher-agent-transmission-state-base-prompt state) gptel-system-prompt)
    (setf (macher-agent-transmission-state-temperature state) (bound-and-true-p gptel-temperature))
    (setf (macher-agent-transmission-state-max-tokens state) (bound-and-true-p gptel-max-tokens))
    (setf (macher-agent-transmission-state-tools state) gptel-tools)
    (setf (macher-agent-transmission-state-ptc-primitives state)
          (or (bound-and-true-p macher-agent-ptc-primitives)
              (bound-and-true-p macher-agent--active-ptc-primitives)))
    (setf (macher-agent-transmission-state-known-presets state) (bound-and-true-p gptel--known-presets)))
  state)

(defun macher-agent-pipe--apply-skills-and-presets (state _orig-buf presets skills _redirected-skill)
  "Merge active PRESETS and transmission skills into STATE."
  (let ((combined-modules skills))
    (when combined-modules
      (let* ((known (macher-agent-transmission-state-known-presets state))
             (payload-plist (list :known-presets known
                                  :system (macher-agent-transmission-state-base-prompt state)
                                  :tools (macher-agent-transmission-state-tools state)
                                  :ptc-primitives (macher-agent-transmission-state-ptc-primitives state)))
             (composed (macher-agent-compose-payload payload-plist combined-modules)))
        (when (plist-get composed :system)
          (setf (macher-agent-transmission-state-base-prompt state) (plist-get composed :system)))
        (when (plist-get composed :tools)
          (setf (macher-agent-transmission-state-tools state) (plist-get composed :tools)))
        (when (plist-get composed :ptc-primitives)
          (setf (macher-agent-transmission-state-ptc-primitives state) (plist-get composed :ptc-primitives))))))
  state)

(defun macher-agent-pipe--inject-dynamic-context-tools
    (state orig-buf _presets _skills _redirected-skill)
  "Inject dynamic context tools into STATE if ORIG-BUF size exceeds limits."
  (let* ((token-limit (with-current-buffer orig-buf
                        (or (bound-and-true-p gptel-max-tokens) 2048)))
         (max-chars (* token-limit macher-agent-token-multiplier))
         (buf-size (with-current-buffer orig-buf (buffer-size))))
    (when (> buf-size max-chars)
      (let ((mem-tool (ignore-errors
                        (macher-agent-resolve-tool "search_conversation_history" nil nil nil))))
        (when (macher-tool-valid-p mem-tool)
          (setf (macher-agent-transmission-state-tools state)
                (append (macher-agent-transmission-state-tools state) (list mem-tool)))))))
  state)

(defun macher-agent-pipe--process-hidden-blocks (state _orig-buf _presets _skills _redirected-skill)
  "Strip out UI-specific or hidden metadata before the LLM sees it.
Currently a STATE pass-through, but acts as a hook for sanitization."
  state)

(defun macher-agent-pipe--init-core-directives (state orig-buf _presets _skills _redirect)
  "Mandate the task submission rule if ORIG-BUF is a subagent."
  (when (buffer-local-value 'macher-agent--is-subagent orig-buf)
    (push "CRITICAL DIRECTIVE: You MUST use the `submit_task_result' tool to submit your final answer when you are completely finished. Do NOT output your final answer as standard text. IMMEDIATELY STOP after."
          (macher-agent-transmission-state-directives state)))
  state)

(defun macher-agent-pipe--append-boot-directive (state orig-buf _presets _skills _redirect)
  "Append boot directive from ORIG-BUF to STATE on initial request turn."
  (let ((boot-dir (when (and orig-buf (buffer-live-p orig-buf))
                    (with-current-buffer orig-buf
                      (bound-and-true-p macher-agent--boot-directive)))))
    (when (and boot-dir (stringp boot-dir) (not (string-empty-p boot-dir))
               (not (with-current-buffer orig-buf
                      (text-property-any (point-min) (point-max) 'gptel 'response))))
      (push boot-dir (macher-agent-transmission-state-directives state))))
  state)

(defun macher-agent-pipe--drain-thought-queue (state orig-buf _presets _skills _redirect)
  "Inject the pending instructions queue from ORIG-BUF into STATE.
Retained across turns until task completion."
  (let ((queue (buffer-local-value 'macher-agent--pending-instructions-queue orig-buf)))
    (dolist (instruction queue)
      (push instruction (macher-agent-transmission-state-directives state))))
  state)

(defun macher-agent-pipe--compile-directives (state _orig-buf _presets _skills _redirect)
  "Compile all collected directives in STATE and append them cleanly
to the base prompt for this single request frame."
  (let ((dirs (nreverse (macher-agent-transmission-state-directives state)))
        (sys (macher-agent-transmission-state-base-prompt state)))
    (if dirs
        (let ((compiled-directives (string-join dirs "\n\n")))
          (setf (macher-agent-transmission-state-compiled-prompt state)
                (concat (or sys "") "\n\n" compiled-directives)))
      (setf (macher-agent-transmission-state-compiled-prompt state) sys)))
  state)

(defun macher-agent--compile-transmission-payload (orig-buf presets skills redirected-skill)
  "Build final network state for ORIG-BUF by passing state through pipeline."
  (let ((initial-state (make-macher-agent-transmission-state :target-buffer orig-buf))
        (all-steps (macher-agent-get-pipeline-steps 'transmission)))
    (seq-reduce (lambda (state pipe-fn)
                  (funcall pipe-fn state orig-buf presets skills redirected-skill))
                all-steps
                initial-state)))

(defun macher-agent-pipe--extract-redirect (state orig-buf _presets _skills redirected-skill)
  "Route redirected skill text in STATE and natively merge its tools."
  (when redirected-skill
    (let* ((known (with-current-buffer orig-buf (bound-and-true-p gptel--known-presets)))
           (redirect-state (macher-agent-compose-payload (list :known-presets known)
                                                         (list redirected-skill)))
           (redirect-text (plist-get redirect-state :system))
           (redirect-tools (plist-get redirect-state :tools)))

      (when redirect-text
        (setf (macher-agent-transmission-state-redirect-prompt state) redirect-text))

      (when redirect-tools
        (setf (macher-agent-transmission-state-tools state)
              (macher-agent-normalize-tools
               (append (macher-agent-transmission-state-tools state) redirect-tools))))))
  state)

(defun macher-agent--transformer-apply-state
    (orig-buf buffer-presets transmission-skills redirected-skill prompt-start fsm)
  "Apply compiled prompt and tool state to buffer and FSM.

ORIG-BUF is the original buffer containing model parameters.
BUFFER-PRESETS is the list of buffer preset symbols.
TRANSMISSION-SKILLS is the list of transmission skill symbols.
REDIRECTED-SKILL is the optional redirected skill symbol.
PROMPT-START is the point integer where prompt begins in current buffer.
FSM is the optional finite-state machine object.

Return nil.
Side effects: Modifies current buffer region and FSM info."
  (let* ((state (macher-agent--compile-transmission-payload
                 orig-buf buffer-presets transmission-skills redirected-skill))
         (payload-plist (list :system (macher-agent-transmission-state-base-prompt state)
                              :tools (macher-agent-transmission-state-tools state))))

    (macher-agent--apply-payload-locally payload-plist)

    (when-let* ((fsm-obj fsm))
      (macher-agent--update-fsm-info-from-transmission-state fsm-obj state))

    (when-let* ((redirect-text (macher-agent-transmission-state-redirect-prompt state)))
      (delete-region prompt-start (point-max))
      (insert redirect-text)
      (when fsm
        (when (fboundp 'gptel-fsm-prompt)
          (with-no-warnings (setf (gptel-fsm-prompt fsm) redirect-text)))
        (setf (gptel-fsm-info fsm)
              (plist-put (gptel-fsm-info fsm) :prompt redirect-text))))))

(defun macher-agent-sync-prompt-transformer (async-fn fsm)
  "Synchronise the VFS and normalise the active tools list.
Compose skill profiles securely.

ASYNC-FN is a function to call asynchronously upon completion.
FSM is the finite-state machine object.

Return nil.
Side effects: Synchronises context, updates buffer local state,
and transforms prompt."
  (let* ((temp-buf (current-buffer))
         (active-fsm (macher-agent-get-active-fsm fsm))
         (info (when-let* ((fsm-obj active-fsm)
                           ((fboundp 'gptel-fsm-info)))
                 (ignore-errors (gptel-fsm-info fsm-obj))))
         (orig-buf (or (and info (plist-get info :buffer)) temp-buf))
         (context (macher-agent-resolve-context active-fsm)))

    (when (buffer-live-p orig-buf)
      (macher-agent--transformer-sync-context context orig-buf)
      (when context
        (when-let* ((info-prompt (and info (plist-get info :prompt))))
          (unless (macher-agent--get-context-prompt context)
            (macher-agent--set-context-prompt context info-prompt)))
        (when-let* ((p (macher-agent--get-context-prompt context)))
          (macher-agent--set-context-prompt context p)))
      (let* ((prompt-start
              (save-excursion
                (goto-char (or (previous-single-property-change (point-max) 'gptel)
                               (point-min)))
                (point)))
             (extraction (macher-agent--extract-inline-skills prompt-start orig-buf))
             (inline-skills (car extraction))
             (inline-preset-used (cdr extraction)))

        (macher-agent--transformer-sync-ui-presets orig-buf)

        (let* ((buffer-presets
                (with-current-buffer orig-buf (bound-and-true-p macher-agent-presets)))
               (known
                (with-current-buffer orig-buf (bound-and-true-p gptel--known-presets)))
               (transmission-skills
                (macher-agent--transformer-resolve-skills
                 buffer-presets inline-skills known))
               (redirected-skill
                (macher-agent--transformer-detect-redirect
                 inline-preset-used prompt-start inline-skills))

               (state (macher-agent--compile-transmission-payload
                       orig-buf buffer-presets transmission-skills redirected-skill)))

          (macher-agent--apply-payload-locally
           (list :model (macher-agent-transmission-state-model state)
                 :backend (with-current-buffer orig-buf (bound-and-true-p gptel-backend))
                 :temperature (macher-agent-transmission-state-temperature state)
                 :max-tokens (macher-agent-transmission-state-max-tokens state)
                 :tools (macher-agent-transmission-state-tools state)
                 :ptc-primitives (macher-agent-transmission-state-ptc-primitives state)))

          (when-let* ((compiled (macher-agent-transmission-state-compiled-prompt state)))
            (setq-local gptel-system-prompt compiled))

          (when active-fsm
            (macher-agent--update-fsm-info-from-transmission-state active-fsm state))

          (when-let* ((redirect-text (macher-agent-transmission-state-redirect-prompt state)))
            (delete-region prompt-start (point-max))
            (insert redirect-text)
            (when active-fsm
              (when (fboundp 'gptel-fsm-prompt)
                (with-no-warnings (setf (gptel-fsm-prompt active-fsm) redirect-text)))
              (setf (gptel-fsm-info active-fsm)
                    (plist-put (gptel-fsm-info active-fsm) :prompt redirect-text)))))))

    (when-let* ((fn async-fn)
                ((functionp fn)))
      (funcall fn))))

;;; Transmission and Sub-Agent Reaping

(defvar-local macher-agent--reap-timer nil
  "Timer object tracking scheduled asynchronous buffer reap.")
(put 'macher-agent--reap-timer 'permanent-local t)

(defun macher-agent--schedule-buffer-reap (&optional buf)
  "Schedule BUF to be reaped asynchronously.

Schedule buffer BUF (defaulting to current buffer) for asynchronous
reaping and disposal.

Return timer object created by `run-at-time'.
Side effects: Schedules asynchronous timer to reap target buffer."
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

(defun macher-agent-sweep-subagents (originator-name)
  "Sweep and reap all active sub-agent buffers associated with ORIGINATOR-NAME.

ORIGINATOR-NAME is a string or buffer identifying the originator or orchestrator.

Query `macher-agent--a2a-ownership' with workspace scope awareness, recursively
sweep and reap nested sub-agents (grandchildren), resolve live buffer objects,
do not kill sub-agents still busy executing background tasks or whose
`macher-agent-task-finished' is nil, schedule disposal for finished sub-agents,
and update `macher-agent-active-subagents', workspace active subagents, and
`macher-agent--a2a-ownership'.

Return nil.
Side effects: Marks sub-agent buffers for reaping, schedules disposal,
and updates registries."
  (let* ((orig-buf (cond
                    ((bufferp originator-name) (when (buffer-live-p originator-name) originator-name))
                    ((stringp originator-name) (get-buffer originator-name))
                    (t nil)))
         (orig-name (cond
                     ((stringp originator-name) originator-name)
                     ((bufferp originator-name) (buffer-name originator-name))
                     (t nil)))
         (visited (make-hash-table :test 'equal)))
    (when orig-name
      (letrec
          ((sweep-subagent-tree
            (lambda (parent-ident)
              (let* ((p-buf (cond
                             ((bufferp parent-ident) (when (buffer-live-p parent-ident) parent-ident))
                             ((stringp parent-ident) (get-buffer parent-ident))
                             (t nil)))
                     (p-name (cond
                              ((stringp parent-ident) parent-ident)
                              ((bufferp parent-ident) (buffer-name parent-ident))
                              (t nil)))
                     (p-ws (when p-buf
                             (or (let ((ctx (buffer-local-value 'macher-agent--persistent-context p-buf)))
                                   (when ctx (ignore-errors (macher-agent--get-context-workspace ctx))))
                                 (when (fboundp 'macher-workspace)
                                   (ignore-errors (macher-workspace p-buf))))))
                     (p-ws-root (when p-ws
                                  (ignore-errors (macher-agent-workspace-project-root p-ws))))
                     (p-scoped-key (when (and p-ws-root p-name)
                                     (format "%s::%s" (expand-file-name p-ws-root) p-name)))
                     (all-scoped-keys
                      (if (and p-buf (buffer-live-p p-buf) p-scoped-key)
                          (list p-scoped-key)
                        (when (and p-name (boundp 'macher-agent--a2a-ownership) (hash-table-p macher-agent--a2a-ownership))
                          (let ((suffix (format "::%s" p-name))
                                (matched nil))
                            (maphash (lambda (k _v)
                                       (when (and (stringp k) (string-suffix-p suffix k))
                                         (push k matched)))
                                     macher-agent--a2a-ownership)
                            matched))))
                     (parent-keys
                      (delete-dups
                       (delq nil (cons p-name all-scoped-keys))))
                     (raw-subagents
                      (delete-dups
                       (let ((acc nil))
                         (when (and (boundp 'macher-agent--a2a-ownership) (hash-table-p macher-agent--a2a-ownership))
                           (dolist (k parent-keys)
                             (let ((v (gethash k macher-agent--a2a-ownership nil)))
                               (when (listp v)
                                 (setq acc (append acc v))))))
                         acc))))
                (dolist (name-or-buf raw-subagents)
                  (let* ((child-buf (if (bufferp name-or-buf) name-or-buf (get-buffer name-or-buf)))
                         (child-name (if (bufferp name-or-buf) (buffer-name name-or-buf) name-or-buf)))
                    (when (and child-name (not (gethash child-name visited)))
                      (puthash child-name t visited)
                      (funcall sweep-subagent-tree child-name)
                      (if (and child-buf (buffer-live-p child-buf))
                          (with-current-buffer child-buf
                            (let* ((is-sub (macher-agent-subagent-p))
                                   (ready (macher-agent-ready-to-reap-p))
                                   (finished (bound-and-true-p macher-agent-task-finished)))
                              (when (and is-sub (or ready finished))
                                (setq-local macher-agent--ready-to-reap t)
                                (macher-agent--schedule-buffer-reap child-buf)
                                (macher-agent--remove-active-subagent-registries child-name child-buf))))
                        (macher-agent--remove-active-subagent-registries child-name nil))))))
              (when (and (boundp 'macher-agent--a2a-ownership) (hash-table-p macher-agent--a2a-ownership))
                (dolist (k parent-keys)
                  (let* ((current-list (gethash k macher-agent--a2a-ownership nil))
                         (remaining (when (listp current-list)
                                      (cl-remove-if-not
                                       (lambda (entry)
                                         (let* ((c-buf (if (bufferp entry) entry (get-buffer entry)))
                                                (c-name (if (bufferp entry) (buffer-name entry) entry)))
                                           (and c-buf (buffer-live-p c-buf)
                                                (with-current-buffer c-buf
                                                  (let ((is-sub (macher-agent-subagent-p))
                                                        (ready (macher-agent-ready-to-reap-p))
                                                        (finished (bound-and-true-p macher-agent-task-finished)))
                                                    (not (and is-sub (or ready finished))))))))
                                       current-list))))
                    (if remaining
                        (puthash k remaining macher-agent--a2a-ownership)
                      (remhash k macher-agent--a2a-ownership))))))))
        (funcall sweep-subagent-tree orig-name)))))

(defun macher-agent-post-response-reaper (_beg _end)
  "Reap sub-agent buffer or sweep sub-agents for orchestrator on completion.

Check if current buffer is a sub-agent and ready to be reaped.  If the
current buffer is a top-level orchestrator (`macher-agent--is-subagent' is nil)
and no tool calls were generated in the current response cycle for this buffer,
conclude the workflow and sweep all owned sub-agents.

_BEG is the starting position of the text region integer.
_END is the ending position of the text region integer.

Return nil.
Side effects: May schedule current buffer for asynchronous disposal or
trigger `macher-agent-sweep-subagents'."
  (if (macher-agent-subagent-p)
      (when (or (macher-agent-ready-to-reap-p)
                (bound-and-true-p macher-agent-task-finished)
                (and (bound-and-true-p macher-agent--is-ephemeral)
                     (bound-and-true-p macher-agent-task-finished))
                (and (bound-and-true-p macher-agent--is-background)
                     (bound-and-true-p macher-agent-task-finished)))
        (setq-local macher-agent--ready-to-reap t)
        (macher-agent--schedule-buffer-reap (current-buffer)))
    (let* ((cur (current-buffer))
           (matching-fsm (macher-agent-get-active-fsm))
           (info (when matching-fsm (macher-agent--extract-fsm-info matching-fsm)))
           (fsm-buf (when info (plist-get info :buffer)))
           (matching-info (when (and fsm-buf (eq fsm-buf cur)) info))
           (tool-calls (and matching-info (plist-get matching-info :tool-use))))
      (unless tool-calls
        (macher-agent-sweep-subagents (buffer-name cur))))))

(defun macher-agent--make-transmit-response-hook (_success-cb error-cb)
  "Create a post-response hook closure for transmit.

Generate closure function to handle response completion for sub-agents.
_SUCCESS-CB is the success callback function.
ERROR-CB is the error callback function.

Return a closure function for `gptel-post-response-functions'.
Side effects: None."
  (let ((hook-fn nil))
    (setq
     hook-fn
     (lambda (_beg _end)
       (when (macher-agent-subagent-p)
         (let*
             ((res (string-trim (buffer-substring-no-properties (point-min) (point-max))))
              (fsm (macher-agent-get-active-fsm))
              (info (and fsm (fboundp 'gptel-fsm-info) (ignore-errors (gptel-fsm-info fsm))))
              (err-data (and info (plist-get info :error))))
           (when error-cb
             (cond
              (err-data
               (let ((err-msg (if (stringp err-data) err-data
                                (or (plist-get err-data :message)
                                    (format "%s" err-data)))))
                 (funcall error-cb err-msg)))
              ((string-empty-p res)
               (funcall error-cb "Buffer stopped silently or returned empty."))
              ((not (bound-and-true-p macher-agent-task-finished))
               (funcall error-cb "Agent halted without invoking submit_task_result."))))))
       (remove-hook 'gptel-post-response-functions hook-fn t)))
    hook-fn))

(defun macher-agent-gptel-transmit (task-context callbacks)
  "Transmit network request restoring buffer-centric execution.

Dispatch network request using context and callback handlers.
TASK-CONTEXT is a task context structure representing the execution task.
CALLBACKS is a property list containing `:on-success' and `:on-error' handlers.

Return result of `gptel-send'.
Side effects: Modifies `gptel-system-prompt', adds response hook, and
calls `gptel-send'."
  (let* ((target-buffer (macher-agent-task-context-target-buffer task-context))
         (sys-msg (macher-agent-task-context-system-message task-context))
         (success-cb (or (plist-get callbacks :on-success)
                         (plist-get callbacks :success-cb)))
         (error-cb (or (plist-get callbacks :on-error)
                       (plist-get callbacks :error-cb))))
    (with-current-buffer target-buffer
      (when sys-msg
        (setq-local gptel-system-prompt sys-msg))
      (add-hook 'gptel-post-response-functions
                (macher-agent--make-transmit-response-hook success-cb error-cb)
                nil t)
      (goto-char (point-max))
      (gptel-send))))

;;; Media Injection Advice

(defvar macher-agent--in-media-injection nil
  "Track whether media injection is currently in progress.
Prevent recursive media injection loops during state transitions.")

(defun macher-agent--perform-pending-media-injection (fsm)
  "Inject pending media into FSM payload if pending media exists."
  (let* ((info (ignore-errors (gptel-fsm-info fsm)))
         (b-prop (macher-agent--extract-prop info :buffer))
         (target-buf (if (eq b-prop 'macher-missing) nil b-prop))
         (ctx-raw (or (macher-agent--extract-prop info :macher-agent-context)
                      (macher-agent--extract-prop info :macher-context)))
         (ctx (if (not (eq ctx-raw 'macher-missing)) ctx-raw
                (when (and target-buf (buffer-live-p target-buf))
                  (buffer-local-value 'macher-agent--persistent-context target-buf))))
         (pending (when ctx (macher-agent--get-context-data ctx :pending-media))))
    (when pending
      (let* ((macher-agent--in-media-injection t)
             (msg-plist (list :role "user"
                              :content "Tool execution complete. Here is the requested visual data:"))
             (prompts (list msg-plist))
             (gptel-context pending))
        (when (fboundp 'gptel--inject-media)
          ;; Dynamically intercept base64 encoding just for this block!
          ;; gptel expects a file path, but 'pending' contains massive base64 strings.
          ;; This intercepts the read and feeds the base64 directly to the API formatter,
          ;; achieving exactly what the old global advice did, but 100% MELPA compliant.
          (cl-letf (((symbol-function 'gptel--base64-encode)
                     (lambda (file)
                       (if (and (stringp file) (> (length file) 1000) (not (file-exists-p file)))
                           file
                         (with-temp-buffer
                           (insert-file-contents-literally file)
                           (base64-encode-region (point-min) (point-max) :no-line-break)
                           (buffer-string))))))
            (gptel--inject-media (plist-get info :backend) prompts)))
        (when (fboundp 'gptel--inject-prompt)
          (gptel--inject-prompt (plist-get info :backend)
                                (plist-get info :data)
                                (car prompts)))
        (macher-agent--set-context-data ctx :pending-media nil)))))

(defun macher-agent--inject-media-fsm-logic (fsm)
  "Inject pending tool media into FSM payload when entering WAIT state."
  (unless macher-agent--in-media-injection
    (macher-agent--perform-pending-media-injection fsm)))

;;; Response Protection and Callback Management

(defvar macher-agent--captured-buffer-tools nil
  "Store dynamic snapshot of active buffer tools.

Hold list of tool objects captured during buffer setup or context binding.

Return list of captured tool objects or nil.
Side effects: None.")

(defun macher-agent--protect-nil-responses (orig-fun response info &optional raw)
  "Prevent nil string crashes at response insertion point.

Ensure RESPONSE is non-nil before delegating to ORIG-FUN.
ORIG-FUN is the original response insertion function.
RESPONSE is the string response, which may be nil.
INFO is the response information plist.
RAW is an optional boolean flag.

Return result of calling ORIG-FUN.
Side effects: None."
  (when (or response (plist-get info :tool-use))
    (funcall orig-fun (or response "") info raw)))

;;; State Restoration and Workspace Registration

(defvar macher-agent--allow-gptel-restore nil
  "Control whether `gptel--restore-state' execution is permitted.

When non-nil, allow `gptel--restore-state' advice to execute restoration.

Return non-nil when state restoration is allowed, nil otherwise.
Side effects: Dynamic control variable.")

(defvar-local macher-agent--is-restored-session nil
  "Track whether current buffer is restored from a saved state.

Indicate if buffer state was populated by restoring a saved session.

Return non-nil if session was restored, nil otherwise.
Side effects: Buffer-local variable.")

(defun macher-agent--register-context-workspace-paths
    (ctx &optional current-root)
  "Register CTX in active workspaces hash-table under root paths.

Store CTX mapping in `macher-agent-active-workspaces' for CURRENT-ROOT and
CTX root.
CTX is the context structure.
CURRENT-ROOT is an optional root directory path string.

Return nil.
Side effects: Modifies `macher-agent-active-workspaces' hash-table entries."
  (when ctx
    (when current-root
      (puthash (expand-file-name current-root) ctx macher-agent-active-workspaces)
      (puthash (file-name-as-directory
                (expand-file-name current-root)) ctx macher-agent-active-workspaces)
      (puthash (directory-file-name
                (expand-file-name current-root)) ctx macher-agent-active-workspaces))
    (when-let* ((ctx-root (ignore-errors (macher-agent-context-root ctx))))
      (puthash (expand-file-name ctx-root) ctx macher-agent-active-workspaces)
      (puthash (file-name-as-directory
                (expand-file-name ctx-root)) ctx macher-agent-active-workspaces)
      (puthash (directory-file-name
                (expand-file-name ctx-root)) ctx macher-agent-active-workspaces))))

;;; Backend and Model Resolution

(defun macher-agent--find-model-in-backend (backend model-str)
  "Search BACKEND for a model matching MODEL-STR.

Iterate through models in BACKEND looking for entry matching MODEL-STR.
BACKEND is the backend structure or symbol.
MODEL-STR is the target model name string.

Return a cons cell (BACKEND . MODEL-RAW) if found, otherwise nil.
Side effects: None."
  (let ((models (gptel-backend-models backend))
        result)
    (dolist (m models)
      (unless result
        (let* ((m-raw (if (consp m) (car m) m))
               (m-str (if (symbolp m-raw) (symbol-name m-raw) m-raw)))
          (when (equal m-str model-str)
            (setq result (cons backend m-raw))))))
    result))

(defun macher-agent-resolve-backend-and-model (model-name)
  "Find first backend and model format matching MODEL-NAME.

Search known backends to resolve matching backend object and model identifier.
MODEL-NAME is a string or symbol specifying target model.

Return a cons cell (BACKEND . MODEL-FORMAT) if found, otherwise nil.
Side effects: None."
  (when (and model-name (boundp 'gptel--known-backends))
    (let ((model-str (if (symbolp model-name) (symbol-name model-name) model-name))
          result)
      (dolist (item gptel--known-backends)
        (unless result
          (setq result (macher-agent--find-model-in-backend (cdr item) model-str))))
      result)))

;;; Tool Scope Enforcement and Buffer Setup

(defun macher-agent--enforce-tool-scope (tool &optional fsm &rest _args)
  "Enforce that TOOL is explicitly within authorised scope.

Block execution if TOOL is not listed in active FSM authorised tools list.
TOOL is the tool object or name to be verified.
FSM is the optional finite-state machine object.
_ARGS represents unused arguments passed from hook.

Return a block list if TOOL is out of scope, otherwise nil.
Side effects: None."
  (let* ((canonical-name (macher-agent-canonical-tool-name tool))
         (fsm-obj (macher-agent-get-active-fsm fsm))
         (info (when fsm-obj (gptel-fsm-info fsm-obj)))
         (fsm-tools (when info (plist-get info :tools)))
         (authorised-names (mapcar #'macher-agent-canonical-tool-name fsm-tools)))
    (unless (and canonical-name (member canonical-name authorised-names))
      (list
       :block (format "ERROR: Tool '%s' is not accessible in this context or is no longer available. Please select another tool or approach."
                      (or canonical-name tool))))))

(defvar macher-agent--wrapped-tools-hash (make-hash-table :test 'eq)
  "Track wrapped `gptel-tool' instances in a hash table.

Store `gptel-tool' objects that have already been wrapped
by Macher Agent to prevent duplicate tool wrapping.

Return the hash table instance.

Side effects: Global variable storing hash table state.")

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
              (let* ((fsm (macher-agent-get-active-fsm))
                     (target-buf (or (when (and fsm (fboundp 'gptel-fsm-info))
                                       (ignore-errors (plist-get (gptel-fsm-info fsm) :buffer)))
                                     (current-buffer)))
                     (agent-ctx (macher-agent-resolve-context (or fsm target-buf))))
                (if agent-ctx
                    (progn
                      (when (macher-agent-valid-context-p orphaned-context)
                        (when-let* ((user-prompt
                                     (or (macher-agent--get-context-prompt orphaned-context)
                                         (when (fboundp 'macher-context-prompt)
                                           (ignore-errors (macher-context-prompt orphaned-context)))
                                         (macher-agent--get-context-data orphaned-context :prompt))))
                          (macher-agent--set-context-prompt agent-ctx user-prompt)))
                      (when (fboundp 'macher-agent--inject-context-into-fsm-info)
                        (macher-agent--inject-context-into-fsm-info agent-ctx fsm))
                      (with-current-buffer target-buf
                        (apply orig-fn agent-ctx callback args)))
                  (with-current-buffer target-buf
                    (apply orig-fn orphaned-context callback args))))))
      (puthash tool t macher-agent--wrapped-tools-hash))))

(defun macher-agent--wrap-macher-tools ()
  "Wrap all Macher tools to inject persistent VFS context.

Iterate through tools registered under the \"macher\"
category in `gptel--known-tools', filter out and delete
`search_in_workspace', and apply
`macher-agent--wrap-single-tool' to each remaining tool.

Return nil.

Side effects: Mutates tool functions in `gptel--known-tools' and populates
`macher-agent--wrapped-tools-hash'."
  (when-let* ((tools-entry (assoc "macher" (bound-and-true-p gptel--known-tools))))
    (let* ((macher-tools (cdr tools-entry))
           (filtered (cl-remove-if
                      (lambda (item)
                        (let* ((tool (if (consp item) (cdr item) item))
                               (name (cond
                                      ((and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
                                       (gptel-tool-name tool))
                                      ((macher-tool-valid-p tool)
                                       (gptel-tool-name tool))
                                      ((consp item) (car item))
                                      ((stringp item) item)
                                      ((symbolp item) (symbol-name item))
                                      (t nil))))
                          (and name (string= (format "%s" name) "search_in_workspace"))))
                      macher-tools)))
      (setcdr tools-entry filtered)
      (dolist (item filtered)
        (let ((tool (if (consp item) (cdr item) item)))
          (when (or (and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
                    (macher-tool-valid-p tool))
            (macher-agent--wrap-single-tool tool)))))))

(defun macher-agent-setup-gptel-buffer ()
  "Set up a gptel buffer with macher-agent capabilities.

Configure prompt transform and pre-tool call hooks in current buffer.

Return nil.
Side effects: Sets buffer-local variables and adds buffer-local hooks."
  (let* ((macher-agent--allow-lazy-init nil)
         (ctx (ignore-errors (macher-agent-resolve-context))))
    (when ctx
      (when (bound-and-true-p macher-agent--is-restored-session)
        (setq-local macher-agent-presets nil)
        (setq-local macher-agent--is-restored-session nil))
      (add-hook 'gptel-prompt-transform-functions
                #'macher-agent-sync-prompt-transformer nil t)
      (add-hook 'gptel-prompt-transform-functions
                #'macher-agent--transform-inject-context nil t)

      (dolist (transformer macher-agent-prompt-transformers)
        (add-hook 'gptel-prompt-transform-functions transformer t t))
      (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t))))

(defvar macher-agent--active-fsm nil
  "Store active FSM state machine during tool execution hooks.

Hold dynamic binding of active state machine struct during tool hook evaluation.

Return active FSM struct or nil.
Side effects: Global dynamic binding variable.")

(defun macher-agent--init-workspace-state (workspace-root)
  "Initialise the workspace state and active context for WORKSPACE-ROOT.

WORKSPACE-ROOT is the project root directory string.

Return nil.
Side effects: Sets buffer-local workspace state, registers active workspace
root, and adds prompt
transformer hook."
  (setq-local macher-agent--is-workspace t)
  (let*
      ((expanded (expand-file-name workspace-root))
       (existing
        (or (gethash expanded macher-agent-active-workspaces)
            (gethash (file-name-as-directory expanded) macher-agent-active-workspaces)
            (gethash (directory-file-name expanded) macher-agent-active-workspaces)))
       (workspace (or (and existing (macher-agent--get-context-workspace existing))
                      (make-macher-agent-workspace :project-root workspace-root)))
       (canonical-context
        (or existing
            (when (fboundp 'macher-agent--make-vfs-context)
              (macher-agent--make-vfs-context :workspace workspace :contents nil))))
       (buffer-context (if (and canonical-context (fboundp 'macher-agent--clone-context))
                           (macher-agent--clone-context canonical-context)
                         canonical-context)))
    (setq-local macher--workspace workspace)
    (when (and buffer-context (fboundp 'macher-agent--inject-context-state))
      (macher-agent--inject-context-state buffer-context))

    (unless existing
      (when (and canonical-context (fboundp 'macher-agent--register-active-workspace-root))
        (macher-agent--register-active-workspace-root workspace-root canonical-context)))

    (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)

    (let ((skills-dir (expand-file-name "skills" workspace-root))
          (bundled
           (or
            (and (boundp 'macher-agent--bundled-skills-dir) macher-agent--bundled-skills-dir)
            (and (boundp 'macher-agent-bundled-skills-directory)
                 macher-agent-bundled-skills-directory))))
      (when bundled
        (macher-agent-initialize-skills buffer-context bundled))
      (when (file-directory-p skills-dir)
        (macher-agent-initialize-skills buffer-context skills-dir)))))

(defun macher-agent-gptel-mode-setup ()
  "Initialise agent defaults for gptel buffers inside a workspace.

When operating outside a project workspace, permits standard gptel execution
and logs an informational message.

Return nil.

Side effects: Binds buffer-local gptel variables and restores session state."
  (let ((is-workspace
         (and default-directory
              (or (and (fboundp 'project-current) (project-current nil default-directory))
                  (and (fboundp 'vc-root-dir) (vc-root-dir))))))
    (if is-workspace
        (progn
          (make-local-variable 'gptel--preset)
          (make-local-variable 'gptel-tools)
          (make-local-variable 'gptel-model)
          (make-local-variable 'gptel-backend)
          (make-local-variable 'gptel-system-prompt)
          (make-local-variable 'gptel-temperature)
          (make-local-variable 'gptel-max-tokens)
          (make-local-variable 'gptel--tool-names)
          (make-local-variable 'gptel--backend-name)

          (setq-local gptel--set-buffer-locally t)

          (setq-local gptel-tools nil)

          (unless (macher-agent-subagent-p)
            (macher-agent--init-workspace-state
             (file-name-as-directory (macher-agent-root default-directory))))

          (when (fboundp 'gptel--restore-state)
            (let ((macher-agent--allow-gptel-restore t))
              (gptel--restore-state))))

      (message "Macher-Agent: Not in a recognised project workspace. Running standard gptel."))))

(defun macher-agent-force-enable ()
  "Force Macher Agent to treat current directory as a workspace root.

Return nil.

Side effects: Initialises workspace state and sets up gptel buffer defaults."
  (interactive)
  (unless (macher-agent-subagent-p)
    (macher-agent--init-workspace-state
     (file-name-as-directory (expand-file-name default-directory))))
  (macher-agent-setup-gptel-buffer)
  (message "Macher-Agent manually forced on for: %s" default-directory))

(defun macher-agent-branch-chat (new-name)
  "Clone current chat buffer establishing lineage and inheriting agent state.

NEW-NAME is the name string for the new branch buffer.

Return nil.

Side effects: Creates new chat buffer and sets buffer-local agent variables."
  (interactive "sNew branch name: ")
  (let* ((parent-buf (current-buffer))
         (parent-name (buffer-name parent-buf))
         (parent-mode major-mode)
         (content (buffer-string))
         (active-backend gptel-backend)
         (active-model gptel-model)
         (active-sys gptel-system-prompt)
         (active-skill (bound-and-true-p macher-agent--active-skill-sym)))

    (with-current-buffer (generate-new-buffer new-name)
      (funcall parent-mode)
      (gptel-mode)
      (insert content)

      (when-let* ((backend active-backend))
        (setq-local gptel-backend backend))
      (when-let* ((model active-model))
        (setq-local gptel-model model))
      (when-let* ((sys active-sys))
        (setq-local gptel-system-prompt sys))
      (when-let* ((skill active-skill))
        (setq-local macher-agent--active-skill-sym skill))

      (setq-local macher-agent-presets (with-current-buffer parent-buf macher-agent-presets))
      (setq-local macher-agent--active-ptc-primitives
                  (with-current-buffer parent-buf macher-agent--active-ptc-primitives))
      (setq-local gptel-tools (with-current-buffer parent-buf gptel-tools))

      (switch-to-buffer (current-buffer)))))

(defun macher-agent-clear-context ()
  "Clear the persistent VFS context for the current sub-agent buffer.

Return nil.
Side effects: Clears `macher-agent--persistent-context' and resets FSM context."
  (interactive)
  (if (not (bound-and-true-p macher-agent--persistent-context))
      (message "Macher-Agent: No persistent context to clear in buffer '%s'." (buffer-name))
    (let*
        ((raw-ws (or (and (bound-and-true-p macher-agent--persistent-context)
                          (macher-agent--get-context-workspace macher-agent--persistent-context))
                     (ignore-errors (macher-workspace (current-buffer)))
                     (bound-and-true-p macher--workspace)))
         (ws (if (stringp raw-ws) (cons 'agent raw-ws) raw-ws))
         (fresh-ctx (when (and ws (fboundp 'macher-agent--make-vfs-context))
                      (macher-agent--make-vfs-context :workspace ws :contents nil)))
         (root (and ws (or (macher-agent-workspace-project-root ws)
                           (when (fboundp 'macher--workspace-root)
                             (macher--workspace-root ws)))))
         (expanded-root (and root (expand-file-name root))))
      (setq macher-agent--persistent-context fresh-ctx)
      (when (and expanded-root (not (bound-and-true-p macher-agent--is-subagent)))
        (when (fboundp 'macher-agent--register-active-workspace-root)
          (macher-agent--register-active-workspace-root expanded-root fresh-ctx)))
      (macher-agent-bridge-reset-fsm-context fresh-ctx)
      (message
       "Macher-Agent: VFS context successfully cleared. Agent reset to physical baseline."))))

(defun macher-agent-bridge-transmit ()
  "Bridge function to trigger network transmission in current buffer."
  (gptel-send))

(defun macher-agent-bridge-abort (buffer)
  "Bridge function to abort network transmission in BUFFER."
  (ignore-errors
    (condition-case nil
        (gptel-abort buffer)
      (wrong-number-of-arguments
       (gptel-abort)))))

;;;

(defun macher-agent-bridge-register-tool (name desc category args command-fn)
  "Bridge function to construct and register a gptel tool."
  (gptel-make-tool
   :name name
   :description desc
   :category category
   :args args
   :async t
   :function command-fn))

(defun macher-agent-bridge-reset-fsm-context (fresh-ctx)
  "Bridge function to reset context in the backend state machine to FRESH-CTX.

FRESH-CTX is the new context structure.

Return nil.
Side effects: Mutates info property list of the active backend FSM if bound."
  (when-let* ((fsm (macher-agent-get-active-fsm)))
    (let ((info (gptel-fsm-info fsm)))
      (when (or (plist-get info :macher--context)
                (plist-get info :macher-context)
                (plist-get info :macher-agent-context))
        (setf (gptel-fsm-info fsm) (plist-put info :macher--context fresh-ctx))
        (setf (gptel-fsm-info fsm) (plist-put info :macher-context fresh-ctx))
        (setf (gptel-fsm-info fsm)
              (plist-put info :macher-agent-context fresh-ctx))))))

;;;

(defun macher-agent-gptel--trigger-flush (fsm &rest _args)
  "Broadcast task completion using the associated agent context."
  (when (and fsm (fboundp 'gptel-fsm-state) (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT)))
    (let* ((macher-agent--active-fsm fsm)
           (info (ignore-errors (gptel-fsm-info fsm)))
           (b-prop (macher-agent--extract-prop info :buffer))
           (target-buf (if (eq b-prop 'macher-missing) nil b-prop))
           (agent-ctx (or (ignore-errors (macher-agent-resolve-context fsm))
                          (when (and target-buf (buffer-live-p target-buf))
                            (buffer-local-value 'macher-agent--persistent-context target-buf)))))
      (when agent-ctx
        (if (and target-buf (buffer-live-p target-buf))
            (with-current-buffer target-buf
              (setq-local macher-agent--pending-instructions-queue nil)
              (macher-agent-run-task-flush-hook agent-ctx))
          (macher-agent-run-task-flush-hook agent-ctx))))))

(provide 'macher-agent-gptel)
;;; macher-agent-gptel.el ends here
