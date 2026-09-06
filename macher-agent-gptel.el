;;; macher-agent-gptel.el --- Clean gptel boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; Clean gptel boundary implementation for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(eval-and-compile
  (require 'gv))
(require 'gptel)
(require 'macher-agent-core)
(require 'macher-agent-presets)

(defvar gptel--ptc-primitives nil)
(defvar gptel--boot-directive nil)
(defvar gptel--context-dir nil)
(defvar gptel--has-tools nil)
(defvar gptel--exclusive nil)

(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function vc-root-dir "vc-hooks" ())

(defvar gptel-system-prompt)
(defvar macher-agent--pending-callbacks)
(defvar macher-agent-token-multiplier 4
  "Estimated characters per token used for context truncation.")

(defvar-local macher-agent--active-fsm nil
  "Store active finite-state machine (FSM) instance for current buffer.

Return active FSM struct or nil.
Side effects: Buffer-local variable.")

(defvar-local macher-agent-fsm-id nil
  "Buffer-local identifier bridging user interface buffers to the state machine.

Stores the unique state machine identifier associated with the current
user interface buffer session.

Return the state machine identifier string or symbol, or nil if unassigned.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--is-restored-session nil
  "Track whether current buffer is restored from a saved state.

Indicate if buffer state was populated by restoring a saved session.

Return non-nil if session was restored, nil otherwise.
Side effects: Buffer-local variable.")

(cl-defstruct macher-agent-transmission-state
  target-buffer
  presets
  skills
  redirected-skill
  model
  temperature
  max-tokens
  tools
  ptc-primitives
  known-presets
  base-prompt
  directives
  compiled-prompt
  redirect-prompt
  context)

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

(defun macher-agent--fsm-hijack-transform (arg1 &optional arg2)
  "Adapter Boundary: Mutate the FSM to inject media, callbacks, and patches.
Dynamically resolves incoming arguments to support varying Emacs execution trampolines."

  (let* ((callback (cond ((functionp arg1) arg1)
                         ((functionp arg2) arg2)
                         (t nil)))
         (actual-fsm (cond ((and (recordp arg1) (not (functionp arg1))) arg1)
                           ((and arg2 (recordp arg2) (not (functionp arg2))) arg2)
                           (t (macher-agent-get-active-fsm))))

         (info (when actual-fsm (gptel-fsm-info actual-fsm)))
         (orig-cb (when info (plist-get info :callback)))
         (target-buf (when info (plist-get info :buffer))))

    (when (and target-buf (buffer-live-p target-buf))
      (with-current-buffer target-buf (setq-local macher-agent--active-fsm actual-fsm))
      (let* ((ctx (buffer-local-value 'macher-agent--persistent-context target-buf))
             (inf (gptel-fsm-info actual-fsm)))
        (setq inf (plist-put inf :origin-buffer target-buf))
        (when (macher-agent-valid-context-p ctx)
          (setq inf (plist-put inf :macher-agent-context ctx)))

        (let ((prompt (plist-get inf :prompt)))
          (when (and prompt (stringp prompt))
            (let ((raw-prompt (replace-regexp-in-string "^[#>*[:space:]]+" "" (string-trim prompt))))
              (setq inf (plist-put inf :prompt raw-prompt))
              (when ctx (setf (macher-agent-context-prompt ctx) raw-prompt)))))

        (macher-agent--set-fsm-info actual-fsm inf)
        (setf (gptel-fsm-info actual-fsm) inf)))

    (when (and actual-fsm orig-cb)
      (let ((inf (plist-put (gptel-fsm-info actual-fsm) :callback
                            (lambda (res)
                              (when (and target-buf (buffer-live-p target-buf))
                                (with-current-buffer target-buf (setq-local macher-agent--active-fsm nil)))
                              (funcall orig-cb res)))))
        (macher-agent--set-fsm-info actual-fsm inf)
        (setf (gptel-fsm-info actual-fsm) inf)))

    (when actual-fsm
      (let* ((handlers (gptel-fsm-handlers actual-fsm))
             (all-states (delete-dups (append (mapcar #'car handlers) '(WAIT DONE ERRS ABRT))))
             (augmented-handlers
              (cl-loop for state in all-states
                       for funcs = (alist-get state handlers)
                       collect (cons state
                                     (cond
                                      ((eq state 'WAIT)
                                       (if (memq #'macher-agent--inject-media-fsm-logic funcs) funcs (cons #'macher-agent--inject-media-fsm-logic funcs)))
                                      ((memq state '(DONE ERRS ABRT))
                                       (if (memq #'macher-agent-gptel--trigger-flush funcs) funcs (append funcs (list #'macher-agent-gptel--trigger-flush))))
                                      (t funcs))))))
        (macher-agent--set-fsm-handlers actual-fsm augmented-handlers)
        (setf (gptel-fsm-handlers actual-fsm) augmented-handlers)))

    (when callback (funcall callback))))

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
           (ctx restored-ctx))
      (when ctx
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
          (let* ((sym-name (match-string-no-properties 1))
                 (sym (intern sym-name))
                 (is-valid
                  (with-current-buffer orig-buf
                    (or (assoc sym known)
                        (when (bound-and-true-p macher-agent-global-skills-alist)
                          (assoc sym macher-agent-global-skills-alist))
                        (when-let* ((ctx (bound-and-true-p macher-agent--persistent-context)))
                          (when (fboundp 'macher-agent-workspace-skills-alist)
                            (ignore-errors (assoc sym (macher-agent-workspace-skills-alist ctx)))))
                        (when (fboundp 'gptel-get-tool)
                          (or (ignore-errors (gptel-get-tool sym-name))
                              (ignore-errors (gptel-get-tool (replace-regexp-in-string "-" "_" sym-name)))
                              (ignore-errors (gptel-get-tool (replace-regexp-in-string "_" "-" sym-name)))))))))
            (when is-valid
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
      (macher-agent--set-fsm-info fsm new-info))))

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

(defun macher-agent-presets-pipe--initialise-skills (state)
  "Initialise skills against transmission STATE struct."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((buf (or (macher-agent-transmission-state-target-buffer state)
                  (current-buffer)))
         (context (or (macher-agent-transmission-state-context state)
                      (when (and buf (buffer-live-p buf))
                        (buffer-local-value 'macher-agent--persistent-context buf)))))
    (when (and context buf (buffer-live-p buf))
      (with-current-buffer buf
        (unless (and (local-variable-p 'gptel--known-presets buf)
                     (buffer-local-value 'gptel--known-presets buf))
          (when (local-variable-p 'gptel--known-presets buf)
            (macher-agent-initialize-skills context))))
      (when (local-variable-p 'gptel--known-presets buf)
        (setf (macher-agent-transmission-state-known-presets state)
              (buffer-local-value 'gptel--known-presets buf)))))
  state)


(defun macher-agent--transformer-sync-context (orig-buf context)
  "Sync CONTEXT struct to live ORIG-BUF."
  (when (and (macher-agent-context-p orig-buf) (bufferp context))
    (cl-rotatef orig-buf context))
  (cl-check-type context macher-agent-context)
  (cl-check-type orig-buf buffer)
  (when (buffer-live-p orig-buf)
    (macher-agent--inject-context-state context (buffer-local-value 'gptel-directives orig-buf))
    (when (fboundp 'macher-agent--auto-sync-context)
      (macher-agent--auto-sync-context context)))
  (let* ((active-fsm (macher-agent-get-active-fsm))
         (info-prompt (when active-fsm
                        (ignore-errors (plist-get (gptel-fsm-info active-fsm) :prompt)))))
    (when info-prompt
      (unless (macher-agent-context-prompt context)
        (setf (macher-agent-context-prompt context) info-prompt)))
    (when (and active-fsm (fboundp 'macher-agent--inject-context-into-fsm-info))
      (macher-agent--inject-context-into-fsm-info context active-fsm)))
  context)

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

(defvar macher-agent-transmission-pipeline-functions nil
  "Hook for transmission pipeline functions.")

(defun macher-agent-transmission-install ()
  "Register core transmission pipeline steps in strict execution order."
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--hydrate-base-state 10)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--apply-skills-and-presets 20)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--extract-redirect 30)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--inject-dynamic-context-tools 40)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--process-hidden-blocks 50)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--init-core-directives 60)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--append-boot-directive 70)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--inject-skill-directive 87)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--drain-thought-queue 90)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--compile-directives 100)
  (add-hook 'macher-agent-transmission-pipeline-functions
            #'macher-agent-pipe--inject-skill-directive 87))

(defun macher-agent-pipe--hydrate-base-state (state)
  "Extract pristine local variables from target buffer into STATE."
  (cl-check-type state macher-agent-transmission-state)
  (let ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                        (current-buffer))))
    (when (and target-buf (buffer-live-p target-buf))
      (with-current-buffer target-buf
        (setf (macher-agent-transmission-state-model state) gptel-model)
        (setf (macher-agent-transmission-state-base-prompt state) gptel-system-prompt)
        (setf (macher-agent-transmission-state-compiled-prompt state) gptel-system-prompt)
        (setf (macher-agent-transmission-state-temperature state) (bound-and-true-p gptel-temperature))
        (setf (macher-agent-transmission-state-max-tokens state) (bound-and-true-p gptel-max-tokens))
        (setf (macher-agent-transmission-state-tools state) gptel-tools)
        (setf (macher-agent-transmission-state-ptc-primitives state)
              (or (bound-and-true-p macher-agent-ptc-primitives)
                  (bound-and-true-p macher-agent--active-ptc-primitives)))
        (setf (macher-agent-transmission-state-known-presets state)
              (when (local-variable-p 'gptel--known-presets target-buf)
                (buffer-local-value 'gptel--known-presets target-buf))))))
  state)

(defun macher-agent-pipe--apply-skills-and-presets (state)
  "Merge active presets and transmission skills into STATE."
  (cl-check-type state macher-agent-transmission-state)
  (let ((combined-modules (macher-agent-transmission-state-skills state)))
    (when combined-modules
      (let* ((known (macher-agent-transmission-state-known-presets state))
             (payload-plist (list :known-presets known
                                  :model (macher-agent-transmission-state-model state)
                                  :system (macher-agent-transmission-state-base-prompt state)
                                  :tools (macher-agent-transmission-state-tools state)
                                  :temperature (macher-agent-transmission-state-temperature state)
                                  :max-tokens (macher-agent-transmission-state-max-tokens state)
                                  :ptc-primitives (macher-agent-transmission-state-ptc-primitives state)))
             (composed (macher-agent-compose-payload payload-plist combined-modules)))
        (when (plist-get composed :system)
          (setf (macher-agent-transmission-state-base-prompt state) (plist-get composed :system)))
        (when (plist-get composed :model)
          (setf (macher-agent-transmission-state-model state) (plist-get composed :model)))
        (when (plist-get composed :temperature)
          (setf (macher-agent-transmission-state-temperature state) (plist-get composed :temperature)))
        (when (plist-get composed :max-tokens)
          (setf (macher-agent-transmission-state-max-tokens state) (plist-get composed :max-tokens)))
        (when (plist-get composed :tools)
          (setf (macher-agent-transmission-state-tools state) (plist-get composed :tools)))
        (when (plist-get composed :ptc-primitives)
          (setf (macher-agent-transmission-state-ptc-primitives state) (plist-get composed :ptc-primitives))))))
  state)

(defun macher-agent-pipe--inject-dynamic-context-tools (state)
  "Inject dynamic context tools into STATE if target buffer size exceeds limits."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                         (current-buffer)))
         (max-chars (macher-agent--get-max-context-chars target-buf))
         (threshold (* max-chars 0.8))
         (buf-size (if (and target-buf (buffer-live-p target-buf))
                       (with-current-buffer target-buf (buffer-size))
                     0)))
    (when (> buf-size threshold)
      (let ((mem-tool (ignore-errors
                        (macher-agent-resolve-tool "search_conversation_history" nil nil nil))))
        (when (macher-tool-valid-p mem-tool)
          (setf (macher-agent-transmission-state-tools state)
                (append (macher-agent-transmission-state-tools state) (list mem-tool)))))))
  state)

(defun macher-agent-pipe--process-hidden-blocks (state)
  "Strip out UI-specific or hidden metadata before the LLM sees it.
Currently a STATE pass-through, but acts as a hook for sanitization."
  (cl-check-type state macher-agent-transmission-state)
  state)

(defun macher-agent-pipe--init-core-directives (state)
  "Mandate the task submission rule if `submit_task_result' is present in tools."
  (cl-check-type state macher-agent-transmission-state)
  (when (cl-some (lambda (tool)
                   (equal (macher-agent-canonical-tool-name tool) "submit_task_result"))
                 (macher-agent-transmission-state-tools state))
    (push "CRITICAL DIRECTIVE: You MUST use the `submit_task_result' tool to submit your final answer when you are completely finished. Do NOT output your final answer as standard text. IMMEDIATELY STOP after."
          (macher-agent-transmission-state-directives state)))
  state)

(defun macher-agent-pipe--append-boot-directive (state)
  "Append boot directive to STATE struct."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                         (current-buffer)))
         (boot-dir (when (and target-buf (buffer-live-p target-buf))
                     (with-current-buffer target-buf
                       (bound-and-true-p macher-agent--boot-directive)))))
    (when (and boot-dir (stringp boot-dir) (not (string-empty-p boot-dir))
               target-buf
               (not (with-current-buffer target-buf
                      (text-property-any (point-min) (point-max) 'gptel 'response))))
      (push boot-dir (macher-agent-transmission-state-directives state))))
  state)

(defun macher-agent-pipe--inject-skill-directive (state)
  "Inject available skills into the compiled prompt.

STATE is the transmission pipeline state struct.

Return STATE, possibly modified.
Side effects: Injects an `<available_skills>` directive into
`macher-agent-transmission-state-compiled-prompt' and
`macher-agent-transmission-state-directives' when use_skill tool is present."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((tools (macher-agent-transmission-state-tools state))
         (has-use-skill (cl-some (lambda (tool)
                                   (equal (macher-agent-canonical-tool-name tool) "use_skill"))
                                 tools)))
    (when has-use-skill
      (let* ((target-buf (macher-agent-transmission-state-target-buffer state))
             (buffer-presets
              (cond
               ((and target-buf (buffer-live-p target-buf))
                (cond
                 ((and (local-variable-p 'gptel--known-presets target-buf)
                       (buffer-local-value 'gptel--known-presets target-buf)))
                 ((and (local-variable-p 'macher-agent-presets target-buf)
                       (buffer-local-value 'macher-agent-presets target-buf)))
                 ((macher-agent-transmission-state-known-presets state))
                 ((macher-agent-transmission-state-presets state))
                 (t nil)))
               (t
                (or (when (and (local-variable-p 'gptel--known-presets)
                               (bound-and-true-p gptel--known-presets))
                      gptel--known-presets)
                    (when (and (local-variable-p 'macher-agent-presets)
                               (bound-and-true-p macher-agent-presets))
                      macher-agent-presets)
                    (macher-agent-transmission-state-known-presets state)
                    (macher-agent-transmission-state-presets state)))))
             (presets-list (if (and buffer-presets (atom buffer-presets))
                               (list buffer-presets)
                             buffer-presets))
             (seen (make-hash-table :test 'equal))
             (skill-lines nil))
        (dolist (entry presets-list)
          (let* ((sym (if (consp entry) (car entry) entry))
                 (name (cond ((symbolp sym) (symbol-name sym))
                             ((stringp sym) sym)
                             (t (format "%s" sym)))))
            (unless (gethash name seen)
              (puthash name t seen)
              (let ((desc (cond
                           ((and (consp entry) (listp (cdr entry)))
                            (plist-get (cdr entry) :description))
                           ((and (consp entry) (stringp (cdr entry)))
                            (cdr entry))
                           (t nil))))
                (push (if (and desc (stringp desc) (not (string-empty-p (string-trim desc))))
                          (format "- %s: %s" name (string-trim desc))
                        (format "- %s" name))
                      skill-lines)))))
        (let* ((skills-text (if skill-lines
                                (string-join (nreverse skill-lines) "\n")
                              ""))
               (directive (if (and skills-text (not (string-empty-p skills-text)))
                              (format "<available_skills>\n%s\n</available_skills>" skills-text)
                            "<available_skills>\n</available_skills>")))
          (push directive (macher-agent-transmission-state-directives state))
          (let ((current (or (macher-agent-transmission-state-compiled-prompt state)
                             (macher-agent-transmission-state-base-prompt state))))
            (setf (macher-agent-transmission-state-compiled-prompt state)
                  (if (and current (not (string-empty-p current)))
                      (concat current "\n\n" directive)
                    directive)))))))
  state)

(defun macher-agent-pipe--drain-thought-queue (state)
  "Inject the pending instructions queue from target buffer into STATE.
Retained across turns until task completion."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                         (current-buffer)))
         (queue (when (and target-buf (buffer-live-p target-buf))
                  (buffer-local-value 'macher-agent--pending-instructions-queue target-buf))))
    (dolist (instruction queue)
      (push instruction (macher-agent-transmission-state-directives state))))
  state)

(defun macher-agent-pipe--compile-directives (state)
  "Compile all collected directives in STATE and append them cleanly
to the base prompt for this single request frame."
  (cl-check-type state macher-agent-transmission-state)
  (let ((dirs (nreverse (macher-agent-transmission-state-directives state)))
        (sys (macher-agent-transmission-state-base-prompt state)))
    (if dirs
        (let ((compiled-directives (string-join dirs "\n\n")))
          (setf (macher-agent-transmission-state-compiled-prompt state)
                (concat (or sys "") "\n\n" compiled-directives)))
      (setf (macher-agent-transmission-state-compiled-prompt state) sys)))
  state)

(defun macher-agent--compile-transmission-payload (orig-buf presets skills redirected-skill &optional context)
  "Compile payload strictly for live ORIG-BUF using unary transmission pipeline steps."
  (cl-check-type orig-buf buffer)
  (let ((initial-state (make-macher-agent-transmission-state
                        :target-buffer orig-buf
                        :presets presets
                        :skills skills
                        :redirected-skill redirected-skill
                        :context (or context
                                     (when (buffer-live-p orig-buf)
                                       (buffer-local-value 'macher-agent--persistent-context orig-buf)))))
        (all-steps (macher-agent-get-pipeline-steps 'transmission)))
    (seq-reduce (lambda (state pipe-fn)
                  (cl-check-type state macher-agent-transmission-state)
                  (if (functionp pipe-fn)
                      (funcall pipe-fn state)
                    state))
                all-steps
                initial-state)))

(defun macher-agent-pipe--extract-redirect (state)
  "Route redirected skill text in STATE and natively merge its tools."
  (cl-check-type state macher-agent-transmission-state)
  (let ((redirected-skill (macher-agent-transmission-state-redirected-skill state))
        (target-buf (or (macher-agent-transmission-state-target-buffer state)
                        (current-buffer))))
    (when redirected-skill
      (let* ((known (if (and target-buf (buffer-live-p target-buf))
                        (with-current-buffer target-buf (bound-and-true-p gptel--known-presets))
                      (macher-agent-transmission-state-known-presets state)))
             (redirect-state (macher-agent-compose-payload (list :known-presets known)
                                                           (list redirected-skill)))
             (redirect-text (plist-get redirect-state :system))
             (redirect-tools (plist-get redirect-state :tools)))

        (when redirect-text
          (setf (macher-agent-transmission-state-redirect-prompt state) redirect-text))

        (when redirect-tools
          (setf (macher-agent-transmission-state-tools state)
                (macher-agent-normalize-tools
                 (append (macher-agent-transmission-state-tools state) redirect-tools)))))))
  state)

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
         (context (or (when active-fsm
                        (macher-agent-gptel-context-from-fsm active-fsm orig-buf))
                      (when (and orig-buf (buffer-live-p orig-buf))
                        (let ((bctx (buffer-local-value 'macher-agent--persistent-context orig-buf)))
                          (when (macher-agent-valid-context-p bctx) bctx))))))

    (when (buffer-live-p orig-buf)
      (with-current-buffer orig-buf
        (when active-fsm
          (setq-local macher-agent--active-fsm active-fsm))
        (when context
          (macher-agent--transformer-sync-context orig-buf context)
          (when-let* ((info-prompt (and info (plist-get info :prompt))))
            (unless (macher-agent-context-prompt context)
              (setf (macher-agent-context-prompt context) info-prompt))))))

    (when (and active-fsm context)
      (macher-agent--set-fsm-info active-fsm
                                  (plist-put (gptel-fsm-info active-fsm) :macher-agent-context context)))

    (let* ((buffer-presets (when (buffer-live-p orig-buf)
                             (buffer-local-value 'macher-agent-presets orig-buf)))
           (known (when (buffer-live-p orig-buf)
                    (buffer-local-value 'gptel--known-presets orig-buf)))

           (prompt-start (save-excursion
                           (goto-char (or (previous-single-property-change (point-max) 'gptel)
                                          (point-min)))
                           (point)))

           (extraction (macher-agent--extract-inline-skills prompt-start orig-buf))
           (inline-skills (car extraction))
           (inline-preset-used (cdr extraction))

           (transmission-skills (macher-agent--transformer-resolve-skills
                                 buffer-presets inline-skills known))
           (redirected-skill (macher-agent--transformer-detect-redirect
                              inline-preset-used prompt-start inline-skills))

           (state (macher-agent--compile-transmission-payload
                   orig-buf buffer-presets transmission-skills redirected-skill context))

           (payload (list :model (macher-agent-transmission-state-model state)
                          :backend (when (buffer-live-p orig-buf) (buffer-local-value 'gptel-backend orig-buf))
                          :temperature (macher-agent-transmission-state-temperature state)
                          :max-tokens (macher-agent-transmission-state-max-tokens state)
                          :tools (macher-agent-normalize-tools (macher-agent-transmission-state-tools state))
                          :ptc-primitives (macher-agent-transmission-state-ptc-primitives state))))

      (macher-agent--apply-payload-locally payload)

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
          (macher-agent--set-fsm-info active-fsm
                                      (plist-put (gptel-fsm-info active-fsm) :prompt redirect-text)))))

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

(defun macher-agent--get-ownership-keys (parent-ident)
  "Extract ownership tracking keys for string PARENT-IDENT."
  (cl-check-type parent-ident string)
  (let* ((p-buf (get-buffer parent-ident))
         (keys (list parent-ident)))
    (when-let* ((p-buf)
                ((buffer-live-p p-buf))
                (ctx (buffer-local-value 'macher-agent--persistent-context p-buf))
                (p-ws (ignore-errors (macher-agent-context-workspace ctx)))
                (p-ws-root (ignore-errors (macher-agent-workspace-project-root p-ws)))
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

(defun macher-agent--reap-child-if-ready (child-ident &optional force-terminal)
  "Evaluate reap conditions strictly for live CHILD-IDENT buffer."
  (cl-check-type child-ident buffer)
  (when (buffer-live-p child-ident)
    (with-current-buffer child-ident
      (let ((ready (macher-agent-ready-to-reap-p))
            (finished (bound-and-true-p macher-agent-task-finished))
            (eph (bound-and-true-p macher-agent--is-ephemeral))
            (bg (bound-and-true-p macher-agent--is-background)))
        (when (or ready
                  (and (or eph bg) finished)
                  (and force-terminal finished))
          (setq-local macher-agent--ready-to-reap t)
          (macher-agent--schedule-buffer-reap child-ident)
          (macher-agent--remove-active-subagent-registries child-ident))))))

(defun macher-agent--clean-ownership-registry (parent-keys &optional force-terminal)
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
                    (with-current-buffer c-buf
                      (let ((eph (bound-and-true-p macher-agent--is-ephemeral)))
                        (not (or (macher-agent-ready-to-reap-p)
                                 (and (or eph force-terminal)
                                      (bound-and-true-p macher-agent-task-finished))))))))
                current-list)))
          (if remaining
              (puthash k remaining macher-agent--a2a-ownership)
            (remhash k macher-agent--a2a-ownership)))))))

(defun macher-agent-sweep-subagents (originator-name &optional force-terminal)
  "Execute sweep exclusively for string ORIGINATOR-NAME.
When FORCE-TERMINAL is non-nil, cleans up both ephemeral and non-ephemeral children."
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

(defun macher-agent-post-response-reaper (_beg _end)
  "Reap finished buffer or sweep sub-agents for orchestrator on completion.

Check if current buffer is ready to be reaped or finished.  If not ready,
conclude the workflow and sweep all owned child buffers.

_BEG is the starting position of the text region integer.
_END is the ending position of the text region integer.

Return nil.
Side effects: May schedule current buffer for asynchronous disposal or
trigger `macher-agent-sweep-subagents`."
  (let* ((cur (current-buffer))
         (ready (macher-agent-ready-to-reap-p))
         (finished (bound-and-true-p macher-agent-task-finished))
         (eph (bound-and-true-p macher-agent--is-ephemeral))
         (bg (bound-and-true-p macher-agent--is-background))
         (task-id (bound-and-true-p macher-agent--current-task-id)))

    (when (and task-id (not finished) (not ready))
      (let ((cb (when (boundp 'macher-agent--pending-callbacks)
                  (gethash task-id macher-agent--pending-callbacks))))
        (when cb
          (remhash task-id macher-agent--pending-callbacks)
          (funcall cb (list :status 'error :error "Sub-agent halted before submitting task result." :task-id task-id)))))

    (if (or ready
            (and (or eph bg) finished))
        (progn
          (setq-local macher-agent--ready-to-reap t)
          (macher-agent--schedule-buffer-reap cur))
      (let* ((matching-fsm (macher-agent-get-active-fsm))
             (info (when matching-fsm (macher-agent--extract-fsm-info matching-fsm)))
             (fsm-buf (when info (plist-get info :buffer)))
             (matching-info (when (and fsm-buf (eq fsm-buf cur)) info))
             (tool-calls (and matching-info (plist-get matching-info :tool-use))))
        (unless tool-calls
          (macher-agent-sweep-subagents (buffer-name cur) t))))))

(defun macher-agent--make-transmit-response-hook (error-cb &optional err-cb2)
  "Build response hook relying strictly on ERROR-CB function closure."
  (let ((actual-error-cb (if (and err-cb2 (functionp err-cb2)) err-cb2 error-cb)))
    (cl-check-type actual-error-cb function)
    (let ((hook-fn nil))
      (setq
       hook-fn
       (lambda (_beg _end)
         (let* ((res (string-trim (buffer-substring-no-properties (point-min) (point-max))))
                (fsm (macher-agent-get-active-fsm))
                (info (and fsm (fboundp 'gptel-fsm-info) (ignore-errors (gptel-fsm-info fsm))))
                (err-data (and info (plist-get info :error))))
           (cond
            (err-data
             (let ((err-msg (if (stringp err-data) err-data
                              (or (plist-get err-data :message)
                                  (format "%s" err-data)))))
               (funcall actual-error-cb err-msg)))
            ((string-empty-p res)
             (funcall actual-error-cb "Buffer stopped silently or returned empty."))
            ((not (bound-and-true-p macher-agent-task-finished))
             (funcall actual-error-cb "Agent halted without invoking submit_task_result.")))
           (remove-hook 'gptel-post-response-functions hook-fn t))))
      hook-fn)))

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
         (ctx (when (and target-buffer (buffer-live-p target-buffer))
                (buffer-local-value 'macher-agent--persistent-context target-buffer)))
         (success-cb (or (plist-get callbacks :on-success)
                         (plist-get callbacks :success-cb)))
         (error-cb (or (plist-get callbacks :on-error)
                       (plist-get callbacks :error-cb))))
    (when (and ctx (macher-agent-valid-context-p ctx) sys-msg)
      (unless (macher-agent-context-prompt ctx)
        (setf (macher-agent-context-prompt ctx) sys-msg)))
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

(defun macher-agent-gptel--fsm-target-buffer (fsm)
  "Extract target buffer from FSM safely."
  (when fsm
    (let* ((info (ignore-errors (macher-agent--extract-fsm-info fsm))))
      (when (macher-agent--plist-p info)
        (or (plist-get info :origin-buffer)
            (plist-get info :buffer))))))

(defun macher-agent-gptel-context-from-fsm (fsm &optional target-buf)
  "Extract contextual targets derived explicitly from compliant FSM context."
  (if (macher-agent-context-p fsm)
      (progn
        (cl-check-type fsm macher-agent-context)
        fsm)
    (let* ((buf (or target-buf (macher-agent-gptel--fsm-target-buffer fsm) (current-buffer)))
           (info (ignore-errors (macher-agent--extract-fsm-info fsm)))
           (ctx-raw (when (macher-agent--plist-p info)
                      (or (plist-get info :macher-agent-context)
                          (plist-get info :context))))
           (ctx (cond
                 ((and ctx-raw (macher-agent-valid-context-p ctx-raw)) ctx-raw)
                 ((and buf (buffer-live-p buf))
                  (let ((bctx (buffer-local-value 'macher-agent--persistent-context buf)))
                    (when (macher-agent-valid-context-p bctx) bctx))))))
      (when ctx
        (cl-check-type ctx macher-agent-context))
      ctx)))

(defun macher-agent-gptel-spoof-tool-ui (target-buf tool-name)
  "Simulate tool UI execution precisely for TOOL-NAME symbol in TARGET-BUF."
  (cl-check-type target-buf buffer)
  (cl-check-type tool-name symbol)
  (let ((name-str (replace-regexp-in-string "_" "-" (symbol-name tool-name))))
    (when (buffer-live-p target-buf)
      (with-current-buffer target-buf
        (let ((fsm (or (macher-agent-get-active-fsm)
                       (bound-and-true-p macher-agent--active-fsm)
                       (bound-and-true-p gptel--fsm-last))))
          (if (and fsm (fboundp 'gptel-fsm-info) (fboundp 'gptel--update-tool-call))
              (let* ((info (gptel-fsm-info fsm))
                     (mock-info (copy-sequence info))
                     (spoofed-calls (list (list :name (format "PTC: %s" name-str)))))
                (setq mock-info (plist-put mock-info :tool-use spoofed-calls))
                (setq mock-info (plist-put mock-info :buffer target-buf))

                (unwind-protect
                    (progn
                      (macher-agent--set-fsm-info fsm mock-info)
                      (gptel--update-tool-call fsm))
                  (macher-agent--set-fsm-info fsm info)))
            (when (fboundp 'gptel--update-status)
              (gptel--update-status
               (concat
                (propertize " Calling tool (" 'face 'mode-line-emphasis)
                (propertize (format "PTC: %s" name-str) 'face 'font-lock-keyword-face)
                (propertize ")" 'face 'mode-line-emphasis))))))
        t))))

(defun macher-agent--perform-pending-media-injection (fsm)
  "Inject base64 media directly into FSM payload enforcing a strict string contract."
  (let* ((info (ignore-errors (gptel-fsm-info fsm)))
         (ctx (when (macher-agent--plist-p info)
                (or (plist-get info :macher-agent-context)
                    (plist-get info :context))))
         (media (when ctx
                  (if (macher-agent-context-p ctx)
                      (or (macher-agent-context-media-queue ctx)
                          (plist-get (macher-agent-context-plugins ctx) :pending-media))
                    (when (macher-agent--plist-p ctx)
                      (or (plist-get ctx :media-queue)
                          (plist-get ctx :pending-media)))))))
    (when media
      (unless (or (stringp media) (consp media))
        (error "SECURITY ERROR: Media injection strictly requires a base64 encoded string or media descriptor."))
      (let* ((backend (plist-get info :backend))
             (data (plist-get info :data))
             (msg-text "Tool execution complete. Here is the requested visual data:")

             (parsed-list (when (fboundp 'gptel--parse-list)
                            (gptel--parse-list backend (list msg-text))))
             (msg-plist (if parsed-list
                            (car parsed-list)
                          (list :role "user" :content msg-text)))
             
             (prompts (list msg-plist))
             (media-spec (if (consp media) media (list media :mime "image/png")))
             (gptel-context (list media-spec)))
        (when (and backend (fboundp 'gptel--inject-media))
          (cl-letf (((symbol-function 'gptel--base64-encode)
                     (lambda (file)
                       (if (and (stringp file) (or (> (length file) 1000) (not (file-exists-p file))))
                           file
                         (with-temp-buffer
                           (insert-file-contents-literally file)
                           (base64-encode-region (point-min) (point-max) :no-line-break)
                           (buffer-string))))))
            (gptel--inject-media backend prompts)))
        (when (and backend data (fboundp 'gptel--inject-prompt))
          (gptel--inject-prompt backend data (car prompts))))
      (when (macher-agent-context-p ctx)
        (setf (macher-agent-context-media-queue ctx) nil)
        (when-let* ((plugins (macher-agent-context-plugins ctx)))
          (setf (macher-agent-context-plugins ctx)
                (plist-put (copy-sequence plugins) :pending-media nil)))))))

(defun macher-agent--inject-media-fsm-logic (fsm)
  "Inject pending tool media into FSM payload when entering WAIT state."
  (unless macher-agent--in-media-injection
    (macher-agent--perform-pending-media-injection fsm)))

;;; State Restoration

(defvar macher-agent--allow-gptel-restore nil
  "Control whether `gptel--restore-state' execution is permitted.

When non-nil, allow `gptel--restore-state' advice to execute restoration.

Return non-nil when state restoration is allowed, nil otherwise.
Side effects: Dynamic control variable.")

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
         (info (when fsm-obj (ignore-errors (gptel-fsm-info fsm-obj))))
         (fsm-tools (or (when info (plist-get info :tools))
                        (bound-and-true-p gptel-tools)))
         (authorised-names (mapcar #'macher-agent-canonical-tool-name fsm-tools)))
    (unless (and canonical-name (member canonical-name authorised-names))
      (list
       :block (format "ERROR: Tool '%s' is not accessible in this context or is no longer available. Please select another tool or approach."
                      (or canonical-name tool))))))

(defun macher-agent--log-gptel-pre-tool (tool &optional fsm &rest args)
  "Log GPTEL pre-tool call intent to context audit log.

TOOL is the tool structure or property list.
FSM is the optional state machine object.
ARGS represents additional arguments passed to the tool call.

Return nil.

Side effects: Appends tool call entry to active context audit log."
  (let ((context (or (when fsm (macher-agent-gptel-context-from-fsm fsm))
                     (macher-agent-gptel-context-from-fsm (macher-agent-get-active-fsm))
                     (bound-and-true-p macher-agent--persistent-context)))
        (tool-name (macher-agent-canonical-tool-name tool))
        (tool-args (if (and (macher-agent--plist-p tool) (plist-member tool :args))
                       (plist-get tool :args)
                     args)))
    (when (and context (fboundp 'macher-agent-log-tool-intent))
      (macher-agent-log-tool-intent context "gptel-tool" tool-name tool-args))))

(defun macher-agent-setup-gptel-buffer ()
  "Set up a gptel buffer with macher-agent capabilities.

Configure prompt transform and pre-tool call hooks in current buffer.

Return nil.
Side effects: Sets buffer-local variables and adds buffer-local hooks."
  (setq-local macher-agent--active-fsm nil)
  (when (bound-and-true-p macher-agent--is-restored-session)
    (setq-local macher-agent-presets nil)
    (setq-local macher-agent--is-restored-session nil))
  (add-hook 'gptel-prompt-transform-functions
            #'macher-agent-sync-prompt-transformer nil t)
  (dolist (transformer macher-agent-prompt-transformers)
    (add-hook 'gptel-prompt-transform-functions transformer t t))
  (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t))

(defun macher-agent--init-workspace-state (workspace-root)
  "Initialise the workspace state and active context for WORKSPACE-ROOT.

WORKSPACE-ROOT is the project root directory string.

Return nil.
Side effects: Sets buffer-local workspace state, registers active workspace
root, and adds prompt
transformer hook."
  (unless (bound-and-true-p macher-agent--is-workspace)
    (setq-local macher-agent--is-workspace t)
    (setq-local macher-agent--active-fsm nil)
    (let* ((expanded (file-truename (expand-file-name workspace-root)))
           (existing (bound-and-true-p macher-agent--persistent-context))
           (workspace (or (and existing (macher-agent-context-workspace existing))
                          (make-macher-agent-workspace :project-root expanded)))
           (canonical-context
            (or existing
                (when (fboundp 'macher-agent--make-vfs-context)
                  (macher-agent--make-vfs-context :workspace workspace :contents nil))))
           (buffer-context (if (and canonical-context (fboundp 'macher-agent--clone-context))
                               (macher-agent--clone-context canonical-context)
                             canonical-context)))
      (when (and buffer-context (fboundp 'macher-agent--inject-context-state))
        (macher-agent--inject-context-state buffer-context))
      (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)
      (when buffer-context
        (macher-agent-refresh-skills-and-tools buffer-context)))))

(defun macher-agent-gptel-mode-setup ()
  "Initialise agent defaults for gptel buffers inside a workspace.

When operating outside a project workspace, disables `macher-agent-mode',
ensures no hooks are attached, and signals an error to fail fast.

Return non-nil on success, or signals an error when not in a workspace."
  (let ((is-workspace
         (and default-directory
              (or (bound-and-true-p macher-agent--is-workspace)
                  (and (fboundp 'project-current)
                       (project-current nil default-directory))
                  (and (fboundp 'vc-root-dir)
                       (let ((default-directory default-directory))
                         (vc-root-dir)))))))
    (if (not is-workspace)
        (progn
          (when (bound-and-true-p macher-agent-mode)
            (setq macher-agent-mode nil))
          (remove-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer t)
          (dolist (transformer macher-agent-prompt-transformers)
            (remove-hook 'gptel-prompt-transform-functions transformer t))
          (remove-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope t)
          (user-error "Macher-Agent: Directory '%s' is not in a recognised project workspace" default-directory))
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

      (macher-agent--init-workspace-state
       (file-name-as-directory (macher-agent-root default-directory)))
      t)))

(defun macher-agent-force-enable ()
  "Force Macher Agent to treat current directory as a workspace root.

Return nil.

Side effects: Initialises workspace state and sets up gptel buffer defaults."
  (interactive)
  (setq-local macher-agent--is-workspace t)
  (macher-agent--init-workspace-state
   (file-name-as-directory (expand-file-name default-directory)))
  (if (fboundp 'macher-agent-mode)
      (funcall 'macher-agent-mode 1)
    (macher-agent-setup-gptel-buffer))
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
      (setq macher-agent--active-ptc-primitives
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
        ((raw-ws (when (bound-and-true-p macher-agent--persistent-context)
                   (macher-agent-context-workspace macher-agent--persistent-context)))
         (ws (if (stringp raw-ws) (cons 'agent raw-ws) raw-ws))
         (fresh-ctx (when (and ws (fboundp 'macher-agent--make-vfs-context))
                      (macher-agent--make-vfs-context :workspace ws :contents nil))))
      (setq-local macher-agent--persistent-context fresh-ctx)
      (when (and (bound-and-true-p macher-agent--active-fsm)
                 (fboundp 'gptel-fsm-info))
        (let ((inf (gptel-fsm-info macher-agent--active-fsm)))
          (when (macher-agent--plist-p inf)
            (setq inf (plist-put inf :macher-agent-context fresh-ctx))
            (if (fboundp 'macher-agent--set-fsm-info)
                (macher-agent--set-fsm-info macher-agent--active-fsm inf)
              (setf (gptel-fsm-info macher-agent--active-fsm) inf)))))
      (message
       "Macher-Agent: VFS context successfully cleared. Agent reset to physical baseline."))))

(defun macher-agent-send (&rest args)
  "Trigger transmission in current buffer using gptel-send.

ARGS are passed directly to `gptel-send'.

Return result of `gptel-send'.
Side effects: Initiates gptel network request."
  (apply #'gptel-send args))

;;;

(defun macher-agent-gptel--trigger-flush (fsm &rest _args)
  "Broadcast task completion using the associated agent context."
  (when (and fsm (fboundp 'gptel-fsm-state) (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT)))
    (when-let* ((info (ignore-errors (gptel-fsm-info fsm)))
                (target-buf (plist-get info :buffer)))
      (let* ((raw-ctx (or (plist-get info :macher-agent-context)
                          (when (buffer-live-p target-buf)
                            (buffer-local-value 'macher-agent--persistent-context target-buf))))
             (agent-ctx (when (macher-agent-valid-context-p raw-ctx) raw-ctx)))
        (when (buffer-live-p target-buf)
          (with-current-buffer target-buf
            (setq-local macher-agent--active-fsm nil)
            (setq-local macher-agent--pending-instructions-queue nil))
          (macher-agent-sweep-subagents (buffer-name target-buf) t))
        (when (and agent-ctx (fboundp 'macher-agent-run-task-flush-hook))
          (macher-agent-run-task-flush-hook agent-ctx))))))

(defun macher-agent-get-active-fsm (&optional current-fsm)
  "Correlate UI operations utilizing validated CURRENT-FSM string map."
  (when (stringp current-fsm)
    (cl-check-type current-fsm string))
  (or (and current-fsm
           (not (macher-agent-transit-payload-p current-fsm))
           (not (macher-agent-context-p current-fsm))
           (not (bufferp current-fsm))
           current-fsm)
      (bound-and-true-p macher-agent--active-fsm)
      (bound-and-true-p gptel--fsm)))

(defun macher-agent--extract-fsm-info (fsm)
  "Correlate background FSM info using explicitly parsed struct definitions."
  (cl-check-type fsm gptel-fsm)
  (gptel-fsm-info fsm))

(defun macher-agent--set-fsm-info (fsm info)
  "Attach specific INFO sequence targeting explicit FSM struct parameters."
  (unless (recordp fsm)
    (cl-check-type fsm vector))
  (when fsm
    (cond
     ((fboundp 'set-gptel-fsm-info)
      (set-gptel-fsm-info fsm info))
     ((or (recordp fsm) (vectorp fsm))
      (when (> (length fsm) 4)
        (aset fsm 4 info)))
     ((fboundp '\(setf\ gptel-fsm-info\))
      (\(setf\ gptel-fsm-info\) info fsm))
     (t nil))
    info))

(defun macher-agent--set-fsm-handlers (fsm handlers)
  "Attach defined HANDLERS targeting explicit FSM struct definitions."
  (unless (recordp fsm)
    (cl-check-type fsm vector))
  (when fsm
    (cond
     ((fboundp 'set-gptel-fsm-handlers)
      (set-gptel-fsm-handlers fsm handlers))
     ((or (recordp fsm) (vectorp fsm))
      (when (> (length fsm) 3)
        (aset fsm 3 handlers)))
     ((fboundp '\(setf\ gptel-fsm-handlers\))
      (\(setf\ gptel-fsm-handlers\) handlers fsm))
     (t nil))
    handlers))

(defun macher-agent--transform-inject-context (fsm async-fn)
  "Inject context variables wrapped by ASYNC-FN closure into FSM struct."
  (when (and (functionp fsm) (not (functionp async-fn)))
    (cl-rotatef fsm async-fn))
  (cl-check-type fsm gptel-fsm)
  (cl-check-type async-fn function)
  (let* ((info (gptel-fsm-info fsm))
         (target-buf (when (macher-agent--plist-p info) (plist-get info :buffer))))
    (when (and target-buf (buffer-live-p target-buf))
      (with-current-buffer target-buf
        (setq macher-agent--active-fsm fsm))
      (let ((ctx (buffer-local-value 'macher-agent--persistent-context target-buf)))
        (when (and ctx (macher-agent-valid-context-p ctx))
          (when (fboundp 'macher-agent--inject-context-into-fsm-info)
            (funcall 'macher-agent--inject-context-into-fsm-info ctx fsm))))))
  (funcall async-fn))

(defvar macher-agent--wrapped-tools-hash (make-hash-table :test 'eq)
  "Track wrapped `gptel-tool' instances in a hash table.")

(defun macher-agent--extract-tool-name (tool)
  "Extract canonical name string from TOOL struct, string, symbol, or alist pair."
  (let ((actual-tool (if (consp tool) (cdr tool) tool)))
    (cond
     ((and (fboundp 'gptel-tool-p) (gptel-tool-p actual-tool))
      (gptel-tool-name actual-tool))
     ((stringp tool) tool)
     ((symbolp tool) (symbol-name tool))
     (t (error "Invalid tool representation: %S" tool)))))

(defun macher-agent--wrap-single-tool (tool)
  "Wrap single TOOL to intercept orphaned context.

Modify function slot of `gptel-tool' structure TOOL to
intercept calls, transfer user prompt from orphaned
context to sub-agent persistent context. If TOOL is
`search_in_workspace', fully replace it.

Return non-nil if TOOL was wrapped, or nil if already
wrapped.

Side effects: Mutates function slot of TOOL and updates
`macher-agent--wrapped-tools-hash'."
  (when-let* (((not (gethash tool macher-agent--wrapped-tools-hash)))
              (tool-name (macher-agent--extract-tool-name tool)))

    (if-let* (((string= (format "%s" tool-name) "search_in_workspace"))
              (replacement (macher-agent--get-search-in-workspace-tool)))
        (progn
          (setf (gptel-tool-name tool) (gptel-tool-name replacement)
                (gptel-tool-function tool) (gptel-tool-function replacement)
                (gptel-tool-description tool) (gptel-tool-description replacement)
                (gptel-tool-args tool) (gptel-tool-args replacement)
                (gptel-tool-category tool) (gptel-tool-category replacement)
                (gptel-tool-async tool) (gptel-tool-async replacement)
                (gptel-tool-confirm tool) (gptel-tool-confirm replacement)
                (gptel-tool-include tool) (gptel-tool-include replacement))
          (puthash tool t macher-agent--wrapped-tools-hash))

      (when-let* ((category (gptel-tool-category tool))
                  ((string= (format "%s" category) "macher"))
                  (orig-fn (gptel-tool-function tool)))
        (setf (gptel-tool-function tool)
              (lambda (orphaned-context callback &rest args)
                (unless (and (fboundp 'macher-context-p) (macher-context-p orphaned-context))
                  (error "Macher Agent: Tool called without an upstream context"))

                (let* ((fsm (macher-agent-get-active-fsm))
                       (target-buf (or (when (and fsm (fboundp 'gptel-fsm-info))
                                         (ignore-errors (plist-get (gptel-fsm-info fsm) :buffer)))
                                       (current-buffer)))
                       (agent-ctx (or (when fsm
                                        (macher-agent-gptel-context-from-fsm fsm target-buf))
                                      (when (and target-buf (buffer-live-p target-buf))
                                        (buffer-local-value 'macher-agent--persistent-context target-buf)))))

                  (unless agent-ctx
                    (error "Macher Agent: Could not resolve master agent context from buffer %s" target-buf))

                  (let* ((before-contents (macher-agent--get-context-contents agent-ctx))
                         (root (or (when (macher-agent-context-p agent-ctx)
                                     (macher-agent-context-project-root agent-ctx))
                                   (macher-agent-context-root agent-ctx)
                                   default-directory))
                         (partitioned (if (fboundp 'macher-agent--partition-vfs-entries)
                                          (macher-agent--partition-vfs-entries before-contents root)
                                        (cons nil before-contents)))
                         (buffer-entries (car partitioned))
                         (file-entries (cdr partitioned))
                         (marshalled-files
                          (mapcar (lambda (entry)
                                    (let ((path (if (macher-agent-vfs-entry-p entry) (macher-agent-vfs-entry-path entry) (car-safe entry)))
                                          (orig (if (macher-agent-vfs-entry-p entry) (macher-agent-vfs-entry-orig entry) (when (consp (cdr-safe entry)) (cadr entry))))
                                          (curr (if (macher-agent-vfs-entry-p entry) (macher-agent-vfs-entry-curr entry) (if (consp (cdr-safe entry)) (cddr entry) (cdr-safe entry)))))
                                      (cons path (cons orig curr))))
                                  file-entries))
                         (ws (or (when (macher-agent-context-p agent-ctx)
                                   (macher-agent-context-workspace agent-ctx))
                                 (when (fboundp 'macher-context-workspace)
                                   (ignore-errors (macher-context-workspace orphaned-context)))))
                         (proxy (when (fboundp 'macher--make-context)
                                  (let ((macher-agent--persistent-context nil))
                                    (macher--make-context
                                     :workspace ws
                                     :contents marshalled-files
                                     :prompt (macher-agent-context-prompt agent-ctx))))))

                    (unless proxy
                      (error "Macher Agent: Failed to construct upstream proxy context"))

                    (let* ((res (with-current-buffer target-buf
                                  (apply orig-fn proxy callback args)))
                           (raw-after-files (if (fboundp 'macher-context-contents)
                                                (with-no-warnings (macher-context-contents proxy))
                                              marshalled-files))
                           (after-files
                            (if (listp raw-after-files)
                                (mapcar (lambda (item)
                                          (cond
                                           ((macher-agent-vfs-entry-p item) item)
                                           ((consp item)
                                            (let ((path (car item))
                                                  (rest (cdr item)))
                                              (if (consp rest)
                                                  (make-macher-agent-vfs-entry :path path :orig (car rest) :curr (cdr rest))
                                                (make-macher-agent-vfs-entry :path path :orig nil :curr rest))))
                                           (t item)))
                                        raw-after-files)
                              raw-after-files))
                           (after-contents (append buffer-entries after-files)))

                      (macher-agent--set-context-contents agent-ctx after-contents)
                      (unless (equal before-contents after-contents)
                        (macher-agent--set-context-dirty-p agent-ctx t))
                      res)))))
        (puthash tool t macher-agent--wrapped-tools-hash)))))

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
                        (let ((name (macher-agent--extract-tool-name item)))
                          (and name (string= (format "%s" name) "search_in_workspace"))))
                      macher-tools)))
      (setcdr tools-entry filtered)
      (dolist (item filtered)
        (let ((tool (if (consp item) (cdr item) item)))
          (when (or (and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
                    (macher-tool-valid-p tool))
            (macher-agent--wrap-single-tool tool)))))))

(defun macher-agent--get-search-in-workspace-tool ()
  "Return the tool struct defined in skills/scripts/search_in_workspace.el."
  (let ((tool (when (boundp 'macher-agent-search-in-workspace-tool)
                macher-agent-search-in-workspace-tool)))
    (unless (and tool (or (and (fboundp 'gptel-tool-p) (gptel-tool-p tool))
                          (macher-tool-valid-p tool)))
      (let* ((script-candidates
              (delq nil
                    (list
                     (when (bound-and-true-p macher-agent--bundled-skills-dir)
                       (expand-file-name "scripts/search_in_workspace.el" macher-agent--bundled-skills-dir))
                     (when (bound-and-true-p macher-agent-bundled-skills-directory)
                       (expand-file-name "scripts/search_in_workspace.el" macher-agent-bundled-skills-directory))
                     (locate-file "skills/scripts/search_in_workspace.el" load-path)
                     (locate-file "search_in_workspace.el" load-path)
                     (expand-file-name "skills/scripts/search_in_workspace.el" default-directory)
                     (let ((root (locate-dominating-file default-directory "skills")))
                       (when root
                         (expand-file-name "skills/scripts/search_in_workspace.el" root))))))
             (found (cl-find-if #'file-exists-p script-candidates)))
        (when found
          (load found nil t))
        (setq tool (when (boundp 'macher-agent-search-in-workspace-tool)
                     macher-agent-search-in-workspace-tool))))
    (or tool
        (when (bound-and-true-p macher-agent-tools-registry)
          (gethash "search_in_workspace" macher-agent-tools-registry))
        (when (bound-and-true-p gptel--known-tools)
          (or (alist-get "search_in_workspace" (alist-get "perception" gptel--known-tools nil nil #'equal) nil nil #'equal)
              (ignore-errors (gptel-get-tool "search_in_workspace")))))))

(defun macher-agent-bridge-abort (buf)
  "Abort operations exclusively in live BUF object."
  (cl-check-type buf buffer)
  (when (and (buffer-live-p buf) (fboundp 'gptel-abort))
    (with-current-buffer buf
      (condition-case nil
          (gptel-abort buf)
        (wrong-number-of-arguments
         (gptel-abort))))))

(provide 'macher-agent-gptel)
;;; macher-agent-gptel.el ends here
