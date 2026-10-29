;;; macher-agent-gptel.el --- Clean gptel boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; Clean gptel boundary implementation for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'rx)
(eval-and-compile
  (require 'gv))
(require 'gptel)
(require 'macher-agent-core)
(require 'macher-agent-presets)

(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function vc-root-dir "vc-hooks" ())

(defvar gptel-system-prompt)
(defvar gptel-directives)
(defvar gptel--known-presets)
(defvar gptel--known-tools)
(defvar gptel--known-backends)

(defvar-local macher-agent--is-restored-session nil
  "Track whether current buffer is restored from a saved state.")

(defvar-local macher-agent--is-workspace nil
  "Flag whether current buffer is initialised as a Macher Agent workspace.")
(put 'macher-agent--is-workspace 'permanent-local t)

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
  context
  plugins)

(defcustom macher-agent-prompt-transformers
  '(macher-agent-transformer-deduplicate-tools)
  "List of transformer functions to clean buffer prior to transmission."
  :type '(repeat function)
  :group 'macher-agent)

(defcustom macher-agent-max-duplicate-tools 2
  "Maximum number of identical tool calls to retain in context history."
  :type 'integer
  :group 'macher-agent)

(defun macher-agent--fsm-hijack-transform (arg1 &optional arg2)
  "Mutate the FSM to inject media, callbacks, and patches."
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
      (with-current-buffer target-buf
        (setq-local macher-agent--active-fsm actual-fsm))
      (let* ((ctx (macher-agent-resolve-context target-buf))
             (inf (gptel-fsm-info actual-fsm)))
        (setq inf (plist-put inf :origin-buffer target-buf))
        (when (macher-agent-valid-context-p ctx)
          (setf (macher-agent-context-owner-buffer ctx) target-buf)
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
                                (with-current-buffer target-buf
                                  (setq-local macher-agent--active-fsm nil)))
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
                                       (if (memq #'macher-agent--inject-media-fsm-logic funcs)
                                           funcs
                                         (cons #'macher-agent--inject-media-fsm-logic funcs)))
                                      ((memq state '(DONE ERRS ABRT))
                                       (if (memq #'macher-agent-gptel--trigger-flush funcs)
                                           funcs
                                         (append funcs (list #'macher-agent-gptel--trigger-flush))))
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
    (when-let* ((ctx (macher-agent-resolve-context (current-buffer))))
      (macher-agent-initialize-skills ctx))
    (when (fboundp 'gptel--update-header-line)
      (gptel--update-header-line))))

(defun macher-agent--get-max-context-chars (&optional buf)
  "Resolve max context characters based on the model in BUF."
  (let* ((target-buf (or buf (current-buffer)))
         (model (when (buffer-live-p target-buf)
                  (buffer-local-value 'gptel-model target-buf)))
         (model-sym (if (stringp model) (intern model) model)))
    (or (alist-get model-sym macher-agent-max-context-chars)
        (alist-get nil macher-agent-max-context-chars)
        2000000)))

;;; Prompt Transformation and Inline Skill Extraction

(defun macher-agent--extract-inline-skills (prompt-start orig-buf)
  "Extract inline skill tags starting from PROMPT-START in current buffer."
  (let ((matched-skills nil)
        (inline-preset-used nil)
        (known (with-current-buffer orig-buf (bound-and-true-p gptel--known-presets)))
        (first-non-ws (save-excursion
                        (goto-char prompt-start)
                        (skip-chars-forward " \t\r\n")
                        (point)))
        (stop nil))
    (save-excursion
      (goto-char prompt-start)
      (while (and (not stop)
                  (re-search-forward "@\\([[:alnum:]_-]+\\)" nil t))
        (when (or (= (match-beginning 0) (point-min))
                  (memq (char-before (match-beginning 0)) '(32 ?\t ?\n ?\r 62)))
          (let* ((match-start (match-beginning 0))
                 (sym-name (match-string-no-properties 1))
                 (sym (intern sym-name))
                 (is-cmd (when (= match-start first-non-ws)
                           (with-current-buffer orig-buf
                             (let ((spec (or (when (fboundp 'gptel-get-preset)
                                               (gptel-get-preset sym))
                                             (alist-get sym known)
                                             (when-let* ((ctx (macher-agent-resolve-context orig-buf)))
                                               (and (macher-agent-valid-context-p ctx)
                                                    (alist-get sym (macher-agent-context-skills ctx)))))))
                               (and spec (plist-get spec :is-command))))))
                 (is-valid
                  (or is-cmd
                      (with-current-buffer orig-buf
                        (or (assoc sym known)
                            (when (bound-and-true-p macher-agent-global-skills-alist)
                              (assoc sym macher-agent-global-skills-alist))
                            (when-let* ((ctx (macher-agent-resolve-context orig-buf)))
                              (when (fboundp 'macher-agent-workspace-skills-alist)
                                (ignore-errors (assoc sym (macher-agent-workspace-skills-alist ctx)))))
                            (when (fboundp 'gptel-get-tool)
                              (or (ignore-errors (gptel-get-tool sym-name))
                                  (ignore-errors (gptel-get-tool (replace-regexp-in-string "-" "_" sym-name)))
                                  (ignore-errors (gptel-get-tool (replace-regexp-in-string "_" "-" sym-name))))))))))
            (when is-valid
              (setq inline-preset-used t)
              (push sym matched-skills)
              (replace-match "")
              (when (looking-at "[ \t]+")
                (replace-match ""))
              (when is-cmd
                (setq stop t)))))))
    (cons (nreverse matched-skills) inline-preset-used)))

(defun macher-agent--update-fsm-info-from-transmission-state (fsm state)
  "Update FSM info plist with values from transmission STATE."
  (when (and fsm state)
    (let ((new-info (copy-sequence (gptel-fsm-info fsm))))
      (setq new-info (plist-put new-info :system (macher-agent-transmission-state-compiled-prompt state)))
      (setq new-info (plist-put new-info :model (macher-agent-transmission-state-model state)))
      (setq new-info (plist-put new-info :temperature (macher-agent-transmission-state-temperature state)))
      (setq new-info (plist-put new-info :max-tokens (macher-agent-transmission-state-max-tokens state)))
      (setq new-info (plist-put new-info :tools (macher-agent-transmission-state-tools state)))
      (macher-agent--set-fsm-info fsm new-info))))

(defun macher-agent-transformer-deduplicate-tools (async-fn _fsm)
  "Remove redundant tool usage blocks from context history."
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

(defun macher-agent--transformer-sync-context (orig-buf context)
  "Synchronise CONTEXT struct to live ORIG-BUF."
  (cl-check-type orig-buf buffer)
  (cl-check-type context macher-agent-context)
  (when (buffer-live-p orig-buf)
    (macher-agent--inject-context-state context (buffer-local-value 'gptel-directives orig-buf))
    (run-hook-with-args 'macher-agent-context-sync-hook context))
  (let* ((active-fsm (macher-agent-get-active-fsm))
         (info-prompt (when active-fsm
                        (ignore-errors (plist-get (gptel-fsm-info active-fsm) :prompt)))))
    (when (and info-prompt (not (string-empty-p (string-trim info-prompt))))
      (setf (macher-agent-context-prompt context) info-prompt)))
  context)

(defun macher-agent--transformer-resolve-skills (buffer-presets inline-skills known)
  "Resolve transmission skills from BUFFER-PRESETS and INLINE-SKILLS using KNOWN."
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

(defun macher-agent--transformer-detect-redirect (inline-preset-used inline-skills known ctx)
  "Detect whether INLINE-PRESET-USED should redirect to a command prompt."
  (when-let* ((_ inline-preset-used)
              (skill (car inline-skills)))
    (let* ((spec (or (when (and known (symbolp skill)) (alist-get skill known))
                     (when (and ctx (macher-agent-valid-context-p ctx) (symbolp skill))
                       (alist-get skill (macher-agent-context-skills ctx)))))
           (is-command (or (and (listp skill) (plist-get skill :is-command))
                           (and spec (plist-get spec :is-command)))))
      (when is-command
        skill))))

(defun macher-agent-transmission-install ()
  "Register core transmission pipeline steps in strict execution order."
  (setq macher-agent-active-fsm-function #'macher-agent-get-active-fsm)
  (setq macher-agent-fsm-info-function #'gptel-fsm-info)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--hydrate-base-state 10)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--apply-skills-and-presets 20)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--extract-redirect 30)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--inject-dynamic-context-tools 40)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--process-hidden-blocks 50)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--auto-enable-task-tool 54)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--init-core-directives 58)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--append-boot-directive 70)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--inject-skill-directive 87)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--compile-directives 100))

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
                                  :max-tokens (macher-agent-transmission-state-max-tokens state)))
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
          (setf (macher-agent-transmission-state-tools state) (plist-get composed :tools))))))
  state)

(defun macher-agent-pipe--inject-dynamic-context-tools (state)
  "Inject dynamic context tools into STATE when target buffer exceeds `max-chars'."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                         (current-buffer)))
         (max-chars (macher-agent--get-max-context-chars target-buf))
         (buf-size (if (and target-buf (buffer-live-p target-buf))
                       (with-current-buffer target-buf (buffer-size))
                     0)))
    (when (> buf-size max-chars)
      (let ((mem-tool (ignore-errors
                        (macher-agent-resolve-tool "search_conversation_history" nil nil nil))))
        (when (and (macher-tool-valid-p mem-tool)
                   (not (cl-some (lambda (tl)
                                   (equal (macher-agent-canonical-tool-name tl)
                                          "search_conversation_history"))
                                 (macher-agent-transmission-state-tools state))))
          (setf (macher-agent-transmission-state-tools state)
                (append (macher-agent-transmission-state-tools state) (list mem-tool)))))))
  state)

(defun macher-agent-pipe--process-hidden-blocks (state)
  "Strip out UI-specific or hidden metadata before the LLM sees it."
  (cl-check-type state macher-agent-transmission-state)
  state)

(defun macher-agent-pipe--auto-enable-task-tool (state)
  "Auto-enable `submit_task_result' when buffer holds an active task ID or routing frame."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                         (current-buffer)))
         (has-task (when (and target-buf (buffer-live-p target-buf))
                     (or (buffer-local-value 'macher-agent--routing-stack target-buf)
                         (buffer-local-value 'macher-agent--current-task-id target-buf)))))
    (when (and has-task
               (not (cl-some (lambda (tool)
                               (equal (macher-agent-canonical-tool-name tool) "submit_task_result"))
                             (macher-agent-transmission-state-tools state))))
      (when-let* ((tool (and (fboundp 'gptel-get-tool)
                             (gptel-get-tool "submit_task_result"))))
        (setf (macher-agent-transmission-state-tools state)
              (append (macher-agent-transmission-state-tools state) (list tool))))))
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
  "Inject available skills into the compiled prompt when `use_skill' is active."
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
                (or (and (local-variable-p 'gptel--known-presets target-buf)
                         (buffer-local-value 'gptel--known-presets target-buf))
                    (and (local-variable-p 'macher-agent-presets target-buf)
                         (buffer-local-value 'macher-agent-presets target-buf))
                    (macher-agent-transmission-state-known-presets state)
                    (macher-agent-transmission-state-presets state)))
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
          (push directive (macher-agent-transmission-state-directives state))))))
  state)

(defun macher-agent-pipe--compile-directives (state)
  "Compile all collected directives in STATE and append them to the base prompt."
  (cl-check-type state macher-agent-transmission-state)
  (let ((dirs (nreverse (macher-agent-transmission-state-directives state)))
        (sys (macher-agent-transmission-state-base-prompt state)))
    (if dirs
        (let ((compiled-directives (string-join dirs "\n\n")))
          (setf (macher-agent-transmission-state-compiled-prompt state)
                (if (and sys (not (string-empty-p sys)))
                    (concat sys "\n\n" compiled-directives)
                  compiled-directives)))
      (setf (macher-agent-transmission-state-compiled-prompt state) sys)))
  state)

(defun macher-agent--interpolate-command-body (body user-args)
  "Interpolate USER-ARGS into command BODY in a single pass."
  (let* ((args-list (cond
                     ((listp user-args) user-args)
                     ((stringp user-args)
                      (let ((trimmed (string-trim user-args)))
                        (if (string-empty-p trimmed)
                            nil
                          (or (ignore-errors (split-string-and-unquote trimmed))
                              (split-string trimmed)))))
                     (t nil)))
         (full-args (if (stringp user-args)
                        (string-trim user-args)
                      (mapconcat #'identity (or args-list nil) " "))))
    (replace-regexp-in-string
     "\\$\\(arguments\\|args\\|[1-9][0-9]*\\)\\b"
     (lambda (match)
       (let ((token (substring match 1)))
         (cond
          ((or (string= token "arguments") (string= token "args"))
           full-args)
          (t
           (let ((idx (1- (string-to-number token))))
             (if (and (>= idx 0) (< idx (length args-list)))
                 (nth idx args-list)
               match))))))
     body t t)))

(defun macher-agent--compile-transmission-payload (orig-buf presets skills redirected-skill &optional context)
  "Compile payload strictly for live ORIG-BUF."
  (cl-check-type orig-buf buffer)
  (let* ((active-context (or context (macher-agent-resolve-context orig-buf)))
         (known (when (buffer-live-p orig-buf)
                  (buffer-local-value 'gptel--known-presets orig-buf)))
         (clean-skills (seq-remove (lambda (s)
                                     (let ((spec (if (listp s) s
                                                   (or (alist-get s known)
                                                       (when active-context
                                                         (alist-get s (macher-agent-context-skills active-context)))))))
                                       (plist-get spec :is-command)))
                                   skills))
         (initial-state (make-macher-agent-transmission-state
                         :target-buffer orig-buf
                         :presets presets
                         :skills clean-skills
                         :redirected-skill redirected-skill
                         :context active-context))
         (all-steps (macher-agent-get-pipeline-steps 'transmission)))
    (seq-reduce (lambda (st pipe-fn)
                  (cl-check-type st macher-agent-transmission-state)
                  (if (functionp pipe-fn)
                      (funcall pipe-fn st)
                    st))
                all-steps
                initial-state)))

(defun macher-agent-pipe--extract-redirect (state)
  "Route redirected skill text in STATE, merging tools and handling commands."
  (cl-check-type state macher-agent-transmission-state)
  (let ((redirected-skill (macher-agent-transmission-state-redirected-skill state))
        (target-buf (macher-agent-transmission-state-target-buffer state)))
    (when redirected-skill
      (let* ((known (if (and target-buf (buffer-live-p target-buf))
                        (with-current-buffer target-buf (bound-and-true-p gptel--known-presets))
                      (macher-agent-transmission-state-known-presets state)))
             (is-plist (listp redirected-skill))
             (ctx (macher-agent-transmission-state-context state))
             (spec (unless is-plist
                     (or (alist-get redirected-skill known)
                         (when ctx (alist-get redirected-skill (macher-agent-context-skills ctx))))))
             (redirect-state (unless is-plist
                               (macher-agent-compose-payload
                                (list :known-presets known)
                                (list redirected-skill))))
             (redirect-text (if is-plist
                                (plist-get redirected-skill :system)
                              (or (plist-get spec :system) (plist-get redirect-state :system))))
             (redirect-tools (if is-plist
                                 (plist-get redirected-skill :tools)
                               (plist-get redirect-state :tools)))
             (is-command (if is-plist
                             (plist-get redirected-skill :is-command)
                           (plist-get spec :is-command))))
        (when redirect-text
          (if is-command
              (let* ((raw-args (and ctx (macher-agent-context-prompt ctx)))
                     (cmd-name (if is-plist
                                   (symbol-name (or (plist-get redirected-skill :name-sym) redirected-skill))
                                 (symbol-name redirected-skill)))
                     (clean-args (if (and cmd-name raw-args)
                                     (replace-regexp-in-string
                                      (format "^[ \t]*@%s\\b[ \t]*" (regexp-quote cmd-name)) "" raw-args)
                                   raw-args))
                     (interpolated (macher-agent--interpolate-command-body redirect-text clean-args)))
                (setf (macher-agent-transmission-state-redirect-prompt state) interpolated)
                (setf (macher-agent-transmission-state-compiled-prompt state)
                      (macher-agent-transmission-state-base-prompt state)))
            (setf (macher-agent-transmission-state-redirect-prompt state) redirect-text)
            (setf (macher-agent-transmission-state-base-prompt state) redirect-text)
            (setf (macher-agent-transmission-state-compiled-prompt state) redirect-text)))
        (when redirect-tools
          (setf (macher-agent-transmission-state-tools state)
                (append redirect-tools (macher-agent-transmission-state-tools state)))))))
  state)

(defun macher-agent-sync-prompt-transformer (async-fn fsm)
  "Synchronise the VFS, extract current prompt, and compose skill profiles."
  (let* ((temp-buf (current-buffer))
         (active-fsm (macher-agent-get-active-fsm fsm))
         (info (when-let* ((fsm-obj active-fsm)
                           ((fboundp 'gptel-fsm-info)))
                 (ignore-errors (gptel-fsm-info fsm-obj))))
         (orig-buf (or (and info (plist-get info :buffer)) temp-buf))
         (context (or (when active-fsm
                        (macher-agent-gptel-context-from-fsm active-fsm orig-buf))
                      (macher-agent-resolve-context orig-buf)))
         (prompt-start (save-excursion
                         (goto-char (or (previous-single-property-change (point-max) 'gptel)
                                        (point-min)))
                         (point))))

    (when (buffer-live-p orig-buf)
      (with-current-buffer orig-buf
        (when active-fsm
          (setq-local macher-agent--active-fsm active-fsm))
        (when context
          (macher-agent--transformer-sync-context orig-buf context)
          (let ((raw-prompt (with-current-buffer temp-buf
                              (string-trim (buffer-substring-no-properties prompt-start (point-max))))))
            (unless (string-empty-p raw-prompt)
              (setf (macher-agent-context-prompt context) raw-prompt)))
          (when-let* ((info-prompt (and info (plist-get info :prompt))))
            (unless (string-empty-p (string-trim info-prompt))
              (setf (macher-agent-context-prompt context) info-prompt))))))

    (when (and active-fsm context)
      (macher-agent--set-fsm-info active-fsm
                                  (plist-put (gptel-fsm-info active-fsm) :macher-agent-context context)))

    (let* ((buffer-presets (when (buffer-live-p orig-buf)
                             (buffer-local-value 'macher-agent-presets orig-buf)))
           (known (when (buffer-live-p orig-buf)
                    (buffer-local-value 'gptel--known-presets orig-buf)))
           (extraction (macher-agent--extract-inline-skills prompt-start orig-buf))
           (inline-skills (car extraction))
           (inline-preset-used (cdr extraction))
           (transmission-skills (macher-agent--transformer-resolve-skills
                                 buffer-presets inline-skills known))
           (redirected-skill (macher-agent--transformer-detect-redirect
                              inline-preset-used inline-skills known context))
           (state (macher-agent--compile-transmission-payload
                   orig-buf buffer-presets transmission-skills redirected-skill context))
           (payload (list :model (macher-agent-transmission-state-model state)
                          :backend (when (buffer-live-p orig-buf) (buffer-local-value 'gptel-backend orig-buf))
                          :temperature (macher-agent-transmission-state-temperature state)
                          :max-tokens (macher-agent-transmission-state-max-tokens state)
                          :tools (macher-agent-normalize-tools (macher-agent-transmission-state-tools state)))))

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

;;; Transmission Execution

(defun macher-agent--make-transmit-response-hook (on-error &optional on-success)
  "Build response hook invoking ON-ERROR on failure or ON-SUCCESS when no routing frame awaits submission."
  (cl-check-type on-error function)
  (let ((hook-fn nil))
    (setq
     hook-fn
     (lambda (_beg _end)
       (let* ((res (string-trim (buffer-substring-no-properties (point-min) (point-max))))
              (fsm (macher-agent-get-active-fsm))
              (info (and fsm (fboundp 'gptel-fsm-info) (ignore-errors (gptel-fsm-info fsm))))
              (err-data (and info (plist-get info :error)))
              (tool-calls (and info (plist-get info :tool-use)))
              (has-routing (or (bound-and-true-p macher-agent--routing-stack)
                               (bound-and-true-p macher-agent--current-task-id))))
         (cond
          (err-data
           (let ((err-msg (if (stringp err-data) err-data
                            (or (plist-get err-data :message)
                                (format "%s" err-data)))))
             (funcall on-error err-msg)))
          ((string-empty-p res)
           (funcall on-error "Buffer stopped silently or returned empty."))
          ((and (not has-routing)
                (not tool-calls))
           (setq-local macher-agent-task-finished t)
           (when-let* ((ctx (bound-and-true-p macher-agent--persistent-context))
                       ((macher-agent-context-p ctx)))
             (setf (macher-agent-session-state (macher-agent-context-session ctx)) :finished))
           (when (functionp on-success)
             (funcall on-success res)))
          ((and has-routing
                (not tool-calls)
                (not (bound-and-true-p macher-agent-task-finished)))
           (funcall on-error "Agent halted without invoking submit_task_result.")))
         (remove-hook 'gptel-post-response-functions hook-fn t))))
    hook-fn))

(defun macher-agent-gptel-transmit (task-context callbacks)
  "Transmit network request restoring buffer-centric execution."
  (let* ((target-buffer (macher-agent-task-context-target-buffer task-context))
         (sys-msg (macher-agent-task-context-system-message task-context))
         (on-error (plist-get callbacks :on-error))
         (on-success (plist-get callbacks :on-success)))
    (with-current-buffer target-buffer
      (when sys-msg
        (setq-local gptel-system-prompt sys-msg))
      (when (functionp on-error)
        (add-hook 'gptel-post-response-functions
                  (macher-agent--make-transmit-response-hook on-error on-success)
                  nil t))
      (goto-char (point-max))
      (gptel-send))))

;;; Media Injection

(defvar macher-agent--in-media-injection nil
  "Track whether media injection is currently in progress.")

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
      fsm
    (or (macher-agent-resolve-context fsm)
        (macher-agent-resolve-context (or target-buf (macher-agent-gptel--fsm-target-buffer fsm) (current-buffer))))))

(defun macher-agent--perform-pending-media-injection (fsm)
  "Inject base64 media directly into FSM payload."
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
        (error "SECURITY ERROR: Media injection strictly requires a base64 encoded string or media descriptor"))
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

;;; Backend and Model Resolution

(defun macher-agent--find-model-in-backend (backend model-str)
  "Search BACKEND for a model matching MODEL-STR."
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
  "Find first backend and model format matching MODEL-NAME."
  (when (and model-name (boundp 'gptel--known-backends))
    (let ((model-str (if (symbolp model-name) (symbol-name model-name) model-name))
          result)
      (dolist (item gptel--known-backends)
        (unless result
          (setq result (macher-agent--find-model-in-backend (cdr item) model-str))))
      result)))

;;; Tool Scope Enforcement and Buffer Setup

(defun macher-agent--enforce-tool-scope (tool &optional fsm &rest _args)
  "Enforce that TOOL is explicitly within authorised scope."
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
  "Log GPTEL pre-tool call intent for TOOL, FSM, and ARGS to context audit log."
  (let ((context (or (when fsm (macher-agent-gptel-context-from-fsm fsm))
                     (macher-agent-resolve-context)))
        (tool-name (macher-agent-canonical-tool-name tool))
        (tool-args (if (and (macher-agent--plist-p tool) (plist-member tool :args))
                       (plist-get tool :args)
                     args)))
    (when context
      (macher-agent-log-tool-intent context "gptel-tool" tool-name tool-args))))

(defun macher-agent-setup-gptel-buffer ()
  "Set up a gptel buffer with macher-agent capabilities."
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
  "Initialise the workspace state and active context for WORKSPACE-ROOT."
  (unless (bound-and-true-p macher-agent--is-workspace)
    (setq-local macher-agent--is-workspace t)
    (setq-local macher-agent--active-fsm nil)
    (let* ((expanded (file-truename (expand-file-name workspace-root)))
           (existing (bound-and-true-p macher-agent--persistent-context))
           (workspace (or (and existing (macher-agent-context-workspace existing))
                          (make-macher-agent-workspace :project-root expanded)))
           (canonical-context
            (or existing
                (macher-agent--make-context
                 :project-root expanded
                 :origin-buffer (current-buffer)
                 :tools (make-hash-table :test 'equal)
                 :plugins (list :workspace workspace))))
           (buffer-context (when canonical-context
                             (macher-agent--clone-context canonical-context))))
      (when buffer-context
        (setf (macher-agent-context-owner-buffer buffer-context) (current-buffer))
        (setf (macher-agent-context-origin-buffer buffer-context) (current-buffer))
        (macher-agent--register-active-workspace-root expanded buffer-context)
        (macher-agent--inject-context-state buffer-context))
      (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)
      (when buffer-context
        (macher-agent-refresh-skills-and-tools buffer-context)))))

(defun macher-agent-gptel-mode-setup ()
  "Initialise agent defaults for gptel buffers inside a workspace."
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

(defun macher-agent-branch-chat (new-name)
  "Clone current chat buffer into NEW-NAME, inheriting agent state."
  (interactive "sNew branch name: ")
  (let* ((parent-buf (current-buffer))
         (parent-mode major-mode)
         (content (buffer-string))
         (active-backend gptel-backend)
         (active-model gptel-model)
         (active-sys gptel-system-prompt)
         (active-skill (bound-and-true-p macher-agent--active-skill-sym))
         (parent-ctx (macher-agent-resolve-context parent-buf)))
    (with-current-buffer (generate-new-buffer new-name)
      (funcall parent-mode)
      (gptel-mode)
      (insert content)
      (when active-backend (setq-local gptel-backend active-backend))
      (when active-model (setq-local gptel-model active-model))
      (when active-sys (setq-local gptel-system-prompt active-sys))
      (when active-skill (setq-local macher-agent--active-skill-sym active-skill))
      (when parent-ctx
        (let ((cloned (macher-agent--clone-context parent-ctx)))
          (setf (macher-agent-context-owner-buffer cloned) (current-buffer))
          (setf (macher-agent-context-parent-buffer cloned) parent-buf)
          (setq-local macher-agent--persistent-context cloned)))
      (setq-local macher-agent-presets (with-current-buffer parent-buf macher-agent-presets))
      (setq-local gptel-tools (with-current-buffer parent-buf gptel-tools))
      (switch-to-buffer (current-buffer)))))

(defun macher-agent-clear-context ()
  "Clear the persistent context for the current sub-agent buffer."
  (interactive)
  (if (not (bound-and-true-p macher-agent--persistent-context))
      (message "Macher-Agent: No persistent context to clear in buffer '%s'." (buffer-name))
    (let* ((raw-ws (macher-agent-context-workspace macher-agent--persistent-context))
           (ws (if (stringp raw-ws) (cons 'agent raw-ws) raw-ws))
           (root (or (macher-agent-context-project-root macher-agent--persistent-context)
                     default-directory))
           (fresh-ctx (when ws
                        (macher-agent--make-context
                         :project-root (expand-file-name root)
                         :origin-buffer (current-buffer)
                         :tools (make-hash-table :test 'equal)
                         :plugins (list :workspace ws)))))
      (when fresh-ctx
        (setf (macher-agent-context-owner-buffer fresh-ctx) (current-buffer)))
      (setq-local macher-agent--persistent-context fresh-ctx)
      (when (and (bound-and-true-p macher-agent--active-fsm)
                 (fboundp 'gptel-fsm-info))
        (let ((inf (gptel-fsm-info macher-agent--active-fsm)))
          (when (macher-agent--plist-p inf)
            (setq inf (plist-put inf :macher-agent-context fresh-ctx))
            (macher-agent--set-fsm-info macher-agent--active-fsm inf))))
      (message "Macher-Agent: VFS context successfully cleared. Agent reset to physical baseline."))))

(defun macher-agent-send (&rest args)
  "Trigger transmission in current buffer using `gptel-send' with ARGS."
  (apply #'gptel-send args))

(defun macher-agent-gptel--trigger-flush (fsm &rest _args)
  "Broadcast task completion for FSM using the associated agent context."
  (when (and fsm (fboundp 'gptel-fsm-state) (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT)))
    (when-let* ((info (ignore-errors (gptel-fsm-info fsm)))
                (target-buf (plist-get info :buffer)))
      (let* ((raw-ctx (or (plist-get info :macher-agent-context)
                          (macher-agent-resolve-context target-buf)))
             (agent-ctx (when (macher-agent-valid-context-p raw-ctx) raw-ctx)))
        (if (buffer-live-p target-buf)
            (with-current-buffer target-buf
              (setq-local macher-agent--active-fsm nil)
              (run-hook-with-args 'macher-agent-buffer-flush-hook target-buf)
              (when agent-ctx
                (macher-agent-run-task-flush-hook agent-ctx)))
          (when agent-ctx
            (macher-agent-run-task-flush-hook agent-ctx)))))))

(defun macher-agent-get-active-fsm (&optional current-fsm)
  "Correlate UI operations using validated CURRENT-FSM."
  (or (and current-fsm
           (not (macher-agent-transit-payload-p current-fsm))
           (not (macher-agent-context-p current-fsm))
           (not (bufferp current-fsm))
           current-fsm)
      (bound-and-true-p macher-agent--active-fsm)
      (bound-and-true-p gptel--fsm)))

(setq macher-agent-active-fsm-function #'macher-agent-get-active-fsm)
(setq macher-agent-fsm-info-function #'gptel-fsm-info)

(defun macher-agent--extract-fsm-info (fsm)
  "Extract info plist from FSM struct."
  (cl-check-type fsm gptel-fsm)
  (gptel-fsm-info fsm))

(defun macher-agent--set-fsm-info (fsm info)
  "Set INFO plist on FSM struct."
  (when fsm
    (setf (gptel-fsm-info fsm) info)
    info))

(defun macher-agent--set-fsm-handlers (fsm handlers)
  "Set HANDLERS alist on FSM struct."
  (when fsm
    (setf (gptel-fsm-handlers fsm) handlers)
    handlers))

(defun macher-agent-bridge-abort (buf)
  "Abort operations exclusively in live BUF object."
  (cl-check-type buf buffer)
  (when (and (buffer-live-p buf) (fboundp 'gptel-abort))
    (with-current-buffer buf
      (condition-case nil
          (gptel-abort buf)
        (wrong-number-of-arguments
         (funcall (symbol-function 'gptel-abort)))))))

(provide 'macher-agent-gptel)
;;; macher-agent-gptel.el ends here
