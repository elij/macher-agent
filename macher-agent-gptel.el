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
(declare-function gptel--create-prompt-buffer "gptel-request" (&optional prompt-end))
(declare-function gptel--parse-buffer "gptel-request" (backend max-entries))
(declare-function gptel--parse-list-and-insert "gptel-request" (prompts))
(declare-function gptel--model-request-params "gptel-request" (model))
(declare-function gptel--error-p "gptel-request" (info))
(declare-function gptel--request-data "gptel-request" (backend prompts))
(declare-function gptel-prompt-prefix-string "gptel" ())

(defvar gptel-system-prompt)
(defvar gptel-directives)
(defvar gptel--preset)
(defvar gptel--bounds)
(defvar gptel--tool-names)
(defvar gptel--backend-name)
(defvar gptel--known-presets)
(defvar gptel--known-tools)
(defvar gptel--known-backends)
(defvar gptel--num-messages-to-send)
(defvar gptel-track-response)
(defvar gptel-save-state-hook)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-stream)
(defvar gptel-use-curl)
(defvar gptel-use-tools)
(defvar gptel-tools)
(defvar gptel-temperature)
(defvar gptel-max-tokens)

(defvar-local macher-agent--is-restored-session nil
  "Track whether current buffer is restored from a saved state.")

(defvar-local macher-agent--is-workspace nil
  "Flag whether current buffer is initialised as a Macher Agent workspace.")
(put 'macher-agent--is-workspace 'permanent-local t)

(cl-defstruct macher-agent-transmission-state
  "Transmission pipeline state for compiling a gptel request."
  target-buffer
  presets
  skills
  redirected-skill
  backend
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
  plugins
  (consume-boot-directive t))

(defcustom macher-agent-prompt-transformers
  '(macher-agent-transformer-deduplicate-tools)
  "List of transformer functions to clean buffer prior to transmission."
  :type '(repeat function)
  :group 'macher-agent)

(defcustom macher-agent-max-duplicate-tools 2
  "Maximum number of identical tool calls to retain in context history."
  :type 'integer
  :group 'macher-agent)

(defun macher-agent--task-submitted-p (info)
  "Return non-nil when FSM INFO buffer holds a staged `submit_task_result'."
  (when-let* ((buf (and (macher-agent--plist-p info) (plist-get info :buffer)))
              ((buffer-live-p buf))
              ((buffer-local-value 'macher-agent--staged-submission buf)))
    (plist-put info :tool-use nil)
    (plist-put info :tool-result nil)
    t))

(defun macher-agent--install-tret-done-edge (fsm)
  "Ensure transition table for FSM routes `TRET' to `DONE' on staged submit."
  (when (and fsm (fboundp 'gptel-fsm-table))
    (let* ((orig-table (gptel-fsm-table fsm))
           (tret-cell (assq 'TRET orig-table)))
      (when tret-cell
        (let ((tret-edges (copy-alist (cdr tret-cell))))
          (unless (assq #'macher-agent--task-submitted-p tret-edges)
            (let* ((err-edge (cl-find-if (lambda (e) (eq (cdr e) 'ERRS)) tret-edges))
                   (rest-edges (if err-edge (remove err-edge tret-edges) tret-edges))
                   (new-edges (if err-edge
                                  (cons err-edge
                                        (cons (cons #'macher-agent--task-submitted-p 'DONE)
                                              rest-edges))
                                (cons (cons #'macher-agent--task-submitted-p 'DONE)
                                      rest-edges)))
                   (new-table (copy-alist orig-table)))
              (setf (alist-get 'TRET new-table) new-edges)
              (setf (gptel-fsm-table fsm) new-table))))))))

(defun macher-agent--fsm-hijack-transform (arg1 &optional arg2)
  "Mutate the FSM using ARG1 and optional ARG2 to inject hooks and media."
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
        (setq-local macher-agent--active-fsm actual-fsm)
        (setq-local macher-agent--turn-submission-delivered nil))
      (let* ((ctx (macher-agent-resolve-context target-buf))
             (inf (gptel-fsm-info actual-fsm)))
        (setq inf (plist-put inf :origin-buffer target-buf))
        (when (eq (bound-and-true-p gptel-use-tools) 'force)
          (setq inf (plist-put inf :use-tools 'force)))
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
                            (lambda (&rest cb-args)
                              (when (and target-buf
                                         (buffer-live-p target-buf)
                                         (fboundp 'gptel-fsm-state)
                                         (memq (gptel-fsm-state actual-fsm) '(DONE ERRS ABRT))
                                         (eq (buffer-local-value 'macher-agent--active-fsm target-buf)
                                             actual-fsm))
                                (with-current-buffer target-buf
                                  (setq-local macher-agent--active-fsm nil)))
                              (apply orig-cb cb-args)))))
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
        (setf (gptel-fsm-handlers actual-fsm) augmented-handlers))
      (macher-agent--install-tret-done-edge actual-fsm))

    (when callback (funcall callback))))

(defun macher-agent--restore-saved-gptel-state ()
  "Restore saved gptel preset, tools, and bounds.
Preserves any file-local variable overrides."
  (when (and (buffer-file-name) (fboundp 'gptel--restore-state))
    (let* ((flv (bound-and-true-p file-local-variables-alist))
           (override-vars '(gptel-system-prompt gptel-model
                            gptel-temperature gptel-max-tokens
                            gptel--num-messages-to-send))
           (saved-overrides
            (cl-loop for var in override-vars
                     for cell = (assq var flv)
                     when cell collect cell))
           (preset-sym (bound-and-true-p gptel--preset))
           (preset-spec (when preset-sym
                          (macher-agent--lookup-skill-spec preset-sym nil (current-buffer)))))
      (when (and preset-spec (plist-get preset-spec :is-command))
        (setq-local gptel--preset nil))
      (ignore-errors (gptel--restore-state))
      (dolist (cell saved-overrides)
        (set (make-local-variable (car cell)) (cdr cell))))))

(defun macher-agent--restore-local-state ()
  "Restore agent variables natively after Emacs parses file-local variables."
  (when (or (local-variable-p 'gptel--preset)
            (local-variable-p 'gptel--bounds)
            (local-variable-p 'gptel-model)
            (local-variable-p 'gptel-backend)
            (local-variable-p 'gptel--backend-name)
            (local-variable-p 'gptel--tool-names))
    (setq-local macher-agent--is-restored-session t)
    (let ((current-root (macher-agent-root default-directory)))
      (when current-root
        (macher-agent--init-workspace-state current-root)))
    (macher-agent--restore-saved-gptel-state)
    (macher-agent--sync-preset-before-save)
    (when (fboundp 'gptel--update-header-line)
      (gptel--update-header-line))))

(defun macher-agent--get-max-context-chars (&optional buf model-override)
  "Resolve max context characters based on MODEL-OVERRIDE or the model in BUF."
  (let* ((target-buf (or buf (current-buffer)))
         (model (or model-override
                    (when (buffer-live-p target-buf)
                      (buffer-local-value 'gptel-model target-buf))))
         (model-sym (if (stringp model) (intern model) model)))
    (or (alist-get model-sym macher-agent-max-context-chars)
        (alist-get nil macher-agent-max-context-chars)
        2000000)))

;;; Prompt Transformation and Inline Skill Extraction

(defun macher-agent--skip-prompt-prefix (pos)
  "Advance from POS past whitespace and prompt prefix (`### ', `*** ')."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t\r\n")
    (let ((pfx (when (fboundp 'gptel-prompt-prefix-string)
                 (ignore-errors (gptel-prompt-prefix-string)))))
      (cond
       ((and (stringp pfx)
             (not (string-empty-p pfx))
             (looking-at (regexp-quote pfx)))
        (goto-char (match-end 0)))
       ((looking-at "\\(?:#+\\|\\*+\\|>+\\)[ \t]+")
        (goto-char (match-end 0)))))
    (skip-chars-forward " \t")
    (point)))

(defun macher-agent--extract-inline-skills (prompt-start orig-buf)
  "Extract inline skill tags from PROMPT-START using ORIG-BUF context."
  (let ((matched-skills nil)
        (inline-preset-used nil)
        (known (with-current-buffer orig-buf (bound-and-true-p gptel--known-presets)))
        (first-non-ws (macher-agent--skip-prompt-prefix prompt-start))
        (stop nil))
    (save-excursion
      (goto-char prompt-start)
      (while (and (not stop) (< (point) (point-max)))
        (cond
         ((looking-at "^[ \t]*```")
          (forward-line 1)
          (unless (re-search-forward "^[ \t]*```[ \t]*$" nil 'move)
            (goto-char (point-max))))
         ((let ((case-fold-search t))
            (looking-at "^[ \t]*#\\+begin_\\(src\\|example\\|tool\\)\\b"))
          (let ((end-re (format "^[ \t]*#\\+end_%s\\b" (match-string 1)))
                (case-fold-search t))
            (forward-line 1)
            (unless (re-search-forward end-re nil 'move)
              (goto-char (point-max)))))
         ((re-search-forward "@\\([[:alnum:]_-]+\\)" (line-end-position) t)
          (when (or (= (match-beginning 0) (point-min))
                    (memq (char-before (match-beginning 0)) '(32 ?\t ?\n ?\r 62)))
            (let* ((match-start (match-beginning 0))
                   (sym-name (match-string-no-properties 1))
                   (sym (intern sym-name))
                   (is-cmd (when (= match-start first-non-ws)
                             (with-current-buffer orig-buf
                               (let ((spec (macher-agent--lookup-skill-spec sym nil orig-buf)))
                                 (and spec (plist-get spec :is-command))))))
                   (is-valid
                    (or is-cmd
                        (with-current-buffer orig-buf
                          (or (assoc sym known)
                              (and (fboundp 'gptel-get-preset)
                                   (ignore-errors (gptel-get-preset sym)))
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
                  (setq stop t))))))
         (t
          (forward-line 1)))))
    (cons (nreverse matched-skills) inline-preset-used)))

(defun macher-agent--update-fsm-info-from-transmission-state (fsm state)
  "Update FSM info plist and encoded request data from transmission STATE."
  (when (and fsm state)
    (let* ((old-info (gptel-fsm-info fsm))
           (prev-backend (when (macher-agent--plist-p old-info)
                           (plist-get old-info :backend)))
           (new-info (copy-sequence old-info))
           (compiled (macher-agent-transmission-state-compiled-prompt state))
           (model (macher-agent-transmission-state-model state))
           (backend (macher-agent-transmission-state-backend state))
           (tools (macher-agent-transmission-state-tools state)))
      (setq new-info (plist-put new-info :system compiled))
      (when backend
        (setq new-info (plist-put new-info :backend backend)))
      (setq new-info (plist-put new-info :model model))
      (setq new-info (plist-put new-info :temperature (macher-agent-transmission-state-temperature state)))
      (setq new-info (plist-put new-info :max-tokens (macher-agent-transmission-state-max-tokens state)))
      (setq new-info (plist-put new-info :tools tools))
      (setq new-info (plist-put new-info :ptc-primitives
                                (macher-agent-transmission-state-ptc-primitives state)))
      (macher-agent--set-fsm-info fsm new-info)
      (macher-agent--sync-fsm-request-data
       fsm compiled tools model backend prev-backend))))

(defun macher-agent-transformer-deduplicate-tools (async-fn _fsm)
  "Remove redundant tool usage blocks and invoke ASYNC-FN."
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

(defun macher-agent--transformer-resolve-skills (buffer-presets inline-skills known &optional context target-buf)
  "Resolve transmission skills from BUFFER-PRESETS and INLINE-SKILLS.
Uses KNOWN presets, optional CONTEXT, and TARGET-BUF."
  (let ((combined (delete-dups (append buffer-presets inline-skills nil)))
        (ctx (or (and (macher-agent-valid-context-p context) context)
                 (macher-agent-resolve-context target-buf)))
        (acc nil)
        (exclusive-found nil))
    (dolist (p combined (nreverse acc))
      (let* ((spec (when (symbolp p)
                     (macher-agent--lookup-skill-spec p ctx target-buf known t)))
             (excl (plist-get spec :exclusive)))
        (if excl
            (progn
              (setq acc (list p))
              (setq exclusive-found t))
          (unless exclusive-found
            (push p acc)))))))

(defun macher-agent--transformer-detect-redirect (inline-preset-used inline-skills known ctx)
  "Detect whether INLINE-PRESET-USED triggers a command redirect.
Inspects INLINE-SKILLS against KNOWN presets and CTX."
  (when-let* ((_ inline-preset-used)
              (skill (car inline-skills)))
    (let* ((spec (when (symbolp skill)
                   (macher-agent--lookup-skill-spec skill ctx nil known t)))
           (is-command (or (and (listp skill) (plist-get skill :is-command))
                           (and spec (plist-get spec :is-command)))))
      (when is-command
        skill))))

(defun macher-agent-transmission-install ()
  "Register core transmission pipeline steps in strict execution order."
  (setq macher-agent-active-fsm-function #'macher-agent-get-active-fsm)
  (setq macher-agent-fsm-info-function #'gptel-fsm-info)
  (setq macher-agent-mid-turn-recompile-function #'macher-agent--recompile-and-sync-buffer-turn)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--hydrate-base-state 10)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--apply-skills-and-presets 20)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--extract-redirect 30)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--inject-dynamic-context-tools 40)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--process-hidden-blocks 50)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--auto-retain-use-skill 53)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--auto-enable-task-tool 54)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--append-boot-directive 70)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--inject-skill-directive 87)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--init-core-directives 95)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-pipe--compile-directives 100))

(defun macher-agent--merge-tools (tools1 tools2)
  "Merge TOOLS1 and TOOLS2, deduplicating by canonical tool name."
  (let ((seen (make-hash-table :test 'equal))
        (merged nil))
    (dolist (tool (append tools1 tools2))
      (when tool
        (let* ((name (macher-agent-canonical-tool-name tool))
               (key (if (stringp name)
                        (downcase (replace-regexp-in-string "-" "_" name))
                      name)))
          (unless (gethash key seen)
            (puthash key t seen)
            (push tool merged)))))
    (nreverse merged)))

(defun macher-agent--refresh-tool-struct (tool context)
  "Return refreshed `gptel-tool' struct for TOOL in CONTEXT when resolvable."
  (if (macher-tool-valid-p tool)
      (let* ((cname (macher-agent-canonical-tool-name tool))
             (resolved (when cname
                         (ignore-errors
                           (macher-agent-resolve-tool cname nil nil context)))))
        (if (macher-tool-valid-p resolved)
            resolved
          tool))
    tool))

(defun macher-agent--ensure-tool-in-list (tools tool-name &optional ctx target-buf fallback-var)
  "Return TOOLS with TOOL-NAME appended when not already present.
Resolves TOOL-NAME via CTX, TARGET-BUF `gptel-get-tool', or FALLBACK-VAR."
  (if (cl-some (lambda (tl)
                 (equal (macher-agent-canonical-tool-name tl) tool-name))
               tools)
      tools
    (let* ((resolved (ignore-errors (macher-agent-resolve-tool tool-name nil nil ctx)))
           (tool (or (and (macher-tool-valid-p resolved) resolved)
                     (and (fboundp 'gptel-get-tool)
                          (if (and (bufferp target-buf) (buffer-live-p target-buf))
                              (with-current-buffer target-buf
                                (ignore-errors (gptel-get-tool tool-name)))
                            (ignore-errors (gptel-get-tool tool-name))))
                     (and fallback-var (boundp fallback-var) (symbol-value fallback-var)))))
      (if (macher-tool-valid-p tool)
          (append tools (list tool))
        tools))))

(defun macher-agent-pipe--hydrate-base-state (state)
  "Extract pristine local variables from target buffer into STATE."
  (cl-check-type state macher-agent-transmission-state)
  (let ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                        (current-buffer))))
    (when (and target-buf (buffer-live-p target-buf))
      (with-current-buffer target-buf
        (let* ((active-context (or (macher-agent-transmission-state-context state)
                                   (macher-agent-resolve-context target-buf)))
               (flv (bound-and-true-p file-local-variables-alist))
               (buf-presets (bound-and-true-p macher-agent-presets))
               (active-preset (bound-and-true-p gptel--preset))
               (preset-spec (when (and (null buf-presets)
                                       (null (macher-agent-transmission-state-presets state))
                                       active-preset)
                              (let ((sp (macher-agent--lookup-skill-spec
                                         active-preset active-context target-buf)))
                                (when (and sp (not (plist-get sp :is-command)))
                                  sp))))
               (inherited (macher-agent-transmission-state-tools state))
               (local-p (local-variable-p 'gptel-tools target-buf))
               (buf-tools (when (boundp 'gptel-tools) gptel-tools))
               (tools-suppressed (and local-p (null buf-tools)))
               (merged-tools (unless tools-suppressed
                               (macher-agent--merge-tools inherited buf-tools)))
               (refreshed-tools
                (when merged-tools
                  (mapcar (lambda (tl)
                            (macher-agent--refresh-tool-struct tl active-context))
                          merged-tools))))
          (setf (macher-agent-transmission-state-backend state) (bound-and-true-p gptel-backend))
          (setf (macher-agent-transmission-state-model state) gptel-model)
          (setf (macher-agent-transmission-state-base-prompt state) gptel-system-prompt)
          (setf (macher-agent-transmission-state-compiled-prompt state) gptel-system-prompt)
          (setf (macher-agent-transmission-state-temperature state) (bound-and-true-p gptel-temperature))
          (setf (macher-agent-transmission-state-max-tokens state) (bound-and-true-p gptel-max-tokens))
          (setf (macher-agent-transmission-state-tools state) refreshed-tools)
          (setf (macher-agent-transmission-state-ptc-primitives state)
                (bound-and-true-p macher-agent--active-ptc-primitives))
          (setf (macher-agent-transmission-state-known-presets state)
                (when (local-variable-p 'gptel--known-presets target-buf)
                  (buffer-local-value 'gptel--known-presets target-buf)))
          (when preset-spec
            (let ((filtered-spec (copy-sequence preset-spec)))
              (when (assq 'gptel-model flv)
                (cl-remf filtered-spec :model))
              (when (or (assq 'gptel-backend flv) (assq 'gptel--backend-name flv))
                (cl-remf filtered-spec :backend))
              (when (assq 'gptel-temperature flv)
                (cl-remf filtered-spec :temperature))
              (when (assq 'gptel-max-tokens flv)
                (cl-remf filtered-spec :max-tokens))
              (let ((params
                     (macher-agent-preset-pipe--parameters
                      (list :backend (macher-agent-transmission-state-backend state)
                            :model (macher-agent-transmission-state-model state)
                            :temperature (macher-agent-transmission-state-temperature state)
                            :max-tokens (macher-agent-transmission-state-max-tokens state))
                      (list 'preset active-preset filtered-spec))))
                (setf (macher-agent-transmission-state-backend state) (plist-get params :backend))
                (setf (macher-agent-transmission-state-model state) (plist-get params :model))
                (setf (macher-agent-transmission-state-temperature state) (plist-get params :temperature))
                (setf (macher-agent-transmission-state-max-tokens state) (plist-get params :max-tokens))))
            (when-let* ((raw-sys (or (plist-get preset-spec :system)
                                     (plist-get preset-spec :system-message)))
                        ((not (assq 'gptel-system-prompt flv))))
              (let ((resolved-sys
                     (cond
                      ((stringp raw-sys) raw-sys)
                      ((and (symbolp raw-sys) (not (functionp raw-sys)))
                       (or (alist-get raw-sys (bound-and-true-p gptel-directives))
                           raw-sys))
                      ((and (consp raw-sys) (not (keywordp (car raw-sys))))
                       raw-sys)
                      (t nil))))
                (when resolved-sys
                  (setf (macher-agent-transmission-state-base-prompt state) resolved-sys)
                  (setf (macher-agent-transmission-state-compiled-prompt state) resolved-sys))))
            (let ((has-flv-tools
                   (or (assq 'gptel--tool-names flv)
                       (assq 'gptel-tools flv)
                       (and (derived-mode-p 'org-mode)
                            (fboundp 'org-entry-get)
                            (org-entry-get (point-min) "GPTEL_TOOLS" 'selective))))
                  (preset-tools-spec (or (plist-get preset-spec :tools)
                                         (plist-get preset-spec :allowed-tools))))
              (when (and (not tools-suppressed)
                         (not has-flv-tools)
                         preset-tools-spec)
                (let* ((is-mod (and (consp preset-tools-spec)
                                    (keywordp (car preset-tools-spec))))
                       (merged (macher-agent--compose-merge-tools
                                (and is-mod refreshed-tools)
                                preset-tools-spec))
                       (resolved-preset-tools
                        (mapcar (lambda (tl)
                                  (macher-agent--refresh-tool-struct tl active-context))
                                (macher-agent-normalize-tools merged))))
                  (when resolved-preset-tools
                    (setf (macher-agent-transmission-state-tools state)
                          (macher-agent--merge-tools
                           resolved-preset-tools
                           (unless is-mod refreshed-tools)))))))
            (when-let* ((preset-prims (plist-get preset-spec :ptc-primitives)))
              (setf (macher-agent-transmission-state-ptc-primitives state)
                    (delete-dups
                     (append (ensure-list preset-prims)
                             (macher-agent-transmission-state-ptc-primitives state))))))))))
  state)

(defun macher-agent-pipe--apply-skills-and-presets (state)
  "Merge active presets and transmission skills into STATE."
  (cl-check-type state macher-agent-transmission-state)
  (let ((combined-modules (macher-agent-transmission-state-skills state))
        (target-buf (macher-agent-transmission-state-target-buffer state)))
    (when combined-modules
      (let* ((known (macher-agent-transmission-state-known-presets state))
             (payload-plist (list :known-presets known
                                  :backend (macher-agent-transmission-state-backend state)
                                  :model (macher-agent-transmission-state-model state)
                                  :system (macher-agent-transmission-state-base-prompt state)
                                  :tools (macher-agent-transmission-state-tools state)
                                  :ptc-primitives (macher-agent-transmission-state-ptc-primitives state)
                                  :temperature (macher-agent-transmission-state-temperature state)
                                  :max-tokens (macher-agent-transmission-state-max-tokens state)))
             (composed (if (and target-buf (buffer-live-p target-buf))
                           (with-current-buffer target-buf
                             (macher-agent-compose-payload payload-plist combined-modules))
                         (macher-agent-compose-payload payload-plist combined-modules))))
        (when (plist-member composed :system)
          (setf (macher-agent-transmission-state-base-prompt state) (plist-get composed :system)))
        (when (plist-get composed :backend)
          (setf (macher-agent-transmission-state-backend state) (plist-get composed :backend)))
        (when (plist-get composed :model)
          (setf (macher-agent-transmission-state-model state) (plist-get composed :model)))
        (when (plist-get composed :temperature)
          (setf (macher-agent-transmission-state-temperature state) (plist-get composed :temperature)))
        (when (plist-get composed :max-tokens)
          (setf (macher-agent-transmission-state-max-tokens state) (plist-get composed :max-tokens)))
        (when (plist-member composed :tools)
          (setf (macher-agent-transmission-state-tools state) (plist-get composed :tools)))
        (when (plist-member composed :ptc-primitives)
          (setf (macher-agent-transmission-state-ptc-primitives state)
                (plist-get composed :ptc-primitives))))))
  state)

(defun macher-agent-pipe--inject-dynamic-context-tools (state)
  "Inject dynamic context tools into STATE when buffer exceeds `max-chars'."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                         (current-buffer)))
         (max-chars (macher-agent--get-max-context-chars
                     target-buf (macher-agent-transmission-state-model state)))
         (buf-size (if (and target-buf (buffer-live-p target-buf))
                       (with-current-buffer target-buf (buffer-size))
                     0)))
    (when (> buf-size max-chars)
      (let ((ctx (or (macher-agent-transmission-state-context state)
                     (macher-agent-resolve-context target-buf))))
        (setf (macher-agent-transmission-state-tools state)
              (macher-agent--ensure-tool-in-list
               (macher-agent-transmission-state-tools state)
               "search_conversation_history" ctx target-buf)))))
  state)

(defun macher-agent-pipe--process-hidden-blocks (state)
  "Strip UI-specific or hidden metadata from STATE before transmission."
  (cl-check-type state macher-agent-transmission-state)
  state)

(defun macher-agent-pipe--auto-retain-use-skill (state)
  "Retain `use_skill' in STATE when target buffer has cached presets."
  (cl-check-type state macher-agent-transmission-state)
  (let ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                        (current-buffer))))
    (when (macher-agent--has-cached-skill-state-p target-buf)
      (let ((ctx (or (macher-agent-transmission-state-context state)
                     (macher-agent-resolve-context target-buf))))
        (setf (macher-agent-transmission-state-tools state)
              (macher-agent--ensure-tool-in-list
               (macher-agent-transmission-state-tools state)
               "use_skill" ctx target-buf 'macher-agent-use-skill-tool)))))
  state)

(defun macher-agent-pipe--auto-enable-task-tool (state)
  "Auto-enable `submit_task_result' in STATE when a task or route is active."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                         (current-buffer)))
         (has-task (when (and target-buf (buffer-live-p target-buf))
                     (or (buffer-local-value 'macher-agent--routing-stack target-buf)
                         (buffer-local-value 'macher-agent--current-task-id target-buf)))))
    (when has-task
      (let ((ctx (or (macher-agent-transmission-state-context state)
                     (macher-agent-resolve-context target-buf))))
        (setf (macher-agent-transmission-state-tools state)
              (macher-agent--ensure-tool-in-list
               (macher-agent-transmission-state-tools state)
               "submit_task_result" ctx target-buf 'macher-agent-submit-task-result-tool)))))
  state)

(defun macher-agent-pipe--init-core-directives (state)
  "Mandate task submission in STATE if `submit_task_result' is enabled."
  (cl-check-type state macher-agent-transmission-state)
  (when (cl-some (lambda (tool)
                   (equal (macher-agent-canonical-tool-name tool) "submit_task_result"))
                 (macher-agent-transmission-state-tools state))
    (push "CRITICAL DIRECTIVE: You MUST use the `submit_task_result` tool to submit your final answer when you are completely finished. Do NOT output your final answer as standard text. IMMEDIATELY STOP after."
          (macher-agent-transmission-state-directives state)))
  state)

(defun macher-agent-pipe--append-boot-directive (state)
  "Append boot directive to STATE struct."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                         (current-buffer)))
         (boot-dir (when (and target-buf (buffer-live-p target-buf))
                     (with-current-buffer target-buf
                       (bound-and-true-p macher-agent--boot-directive))))
         (forced (when (and target-buf (buffer-live-p target-buf))
                   (buffer-local-value 'macher-agent--force-boot-directive target-buf))))
    (when (and boot-dir (stringp boot-dir) (not (string-empty-p boot-dir))
               target-buf
               (or forced
                   (not (with-current-buffer target-buf
                          (text-property-any (point-min) (point-max) 'gptel 'response)))))
      (push boot-dir (macher-agent-transmission-state-directives state))
      (when (and forced
                 (macher-agent-transmission-state-consume-boot-directive state)
                 (buffer-live-p target-buf))
        (with-current-buffer target-buf
          (setq-local macher-agent--force-boot-directive nil)))))
  state)

(defun macher-agent-pipe--inject-skill-directive (state)
  "Inject available non-command skills into STATE when `use_skill' is active."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((tools (macher-agent-transmission-state-tools state))
         (has-use-skill (cl-some (lambda (tool)
                                   (equal (macher-agent-canonical-tool-name tool) "use_skill"))
                                 tools)))
    (when has-use-skill
      (let* ((target-buf (macher-agent-transmission-state-target-buffer state))
             (buf (if (and target-buf (buffer-live-p target-buf))
                      target-buf
                    (current-buffer)))
             (ctx (or (macher-agent-transmission-state-context state)
                      (macher-agent-resolve-context target-buf)))
             (buffer-presets
              (or (and (local-variable-p 'gptel--known-presets buf)
                       (buffer-local-value 'gptel--known-presets buf))
                  (and (macher-agent-valid-context-p ctx)
                       (macher-agent-workspace-skills-alist ctx))
                  (and (local-variable-p 'macher-agent-presets buf)
                       (buffer-local-value 'macher-agent-presets buf))
                  (macher-agent-transmission-state-known-presets state)
                  (macher-agent-transmission-state-presets state)))
             (presets-list (if (and buffer-presets (atom buffer-presets))
                               (list buffer-presets)
                             buffer-presets))
             (seen (make-hash-table :test 'equal))
             (skill-lines nil))
        (dolist (entry presets-list)
          (let* ((sym (if (consp entry) (car entry) entry))
                 (base-spec (when (and (consp entry) (listp (cdr entry))) (cdr entry)))
                 (lookup-spec (when (or (symbolp sym) (stringp sym))
                                (macher-agent--lookup-skill-spec sym ctx target-buf)))
                 (spec (if (symbolp sym)
                           (when (or base-spec lookup-spec)
                             (macher-agent--merge-preset-meta sym base-spec lookup-spec))
                         (or base-spec lookup-spec)))
                 (is-cmd (and (listp spec) (plist-get spec :is-command)))
                 (name (cond ((symbolp sym) (symbol-name sym))
                             ((stringp sym) sym)
                             (t (format "%s" sym)))))
            (unless (or is-cmd (gethash name seen))
              (puthash name t seen)
              (let ((desc (cond
                           ((and (listp spec) (plist-get spec :description))
                            (plist-get spec :description))
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
  "Compile collected directives in STATE and append them to base prompt."
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

(defun macher-agent--compile-transmission-payload (orig-buf presets skills redirected-skill &optional context consume-boot)
  "Compile transmission payload for live ORIG-BUF.
Applies PRESETS, SKILLS, and REDIRECTED-SKILL with optional CONTEXT and
CONSUME-BOOT mode."
  (cl-check-type orig-buf buffer)
  (let* ((active-context (or context (macher-agent-resolve-context orig-buf)))
         (known (when (buffer-live-p orig-buf)
                  (buffer-local-value 'gptel--known-presets orig-buf)))
         (clean-skills (seq-remove (lambda (s)
                                     (let ((spec (if (listp s) s
                                                   (macher-agent--lookup-skill-spec
                                                    s active-context orig-buf known t))))
                                       (plist-get spec :is-command)))
                                   skills))
         (initial-state (make-macher-agent-transmission-state
                         :target-buffer orig-buf
                         :presets presets
                         :skills clean-skills
                         :redirected-skill redirected-skill
                         :context active-context
                         :consume-boot-directive (if (null consume-boot)
                                                     t
                                                   (not (eq consume-boot :preserve)))))
         (all-steps (macher-agent-get-pipeline-steps 'transmission)))
    (seq-reduce (lambda (st pipe-fn)
                  (cl-check-type st macher-agent-transmission-state)
                  (if (functionp pipe-fn)
                      (funcall pipe-fn st)
                    st))
                all-steps
                initial-state)))

(defun macher-agent--assign-missing-buffer-tool-ids ()
  "Assign deterministic IDs to `(tool . nil)' regions in current buffer.
Returns `(COUNTS . USED)' hash tables tracking per-tool call counts and IDs."
  (let ((counts (make-hash-table :test 'equal))
        (used (make-hash-table :test 'equal))
        (inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((prop (get-char-property (point) 'gptel))
              (next (next-single-property-change (point) 'gptel nil (point-max))))
          (when (and (consp prop) (eq (car prop) 'tool))
            (let* ((form (save-excursion (ignore-errors (read (current-buffer)))))
                   (tname (or (and (listp form) (plist-get form :name)) "tool"))
                   (n (1+ (gethash tname counts 0)))
                   (existing-id (cdr prop))
                   (chosen-id
                    (or existing-id
                        (let ((cand (if (= n 1)
                                        (format "call_%s" tname)
                                      (format "call_%s_%d" tname n))))
                          (while (gethash cand used)
                            (setq n (1+ n)
                                  cand (format "call_%s_%d" tname n)))
                          cand))))
              (puthash tname n counts)
              (puthash (format "%s" chosen-id) t used)
              (unless existing-id
                (put-text-property (point) next 'gptel (cons 'tool chosen-id)))))
          (goto-char next))))
    (cons counts used)))

(defun macher-agent--ensure-tool-use-ids (tool-use &optional counts used)
  "Assign deterministic `:id' values to entries in TOOL-USE lacking one.
Uses optional COUNTS and USED tables from prior buffer tool blocks."
  (when (and (listp tool-use) tool-use)
    (let ((counts (or counts (make-hash-table :test 'equal)))
          (used (or used (make-hash-table :test 'equal))))
      (dolist (tc tool-use)
        (when-let* ((id (and (macher-agent--plist-p tc) (plist-get tc :id))))
          (puthash (format "%s" id) t used)))
      (dolist (tc tool-use)
        (when (and (macher-agent--plist-p tc) (not (plist-get tc :id)))
          (let* ((tname (or (plist-get tc :name) "tool"))
                 (n (1+ (gethash tname counts 0)))
                 (cand (if (= n 1)
                           (format "call_%s" tname)
                         (format "call_%s_%d" tname n))))
            (while (gethash cand used)
              (setq n (1+ n)
                    cand (format "call_%s_%d" tname n)))
            (puthash tname n counts)
            (puthash cand t used)
            (plist-put tc :id cand)))))))

(defun macher-agent--merge-parallel-open-prompts (items)
  "Merge consecutive ITEMS sharing a `:role' by concatenating vector fields."
  (let ((acc nil))
    (dolist (item items (nreverse acc))
      (let ((prev (car acc))
            (role (and (listp item) (plist-get item :role))))
        (if (and role (listp prev) (equal role (plist-get prev :role)))
            (let ((merged (copy-sequence prev))
                  (tail item))
              (while (consp tail)
                (let ((k (pop tail))
                      (v (pop tail)))
                  (when (and (vectorp v) (vectorp (plist-get merged k)))
                    (setq merged (plist-put merged k (vconcat (plist-get merged k) v))))))
              (setcar acc merged))
          (push item acc))))))

(defun macher-agent--existing-prompts-from-data (data)
  "Extract non-system prompt list from encoded request DATA."
  (let ((vec (cond
              ((plist-member data :contents) (plist-get data :contents))
              ((plist-member data :input) (plist-get data :input))
              ((plist-member data :messages) (plist-get data :messages)))))
    (when (or (vectorp vec) (listp vec))
      (cl-remove-if
       (lambda (msg)
         (and (listp msg)
              (member (plist-get msg :role) '("system" "developer"))))
       (append vec nil)))))

(defun macher-agent--build-prompts-from-buffer (buf backend tool-use &optional info)
  "Build BACKEND prompt list from BUF and open TOOL-USE calls using FSM INFO."
  (let* ((tracking (and (macher-agent--plist-p info) (plist-get info :tracking-marker)))
         (pos (and (macher-agent--plist-p info) (plist-get info :position)))
         (end-pos (when (and (bufferp buf) (buffer-live-p buf))
                    (or (and (markerp tracking) (eq (marker-buffer tracking) buf)
                             (marker-position tracking))
                        (and (markerp pos) (eq (marker-buffer pos) buf)
                             (marker-position pos)))))
         (id-tables nil)
         (prompts
          (when (and (bufferp buf) (buffer-live-p buf)
                     (fboundp 'gptel--create-prompt-buffer))
            (with-current-buffer buf
              (save-excursion
                (goto-char (or end-pos (point-max)))
                (let* ((mark-active nil)
                       (copy-buf (gptel--create-prompt-buffer (point))))
                  (unwind-protect
                      (with-current-buffer copy-buf
                        (setq id-tables (macher-agent--assign-missing-buffer-tool-ids))
                        (setq-local gptel-track-response t)
                        (setq-local gptel-backend backend)
                        (goto-char (point-max))
                        (unless (string-blank-p
                                 (buffer-substring-no-properties (point-min) (point-max)))
                          (gptel--parse-buffer
                           backend
                           (and (bound-and-true-p gptel--num-messages-to-send)
                                (* 2 gptel--num-messages-to-send)))))
                    (when (buffer-live-p copy-buf)
                      (kill-buffer copy-buf))))))))
         (_ (macher-agent--ensure-tool-use-ids
             tool-use (car id-tables) (cdr id-tables)))
         (open-prompts
          (when (and tool-use (fboundp 'gptel--parse-list-and-insert))
            (macher-agent--merge-parallel-open-prompts
             (with-temp-buffer
               (setq-local gptel-track-response t)
               (setq-local gptel-backend backend)
               (let ((acc nil))
                 (dolist (tc tool-use (nreverse acc))
                   (erase-buffer)
                   (gptel--parse-list-and-insert
                    (list (cons 'tool
                                (list :id (plist-get tc :id)
                                      :name (plist-get tc :name)
                                      :args (plist-get tc :args)
                                      :result "pending"))))
                   (goto-char (point-max))
                   (dolist (p (butlast (gptel--parse-buffer backend nil)))
                     (push p acc)))))))))
    (append prompts open-prompts)))

(defun macher-agent--resolve-backend-url (backend model info)
  "Resolve URL string for BACKEND and MODEL given FSM INFO, or nil."
  (when (and backend (fboundp 'gptel-backend-p) (gptel-backend-p backend))
    (let* ((gptel-backend backend)
           (gptel-model (or model (plist-get info :model) (bound-and-true-p gptel-model)))
           (gptel-stream (plist-get info :stream))
           (effective-info (if (and (macher-agent--plist-p info)
                                    (eq (plist-get info :backend) backend))
                               info
                             (plist-put (copy-sequence (and (macher-agent--plist-p info) info))
                                        :backend backend)))
           (raw-url (gptel-backend-url backend)))
      (cond
       ((stringp raw-url) raw-url)
       ((functionp raw-url)
        (ignore-errors
          (condition-case nil
              (funcall raw-url effective-info)
            (wrong-number-of-arguments
             (funcall raw-url)))))
       (t nil)))))

(defun macher-agent--sync-fsm-request-data (fsm compiled-prompt normalized-tools &optional model backend prev-backend)
  "Synchronise `:data', `:stream', `:tool-use', and `:url' on FSM.
Rebuilds `:data' natively via `gptel--request-data' using COMPILED-PROMPT,
NORMALIZED-TOOLS, MODEL, and BACKEND, tracking transitions from PREV-BACKEND."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (let* ((info (gptel-fsm-info fsm))
           (is-plist (macher-agent--plist-p info))
           (data (when is-plist (plist-get info :data)))
           (info-backend (when is-plist (plist-get info :backend)))
           (effective-backend (when is-plist
                                (or backend
                                    info-backend
                                    (bound-and-true-p gptel-backend))))
           (effective-model (or model
                                (when is-plist (plist-get info :model))
                                (bound-and-true-p gptel-model)))
           (model-sym (cond ((symbolp effective-model) effective-model)
                            ((stringp effective-model) (intern effective-model))
                            (t effective-model)))
           (effective-prev-backend (or prev-backend info-backend))
           (backend-changed-p (and effective-backend effective-prev-backend
                                   (not (eq effective-backend effective-prev-backend))))
           (info-updated nil))
      (when is-plist
        (when (and backend (not (eq backend info-backend)))
          (unless info-updated (setq info (copy-sequence info)))
          (setq info (plist-put info :backend backend)
                info-updated t))
        (when (and model (not (eq model (plist-get info :model))))
          (unless info-updated (setq info (copy-sequence info)))
          (setq info (plist-put info :model model)
                info-updated t))
        (cond
         ((and normalized-tools
               (eq (bound-and-true-p gptel-use-tools) 'force)
               (not (eq (plist-get info :use-tools) 'force)))
          (unless info-updated (setq info (copy-sequence info)))
          (setq info (plist-put info :use-tools 'force)
                info-updated t))
         ((and (null normalized-tools)
               (plist-member info :use-tools))
          (unless info-updated (setq info (copy-sequence info)))
          (cl-remf info :use-tools)
          (setq info-updated t)))
        (when (and effective-backend
                   (fboundp 'gptel-backend-p)
                   (gptel-backend-p effective-backend)
                   (fboundp 'gptel-backend-stream))
          (cond
           ((not (gptel-backend-stream effective-backend))
            (when (plist-member info :stream)
              (unless info-updated (setq info (copy-sequence info)))
              (cl-remf info :stream)
              (setq info-updated t)))
           ((and backend-changed-p
                 effective-prev-backend
                 (gptel-backend-p effective-prev-backend)
                 (not (gptel-backend-stream effective-prev-backend))
                 (gptel-backend-stream effective-backend)
                 (bound-and-true-p gptel-stream)
                 (bound-and-true-p gptel-use-curl)
                 (not (plist-get info :stream)))
            (let* ((model-params (when (fboundp 'gptel--model-request-params)
                                   (gptel--model-request-params model-sym)))
                   (stream-spec (plist-get model-params :stream)))
              (when (or (not (memq :stream model-params))
                        (and stream-spec (not (eq stream-spec :json-false))))
                (unless info-updated (setq info (copy-sequence info)))
                (setq info (plist-put info :stream t)
                      info-updated t)))))))
      (cond
       ((and (listp data)
             (keywordp (car data))
             effective-backend
             (fboundp 'gptel-backend-p)
             (gptel-backend-p effective-backend)
             (fboundp 'gptel--request-data))
        (let* ((gptel-backend effective-backend)
               (gptel-model model-sym)
               (gptel-system-prompt
                (when (and (stringp compiled-prompt)
                           (not (string-empty-p compiled-prompt)))
                  compiled-prompt))
               (gptel-tools normalized-tools)
               (gptel-use-tools
                (if normalized-tools
                    (if (or (eq (bound-and-true-p gptel-use-tools) 'force)
                            (eq (plist-get info :use-tools) 'force))
                        'force
                      (or (bound-and-true-p gptel-use-tools) t))
                  nil))
               (gptel-stream (plist-get info :stream))
               (gptel-temperature (or (plist-get info :temperature)
                                      (bound-and-true-p gptel-temperature)))
               (gptel-max-tokens (or (plist-get info :max-tokens)
                                     (bound-and-true-p gptel-max-tokens)))
               (prompts (if backend-changed-p
                            (macher-agent--build-prompts-from-buffer
                             (plist-get info :buffer)
                             effective-backend
                             (plist-get info :tool-use)
                             info)
                          (or (macher-agent--existing-prompts-from-data data)
                              (macher-agent--build-prompts-from-buffer
                               (plist-get info :buffer)
                               effective-backend
                               (plist-get info :tool-use)
                               info))))
               (reencoded (condition-case nil
                              (gptel--request-data effective-backend prompts)
                            (error nil))))
          (when (and (listp reencoded) (keywordp (car reencoded)))
            (unless info-updated (setq info (copy-sequence info)))
            (setq info (plist-put info :data reencoded)
                  info-updated t))))
       ((and (null effective-backend)
             (listp data)
             (keywordp (car data))
             (stringp compiled-prompt)
             (plist-member data :system))
        (unless info-updated (setq info (copy-sequence info)))
        (setq info (plist-put info :data
                              (plist-put (copy-sequence data) :system compiled-prompt))
              info-updated t)))
      (when (and effective-backend
                 (fboundp 'gptel-backend-p)
                 (gptel-backend-p effective-backend)
                 (or backend-changed-p
                     (plist-member info :url)
                     (functionp (gptel-backend-url effective-backend))))
        (let ((resolved-url (macher-agent--resolve-backend-url
                             effective-backend effective-model info)))
          (if resolved-url
              (progn
                (unless info-updated
                  (setq info (copy-sequence info)))
                (setq info (plist-put info :url resolved-url)
                      info-updated t))
            (when (plist-member info :url)
              (unless info-updated
                (setq info (copy-sequence info)))
              (cl-remf info :url)
              (setq info-updated t)))))
      (when info-updated
        (macher-agent--set-fsm-info fsm info)))))

(defun macher-agent--resolve-active-preset-for-buffer (buffer-presets ctx target-buf)
  "Return the primary active preset symbol for TARGET-BUF, or nil.
Inspects BUFFER-PRESETS and CTX."
  (if buffer-presets
      (macher-agent--primary-preset-symbol buffer-presets ctx target-buf t)
    (bound-and-true-p gptel--preset)))

(defun macher-agent--recompile-and-sync-buffer-turn (target-buf &optional context)
  "Recompile transmission state for TARGET-BUF using CONTEXT and sync FSM.
Leaves the buffer prompt unmodified."
  (when (and (bufferp target-buf) (buffer-live-p target-buf))
    (with-current-buffer target-buf
      (let* ((ctx (or context (macher-agent-resolve-context target-buf)))
             (flv (bound-and-true-p file-local-variables-alist))
             (active-fsm (bound-and-true-p macher-agent--active-fsm))
             (fsm-info (when (and active-fsm (fboundp 'gptel-fsm-info))
                         (ignore-errors (gptel-fsm-info active-fsm))))
             (matching-fsm (when (and active-fsm
                                      (macher-agent--plist-p fsm-info)
                                      (eq (plist-get fsm-info :buffer) target-buf))
                             active-fsm))
             (has-live-request-data
              (when matching-fsm
                (let ((d (plist-get fsm-info :data)))
                  (and (listp d) (keywordp (car d))))))
             (buffer-presets (bound-and-true-p macher-agent-presets))
             (active-preset-sym (macher-agent--resolve-active-preset-for-buffer
                                 buffer-presets ctx target-buf))
             (active-preset-spec (when active-preset-sym
                                   (let ((sp (macher-agent--lookup-skill-spec
                                              active-preset-sym ctx target-buf)))
                                     (when (and sp (not (plist-get sp :is-command)))
                                       sp)))))
        (when (and (local-variable-p 'gptel-tools target-buf)
                   (listp gptel-tools)
                   gptel-tools)
          (setq-local gptel-tools
                      (mapcar (lambda (tl)
                                (macher-agent--refresh-tool-struct tl ctx))
                              gptel-tools)))
        (when (and active-preset-spec
                   (not (assq 'gptel-system-prompt flv)))
          (when-let* ((raw-sys (or (plist-get active-preset-spec :system)
                                   (plist-get active-preset-spec :system-message)))
                      ((stringp raw-sys)))
            (setq-local gptel-system-prompt raw-sys)))
        (let* ((known (bound-and-true-p gptel--known-presets))
               (tx-skills (macher-agent--transformer-resolve-skills
                           buffer-presets nil known ctx target-buf))
               (state (macher-agent--compile-transmission-payload
                       target-buf buffer-presets tx-skills nil ctx
                       (if has-live-request-data t :preserve)))
               (norm-tools (macher-agent-normalize-tools
                            (macher-agent-transmission-state-tools state)))
               (resolved-model (macher-agent-transmission-state-model state))
               (resolved-backend (macher-agent-transmission-state-backend state)))
          (setf (macher-agent-transmission-state-tools state) norm-tools)
          (when (macher-agent--has-cached-skill-state-p target-buf)
            (setq-local gptel-tools
                        (macher-agent--ensure-tool-in-list
                         gptel-tools "use_skill" ctx target-buf 'macher-agent-use-skill-tool)))
          (when (and resolved-model (not (assq 'gptel-model flv)))
            (setq-local gptel-model resolved-model))
          (when (and resolved-backend
                     (not (assq 'gptel-backend flv))
                     (not (assq 'gptel--backend-name flv)))
            (setq-local gptel-backend resolved-backend))
          (when matching-fsm
            (macher-agent--update-fsm-info-from-transmission-state matching-fsm state))
          state)))))

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
                     (macher-agent--lookup-skill-spec
                      redirected-skill ctx target-buf known t)))
             (redirect-state (unless is-plist
                               (if (and target-buf (buffer-live-p target-buf))
                                   (with-current-buffer target-buf
                                     (macher-agent-compose-payload
                                      (list :known-presets known)
                                      (list redirected-skill)))
                                 (macher-agent-compose-payload
                                  (list :known-presets known)
                                  (list redirected-skill)))))
             (redirect-text (if is-plist
                                (plist-get redirected-skill :system)
                              (or (plist-get spec :system) (plist-get redirect-state :system))))
             (redirect-tools (if is-plist
                                 (plist-get redirected-skill :tools)
                               (plist-get redirect-state :tools)))
             (redirect-model (if is-plist
                                 (plist-get redirected-skill :model)
                               (or (plist-get spec :model)
                                   (plist-get redirect-state :model))))
             (redirect-backend (if is-plist
                                   (plist-get redirected-skill :backend)
                                 (or (plist-get spec :backend)
                                     (plist-get redirect-state :backend))))
             (is-command (if is-plist
                             (plist-get redirected-skill :is-command)
                           (plist-get spec :is-command))))
        (when (or redirect-model redirect-backend)
          (pcase-let ((`(,resolved-backend . ,resolved-model)
                       (macher-agent--resolve-model-and-backend
                        (macher-agent-transmission-state-backend state)
                        (macher-agent-transmission-state-model state)
                        redirect-backend
                        redirect-model)))
            (when resolved-backend
              (setf (macher-agent-transmission-state-backend state) resolved-backend))
            (when resolved-model
              (setf (macher-agent-transmission-state-model state) resolved-model))))
        (when redirect-text
          (if is-command
              (let* ((raw-args (if ctx
                                   (macher-agent-context-prompt ctx)
                                 (let ((ps (or (previous-single-property-change (point-max) 'gptel)
                                               (point-min))))
                                   (string-trim
                                    (buffer-substring-no-properties
                                     (macher-agent--skip-prompt-prefix ps)
                                     (point-max))))))
                     (cmd-name (if is-plist
                                   (symbol-name (or (plist-get redirected-skill :name-sym) redirected-skill))
                                 (symbol-name redirected-skill)))
                     (clean-args (if (and cmd-name raw-args)
                                     (replace-regexp-in-string
                                      (format "\\`[ \t\r\n#*>]*@%s\\b[ \t]*" (regexp-quote cmd-name)) "" raw-args)
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
  "Synchronise VFS, extract prompt, and compose skills for FSM.
Invokes ASYNC-FN when transformation finishes."
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
        (setq-local macher-agent--turn-submission-delivered nil)
        (when active-fsm
          (setq-local macher-agent--active-fsm active-fsm))
        (when context
          (macher-agent--transformer-sync-context orig-buf context)
          (when (macher-agent-valid-context-p context)
            (unless (local-variable-p 'gptel--known-presets orig-buf)
              (setq-local gptel--known-presets
                          (copy-alist (bound-and-true-p gptel--known-presets))))
            (unless (local-variable-p 'gptel-directives orig-buf)
              (setq-local gptel-directives
                          (copy-alist (bound-and-true-p gptel-directives))))
            (unless (local-variable-p 'gptel--known-tools orig-buf)
              (setq-local gptel--known-tools
                          (copy-tree (bound-and-true-p gptel--known-tools))))
            (ignore-errors (macher-agent-initialize-skills context)))
          (let ((raw-prompt (with-current-buffer temp-buf
                              (string-trim
                               (buffer-substring-no-properties
                                (macher-agent--skip-prompt-prefix prompt-start)
                                (point-max))))))
            (unless (string-empty-p raw-prompt)
              (setf (macher-agent-context-prompt context) raw-prompt)))
          (when-let* ((info-prompt (and info (plist-get info :prompt))))
            (let ((clean-info (replace-regexp-in-string "\\`[#>*[:space:]]+" "" (string-trim info-prompt))))
              (unless (string-empty-p clean-info)
                (setf (macher-agent-context-prompt context) clean-info)))))))

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
                                 buffer-presets inline-skills known context orig-buf))
           (redirected-skill (macher-agent--transformer-detect-redirect
                              inline-preset-used inline-skills known context))
           (state (macher-agent--compile-transmission-payload
                   orig-buf buffer-presets transmission-skills redirected-skill context))
           (normalized-tools
            (if (buffer-live-p orig-buf)
                (with-current-buffer orig-buf
                  (macher-agent-normalize-tools (macher-agent-transmission-state-tools state)))
              (macher-agent-normalize-tools (macher-agent-transmission-state-tools state))))
           (payload (list :model (macher-agent-transmission-state-model state)
                          :backend (or (macher-agent-transmission-state-backend state)
                                       (when (buffer-live-p orig-buf) (buffer-local-value 'gptel-backend orig-buf)))
                          :temperature (macher-agent-transmission-state-temperature state)
                          :max-tokens (macher-agent-transmission-state-max-tokens state)
                          :tools normalized-tools
                          :ptc-primitives (macher-agent-transmission-state-ptc-primitives state))))

      (setf (macher-agent-transmission-state-tools state) normalized-tools)
      (macher-agent--apply-payload-locally payload)

      (when-let* ((compiled (macher-agent-transmission-state-compiled-prompt state)))
        (setq-local gptel-system-prompt compiled))

      (when active-fsm
        (macher-agent--update-fsm-info-from-transmission-state active-fsm state))

      (when-let* ((redirect-text (macher-agent-transmission-state-redirect-prompt state)))
        (let ((replace-from (macher-agent--skip-prompt-prefix prompt-start)))
          (delete-region replace-from (point-max))
          (goto-char replace-from)
          (insert redirect-text))
        (when active-fsm
          (when (fboundp 'gptel-fsm-prompt)
            (with-no-warnings (setf (gptel-fsm-prompt active-fsm) redirect-text)))
          (macher-agent--set-fsm-info active-fsm
                                      (plist-put (gptel-fsm-info active-fsm) :prompt redirect-text)))))

    (when-let* ((fn async-fn)
                ((functionp fn)))
      (funcall fn))))

;;; Transmission Execution

(defun macher-agent--submission-task-id (submission)
  "Extract `:task-id' from staged or delivered SUBMISSION record, or nil."
  (cond
   ((macher-agent--plist-p submission)
    (or (plist-get submission :task-id)
        (when-let* ((payload (plist-get submission :a2a-payload))
                    ((macher-agent-transit-payload-p payload)))
          (macher-agent-transit-payload-task-id payload))))
   ((stringp submission) submission)
   ((and (symbolp submission) (not (memq submission '(nil t))))
    submission)
   (t nil)))

(defun macher-agent--submission-matches-expected-p (submission expected-task-id)
  "Return non-nil when SUBMISSION belongs to EXPECTED-TASK-ID."
  (when submission
    (let ((sub-id (macher-agent--submission-task-id submission)))
      (or (null expected-task-id)
          (null sub-id)
          (equal sub-id expected-task-id)))))

(defun macher-agent--make-transmit-response-hook (on-error &optional on-success expected-task-id)
  "Build response hook invoking ON-ERROR or ON-SUCCESS on turn completion.
Skips ON-SUCCESS while a routing frame awaits `submit_task_result'.
When EXPECTED-TASK-ID is non-nil or routing is active at hook creation,
scope submission and error checks to that task ID."
  (cl-check-type on-error function)
  (let ((hook-fn nil)
        (recovery-attempted nil)
        (expected-task-id
         (or expected-task-id
             (plist-get (car-safe (bound-and-true-p macher-agent--routing-stack)) :task-id)
             (bound-and-true-p macher-agent--current-task-id))))
    (setq
     hook-fn
     (lambda (_beg _end)
       (let* ((res (string-trim (buffer-substring-no-properties (point-min) (point-max))))
              (fsm (macher-agent-get-active-fsm))
              (info (and fsm (fboundp 'gptel-fsm-info) (ignore-errors (gptel-fsm-info fsm))))
              (err-data (and info (plist-get info :error)))
              (tool-calls (and info (plist-get info :tool-use)))
              (routing-stack (bound-and-true-p macher-agent--routing-stack))
              (top-task-id (or (plist-get (car-safe routing-stack) :task-id)
                               (bound-and-true-p macher-agent--current-task-id)))
              (has-routing (or routing-stack
                               (bound-and-true-p macher-agent--current-task-id)))
              (staged (bound-and-true-p macher-agent--staged-submission))
              (delivered (bound-and-true-p macher-agent--turn-submission-delivered))
              (finished (bound-and-true-p macher-agent-task-finished))
              (staged-matches
               (macher-agent--submission-matches-expected-p staged expected-task-id))
              (delivered-matches
               (macher-agent--submission-matches-expected-p delivered expected-task-id))
              (other-submission-active
               (or (and staged (not staged-matches))
                   (and delivered (not delivered-matches))))
              (other-task-on-top
               (and expected-task-id
                    top-task-id
                    (not (equal expected-task-id top-task-id))))
              (expected-frame-gone
               (and expected-task-id
                    (not (equal expected-task-id
                                (bound-and-true-p macher-agent--current-task-id)))
                    (not (cl-some (lambda (frame)
                                    (equal (plist-get frame :task-id) expected-task-id))
                                  routing-stack)))))
         (cond
          ((and err-data (not other-task-on-top))
           (setq-local macher-agent--submit-recovery-pending nil)
           (setq-local macher-agent--staged-submission nil)
           (remove-hook 'gptel-post-response-functions hook-fn t)
           (let ((err-msg (if (stringp err-data) err-data
                            (or (plist-get err-data :message)
                                (format "%s" err-data)))))
             (funcall on-error err-msg)))
          (expected-frame-gone
           (unless other-task-on-top
             (setq-local macher-agent--submit-recovery-pending nil))
           (remove-hook 'gptel-post-response-functions hook-fn t))
          ((or staged-matches
               delivered-matches
               (and finished (not other-task-on-top) (not other-submission-active)))
           (setq-local macher-agent--submit-recovery-pending nil)
           (remove-hook 'gptel-post-response-functions hook-fn t))
          ((or other-submission-active other-task-on-top)
           nil)
          ((string-empty-p res)
           (setq-local macher-agent--submit-recovery-pending nil)
           (remove-hook 'gptel-post-response-functions hook-fn t)
           (funcall on-error "Buffer stopped silently or returned empty."))
          ((and (not has-routing)
                (not tool-calls))
           (setq-local macher-agent--submit-recovery-pending nil)
           (setq-local macher-agent-task-finished t)
           (when-let* ((ctx (bound-and-true-p macher-agent--persistent-context))
                       ((macher-agent-context-p ctx)))
             (setf (macher-agent-session-state (macher-agent-context-session ctx)) :finished))
           (remove-hook 'gptel-post-response-functions hook-fn t)
           (when (functionp on-success)
             (funcall on-success res)))
          ((and has-routing
                (not tool-calls)
                (not finished))
           (if (not recovery-attempted)
               (progn
                 (setq recovery-attempted t)
                 (setq-local macher-agent--submit-recovery-pending t)
                 (save-excursion
                   (goto-char (point-max))
                   (let* ((raw-prefix (or (and (boundp 'gptel-prompt-prefix-alist)
                                               (alist-get major-mode gptel-prompt-prefix-alist))
                                          (when (fboundp 'gptel-prompt-prefix-string)
                                            (ignore-errors (gptel-prompt-prefix-string)))
                                          ""))
                          (prefix (if (stringp raw-prefix) raw-prefix ""))
                          (formatted-prefix
                           (cond
                            ((string-empty-p prefix) "")
                            ((string-match-p "[ \t\r\n]\\'" prefix) prefix)
                            (t (concat prefix " "))))
                          (sep (cond
                                ((bobp) "")
                                ((looking-back "\n\n" (max (point-min) (- (point) 2))) "")
                                ((looking-back "\n" (max (point-min) (- (point) 1))) "\n")
                                (t "\n\n")))
                          (msg-body "CRITICAL: You must submit your final answer using the `submit_task_result` tool. Do not reply with plain text. Call `submit_task_result` now.")
                          (msg-text (concat sep formatted-prefix msg-body))
                          (ins-beg (point)))
                     (insert msg-text)
                     (remove-text-properties ins-beg (point) '(gptel nil front-sticky nil))))
                 (goto-char (point-max))
                 (condition-case send-err
                     (gptel-send)
                   (error
                    (setq-local macher-agent--submit-recovery-pending nil)
                    (remove-hook 'gptel-post-response-functions hook-fn t)
                    (funcall on-error (error-message-string send-err)))))
             (setq-local macher-agent--submit-recovery-pending nil)
             (remove-hook 'gptel-post-response-functions hook-fn t)
             (funcall on-error "Agent halted without invoking submit_task_result.")))))))
    hook-fn))

(defun macher-agent-gptel-transmit (task-context callbacks)
  "Transmit request for TASK-CONTEXT using CALLBACKS plist."
  (let* ((target-buffer (macher-agent-task-context-target-buffer task-context))
         (sys-msg (macher-agent-task-context-system-message task-context))
         (ctx-task-id (macher-agent-task-context-task-id task-context))
         (on-error (plist-get callbacks :on-error))
         (on-success (plist-get callbacks :on-success)))
    (with-current-buffer target-buffer
      (let ((expected-task-id
             (or ctx-task-id
                 (plist-get (car-safe (bound-and-true-p macher-agent--routing-stack)) :task-id)
                 (bound-and-true-p macher-agent--current-task-id))))
        (setq-local macher-agent--turn-submission-delivered nil)
        (when sys-msg
          (setq-local gptel-system-prompt sys-msg))
        (when (functionp on-error)
          (add-hook 'gptel-post-response-functions
                    (macher-agent--make-transmit-response-hook
                     on-error on-success expected-task-id)
                    nil t))
        (goto-char (point-max))
        (gptel-send)))))

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
  "Extract agent context from FSM, falling back to TARGET-BUF."
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

;;; Tool Scope Enforcement and Buffer Setup

(defun macher-agent--enforce-tool-scope (tool &optional fsm &rest _args)
  "Enforce that TOOL is within the authorised scope of FSM."
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
  "Log pre-tool call intent for TOOL, FSM, and ARGS to audit log."
  (let ((context (or (when fsm (macher-agent-gptel-context-from-fsm fsm))
                     (macher-agent-resolve-context)))
        (tool-name (macher-agent-canonical-tool-name tool))
        (tool-args (if (and (macher-agent--plist-p tool) (plist-member tool :args))
                       (plist-get tool :args)
                     args)))
    (when context
      (macher-agent-log-tool-intent context "gptel-tool" tool-name tool-args))))

(defun macher-agent--missing-resolvable-tool-p (saved-names)
  "Return non-nil when any SAVED-NAMES tool is resolvable but not active.
Checks whether a resolvable tool is absent from `gptel-tools'."
  (when (and (listp saved-names) saved-names (fboundp 'gptel-get-tool))
    (let ((active-names
           (delq nil (mapcar #'macher-agent-canonical-tool-name
                             (bound-and-true-p gptel-tools)))))
      (cl-some
       (lambda (tname)
         (when-let* ((resolved (ignore-errors (gptel-get-tool tname))))
           (let ((tools (if (and (listp resolved)
                                 (not (macher-agent--plist-p resolved)))
                            resolved
                          (list resolved))))
             (cl-some
              (lambda (tl)
                (when-let* ((cname (macher-agent-canonical-tool-name tl)))
                  (not (member cname active-names))))
              tools))))
       saved-names))))

(defun macher-agent--needs-preset-reconcile-p ()
  "Return non-nil when file-backed buffer metadata needs re-restoration."
  (when (buffer-file-name)
    (let* ((flv (bound-and-true-p file-local-variables-alist))
           (has-file-meta
            (or (assq 'gptel--preset flv)
                (assq 'gptel--bounds flv)
                (assq 'gptel--tool-names flv)
                (assq 'gptel--backend-name flv)
                (and (derived-mode-p 'org-mode)
                     (fboundp 'org-entry-get)
                     (or (org-entry-get (point-min) "GPTEL_PRESET" 'selective)
                         (org-entry-get (point-min) "GPTEL_BOUNDS" 'selective)
                         (org-entry-get (point-min) "GPTEL_TOOLS" 'selective)))))
           (preset-sym (bound-and-true-p gptel--preset))
           (full-spec (when preset-sym
                        (macher-agent--lookup-skill-spec preset-sym nil (current-buffer))))
           (preset-spec (and preset-sym
                             (not (plist-get full-spec :is-command))
                             (fboundp 'gptel-get-preset)
                             (ignore-errors (gptel-get-preset preset-sym))))
           (parsed-sys (car-safe (gptel--parse-directive gptel-system-prompt))))
      (and has-file-meta
           (or (and preset-spec
                    (not (assq 'gptel-system-prompt flv))
                    (fboundp 'gptel--preset-mismatch-value)
                    (gptel--preset-mismatch-value preset-spec :system parsed-sys))
               (and (derived-mode-p 'org-mode)
                    (null preset-sym)
                    (fboundp 'org-entry-get)
                    (when-let* ((raw-org-preset
                                 (org-entry-get (point-min) "GPTEL_PRESET" 'selective))
                                (trimmed (string-trim raw-org-preset))
                                ((not (string-empty-p trimmed)))
                                (org-preset-sym (macher-normalise-preset-name trimmed))
                                (org-full-spec (macher-agent--lookup-skill-spec
                                                org-preset-sym nil (current-buffer))))
                      (and (not (plist-get org-full-spec :is-command))
                           (fboundp 'gptel-get-preset)
                           (ignore-errors (gptel-get-preset org-preset-sym)))))
               (and (derived-mode-p 'org-mode)
                    (fboundp 'org-entry-get)
                    (when-let* ((org-tools (org-entry-get (point-min) "GPTEL_TOOLS" 'selective)))
                      (macher-agent--missing-resolvable-tool-p (split-string org-tools))))
               (and (bound-and-true-p gptel--tool-names)
                    (macher-agent--missing-resolvable-tool-p gptel--tool-names)))))))

(defun macher-agent--install-gptel-save-hooks ()
  "Register buffer-local save and prompt-transform hooks for Macher Agent."
  (add-hook 'gptel-save-state-hook #'macher-agent--sync-preset-before-save nil t)
  (add-hook 'before-save-hook #'macher-agent--persist-missing-preset-tools 90 t)
  (remove-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer t)
  (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t))

(defun macher-agent--teardown-gptel-buffer ()
  "Remove buffer-local `gptel' hooks installed by Macher Agent."
  (remove-hook 'gptel-save-state-hook #'macher-agent--sync-preset-before-save t)
  (remove-hook 'before-save-hook #'macher-agent--persist-missing-preset-tools t)
  (remove-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer t)
  (dolist (transformer macher-agent-prompt-transformers)
    (remove-hook 'gptel-prompt-transform-functions transformer t))
  (remove-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope t))

(defun macher-agent-setup-gptel-buffer ()
  "Set up a gptel buffer with macher-agent capabilities."
  (setq-local macher-agent--active-fsm nil)
  (when (or (bound-and-true-p macher-agent--is-restored-session)
            (macher-agent--needs-preset-reconcile-p))
    (setq-local macher-agent-presets nil)
    (setq-local macher-agent--is-restored-session nil)
    (macher-agent--restore-saved-gptel-state)
    (macher-agent--sync-preset-before-save)
    (when (fboundp 'gptel--update-header-line)
      (gptel--update-header-line)))
  (dolist (transformer (reverse macher-agent-prompt-transformers))
    (remove-hook 'gptel-prompt-transform-functions transformer t)
    (add-hook 'gptel-prompt-transform-functions transformer nil t))
  (macher-agent--install-gptel-save-hooks)
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
      (macher-agent--install-gptel-save-hooks)
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
          (macher-agent--teardown-gptel-buffer)
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
         (active-preset (bound-and-true-p gptel--preset))
         (active-skill (bound-and-true-p macher-agent--active-skill-sym))
         (parent-ctx (macher-agent-resolve-context parent-buf)))
    (with-current-buffer (generate-new-buffer new-name)
      (funcall parent-mode)
      (gptel-mode)
      (insert content)
      (when active-backend (setq-local gptel-backend active-backend))
      (when active-model (setq-local gptel-model active-model))
      (when active-sys (setq-local gptel-system-prompt active-sys))
      (when active-preset (setq-local gptel--preset active-preset))
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
              (when (memq (gptel-fsm-state fsm) '(ERRS ABRT))
                (setq-local macher-agent--staged-submission nil))
              (when (eq macher-agent--active-fsm fsm)
                (setq-local macher-agent--active-fsm nil))
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
