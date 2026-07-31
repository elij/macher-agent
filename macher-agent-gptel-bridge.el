;;; macher-agent-gptel-bridge.el --- Clean gptel boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; Clean gptel boundary implementation for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'text-property-search)
(require 'macher)
(require 'macher-agent-vfs-client)

(declare-function macher-agent-resolve-context "macher-agent-vfs-client")
(declare-function macher-agent--auto-sync-context "macher-agent-vfs-client")
(declare-function macher-agent--init-workspace-state "macher-agent-vfs-client")
(declare-function macher-agent--split-context "macher-agent-vfs-client")
(declare-function macher-agent--build-virtual-patch "macher-agent-vfs-client")
(declare-function macher-agent--extract-fsm-info "macher-agent-vfs-client")
(declare-function macher-agent--get-context-data "macher-agent-vfs-client")
(declare-function macher-agent--set-context-data "macher-agent-vfs-client")
(declare-function macher-agent-context-root "macher-agent-vfs-client")
(declare-function macher-agent-root "macher-agent-vfs-client")
(declare-function macher-agent--reap-buffer "macher-agent-orchestration")
(declare-function macher-agent-compose-payload "macher-agent-orchestration")
(declare-function macher-agent-subagent-p "macher-agent-orchestration")
(declare-function macher-agent-ready-to-reap-p "macher-agent-orchestration")
(declare-function macher-agent--apply-payload-locally "macher-agent-orchestration")
(declare-function macher-agent-initialize-skills "macher-agent-api")
(declare-function macher-agent-canonical-tool-name "macher-agent-api")

(defvar gptel-system-prompt)
(defvar macher-agent-active-workspaces)

(defcustom macher-agent-prompt-transformers
  '(macher-agent-transformer-deduplicate-tools
    macher-agent-transformer-snip-context)
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

(defcustom macher-agent-max-context-chars 2000000
  "Maximum character length of context history to send in prompt.

Actual length will snap to nearest safe message turn boundary.

Return maximum allowed context characters integer.
Side effects: None."
  :type 'integer
  :group 'macher-agent)

(defvar-local macher-agent--active-ptc-primitives nil
  "Store active Programmatic Tool Calling primitives for current buffer.

Hold a list of primitive tool symbols or names active in the buffer-local
execution context.

Return list of active primitive symbols or names.
Side effects: Buffer-local variable.")

;;; Programmatic Tool Calling (PTC)

(defun macher-agent--build-tool-arg-type-spec (arg lisp-name lisp-arg-name)
  "Build type spec and optional deftype string for ARG.

Construct type spec and optional deftype definition for tool parameter ARG.
LISP-NAME is the Lisp function symbol for the tool.
LISP-ARG-NAME is the string name of the argument.

Return a cons cell (TYPE-SPEC . DEFTYPE-STR).
Side effects: None."
  (let* ((arg-type (plist-get arg :type))
         (items (plist-get arg :items))
         (props (when items (plist-get items :properties))))
    (cond
     ((and (member arg-type '(array "array")) props)
      (let ((type-name (intern (format "%s-%s" lisp-name lisp-arg-name)))
            (prop-strs nil))
        (cl-loop for (p-key p-val) on props by #'cddr
                 for p-type = (plist-get p-val :type)
                 do (push (format ":%s %s"
                                  p-key
                                  (pcase p-type
                                    ((or 'string "string") 'string)
                                    ((or 'number "number") 'number)
                                    ((or 'boolean "boolean") 'boolean)
                                    ((or 'array "array") '(or null (list string)))
                                    (_ 't)))
                          prop-strs))
        (cons (list 'list type-name)
              (format "(cl-deftype %s () \n  \"Property list for %s.\"\n  '(plist %s))"
                      type-name lisp-arg-name
                      (string-join (nreverse prop-strs) " ")))))

     ((member arg-type '(array "array"))
      (cons '(or null (list string)) nil))

     ((member arg-type '(object "object"))
      (cons 'plist nil))

     (t
      (cons (pcase arg-type
              ((or 'string "string") 'string)
              ((or 'number "number") 'number)
              ((or 'boolean "boolean") 'boolean)
              (_ 't))
            nil)))))

(defun macher-agent--format-tool-defun
    (name lisp-name desc arg-names arg-types return-type dummy-return &optional deftypes)
  "Format tool defun string for tool NAME.

Construct Lisp function definition string for tool NAME using given specs.
LISP-NAME is the function symbol.
DESC is the tool description string.
ARG-NAMES is the list of argument name strings.
ARG-TYPES is the list of argument type specifications.
RETURN-TYPE is the declared return type specification.
DUMMY-RETURN is the dummy return value string representation.
DEFTYPES is an optional list of deftype strings.

Return a formatted defun string representation.
Side effects: None."
  (let* ((formatted-arg-types (mapcar (lambda (t-spec)
                                        (if (symbolp t-spec)
                                            (symbol-name t-spec)
                                          (prin1-to-string t-spec)))
                                      (nreverse arg-types)))
         (defun-str
          (format "%s -> (defun %s (%s)\n  %S\n  (declare (type (function (%s) %s)))\n  %s)"
                  name
                  lisp-name
                  (string-join (nreverse arg-names) " ")
                  desc
                  (string-join formatted-arg-types " ")
                  (if (symbolp return-type)
                      (symbol-name return-type) (prin1-to-string return-type))
                  dummy-return)))
    (if deftypes
        (concat (string-join (nreverse deftypes) "\n\n") "\n\n" defun-str)
      defun-str)))

(defun macher-agent--format-tool-lisp-docstring (tool-obj)
  "Infer declarative Lisp signature for TOOL-OBJ.

Extract schema and metadata from TOOL-OBJ to construct a declarative
Lisp signature string with type hints.

Return formatted tool Lisp signature string.
Side effects: None."
  (let* ((name (gptel-tool-name tool-obj))
         (lisp-name (intern (replace-regexp-in-string "_" "-" name)))
         (desc (gptel-tool-description tool-obj))
         (args (gptel-tool-args tool-obj))
         (deftypes nil)
         (arg-names nil)
         (arg-types nil)
         (is-delegate (string-match-p "delegate" name))
         (return-type (if is-delegate '(list string) 'string))
         (dummy-return (if is-delegate "(list \"\")" "\"\"")))

    (dolist (arg args)
      (let* ((raw-name (if (symbolp (plist-get arg :name))
                           (symbol-name (plist-get arg :name))
                         (plist-get arg :name)))
             (lisp-arg-name (replace-regexp-in-string "_" "-" raw-name))
             (spec-pair (macher-agent--build-tool-arg-type-spec arg lisp-name lisp-arg-name))
             (type-spec (car spec-pair))
             (deftype-str (cdr spec-pair)))
        (push lisp-arg-name arg-names)
        (when deftype-str
          (push deftype-str deftypes))
        (push type-spec arg-types)))

    (macher-agent--format-tool-defun
     name lisp-name desc arg-names arg-types return-type dummy-return deftypes)))

(defun macher-agent--filter-ptc-matching-tools (tools active)
  "Filter TOOLS matching any active primitive in ACTIVE.

Iterate through TOOLS list and collect tool objects matching active
primitive symbols or names present in ACTIVE.

Return list of matching gptel tool objects.
Side effects: None."
  (cl-loop for t-obj in tools
           when
           (and (gptel-tool-p t-obj)
                (cl-some
                 (lambda (prim)
                   (string=
                    (replace-regexp-in-string "_" "-"
                                              (if (symbolp prim)
                                                  (symbol-name prim)
                                                prim))
                    (replace-regexp-in-string "_" "-" (gptel-tool-name t-obj))))
                 active))
           collect t-obj))

(defun macher-agent--inject-ptc-prompt
    (existing-prompt &optional active-primitives available-tools)
  "Append Programmatic Tool Calling instructions to EXISTING-PROMPT.

Dynamically append Programmatic Tool Calling instructions with compiler hints
to EXISTING-PROMPT.
ACTIVE-PRIMITIVES is an optional list of active primitive symbols or names.
AVAILABLE-TOOLS is an optional list of gptel tool objects.

Return modified prompt string with PTC instructions, or EXISTING-PROMPT.
Side effects: None."
  (let* ((active (or active-primitives (bound-and-true-p macher-agent--active-ptc-primitives)))
         (tools (or available-tools (bound-and-true-p gptel-tools)))
         (matching-tools (macher-agent--filter-ptc-matching-tools tools active)))
    (if (null matching-tools)
        existing-prompt
      (concat (if-let* ((prompt existing-prompt)) prompt "")
              "\n\n=== PROGRAMMATIC TOOL CALLING (PTC) ===
You are equipped with a `ptc_execution` tool. Instead of outputting multiple sequential \
JSON tool calls, you MUST use `ptc_execution` to orchestrate complex tasks via an Emacs \
Lisp script.

lisp tool aliases:\n"
              (string-join
               (mapcar #'macher-agent--format-tool-lisp-docstring matching-tools) "\n\n")
              "\n\n;; EXPECTED SCRIPT RETURN TYPE:
Keep everything within a let*. Return the output object without formatting.\n"))))

;;; Prompt Transformation and Inline Skill Extraction

(defun macher-agent--extract-inline-skills (prompt-start orig-buf)
  "Extract inline skill tags starting from PROMPT-START in current buffer.

Scan for inline skill tags starting at PROMPT-START and strip matched
tags from current buffer.  ORIG-BUF is the original buffer used to locate
fallback directives if no inline skills were matched.

Return a cons cell (MATCHED-SKILLS . INLINE-PRESET-USED).
Side effects: Modifies buffer text by stripping matched inline tags."
  (let ((matched-skills nil)
        (inline-preset-used nil))
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
    (cons matched-skills inline-preset-used)))

(defun macher-agent--update-fsm-info-from-payload (fsm payload)
  "Update FSM info plist with keys from PAYLOAD if FSM is non-nil.

Copy existing info property list from state machine FSM and update
keys present in PAYLOAD plist.

Return updated info plist, or nil if FSM or PAYLOAD is nil.
Side effects: Mutates info property list of FSM state machine object."
  (when (and fsm payload)
    (let ((new-info (copy-sequence (gptel-fsm-info fsm))))
      (dolist (key '(:system :model :temperature :max-tokens :tools))
        (when (plist-member payload key)
          (setq new-info (plist-put new-info key (plist-get payload key)))))
      (setf (gptel-fsm-info fsm) new-info))))

(defun macher-agent--process-redirected-skill (redirected-skill base-state prompt-start fsm)
  "Process REDIRECTED-SKILL when present as a command prompt.

Replace region in current buffer with system prompt composed from
REDIRECTED-SKILL if present.
BASE-STATE is the base state configuration plist.
PROMPT-START is the point integer in temporary buffer where prompt begins.
FSM is the optional finite-state machine object.

Return nil.
Side effects: Modifies buffer region and updates info property list of FSM."
  (when redirected-skill
    (when-let* ((dummy-base (plist-put (copy-sequence base-state) :system ""))
                (dummy-payload
                 (macher-agent-compose-payload dummy-base (list redirected-skill)))
                (sys-prompt (plist-get dummy-payload :system))
                ((stringp sys-prompt))
                ((not (string-empty-p sys-prompt))))
      (delete-region prompt-start (point-max))
      (insert sys-prompt)
      (when fsm
        (setf (gptel-fsm-info fsm)
              (plist-put (gptel-fsm-info fsm) :prompt sys-prompt))))))

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

(defun macher-agent-transformer-snip-context (async-fn _fsm)
  "Truncate hidden prompt buffer to maximum character limit safely.

ASYNC-FN is a function to call asynchronously.
_FSM is the finite-state machine object.

Return nil.
Side effects: Truncates context history in temporary prompt buffer."
  (when (and (numberp macher-agent-max-context-chars)
             (> (buffer-size) macher-agent-max-context-chars))
    (save-excursion
      (goto-char (point-min))
      (let ((safe-min (point-min)))
        (when (looking-at "^---\n")
          (when (re-search-forward "^---\n" nil t 2)
            (setq safe-min (point))))
        (let* ((target-pt (max safe-min (- (point-max) macher-agent-max-context-chars)))
               (safe-pt safe-min)
               (match nil)
               (found nil))
          (goto-char target-pt)
          (while (and (not found)
                      (setq match (text-property-search-backward 'gptel 'response t))
                      (>= (prop-match-beginning match) safe-min))
            (let* ((end-pt (prop-match-end match))
                   (next-match (save-excursion
                                 (goto-char end-pt)
                                 (text-property-search-forward 'gptel 'response t)))
                   (next-pt (if next-match (prop-match-beginning next-match) (point-max)))
                   (has-tool (save-excursion
                               (goto-char end-pt)
                               (re-search-forward "^[ \t]*\\(```\\|#\\+begin_src \\)tool" next-pt t))))
              (unless has-tool
                (setq safe-pt end-pt)
                (setq found t))))
          (unless found
            (goto-char target-pt)
            (while (and (not found)
                        (setq match (text-property-search-forward 'gptel 'response t)))
              (let* ((end-pt (prop-match-end match))
                     (next-match (save-excursion
                                   (goto-char end-pt)
                                   (text-property-search-forward 'gptel 'response t)))
                     (next-pt (if next-match (prop-match-beginning next-match) (point-max)))
                     (has-tool (save-excursion
                                 (goto-char end-pt)
                                 (re-search-forward "^[ \t]*\\(```\\|#\\+begin_src \\)tool" next-pt t))))
                (unless has-tool
                  (setq safe-pt end-pt)
                  (setq found t)))))
          (when (> safe-pt safe-min)
            (delete-region safe-min safe-pt)
            (goto-char safe-min)
            (insert "\n[... macher-agent truncated early history to conserve tokens ...]\n\n")
            (when (looking-at "^[ \t\n\r]+")
              (replace-match "")))))))
  (when-let* ((fn async-fn)
              ((functionp fn)))
    (funcall fn)))

(defun macher-agent-sync-prompt-transformer (async-fn fsm)
  "Synchronise the VFS and normalise the active tools list.
Compose skill profiles securely.

This function acts exclusively as a pre-wire transformer.  It executes
within the temporary transmission buffer managed by the system.

It parses and strips inline skill tags locally, preventing destructive
side effects to the user's source buffer.  It then invokes the
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
        (macher-agent-initialize-skills ctx)))

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

      (when matched-skills
        (with-current-buffer orig-buf
          (setq-local macher-agent-presets 
                      (delete-dups (append (bound-and-true-p macher-agent-presets) 
                                           (reverse matched-skills))))))

      (let* ((active-presets (with-current-buffer orig-buf (bound-and-true-p macher-agent-presets)))
             (active-sys (with-current-buffer orig-buf gptel-system-prompt))
             (directives (with-current-buffer orig-buf gptel-directives))
             (clean-sys (if (and active-sys (string-match "\\`\\(\\(?:.\\|\n\\)*?\\)\n\n=== PROGRAMMATIC TOOL CALLING" active-sys))
                            (string-trim (match-string 1 active-sys))
                          active-sys)))

        (unless matched-skills
          (if active-presets
              (setq matched-skills (if (listp active-presets)
                                       (append matched-skills active-presets)
                                     (append matched-skills (list active-presets))))
            (when-let* ((sym (cl-loop for (s . sys) in directives
                                      when (equal sys clean-sys) return s)))
              (push sym matched-skills))))

        (let ((redirected-skill nil))
          (when inline-preset-used
            (let ((remaining-text (buffer-substring-no-properties prompt-start (point-max))))
              (unless (string-match-p "[A-Za-z]" remaining-text)
                (setq redirected-skill (pop matched-skills)))))

          (when (let ((base-state
                       (with-current-buffer orig-buf
                         (list :model gptel-model
                               :system clean-sys 
                               :temperature (bound-and-true-p gptel-temperature)
                               :max-tokens (bound-and-true-p gptel-max-tokens)
                               :tools gptel-tools
                               :known-presets (bound-and-true-p gptel--known-presets))))))
            
            (let* ((remaining-skills (nreverse matched-skills))
                   (all-skills (if redirected-skill
                                   (append remaining-skills (list redirected-skill))
                                 remaining-skills))
                   (payload (when all-skills
                              (macher-agent-compose-payload base-state all-skills))))

              (when payload
                (let*
                    ((sys-payload
                      (when remaining-skills
                        (macher-agent-compose-payload base-state remaining-skills)))
                     (final-sys-prompt (if sys-payload
                                           (plist-get sys-payload :system)
                                         (plist-get base-state :system)))
                     (composed-active (plist-get payload :ptc-primitives))
                     (composed-tools (plist-get payload :tools)))

                  (setq payload
                        (plist-put payload
                                   :system
                                   (macher-agent--inject-ptc-prompt
                                    final-sys-prompt composed-active composed-tools))))

                (macher-agent--apply-payload-locally payload)

                (when fsm
                  (let ((new-info (copy-sequence (gptel-fsm-info fsm))))
                    (dolist (key '(:system :model :temperature :max-tokens :tools))
                      (when (plist-member payload key)
                        (setq new-info (plist-put new-info key (plist-get payload key)))))
                    (setf (gptel-fsm-info fsm) new-info))))

              (when redirected-skill
                (when-let* ((dummy-base (plist-put (copy-sequence base-state) :system ""))
                            (dummy-payload
                             (macher-agent-compose-payload dummy-base
                                                           (list redirected-skill)))
                            (sys-prompt (plist-get dummy-payload :system))
                            ((stringp sys-prompt))
                            ((not (string-empty-p sys-prompt))))
                  (delete-region prompt-start (point-max))
                  (insert sys-prompt)
                  (when fsm
                    (setf (gptel-fsm-info fsm)
                          (plist-put (gptel-fsm-info fsm) :prompt sys-prompt))))))
            t))

        (when-let* ((fn async-fn)
                    ((functionp fn)))
          (funcall fn))))))

;;; Transmission and Sub-Agent Reaping

(defun macher-agent--schedule-buffer-reap (&optional buf)
  "Schedule BUF to be reaped asynchronously.

Schedule buffer BUF (defaulting to current buffer) for asynchronous
reaping and disposal.

Return timer object created by `run-at-time'.
Side effects: Schedules asynchronous timer to reap target buffer."
  (let ((target-buf (or buf (current-buffer))))
    (run-at-time 0 nil (lambda ()
                         (when (buffer-live-p target-buf)
                           (macher-agent--reap-buffer target-buf))))))

(defun macher-agent-post-response-reaper (_beg _end)
  "Reap sub-agent buffer if flagged for disposal.

Check if current buffer is a sub-agent and ready to be reaped.
_BEG is the starting position of the text region integer.
_END is the ending position of the text region integer.

Return nil.
Side effects: May schedule current buffer for asynchronous disposal."
  (when (and (macher-agent-subagent-p)
             (macher-agent-ready-to-reap-p))
    (macher-agent--schedule-buffer-reap (current-buffer))))

(defun macher-agent--make-transmit-response-hook (success-cb error-cb)
  "Create a post-response hook closure for transmit.

Generate closure function to handle response completion for sub-agents.
SUCCESS-CB is the success callback function.
ERROR-CB is the error callback function.

Return a closure function for `gptel-post-response-functions'.
Side effects: None."
  (let ((hook-fn nil))
    (setq hook-fn
          (lambda (_beg _end)
            (let ((res (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
              (when (macher-agent-subagent-p)
                (if (not (string-empty-p res))
                    (when success-cb (funcall success-cb res))
                  (when error-cb
                    (funcall error-cb "Buffer stopped silently or returned empty."))))
              (remove-hook 'gptel-post-response-functions hook-fn t))))
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
         (success-cb (plist-get callbacks :on-success))
         (error-cb (plist-get callbacks :on-error)))

    (with-current-buffer target-buffer
      (setq-local gptel-system-prompt (macher-agent--inject-ptc-prompt sys-msg))
      (add-hook 'gptel-post-response-functions
                (macher-agent--make-transmit-response-hook success-cb error-cb)
                nil t)
      (goto-char (point-max))
      (gptel-send))))

;;; Media Injection Advice

(defvar macher-agent--in-media-injection nil
  "Track whether media injection is currently in progress.

Prevent recursive media injection loops during state transitions.

Return non-nil when media injection is active, nil otherwise.
Side effects: Global dynamic binding flag.")

(defun macher-agent--perform-pending-media-injection (fsm)
  "Inject pending media into FSM payload if pending media exists.

Extract pending visual data from context of state machine FSM and inject
it into payload prompts.

Return nil.
Side effects: Invokes media injection functions and resets `:pending-media'
in CTX."
  (let* ((info (macher-agent--extract-fsm-info fsm))
         (ctx (plist-get info :macher-agent-context))
         (pending (when ctx (macher-agent--get-context-data ctx :pending-media))))
    (when pending
      (let* ((macher-agent--in-media-injection t)
             (msg-plist (list :role "user"
                              :content "Tool execution complete. \
Here is the requested visual data:"))
             (prompts (list msg-plist))
             (gptel-context pending))

        (when (fboundp 'gptel--inject-media)
          (gptel--inject-media (plist-get info :backend) prompts))

        (when (fboundp 'gptel--inject-prompt)
          (gptel--inject-prompt (plist-get info :backend)
                                (plist-get info :data)
                                (car prompts)))

        (macher-agent--set-context-data ctx :pending-media nil)))))

(defun macher-agent--inject-media-fsm-advice (orig-fun fsm &rest args)
  "Inject pending tool media into FSM payload before WAIT transition.

Advise FSM state transitions to handle pending tool media before entering
WAIT state.
ORIG-FUN is the original FSM transition function.
FSM is the active state machine structure.
ARGS represents additional transition arguments.

Return result of calling ORIG-FUN.
Side effects: May inject media into FSM payload prior to state transition."
  (if macher-agent--in-media-injection
      (apply orig-fun fsm args)
    (let* ((new-state (car args))
           (target-state (or new-state (ignore-errors (gptel--fsm-next fsm)))))
      (when (or (eq target-state 'WAIT) (null target-state))
        (macher-agent--perform-pending-media-injection fsm))
      (apply orig-fun fsm args))))

(advice-add 'gptel--fsm-transition :around #'macher-agent--inject-media-fsm-advice)

;;; Response Protection and Callback Management

(defun macher-agent--make-safe-callback (orig-cb)
  "Generate safe callback closure wrapping ORIG-CB.

Create closure capturing ORIG-CB so callback survives async network delays.
ORIG-CB is the original callback function.

Return closure function wrapping ORIG-CB.
Side effects: None."
  (lambda (response &rest cb-args)
    (apply orig-cb (or response "") cb-args)))

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
  (funcall orig-fun (or response "") info raw))

(advice-add 'gptel--insert-response :around #'macher-agent--protect-nil-responses)
(advice-add 'gptel-curl--stream-insert-response :around #'macher-agent--protect-nil-responses)

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

(defun macher-agent--gptel-restore-advice (orig-fun &rest args)
  "Bypass `gptel--restore-state' unless allowed, tagging buffer.

Guard state restoration, initialising workspace state and skills when permitted.
ORIG-FUN is the original restoration function.
ARGS represents arguments passed to ORIG-FUN.

Return result of calling ORIG-FUN.
Side effects: May set `macher-agent--is-restored-session' and update workspaces."
  (when macher-agent--allow-gptel-restore
    (setq-local macher-agent--is-restored-session t)
    (let ((current-root (macher-agent-root default-directory)))
      (when current-root
        (macher-agent--init-workspace-state current-root)))
    (let* ((res (apply orig-fun args))
           (restored-ctx (bound-and-true-p macher-agent--persistent-context))
           (ctx (or restored-ctx (ignore-errors (macher-agent-resolve-context))))
           (current-root (macher-agent-root default-directory)))
      (when ctx
        (macher-agent--register-context-workspace-paths ctx current-root)
        (macher-agent-initialize-skills ctx))
      res)))

(advice-add 'gptel--restore-state :around #'macher-agent--gptel-restore-advice)

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
         (fsm-obj (or fsm
                      (bound-and-true-p macher-agent--active-fsm)
                      (bound-and-true-p macher--fsm-latest)
                      (bound-and-true-p gptel--fsm-last)))
         (info (when fsm-obj (gptel-fsm-info fsm-obj)))
         (fsm-tools (when info (plist-get info :tools)))
         (authorised-names (mapcar #'macher-agent-canonical-tool-name fsm-tools)))
    (unless (and canonical-name (member canonical-name authorised-names))
      (list
       :block (format "ERROR: Tool '%s' is not accessible in this context or is no longer available. Please select another tool or approach."
                      (or canonical-name tool))))))

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

      (dolist (transformer macher-agent-prompt-transformers)
        (add-hook 'gptel-prompt-transform-functions transformer t t))
      (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t))))

(defvar macher-agent--active-fsm nil
  "Store active FSM state machine during tool execution hooks.

Hold dynamic binding of active state machine struct during tool hook evaluation.

Return active FSM struct or nil.
Side effects: Global dynamic binding variable.")

(defun macher-agent--bind-active-fsm-advice (orig-fn fsm &rest args)
  "Capture FSM state machine during tool handling execution.

Bind `macher-agent--active-fsm' dynamically around ORIG-FN invocation.
ORIG-FN is the original tool handler function.
FSM is the active state machine structure.
ARGS represents additional arguments passed to ORIG-FN.

Return result of calling ORIG-FN.
Side effects: Dynamically binds `macher-agent--active-fsm'."
  (let* ((macher-agent--active-fsm fsm)
         (tool (car-safe args)))

    (if (and args (not tool))
        (let ((dummy (gptel-make-tool :name "unavailable_tool"
                                      :function #'ignore
                                      :description "")))
          (apply orig-fn fsm dummy (cdr args)))

      (apply orig-fn fsm args))))

(advice-add 'gptel--handle-pre-tool :around #'macher-agent--bind-active-fsm-advice)
(advice-add 'gptel--handle-tool-use :around #'macher-agent--bind-active-fsm-advice)
(advice-add 'gptel--handle-post-tool :around #'macher-agent--bind-active-fsm-advice)

(provide 'macher-agent-gptel-bridge)
;;; macher-agent-gptel-bridge.el ends here
