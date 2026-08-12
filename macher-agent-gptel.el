;;; macher-agent-gptel.el --- Clean gptel boundary -*- lexical-binding: t; -*-

;;; Commentary:

;; Clean gptel boundary implementation for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'text-property-search)
(require 'macher-agent-core)
(require 'macher-agent-vfs)
(require 'macher-agent-presets)

(defvar gptel-system-prompt)
(defvar macher-agent-active-workspaces)
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

(defun macher-agent--tools-require-complex-types-p (tools)
  "Check if any of the TOOLS require complex object or array arguments."
  (cl-some (lambda (tool)
             (cl-some (lambda (arg)
                        (let ((type (plist-get arg :type)))
                          (member type '(array "array" object "object"))))
                      (gptel-tool-args tool)))
           tools))

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
      (concat
       (if-let* ((prompt existing-prompt)) prompt "")
       "\n\n=== PROGRAMMATIC TOOL CALLING (PTC) ===\n"
       "You are equipped with a `ptc_execution` tool. Instead of outputting multiple sequential \\\n"
       "JSON tool calls, you MUST use `ptc_execution` to orchestrate complex tasks via an Emacs \\\n"
       "Lisp script.\n\n"
       "lisp tool aliases:\n"
       (string-join
        (mapcar #'macher-agent--format-tool-lisp-docstring matching-tools) "\n\n")
       (if (macher-agent--tools-require-complex-types-p matching-tools)
           "\n\n;; JSON TO EMACS LISP MAPPING RULES:\n;; 1. JSON Objects MUST translate to flat property lists using keywords (for example, `(:key \"value\")`). DO NOT use alists or dotted pairs.\n;; 2. JSON Arrays MUST translate to standard Lisp lists. Construct them using the `list` function rather than quoting.\n;; Syntax example for complex arguments: `(tool-name (list (list :buffer_name \"agent\" :instructions \"Task\")))`"
         "")
       "\n\n;; EXPECTED SCRIPT RETURN TYPE:\n"
       "Keep everything within a let*. Return the output object without formatting.\n"))))

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
                  (memq (char-before (match-beginning 0)) '(?\s ?\t ?\n ?\r ?>)))
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

(defun macher-agent-transformer-snip-context (async-fn _fsm)
  "Truncate hidden prompt buffer to maximum character limit safely.

ASYNC-FN is a function to call asynchronously.
_FSM is the finite-state machine object.

Return nil.
Side effects: Truncates context history in temporary prompt buffer."
  (let* ((token-limit (or (bound-and-true-p gptel-max-tokens) 2048))
         (max-chars (* token-limit macher-agent-token-multiplier)))

    (when (> (buffer-size) max-chars)
      (save-excursion
        (goto-char (point-min))
        (let ((safe-min (point-min)))
          (when (looking-at "^---\n")
            (when (re-search-forward "^---\n" nil t 2)
              (setq safe-min (point))))
          (let* ((target-pt (max safe-min (- (point-max) max-chars)))
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
                     (has-tool
                      (save-excursion
                        (goto-char end-pt)
                        (re-search-forward "^[ \t]*\\(```\\|#\\+begin_src \\)tool" next-pt t))))
                (unless has-tool
                  (setq safe-pt end-pt)
                  (setq found t))))
            (unless found
              (goto-char target-pt)
              (while (and (not found)
                          (setq match (text-property-search-forward 'gptel 'response t)))
                (let*
                    ((end-pt (prop-match-end match))
                     (next-match (save-excursion
                                   (goto-char end-pt)
                                   (text-property-search-forward 'gptel 'response t)))
                     (next-pt (if next-match (prop-match-beginning next-match) (point-max)))
                     (has-tool
                      (save-excursion
                        (goto-char end-pt)
                        (re-search-forward "^[ \t]*\\(```\\|#\\+begin_src \\)tool" next-pt t))))
                  (unless has-tool
                    (setq safe-pt end-pt)
                    (setq found t)))))
            (when (> safe-pt safe-min)
              (let ((lines-deleted (count-lines safe-min safe-pt)))
                (delete-region safe-min safe-pt)
                (goto-char safe-min)
                (insert (format "\n[... SYSTEM ALERT: macher-agent truncated %d lines of early \
history to conserve tokens. If you need this context, use the `search_conversation_history` tool \
...]\n\n" lines-deleted))
                (when (looking-at "^[ \t\n\r]+")
                  (replace-match ""))))))))
    (when-let* ((fn async-fn)
                ((functionp fn)))
      (funcall fn))))

(defun macher-agent--transformer-sync-context (fsm orig-buf)
  "Synchronise workspace context for FSM in ORIG-BUF buffer.

Resolve context object for finite-state machine FSM in original buffer
ORIG-BUF, update FSM info plist, auto-synchronise context and initialise
skills.

Return context object or nil.
Side effects: Updates FSM info plist and buffer local state."
  (with-current-buffer orig-buf
    (let* ((ctx (ignore-errors (macher-agent-resolve-context fsm)))
           (pers-ctx (bound-and-true-p macher-agent--persistent-context))
           (target-ctx (or pers-ctx ctx)))

      (when (and ctx fsm (fboundp 'gptel-fsm-info))
        (let ((info (gptel-fsm-info fsm)))
          (unless (plist-get info :macher-agent-context)
            (setf (gptel-fsm-info fsm) (plist-put info :macher-agent-context ctx)))))

      (when target-ctx
        (when (fboundp 'macher-agent--auto-sync-context)
          (macher-agent--auto-sync-context target-ctx))
        (when (fboundp 'macher-agent-initialize-skills)
          (macher-agent-initialize-skills target-ctx)))

      (when-let* ((active-sys (or (bound-and-true-p macher-agent-base-system-prompt)
                                  (bound-and-true-p gptel-system-prompt)))
                  (directives (bound-and-true-p gptel-directives))
                  (ui-fallback-sym (cl-loop for (s . sys) in directives
                                            when (equal sys active-sys) return s)))
        (let ((existing (bound-and-true-p macher-agent-presets)))
          (when-let* (((not (memq ui-fallback-sym existing))))
            (setq-local macher-agent-presets (list ui-fallback-sym)))))

      ctx)))

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
        (when-let* (((not (memq ui-fallback-sym existing))))
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
        (when-let* (((not exclusive-found)))
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

(defvar macher-agent-transmission-pipeline-functions
  '(macher-agent-pipe--hydrate-base-state
    macher-agent-pipe--apply-skills-and-presets
    macher-agent-pipe--extract-redirect
    macher-agent-pipe--inject-dynamic-context-tools
    macher-agent-pipe--process-hidden-blocks
    macher-agent-pipe--init-core-directives
    macher-agent-pipe--append-boot-directive
    macher-agent-pipe--append-ptc-directive
    macher-agent-pipe--drain-thought-queue
    macher-agent-pipe--compile-directives)
  "Chained reducer pipeline to build final transmission state payload.")

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
    (push "CRITICAL DIRECTIVE: You MUST use the `submit_task_result` tool to submit your final \
answer when you are completely finished. Do NOT output your final answer as standard text. IMMEDIATELY \
STOP after."
          (macher-agent-transmission-state-directives state)))
  state)

(defun macher-agent-pipe--append-boot-directive (state orig-buf _presets _skills _redirect)
  "Append boot directive from ORIG-BUF to STATE on initial request turn."
  (let ((boot-dir (buffer-local-value 'macher-agent--boot-directive orig-buf)))
    (when (and boot-dir (stringp boot-dir) (not (string-empty-p boot-dir))
               (not (with-current-buffer orig-buf
                      (text-property-any (point-min) (point-max) 'gptel 'response))))
      (push boot-dir (macher-agent-transmission-state-directives state))))
  state)

(defun macher-agent-pipe--append-ptc-directive (state _orig-buf _presets _skills _redirect)
  "Append Programmatic Tool Calling hint directives and aliases."
  (let ((prims (macher-agent-transmission-state-ptc-primitives state)))
    (when prims
      (let* ((tools (macher-agent-transmission-state-tools state))
             (compiled-ptc-block (macher-agent--inject-ptc-prompt "" prims tools)))
        (unless (string-empty-p compiled-ptc-block)
          (push (string-trim compiled-ptc-block)
                (macher-agent-transmission-state-directives state))))))
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
  (seq-reduce (lambda (state pipe-fn)
                (funcall pipe-fn state orig-buf presets skills redirected-skill))
              macher-agent-transmission-pipeline-functions
              (make-macher-agent-transmission-state :target-buffer orig-buf)))

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

(defun macher-agent-sync-prompt-transformer (async-fn &optional fsm)
  "Synchronise the VFS and normalise the active tools list.
Compose skill profiles securely.

ASYNC-FN is a function to call asynchronously upon completion.
FSM is the optional finite-state machine object.

Return nil.
Side effects: Synchronises context, updates buffer local state,
and transforms prompt."
  (let* ((temp-buf (current-buffer))
         (active-fsm (or fsm 
                         (bound-and-true-p gptel--fsm)
                         (bound-and-true-p macher-agent--active-fsm)
                         (bound-and-true-p gptel--fsm-last)))
         (info (when-let* ((fsm-obj active-fsm)
                           ((fboundp 'gptel-fsm-info)))
                 (ignore-errors (gptel-fsm-info fsm-obj))))
         (orig-buf (or (and info (plist-get info :buffer)) temp-buf)))
    
    (when (buffer-live-p orig-buf)
      (macher-agent--transformer-sync-context active-fsm orig-buf)
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

(defun macher-agent--make-transmit-response-hook (_success-cb error-cb)
  "Create a post-response hook closure for transmit.

Generate closure function to handle response completion for sub-agents.
_SUCCESS-CB is the success callback function.
ERROR-CB is the error callback function.

Return a closure function for `gptel-post-response-functions'.
Side effects: None."
  (let ((hook-fn nil))
    (setq hook-fn
          (lambda (_beg _end)
            (when (macher-agent-subagent-p)
              (let ((res (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
                (when (and error-cb (string-empty-p res))
                  (funcall error-cb "Buffer stopped silently or returned empty."))))
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
                      (macher-agent--get-fsm-latest)))
         (info (when fsm-obj (gptel-fsm-info fsm-obj)))
         (fsm-tools (when info (plist-get info :tools)))
         (authorised-names (mapcar #'macher-agent-canonical-tool-name fsm-tools)))
    (unless (and canonical-name (member canonical-name authorised-names))
      (list
       :block (format "ERROR: Tool '%s' is not accessible in this context or is no longer \
available. Please select another tool or approach."
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
            (macher-agent--make-vfs-context :workspace workspace :contents nil)))
       (buffer-context (macher-agent--clone-context canonical-context)))
    (setq-local macher--workspace workspace)
    (macher-agent--inject-context-state buffer-context)

    (unless existing
      (macher-agent--register-active-workspace-root workspace-root canonical-context))

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

      (setq-local macher-agent-parent-buffer parent-name)

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
Side effects: Clears `macher-agent--persistent-context` and resets FSM context."
  (interactive)
  (if (not (bound-and-true-p macher-agent--persistent-context))
      (message "Macher-Agent: No persistent context to clear in buffer '%s'." (buffer-name))
    (let*
        ((ws (or (and (bound-and-true-p macher-agent--persistent-context)
                      (macher-agent--get-context-workspace macher-agent--persistent-context))
                 (ignore-errors (macher-workspace (current-buffer)))
                 (bound-and-true-p macher--workspace)))
         (fresh-ctx (when ws (macher-agent--make-vfs-context :workspace ws :contents nil)))
         (root (and ws (macher-agent-workspace-project-root ws)))
         (expanded-root (and root (expand-file-name root))))
      (setq macher-agent--persistent-context fresh-ctx)
      (when (and expanded-root (not (bound-and-true-p macher-agent--is-subagent)))
        (macher-agent--register-active-workspace-root expanded-root fresh-ctx))
      (macher-agent-bridge-reset-fsm-context fresh-ctx)
      (message
       "Macher-Agent: VFS context successfully cleared. Agent reset to physical baseline."))))

(defun macher-agent-bridge-transmit ()
  "Bridge function to trigger network transmission in current buffer."
  (gptel-send))

(defun macher-agent-bridge-abort (buffer)
  "Bridge function to abort network transmission in BUFFER."
  (gptel-abort buffer))

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

(defun macher-agent--extract-fsm-info (fsm)
  "Extract the info plist from a finite-state machine (FSM).

FSM is the finite-state machine object or info plist.

Return the info property list."
  (cond
   ((null fsm) nil)
   ((and (fboundp 'gptel-fsm-p) (gptel-fsm-p fsm)) (gptel-fsm-info fsm))
   ((listp fsm) fsm)
   ((fboundp 'gptel-fsm-info) (ignore-errors (gptel-fsm-info fsm)))
   (t nil)))

(defun macher-agent--extract-fsm-context (fsm)
  "Extract the active context from a finite-state machine (FSM).

FSM is the finite-state machine object.

Return the active context structure, or nil."
  (when-let* ((fsm fsm)
              (info (macher-agent--extract-fsm-info fsm)))
    (or (plist-get info :macher-agent-context)
        (plist-get info :macher--context))))

(defun macher-agent-bridge-get-active-context ()
  "Retrieve the active context from the backend state machine."
  (let ((fsm (or (bound-and-true-p macher-agent--active-fsm)
                 (macher-agent--get-fsm-latest))))
    (macher-agent--extract-fsm-context fsm)))

(defun macher-agent-bridge-get-target-buffer ()
  "Retrieve the target buffer of the current backend execution."
  (let* ((fsm (or (bound-and-true-p macher-agent--active-fsm)
                  (macher-agent--get-fsm-latest)))
         (info (ignore-errors (gptel-fsm-info fsm))))
    (plist-get info :buffer)))

(defun macher-agent-bridge-reset-fsm-context (fresh-ctx)
  "Bridge function to reset context in the backend state machine to FRESH-CTX.

FRESH-CTX is the new context structure.

Return nil.
Side effects: Mutates info property list of the active backend FSM if bound."
  (when (and (boundp 'gptel--fsm) gptel--fsm)
    (let ((info (gptel-fsm-info gptel--fsm)))
      (when (plist-get info :macher--context)
        (setf (gptel-fsm-info gptel--fsm) (plist-put info :macher--context fresh-ctx))
        (setf (gptel-fsm-info gptel--fsm)
              (plist-put info :macher-agent-context fresh-ctx))))))

;;;

(defun macher-agent--bind-active-fsm-advice (orig-fn fsm &rest args)
  "Capture FSM state machine with ORIG-FN during tool handling execution."
  (let* ((macher-agent--active-fsm fsm)
         (tool (car-safe args))
         (info (when fsm (ignore-errors (gptel-fsm-info fsm))))
         (target-buf (when info (plist-get info :buffer)))
         (ctx (when (and target-buf (buffer-live-p target-buf))
                (buffer-local-value 'macher-agent--persistent-context target-buf))))

    (when ctx
      (when (fboundp 'macher-agent--auto-sync-context)
        (macher-agent--auto-sync-context ctx)))

    (if (and args (not tool))
        (let ((dummy (gptel-make-tool :name "unavailable_tool"
                                      :function #'ignore
                                      :description "")))
          (apply orig-fn fsm dummy (cdr args)))
      (apply orig-fn fsm args))))

(advice-add 'gptel--handle-pre-tool :around #'macher-agent--bind-active-fsm-advice)
(advice-add 'gptel--handle-tool-use :around #'macher-agent--bind-active-fsm-advice)
(advice-add 'gptel--handle-post-tool :around #'macher-agent--bind-active-fsm-advice)

(provide 'macher-agent-gptel)
;;; macher-agent-gptel.el ends here
