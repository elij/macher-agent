;;; macher-agent-orchestration.el --- Orchestration -*- lexical-binding: t; -*-

;;; Commentary:

;; Interactive orchestration commands for Macher Agent.  This file provides
;; functions to coordinate and execute tasks.

;;; Code:

(require 'macher)
(require 'macher-agent-macher-bridge)
(require 'gptel nil t)
(require 'macher-agent-vfs-client)
(require 'macher-agent-gptel-tools)
(require 'generator)
(require 'subr-x)
(require 'cl-lib)

(declare-function macher-agent-gptel-transmit "macher-agent-gptel-bridge" (task-context callbacks))
(declare-function macher-agent-sync-prompt-transformer "macher-agent-gptel-bridge" (async-fn fsm))
(declare-function macher-agent-post-response-reaper "macher-agent-gptel-bridge" (beg end))
(declare-function macher-agent--enforce-tool-scope "macher-agent-gptel-bridge" (tool &optional fsm &rest args))
(declare-function macher-agent-resolve-context "macher-agent-vfs-client" (&optional ctx-or-fsm))
(declare-function macher-agent--inject-context-state "macher-agent-vfs-client" (context &optional directives))
(declare-function macher-agent--init-workspace-state "macher-agent-vfs-client")
(declare-function macher-agent--auto-sync-context "macher-agent-vfs-client" (&optional ctx fsm))
(declare-function macher-agent-normalize-tools "macher-agent-api" (tools))
(declare-function macher-agent-resolve-tool "macher-agent-api" (tool-name tools-registry &optional dir-context context))
(declare-function macher-agent-initialize-skills "macher-agent-api" (&optional context dir))
(declare-function macher-agent-scope-add-file "macher-agent-api" (buffer-name context))
(declare-function macher-agent-sandbox--eval-iter "macher-agent-sandbox" (ast env))
(declare-function macher-agent--read-file-vfs-aware "macher-agent-gptel-tools" (file-path context))
(declare-function macher-agent-context-root "macher-agent-vfs-client" (context))
(declare-function macher-agent--merge-contexts "macher-agent-vfs-client" (parent-ctx child-ctx))
(declare-function macher-agent--clone-context "macher-agent-vfs-client" (ctx))
(declare-function macher-agent--get-context-workspace "macher-agent-vfs-client" (ctx))
(declare-function macher-agent--get-context-contents "macher-agent-vfs-client" (ctx))
(declare-function macher-agent-workspace-active-subagents "macher-agent-vfs-client" (ws-or-ctx))
(declare-function macher-agent--set-workspace-active-subagents "macher-agent-vfs-client" (ws-or-ctx val))
(declare-function macher-agent-vfs-entry-path "macher-agent-vfs-client" (entry))
(declare-function macher-agent-vfs-entry-curr "macher-agent-vfs-client" (entry))
(declare-function macher-context-p "macher-agent-macher-bridge" (ctx))
(declare-function macher-agent--show-ui "macher-agent-gptel-tools" (buf))
(declare-function make-macher-agent-delegate-response "macher-agent-gptel-tools" (&rest args))
(declare-function gptel-abort "gptel" (&optional buffer))
(declare-function gptel-get-tool "gptel" (name))
(declare-function gptel--modify-value "gptel" (value modification))

(defvar macher-agent-submit-task-result-tool nil
  "Store the tool object for task result submission.

Holds the gptel tool object that sub-agents call to submit their final result.

Return the submit task result tool object, or nil.

Side effects: None.")

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
        (cl-some (lambda (buf)
                   (when (or (equal (buffer-name buf) name)
                             (and (buffer-file-name buf)
                                  (or (equal (buffer-file-name buf) name)
                                      (equal (buffer-file-name buf) (expand-file-name name)))))
                     (buffer-name buf)))
                 (buffer-list))
        name))
   (t name)))

(defun macher-agent--handle-parallel-task-result (idx result completed-flags results counter-box total final-callback)
  "Record RESULT for task IDX, updating COMPLETED-FLAGS and RESULTS.

IDX is the zero-based task index.
RESULT is the task result payload object.
COMPLETED-FLAGS is a vector tracking completion status per task index.
RESULTS is a list storing results in task index order.
COUNTER-BOX is a cons cell storing the number of completed tasks.
TOTAL is the total number of tasks.
FINAL-CALLBACK is the function to run when all tasks complete.

Return nil.

Side effects: Mutates COMPLETED-FLAGS, RESULTS, and COUNTER-BOX."
  (unless (aref completed-flags idx)
    (aset completed-flags idx t)
    (setf (nth idx results) result)
    (setcar counter-box (1+ (car counter-box)))
    (when (= (car counter-box) total)
      (funcall final-callback results))))

(defun macher-agent-execute-parallel (tasks final-callback)
  "Execute TASKS in parallel and run FINAL-CALLBACK upon completion.

TASKS is the list of tasks to spawn.
FINAL-CALLBACK is the function to run with the results.

Return nil.

Side effects: Spawns parallel tasks."
  (let* ((task-list (append tasks nil))
         (total (length task-list))
         (counter-box (cons 0 nil))
         (results (make-list total nil))
         (completed-flags (make-vector total nil)))
    (if (= total 0)
        (funcall final-callback nil)
      (cl-loop for task in task-list
               for index from 0
               do (let ((idx index))
                    (macher-agent-spawn-task
                     task
                     (lambda (result)
                       (macher-agent--handle-parallel-task-result
                        idx result completed-flags results counter-box total final-callback))))))))

(defun macher-agent-subagent-p (&optional buffer)
  "Check whether BUFFER is a sub-agent buffer.

BUFFER is the optional target buffer, defaulting to the current buffer.

Return non-nil if BUFFER is a sub-agent, otherwise nil.

Side effects: None."
  (with-current-buffer (or buffer (current-buffer))
    (bound-and-true-p macher-agent--is-subagent)))

(defun macher-agent-ready-to-reap-p (&optional buffer)
  "Check whether BUFFER is ready to be reaped.

BUFFER is the optional target buffer, defaulting to the current buffer.

Return non-nil if BUFFER is ready to be reaped, otherwise nil.

Side effects: None."
  (with-current-buffer (or buffer (current-buffer))
    (bound-and-true-p macher-agent--ready-to-reap)))

(defun macher-agent--reap-buffer (buf)
  "Reap sub-agent BUF by aborting gptel operations and killing it.

BUF is the sub-agent buffer to reap.

Return nil.

Side effects: Aborts pending gptel operations and kills BUF."
  (when (and (buffer-live-p buf)
             (with-current-buffer buf
               (and (macher-agent-subagent-p)
                    (macher-agent-ready-to-reap-p))))
    (with-current-buffer buf
      (set-buffer-modified-p nil)
      (gptel-abort buf)
      (let ((kill-buffer-query-functions nil)
            (kill-buffer-hook nil))
        (kill-buffer buf)))))

(defun macher-normalise-preset-name (preset)
  "Normalise PRESET name by stripping leading @ symbols.

PRESET is the preset identifier (string, symbol, or list).

Return the normalised preset symbol, or nil.

Side effects: None."
  (let ((str (cond ((stringp preset) preset)
                   ((symbolp preset) (symbol-name preset))
                   ((and (listp preset) (stringp (car preset))) (car preset))
                   (t (format "%s" preset)))))
    (intern (replace-regexp-in-string "^@+" "" str))))

(defun macher-agent--compose-merge-system-prompt (current-sys sym sys-spec)
  "Merge SYS-SPEC into CURRENT-SYS for skill SYM.

CURRENT-SYS is the existing system prompt (string, list, or nil).
SYM is the preset symbol.
SYS-SPEC is the system prompt specification (string or modifier).

Return the updated system prompt value.

Side effects: None."
  (if (stringp sys-spec)
      (let ((cur (if (listp current-sys)
                     (string-join current-sys "\n---\n")
                   (or current-sys ""))))
        (concat cur
                (if (string-empty-p cur) "" "\n---\n")
                (format "### Skill: %s\n%s\n" sym sys-spec)))
    (gptel--modify-value current-sys sys-spec)))

(defun macher-agent--compose-merge-tools (current-tools tools-spec)
  "Merge TOOLS-SPEC into CURRENT-TOOLS.

CURRENT-TOOLS is the current list of tools.
TOOLS-SPEC is the tools specification (modifier, list, string, or similar).

Return the updated tools list.

Side effects: None."
  (let ((merged-tools (gptel--modify-value current-tools tools-spec)))
    (cl-loop for t-obj in (if (listp merged-tools) merged-tools (list merged-tools))
             collect (if (stringp t-obj)
                         (or (ignore-errors (gptel-get-tool t-obj)) t-obj)
                       t-obj))))

(defun macher-agent--resolve-preset-or-tool (sym known)
  "Resolve SYM against KNOWN presets or gptel tools.

SYM is the preset or tool symbol or string.
KNOWN is an alist of known preset specifications.

Return a cons cell (TYPE . VALUE) where TYPE is 'preset or 'tool, or nil.

Side effects: None."
  (when-let* ((clean-sym (macher-normalise-preset-name sym)))
    (if-let* ((spec (alist-get clean-sym known)))
        (cons 'preset spec)
      (when-let* ((tool (or (ignore-errors (gptel-get-tool (symbol-name clean-sym)))
                            (ignore-errors (gptel-get-tool (replace-regexp-in-string "-" "_" (symbol-name clean-sym)))))))
        (cons 'tool tool)))))

(defun macher-agent-compose-payload (base-state inline-presets)
  "Compose a transmission payload by merging BASE-STATE with INLINE-PRESETS.

Merges BASE-STATE with the resolved configuration of INLINE-PRESETS
using native gptel modification values.

The function clones the initial base state and processes each preset or
standalone tool sequentially.  For each preset, it recursively resolves
any declared parent dependencies first.

If a preset declares the `:exclusive' flag, it completely clears the
accumulated system prompt and tools before applying its own configurations,
preventing base environment bleed.

System prompts and tools are merged using gptel's declarative modifier logic.
Legacy string-based system prompts are automatically wrapped in native
append operations, and the original `\n---\n' delimiter is dynamically
injected if a preceding system prompt exists.  Flat properties like model,
temperature, and token limits are applied as direct overrides.

Finally, if available, `macher-agent-normalize-tools' is invoked on the
composed tools list.

BASE-STATE is the base state property list.
INLINE-PRESETS is the list of inline preset symbols.

Return the unified state property list.

Side effects: None."
  (let ((state (copy-sequence base-state))
        (known (plist-get base-state :known-presets)))

    (cl-labels
        ((apply-spec (sym spec)
           (when-let* ((parents (plist-get spec :parents)))
             (dolist (parent (if (listp parents) parents (list parents)))
               (when-let* ((parent-spec (alist-get parent known)))
                 (apply-spec parent parent-spec))))

           (when (plist-get spec :exclusive)
             (setq state (plist-put state :system nil))
             (setq state (plist-put state :tools nil))
             (setq state (plist-put state :ptc-primitives nil)))

           (when-let* ((sys-spec (or (plist-get spec :system) (plist-get spec :system-message))))
             (setq state (plist-put state :system
                                    (macher-agent--compose-merge-system-prompt
                                     (plist-get state :system) sym sys-spec))))

           (when-let* ((tools-spec (or (plist-get spec :tools) (plist-get spec :allowed-tools))))
             (setq state (plist-put state :tools
                                    (macher-agent--compose-merge-tools
                                     (plist-get state :tools) tools-spec))))

           (when-let* ((ptc-spec (plist-get spec :ptc-primitives)))
             (let ((current-ptc (plist-get state :ptc-primitives))
                   (ptc-list (if (listp ptc-spec) ptc-spec (list ptc-spec))))
               (setq state (plist-put state :ptc-primitives
                                      (cl-delete-duplicates (append current-ptc ptc-list) :test #'equal)))))

           (dolist (key '(:model :backend :temperature :max-tokens))
             (when-let* ((val (plist-get spec key)))
               (setq state (plist-put state key val))))))

      (dolist (sym inline-presets)
        (pcase (macher-agent--resolve-preset-or-tool sym known)
          (`(preset . ,spec)
           (apply-spec sym spec))
          (`(tool . ,tool)
           (setq state (plist-put state :tools (append (plist-get state :tools) (list tool))))))))

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
      (setq-local macher-agent--active-ptc-primitives (plist-get payload :ptc-primitives)))))

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
             (spec (when primary-sym (alist-get primary-sym known)))
             (raw-sys (or (plist-get spec :system) (plist-get spec :system-message))))
        (when (stringp raw-sys)
          (setq payload (plist-put payload :system raw-sys)))))

    (macher-agent--apply-payload-locally payload)))

(defvar macher-agent--is-subagent nil
  "Flag whether the current buffer is managed as a sub-agent.

Non-nil indicates that the buffer is an isolated sub-agent buffer.

Return non-nil if current buffer is a sub-agent, otherwise nil.

Side effects: Buffer-local variable.")

(defvar macher-agent--ready-to-reap nil
  "Flag whether the sub-agent buffer is ready for garbage collection.

Non-nil indicates that the sub-agent task has completed and can be reaped.

Return non-nil if sub-agent buffer is ready to be reaped, otherwise nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent-presets nil
  "Store list of active preset or skill symbols for current buffer.

Holds the active preset and skill symbols configured for the current buffer.

Return list of active preset or skill symbols, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--parent-buffer nil
  "Store parent orchestrator buffer for current sub-agent.

Holds the buffer object of the parent agent that spawned this sub-agent.

Return parent buffer object, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--pending-children nil
  "Track child sub-agent buffer names pending completion.

Holds a list of string buffer names for child agents that have not finished.

Return list of child buffer names, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--child-results nil
  "Store accumulated results from completed child sub-agents.

Holds an association list mapping sub-agent names to their result payloads.

Return alist of child sub-agent results, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--resume-callback nil
  "Store callback function for resuming parent orchestrator.

Holds the function to invoke when all child sub-agent tasks complete.

Return resume callback function, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--active-ptc-primitives nil
  "Store active Programmatic Tool Calling primitives for buffer.

Holds list of symbols representing enabled PTC primitive operations.

Return list of PTC primitive symbols, or nil.

Side effects: Buffer-local variable.")

(defvar-local macher-agent--suppress-patch nil
  "Flag whether patch review generation is suppressed.

Non-nil suppresses virtual patch review generation on task completion.

Return non-nil if patch creation is suppressed, otherwise nil.

Side effects: Buffer-local variable.")

(put 'macher-agent--is-subagent 'permanent-local t)
(put 'macher-agent--ready-to-reap 'permanent-local t)
(put 'macher-agent-presets 'permanent-local t)
(put 'macher-agent--parent-buffer 'permanent-local t)
(put 'macher-agent--pending-children 'permanent-local t)
(put 'macher-agent--child-results 'permanent-local t)
(put 'macher-agent--resume-callback 'permanent-local t)
(put 'macher-agent--active-ptc-primitives 'permanent-local t)
(put 'macher-agent--suppress-patch 'permanent-local t)

(defun macher-agent--ptc-primitive-p (sym)
  "Check whether SYM is an active Programmatic Tool Calling primitive.

Matches both direct symbol equivalence and dynamic translation between
hyphens and underscores for SYM.

SYM is the symbol to test.

Return t if SYM is an active primitive, otherwise nil.

Side effects: None."
  (let* ((sym-name (symbol-name sym))
         (norm-sym (replace-regexp-in-string "_" "-" sym-name))

         (fsm-obj (or (bound-and-true-p macher-agent--active-fsm)
                      (bound-and-true-p macher--fsm-latest)
                      (bound-and-true-p gptel--fsm-last)))
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

(defun macher-agent--make-lisp-task-callback (buf callback)
  "Create a parent callback for Lisp task execution in BUF.

BUF is the Lisp task sub-agent buffer.
CALLBACK is the completion callback function.

Return a unary callback function taking RES.

Side effects: Returns a closure that marks BUF ready to reap on completion."
  (let ((fired nil))
    (lambda (res)
      (unless fired
        (setq fired t)
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq-local macher-agent--ready-to-reap t)))
        (funcall callback res)))))

(defun macher-agent-execute-lisp-task (payload callback)
  "Execute an asynchronous Lisp PAYLOAD in an isolated buffer.

PAYLOAD is a function taking a single callback argument, or an immediate value.
CALLBACK is the function to run with the final result.

Return nil.

Side effects: Spawns a temporary buffer and executes PAYLOAD."
  (let* ((parent-buf (current-buffer))
         (buf-name (generate-new-buffer-name " *macher-agent-lisp-task*"))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (setq-local macher-agent--is-subagent t)
      (setq-local macher-agent--parent-buffer parent-buf)
      (setq-local macher-agent--parent-callback
                  (macher-agent--make-lisp-task-callback buf callback))
      (condition-case err
          (if (functionp payload)
              (funcall payload macher-agent--parent-callback)
            (funcall macher-agent--parent-callback payload))
        (error
         (funcall macher-agent--parent-callback (format "LISP EXECUTION ERROR: %s" err)))))))

(defun macher-agent--extract-first-preset-symbol (resolved-presets)
  "Extract the normalised first preset symbol from RESOLVED-PRESETS.

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

(defun macher-agent--merge-subagent-parent-context (buf parent-buf)
  "Merge persistent context from child BUF into PARENT-BUF.

BUF is the sub-agent child buffer.
PARENT-BUF is the parent orchestrator buffer.

Return nil.

Side effects: Merges persistent context into PARENT-BUF."
  (when (and (buffer-live-p buf) (buffer-live-p parent-buf))
    (let ((child-ctx (with-current-buffer buf (bound-and-true-p macher-agent--persistent-context)))
          (parent-ctx (or (with-current-buffer parent-buf (bound-and-true-p macher-agent--persistent-context))
                          (ignore-errors (with-current-buffer parent-buf (macher-agent-resolve-context))))))
      (when (and child-ctx parent-ctx (not (eq child-ctx parent-ctx)))
        (macher-agent--merge-contexts parent-ctx child-ctx)))))

(defun macher-agent--make-subagent-parent-callback (buf parent-buf is-background callback)
  "Create a parent callback for BUF and PARENT-BUF.

BUF is the sub-agent buffer.
PARENT-BUF is the parent orchestrator buffer.
IS-BACKGROUND is non-nil if sub-agent task runs in background.
CALLBACK is the user-supplied task completion callback.

Return a unary callback function taking RES.

Side effects: Returns a closure that updates parent context and reaper state."
  (let ((fired nil))
    (lambda (res)
      (macher-agent--merge-subagent-parent-context buf parent-buf)
      (unless fired
        (setq fired t)
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (unless is-background
              (setq-local macher-agent--ready-to-reap t))))
        (funcall callback res)))))

(defun macher-agent-add-subagent (name &optional presets parent-buf dir context)
  "Interactively or programmatically create sub-agent buffer NAME.

NAME is the target sub-agent buffer name string.
PRESETS is optional preset specification, list, vector, or string.
PARENT-BUF is optional parent orchestrator buffer.
DIR is optional directory path string.
CONTEXT is optional VFS context structure.

Return the created sub-agent buffer object.

Side effects: Creates buffer, updates local state, registers in global tracking lists."
  (interactive "sSub-agent name: ")
  (let ((actual-presets presets)
        (actual-parent parent-buf)
        (actual-dir dir)
        (actual-ctx context))
    (when (and (stringp actual-presets)
               (or (file-directory-p actual-presets)
                   (string-prefix-p "/" actual-presets)
                   (string-suffix-p "/" actual-presets)))
      (unless actual-dir
        (setq actual-dir actual-presets))
      (if (and (listp actual-ctx) (not (and (fboundp 'macher-context-p) (macher-context-p actual-ctx))))
          (progn
            (setq actual-presets actual-ctx)
            (setq actual-ctx nil))
        (setq actual-presets nil)))

    (when (and (fboundp 'macher-context-p) (macher-context-p actual-presets))
      (setq actual-ctx actual-presets)
      (setq actual-presets nil))

    (when (and (fboundp 'macher-context-p) (macher-context-p actual-dir))
      (setq actual-ctx actual-dir)
      (setq actual-dir (if (and (stringp presets) (not (equal presets actual-ctx))) presets nil)))

    (when (and (fboundp 'macher-context-p) (macher-context-p actual-parent))
      (setq actual-ctx actual-parent)
      (setq actual-parent nil))

    (unless actual-parent
      (setq actual-parent (current-buffer)))

    (let* ((resolved-ctx (or actual-ctx
                             (when (buffer-live-p actual-parent)
                               (with-current-buffer actual-parent
                                 (bound-and-true-p macher-agent--persistent-context)))
                             (ignore-errors (macher-agent-resolve-context actual-dir))))
           (cloned-ctx (when resolved-ctx
                         (if (fboundp 'macher-agent--clone-context)
                             (macher-agent--clone-context resolved-ctx)
                           resolved-ctx)))
           (target-dir (or actual-dir
                           (when cloned-ctx (ignore-errors (macher-agent-context-root cloned-ctx)))
                           default-directory))
           (buf (get-buffer-create name)))

      (with-current-buffer buf
        (when (and (fboundp 'markdown-mode) (not (derived-mode-p 'markdown-mode)))
          (markdown-mode))
        (when (and (fboundp 'gptel-mode) (not gptel-mode))
          (gptel-mode 1))

        (setq-local macher-agent--is-subagent t)
        (setq-local macher-agent--is-workspace t)
        (setq-local gptel-stream nil)
        (setq-local macher-agent--parent-buffer actual-parent)
        (setq-local default-directory (file-name-as-directory target-dir))

        (when (buffer-live-p actual-parent)
          (setq-local gptel-model (buffer-local-value 'gptel-model actual-parent))
          (setq-local gptel-backend (buffer-local-value 'gptel-backend actual-parent))
          (setq-local gptel-temperature (buffer-local-value 'gptel-temperature actual-parent))
          (setq-local gptel-max-tokens (buffer-local-value 'gptel-max-tokens actual-parent)))

        (when cloned-ctx
          (setq-local macher-agent--persistent-context cloned-ctx))

        (when actual-presets
          (let ((preset-list (cond ((listp actual-presets) actual-presets)
                                   ((vectorp actual-presets) (append actual-presets nil))
                                   (t (list actual-presets)))))
            (setq-local macher-agent-presets preset-list)
            (macher-agent--apply-preset preset-list)))

        (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)
        (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t)
        (add-hook 'gptel-post-response-functions #'macher-agent-post-response-reaper nil t))

      (when (boundp 'macher-agent-active-subagents)
        (setq macher-agent-active-subagents
              (cons (cons name target-dir)
                    (cl-delete name macher-agent-active-subagents :key #'car :test #'equal))))

      (when-let* ((ws (when cloned-ctx (ignore-errors (macher-agent--get-context-workspace cloned-ctx)))))
        (let ((subs (ignore-errors (macher-agent-workspace-active-subagents ws))))
          (macher-agent--set-workspace-active-subagents
           ws
           (cons (cons name target-dir)
                 (cl-delete name subs :key #'car :test #'equal)))))

      buf)))

(defun macher-agent--prepare-subagent-instructions (subagent-buf instructions &optional presets)
  "Prepare instruction text and system message payload locally in SUBAGENT-BUF.

SUBAGENT-BUF is the target sub-agent buffer object or buffer name.
INSTRUCTIONS is the directive string to insert into the sub-agent buffer.
PRESETS is optional skill preset or list of presets to apply.

Return SUBAGENT-BUF.

Side effects: Applies presets, sets buffer text, and registers hooks in SUBAGENT-BUF."
  (when-let* ((buf (if (bufferp subagent-buf)
                       subagent-buf
                     (get-buffer subagent-buf))))
    (with-current-buffer buf
      (when presets
        (let ((preset-list (cond ((listp presets) presets)
                                 ((vectorp presets) (append presets nil))
                                 (t (list presets)))))
          (setq-local macher-agent-presets preset-list)
          (macher-agent--apply-preset preset-list)))
      (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)
      (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t)
      (when (and (stringp instructions) (not (string-empty-p instructions)))
        (erase-buffer)
        (insert instructions)))
    buf))

(defun macher-agent-spawn-task (task callback)
  "Spawn TASK inside a target sub-agent buffer.

TASK is the task specification list or buffer name.
CALLBACK is the completion callback function.

Return nil.

Side effects: Transmits task context to gptel and triggers CALLBACK on result."
  (if-let* ((buf-name (if (listp task) (plist-get task :buffer_name) task))
            (buf (get-buffer buf-name)))
      (let* ((parent-buf (current-buffer))
             (instructions (if (listp task) (plist-get task :instructions) ""))
             (presets
              (if (listp task) (or (plist-get task :presets) (plist-get task :preset)) nil))
             (is-background (and (listp task) (plist-get task :background)))
             (suppress-patch (and (listp task) (plist-get task :suppress_patch)))
             (parent-active-presets
              (with-current-buffer parent-buf (bound-and-true-p macher-agent-presets)))
             (target-active-presets
              (with-current-buffer buf (bound-and-true-p macher-agent-presets)))
             (resolved-presets (or presets target-active-presets)))
        (macher-agent--prepare-subagent-instructions buf instructions resolved-presets)
        (with-current-buffer buf
          (unless is-background
            (macher-agent--show-ui buf))

          (setq-local macher-agent--is-subagent t)
          (setq-local macher-agent--parent-buffer parent-buf)
          (setq-local macher-agent--suppress-patch suppress-patch)

          (add-hook
           'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)
          (add-hook 'gptel-pre-tool-call-functions #'macher-agent--enforce-tool-scope nil t)

          (setq-local macher-agent--parent-callback
                      (macher-agent--make-subagent-parent-callback
                       buf parent-buf is-background callback))

          (let* ((clean-sym (macher-agent--extract-first-preset-symbol resolved-presets))
                 (task-ctx (make-macher-agent-task-context
                            :workspace nil
                            :target-buffer buf
                            :skill-sym clean-sym
                            :system-message (bound-and-true-p gptel-system-prompt))))

            (macher-agent-gptel-transmit
             task-ctx
             (list
              :on-success
              (lambda (res)
                (funcall
                 macher-agent--parent-callback
                 (make-macher-agent-delegate-response
                  :status 'success :data res :buffer-name buf-name)))
              :on-error
              (lambda (err)
                (funcall
                 macher-agent--parent-callback
                 (make-macher-agent-delegate-response
                  :status 'error :error (format "ERROR: %s" err) :buffer-name buf-name))))))))
    (let
        ((buf-name (if (listp task) (plist-get task :buffer_name) task)))
      (funcall
       callback
       (make-macher-agent-delegate-response
        :status 'error
        :error
        (format "ERROR: Sub-agent buffer '%s' not found." buf-name) :buffer-name buf-name)))))

(defun macher-agent--apply-single-virtual-buffer (entry)
  "Apply a single VFS virtual edit ENTRY to a live Emacs buffer.

ENTRY is a VFS context entry structure or cell.

Return non-nil if applied to a live buffer, otherwise nil.

Side effects: Modifies the target live buffer contents."
  (when entry
    (let* ((path (ignore-errors (macher-agent-vfs-entry-path entry)))
           (curr (ignore-errors (macher-agent-vfs-entry-curr entry)))
           (buf-name (when path (macher-agent--resolve-buffer-name path)))
           (buf (when buf-name (get-buffer buf-name))))
      (when (and buf (buffer-live-p buf) (stringp curr))
        (with-current-buffer buf
          (erase-buffer)
          (insert curr))
        t))))

(defun macher-agent-apply-virtual-buffers (&optional context)
  "Apply pending VFS virtual edits to live Emacs buffers.

CONTEXT is the optional VFS context structure. If omitted, resolves
the active context.

Return nil.

Side effects: Modifies contents of live buffers matching VFS entries."
  (let* ((ctx (or context (ignore-errors (macher-agent-resolve-context))))
         (contents (when ctx (ignore-errors (macher-agent--get-context-contents ctx)))))
    (dolist (entry contents)
      (macher-agent--apply-single-virtual-buffer entry))
    (when (fboundp 'macher-agent--auto-sync-context)
      (macher-agent--auto-sync-context ctx))))

(provide 'macher-agent-orchestration)
;;; macher-agent-orchestration.el ends here
