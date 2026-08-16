;;; macher-agent-tools.el --- Pure gptel orchestration tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure gptel orchestration tools for Macher Agent.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'map)
(require 'transient)
(require 'subr-x)
(require 'macher-agent-core)
(require 'macher-agent-sandbox)
(require 'macher-agent-vfs)
(require 'macher-agent-presets)
(require 'macher-agent-orchestration)
(require 'macher-agent-zero-mem)

;;; Customisation Variables

(defvar macher-agent-ptc-raw-mode nil
  "Toggle raw Lisp data return for Programmatic Tool Calling execution.

If non-nil, tools bypass string formatting and return raw Lisp data.

Return non-nil when raw mode is active, nil otherwise.
Side effects: None.")

(defvar macher-agent--active-ptc-execution nil
  "Indicate whether a Programmatic Tool Calling Lisp script is active.

If non-nil, evaluation of a Programmatic Tool Calling Lisp script
is in progress.

Return non-nil when active, nil otherwise.
Side effects: None.")

(defvar macher-agent-allowed-tools nil
  "Hold custom tool names that receive the active Macher context.

List of custom tool name symbols permitted to access context.

Return list of allowed tool symbol names.
Side effects: None.")

(put 'macher-agent--pending-instructions-queue 'permanent-local t)

(defvar-local macher-agent--final-result nil
  "Store the synthesised final answer from the sub-agent.

Return final result string or object.
Side effects: None.")

(defvar macher-agent--pending-tool-media-alist nil
  "Maintain mapping of buffer objects to pending media items.

Return alist mapping buffers to pending media files or data.
Side effects: None.")

;;; Hooks

(defvar macher-agent-pre-tool-use-hook nil
  "Provide hook run before a tool executes.

Called with parameters (TOOL-NAME-SYM ARGUMENTS-PLIST).

Return hook run function list.
Side effects: None.")

(defvar macher-agent-permission-request-hook nil
  "Provide hook run for interactive approval before tool execution.

Called with parameters (TOOL-NAME-SYM ARGUMENTS-PLIST).

Return hook run function list.
Side effects: None.")

(defvar macher-agent-post-tool-use-hook nil
  "Provide hook run after a tool completes successfully.

Called with parameters (TOOL-NAME-SYM ARGUMENTS-PLIST OUTPUT).

Return hook run function list.
Side effects: None.")

(defvar macher-agent-post-tool-use-failure-hook nil
  "Provide hook run if a tool execution fails.

Called with parameters (TOOL-NAME-SYM ARGUMENTS-PLIST ERROR-DATA).

Return hook run function list.
Side effects: None.")

;;; Execution Helpers

(defun macher-agent--get-active-context ()
  "Extract the active VFS context from the currently executing or latest FSM.
Returns the `macher-context` object or nil if unavailable."
  (when-let* ((active-fsm (or (bound-and-true-p gptel--fsm)
                              (bound-and-true-p macher-agent--active-fsm)
                              (bound-and-true-p gptel--fsm-last))))
    (macher-agent--resolve-context active-fsm)))

(defun macher-agent--wrap-callback (gptel-cb &optional ptc-exec)
  "Create a pure callback wrapper that processes results.

GPTEL-CB is the original gptel callback function, which may be nil.
PTC-EXEC is an optional boolean indicating active PTC execution context.

Return a unary callback function accepting a plist result structure.
Side effects: None."
  (let ((is-ptc (or ptc-exec (bound-and-true-p macher-agent--active-ptc-execution))))
    (lambda (plist-result)
      (let ((final-str (if (eq (plist-get plist-result :status) 'success)
                           (plist-get plist-result :data)
                         (plist-get plist-result :error))))
        (when (eq (plist-get plist-result :status) 'error)
          (message "MACHER AGENT ERROR: %S" final-str))
        (let
            ((macher-agent--active-ptc-execution
              (or is-ptc (bound-and-true-p macher-agent--active-ptc-execution))))
          (if gptel-cb
              (funcall gptel-cb final-str)
            final-str))))))

(defun macher-agent--extract-prop (obj key)
  "Extract KEY property from OBJ structure.

OBJ can be a plist, alist, or hash-table.
KEY is the property key symbol or string to extract.

Return the property value if found, or symbol `macher-missing' if missing.
Side effects: None."
  (let ((norm-key (if (stringp key) (intern (concat ":" (replace-regexp-in-string "_" "-" key))) key)))
    (map-elt obj norm-key 'macher-missing)))

;;; Schema Validation

(defun macher-agent--validate-primitive-schema (type value name)
  "Validate primitive VALUE against expected TYPE for parameter NAME.

TYPE is the expected JSON primitive type symbol or string.
VALUE is the actual parameter value to check.
NAME is the parameter name string for error reporting.

Return t if validation succeeds.
Side effects: Signals error if value does not match expected primitive type."
  (let ((is-valid
         (pcase type
           ((or 'string "string")   (stringp value))
           ((or 'number "number")   (numberp value))
           ((or 'integer "integer") (integerp value))
           ((or 'boolean "boolean") (memq value '(t :json-false)))
           ((or 'array "array")      (vectorp value))
           ((or 'object "object")   (or (listp value) (hash-table-p value)))
           (_ (error "%s" (format "Unknown schema type: %S" type))))))
    (unless is-valid
      (error "%s" (format "The '%s' parameter must be an %s, not %s"
                          name type (type-of value))))))

(defun macher-agent--validate-array-schema (spec value name)
  "Validate array VALUE against item SPEC for parameter NAME.

SPEC is the array schema specification plist.
VALUE is the array vector or list to validate.
NAME is the parameter path string for error messages.

Return nil upon successful validation.
Side effects: Signals error if any element fails schema validation."
  (let ((items-spec (plist-get spec :items)))
    (when (and (consp items-spec) (eq (car items-spec) 'quote))
      (setq items-spec (cadr items-spec)))
    (when items-spec
      (cl-loop for item in (append value nil)
               for idx from 0
               do (macher-agent--validate-schema
                   items-spec item (format "%s[%d]" name idx))))))

(defun macher-agent--validate-object-schema (spec value name)
  "Validate object VALUE against property SPEC for parameter NAME.

SPEC is the object schema specification plist.
VALUE is the object value list or hash-table.
NAME is the parameter path string for error messages.

Return nil upon successful validation.
Side effects: Signals error if missing required properties or invalid types."
  (unless (or (hash-table-p value) (listp value))
    (error "Validation error (%s): Expected object structure, got %S" name value))
  (let ((props (plist-get spec :properties))
        (reqs (plist-get spec :required)))
    (when (and (consp props) (eq (car props) 'quote)) (setq props (cadr props)))
    (when (and (consp reqs) (eq (car reqs) 'quote)) (setq reqs (cadr reqs)))
    (when (vectorp reqs) (setq reqs (append reqs nil)))
    (when props
      (unless (and (listp props) (evenp (length props)))
        (error "%s" (format
                     "Schema properties for '%s' must be a flat plist, got %S" name props)))
      (cl-loop
       for (k v-spec) on props by #'cddr
       for prop-name = (if (keywordp k) (substring (symbol-name k) 1) (format "%s" k))
       for child-val = (macher-agent--extract-prop value k)
       do (if (not (eq child-val 'macher-missing))
              (macher-agent--validate-schema v-spec child-val (format "%s.%s" name prop-name))
            (when (or (and reqs
                           (or (member prop-name reqs)
                               (member (intern prop-name) reqs)
                               (member (intern (concat ":" prop-name)) reqs)))
                      (and (not reqs)
                           (plist-member v-spec :optional)
                           (not (plist-get v-spec :optional))))
              (error "%s" (format "Missing required parameter: '%s.%s'" name prop-name))))))))

(defun macher-agent--validate-schema (spec value &optional path)
  "Recursively validate VALUE against JSON SPEC with optional PATH.

SPEC is the target JSON schema specification plist.
VALUE is the input data value to validate.
PATH is an optional string tracking property hierarchy for errors.

Return nil upon successful validation.
Side effects: Signals error if schema validation fails."
  (when (and (consp spec) (eq (car spec) 'quote))
    (setq spec (cadr spec)))
  (let* ((type (plist-get spec :type))
         (name (or path (plist-get spec :name) "value")))
    (when (and (consp type) (eq (car type) 'quote))
      (setq type (cadr type)))
    (cond
     ((null value)
      (unless (plist-get spec :optional)
        (error "%s" (format "Missing required parameter: '%s'" name))))
     ((eq value :null)
      (unless (eq type 'null)
        (error "%s" (format "The '%s' parameter cannot be null." name))))
     (t
      (macher-agent--validate-primitive-schema type value name)
      (pcase type
        ((or 'array "array")
         (macher-agent--validate-array-schema spec value name))
        ((or 'object "object")
         (macher-agent--validate-object-schema spec value name)))))))

(defun macher-agent--extract-payload (tool-args args-spec)
  "Normalise TOOL-ARGS against ARGS-SPEC into a flat keyword plist.

TOOL-ARGS is the list of supplied tool arguments.
ARGS-SPEC is the expected schema specification list.

Return flat keyword plist of argument values.
Side effects: None."
  (cond
   ((and tool-args (keywordp (car tool-args)))
    tool-args)
   ((and tool-args (listp (car tool-args)) (keywordp (caar tool-args)))
    (car tool-args))
   (t
    (cl-loop for arg in tool-args
             for spec in args-spec
             for arg-name = (plist-get spec :name)
             for key = (intern (concat ":" (if (symbolp arg-name)
                                               (symbol-name arg-name)
                                             arg-name)))
             append (list key arg)))))

(defun macher-agent--validate-payload (payload args-spec)
  "Validate PAYLOAD against ARGS-SPEC using internal schema validator.

PAYLOAD is the argument payload plist to validate.
ARGS-SPEC is the expected argument schema list.

Return nil upon successful validation.
Side effects: Signals error if payload validation fails."
  (cl-loop for spec in args-spec
           for arg-name = (plist-get spec :name)
           for key = (intern (concat ":" (if (symbolp arg-name)
                                             (symbol-name arg-name)
                                           arg-name)))
           for arg-val = (plist-get payload key)
           do (macher-agent--validate-schema spec arg-val)))

(defun macher-agent--run-pre-hooks (name-sym payload)
  "Execute pre-tool and permission hooks for NAME-SYM with PAYLOAD.

NAME-SYM is the tool symbol name.
PAYLOAD is the argument plist payload.

Return error string if blocked or errored, nil if execution is permitted.
Side effects: Runs hooks in `macher-agent-pre-tool-use-hook' and
permission hook."
  (let ((pre-blocked nil)
        (pre-error nil))
    (condition-case hook-err
        (cond
         ((and macher-agent-pre-tool-use-hook
               (not (run-hook-with-args-until-failure 'macher-agent-pre-tool-use-hook name-sym payload)))
          (setq pre-blocked "Execution blocked by macher-agent-pre-tool-use-hook"))
         ((and macher-agent-permission-request-hook
               (not (run-hook-with-args-until-failure 'macher-agent-permission-request-hook name-sym payload)))
          (setq pre-blocked "Permission denied by macher-agent-permission-request-hook")))
      (error
       (setq pre-error (format "Execution blocked by error in macher-agent-pre-tool-use-hook: %s"
                               (error-message-string hook-err)))))
    (or pre-blocked pre-error)))

;;; Tool Construction Macro

(defmacro macher-agent--with-tool-error-handling (name-sym payload wrap-cb &rest body)
  "Wrap BODY in error handling for NAME-SYM with PAYLOAD using WRAP-CB.

NAME-SYM is the tool symbol name.
PAYLOAD is the tool arguments plist.
WRAP-CB is the callback function wrapper.
BODY is the expression sequence to execute.

Return result of evaluating BODY or error callback invocation.
Side effects: Invokes failure hook and callback on error."
  (declare (indent 3))
  `(condition-case err
       (progn ,@body)
     (error
      (run-hook-with-args 'macher-agent-post-tool-use-failure-hook ,name-sym ,payload err)
      (funcall ,wrap-cb (list :status 'error :error (error-message-string err))))))

(defmacro macher-agent--build-success-callback (name-sym payload success-fn output-filter-fn wrap-cb &optional is-ptc)
  "Generate success callback for NAME-SYM using PAYLOAD and handlers.

NAME-SYM is the tool symbol name.
PAYLOAD is the argument plist payload.
SUCCESS-FN is the optional result formatting function.
OUTPUT-FILTER-FN is the optional output filtering function.
WRAP-CB is the callback function wrapper.
IS-PTC is an optional boolean indicating active PTC execution context.

Return a lambda callback function accepting response object.
Side effects: Runs `macher-agent-post-tool-use-hook' on success or
failure hook on error."
  `(lambda (res-obj)
     (condition-case cb-err
         (let*
             ((s-fn ,success-fn)
              (f-fn ,output-filter-fn)
              (raw-payload res-obj)
              (active-ptc (or ,is-ptc (bound-and-true-p macher-agent--active-ptc-execution)))
              (success-data
               (if (and s-fn (not active-ptc))
                   (let ((arity (func-arity s-fn)))
                     (if (or (eq (cdr arity) 'many)
                             (and (numberp (cdr arity)) (>= (cdr arity) 2)))
                         (funcall s-fn raw-payload ,payload)
                       (funcall s-fn raw-payload)))
                 raw-payload))
              (final-data (if f-fn (funcall f-fn success-data) success-data)))
           (run-hook-with-args 'macher-agent-post-tool-use-hook ,name-sym ,payload final-data)
           (funcall ,wrap-cb (list :status 'success :data final-data)))
       (error
        (run-hook-with-args 'macher-agent-post-tool-use-failure-hook ,name-sym ,payload cb-err)
        (funcall ,wrap-cb (list :status 'error :error (error-message-string cb-err)))))))

(cl-defmacro macher-agent-make-tool (name-symbol description &key category args command-fn success-fn output-filter-fn)
  "Define NAME-SYMBOL as a tool compatible with gptel using DESCRIPTION.

NAME-SYMBOL is the symbol naming the tool variable.
DESCRIPTION is the string description of tool functionality.
CATEGORY is the optional category string, defaulting to macher-agent.
ARGS is the argument schema specification list.
COMMAND-FN is the function executing core tool logic.
SUCCESS-FN is the optional callback formatting successful output.
OUTPUT-FILTER-FN is the optional filter function for final result data.

Return expanding form defining and initialising the tool variable.
Side effects: Sets `NAME-SYMBOL' variable to constructed gptel tool object."
  (declare (indent 2))
  (let* ((stripped-name (replace-regexp-in-string "^macher-agent-tool-\\|^macher-agent-\\|-tool$" "" (symbol-name name-symbol)))
         (name (replace-regexp-in-string "-" "_" stripped-name)))
    `(progn
       (defvar ,name-symbol nil)
       (put ',name-symbol 'command-fn ,command-fn)
       (setq ,name-symbol
             (macher-agent-bridge-register-tool
              ,name
              ,description
              ,(or category "macher-agent")
              ,args
              (lambda (callback &rest tool-args)
                (let*
                    ((ptc-exec (bound-and-true-p macher-agent--active-ptc-execution))
                     (wrap-cb (macher-agent--wrap-callback callback ptc-exec))
                     (payload (macher-agent--extract-payload tool-args ,args)))
                  (macher-agent--with-tool-error-handling
                      ',name-symbol payload wrap-cb
                    (macher-agent--validate-payload payload ,args)
                    (let*
                        ((context (macher-agent--get-active-context))
                         (root
                          (if context (macher-agent-context-root context) default-directory))
                         (hook-rejection (macher-agent--run-pre-hooks ',name-symbol payload)))
                      (if hook-rejection
                          (funcall wrap-cb (list :status 'error :error hook-rejection))
                        (let*
                            ((on-success
                              (macher-agent--build-success-callback
                               ',name-symbol payload ,success-fn
                               ,output-filter-fn wrap-cb ptc-exec))
                             (cmd-fn ,command-fn)
                             (arity (func-arity cmd-fn)))
                          (if (or (eq (cdr arity) 'many)
                                  (and (numberp (cdr arity)) (>= (cdr arity) 4)))
                              (funcall cmd-fn payload context root on-success)
                            (let ((action-res (funcall cmd-fn payload context root)))
                              (if (functionp action-res)
                                  (funcall action-res on-success)
                                (funcall on-success action-res)))))))))))))))

(defun macher-agent-execute-ptc-script
    (script-string context on-success on-error &optional extra-primitives target-buf)
  "Execute Programmatic Tool Calling SCRIPT-STRING using CONTEXT.

SCRIPT-STRING is the string containing Lisp code to evaluate.
CONTEXT is the active VFS context structure.
ON-SUCCESS is the callback function invoked with success result.
ON-ERROR is the callback function invoked with error result.
EXTRA-PRIMITIVES is an optional list of allowed primitive functions.
TARGET-BUF is an optional target buffer for execution.

Return nil.
Side effects: Evaluates Lisp code in a sandboxed environment."
  (let ((prims (or extra-primitives
                   '(nreverse sort delete delq nconc plist-put aset puthash remhash error signal message random emacs-version)))
        (buf (or target-buf (current-buffer))))
    (let ((macher-agent--active-ptc-execution t))
      (condition-case err
          (progn
            (with-current-buffer buf
              (setq macher-agent-sandbox--globals (make-hash-table :test 'eq)
                    macher-agent-sandbox--primitives (make-hash-table :test 'eq)
                    macher-agent-sandbox--functions (make-hash-table :test 'eq))
              (macher-agent-sandbox--init prims))
            (let* ((ast (macroexpand-all (read script-string)))
                   (iterator (with-current-buffer buf (macher-agent-sandbox--eval-iter ast nil)))
                   (stop-iter-fn (lambda (reason)
                                   (funcall on-error (list :status 'error :error reason)))))
              (letrec
                  ((ptc-resume-loop
                    (lambda (input)
                      (condition-case iter-err
                          (let ((step (with-current-buffer buf (iter-next iterator input))))
                            (if (and (listp step) (eq (plist-get step :interrupt) 'tool-call))
                                (macher-agent--ptc-handle-yielded-value
                                 step context ptc-resume-loop on-error stop-iter-fn)
                              (funcall ptc-resume-loop step)))
                        (iter-end-of-sequence
                         (funcall on-success (cdr iter-err)))
                        (error
                         (funcall on-error (list :status 'error :error (error-message-string iter-err))))))))
                (funcall ptc-resume-loop nil))))
        (error
         (funcall on-error (list :status 'error :error (error-message-string err))))))))

;;; PTC Integration

(defun macher-agent--display-ptc-tool-execution (tool-sym args context)
  "Display mode-line status and run pre-execution hooks for PTC tool.

TOOL-SYM is the symbol name of the target PTC tool.
ARGS is the list of arguments supplied to the tool call.
CONTEXT is the active agent context structure.

Return cons cell of (ORIGINAL-NAME-STRING . RESOLVED-TOOL).
Side effects: Updates mode-line status and executes pre-tool-call hooks."
  (let* ((target-lisp-name (symbol-name tool-sym))
         (original-name-str (replace-regexp-in-string "-" "_" target-lisp-name))
         (resolved-tool (macher-agent-resolve-tool original-name-str nil nil context))
         (desc (if (macher-tool-valid-p resolved-tool)
                   (gptel-tool-description resolved-tool)
                 original-name-str))
         (block-reason nil))

    (when (bound-and-true-p gptel-pre-tool-call-functions)
      (run-hook-wrapped
       'gptel-pre-tool-call-functions
       (lambda (f)
         (let ((res (ignore-errors
                      (funcall f (list :name original-name-str :args args
                                       :buffer (buffer-name) :model (bound-and-true-p gptel-model))))))
           (when (and res (listp res) (plist-get res :block))
             (setq block-reason (plist-get res :block))
             t)))))

    (if block-reason
        (error "%s" block-reason)
      (message "PTC Executing: %s"
               (or desc original-name-str))
      (when (fboundp 'gptel--update-status)
        (gptel--update-status
         (concat
          (propertize " Calling PTC tool (" 'face 'mode-line-emphasis)
          (propertize (format "%s" tool-sym) 'face 'font-lock-keyword-face)
          (propertize ")" 'face 'mode-line-emphasis))))
      (cons original-name-str resolved-tool))))

(defun macher-agent--format-ptc-result-block (truncated-call call-str str-res)
  "Format PTC result block string for display in buffer.

TRUNCATED-CALL is the truncated call header string.
CALL-STR is the full call string representation.
STR-RES is the formatted string response payload.

Return formatted result block string matching mode syntax.
Side effects: None."
  (if (derived-mode-p 'org-mode)
      (format "\n#+begin_tool %s\n%s\n\n%s\n#+end_tool\n"
              truncated-call call-str str-res)
    (format "\n``` tool %s\n%s\n\n%s\n```\n"
            truncated-call call-str str-res)))

(defun macher-agent--cycle-ptc-result-block ()
  "Cycle visibility of the PTC result block at point.

Return result of cycling command evaluation or nil on error.
Side effects: Toggles block visibility in Org mode or Markdown mode."
  (ignore-errors
    (if (derived-mode-p 'org-mode)
        (org-cycle)
      (when (fboundp 'gptel-markdown-cycle-block)
        (gptel-markdown-cycle-block)))))

(defun macher-agent--insert-ptc-tool-result (original-name-str args res-data)
  "Insert collapsible tool result block into current buffer.

ORIGINAL-NAME-STR is the original tool name string.
ARGS is the list of arguments passed to tool.
RES-DATA is the response data payload or string.

Return nil.
Side effects: Inserts formatted result block text into buffer at buffer max."
  (when-let* ((str-res (if (stringp res-data) res-data (prin1-to-string res-data)))
              (call-str (prin1-to-string `(:name ,original-name-str :args ,args))))
    (when (bound-and-true-p gptel-post-tool-call-functions)
      (run-hook-wrapped
       'gptel-post-tool-call-functions
       (lambda (f)
         (ignore-errors
           (funcall f (list :name original-name-str :args args :result str-res
                            :buffer (buffer-name) :model (bound-and-true-p gptel-model)))))))
    (when (bound-and-true-p gptel-include-tool-results)
      (save-excursion
        (goto-char (point-max))
        (let* ((inhibit-read-only t)
               (arg-str (string-trim (prin1-to-string args) "(" ")"))
               (raw-call (format "(%s %s)" original-name-str arg-str))
               (truncated-call (truncate-string-to-width raw-call 60 nil nil " ...)"))
               (start-pt (point)))
          (insert (macher-agent--format-ptc-result-block truncated-call call-str str-res))
          (goto-char start-pt)
          (forward-line 1)
          (macher-agent--cycle-ptc-result-block))))))

(defun macher-agent--coerce-val (val spec)
  "Coerce VAL to match expected schema type in SPEC.

VAL is the input value to coerce.
SPEC is the JSON schema property specification plist.

Return coerced value matching expected type structure.
Side effects: None."
  (if (null spec) val
    (let ((type (plist-get spec :type)))
      (cond
       ((and (member type '(string "string")) (bufferp val))
        (buffer-name val))
       ((and (member type '(array "array")) (listp val))
        (vconcat (cl-loop for item in val
                          collect (macher-agent--coerce-val item (plist-get spec :items)))))
       ((and (member type '(object "object")) (listp val))
        (let* ((normalized-val
                (if (and val (consp (car val)))
                    (cl-loop for cell in val append (list (car cell) (cdr cell)))
                  val))
               (props (plist-get spec :properties)))
          (if props
              (cl-loop for (k v) on normalized-val by #'cddr
                       for p-name = (if (keywordp k) (substring (symbol-name k) 1) (format "%s" k))
                       for p-spec = (plist-get props (intern (concat ":" p-name)))
                       append (list k (macher-agent--coerce-val v p-spec)))
            normalized-val)))
       (t val)))))

(defun macher-agent--coerce-plist-args (args schema)
  "Coerce plist ARGS using target SCHEMA list.

ARGS is the plist of arguments to coerce.
SCHEMA is the argument schema specification list.

Return coerced plist of arguments.
Side effects: None."
  (let ((coerced-args nil))
    (cl-loop
     for (k v) on args by #'cddr
     for spec =
     (cl-find-if (lambda (s)
                   (let ((n (plist-get s :name)))
                     (string= (replace-regexp-in-string "_" "-" (if (symbolp n) (symbol-name n) n))
                              (substring (symbol-name k) 1))))
                 schema)
     do (progn (push k coerced-args)
               (push (if spec (macher-agent--coerce-val v spec) v) coerced-args)))
    (nreverse coerced-args)))

(defun macher-agent--coerce-positional-args (args schema)
  "Coerce positional ARGS using target SCHEMA list.

ARGS is the positional argument list to coerce.
SCHEMA is the argument schema specification list.

Return coerced list of positional arguments.
Side effects: None."
  (let ((coerced-args nil))
    (cl-loop for arg in args
             for spec in schema
             do (push (macher-agent--coerce-val arg spec) coerced-args))
    (nreverse coerced-args)))

(defun macher-agent--coerce-ptc-args (args tool)
  "Coerce evaluated Lisp ARGS to strictly match TOOL schema types.

ARGS is the evaluated argument list or plist.
TOOL is the resolved gptel tool structure.

Return coerced arguments list matching tool schema types.
Side effects: None."
  (if (not (macher-tool-valid-p tool))
      args
    (let ((schema (gptel-tool-args tool)))
      (if (and args (keywordp (car args)))
          (macher-agent--coerce-plist-args args schema)
        (macher-agent--coerce-positional-args args schema)))))

(defun macher-agent--dispatch-ptc-primitive
    (tool-lisp-sym args resolved-tool original-name-str ptc-callback on-error stop-iter-fn)
  "Dispatch TOOL-LISP-SYM with ARGS or RESOLVED-TOOL for ORIGINAL-NAME-STR.

TOOL-LISP-SYM is the symbol of the tool function.
ARGS is the list of arguments passed to the tool.
RESOLVED-TOOL is the resolved gptel tool structure.
ORIGINAL-NAME-STR is the string name of the original tool.
PTC-CALLBACK is the callback function upon successful execution.
ON-ERROR is the callback function invoked upon error.
STOP-ITER-FN is the function to halt the iterator.

Return result of dispatching tool execution.
Side effects: Invokes tool function and callbacks."
  (if (fboundp tool-lisp-sym)
      (let ((lisp-res (apply tool-lisp-sym args)))
        (funcall ptc-callback lisp-res))
    (if-let* ((_ (macher-tool-valid-p resolved-tool))
              (tool-fn (gptel-tool-function resolved-tool)))
        (let ((coerced-args (macher-agent--coerce-ptc-args args resolved-tool)))
          (if (gptel-tool-async resolved-tool)
              (let ((called nil))
                (apply tool-fn
                       (lambda (res &optional err)
                         (unless called
                           (setq called t)
                           (if err
                               (funcall stop-iter-fn err)
                             (funcall ptc-callback res))))
                       coerced-args))
            (let ((sync-result (apply tool-fn coerced-args)))
              (funcall ptc-callback sync-result))))
      (when stop-iter-fn (funcall stop-iter-fn "Tool not accessible"))
      (funcall on-error
               (list :status 'error
                     :error (format "ERROR: Tool '%s' is not accessible." original-name-str))))))

(defun macher-agent--ptc-handle-yielded-value (yielded-val context ptc-resume-loop on-error stop-iter-fn)
  "Handle YIELDED-VAL returned during PTC execution.

YIELDED-VAL is the value yielded from sandbox evaluation iterator.
CONTEXT is the active agent context structure.
PTC-RESUME-LOOP is the loop continuation callback function.
ON-ERROR is the error callback function on failure.
STOP-ITER-FN is the callback function halting iterator.

Return result of dispatching yielded value handling.
Side effects: Triggers tool execution and schedules loop resumption."
  (if (and (consp yielded-val)
           (eq (plist-get yielded-val :interrupt) 'tool-call))
      (let* ((tool-lisp-sym (plist-get yielded-val :name))
             (args (plist-get yielded-val :args))
             (tool-info (macher-agent--display-ptc-tool-execution tool-lisp-sym args context))
             (original-name-str (car tool-info))
             (resolved-tool (cdr tool-info))
             (ptc-callback (lambda (res-data)
                             (macher-agent--insert-ptc-tool-result original-name-str args res-data)
                             (funcall ptc-resume-loop res-data))))
        (macher-agent--dispatch-ptc-primitive
         tool-lisp-sym args resolved-tool original-name-str
         ptc-callback on-error stop-iter-fn))
    (when stop-iter-fn (funcall stop-iter-fn))
    (funcall on-error (list :status 'error
                            :error (format "Unexpected yield from PTC sandbox: %S" yielded-val)))))

;;; Sandbox Execution

(defun macher-agent--run-in-persistent-sandbox (context command on-success on-error)
  "Execute COMMAND asynchronously within dynamically generated VFS sandbox.

CONTEXT is the active agent context structure.
COMMAND is the shell command string to execute.
ON-SUCCESS is the success callback function.
ON-ERROR is the error callback function.

Return nil.
Side effects: Creates temporary sandbox directory and spawns process."
  (let* ((workspace-root (if context (macher-agent-context-root context) default-directory))
         (sandbox-dir (make-temp-file "macher-sandbox-" t))
         (contents (when context (macher-agent--get-context-contents context))))
    (condition-case err
        (progn
          (macher-agent--vfs-verify-clean-merge workspace-root contents)
          (macher-agent--vfs-sync-baseline workspace-root sandbox-dir)
          (when contents
            (macher-agent--vfs-apply-overlay-stateless contents workspace-root sandbox-dir))

          (let* ((out-buf (generate-new-buffer " *macher-sandbox-out*"))
                 (default-directory (file-name-as-directory sandbox-dir)))
            (make-process
             :name "macher-sandbox-process"
             :buffer out-buf
             :command (list shell-file-name shell-command-switch command)
             :sentinel
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (let ((output (with-current-buffer out-buf (buffer-string)))
                       (exit-code (process-exit-status proc)))

                   (kill-buffer out-buf)
                   (ignore-errors (delete-directory sandbox-dir t))

                   (if (= exit-code 0)
                       (funcall on-success output)
                     (funcall on-error (list :status 'error :error output)))))))))
      (error
       (ignore-errors (delete-directory sandbox-dir t))
       (funcall on-error (list :status 'error :error (error-message-string err)))))))

(defun macher-agent--read-file-vfs-aware (file-path context)
  "Read FILE-PATH prioritising uncommitted VFS memory over physical disk.

FILE-PATH is the file path string to read.
CONTEXT is the active agent context structure.

Return file content string, or nil if file cannot be read.
Side effects: None."
  (let* ((vfs-entry
          (when context (cl-find file-path (macher-agent--get-context-contents context) :key #'car :test #'equal)))
         (vfs-content (when vfs-entry (if (consp (cdr vfs-entry)) (cddr vfs-entry) (cdr vfs-entry)))))
    (cond
     (vfs-content vfs-content)
     ((file-exists-p file-path)
      (with-temp-buffer (insert-file-contents file-path) (buffer-string)))
     (t nil))))

(defun macher-agent-add-pending-instruction (instruction)
  "Push INSTRUCTION to the end of the queue for the next network turn.

INSTRUCTION is the directive string to append to the queue.

Return new list of pending instructions.

Side effects: Updates `macher-agent--pending-instructions-queue' buffer-locally."
  (setq-local macher-agent--pending-instructions-queue
              (append macher-agent--pending-instructions-queue
                      (list (format "USER OVERRIDE DIRECTIVE:\n%s" instruction)))))

;;; Media Integration

(defvar gptel-track-media)
(defvar gptel-context)
(declare-function gptel-add-file "gptel")
(declare-function gptel--parse-buffer "gptel")
(declare-function gptel-context-remove "gptel-context" (file))

(defun macher-agent--gptel-base64-encode-advice (orig-fun file)
  "Read FILE from VFS if available before base64 encoding.

ORIG-FUN is the original `gptel--base64-encode' function.
FILE is the string path of file or raw base64 data.

Return base64-encoded string representation.
Side effects: None."
  (if-let* ((fsm (or (bound-and-true-p macher-agent--active-fsm)
                     (macher-agent--get-fsm-latest)))
            (info (ignore-errors (gptel-fsm-info fsm)))
            (ctx (plist-get info :macher-agent-context))
            (pending (when ctx (macher-agent--get-context-data ctx :pending-media)))
            ((cl-some (lambda (item) (string= file (car item))) pending)))
      file
    (if-let* ((ctx (ignore-errors (macher-agent-current-context)))
              (workspace (macher-agent--get-context-workspace ctx))
              (workspace-root (macher-agent--get-workspace-root workspace))
              (actual-name (if (file-name-absolute-p file)
                               (file-relative-name file workspace-root)
                             file))
              (content (ignore-errors (macher-agent--read-context-file ctx actual-name))))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert content)
          (base64-encode-region (point-min) (point-max) :no-line-break)
          (buffer-string))
      (funcall orig-fun file))))

;;; Memory Tools

(defcustom macher-agent-search-backend 'zero-mem
  "Backend search strategy for conversation history search.
Options are `glob' (regex search) and `zero-mem' (PageRank vector search)."
  :type '(choice (const :tag "Regex Glob" glob)
                 (const :tag "Zero-Mem Fixed-Point PageRank" zero-mem))
  :group 'macher-agent)

(defun macher-agent-search-glob (query orig-buf &optional ctx-lines)
  "Search for QUERY in ORIG-BUF matching CTX-LINES."
  (let ((ctx-lines (if (and ctx-lines (> ctx-lines 0)) ctx-lines 5))
        (results nil)
        (invalid-re nil))
    (if (not (buffer-live-p orig-buf))
        "Error: Cannot locate original conversation buffer."
      (with-current-buffer orig-buf
        (save-excursion
          (goto-char (point-min))
          (condition-case err
              (let ((continue t))
                (while (and continue (re-search-forward query nil t))
                  (let* ((match-beg (match-beginning 0))
                         (match-end (match-end 0))
                         (start-pt (save-excursion
                                     (goto-char match-beg)
                                     (forward-line (- ctx-lines))
                                     (line-beginning-position)))
                         (end-pt (save-excursion
                                   (goto-char match-beg)
                                   (forward-line ctx-lines)
                                   (line-end-position)))
                         (snippet (buffer-substring-no-properties start-pt end-pt)))
                    (push (format "--- Match near line %d ---\n%s\n"
                                  (line-number-at-pos match-beg) snippet)
                          results)
                    (when (= match-beg match-end)
                      (if (eobp)
                          (setq continue nil)
                        (forward-char 1))))))
            (invalid-regexp
             (setq invalid-re (error-message-string err))))))
      (cond
       (invalid-re
        (format "Error: Invalid regular expression: %s" invalid-re))
       (results
        (string-join (nreverse results) "\n"))
       (t
        (format "No matches found in history for: %s" query))))))

(defun macher-agent--buffer-to-traces (buffer)
  "Convert BUFFER content into trace plists for Zero-Mem graph construction."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((traces nil)
              (line-num 1))
          (while (not (eobp))
            (let ((line-text (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
              (unless (string-empty-p (string-trim line-text))
                (push (list :text line-text
                            :timestamp (float line-num)
                            :metadata (list :line line-num))
                      traces)))
            (setq line-num (1+ line-num))
            (forward-line 1))
          (nreverse traces))))))

(defun macher-agent-search-zero-mem (query orig-buf &optional ctx-lines)
  "Search for QUERY in ORIG-BUF with CTX-LINES using Zero-Mem PageRank retrieval."
  (if (not (buffer-live-p orig-buf))
      "Error: Cannot locate original conversation buffer."
    (let* ((traces (macher-agent--buffer-to-traces orig-buf))
           (top-k (if (and ctx-lines (> ctx-lines 0)) ctx-lines 5)))
      (if (null traces)
          (format "No matches found in history for: %s" query)
        (let* ((graph (macher-agent-zero-mem-build-graph traces))
               (retrieved
                (macher-agent-zero-mem-retrieve
                 query graph :top-k top-k))
               (results nil))
          (dolist (tr retrieved)
            (let ((line (or (plist-get (macher-agent-zero-mem-trace-metadata tr) :line)
                            (macher-agent-zero-mem-trace-id tr)))
                  (text (macher-agent-zero-mem-trace-text tr)))
              (push (format "--- Match near line %d ---\n%s\n" line text) results)))
          (if results
              (string-join (nreverse results) "\n")
            (format "No matches found in history for: %s" query)))))))

(defun macher-agent-search-dispatch (query orig-buf &optional ctx-lines)
  "Dispatch search for QUERY in ORIG-BUF with CTX-LINES context.
Based on `macher-agent-search-backend'."
  (if (not (buffer-live-p orig-buf))
      "Error: Cannot locate original conversation buffer."
    (pcase macher-agent-search-backend
      ('zero-mem (macher-agent-search-zero-mem query orig-buf ctx-lines))
      (_ (macher-agent-search-glob query orig-buf ctx-lines)))))

(provide 'macher-agent-tools)
;;; macher-agent-tools.el ends here
