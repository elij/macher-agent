;;; macher-agent-gptel-tools.el --- Pure gptel orchestration tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure gptel orchestration tools for Macher Agent.

;;; Code:

(require 'macher)
(require 'json)
(require 'cl-lib)
(require 'transient)
(require 'subr-x)
(require 'macher-agent-vfs-client)

(declare-function macher-agent-current-context "macher-agent-vfs-client")
(declare-function macher-agent--resolve-buffer-name "macher-agent-orchestration")
(declare-function macher-agent--read-context-file "macher-agent-vfs-client")
(declare-function macher-agent--merge-contexts "macher-agent-vfs-client")
(declare-function macher-agent--clone-context "macher-agent-vfs-client")
(declare-function macher-agent--ensure-access "macher-agent-vfs-client")
(declare-function macher-agent-context-classify-entry "macher-agent-vfs-client")

;;; Response Structures

(cl-defstruct macher-agent-tool-response
  "Represent a base tool execution response structure.

PAYLOAD is the underlying response object or data payload.
STATUS is the execution status symbol, such as success or error.
DATA is additional structured data associated with the result.
ERROR is an error message string or object if execution failed.
BUFFER-NAME is the associated buffer name string if applicable.

Return a new `macher-agent-tool-response' struct instance.
Side effects: None."
  payload
  status
  data
  error
  buffer-name)

(cl-defstruct (macher-agent-process-response (:include macher-agent-tool-response))
  "Represent a process tool execution response structure.

Return a new `macher-agent-process-response' struct instance.
Side effects: None.")

(cl-defstruct (macher-agent-delegate-response (:include macher-agent-tool-response))
  "Represent a task delegation response structure.

Return a new `macher-agent-delegate-response' struct instance.
Side effects: None.")

(cl-defstruct (macher-agent-nohup-response (:include macher-agent-tool-response))
  "Represent a detached process response structure.

Return a new `macher-agent-nohup-response' struct instance.
Side effects: None.")

(cl-defstruct (macher-agent-lisp-result-response (:include macher-agent-tool-response))
  "Represent a Lisp evaluation result response structure.

Return a new `macher-agent-lisp-result-response' struct instance.
Side effects: None.")

(cl-defstruct (macher-agent-ptc-response (:include macher-agent-tool-response))
  "Represent a Programmatic Tool Calling response structure.

Return a new `macher-agent-ptc-response' struct instance.
Side effects: None.")

;;; Customisation Variables

(defvar macher-agent-ptc-raw-mode nil
  "Toggle raw Lisp data return for Programmatic Tool Calling execution.

If non-nil, tools bypass string formatting and return raw Lisp data.

Return non-nil when raw mode is active, nil otherwise.
Side effects: None.")

(defvar macher-agent--active-ptc-execution nil
  "Indicate whether a Programmatic Tool Calling Lisp script is active.

If non-nil, evaluation of a Programmatic Tool Calling Lisp script is in progress.

Return non-nil when active, nil otherwise.
Side effects: None.")

(defvar macher-agent-allowed-tools nil
  "Hold custom tool names that receive the active Macher context.

List of custom tool name symbols permitted to access context.

Return list of allowed tool symbol names.
Side effects: None.")

(defcustom macher-agent-display-subagent-fn nil
  "Specify function to display a sub-agent buffer during execution.

BUFFER is the buffer object to display.
If nil, the buffer executes silently in the background.

Return the display function or nil.
Side effects: None."
  :type '(choice (const :tag "Silent Background Execution" nil)
                 function)
  :group 'macher-agent)

(defcustom macher-agent-hide-subagent-fn nil
  "Specify function to hide a sub-agent buffer after execution finishes.

BUFFER is the buffer object to hide.
If nil, no action is taken when finished.

Return the hide function or nil.
Side effects: None."
  :type '(choice (const :tag "Do Nothing" nil)
                 function)
  :group 'macher-agent)

(defvar-local macher-agent--pending-instructions-queue nil
  "Store pending system instruction strings for tool response payloads.

Return list of instruction strings queued for insertion.
Side effects: None.")

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

(defun macher-agent--resolve-context (passed-context)
  "Resolve the active agent context structure.

PASSED-CONTEXT is an optional context object passed directly, which may be nil.

Return the resolved context structure, or nil if context cannot be resolved.
Side effects: None."
  (or passed-context
      (ignore-errors (macher-agent-current-context))
      (when-let* ((fsm (or (bound-and-true-p macher-agent--active-fsm)
                           (bound-and-true-p macher--fsm-latest)
                           (bound-and-true-p gptel--fsm-last)))
                  (info (macher-agent--extract-fsm-info fsm)))
        (plist-get info :macher-agent-context))))

(defun macher-agent--format-directives (result-data)
  "Append pending system instructions to RESULT-DATA if directives exist.

RESULT-DATA is the string or object response from tool execution.

Return formatted string incorporating pending directives.
Side effects: Clears `macher-agent--pending-instructions-queue' when non-empty."
  (let ((final-str (if (stringp result-data) result-data (format "%S" result-data))))
    (when macher-agent--pending-instructions-queue
      (setq final-str (concat final-str "\n\n=== SYSTEM DIRECTIVE ===\n"
                              (string-join (nreverse macher-agent--pending-instructions-queue) "\n")))
      (setq macher-agent--pending-instructions-queue nil))
    final-str))

(defun macher-agent--wrap-callback (gptel-cb)
  "Create a callback wrapper that parses results and formats directives.

GPTEL-CB is the original gptel callback function, which may be nil.

Return a unary callback function accepting a plist result structure.
Side effects: None."
  (lambda (plist-result)
    (let ((final-str (if (eq (plist-get plist-result :status) 'success)
                         (plist-get plist-result :data)
                       (plist-get plist-result :error))))
      (when (eq (plist-get plist-result :status) 'error)
        (message "MACHER AGENT ERROR: %S" final-str))
      (if gptel-cb
          (funcall gptel-cb (macher-agent--format-directives final-str))
        (macher-agent--format-directives final-str)))))

(defun macher-agent--show-ui (buf)
  "Safely trigger display function for buffer BUF.

BUF is the buffer object to display.

Return nil.
Side effects: Invokes `macher-agent-display-subagent-fn' when non-nil."
  (when macher-agent-display-subagent-fn
    (funcall macher-agent-display-subagent-fn buf)))

(defun macher-agent--hide-ui (buf)
  "Safely trigger hide function for buffer BUF.

BUF is the buffer object to hide.

Return nil.
Side effects: Invokes `macher-agent-hide-subagent-fn' when non-nil."
  (when macher-agent-hide-subagent-fn
    (funcall macher-agent-hide-subagent-fn buf)))

(defun macher-agent--insert-hidden (text)
  "Insert TEXT hidden visually via display overlay while remaining readable.

TEXT is the string content to insert.

Return nil.
Side effects: Inserts text and creates an overlay at point."
  (let* ((start (point))
         (_ (insert text))
         (ov (make-overlay start (point))))
    (overlay-put ov 'display "")
    (overlay-put ov 'insert-behind-hooks '(ignore))))

(defun macher-agent--normalize-key-string (key)
  "Normalise KEY to a hyphenated string with underscores replaced.

KEY is a keyword, symbol, or string key.

Return normalised string with underscores converted to hyphens.
Side effects: None."
  (let ((key-str (if (keywordp key)
                     (substring (symbol-name key) 1)
                   (format "%s" key))))
    (replace-regexp-in-string "_" "-" key-str)))

(defun macher-agent--extract-prop-hash (obj norm-key)
  "Extract property matching NORM-KEY from hash-table OBJ.

OBJ is the hash-table object to inspect.
NORM-KEY is the normalised string key to find.

Return the matching value, or symbol `macher-missing' if not found.
Side effects: None."
  (let ((res 'macher-missing))
    (maphash (lambda (k v)
               (when (equal (macher-agent--normalize-key-string k) norm-key)
                 (setq res v)))
             obj)
    res))

(defun macher-agent--extract-prop-list (obj norm-key)
  "Extract property matching NORM-KEY from list, alist, or plist OBJ.

OBJ is the list, alist, or plist structure.
NORM-KEY is the normalised string key to find.

Return the matching value, or symbol `macher-missing' if not found.
Side effects: None."
  (let ((res 'macher-missing)
        (tail obj))
    (while (and (eq res 'macher-missing) tail (consp tail))
      (let ((elem (car tail)))
        (if (consp elem)
            (progn
              (when (equal (macher-agent--normalize-key-string (car elem)) norm-key)
                (setq res (cdr elem)))
              (setq tail (cdr tail)))
          (when (equal (macher-agent--normalize-key-string elem) norm-key)
            (setq res (cadr tail)))
          (setq tail (cddr tail)))))
    res))

(defun macher-agent--extract-prop (obj key)
  "Extract KEY from OBJ handling plists, alists, and hash-tables.

OBJ is a hash-table, plist, or alist object.
KEY is the target key symbol, keyword, or string.

Return matching property value, or nil if not present.
Side effects: None."
  (let* ((norm-key (macher-agent--normalize-key-string key))
         (res (cond
               ((hash-table-p obj) (macher-agent--extract-prop-hash obj norm-key))
               ((listp obj) (macher-agent--extract-prop-list obj norm-key))
               (t 'macher-missing))))
    (if (eq res 'macher-missing) nil res)))

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
  (let ((props (plist-get spec :properties))
        (reqs (plist-get spec :required)))
    (when (and (consp props) (eq (car props) 'quote))
      (setq props (cadr props)))
    (when (and (consp reqs) (eq (car reqs) 'quote))
      (setq reqs (cadr reqs)))
    (when (vectorp reqs)
      (setq reqs (append reqs nil)))
    (when props
      (unless (and (listp props) (evenp (length props)))
        (error "%s" (format "Schema properties for '%s' must be a flat plist, got %S" name props)))
      (cl-loop for (k v-spec) on props by #'cddr
               for prop-name = (if (keywordp k) (substring (symbol-name k) 1) (format "%s" k))
               for child-val = (macher-agent--extract-prop value k)
               do (if child-val
                      (macher-agent--validate-schema
                       v-spec child-val (format "%s.%s" name prop-name))
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
  (if (and tool-args (keywordp (car tool-args)))
      tool-args
    (cl-loop for arg in tool-args
             for spec in args-spec
             for arg-name = (plist-get spec :name)
             for key = (intern (concat ":" (if (symbolp arg-name)
                                               (symbol-name arg-name)
                                             arg-name)))
             append (list key arg))))

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
Side effects: Runs hooks in `macher-agent-pre-tool-use-hook' and permission hook."
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

(defmacro macher-agent--build-success-callback (name-sym payload success-fn output-filter-fn wrap-cb)
  "Generate success callback for NAME-SYM using PAYLOAD and handlers.

NAME-SYM is the tool symbol name.
PAYLOAD is the argument plist payload.
SUCCESS-FN is the optional result formatting function.
OUTPUT-FILTER-FN is the optional output filtering function.
WRAP-CB is the callback function wrapper.

Return a lambda callback function accepting response object.
Side effects: Runs `macher-agent-post-tool-use-hook' on success or failure hook on error."
  `(lambda (res-obj)
     (condition-case cb-err
         (let* ((s-fn ,success-fn)
                (f-fn ,output-filter-fn)
                (raw-payload (if (macher-agent-tool-response-p res-obj)
                                 (macher-agent-tool-response-payload res-obj)
                               res-obj))
                (success-data (if s-fn
                                  (let ((arity (func-arity s-fn)))
                                    (if (or (eq (cdr arity) 'many)
                                            (and (numberp (cdr arity)) (>= (cdr arity) 2)))
                                        (funcall s-fn raw-payload payload)
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
  (let* ((stripped-name (replace-regexp-in-string "^macher-agent-\\|-tool$" "" (symbol-name name-symbol)))
         (name (replace-regexp-in-string "-" "_" stripped-name)))
    `(progn
       (defvar ,name-symbol nil)
       (setq ,name-symbol
             (gptel-make-tool
              :name ,name
              :description ,description
              :category ,(or category "macher-agent")
              :args ,args
              :async t
              :function
              (lambda (callback &rest tool-args)
                (let* ((wrap-cb (macher-agent--wrap-callback callback))
                       (payload (macher-agent--extract-payload tool-args ,args)))
                  (macher-agent--with-tool-error-handling ',name-symbol payload wrap-cb
                    (macher-agent--validate-payload payload ,args)
                    (let* ((context (or (ignore-errors (macher-agent-resolve-context))
                                        (bound-and-true-p macher-agent--persistent-context)))
                           (root (if context (macher-agent-context-root context) default-directory))
                           (hook-rejection (macher-agent--run-pre-hooks ',name-symbol payload)))
                      (if hook-rejection
                          (funcall wrap-cb (list :status 'error :error hook-rejection))
                        (let* ((action (funcall ,command-fn payload context root))
                               (action-res (if (macher-agent-tool-response-p action)
                                               action
                                             (make-macher-agent-lisp-result-response
                                              :payload action)))
                               (on-success (macher-agent--build-success-callback
                                            ',name-symbol payload ,success-fn ,output-filter-fn wrap-cb)))
                          (macher-agent-execute-response action-res context on-success wrap-cb))))))))))))

;;; Response Execution Methods

(cl-defgeneric macher-agent-execute-response (response context on-success on-error)
  "Execute action encapsulated by RESPONSE structure.

RESPONSE is the tool response structure instance.
CONTEXT is the active agent context structure.
ON-SUCCESS is the callback called on successful execution.
ON-ERROR is the callback called on execution failure.

Return result of dispatching to specific method implementation.
Side effects: Performs actions dictated by RESPONSE structure type.")

(cl-defmethod macher-agent-execute-response (res _context on-success _on-error)
  "Handle fallback response execution for raw Lisp objects or structs.

RES is the response struct or raw Lisp payload object.
_CONTEXT is the ignored context object.
ON-SUCCESS is the callback function for success.
_ON-ERROR is the ignored error callback function.

Return result of invoking ON-SUCCESS callback.
Side effects: Invokes ON-SUCCESS callback with response payload or raw value."
  (if (macher-agent-tool-response-p res)
      (funcall on-success (macher-agent-tool-response-payload res))
    (funcall on-success res)))

(cl-defmethod macher-agent-execute-response ((res macher-agent-process-response) context on-success on-error)
  "Execute process response RES in sandbox using CONTEXT.

RES is the process response struct instance.
CONTEXT is the active agent context structure.
ON-SUCCESS is the callback function called on success.
ON-ERROR is the callback function called on failure.

Return result of sandbox execution invocation.
Side effects: Spawns process within persistent sandbox environment."
  (let ((payload (macher-agent-tool-response-payload res)))
    (if (stringp payload)
        (macher-agent--run-in-persistent-sandbox
         context payload
         (lambda (process-output)
           (setf (macher-agent-tool-response-payload res) process-output)
           (funcall on-success res))
         on-error)
      (funcall on-error (list :status 'error :error "Process payload must be a string.")))))

(cl-defmethod macher-agent-execute-response ((res macher-agent-delegate-response) _context on-success _on-error)
  "Delegate task execution in RES in parallel and invoke ON-SUCCESS.

RES is the delegate response struct instance.
_CONTEXT is the ignored context structure.
ON-SUCCESS is the callback function called on completion.
_ON-ERROR is the ignored error callback function.

Return result of parallel execution invocation.
Side effects: Spawns parallel sub-agent processes."
  (macher-agent-execute-parallel
   (macher-agent-tool-response-payload res)
   (lambda (sub-agent-results)
     (setf (macher-agent-tool-response-payload res) sub-agent-results)
     (funcall on-success res))))

(cl-defmethod macher-agent-execute-response ((res macher-agent-nohup-response) _context on-success _on-error)
  "Run detached process command in RES asynchronously and call ON-SUCCESS.

RES is the nohup response struct instance.
_CONTEXT is the ignored context structure.
ON-SUCCESS is the callback function called on launch.
_ON-ERROR is the ignored error callback function.

Return result of invoking ON-SUCCESS callback.
Side effects: Launches detached background command asynchronously."
  (macher-agent--run-async-cmd "detached" (macher-agent-tool-response-payload res) default-directory (lambda (_ _)))
  (setf (macher-agent-tool-response-payload res) "SUCCESS: Process started.")
  (funcall on-success res))

(cl-defmethod macher-agent-execute-response ((res macher-agent-lisp-result-response) _context on-success _on-error)
  "Execute Lisp payload for response RES and invoke ON-SUCCESS.

RES is the Lisp result response struct instance.
_CONTEXT is the ignored context structure.
ON-SUCCESS is the callback function called on completion.
_ON-ERROR is the ignored error callback function.

Return result of evaluating Lisp task.
Side effects: Evaluates Lisp payload task asynchronously."
  (macher-agent-execute-lisp-task
   (macher-agent-tool-response-payload res)
   (lambda (lisp-result)
     (setf (macher-agent-tool-response-payload res) lisp-result)
     (funcall on-success res))))

(cl-defmethod macher-agent-execute-response ((res macher-agent-ptc-response) context on-success on-error)
  "Execute Programmatic Tool Calling script in RES using CONTEXT.

RES is the PTC response struct instance.
CONTEXT is the active agent context structure.
ON-SUCCESS is the callback function called on completion.
ON-ERROR is the callback function called on error.

Return result of evaluating iterator loop.
Side effects: Evaluates Lisp AST and dispatches tool calls with feedback."
  (condition-case general-err
      (let ((macher-agent--active-ptc-execution t)
            (script-string (macher-agent-tool-response-payload res))
            (target-buf (or (macher-agent-tool-response-buffer-name res) (current-buffer))))
        (if (not script-string)
            (funcall on-error (list :status 'error :error "Invalid PTC script payload"))
          (let ((ast (macroexpand-all (read script-string))))
            (with-current-buffer target-buf
              (let ((iterator (macher-agent-sandbox--eval-iter ast nil)))
                (if (not iterator)
                    (funcall on-error (list :status 'error :error "Failed to initialise PTC iterator"))
                  (letrec
                      ((ptc-resume-loop
                        (lambda (last-result)
                          (with-current-buffer target-buf
                            (when iterator
                              (condition-case iter-err
                                  (let ((yielded-val (iter-next iterator last-result)))
                                    (macher-agent--ptc-handle-yielded-value
                                     yielded-val context ptc-resume-loop on-error
                                     (lambda () (setq iterator nil))))
                                (iter-end-of-sequence
                                 (setq iterator nil)
                                 (when (fboundp 'gptel--update-status)
                                   (gptel--update-status " PTC: Complete" 'success))
                                 (funcall on-success (cdr iter-err)))
                                (error
                                 (setq iterator nil)
                                 (funcall on-error (list :status 'error
                                                         :error (error-message-string iter-err))))))))))
                    (funcall ptc-resume-loop nil))))))))
    (error
     (funcall on-error (list :status 'error :error (error-message-string general-err))))))

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
                 original-name-str)))
    (message "PTC Executing: %s" desc)
    (when (fboundp 'gptel--update-status)
      (gptel--update-status (format " PTC: %s..." original-name-str) 'mode-line-emphasis))
    (when (bound-and-true-p gptel-pre-tool-call-functions)
      (run-hook-wrapped 'gptel-pre-tool-call-functions
                        (lambda (f)
                          (ignore-errors
                            (funcall f (list :name original-name-str :args args :buffer (buffer-name) :model (bound-and-true-p gptel-model)))))))
    (cons original-name-str resolved-tool)))

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
      (run-hook-wrapped 'gptel-post-tool-call-functions
                        (lambda (f)
                          (ignore-errors
                            (funcall f (list :name original-name-str :args args :result str-res :buffer (buffer-name) :model (bound-and-true-p gptel-model)))))))
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
    (cl-loop for (k v) on args by #'cddr
             for spec = (cl-find-if (lambda (s)
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

(defun macher-agent--format-ptc-subagent-results (sub-agent-results)
  "Format SUB-AGENT-RESULTS into a list of result plists.

SUB-AGENT-RESULTS is a vector or list of sub-agent response objects.

Return list of formatted result plists.
Side effects: None."
  (let ((vec (if (vectorp sub-agent-results) sub-agent-results (vconcat sub-agent-results))))
    (cl-loop for sub-res across vec
             collect (list :buffer (cond
                                    ((macher-agent-tool-response-p sub-res) (macher-agent-tool-response-buffer-name sub-res))
                                    ((listp sub-res) (plist-get sub-res :buffer))
                                    (t (format "%s" sub-res)))
                           :status (cond
                                    ((macher-agent-tool-response-p sub-res) (macher-agent-tool-response-status sub-res))
                                    ((listp sub-res) (plist-get sub-res :status))
                                    (t 'success))
                           :data (cond
                                  ((macher-agent-tool-response-p sub-res)
                                   (if (eq (macher-agent-tool-response-status sub-res) 'success)
                                       (macher-agent-tool-response-data sub-res)
                                     (macher-agent-tool-response-error sub-res)))
                                  ((listp sub-res) (plist-get sub-res :data))
                                  (t sub-res))))))

(defun macher-agent--dispatch-ptc-primitive (tool-lisp-sym args resolved-tool original-name-str ptc-callback on-error stop-iter-fn)
  "Dispatch TOOL-LISP-SYM with ARGS or RESOLVED-TOOL for ORIGINAL-NAME-STR.

TOOL-LISP-SYM is the symbol naming the Lisp primitive function.
ARGS is the list of arguments for the primitive call.
RESOLVED-TOOL is the resolved tool object or nil.
ORIGINAL-NAME-STR is the original tool name string.
PTC-CALLBACK is the callback function receiving output data.
ON-ERROR is the error callback function on failure.
STOP-ITER-FN is the function called to halt iterator loop.

Return result of primitive execution or callback invocation.
Side effects: Invokes target primitive function or tool callback."
  (if (fboundp tool-lisp-sym)
      (let ((lisp-res (apply tool-lisp-sym args)))
        (funcall ptc-callback
                 (if (macher-agent-tool-response-p lisp-res)
                     (macher-agent-tool-response-payload lisp-res)
                   lisp-res)))
    (if-let* ((_ (macher-tool-valid-p resolved-tool))
              (tool-fn (gptel-tool-function resolved-tool)))
        (let ((coerced-args (macher-agent--coerce-ptc-args args resolved-tool)))
          (cl-letf (((symbol-function 'macher-agent-execute-response)
                     (lambda (action-res _ctx _on-succ _on-err)
                       (if (macher-agent-delegate-response-p action-res)
                           (macher-agent-execute-parallel
                            (macher-agent-tool-response-payload action-res)
                            (lambda (sub-agent-results)
                              (funcall ptc-callback (macher-agent--format-ptc-subagent-results sub-agent-results))))
                         (funcall ptc-callback
                                  (if (macher-agent-tool-response-p action-res)
                                      (macher-agent-tool-response-payload action-res)
                                    action-res))))))
            (if (gptel-tool-async resolved-tool)
                (apply tool-fn (lambda (err-res) (funcall ptc-callback err-res)) coerced-args)
              (let ((sync-result (apply tool-fn coerced-args)))
                (funcall ptc-callback sync-result)))))
      (when stop-iter-fn (funcall stop-iter-fn))
      (funcall on-error
               (list :status 'error
                     :error (format "Unknown PTC primitive requested: %s" original-name-str))))))

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

(defun macher-agent--run-async-cmd (name cmd dir callback)
  "Execute command CMD asynchronously with explicit lexical capture.

NAME is the process name string.
CMD is the shell command string to execute.
DIR is the working directory string.
CALLBACK is the function called upon process exit with status and output.

Return process object.
Side effects: Spawns asynchronous process and creates output buffer."
  (let* ((out-buf (generate-new-buffer (format " *%s*" name)))
         (default-directory dir))
    (make-process
     :name name
     :buffer out-buf
     :command (list shell-file-name shell-command-switch cmd)
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (let ((output (with-current-buffer out-buf (buffer-string)))
                         (exit-code (process-exit-status proc)))
                     (kill-buffer out-buf)
                     (funcall callback exit-code output)))))))

(defun macher-agent--run-in-persistent-sandbox (context command on-success on-error)
  "Execute COMMAND asynchronously within dynamically generated VFS sandbox.

CONTEXT is the active agent context structure.
COMMAND is the shell command string to execute.
ON-SUCCESS is the success callback function.
ON-ERROR is the error callback function.

Return nil.
Side effects: Creates temporary sandbox directory and spawns process."
  (let* ((workspace-root (if context (macher-agent-context-root context) default-directory))
         (sandbox-dir (make-temp-file "macher-sandbox-" t)))
    (condition-case err
        (progn
          (macher-agent--vfs-verify-clean-merge workspace-root context)
          (macher-agent--vfs-sync-baseline workspace-root sandbox-dir)
          (macher-agent--vfs-apply-overlay context sandbox-dir)

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
  (let* ((vfs-entry (when context (cl-find file-path (macher-context-contents context) :key #'car :test #'equal)))
         (vfs-content (when vfs-entry (if (consp (cdr vfs-entry)) (cddr vfs-entry) (cdr vfs-entry)))))
    (cond
     (vfs-content vfs-content)
     ((file-exists-p file-path)
      (with-temp-buffer (insert-file-contents file-path) (buffer-string)))
     (t nil))))

(defun macher-agent-add-pending-instruction (instruction)
  "Push INSTRUCTION directive to steer model after tool execution completes.

INSTRUCTION is the directive string to push.

Return nil.
Side effects: Pushes directive string onto `macher-agent--pending-instructions-queue'."
  (push instruction macher-agent--pending-instructions-queue))

(defun macher-agent--format-error (err)
  "Standardise error message string ERR for language model consumption.

ERR is the error signal data or object.

Return formatted error message string.
Side effects: None."
  (let ((msg (error-message-string err)))
    (if (string-match-p "^\\(ERROR\\|SECURITY ERROR\\):" msg)
        msg
      (format "ERROR: %s" msg))))

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
  (if-let* ((fsm (or (and (boundp 'macher-agent--active-fsm) macher-agent--active-fsm)
                     (and (boundp 'macher--fsm-latest) (symbol-value 'macher--fsm-latest))
                     (and (boundp 'gptel--fsm-last) (symbol-value 'gptel--fsm-last))))
            (info (ignore-errors (gptel-fsm-info fsm)))
            (ctx (plist-get info :macher-agent-context))
            (pending (when ctx (macher-agent--get-context-data ctx :pending-media)))
            ((cl-some (lambda (item) (string= file (car item))) pending)))
      file
    (if-let* ((ctx (ignore-errors (macher-agent-current-context)))
              (workspace (macher-context-workspace ctx))
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

(advice-add 'gptel--base64-encode :around #'macher-agent--gptel-base64-encode-advice)

(with-eval-after-load 'gptel-transient
  (ignore-errors
    (transient-suffix-put 'gptel-menu 'gptel--infix-tools :save-history nil)
    (transient-suffix-put 'gptel-menu 'gptel--infix-system-message :save-history nil)))

(provide 'macher-agent-gptel-tools)
;;; macher-agent-gptel-tools.el ends here