;;; macher-agent-tools.el --- Pure gptel orchestration tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure gptel orchestration tools for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'macher-agent-core)
(require 'macher-agent-gptel)

;;; Customisation Variables

(put 'macher-agent--pending-instructions-queue 'permanent-local t)

;;; Execution Helpers

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
           ((or 'boolean "boolean") (not (null (memq value '(t :json-false)))))
           ((or 'array "array")      (vectorp value))
           ((or 'object "object")   (or (listp value) (hash-table-p value)))
           (_ (error "%s" (format "Unknown schema type: %S" type))))))
    (if is-valid
        t
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

(defun macher-agent--param-name-matches-p (name-a name-b)
  "Return t if NAME-A matches NAME-B, ignoring leading colons and hyphen/underscore differences.

NAME-A and NAME-B are property key symbols or strings.

Return t if names match modulo separator, nil otherwise.
Side effects: None."
  (let* ((str-a (cond ((symbolp name-a) (symbol-name name-a))
                      ((stringp name-a) name-a)
                      (t (format "%s" name-a))))
         (str-b (cond ((symbolp name-b) (symbol-name name-b))
                      ((stringp name-b) name-b)
                      (t (format "%s" name-b))))
         (clean-a (if (string-prefix-p ":" str-a) (substring str-a 1) str-a))
         (clean-b (if (string-prefix-p ":" str-b) (substring str-b 1) str-b))
         (norm-a (replace-regexp-in-string "_" "-" clean-a))
         (norm-b (replace-regexp-in-string "_" "-" clean-b)))
    (equal norm-a norm-b)))

(defun macher-agent--spec-has-param-p (args-spec key)
  "Return non-nil if KEY matches any parameter name in ARGS-SPEC.

ARGS-SPEC is the expected argument schema specification list.
KEY is the candidate parameter symbol or string.

Return t if KEY matches a parameter name in ARGS-SPEC, nil otherwise.
Side effects: None."
  (cl-some (lambda (spec)
             (let ((name (plist-get spec :name)))
               (and name (macher-agent--param-name-matches-p key name))))
           args-spec))

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
       for under-k = (intern (concat ":" (replace-regexp-in-string "-" "_" prop-name)))
       for hyphen-k = (intern (concat ":" (replace-regexp-in-string "_" "-" prop-name)))
       for has-val = (and (macher-agent--plist-p value)
                          (or (plist-member value under-k)
                              (plist-member value hyphen-k)
                              (plist-member value k)))
       for child-val = (when has-val
                         (or (plist-get value under-k)
                             (plist-get value hyphen-k)
                             (plist-get value k)))
       do (if has-val
              (macher-agent--validate-schema v-spec child-val (format "%s.%s" name prop-name))
            (when (or (and reqs
                           (let* ((hyphen-prop (replace-regexp-in-string "_" "-" prop-name))
                                  (under-prop (replace-regexp-in-string "-" "_" prop-name)))
                             (or (member prop-name reqs)
                                 (member hyphen-prop reqs)
                                 (member under-prop reqs)
                                 (member (intern prop-name) reqs)
                                 (member (intern hyphen-prop) reqs)
                                 (member (intern under-prop) reqs)
                                 (member (intern (concat ":" prop-name)) reqs)
                                 (member (intern (concat ":" hyphen-prop)) reqs)
                                 (member (intern (concat ":" under-prop)) reqs))))
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
  (let ((raw-plist
         (cond
          ((and tool-args (keywordp (car tool-args))
                (macher-agent--spec-has-param-p args-spec (car tool-args)))
           tool-args)
          ((and tool-args (listp (car tool-args)) (keywordp (caar tool-args))
                (macher-agent--spec-has-param-p args-spec (caar tool-args)))
           (car tool-args))
          (t
           (cl-loop for arg in tool-args
                    for spec in args-spec
                    for arg-name = (plist-get spec :name)
                    for key = (intern (concat ":" (if (symbolp arg-name)
                                                      (symbol-name arg-name)
                                                    (format "%s" arg-name))))
                    append (list key arg))))))
    (let ((result nil))
      (if (and (null args-spec) raw-plist)
          raw-plist
        (dolist (spec args-spec)
          (let* ((arg-name (plist-get spec :name))
                 (name-str (if (symbolp arg-name) (symbol-name arg-name) (format "%s" arg-name)))
                 (clean-name (if (string-prefix-p ":" name-str) (substring name-str 1) name-str))
                 (under-key (intern (concat ":" (replace-regexp-in-string "-" "_" clean-name))))
                 (hyphen-key (intern (concat ":" (replace-regexp-in-string "_" "-" clean-name))))
                 (val (when (macher-agent--plist-p raw-plist)
                        (or (when (plist-member raw-plist under-key) (plist-get raw-plist under-key))
                            (when (plist-member raw-plist hyphen-key) (plist-get raw-plist hyphen-key))
                            (when (plist-member raw-plist (intern clean-name)) (plist-get raw-plist (intern clean-name)))
                            (when (plist-member raw-plist clean-name) (plist-get raw-plist clean-name))))))
            (when (or (and (macher-agent--plist-p raw-plist)
                           (or (plist-member raw-plist under-key)
                               (plist-member raw-plist hyphen-key)
                               (plist-member raw-plist (intern clean-name))
                               (plist-member raw-plist clean-name)))
                      val)
              (setq result (plist-put result under-key val))
              (setq result (plist-put result hyphen-key val)))))
        (when (and (listp raw-plist) (evenp (length raw-plist)))
          (cl-loop for (k v) on raw-plist by #'cddr
                   do (unless (plist-member result k)
                        (setq result (plist-put result k v)))))
        result))))

(defun macher-agent--validate-payload (payload args-spec)
  "Validate PAYLOAD against ARGS-SPEC using internal schema validator.

PAYLOAD is the argument payload plist to validate.
ARGS-SPEC is the expected argument schema list.

Return nil upon successful validation.
Side effects: Signals error if payload validation fails."
  (cl-loop for spec in args-spec
           for arg-name = (plist-get spec :name)
           for name-str = (if (symbolp arg-name) (symbol-name arg-name) (format "%s" arg-name))
           for clean-name = (if (string-prefix-p ":" name-str) (substring name-str 1) name-str)
           for under-key = (intern (concat ":" (replace-regexp-in-string "-" "_" clean-name)))
           for hyphen-key = (intern (concat ":" (replace-regexp-in-string "_" "-" clean-name)))
           for val = (when (macher-agent--plist-p payload)
                       (or (when (plist-member payload under-key) (plist-get payload under-key))
                           (when (plist-member payload hyphen-key) (plist-get payload hyphen-key))
                           (when (plist-member payload (intern clean-name)) (plist-get payload (intern clean-name)))))
           do (macher-agent--validate-schema spec val)))

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

(cl-defmacro macher-agent-make-tool (name-symbol description &key category args command-fn success-fn output-filter-fn (include nil include-p))
  "Define NAME-SYMBOL as a tool compatible with gptel using DESCRIPTION.

NAME-SYMBOL is the symbol naming the tool variable.
DESCRIPTION is the string description of tool functionality.
CATEGORY is the optional category string, defaulting to macher-agent.
ARGS is the argument schema specification list.
COMMAND-FN is the function executing core tool logic.
SUCCESS-FN is the optional callback formatting successful output.
OUTPUT-FILTER-FN is the optional filter function for final result data.
INCLUDE is the optional inclusion mode for tool results in the buffer.

Return expanding form defining and initialising the tool variable.
Side effects: Sets `NAME-SYMBOL' variable to constructed gptel tool object."
  (declare (indent 2))
  (let* ((stripped-name (replace-regexp-in-string "^macher-agent-tool-\\|^macher-agent-\\|-tool$" "" (symbol-name name-symbol)))
         (name (replace-regexp-in-string "-" "_" stripped-name))
         (extra-args (when include-p (list :include include))))
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
                     (incoming-ctx (when (macher-agent-valid-context-p callback) callback))
                     (actual-cb (if incoming-ctx (car tool-args) callback))
                     (actual-tool-args (if incoming-ctx (cdr tool-args) tool-args))
                     (wrap-cb (macher-agent--wrap-callback actual-cb ptc-exec))
                     (payload (macher-agent--extract-payload actual-tool-args ,args))
                     (tool-call (make-macher-agent-tool-call
                                 :name ',name-symbol
                                 :args payload)))
                  (macher-agent--with-tool-error-handling
                   ',name-symbol (macher-agent-tool-call-args tool-call) wrap-cb
                   (macher-agent--validate-payload (macher-agent-tool-call-args tool-call) ,args)
                   (let*
                       ((fsm (macher-agent-get-active-fsm))
                        (fsm-info (when fsm (macher-agent--extract-fsm-info fsm)))
                        (origin-buf (when (macher-agent--plist-p fsm-info)
                                      (or (plist-get fsm-info :origin-buffer)
                                          (plist-get fsm-info :buffer))))
                        (context
                         (or incoming-ctx
                             (when (macher-agent--plist-p fsm-info)
                               (let ((f-ctx (plist-get fsm-info :macher-agent-context)))
                                 (when (macher-agent-valid-context-p f-ctx)
                                   f-ctx)))
                             (when fsm
                               (macher-agent-resolve-context fsm))
                             (when (and origin-buf (buffer-live-p origin-buf))
                               (let ((buf-ctx (buffer-local-value 'macher-agent--persistent-context origin-buf)))
                                 (if (macher-agent-valid-context-p buf-ctx)
                                     buf-ctx
                                   (macher-agent-resolve-context origin-buf))))
                             (when (bound-and-true-p macher-agent--persistent-context)
                               (when (macher-agent-valid-context-p macher-agent--persistent-context)
                                 macher-agent--persistent-context))
                             (macher-agent-resolve-context (current-buffer))
                             (macher-agent-resolve-context)))
                        (root
                         (if context
                             (macher-agent-context-root context)
                           default-directory))
                        (hook-rejection (macher-agent--run-pre-hooks ',name-symbol (macher-agent-tool-call-args tool-call))))
                     (if hook-rejection
                         (funcall wrap-cb (list :status 'error :error hook-rejection))
                       (let*
                           ((on-success
                             (macher-agent--build-success-callback
                              ',name-symbol (macher-agent-tool-call-args tool-call) ,success-fn
                              ,output-filter-fn wrap-cb ptc-exec))
                            (cmd-fn ,command-fn)
                            (arity (func-arity cmd-fn)))
                         (if (or (eq (cdr arity) 'many)
                                 (and (numberp (cdr arity)) (>= (cdr arity) 4)))
                             (funcall cmd-fn (macher-agent-tool-call-args tool-call) context root on-success)
                           (let ((action-res (funcall cmd-fn (macher-agent-tool-call-args tool-call) context root)))
                             (if (functionp action-res)
                                 (funcall action-res on-success)
                               (funcall on-success action-res))))))))))
              ,@extra-args)))))

;;; Instruction Queue

(defun macher-agent-add-pending-instruction (instruction)
  "Push INSTRUCTION to the end of the queue for the next network turn.

INSTRUCTION is the directive string to append to the queue.

Return new list of pending instructions.

Side effects: Updates `macher-agent--pending-instructions-queue' buffer-locally."
  (cl-assert (stringp instruction) nil "INSTRUCTION must be a string, got: %S" instruction)
  (setq-local macher-agent--pending-instructions-queue
              (append macher-agent--pending-instructions-queue
                      (list (format "USER OVERRIDE DIRECTIVE:\n%s" instruction)))))

(provide 'macher-agent-tools)
;;; macher-agent-tools.el ends here
