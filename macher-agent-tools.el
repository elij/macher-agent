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
(require 'macher-agent-presets)
(require 'macher-agent-orchestration)

;;; Customisation Variables

(put 'macher-agent--pending-instructions-queue 'permanent-local t)

(defvar-local macher-agent--final-result nil
  "Store the synthesised final answer from the sub-agent.

Return final result string or object.
Side effects: None.")

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
       for child-val = (macher-agent--extract-prop value k)
       do (if (not (eq child-val 'macher-missing))
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
                 (val (if arg-name (macher-agent--extract-prop raw-plist arg-name) 'macher-missing)))
            (unless (eq val 'macher-missing)
              (let* ((name-str (if (symbolp arg-name) (symbol-name arg-name) (format "%s" arg-name)))
                     (clean-name (if (string-prefix-p ":" name-str) (substring name-str 1) name-str))
                     (under-name (replace-regexp-in-string "-" "_" clean-name))
                     (hyphen-name (replace-regexp-in-string "_" "-" clean-name))
                     (under-key (intern (concat ":" under-name)))
                     (hyphen-key (intern (concat ":" hyphen-name))))
                (setq result (plist-put result under-key val))
                (setq result (plist-put result hyphen-key val))))))
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
           for val = (if arg-name (macher-agent--extract-prop payload arg-name) 'macher-missing)
           for arg-val = (if (eq val 'macher-missing) nil val)
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
                       ((context (ignore-errors (macher-agent-resolve-context)))
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

;;; Sandbox Execution

(defun macher-agent--run-in-persistent-sandbox (context command on-success on-error)
  "Execute COMMAND asynchronously within dynamically generated VFS sandbox.

CONTEXT is the active agent context structure.
COMMAND is the shell command string to execute.
ON-SUCCESS is the success callback function.
ON-ERROR is the error callback function.

Return nil.
Side effects: Creates temporary sandbox directory and spawns process."
  (let* ((workspace-root (if (and context (fboundp 'macher-agent-context-root))
                             (macher-agent-context-root context)
                           default-directory))
         (sandbox-dir (make-temp-file "macher-sandbox-" t))
         (contents (when (and context (fboundp 'macher-agent--get-context-contents))
                     (macher-agent--get-context-contents context))))
    (condition-case err
        (progn
          (when (fboundp 'macher-agent--vfs-verify-clean-merge)
            (macher-agent--vfs-verify-clean-merge workspace-root contents))
          (when (fboundp 'macher-agent--vfs-sync-baseline)
            (macher-agent--vfs-sync-baseline workspace-root sandbox-dir))
          (when (and contents (fboundp 'macher-agent--vfs-apply-overlay-stateless))
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
  (let* ((contents (when (and context (fboundp 'macher-agent--get-context-contents))
                     (macher-agent--get-context-contents context)))
         (workspace-root (or (and context (fboundp 'macher-agent-context-root)
                                  (macher-agent-context-root context))
                             default-directory))
         (relative-path (if (fboundp 'macher-agent-to-relative-path)
                            (macher-agent-to-relative-path file-path workspace-root)
                          (if (file-name-absolute-p file-path)
                              (file-relative-name file-path workspace-root)
                            file-path)))
         (safe-path (if (fboundp 'macher-agent--resolve-safe-path)
                        (macher-agent--resolve-safe-path relative-path workspace-root)
                      (expand-file-name relative-path workspace-root)))
         (norm-path (if (and context (fboundp 'macher-agent--normalize-path-key))
                        (ignore-errors (macher-agent--normalize-path-key file-path context))
                      file-path))
         (vfs-entry
          (when contents
            (or (cl-find file-path contents :key #'car :test #'equal)
                (when norm-path (cl-find norm-path contents :key #'car :test #'equal))
                (when safe-path (cl-find safe-path contents :key #'car :test #'equal)))))
         (vfs-content (when vfs-entry (if (consp (cdr vfs-entry)) (cddr vfs-entry) (cdr vfs-entry)))))
    (when (and context (fboundp 'macher-agent--ensure-access-stateless))
      (macher-agent--ensure-access-stateless contents file-path))
    (cond
     (vfs-content vfs-content)
     ((and safe-path (file-exists-p safe-path))
      (with-temp-buffer (insert-file-contents safe-path) (buffer-string)))
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
  (if-let* ((fsm (macher-agent-get-active-fsm))
            (info (ignore-errors (macher-agent--extract-fsm-info fsm)))
            (ctx (or (plist-get info :macher-agent-context)
                     (plist-get info :macher-context)
                     (plist-get info :macher--context)))
            (pending (when ctx
                       (macher-agent--get-context-data ctx :pending-media)))
            ((cl-some (lambda (item) (string= file (car item))) pending)))
      file
    (if-let* ((ctx (ignore-errors (macher-agent-resolve-context)))
              (workspace (when ctx
                           (macher-agent--get-context-workspace ctx)))
              (workspace-root (when workspace
                                (macher-agent-workspace-project-root workspace)))
              (actual-name (if workspace-root
                               (if (fboundp 'macher-agent-to-relative-path)
                                   (macher-agent-to-relative-path file workspace-root)
                                 (if (file-name-absolute-p file)
                                     (file-relative-name file workspace-root)
                                   file))
                             file))
              (content (when (and ctx (fboundp 'macher-agent--read-context-file))
                         (ignore-errors (macher-agent--read-context-file ctx actual-name)))))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert content)
          (base64-encode-region (point-min) (point-max) :no-line-break)
          (buffer-string))
      (funcall orig-fun file))))

;;; Memory Tools

(defvar macher-agent-search-backend-function #'macher-agent-search-glob
  "Function variable used to dispatch conversation history searches.
Defaults to the native `macher-agent-search-glob` but can be dynamically
overridden by memory plugins.")

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

(defun macher-agent-search-dispatch (keywords orig-buf &optional ctx-lines)
  "Dispatch search for KEYWORDS in ORIG-BUF with CTX-LINES context."
  (if (not (buffer-live-p orig-buf))
      "Error: Cannot locate original conversation buffer."
    (funcall macher-agent-search-backend-function keywords orig-buf ctx-lines)))

(provide 'macher-agent-tools)
;;; macher-agent-tools.el ends here
