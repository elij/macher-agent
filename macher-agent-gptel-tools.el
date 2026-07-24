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

(cl-defstruct macher-agent-tool-response
  payload
  status
  data
  error
  buffer-name
  ptc-payload)

(defvar macher-agent-ptc-raw-mode nil
  "When non-nil, macher-agent tools bypass string formatting and return raw Lisp objects.")

(defvar macher-agent--active-ptc-execution nil
  "Non-nil when evaluating a Programmatic Tool Calling (PTC) Lisp script.")

(cl-defstruct (macher-agent-process-response (:include macher-agent-tool-response)))
(cl-defstruct (macher-agent-delegate-response (:include macher-agent-tool-response)))
(cl-defstruct (macher-agent-nohup-response (:include macher-agent-tool-response)))
(cl-defstruct (macher-agent-lisp-result-response (:include macher-agent-tool-response)))
(cl-defstruct (macher-agent-ptc-response (:include macher-agent-tool-response)))

(cl-defgeneric macher-agent-execute-response (response context on-success on-error)
  "Execute the action encapsulated by the RESPONSE struct.

RESPONSE is the tool response struct.
CONTEXT is the active agent context.
ON-SUCCESS is the callback called on successful execution.
ON-ERROR is the callback called on execution failure.")

(cl-defmethod macher-agent-execute-response (res _context on-success _on-error)
  "Fallback method for raw Lisp objects, structs, or direct values."
  (if (macher-agent-tool-response-p res)
      (funcall on-success (if (and (bound-and-true-p macher-agent--active-ptc-execution)
                                   (macher-agent-tool-response-ptc-payload res))
                              (macher-agent-tool-response-ptc-payload res)
                            (macher-agent-tool-response-payload res)))
    (funcall on-success res)))

(defvar macher-agent-allowed-tools nil
  "List of custom tool names that should receive the macher-context.")

(defcustom macher-agent-display-subagent-fn nil
  "Function to call with a BUFFER to display it while running.
If nil, the buffer executes silently in the background."
  :type '(choice (const :tag "Silent Background Execution" nil)
                 function)
  :group 'macher-agent)

(defcustom macher-agent-hide-subagent-fn nil
  "Function to call with a BUFFER to hide it once finished."
  :type '(choice (const :tag "Do Nothing" nil)
                 function)
  :group 'macher-agent)

(defvar macher-agent-pre-tool-use-hook nil
  "Hook run before a tool executes.  Called with (tool-name-sym arguments-plist).")

(defvar macher-agent-permission-request-hook nil
  "Hook run for interactive approval.  Called with (tool-name-sym arguments-plist).")

(defvar macher-agent-post-tool-use-hook nil
  "Hook run after a tool completes successfully.  Called with (tool-name-sym arguments-plist output).")

(defvar macher-agent-post-tool-use-failure-hook nil
  "Hook run if a tool execution fails.  Called with (tool-name-sym arguments-plist error-data).")

(defun macher-agent--resolve-context (passed-context)
  "Resolve the current agent context.

PASSED-CONTEXT is the context object passed in, which may be nil.

Return the resolved context structure, or nil."
  (or passed-context
      (ignore-errors (macher-agent-current-context))
      (when-let* ((fsm (or (bound-and-true-p macher-agent--active-fsm)
                           (bound-and-true-p macher--fsm-latest)
                           (bound-and-true-p gptel--fsm-last)))
                  (info (macher-agent--extract-fsm-info fsm)))
        (plist-get info :macher-agent-context))))

(defun macher-agent--format-directives (result-data)
  "Append pending system instructions to RESULT-DATA if any exist.

RESULT-DATA is the string response from a tool execution.

Return the formatted string."
  (let ((final-str (if (stringp result-data) result-data (format "%S" result-data))))
    (when macher-agent--pending-instructions-queue
      (setq final-str (concat final-str "\n\n=== SYSTEM DIRECTIVE ===\n"
                              (string-join (nreverse macher-agent--pending-instructions-queue) "\n")))
      (setq macher-agent--pending-instructions-queue nil))
    final-str))

(defun macher-agent--wrap-callback (gptel-cb)
  "Create a callback wrapper that parses the result and formats directives.

GPTEL-CB is the original gptel callback function, which may be nil.

Return a function callback."
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
  "Internal wrapper to safely trigger the display function.

BUF is the buffer to display.

Return nil."
  (when macher-agent-display-subagent-fn
    (funcall macher-agent-display-subagent-fn buf)))

(defun macher-agent--hide-ui (buf)
  "Internal wrapper to safely trigger the hide function.

BUF is the buffer to hide.

Return nil."
  (when macher-agent-hide-subagent-fn
    (funcall macher-agent-hide-subagent-fn buf)))

(defun macher-agent--insert-hidden (text)
  "Insert TEXT visually hidden via a display overlay, but fully readable by gptel.
This overrides font-lock and prevents `markdown-mode' from revealing the text.

TEXT is the string to insert.

Return nil."
  (let* ((start (point))
         (_ (insert text))
         (ov (make-overlay start (point))))
    (overlay-put ov 'display "")
    (overlay-put ov 'insert-behind-hooks '(ignore))))

(defun macher-agent--extract-prop (obj key)
  "Extract KEY from OBJ robustly handling plists, alists, and hash-tables.
Strips leading colons and converts underscores to hyphens for seamless matching."
  (let* ((key-str (if (keywordp key) (substring (symbol-name key) 1) (format "%s" key)))
         (norm-key (replace-regexp-in-string "_" "-" key-str))
         (res 'macher-missing))
    (cond
     ((hash-table-p obj)
      (maphash (lambda (k v)
                 (let ((k-str (if (keywordp k) (substring (symbol-name k) 1) (format "%s" k))))
                   (when (equal (replace-regexp-in-string "_" "-" k-str) norm-key)
                     (setq res v))))
               obj))
     ((listp obj)
      (let ((tail obj))
        (while (and (eq res 'macher-missing) tail (consp tail))
          (let ((elem (car tail)))
            (if (consp elem)
                (let* ((k (car elem))
                       (k-str (if (keywordp k) (substring (symbol-name k) 1) (format "%s" k))))
                  (when (equal (replace-regexp-in-string "_" "-" k-str) norm-key)
                    (setq res (cdr elem)))
                  (setq tail (cdr tail)))
              (let ((k-str (if (keywordp elem) (substring (symbol-name elem) 1) (format "%s" elem))))
                (when (equal (replace-regexp-in-string "_" "-" k-str) norm-key)
                  (setq res (cadr tail)))
                (setq tail (cddr tail)))))))))
    (if (eq res 'macher-missing) nil res)))

(defun macher-agent--validate-schema (spec value &optional path)
  "Recursively validate VALUE against JSON SPEC. PATH is for error messages."
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
      (let ((is-valid
             (pcase type
               ((or 'string "string")   (stringp value))
               ((or 'number "number")   (numberp value))
               ((or 'integer "integer") (integerp value))
               ((or 'boolean "boolean") (memq value '(t :json-false)))
               ((or 'array "array")      (vectorp value) )
               ((or 'object "object")   (or (listp value) (hash-table-p value)))
               (_ (error "%s" (format "Unknown schema type: %S" type))))))
        (unless is-valid
          (error "%s" (format "The '%s' parameter must be an %s, not %s"
                              name type (type-of value)))))

      (pcase type
        ((or 'array "array")
         (let ((items-spec (plist-get spec :items)))
           (when (and (consp items-spec) (eq (car items-spec) 'quote))
             (setq items-spec (cadr items-spec)))
           (when items-spec
             (cl-loop for item in (append value nil)
                      for idx from 0
                      do (macher-agent--validate-schema
                          items-spec item (format "%s[%d]" name idx))))))
        ((or 'object "object")
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
                             (error "%s" (format "Missing required parameter: '%s.%s'" name prop-name)))))))))))))

(defun macher-agent--extract-payload (tool-args args-spec)
  "Normalise TOOL-ARGS against ARGS-SPEC into a flat keyword plist."
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
  "Validate PAYLOAD against ARGS-SPEC using the internal schema validator."
  (cl-loop for spec in args-spec
           for arg-name = (plist-get spec :name)
           for key = (intern (concat ":" (if (symbolp arg-name)
                                             (symbol-name arg-name)
                                           arg-name)))
           for arg-val = (plist-get payload key)
           do (macher-agent--validate-schema spec arg-val)))

(defun macher-agent--run-pre-hooks (name-sym payload)
  "Execute pre-tool and permission hooks.  Return error string if blocked, else nil."
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

(defmacro macher-agent--with-tool-error-handling (name-sym payload wrap-cb &rest body)
  "Wrap BODY in a condition-case that catches and reports tool execution errors."
  (declare (indent 3))
  `(condition-case err
       (progn ,@body)
     (error
      (run-hook-with-args 'macher-agent-post-tool-use-failure-hook ,name-sym ,payload err)
      (funcall ,wrap-cb (list :status 'error :error (error-message-string err))))))

(defmacro macher-agent--build-success-callback (name-sym payload success-fn output-filter-fn wrap-cb)
  "Generate the success callback lambda for a tool execution."
  `(lambda (res-obj)
     (condition-case cb-err
         (let* ((in-ptc (and (bound-and-true-p macher-agent--active-ptc-execution)
                             (not (eq ',name-sym 'macher-agent-nohup-tool))))
                (has-ptc-payload (and (macher-agent-tool-response-p res-obj)
                                      (macher-agent-tool-response-ptc-payload res-obj)))
                (raw-payload (if (macher-agent-tool-response-p res-obj)
                                 (if in-ptc
                                     (or (macher-agent-tool-response-ptc-payload res-obj)
                                         (macher-agent-tool-response-payload res-obj))
                                   (macher-agent-tool-response-payload res-obj))
                               res-obj))
                (success-data (if (and in-ptc has-ptc-payload)
                                  raw-payload
                                (if ,success-fn (funcall ,success-fn raw-payload) raw-payload)))
                (final-data (if ,output-filter-fn (funcall ,output-filter-fn success-data) success-data)))
           (run-hook-with-args 'macher-agent-post-tool-use-hook ,name-sym ,payload final-data)
           (funcall ,wrap-cb (list :status 'success :data final-data)))
       (error
        (run-hook-with-args 'macher-agent-post-tool-use-failure-hook ,name-sym ,payload cb-err)
        (funcall ,wrap-cb (list :status 'error :error (error-message-string cb-err)))))))

(cl-defmacro macher-agent-make-tool (name-symbol description &key category args command-fn success-fn output-filter-fn)
  "Define a macher-agent tool compatible with gptel's tool framework."
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
										                                            :payload (if (stringp action) action (format "%S" action))
										                                            :ptc-payload action)))
								                                     (on-success (macher-agent--build-success-callback
										                                          ',name-symbol payload ,success-fn ,output-filter-fn wrap-cb)))
								                                (macher-agent-execute-response action-res context on-success wrap-cb))))))))))))

(cl-defmethod macher-agent-execute-response ((res macher-agent-process-response) context on-success on-error)
  "Execute response RES within CONTEXT in a sandbox, then invoke ON-SUCCESS or ON-ERROR."
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
  "Delegate execution of tasks in RES in parallel, then invoke ON-SUCCESS."
  (macher-agent-execute-parallel
   (macher-agent-tool-response-payload res)
   (lambda (sub-agent-results)
     (setf (macher-agent-tool-response-payload res) sub-agent-results)
     (setf (macher-agent-tool-response-ptc-payload res)
           (cl-loop for sub-res across (if (vectorp sub-agent-results) sub-agent-results (vconcat sub-agent-results))
                    collect (list :buffer (macher-agent-tool-response-buffer-name sub-res)
                                  :status (macher-agent-tool-response-status sub-res)
                                  :data (if (eq (macher-agent-tool-response-status sub-res) 'success)
                                            (macher-agent-tool-response-data sub-res)
                                          (macher-agent-tool-response-error sub-res)))))
     (funcall on-success res))))

(cl-defmethod macher-agent-execute-response ((res macher-agent-nohup-response) _context on-success _on-error)
  "Asynchronously run CMD in RES detached, and call ON-SUCCESS immediately."
  (macher-agent--run-async-cmd "detached" (macher-agent-tool-response-payload res) default-directory (lambda (_ _)))
  (setf (macher-agent-tool-response-payload res) "SUCCESS: Process started.")
  (funcall on-success res))

(cl-defmethod macher-agent-execute-response ((res macher-agent-lisp-result-response) _context on-success _on-error)
  "Execute Lisp payload for response RES asynchronously, then invoke ON-SUCCESS."
  (macher-agent-execute-lisp-task
   (macher-agent-tool-response-payload res)
   (lambda (lisp-result)
     (setf (macher-agent-tool-response-payload res) lisp-result)
     (funcall on-success res))))

(defun macher-agent--display-ptc-tool-execution (tool-sym args context)
  "Display mode-line status and run pre-execution hooks for the tool symbol TOOL-SYM.
Returns a cons cell of (ORIGINAL-NAME-STRING . RESOLVED-TOOL)."
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

(defun macher-agent--insert-ptc-tool-result (original-name-str args res-data)
  "Insert collapsible tool result block using the original tool name."
  (when-let* ((str-res (if (stringp res-data) res-data (prin1-to-string res-data)))
              (call-str (prin1-to-string `(:name ,original-name-str :args ,args))))
    (when-let* ((hook-active (bound-and-true-p gptel-post-tool-call-functions)))
      (run-hook-wrapped 'gptel-post-tool-call-functions
                        (lambda (f)
                          (ignore-errors
                            (funcall f (list :name original-name-str :args args :result str-res :buffer (buffer-name) :model (bound-and-true-p gptel-model)))))))
    (when-let* ((include-results (bound-and-true-p gptel-include-tool-results)))
      (save-excursion
        (goto-char (point-max))
        (let* ((inhibit-read-only t)
               (truncated-call (truncate-string-to-width (format "(%s %s)" original-name-str (string-trim (prin1-to-string args) "(" ")")) 60 nil nil " ...)"))
               (start-pt (point)))
          (if-let* (((derived-mode-p 'org-mode)))
              (insert (format "\n#+begin_tool %s\n%s\n\n%s\n#+end_tool\n"
                              truncated-call call-str str-res))
            (insert (format "\n``` tool %s\n%s\n\n%s\n```\n"
                            truncated-call call-str str-res)))
          (goto-char start-pt)
          (forward-line 1)
          (ignore-errors
            (if-let* (((derived-mode-p 'org-mode)))
                (org-cycle)
              (when-let* (((fboundp 'gptel-markdown-cycle-block)))
                (gptel-markdown-cycle-block)))))))))

(defun macher-agent--coerce-ptc-args (args tool)
  "Coerce evaluated Lisp ARGS to strictly match TOOL's JSON schema types.
Dynamically maps parameters from the registry to resolve lists to vectors for arrays,
and alists to flat plists for objects, ensuring native validation passes."
  (if (not (macher-tool-valid-p tool))
      args
    (let ((schema (gptel-tool-args tool))
          (coerced-args nil))
      (cl-labels ((coerce-val (val spec)
                    (if (null spec) val
                      (let ((type (plist-get spec :type)))
                        (cond
                         ((and (member type '(array "array")) (listp val))
                          (vconcat (cl-loop for item in val
                                            collect (coerce-val item (plist-get spec :items)))))
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
                                         append (list k (coerce-val v p-spec)))
                              normalized-val)))
                         (t val))))))
        
        (if (and args (keywordp (car args)))
            (cl-loop for (k v) on args by #'cddr
                     for spec = (cl-find-if (lambda (s)
                                              (let ((n (plist-get s :name)))
                                                (string= (replace-regexp-in-string "_" "-" (if (symbolp n) (symbol-name n) n))
                                                         (substring (symbol-name k) 1))))
                                            schema)
                     do (progn (push k coerced-args)
                               (push (if spec (coerce-val v spec) v) coerced-args)))
          (cl-loop for arg in args
                   for spec in schema
                   do (push (coerce-val arg spec) coerced-args)))
        (nreverse coerced-args)))))

(cl-defmethod macher-agent-execute-response ((res macher-agent-ptc-response) context on-success on-error)
  "Execute the PTC script in RES, dynamically dispatching to host tools with GUI feedback."
  (let ((macher-agent--active-ptc-execution t))
    (if-let* ((script-string (macher-agent-tool-response-payload res))
              (ast (macroexpand-all (read script-string)))
              (target-buf (or (macher-agent-tool-response-buffer-name res) (current-buffer))))
        (with-current-buffer target-buf
          (if-let* ((iterator (macher-agent-sandbox--eval-iter ast nil)))
              (cl-labels ((coerce-val (val spec)
                            (if (null spec) val
                              (let ((type (plist-get spec :type)))
                                (cond
                                 ((and (member type '(array "array")) (listp val))
                                  (vconcat (cl-loop for item in val
                                                    collect (coerce-val item (plist-get spec :items)))))
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
                                                 append (list k (coerce-val v p-spec)))
                                      normalized-val)))
                                 (t val)))))
                          (coerce-ptc-args (args tool)
                            (if (not (macher-tool-valid-p tool))
                                args
                              (let ((schema (gptel-tool-args tool))
                                    (coerced-args nil))
                                (if (and args (keywordp (car args)))
                                    (cl-loop for (k v) on args by #'cddr
                                             for spec = (cl-find-if (lambda (s)
                                                                      (let ((n (plist-get s :name)))
                                                                        (string= (replace-regexp-in-string "_" "-" (if (symbolp n) (symbol-name n) n))
                                                                                 (substring (symbol-name k) 1))))
                                                                    schema)
                                             do (progn (push k coerced-args)
                                                       (push (if spec (coerce-val v spec) v) coerced-args)))
                                  (cl-loop for arg in args
                                           for spec in schema
                                           do (push (coerce-val arg spec) coerced-args)))
                                (nreverse coerced-args)))))
                (letrec
                    ((ptc-resume-loop
                      (lambda (last-result)
                        (with-current-buffer target-buf
                          (when-let* ((_ iterator))
                            (condition-case iter-err
                                (let ((yielded-val (iter-next iterator last-result)))
                                  (if-let*
                                      (((consp yielded-val))
                                       ((eq (plist-get yielded-val :interrupt) 'tool-call))
                                       (tool-lisp-sym (plist-get yielded-val :name))
                                       (args (plist-get yielded-val :args))
                                       (tool-info (macher-agent--display-ptc-tool-execution
                                                   tool-lisp-sym args context))
                                       (original-name-str (car tool-info))
                                       (resolved-tool (cdr tool-info))
                                       (ptc-callback
                                        (lambda (res-data)
                                          (macher-agent--insert-ptc-tool-result
                                           original-name-str args res-data)
                                          (funcall ptc-resume-loop res-data))))
                                      
                                      (if (fboundp tool-lisp-sym)
                                          (let ((lisp-res (apply tool-lisp-sym args)))
                                            (funcall ptc-callback
                                                     (if (macher-agent-tool-response-p lisp-res)
                                                         (or (macher-agent-tool-response-ptc-payload lisp-res)
                                                             (macher-agent-tool-response-payload lisp-res))
                                                       lisp-res)))
                                        (if-let*
                                            (((macher-tool-valid-p resolved-tool))
                                             (tool-fn (gptel-tool-function resolved-tool)))
                                            (let ((coerced-args (coerce-ptc-args args resolved-tool))
                                                  (orig-execute (symbol-function 'macher-agent-execute-response)))
                                              (cl-letf (((symbol-function 'macher-agent-execute-response)
                                                         (lambda (action-res ctx _on-succ _on-err)
                                                           (funcall orig-execute action-res ctx
                                                                    (lambda (res-obj)
                                                                      (funcall ptc-callback
                                                                               (if (macher-agent-tool-response-p res-obj)
                                                                                   (or (macher-agent-tool-response-ptc-payload res-obj)
                                                                                       (macher-agent-tool-response-payload res-obj))
                                                                                 res-obj)))
                                                                    on-error))))
                                                (apply tool-fn (lambda (err-res) (funcall ptc-callback err-res)) coerced-args)))
                                          (setq iterator nil)
                                          (funcall
                                           on-error
                                           (list
                                            :status 'error
                                            :error
                                            (format "Unknown PTC primitive requested: %s" original-name-str)))))
                                    
                                    (progn
                                      (setq iterator nil)
                                      (funcall
                                       on-error
                                       (list
                                        :status 'error
                                        :error
                                        (format "Unexpected yield from PTC sandbox: %S" yielded-val))))))
                              (iter-end-of-sequence
                               (setq iterator nil)
                               (when-let* (((fboundp 'gptel--update-status)))
                                 (gptel--update-status " PTC: Complete" 'success))
                               (funcall on-success (cdr iter-err)))
                              (error
                               (setq iterator nil)
                               (funcall
                                on-error
                                (list :status 'error :error (error-message-string iter-err))))))))))
                  (funcall ptc-resume-loop nil)))
            (funcall
             on-error (list :status 'error :error "Failed to initialise PTC iterator"))))
      (funcall on-error (list :status 'error :error "Invalid PTC script payload")))))

(defun macher-agent--run-async-cmd (name cmd dir callback)
  "Executes a command using explicit lexical capture for background safety.

NAME is the process name string.
CMD is the shell command string to execute.
DIR is the working directory string.
CALLBACK is the function to call upon process exit.

Return the process object."
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
  "Executes COMMAND asynchronously within a dynamically generated VFS sandbox.

CONTEXT is the active context structure.
COMMAND is the shell command string to run.
ON-SUCCESS is the success callback function.
ON-ERROR is the error callback function.

Return nil."
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
  "Read a file, prioritising the uncommitted VFS memory over the physical disk.

FILE-PATH is the string path of the file to read.
CONTEXT is the active context structure.

Return the file contents string, or nil."
  (let* ((vfs-entry (when context (cl-find file-path (macher-context-contents context) :key #'car :test #'equal)))
         (vfs-content (when vfs-entry (if (consp (cdr vfs-entry)) (cddr vfs-entry) (cdr vfs-entry)))))
    (cond
     (vfs-content vfs-content)
     ((file-exists-p file-path)
      (with-temp-buffer (insert-file-contents file-path) (buffer-string)))
     (t nil))))

(defvar-local macher-agent--pending-instructions-queue nil
  "List of instruction strings to append to the tool's return payload.")

(defun macher-agent-add-pending-instruction (instruction)
  "Push an INSTRUCTION directive to steer the LLM after tool execution.

INSTRUCTION is the directive string to push.

Return nil."
  (push instruction macher-agent--pending-instructions-queue))

(defun macher-agent--format-error (err)
  "Standardise the error message string for the LLM.

ERR is the error signal data or object.

Return the formatted error message string."
  (let ((msg (error-message-string err)))
    (if (string-match-p "^\\(ERROR\\|SECURITY ERROR\\):" msg)
        msg
      (format "ERROR: %s" msg))))

(defvar-local macher-agent--final-result nil
  "Stores the clean, synthesised final answer from the sub-agent.")

(defvar gptel-track-media)
(defvar gptel-context)
(declare-function gptel-add-file "gptel")
(declare-function gptel--parse-buffer "gptel")
(declare-function gptel-context-remove "gptel-context" (file))

(defvar macher-agent--pending-tool-media-alist nil
  "Global alist mapping buffers to their pending media.")

(defun macher-agent--gptel-base64-encode-advice (orig-fun file)
  "Read FILE from VFS if available before encoding.
If FILE is the raw base64-encoded media in the active session's pending media,
return it directly without re-encoding.

ORIG-FUN is the original encoding function.
FILE is the string path of the file or base64 data.

Return the base64-encoded representation string."
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
