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
  buffer-name)

(cl-defstruct (macher-agent-process-response (:include macher-agent-tool-response)))
(cl-defstruct (macher-agent-delegate-response (:include macher-agent-tool-response)))
(cl-defstruct (macher-agent-nohup-response (:include macher-agent-tool-response)))
(cl-defstruct (macher-agent-lisp-result-response (:include macher-agent-tool-response)))

(cl-defgeneric macher-agent-execute-response (response context on-success on-error)
  "Execute the action encapsulated by the RESPONSE struct.

RESPONSE is the tool response struct.
CONTEXT is the active agent context.
ON-SUCCESS is the callback called on successful execution.
ON-ERROR is the callback called on execution failure.")

(cl-defmethod macher-agent-execute-response ((res macher-agent-tool-response) _context on-success _on-error)
  "Execute response RES, calling ON-SUCCESS with the payload."
  (funcall on-success (macher-agent-tool-response-payload res)))

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
               ((or 'array "array")     (vectorp value))
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
             (cl-loop for item across value
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
           ;; Convert JSON parsed arrays (vectors) into lists for `member` compatibility
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
                           ;; JSON Schema leniency: ignore missing properties unless explicitly required
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
         (let* ((raw-payload (if (macher-agent-tool-response-p res-obj)
                                 (macher-agent-tool-response-payload res-obj)
                               res-obj))
                (success-data (if ,success-fn (funcall ,success-fn raw-payload) raw-payload))
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
                                              :payload (if (stringp action) action (format "%S" action)))))
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
