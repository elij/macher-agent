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
      (when-let* (((boundp 'macher-agent--active-fsm))
                  (fsm macher-agent--active-fsm)
                  (info (macher-agent--extract-fsm-info fsm)))
        (or (plist-get info :macher-agent-context)
            (plist-get info :macher--context)))))

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

(cl-defmacro macher-agent-make-tool (name-symbol description &key category args command-fn success-fn output-filter-fn)
  "Define a macher-agent tool compatible with gptel's tool framework.

NAME-SYMBOL is the symbol naming the tool variable.
DESCRIPTION is the string describing the tool's purpose.
CATEGORY is the optional classification category of the tool.
ARGS is the list of argument specifications for the tool.
COMMAND-FN is the function running the tool logic.
SUCCESS-FN is the function processing success results.
OUTPUT-FILTER-FN is the optional function filtering tool output."
  (declare (indent 2))
  (let* ((stripped-name (replace-regexp-in-string "^macher-agent-\\|-tool$" "" (symbol-name name-symbol)))
         (name (replace-regexp-in-string "-" "_" stripped-name)))
    `(progn
       (defvar ,name-symbol nil)
       (setq ,name-symbol
             (gptel-make-tool
              :name ,name
              :description ,description :category ,(or category "macher-agent") :args ,args :async t
              :function
              (lambda (&rest all-args)
                (let* ((first-arg (car all-args))
                       (last-arg (car (last all-args)))
                       (is-async-cb-first (and first-arg (functionp first-arg) (not (keywordp first-arg))))
                       (is-async-cb-last (and last-arg (functionp last-arg) (not (keywordp last-arg))))
                       (callback (cond (is-async-cb-first first-arg)
                                       (is-async-cb-last last-arg)
                                       (t nil)))
                       (raw-payload (cond (is-async-cb-first (cdr all-args))
                                          (is-async-cb-last (butlast all-args))
                                          (t all-args)))
                       (payload
                        (if (and raw-payload (keywordp (car raw-payload)))
                            raw-payload
                          (let ((pl nil)
                                (idx 0)
                                (tool-args-spec ,args))
                            (dolist (arg raw-payload)
                              (when-let* ((spec (nth idx tool-args-spec))
                                          (arg-name (plist-get spec :name))
                                          (key-sym (intern (concat ":" (if (symbolp arg-name)
                                                                           (symbol-name arg-name)
                                                                         arg-name)))))
                                (setq pl (plist-put pl key-sym arg)))
                              (setq idx (1+ idx)))
                            pl)))
                       (context (or (ignore-errors (macher-agent-resolve-context))
                                    (bound-and-true-p macher-agent--persistent-context)))
                       (root (if context (macher-agent-context-root context) default-directory))
                       (cmd-eval ,command-fn)
                       (succ-eval ,success-fn)
                       (filter-eval ,output-filter-fn)
                       (wrap-cb (macher-agent--wrap-callback callback)))
                  
                  (let ((pre-ok t)
                        (pre-err nil))
                    (condition-case hook-err
                        (unless (run-hook-with-args-until-failure 'macher-agent-pre-tool-use-hook ',name-symbol payload)
                          (setq pre-ok nil))
                      (error
                       (setq pre-ok nil
                             pre-err (error-message-string hook-err))))
                    (if (not pre-ok)
                        (let ((err-msg (if pre-err
                                           (format "Execution blocked by error in macher-agent-pre-tool-use-hook: %s" pre-err)
                                         "Execution blocked by macher-agent-pre-tool-use-hook")))
                          (funcall wrap-cb (list :status 'error :error err-msg)))
                      
                      (let ((perm-ok t)
                            (perm-err nil))
                        (condition-case hook-err
                            (unless (run-hook-with-args-until-failure 'macher-agent-permission-request-hook ',name-symbol payload)
                              (setq perm-ok nil))
                          (error
                           (setq perm-ok nil
                                 perm-err (error-message-string hook-err))))
                        (if (not perm-ok)
                            (let ((err-msg (if perm-err
                                               (format "Permission denied by error in macher-agent-permission-request-hook: %s" perm-err)
                                             "Permission denied by macher-agent-permission-request-hook")))
                              (funcall wrap-cb (list :status 'error :error err-msg)))
                          
                          (condition-case err
                              (let* ((action (let* ((arity (func-arity cmd-eval))
                                                    (max-args (cdr arity)))
                                               (if (or (eq max-args 'many) (>= max-args 3))
                                                   (funcall cmd-eval payload context root)
                                                 (funcall cmd-eval payload)))))
                                (unless (macher-agent-tool-response-p action)
                                  (setq action (make-macher-agent-lisp-result-response
                                                :payload (if (stringp action) action (format "%S" action)))))
                                (let ((on-success
                                       (lambda (res-obj)
                                         (condition-case cb-err
                                             (let* ((raw-payload (if (macher-agent-tool-response-p res-obj)
                                                                     (macher-agent-tool-response-payload res-obj)
                                                                   res-obj))
                                                    (success-data (if succ-eval (funcall succ-eval raw-payload) raw-payload))
                                                    (final-data (if filter-eval (funcall filter-eval success-data) success-data)))
                                               (run-hook-with-args 'macher-agent-post-tool-use-hook ',name-symbol payload final-data)
                                               (funcall wrap-cb (list :status 'success :data final-data)))
                                           (error
                                            (run-hook-with-args 'macher-agent-post-tool-use-failure-hook ',name-symbol payload cb-err)
                                            (funcall wrap-cb (list :status 'error :error (error-message-string cb-err))))))))
                                  (macher-agent-execute-response action context on-success wrap-cb)))
                            (error
                             (run-hook-with-args 'macher-agent-post-tool-use-failure-hook ',name-symbol payload err)
                             (funcall wrap-cb (list :status 'error :error (error-message-string err))))))))))))))))

(cl-defmethod macher-agent-execute-response ((res macher-agent-process-response) context on-success on-error)
  "Execute RES in a sandbox, then invoke ON-SUCCESS or ON-ERROR."
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
  "Complete execution of RES and call ON-SUCCESS directly."
  (funcall on-success res))

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
            (session (plist-get info :macher-agent-session))
            (pending (macher-agent-session-pending-media session))
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
