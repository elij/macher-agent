;;; macher-agent-gptel-tools.el --- Pure gptel orchestration tools -*- lexical-binding: t; -*-

(require 'macher)
(require 'json)
(require 'cl-lib)
(require 'transient)
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
  "Execute the action encapsulated by the RESPONSE struct.")

(cl-defmethod macher-agent-execute-response ((res macher-agent-tool-response) _context on-success _on-error)
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

(defcustom macher-agent-pre-tool-use-hook nil
  "Hook run before any macher-agent tool logic runs.
Each function in this hook is called with the tool name (as a
symbol) and the evaluated arguments (as a plist).
If any function in this hook returns nil or signals an error, the
tool execution is immediately aborted."
  :type 'hook
  :group 'macher-agent)

(defcustom macher-agent-permission-request-hook nil
  "Hook run after `macher-agent-pre-tool-use-hook' but before the main body.
Each function in this hook is called with the tool name (as a
symbol) and the evaluated arguments (as a plist).
This hook is permissive by default; if the hook is empty, the
execution proceeds.  If any function in this hook returns nil or
signals an error, the execution is immediately aborted."
  :type 'hook
  :group 'macher-agent)

(defcustom macher-agent-post-tool-use-hook nil
  "Hook run immediately after the tool's body completes successfully.
Each function in this hook is called with the tool name (as a
symbol), the evaluated arguments (as a plist), and the
resulting output of the tool."
  :type 'hook
  :group 'macher-agent)

(defcustom macher-agent-post-tool-use-failure-hook nil
  "Hook run if the tool's body throws an Emacs Lisp error.
Each function in this hook is called with the tool name (as a
symbol), the evaluated arguments (as a plist), and the error
signal data."
  :type 'hook
  :group 'macher-agent)

(defun macher-agent--resolve-context (passed-context)
  "Resolve the current agent context.

PASSED-CONTEXT is the context object passed in, which may be nil.

Return the resolved context structure, or nil."
  (or passed-context
      (ignore-errors (macher-agent-current-context))
      (when (and (boundp 'macher-agent--active-fsm) macher-agent--active-fsm)
        (let ((info (macher-agent--extract-fsm-info macher-agent--active-fsm)))
          (or (plist-get info :macher-agent-context)
              (plist-get info :macher--context))))))

(defun macher-agent--format-directives (result-data)
  "Append pending system instructions to RESULT-DATA if any exist.

RESULT-DATA is the string response from a tool execution.

Return the formatted string."
  (let ((final-str result-data))
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
This overrides font-lock and prevents markdown-mode from revealing the text.

TEXT is the string to insert.

Return nil."
  (let* ((start (point))
         (_ (insert text))
         (ov (make-overlay start (point))))
    (overlay-put ov 'display "")
    (overlay-put ov 'insert-behind-hooks '(ignore))))

(defun macher-agent--middleware-pipeline (all-args schema next-fn)
  "Extracts context, isolates callbacks, and perfectly aligns the payload payload.

ALL-ARGS is the list of all arguments passed to the tool.
SCHEMA is the tool's parameter schema list.
NEXT-FN is the function to process the parsed payload and context.

Return the result of calling NEXT-FN."
  (let* ((callback (cl-find-if #'functionp all-args))
         (injected-context (cl-find-if (lambda (x) (and (boundp 'macher-context-p) 
                                                        (fboundp 'macher-context-p) 
                                                        (macher-context-p x)))
                                       all-args))
         (raw-tool-args (cl-remove-if (lambda (x)
                                        (or (and callback (eq x callback))
                                            (and injected-context (eq x injected-context)))) 
                                      all-args))
         (aligned-args (last raw-tool-args (length schema)))
         (context (or injected-context (ignore-errors (macher-agent-current-context))))
         (payload nil))

    (cl-loop for arg-def in schema
             for i from 0
             for expected-type = (plist-get arg-def :type)
             for arg-key = (intern (concat ":" (plist-get arg-def :name)))
             for val = (nth i aligned-args)
             for parsed-val = (cond
                               ((and (eq expected-type 'object) (stringp val))
                                (ignore-errors (json-parse-string val :object-type 'plist)))
                               ((and (eq expected-type 'array) (stringp val))
                                (ignore-errors (json-parse-string val :array-type 'vector)))
                               (t val))
             do (setq payload (plist-put payload arg-key (or parsed-val val))))

    (funcall next-fn payload context (or callback (lambda (res) res)))))

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
              :description ,description :category ,(or category "macher-agent") :args ,args :async t
              :function
              (lambda (&rest all-args)
                (macher-agent--middleware-pipeline all-args ,args
                                                   (lambda (payload context callback)
                                                     (let* ((root (if context (macher-agent-context-root context) default-directory))
                                                            (cmd-eval ,command-fn)
                                                            (succ-eval ,success-fn)
                                                            (filter-eval ,output-filter-fn)
                                                            (wrap-cb (macher-agent--wrap-callback callback)))
                                                       (let ((abort-val
                                                              (catch 'tool-abort
                                                                (condition-case err
                                                                    (unless (run-hook-with-args-until-failure 'macher-agent-pre-tool-use-hook ',name-symbol payload)
                                                                      (throw 'tool-abort (list :error "Execution blocked by macher-agent-pre-tool-use-hook")))
                                                                  (error
                                                                   (throw 'tool-abort (list :error (format "Execution blocked by error in macher-agent-pre-tool-use-hook: %s" (error-message-string err))))))
                                                                (condition-case err
                                                                    (unless (run-hook-with-args-until-failure 'macher-agent-permission-request-hook ',name-symbol payload)
                                                                      (throw 'tool-abort (list :error "Permission denied by macher-agent-permission-request-hook")))
                                                                  (error
                                                                   (throw 'tool-abort (list :error (format "Permission denied by error in macher-agent-permission-request-hook: %s" (error-message-string err))))))
                                                                nil)))
                                                         (if abort-val
                                                             (funcall wrap-cb (list :status 'error :error (plist-get abort-val :error)))
                                                           (let* ((on-success
                                                                   (lambda (response-obj)
                                                                     (condition-case err
                                                                         (let* ((raw-payload (if (macher-agent-tool-response-p response-obj)
                                                                                                 (macher-agent-tool-response-payload response-obj)
                                                                                               response-obj))
                                                                                (success-data (if succ-eval (funcall succ-eval raw-payload) raw-payload))
                                                                                (final-data (if filter-eval (funcall filter-eval success-data) success-data)))
                                                                           (run-hook-with-args 'macher-agent-post-tool-use-hook ',name-symbol payload final-data)
                                                                           (funcall wrap-cb (list :status 'success :data final-data)))
                                                                       (error
                                                                        (run-hook-with-args 'macher-agent-post-tool-use-failure-hook ',name-symbol payload err)
                                                                        (funcall wrap-cb (list :status 'error :error (error-message-string err))))))))
                                                             (condition-case err
                                                                 (let* ((action (let* ((arity (func-arity cmd-eval))
                                                                                       (max-args (cdr arity)))
                                                                                  (if (or (eq max-args 'many) (>= max-args 3))
                                                                                      (funcall cmd-eval payload context root)
                                                                                    (funcall cmd-eval payload)))))

                                                                   (unless (macher-agent-tool-response-p action)
                                                                     (error "Tool contract violation: command must return a macher-agent-tool-response struct"))

                                                                   (macher-agent-execute-response action context on-success wrap-cb))
                                                               (error
                                                                (run-hook-with-args 'macher-agent-post-tool-use-failure-hook ',name-symbol payload err)
                                                                (funcall wrap-cb (list :status 'error :error (error-message-string err)))))))))))))))))

(cl-defmethod macher-agent-execute-response ((res macher-agent-process-response) context on-success on-error)
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
  (macher-agent-execute-parallel
   (macher-agent-tool-response-payload res)
   (lambda (sub-agent-results)
     (setf (macher-agent-tool-response-payload res) sub-agent-results)
     (funcall on-success res))))

(cl-defmethod macher-agent-execute-response ((res macher-agent-nohup-response) _context on-success _on-error)
  (macher-agent--run-async-cmd "detached" (macher-agent-tool-response-payload res) default-directory (lambda (_ _)))
  (setf (macher-agent-tool-response-payload res) "SUCCESS: Process started.")
  (funcall on-success res))

(cl-defmethod macher-agent-execute-response ((res macher-agent-lisp-result-response) _context on-success _on-error)
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
  (let* ((vfs-entry (when context (cl-find file-path (macher-context-contents context) :key #'macher-agent-vfs-entry-path :test #'equal)))
         (vfs-content (when vfs-entry (macher-agent-vfs-entry-curr vfs-entry))))
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
  (let* ((fsm (or (and (boundp 'macher-agent--active-fsm) macher-agent--active-fsm)
                  (and (boundp 'macher--fsm-latest) (symbol-value 'macher--fsm-latest))
                  (and (boundp 'gptel--fsm-last) (symbol-value 'gptel--fsm-last))))
         (info (when fsm (ignore-errors (gptel-fsm-info fsm))))
         (session (when info (plist-get info :macher-agent-session)))
         (pending (when session (macher-agent-session-pending-media session))))
    (if (and pending (cl-some (lambda (item) (string= file (car item))) pending))
        file
      (let* ((ctx (ignore-errors (macher-agent-current-context)))
             (workspace (when ctx (macher-context-workspace ctx)))
             (workspace-root (when workspace (macher-agent--get-workspace-root workspace)))
             (actual-name (if (and workspace-root (file-name-absolute-p file))
                              (file-relative-name file workspace-root)
                            file))
             (content (when ctx
                        (condition-case nil
                            (macher-agent--read-context-file ctx actual-name)
                          (error nil)))))
        (if content
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert content)
              (base64-encode-region (point-min) (point-max) :no-line-break)
              (buffer-string))
          (funcall orig-fun file))))))

(advice-add 'gptel--base64-encode :around #'macher-agent--gptel-base64-encode-advice)

(with-eval-after-load 'gptel-transient
  (ignore-errors
    (transient-suffix-put 'gptel-menu 'gptel--infix-tools :save-history nil)
    (transient-suffix-put 'gptel-menu 'gptel--infix-system-message :save-history nil)))

(provide 'macher-agent-gptel-tools)
;;; macher-agent-gptel-tools.el ends here
