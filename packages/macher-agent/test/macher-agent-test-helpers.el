;;; macher-agent-test-helpers.el --- Shared test fixtures and harness for macher-agent -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Shared test harness providing three core fixtures:
;; 1. Git-backed workspace fixture (`macher-agent-test-with-git-workspace`)
;; 2. Controllable LLM transport stub (`macher-agent-test-with-llm-stub`)
;; 3. Asynchronous process barrier (`macher-agent-test-wait-until`)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'buttercup)
(require 'project)
(require 'gptel)
(require 'gptel-openai nil t)
(require 'gptel-anthropic nil t)
(require 'gptel-gemini nil t)

;; Load core modules in dependency order
(defconst macher-agent-test-root-dir
  (let* ((candidates
          (list (and load-file-name
                     (expand-file-name ".." (file-name-directory (file-truename load-file-name))))
                (and buffer-file-name
                     (expand-file-name ".." (file-name-directory (file-truename buffer-file-name))))
                (locate-dominating-file default-directory "macher-agent.el")
                (ignore-errors (file-truename default-directory))
                (and (boundp 'command-line-default-directory)
                     (stringp command-line-default-directory)
                     (not (string-empty-p command-line-default-directory))
                     (or (locate-dominating-file command-line-default-directory "macher-agent.el")
                         (ignore-errors (file-truename command-line-default-directory))))))
         (matched
          (cl-find-if (lambda (d)
                        (and (stringp d)
                             (not (string-empty-p d))
                             (file-exists-p (expand-file-name "macher-agent.el" d))))
                      candidates)))
    (file-name-as-directory (or matched (file-truename "."))))
  "Canonical absolute root directory of the `macher-agent' package under test.")

(let ((root-dir macher-agent-test-root-dir))
  (when (and root-dir (file-directory-p root-dir))
    (push root-dir load-path)
    (dolist (rel '("gptel" "../../gptel"))
      (let ((gptel-dir (expand-file-name rel root-dir)))
        (when (file-directory-p gptel-dir)
          (add-to-list 'load-path (file-name-as-directory gptel-dir)))))
    (let ((skills-dir (expand-file-name "skills/scripts" root-dir)))
      (when (file-directory-p skills-dir)
        (add-to-list 'load-path (file-name-as-directory skills-dir))))
    (add-to-list 'load-path (file-name-as-directory (expand-file-name "test" root-dir)))))

(require 'macher-agent-core)
(require 'macher-agent-presets)
(require 'macher-agent-gptel)
(require 'macher-agent-tools)
(require 'macher-agent-orchestration)
(require 'macher-agent-api)
(require 'macher-agent)

;; Ensure all skill scripts are loaded so their tool definitions are available
(let ((scripts-dir (expand-file-name "skills/scripts" macher-agent-test-root-dir)))
  (when (file-directory-p scripts-dir)
    (dolist (script (directory-files scripts-dir t "\\.el$"))
      (load script nil t))))

;;; ----------------------------------------------------------------------------
;;; Global Test State Reset Utility
;;; ----------------------------------------------------------------------------

(defun macher-agent-test-reset-env ()
  "Reset all global registries, callbacks, and hooks to clean baseline."
  (unless (and default-directory (file-directory-p default-directory))
    (setq default-directory (file-name-as-directory (file-truename temporary-file-directory))))
  (when (and (boundp 'macher-agent-active-workspaces) (hash-table-p macher-agent-active-workspaces))
    (clrhash macher-agent-active-workspaces))
  (when (and (boundp 'macher-agent-pending-callbacks) (hash-table-p macher-agent-pending-callbacks))
    (clrhash macher-agent-pending-callbacks))
  (when (and (boundp 'macher-agent-task-registry) (hash-table-p macher-agent-task-registry))
    (clrhash macher-agent-task-registry))
  (when (and (boundp 'macher-agent-a2a-ownership) (hash-table-p macher-agent-a2a-ownership))
    (clrhash macher-agent-a2a-ownership))
  (when (and (boundp 'macher-agent-task-callbacks) (hash-table-p macher-agent-task-callbacks))
    (clrhash macher-agent-task-callbacks))
  (when (and (boundp 'macher-agent-wake-callbacks) (hash-table-p macher-agent-wake-callbacks))
    (clrhash macher-agent-wake-callbacks))
  (when (and (boundp 'macher-agent-vfs-callbacks) (hash-table-p macher-agent-vfs-callbacks))
    (clrhash macher-agent-vfs-callbacks))
  (setq macher-agent-active-subagents nil)
  (setq macher-agent-task-flush-hook nil)
  (setq macher-agent-context-mutated-hook nil)
  (setq macher-agent-search-backend-function #'macher-agent-search-glob)
  (setq macher-agent-display-subagent-fn nil)
  (when (fboundp 'macher-agent-install)
    (macher-agent-install)))

;;; ----------------------------------------------------------------------------
;;; 1. Git-Backed Workspace Fixture
;;; ----------------------------------------------------------------------------

(defun macher-agent-test-create-git-repo (&optional initial-files)
  "Create a temporary git repository directory with INITIAL-FILES committed.
INITIAL-FILES is an alist of (REL-PATH . CONTENT).
Returns the canonical directory path string."
  (let* ((temp-dir (make-temp-file "macher-git-ws-" t))
         (canonical (file-name-as-directory (file-truename temp-dir)))
         (default-directory canonical))
    ;; Initialise git repository
    (call-process "git" nil nil nil "-C" canonical "init" "-q")
    (call-process "git" nil nil nil "-C" canonical "config" "user.name" "Macher Test")
    (call-process "git" nil nil nil "-C" canonical "config" "user.email" "test@macher.local")
    (call-process "git" nil nil nil "-C" canonical "config" "commit.gpgsign" "false")
    ;; Create initial files
    (let ((files (or initial-files '(("README.md" . "# Workspace Test\nInitial commit.\n")))))
      (dolist (f files)
        (let* ((rel-path (car f))
               (content (cdr f))
               (full-path (expand-file-name rel-path canonical)))
          (make-directory (file-name-directory full-path) t)
          (with-temp-file full-path
            (insert content)))))
    (call-process "git" nil nil nil "-C" canonical "add" ".")
    (call-process "git" nil nil nil "-C" canonical "commit" "-q" "-m" "Initial commit")
    canonical))

(defmacro macher-agent-test-with-git-workspace (bindings &rest body)
  "Create a Git-backed workspace fixture using BINDINGS and BODY.
BINDINGS is (ROOT-VAR CTX-VAR OWNER-BUF-VAR &optional INITIAL-FILES).
Cleans up temporary directory, child buffers, processes, and timers."
  (declare (indent 1) (debug t))
  (let ((root-sym (nth 0 bindings))
        (ctx-sym (nth 1 bindings))
        (buf-sym (nth 2 bindings))
        (init-files (nth 3 bindings))
        (pre-bufs (gensym "pre-bufs-"))
        (pre-procs (gensym "pre-procs-"))
        (pre-timers (gensym "pre-timers-"))
        (dir-sym (gensym "dir-")))
    `(let* ((,pre-bufs (buffer-list))
            (,pre-procs (process-list))
            (,pre-timers (when (boundp 'timer-list) (copy-sequence timer-list)))
            (,dir-sym (macher-agent-test-create-git-repo ,init-files))
            (,root-sym ,dir-sym)
            (,buf-sym (generate-new-buffer "*macher-test-owner*"))
            (,ctx-sym nil))
       (unwind-protect
           (progn
             (with-current-buffer ,buf-sym
               (setq-local default-directory ,root-sym)
               (setq ,ctx-sym (macher-agent-make-context
                               :project-root ,root-sym
                               :origin-buffer ,buf-sym
                               :tools (make-hash-table :test 'equal)
                               :plugins (list :workspace (if (fboundp 'macher-agent-make-workspace)
                                                             (macher-agent-make-workspace :project-root ,root-sym)
                                                           (cons 'project ,root-sym)))))
               (setf (macher-agent-context-owner-buffer ,ctx-sym) ,buf-sym)
               (setq-local macher-agent-persistent-context ,ctx-sym)
               (setq-local macher-agent-is-workspace t)
               (puthash ,root-sym ,ctx-sym macher-agent-active-workspaces)
               (puthash (directory-file-name ,root-sym) ,ctx-sym macher-agent-active-workspaces))
             ,@body)
         ;; Cleanup child buffers created during the test
         (dolist (b (buffer-list))
           (unless (memq b ,pre-bufs)
             (when (buffer-live-p b)
               (kill-buffer b))))
         ;; Cleanup processes
         (dolist (p (process-list))
           (unless (memq p ,pre-procs)
             (ignore-errors (delete-process p))))
         ;; Cleanup timers
         (when (boundp 'timer-list)
           (dolist (tm (copy-sequence timer-list))
             (unless (memq tm ,pre-timers)
               (ignore-errors (cancel-timer tm)))))
         ;; Cleanup active workspaces registry
         (when (and (boundp 'macher-agent-active-workspaces) (hash-table-p macher-agent-active-workspaces))
           (remhash ,root-sym macher-agent-active-workspaces)
           (remhash (directory-file-name ,root-sym) macher-agent-active-workspaces))
         ;; Reset default-directory before deleting temporary workspace
         (setq default-directory (file-name-as-directory (file-truename temporary-file-directory)))
         ;; Delete temporary git repository
         (when (and ,dir-sym (file-directory-p ,dir-sym))
           (ignore-errors (delete-directory ,dir-sym t)))))))

;;; ----------------------------------------------------------------------------
;;; 2. Controllable LLM Transport Stub
;;; ----------------------------------------------------------------------------

(defvar macher-agent-test-llm-scripted-turns nil
  "List of scripted model turns for `macher-agent-test-with-llm-stub'.
Each element can be:
- `(:text \"Response text\")'
- `(:tool-call \"tool_name\" (:param \"val\"))'
- `(:tool-use ((\"tool_name\" val1 val2 ...)))'
- `(:error \"Simulated error message\")'")

(defvar macher-agent-test-llm-turn-history nil
  "Chronological record of model turn interactions during test execution.")

(defmacro macher-agent-test-with-llm-stub (turns &rest body)
  "Stub `gptel-send' with TURNS and run gptel hooks around BODY.
Fires `gptel-prompt-transform-functions', `gptel-pre-tool-call-functions',
and `gptel-post-response-functions'."
  (declare (indent 1) (debug t))
  `(let* ((macher-agent-test-llm-scripted-turns (copy-tree ,turns))
          (macher-agent-test-llm-turn-history nil)
          (orig-gptel-send (symbol-function 'gptel-send)))
     (cl-letf (((symbol-function 'gptel-send)
                (lambda (&rest args)
                  (let* ((orig-buf (current-buffer))
                         (fsm (or (bound-and-true-p macher-agent-active-fsm)
                                  (macher-agent-get-active-fsm)))
                         (next-turn (pop macher-agent-test-llm-scripted-turns)))
                    (push (list :turn next-turn :buffer orig-buf :args args)
                          macher-agent-test-llm-turn-history)
                    ;; 1. Fire gptel-prompt-transform-functions in sequence
                    (let ((prompt-text (buffer-string)))
                      (dolist (transformer gptel-prompt-transform-functions)
                        (condition-case nil
                            (cond
                             ((= (cdr (func-arity transformer)) 1)
                              (funcall transformer fsm))
                             ((>= (cdr (func-arity transformer)) 2)
                              (funcall transformer nil fsm))
                             (t (funcall transformer)))
                          (error nil))))
                    (cond
                     ;; Turn simulates network/API error
                     ((and next-turn (plist-get next-turn :error))
                      (let ((err-msg (plist-get next-turn :error)))
                        (when fsm
                          (setf (gptel-fsm-info fsm)
                                (plist-put (gptel-fsm-info fsm) :error err-msg))
                          (when (fboundp 'gptel--handle-error)
                            (ignore-errors (gptel--handle-error fsm))))
                        (error "%s" err-msg)))

                     ;; Turn simulates tool call
                     ((and next-turn (or (plist-get next-turn :tool-call)
                                         (plist-get next-turn :tool-use)))
                      (let* ((tool-name (or (plist-get next-turn :tool-call)
                                            (caar (plist-get next-turn :tool-use))))
                             (tool-args (or (plist-get next-turn :args)
                                            (cdar (plist-get next-turn :tool-use))))
                             (tool-obj (or (ignore-errors (gptel-get-tool tool-name))
                                           (cl-find-if (lambda (t-obj)
                                                         (equal (gptel-tool-name t-obj) tool-name))
                                                       (bound-and-true-p gptel-tools))))
                             (blocked nil))
                        ;; 2. Fire gptel-pre-tool-call-functions
                        (dolist (hook gptel-pre-tool-call-functions)
                          (let ((res (condition-case nil
                                         (funcall hook (or tool-obj tool-name) fsm)
                                       (error nil))))
                            (when (and (consp res) (eq (car res) :block))
                              (setq blocked res))))
                        (if blocked
                            (error "%s" (cadr blocked))
                          ;; Execute tool if available
                          (when tool-obj
                            (let ((fn (gptel-tool-function tool-obj)))
                              (when fn
                                (condition-case nil
                                    (apply fn (lambda (_res) nil) tool-args)
                                  (error nil))))))))

                     ;; Turn returns text
                     ((and next-turn (plist-get next-turn :text))
                      (let* ((text (plist-get next-turn :text))
                             (beg (point-max)))
                        (save-excursion
                          (goto-char (point-max))
                          (insert text))
                        (let ((end (point-max)))
                          ;; 3. Fire gptel-post-response-functions
                          (dolist (hook gptel-post-response-functions)
                            (ignore-errors (funcall hook beg end))))))

                     (t nil))))))
       ,@body)))

;;; ----------------------------------------------------------------------------
;;; 3. Asynchronous Process Barrier
;;; ----------------------------------------------------------------------------

(defun macher-agent-test-wait-until (predicate &optional timeout message)
  "Poll PREDICATE until non-nil or TIMEOUT expires.
TIMEOUT defaults to 5.0 seconds.  Signals an error with MESSAGE on timeout."
  (let ((deadline (+ (float-time) (or timeout 5.0)))
        (res nil))
    (while (and (not (setq res (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (sit-for 0.02))
    (unless res
      (error "%s" (or message "Timed out in macher-agent-test-wait-until")))
    res))

(defun macher-agent-test-invoke-tool (tool &rest args)
  "Invoke gptel TOOL `:function' with ARGS and return its result string.
Supports both `:async t' and synchronous `gptel-tool' structs."
  (let ((fn (gptel-tool-function tool)))
    (if (gptel-tool-async tool)
        (let ((captured nil)
              (called nil))
          (apply fn (lambda (res) (setq captured res called t)) args)
          (if called captured nil))
      (apply fn args))))

(defmacro macher-agent-test-with-clean-gptel-registries (&rest body)
  "Execute BODY and restore default gptel tool and preset registries."
  (declare (indent 0) (debug t))
  (let ((orig-tools (gensym "orig-tools-"))
        (orig-presets (gensym "orig-presets-")))
    `(let ((,orig-tools (copy-tree (default-value 'gptel--known-tools)))
           (,orig-presets (copy-alist (default-value 'gptel--known-presets))))
       (unwind-protect
           (progn ,@body)
         (set-default 'gptel--known-tools ,orig-tools)
         (set-default 'gptel--known-presets ,orig-presets)))))

(defun macher-agent-test-make-unregistered-tool (&rest slots)
  "Create a `gptel-tool' struct from SLOTS without modifying `gptel--known-tools'."
  (let ((gptel--known-tools nil))
    (apply #'gptel-make-tool slots)))

(defun macher-agent-test-remove-known-tool (category name)
  "Remove tool NAME from CATEGORY in the active `gptel--known-tools' alist."
  (when-let* ((cat (assoc category gptel--known-tools)))
    (setcdr cat (assoc-delete-all name (cdr cat)))))

(defun macher-agent-test-drop-local-tool-category (category)
  "Buffer-locally remove CATEGORY from `gptel--known-tools'."
  (setq-local gptel--known-tools
              (assoc-delete-all category (copy-tree gptel--known-tools))))

(defun macher-agent-test-localize-known-presets ()
  "Make a buffer-local shallow copy of the default `gptel--known-presets'."
  (setq-local gptel--known-presets
              (copy-alist (default-value 'gptel--known-presets))))

(provide 'macher-agent-test-helpers)
;;; macher-agent-test-helpers.el ends here
