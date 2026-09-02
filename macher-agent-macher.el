;;; macher-agent-macher.el --- Bridge to Macher Core  -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridge implementation for Macher Agent to interact with Macher Core.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'macher-agent-core)

(defvar gptel-directives)
(defvar macher-workspace-types-alist)
(defvar macher-workspace-functions)
(defvar macher--workspace)

(declare-function project-root "project" (project))
(declare-function macher--workspace-hash "macher" (workspace &optional len))
(declare-function macher--workspace-name "macher" (workspace))
(declare-function macher--build-patch "macher" (context &optional fsm))
(declare-function macher--make-context "macher" (&rest args))
(declare-function make-macher-context "macher" (&rest args))
(declare-function macher-context-p "macher" (obj))
(declare-function macher-context-workspace "macher" (ctx))
(declare-function macher-context-contents "macher" (ctx))
(declare-function macher-context-dirty-p "macher" (ctx))
(declare-function macher-context-shadow-buffers "macher" (ctx))
(declare-function macher-context-data "macher" (ctx))
(declare-function macher-context-prompt "macher" (ctx))
(declare-function macher-patch-buffer "macher" (workspace))

(defun macher-agent--unwrap-workspace (ws)
  "Unwrap the raw properties from WS context struct."
  (cl-check-type ws macher-agent-context)
  (or (macher-agent-context-project-root ws)
      (macher-agent-context-root ws)))

(defun macher-agent-macher-safe-workspace-hash (context &optional length)
  "Generate a safe hash strictly from the CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (let* ((ws-id (or (when (fboundp 'macher-agent-context-project-root)
                      (macher-agent-context-project-root context))
                    (when (fboundp 'macher-agent-context-root)
                      (macher-agent-context-root context))
                    (when (fboundp 'macher-agent-context-workspace-root)
                      (macher-agent-context-workspace-root context))
                    ""))
         (hash-input (secure-hash 'sha256 (concat "project" ws-id)))
         (chars "abcdefghijklmnopqrstuvwxyz0123456789")
         (hash-length (or length 16))
         (result ""))
    (dotimes (i hash-length)
      (let* ((hex-char (aref hash-input i))
             (idx (mod (if (>= hex-char ?a) (- hex-char ?a -10) (- hex-char ?0)) (length chars))))
        (setq result (concat result (substring chars idx (1+ idx))))))
    result))

(defun macher-agent-macher-workspace-hash (workspace &optional length)
  "Generate a standard hash for a wrapped WORKSPACE list."
  (cl-check-type workspace list)
  (let* ((len (or length 16))
         (str (or (when (fboundp 'macher--workspace-hash)
                    (condition-case nil
                        (let ((res (macher--workspace-hash workspace len)))
                          (if (stringp res)
                              (substring res 0 (min (length res) len))
                            res))
                      (error nil)))
                  (md5 (format "%s" workspace)))))
    (if (> len 0)
        (substring str 0 (min (length str) len))
      str)))

(defun macher-agent-macher-workspace-name (context)
  "Extract a concise project name strictly from the CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (let ((root (or (when (fboundp 'macher-agent-context-project-root)
                    (macher-agent-context-project-root context))
                  (when (fboundp 'macher-agent-context-root)
                    (macher-agent-context-root context))
                  (when (fboundp 'macher-agent-context-workspace-root)
                    (macher-agent-context-workspace-root context)))))
    (if root
        (file-name-nondirectory (directory-file-name root))
      "workspace")))

(defun macher-agent-vfs-entry-to-macher-entry (entry &optional root)
  "Convert VFS ENTRY to a Macher content cell (rel-path . (orig . curr))."
  (cond
   ((and (fboundp 'macher-agent-vfs-entry-p) (macher-agent-vfs-entry-p entry))
    (let* ((path (macher-agent-vfs-entry-path entry))
           (orig (macher-agent-vfs-entry-orig entry))
           (curr (macher-agent-vfs-entry-curr entry))
           (rel-path (if (and (stringp path) root)
                         (file-relative-name (file-truename (expand-file-name path root))
                                             (file-name-as-directory root))
                       path)))
      (cons rel-path (cons orig curr))))
   ((consp entry)
    (let* ((path (car entry))
           (rest (cdr entry))
           (rel-path (if (and (stringp path) root)
                         (file-relative-name (file-truename (expand-file-name path root))
                                             (file-name-as-directory root))
                       path)))
      (cons rel-path rest)))
   (t entry)))

(defun macher-agent-vfs-modified-files (vfs)
  "Return list of modified files in VFS."
  (cond
   ((and (fboundp 'macher-agent-context-p) (macher-agent-context-p vfs))
    (macher-agent--get-context-contents vfs))
   ((listp vfs)
    (if (plist-member vfs :contents)
        (plist-get vfs :contents)
      vfs))
   (t nil)))

(defun macher-agent-macher-patch-buffer (ws-cons)
  "Return the patch buffer for upstream WS-CONS."
  (cl-check-type ws-cons cons)
  (if (fboundp 'macher-patch-buffer)
      (macher-patch-buffer ws-cons t)
    (get-buffer "*macher-patch*")))

(defun macher-agent-macher-build-patch (ctx prompt &optional files)
  "Build patch buffer via upstream macher using CTX."
  (cl-check-type ctx macher-agent-context)
  (let* ((raw-root (or (when (fboundp 'macher-agent-context-project-root)
                         (macher-agent-context-project-root ctx))
                       (when (fboundp 'macher-agent-context-root)
                         (macher-agent-context-root ctx))
                       (when (fboundp 'macher-agent-context-workspace-root)
                         (macher-agent-context-workspace-root ctx))))
         (canonical-root (when raw-root
                           (file-truename (expand-file-name raw-root))))
         (ws (cons 'project canonical-root))
         (vfs-entries (or files (macher-agent-vfs-modified-files ctx)))
         (mapped-contents (mapcar (lambda (entry)
                                    (macher-agent-vfs-entry-to-macher-entry entry canonical-root))
                                  vfs-entries))
         (ephemeral-ctx (if (fboundp 'macher--make-context)
                            (macher--make-context
                             :contents mapped-contents
                             :workspace ws
                             :prompt prompt
                             :dirty-p t)
                          (when (fboundp 'make-macher-context)
                            (make-macher-context
                             :contents mapped-contents
                             :workspace ws
                             :prompt prompt
                             :dirty-p t))))
         (patch-result (when (fboundp 'macher--build-patch)
                         (let ((default-directory (if canonical-root
                                                      (file-name-as-directory canonical-root)
                                                    default-directory)))
                           (macher--build-patch ephemeral-ctx nil)))))
    (or patch-result
        (macher-agent-macher-patch-buffer ws))))

(defun macher-agent-macher-install ()
  "Install Macher Core integration, upstream alias, and workspace hooks.

Configures `macher--workspace-hash' alias to use safe workspace hashing,
registers base agent workspace handlers in `macher-workspace-types-alist',
and registers `macher-agent-workspace-agent' in `macher-workspace-functions'.

Side effects: Modifies global Macher tables, hooks, and symbol definitions."
  (when (boundp 'macher-workspace-types-alist)
    (let* ((existing (alist-get 'agent macher-workspace-types-alist))
           (merged (append existing '(:get-root macher-agent-workspace-project-root :get-name macher-agent--get-name))))
      (unless (plist-get merged :get-files)
        (setq merged (plist-put merged :get-files 'macher-agent--collect-raw-files)))
      (setf (alist-get 'agent macher-workspace-types-alist) merged)))
  (when (boundp 'macher-workspace-functions)
    (add-hook 'macher-workspace-functions #'macher-agent-workspace-agent))
  (with-eval-after-load 'macher
    (when (boundp 'macher-workspace-types-alist)
      (let* ((existing (alist-get 'agent macher-workspace-types-alist))
             (merged (append existing '(:get-root macher-agent-workspace-project-root :get-name macher-agent--get-name))))
        (unless (plist-get merged :get-files)
          (setq merged (plist-put merged :get-files 'macher-agent--collect-raw-files)))
        (setf (alist-get 'agent macher-workspace-types-alist) merged)))
    (when (boundp 'macher-workspace-functions)
      (add-hook 'macher-workspace-functions #'macher-agent-workspace-agent))))

(defun macher-agent--make-vfs-context (workspace &rest rest)
  "Generate a new VFS context tree explicitly for WORKSPACE list."
  (let* ((ws (cond
              ((eq workspace :workspace) (car rest))
              ((and (listp workspace) (plist-member workspace :workspace))
               (plist-get workspace :workspace))
              (t workspace)))
         (contents (cond
                    ((plist-member rest :contents) (plist-get rest :contents))
                    ((and (listp workspace) (plist-member workspace :contents)) (plist-get workspace :contents))
                    (t nil)))
         (prompt (cond
                  ((plist-member rest :prompt) (plist-get rest :prompt))
                  ((and (listp workspace) (plist-member workspace :prompt)) (plist-get workspace :prompt))
                  (t nil))))
    (cl-check-type ws list)
    (let* ((root (cond
                  ((consp ws) (if (stringp (cdr ws)) (expand-file-name (cdr ws)) (plist-get ws :project-root)))
                  ((plist-member ws :project-root) (plist-get ws :project-root))
                  (t default-directory)))
           (root (if (stringp root) (expand-file-name root) (expand-file-name default-directory)))
           (vfs-state (list :contents (or contents nil) :dirty-p nil))
           (plugins (list :vfs vfs-state :workspace ws))
           (ctx (macher-agent--make-context :project-root root :plugins plugins)))
      (when (and ctx prompt)
        (setf (macher-agent-context-prompt ctx) prompt))

      (when (and (macher-agent-context-p ctx)
                 (or (fboundp 'macher--make-context) (fboundp 'make-macher-context)))
        (let ((proxy (let ((macher-agent--persistent-context nil))
                       (if (fboundp 'macher--make-context)
                           (funcall 'macher--make-context
                                    :workspace ws
                                    :contents contents
                                    :prompt prompt)
                         (funcall 'make-macher-context
                                  :workspace ws
                                  :contents contents
                                  :prompt prompt)))))
          (setf (macher-agent-context-plugins ctx)
                (plist-put (copy-sequence (macher-agent-context-plugins ctx)) :upstream-proxy proxy))))
      ctx)))

(defun macher-agent--get-active-workspace ()
  "Retrieve the globally active workspace.

Return the unwrapped workspace structure, or nil if unbound."
  (let ((ws (bound-and-true-p macher--workspace)))
    (macher-agent--unwrap-workspace ws)))

(defun macher-agent-trigger-patch (&optional ctx)
  "Retrieve the underlying patch buffer for CTX.

Resolve the patch buffer associated with the workspace of CTX,
falling back to the globally active workspace if CTX is nil.

Return the live patch buffer, or nil."
  (let* ((ws (if ctx
                 (macher-agent-context-workspace ctx)
               (macher-agent--get-active-workspace)))
         (patch-buf (and (fboundp 'macher-patch-buffer)
                         (macher-patch-buffer ws))))
    patch-buf))

(defun macher-agent--resolve-context-lazy-init ()
  "Attempt lazy initialisation of context for current directory.

Return non-nil if initialisation succeeded, or signals an error if not allowed.
Side effects: May initialise workspace state for current directory."
  (unless (bound-and-true-p macher-agent--allow-lazy-init)
    (error "Macher-Agent: Not in a recognised project workspace; running standard gptel"))
  (save-excursion
    (when-let* ((current-root (macher-agent-root default-directory)))
      (macher-agent--init-workspace-state current-root)
      (macher-agent-context-from-buffer (current-buffer)))))

(defun macher-agent--register-active-workspace-root (root context)
  "Map active ROOT string to CONTEXT struct in the global registry."
  (cl-check-type root string)
  (cl-check-type context macher-agent-context)
  (when (and (boundp 'macher-agent-active-workspaces) (hash-table-p macher-agent-active-workspaces))
    (let ((expanded (expand-file-name root)))
      (puthash expanded context macher-agent-active-workspaces)
      (puthash (file-name-as-directory expanded) context macher-agent-active-workspaces)
      (puthash (directory-file-name expanded) context macher-agent-active-workspaces)))
  context)

(defmacro macher-agent--with-unresolved-ctx-pipe (state &rest body)
  "Execute BODY unless STATE is already resolved."
  (declare (indent 1) (debug t))
  (let ((st-sym (make-symbol "state-val")))
    `(let ((,st-sym ,state))
       (if (and (macher-agent--plist-p ,st-sym) (plist-get ,st-sym :resolved))
           ,st-sym
         ,@body))))

(defun macher-agent-ctx-pipe--explicit (state)
  "Extract explicit context from STATE plist."
  (cl-check-type state list)
  (macher-agent--with-unresolved-ctx-pipe state
                                          (let ((input (plist-get state :input)))
                                            (if (macher-agent-valid-context-p input)
                                                (plist-put state :resolved input)
                                              state))))

(defun macher-agent-ctx-pipe--buffer (state)
  "Extract buffer context from STATE plist."
  (cl-check-type state list)
  (macher-agent--with-unresolved-ctx-pipe state
                                          (let ((input (plist-get state :input)))
                                            (if (and (bufferp input) (buffer-live-p input))
                                                (let ((ctx (buffer-local-value 'macher-agent--persistent-context input)))
                                                  (if (macher-agent-valid-context-p ctx)
                                                      (plist-put state :resolved ctx)
                                                    state))
                                              state))))

(defun macher-agent-ctx-pipe--lazy-init (state)
  "Context pipeline step: Resolve context via lazy initialisation in STATE.

If `:resolved' in STATE is nil, attempt lazy workspace initialisation.

STATE is the context resolution state plist (:input ... :resolved ...).

Return the updated STATE plist.
Side effects: May initialise workspace state for current directory."
  (macher-agent--with-unresolved-ctx-pipe state
                                          (if-let* ((ctx (condition-case nil
                                                             (macher-agent--resolve-context-lazy-init)
                                                           (error nil)))
                                                    ((macher-agent-valid-context-p ctx)))
                                              (let ((target-buf (if (bufferp (plist-get state :input))
                                                                    (plist-get state :input)
                                                                  (current-buffer))))
                                                (when (buffer-live-p target-buf)
                                                  (with-current-buffer target-buf
                                                    (unless (bound-and-true-p macher-agent--persistent-context)
                                                      (setq-local macher-agent--persistent-context ctx))))
                                                (plist-put state :resolved ctx))
                                            state)))

(defun macher-agent-context-resolution-install ()
  "Install context resolution pipeline steps."
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--explicit 10)
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--buffer 15)
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-resolve-from-transit-payload 15)
  (macher-agent-register-pipeline-step 'context-resolution #'macher-agent-ctx-pipe--lazy-init 80))

(cl-defun make-macher-agent-workspace (&key project-root &allow-other-keys)
  "Construct a standard workspace cons cell `(project . PROJECT-ROOT)`.

PROJECT-ROOT is the root directory path.

Return a workspace cons cell."
  (cons 'project (and project-root (expand-file-name project-root))))

(defun copy-macher-agent-workspace (ws)
  "Create a copy of workspace WS.

WS is the workspace structure to copy.

Return the copied workspace."
  (if (consp ws) (copy-tree ws) ws))

(defun macher-agent-workspace-p (ws)
  "Verify WS is a strictly formatted workspace list."
  (cl-check-type ws list)
  (plist-member ws :workspace-id))

(defun macher-agent-context-lookup (ws-or-id)
  "Retrieve active context mapped to WS-OR-ID string."
  (cl-check-type ws-or-id string)
  (or (macher-agent-context-from-buffer ws-or-id)
      (when (and (boundp 'macher-agent-active-workspaces)
                 (hash-table-p macher-agent-active-workspaces))
        (let ((exp (expand-file-name ws-or-id))
              (true-exp (file-truename (expand-file-name ws-or-id))))
          (or (gethash exp macher-agent-active-workspaces)
              (gethash (file-name-as-directory exp) macher-agent-active-workspaces)
              (gethash (directory-file-name exp) macher-agent-active-workspaces)
              (gethash true-exp macher-agent-active-workspaces)
              (gethash (file-name-as-directory true-exp) macher-agent-active-workspaces)
              (gethash (directory-file-name true-exp) macher-agent-active-workspaces))))))

(defun macher-agent--workspace-get-hash-table (ctx table-key)
  "Extract hash table explicitly from CTX struct."
  (cl-check-type ctx macher-agent-context)
  (let ((plugins (macher-agent-context-plugins ctx)))
    (or (when (macher-agent--plist-p plugins)
          (plist-get plugins table-key))
        (let ((ht (make-hash-table :test 'equal)))
          (setf (macher-agent-context-plugins ctx)
                (plist-put (copy-sequence plugins) table-key ht))
          ht))))

(defun macher-agent-workspace-vfs-buffers (ctx)
  "Retrieve the VFS buffers hash-table for CTX."
  (cl-check-type ctx macher-agent-context)
  (macher-agent--workspace-get-hash-table ctx :vfs-buffers))

(defun macher-agent-workspace-mtime-tracker (ctx)
  "Retrieve the mtime tracker hash-table for CTX."
  (cl-check-type ctx macher-agent-context)
  (macher-agent--workspace-get-hash-table ctx :mtime-tracker))

(defun macher-agent-workspace-tools-registry (ctx)
  "Extract combined tools registry for CTX, overlaying workspace tools on global tools."
  (cl-check-type ctx macher-agent-context)
  (let ((local-table (or (macher-agent-context-tools ctx)
                         (let ((plugins (macher-agent-context-plugins ctx)))
                           (when (macher-agent--plist-p plugins)
                             (plist-get plugins :tools)))))
        (global-table macher-agent-tools-registry)
        (merged-table (make-hash-table :test 'equal)))
    (when (hash-table-p global-table)
      (maphash (lambda (k v) (puthash k v merged-table)) global-table))
    (when (hash-table-p local-table)
      (maphash (lambda (k v) (puthash k v merged-table)) local-table))
    merged-table))

(defun macher-agent-workspace-skills-alist (ctx)
  "Extract skills alist for CTX, merging local workspace skills over global skills."
  (cl-check-type ctx macher-agent-context)
  (let ((local-skills (or (macher-agent-context-skills ctx)
                          (let ((plugins (macher-agent-context-plugins ctx)))
                            (when (macher-agent--plist-p plugins)
                              (plist-get plugins :skills)))))
        (global-skills (bound-and-true-p macher-agent-global-skills-alist)))
    (cl-remove-duplicates (append local-skills global-skills)
                          :key #'car :test #'eq)))

(defun macher-agent-workspace-agent ()
  "Identify if the current buffer is a workspace and return the workspace.
Return the workspace struct, or nil."
  (when (bound-and-true-p macher-agent--is-workspace)
    (bound-and-true-p macher--workspace)))

(defun macher-agent-workspace-active-subagents (ctx)
  "Extract active subagents list directly via CTX struct."
  (cl-check-type ctx macher-agent-context)
  (or (macher-agent-context-subagents ctx)
      (let ((plugins (macher-agent-context-plugins ctx)))
        (when (macher-agent--plist-p plugins)
          (plist-get plugins :active-subagents)))))

(defun macher-agent--set-workspace-skills-alist (ctx alist)
  "Update skills ALIST mapped to CTX struct."
  (cl-check-type alist list)
  (if ctx
      (progn
        (cl-check-type ctx macher-agent-context)
        (setf (macher-agent-context-skills ctx) alist))
    (setq macher-agent-global-skills-alist alist))
  alist)

(gv-define-setter macher-agent-workspace-skills-alist (val ws-or-ctx)
  `(macher-agent--set-workspace-skills-alist ,ws-or-ctx ,val))

(defun macher-agent--set-workspace-tools-registry (ctx registry)
  "Update tools REGISTRY mapped to CTX struct."
  (cl-check-type registry hash-table)
  (if ctx
      (progn
        (cl-check-type ctx macher-agent-context)
        (setf (macher-agent-context-tools ctx) registry))
    (setq macher-agent-tools-registry registry))
  registry)

(gv-define-setter macher-agent-workspace-tools-registry (val ws-or-ctx)
  `(macher-agent--set-workspace-tools-registry ,ws-or-ctx ,val))

(defun macher-agent--set-workspace-active-subagents (ctx subagents)
  "Update SUBAGENTS list mapped to CTX struct."
  (cl-check-type subagents list)
  (when ctx
    (cl-check-type ctx macher-agent-context)
    (setf (macher-agent-context-subagents ctx) subagents))
  subagents)

(gv-define-setter macher-agent-workspace-active-subagents (val ws-or-ctx)
  `(macher-agent--set-workspace-active-subagents ,ws-or-ctx ,val))

(defun macher-agent--copy-context-hash-tables (data)
  "Copy hash tables in DATA plist for context cloning.

DATA is the property list containing context data.

Return a copied property list with cloned hash-tables."
  (when data
    (let ((res (copy-sequence data)))
      (when-let* ((vfs-ht (plist-get res :vfs-buffers)))
        (setq res (plist-put res :vfs-buffers (copy-hash-table vfs-ht))))
      (when-let* ((mtime-ht (plist-get res :mtime-tracker)))
        (setq res (plist-put res :mtime-tracker (copy-hash-table mtime-ht))))
      res)))

(defun macher-agent--clone-context (ctx)
  "Deep clone CTX struct, bypassing polymorphic buffer/string checks."
  (cl-check-type ctx macher-agent-context)
  (let* ((new-ctx (macher-agent--copy-context ctx))
         (orig-plugins (macher-agent-context-plugins ctx))
         (new-plugins (macher-agent--copy-context-hash-tables orig-plugins)))
    (setf (macher-agent-context-plugins new-ctx) (copy-tree new-plugins))
    (when-let* ((tools (macher-agent-context-tools ctx)))
      (setf (macher-agent-context-tools new-ctx)
            (if (hash-table-p tools) (copy-hash-table tools) (copy-tree tools))))
    (when-let* ((skills (macher-agent-context-skills ctx)))
      (setf (macher-agent-context-skills new-ctx) (copy-tree skills)))
    (when-let* ((subs (macher-agent-context-subagents ctx)))
      (setf (macher-agent-context-subagents new-ctx) (copy-tree subs)))
    new-ctx))

(defun macher-agent--inject-context-state (context &optional directives)
  "Inject the active CONTEXT and optional DIRECTIVES into the buffer.

CONTEXT is the active context structure.
DIRECTIVES is the optional directives alist.

Return nil.
Side effects: Sets buffer-local variables
`macher-agent--persistent-context` and `gptel-directives`."
  (when context
    (setq-local macher-agent--persistent-context context)
    (when directives
      (if (boundp 'gptel-directives)
          (setq gptel-directives directives)
        (setq-local gptel-directives directives)))))

(defun macher-agent-apply-patch ()
  "Apply the active patch from `diff-mode' context back to physical files.

Return nil.
Side effects: Invokes external process (`git` or `patch`) to apply patch."
  (interactive)
  (unless (derived-mode-p 'diff-mode) (user-error "Not in a patch/diff buffer"))
  (let* ((patch-content (buffer-substring-no-properties (point-min) (point-max)))
         (ctx (or (macher-agent-context-from-buffer (current-buffer))
                  (bound-and-true-p macher-agent--persistent-context)))
         (root (if ctx
                   (macher-agent-context-root ctx)
                 (or (locate-dominating-file default-directory ".git") default-directory)))
         (default-directory (file-name-as-directory (expand-file-name root)))
         (use-git (locate-dominating-file default-directory ".git"))
         (cmd (if use-git "git" "patch"))
         (args (if use-git '("apply" "-p1" "-") '("-p1"))))
    (with-temp-buffer
      (insert patch-content)
      (let ((exit-code (apply #'call-process-region (point-min) (point-max) cmd nil "*macher-patch-out*" nil args)))
        (if (= exit-code 0)
            (progn
              (message "SUCCESS: Patch applied safely via %s from %s" cmd default-directory)
              (when (get-buffer "*macher-patch-out*")
                (kill-buffer "*macher-patch-out*")))
          (pop-to-buffer "*macher-patch-out*")
          (message "ERROR: Failed to apply patch safely. Check *macher-patch-out* for details."))))))

(provide 'macher-agent-macher)
;;; macher-agent-macher.el ends here
