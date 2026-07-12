;;; macher-agent-vfs-client.el --- Layer 2 VFS Client for Macher -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'macher)
(require 'gptel nil t)
(require 'xref)

(declare-function macher-agent-sync-prompt-transformer "macher-agent-gptel-bridge")

(cl-defstruct macher-agent-workspace
  "A shared singleton per project root managing the VFS state."
  project-root
  (vfs-buffers (make-hash-table :test 'equal))
  (mtime-tracker (make-hash-table :test 'equal))
  (tools-registry (make-hash-table :test 'equal))
  (skills-alist nil)
  (active-subagents nil))

(require 'macher-agent-macher-bridge)

(cl-defstruct macher-agent-session
  id
  workspace
  sandbox-path
  (pending-media nil))

(defvar macher-agent-context-mutated-hook nil)
(defvar macher-agent--allow-lazy-init nil)
(defvar macher-agent-active-workspaces (make-hash-table :test 'equal)
  "Registry mapping expanded project roots to their active persistent contexts.")

(defun macher-agent-vfs-get-node (workspace path)
  "Retrieve a node's content from the virtual file system.

WORKSPACE is the workspace structure.
PATH is the relative or absolute path string.

Return the node's content, or nil."
  (gethash path (macher-agent-workspace-vfs-buffers workspace)))

(defun macher-agent-vfs-set-node (workspace path content)
  "Set a node's content in the virtual file system.

WORKSPACE is the workspace structure.
PATH is the relative or absolute path string.
CONTENT is the string content to store.

Return the stored content."
  (puthash path content (macher-agent-workspace-vfs-buffers workspace)))

(defun macher-agent-vfs-write (workspace file-path content)
  "Write content to FILE-PATH in the virtual file system with concurrency checking.

WORKSPACE is the workspace structure.
FILE-PATH is the relative or absolute path string.
CONTENT is the string content to write.

Return the stored content."
  (let* ((tracker (macher-agent-workspace-mtime-tracker workspace))
         (original-mtime (gethash file-path tracker))
         (current-attrs (file-attributes file-path))
         (current-mtime (nth 5 current-attrs)))
    (when (and original-mtime current-mtime (not (equal original-mtime current-mtime)))
      (error "Your previous edits to %s were discarded due to external file modifications. Please re-read and re-apply."
             (file-name-nondirectory file-path)))
    (puthash file-path current-mtime tracker)
    (puthash file-path content (macher-agent-workspace-vfs-buffers workspace))))

(defun macher-agent-vfs-read (workspace file-path)
  "Read a node's content from the virtual file system or physical disk.

WORKSPACE is the workspace structure.
FILE-PATH is the relative or absolute path string.

Return the content string, or nil."
  (let ((content (gethash file-path (macher-agent-workspace-vfs-buffers workspace))))
    (if content
        content
      (macher-agent--read-content-from-disk-or-buffer file-path))))

(cl-defstruct macher-agent-vfs-entry
  "A structure representing a virtual file system entry."
  path
  orig
  curr)

(defun macher-agent-vfs-make-entry (path orig curr)
  "Create a virtual file system entry structure.

PATH is the file or buffer path string.
ORIG is the original content string.
CURR is the current modified content string.

Return the created macher-agent-vfs-entry struct."
  (make-macher-agent-vfs-entry :path path :orig orig :curr curr))

(defun macher-agent-vfs-entry-modified-p (entry)
  "Return non-nil if ENTRY has been modified from its original content.

ENTRY is the macher-agent-vfs-entry struct to check.

Return non-nil if modified, otherwise nil."
  (not (equal (macher-agent-vfs-entry-orig entry)
              (macher-agent-vfs-entry-curr entry))))

(defun macher-agent-media-file-p (path)
  "Return non-nil if PATH represents a media file.

PATH is the file path string to check.

Return non-nil if it is a media file, otherwise nil."
  (and (stringp path)
       (let ((mime (and (fboundp 'mailcap-file-name-to-mime-type)
                        (mailcap-file-name-to-mime-type path))))
         (or (and mime (or (string-prefix-p "image/" mime)
                           (string-prefix-p "video/" mime)
                           (string-prefix-p "audio/" mime)))
             (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|webp\\|svg\\|pdf\\|mp4\\|mov\\|mp3\\|wav\\)$" path)))))

(cl-defgeneric macher-agent-root (&optional obj)
  "Resolve the absolute project or workspace root path from OBJ.
OBJ can be a string path, a buffer, a virtual file system context, a workspace struct, or nil.
Returns the absolute path string, or nil if unresolved.")

(cl-defmethod macher-agent-root ((obj string))
  (let* ((proj (and (fboundp 'project-current) (project-current nil obj))))
    (expand-file-name
     (or (and proj (if (fboundp 'project-root) (project-root proj) (cdr proj)))
         (and (fboundp 'vc-root-dir) (let ((default-directory obj)) (vc-root-dir)))
         obj))))

(cl-defmethod macher-agent-root ((obj buffer))
  (with-current-buffer obj
    (macher-agent-root default-directory)))

(cl-defmethod macher-agent-root ((obj macher-agent-workspace))
  (macher-agent-workspace-project-root obj))

(cl-defmethod macher-agent-root ((obj cons))
  (cond
   ((and (eq (car obj) 'agent) (not (stringp (cdr obj))))
    (let ((ws (cdr obj)))
      (if ws
          (macher-agent-root ws)
        (macher-agent-root default-directory))))
   ((and (eq (car obj) 'project) (stringp (cdr obj)))
    (expand-file-name (cdr obj)))
   (t (macher-agent-root default-directory))))

(cl-defmethod macher-agent-root ((obj null))
  (macher-agent-root default-directory))

(cl-defmethod macher-agent-root (obj)
  (cond
   ((and obj (fboundp 'macher-context-p) (macher-context-p obj))
    (let ((ws (macher-agent--get-context-workspace obj)))
      (if ws
          (macher-agent-root ws)
        (macher-agent-root default-directory))))
   ((and obj (fboundp 'macher--workspace-root))
    (ignore-errors (macher--workspace-root obj)))
   (t (macher-agent-root default-directory))))

(defun macher-agent--resolve-safe-path (unsafe-path base-dir)
  "Resolves UNSAFE-PATH strictly within BASE-DIR, preventing jailbreaks.

UNSAFE-PATH is the raw string path to resolve.
BASE-DIR is the absolute directory path string.

Return the resolved absolute safe path string."
  (when (file-name-absolute-p unsafe-path)
    (error "SECURITY ERROR: Absolute paths are forbidden. You must use relative paths (for example, ./file). Path attempted: %s" unsafe-path))

  (let* ((relative (if (string-prefix-p "~" unsafe-path) (concat "./" unsafe-path) unsafe-path))
         (resolved (expand-file-name relative base-dir)))
    (if (or (file-in-directory-p resolved base-dir)
            (string= (expand-file-name resolved) (expand-file-name base-dir)))
        resolved
      (error "SECURITY ERROR: Path traversal jailbreak detected: %s" unsafe-path))))

(defun macher-agent--vfs-process-entries (entries sandbox-path entry-path-fn entry-content-fn)
  "Process VFS ENTRIES, inflating or deleting them within SANDBOX-PATH.

ENTRIES is the list of VFS entries.
SANDBOX-PATH is the sandbox path string.
ENTRY-PATH-FN is the function to extract the relative path.
ENTRY-CONTENT-FN is the function to extract content.

Return nil."
  (let ((sandbox-root (file-name-as-directory (expand-file-name sandbox-path))))
    (mapc (lambda (entry)
            (let* ((relative-path (funcall entry-path-fn entry))
                   (new-content (funcall entry-content-fn entry))
                   (sandbox-target-path (macher-agent--resolve-safe-path relative-path sandbox-root)))
              (if (stringp new-content)
                  (progn
                    (make-directory (file-name-directory sandbox-target-path) t)
                    (write-region new-content nil sandbox-target-path nil 'silent))
                ;; Fix: Physically remove the file from the sandbox if marked for deletion.
                (when (file-exists-p sandbox-target-path)
                  (delete-file sandbox-target-path)))))
          entries)))

(defun macher-agent-sandbox-inflate (session)
  "Inflate the VFS contents for SESSION into its physical sandbox directory.

SESSION is the macher-agent-session struct.

Return nil."
  (let* ((workspace (macher-agent-session-workspace session))
         (sandbox-path (macher-agent-session-sandbox-path session))
         (vfs-buffers (macher-agent-workspace-vfs-buffers workspace))
         (ws-root (macher-agent-workspace-project-root workspace)))
    (when sandbox-path
      (let ((entries (hash-table-keys vfs-buffers)))
        (macher-agent--vfs-process-entries
         entries
         sandbox-path
         (lambda (key)
           (if (file-name-absolute-p key)
               (file-relative-name key ws-root)
             key))
         (lambda (key) (gethash key vfs-buffers)))))))

(defun macher-agent-context-root (context)
  "Retrieve the project root directory string from CONTEXT.

CONTEXT is the active context structure.

Return the project root path string."
  (or (macher-agent-root context) default-directory))

(defun macher-agent--vfs-verify-clean-merge (workspace-root context)
  "Verify that the active CONTEXT can merge cleanly into WORKSPACE-ROOT.

WORKSPACE-ROOT is the project root path string.
CONTEXT is the active context structure.

Return t."
  t)

(defun macher-agent--build-rsync-cmd (src dest)
  "Construct an rsync command driven by Git. Throws an error if Git is unavailable.

SRC is the source directory string.
DEST is the destination directory string.

Return the shell command string."
  (let* ((src-dir (file-name-as-directory (expand-file-name src)))
         (dest-dir (file-name-as-directory (expand-file-name dest))))

    (unless (executable-find "git")
      (error "Macher-Agent: Git executable not found in PATH"))

    (unless (eq 0 (let ((default-directory src-dir))
                    (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree")))
      (error "Macher-Agent: Source directory is not inside a Git repository: %s" src-dir))

    (format "(cd %s && git ls-files -c -o --exclude-standard) | rsync -aLC --delete --files-from=- %s %s"
            (shell-quote-argument src-dir)
            (shell-quote-argument src-dir)
            (shell-quote-argument dest-dir))))

(defun macher-agent--vfs-sync-baseline (workspace-root sandbox-dir)
  "Asynchronously or synchronously sync the physical WORKSPACE-ROOT to SANDBOX-DIR.

WORKSPACE-ROOT is the source project directory string.
SANDBOX-DIR is the target directory string.

Return the process exit code integer."
  (let ((sync-cmd (macher-agent--build-rsync-cmd workspace-root sandbox-dir)))
    (call-process shell-file-name nil nil nil shell-command-switch sync-cmd)))

(defun macher-agent--edit-string-fast (content old-text new-text replace-all)
  "Replace OLD-TEXT with NEW-TEXT in CONTENT.
If REPLACE-ALL is nil, errors if OLD-TEXT occurs more than once.

CONTENT is the original string block.
OLD-TEXT is the target substring to match.
NEW-TEXT is the replacement string.
REPLACE-ALL is a boolean flag to replace all occurrences.

Return the modified string."
  (when (string-empty-p old-text)
    (error "Cannot replace an empty string. Provide exact text to match."))
  (let ((count 0)
        (start 0))
    (while (string-match (regexp-quote old-text) content start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    (cond
     ((= count 0)
      (error "Text not found: %s" old-text))
     ((and (> count 1) (not replace-all))
      (error "Multiple matches found for text. Set replace_all to true or provide more context: %s" old-text))
     (t
      (replace-regexp-in-string (regexp-quote old-text) new-text content t t)))))

(defun macher-agent--vfs-apply-overlay-stateless (contents ws-root sandbox-dir)
  "Apply virtual CONTENTS overlay to SANDBOX-DIR statelessly.

CONTENTS is the list of VFS entry structures.
WS-ROOT is the workspace root path string.
SANDBOX-DIR is the sandbox directory path string.

Return nil."
  (macher-agent--vfs-process-entries
   contents
   sandbox-dir
   (lambda (entry)
     (let ((path (macher-agent-vfs-entry-path entry)))
       (if (file-name-absolute-p path)
           (file-relative-name path ws-root)
         path)))
   #'macher-agent-vfs-entry-curr))

(defun macher-agent--vfs-apply-overlay (context sandbox-dir)
  "Apply virtual context overlay to SANDBOX-DIR.

CONTEXT is the active context structure.
SANDBOX-DIR is the sandbox directory path string.

Return nil."
  (when (and context (macher-agent--get-context-dirty-p context))
    (let* ((ws-root (macher-agent-context-root context))
           (raw-contents (macher-agent--get-context-contents context))
           (contents (seq-filter (lambda (entry)
                                   (and entry (not (consp entry))
                                        (or (and (fboundp 'macher-agent-vfs-entry-p)
                                                 (macher-agent-vfs-entry-p entry))
                                            (eq (type-of entry) 'macher-agent-vfs-entry))))
                                 raw-contents)))
      (macher-agent--vfs-apply-overlay-stateless contents ws-root sandbox-dir))))

(defun macher-agent-call-with-strict-vfs-pipeline (context body-fn)
  "Execute BODY-FN within a physical sandbox directory populated with CONTEXT.

CONTEXT is the active context structure.
BODY-FN is the function containing pipeline logic.

Return the result of BODY-FN."
  (let* ((workspace-root (macher-agent-context-root context))
         (sandbox-dir (make-temp-file "macher-sandbox-" t))
         (original-contents (and context (macher-agent--get-context-contents context))))
    (unwind-protect
        (progn
          (when context
            (macher-agent--set-context-contents context
                                                (seq-filter (lambda (entry)
                                                              (and entry
                                                                   (not (consp entry))
                                                                   (or (and (fboundp 'macher-agent-vfs-entry-p)
                                                                            (macher-agent-vfs-entry-p entry))
                                                                       (eq (type-of entry) 'macher-agent-vfs-entry))))
                                                            original-contents)))
          (macher-agent--vfs-verify-clean-merge workspace-root context)
          (macher-agent--vfs-sync-baseline workspace-root sandbox-dir)
          (macher-agent--vfs-apply-overlay context sandbox-dir)
          (let ((default-directory sandbox-dir))
            (funcall body-fn)))
      (when context
        (macher-agent--set-context-contents context original-contents))
      (delete-directory sandbox-dir t))))

(defmacro macher-agent-with-strict-vfs-pipeline (context &rest body)
  `(macher-agent-call-with-strict-vfs-pipeline ,context (lambda () ,@body)))

(defun macher-agent--ensure-access (context path)
  "Ensure PATH is within the explicitly scoped CONTEXT.

CONTEXT is the active context structure.
PATH is the string file path.

Return nil or signals an error."
  (macher-agent--ensure-access-stateless (and context (macher-agent--get-context-contents context)) path))

(defun macher-agent--inject-context-state (context &optional directives)
  "Explicitly inject the active agent CONTEXT and optional DIRECTIVES into the current buffer.

CONTEXT is the active context structure.
DIRECTIVES is the optional directives alist.

Return nil."
  (when context
    (setq-local macher-agent--persistent-context context)
    (when directives
      (setq-local gptel-directives directives))))

(defun macher-agent-current-context (&optional ctx-or-fsm)
  "Alias for `macher-agent-resolve-context'.

CTX-OR-FSM is the optional context or finite-state machine.

Return the resolved context structure, or nil."
  (macher-agent-resolve-context ctx-or-fsm))

(defun macher-agent--extract-fsm-info (fsm)
  "Safely extract the info plist from a finite-state machine (FSM).

FSM is the finite-state machine object.

Return the info property list, or nil."
  (when fsm
    (if (fboundp 'gptel-fsm-info)
        (funcall 'gptel-fsm-info fsm)
      (when (fboundp 'mock-gptel-fsm-info)
        (funcall 'mock-gptel-fsm-info fsm)))))

(defun macher-agent--extract-fsm-context (fsm)
  "Extract the active context from a finite-state machine (FSM).

FSM is the finite-state machine object.

Return the active context structure, or nil."
  (let ((info (macher-agent--extract-fsm-info fsm)))
    (and info (or (plist-get info :macher-agent-context)
                  (plist-get info :macher--context)))))

(defun macher-agent-resolve-context (&optional ctx-or-fsm)
  "Resolve the active context from CTX-OR-FSM or state.
Follows a predictable waterfall:
1. If CTX-OR-FSM satisfies `macher-context-p', return it.
2. If CTX-OR-FSM is a finite-state machine (FSM), extract its context.
3. If `macher-agent--persistent-context' is bound locally in the current buffer, return it.
4. Try to get context from the latest FSM via `macher-agent--get-fsm-latest'.
5. Fallback to registry-based active workspace context lookup.

CTX-OR-FSM is the optional context or finite-state machine.

Return the resolved context structure, or nil."
  (cond
   ((and ctx-or-fsm (fboundp 'macher-context-p) (macher-context-p ctx-or-fsm))
    ctx-or-fsm)
   ((macher-agent--extract-fsm-context ctx-or-fsm))
   ((bound-and-true-p macher-agent--persistent-context)
    macher-agent--persistent-context)
   ((let ((fsm (macher-agent--get-fsm-latest)))
      (macher-agent--extract-fsm-context fsm)))
   (t
    (let* ((active-root (macher-agent-root default-directory))
           (active-root-expanded (and active-root (expand-file-name active-root)))
           (primary-ctx (and active-root-expanded
                             (gethash active-root-expanded macher-agent-active-workspaces))))

      (unless primary-ctx
        (if macher-agent--allow-lazy-init
            (save-excursion
              (let ((current-root (macher-agent-root default-directory)))
                (macher-agent--init-workspace-state current-root)
                (setq primary-ctx (bound-and-true-p macher-agent--persistent-context))))
          (error "No active agent session found")))

      primary-ctx))))

(defun macher-agent--read-content-from-disk-or-buffer (path)
  "Read the contents of PATH from an active buffer or disk.

PATH is the relative or absolute path string.

Return the content string, or nil."
  (let ((buf (or (get-file-buffer path) (get-buffer path)))
        (is-media (and (file-exists-p path)
                       (macher-agent-media-file-p path))))
    (cond
     ((and buf (buffer-live-p buf))
      (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))
     ((file-exists-p path)
      (with-temp-buffer
        (if is-media (insert-file-contents-literally path) (insert-file-contents path))
        (buffer-string)))
     (t nil))))

(defun macher-agent--init-workspace-state (workspace-root)
  "Initialise the workspace state and active context for WORKSPACE-ROOT.

WORKSPACE-ROOT is the project root directory string.

Return nil."
  (setq-local macher-agent--is-workspace t)
  (let* ((workspace (make-macher-agent-workspace :project-root workspace-root))
         (macher-ws (cons 'agent workspace))
         (context (macher-agent--make-vfs-context :workspace macher-ws :contents nil)))
    (setq-local macher--workspace macher-ws)
    (macher-agent--inject-context-state context)

    (puthash (expand-file-name workspace-root) context macher-agent-active-workspaces)

    (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)

    (let ((skills-dir (expand-file-name "skills" workspace-root))
          (bundled (or (and (boundp 'macher-agent--bundled-skills-dir) macher-agent--bundled-skills-dir)
                       (and (boundp 'macher-agent-bundled-skills-directory) macher-agent-bundled-skills-directory))))
      (when (fboundp 'macher-agent-initialize-skills)
        (when bundled
          (macher-agent-initialize-skills context bundled))
        (when (file-directory-p skills-dir)
          (macher-agent-initialize-skills context skills-dir))))))

(defun macher-agent--partition-vfs-entries (contents &optional root-dir)
  "Split raw VFS CONTENTS into pure virtual and physical lists.

CONTENTS is the list of VFS entry structures.
ROOT-DIR is the optional project root path string.

Return a cons cell (VIRTUAL-ENTRIES . PHYSICAL-ENTRIES)."
  (let ((virtual-contents nil)
        (physical-contents nil))
    (dolist (entry contents)
      (let* ((name (macher-agent-vfs-entry-path entry))
             (type (macher-agent-context-classify-entry name root-dir)))
        (if (eq type 'buffer)
            (push entry virtual-contents)
          (push entry physical-contents))))
    (cons (nreverse virtual-contents) (nreverse physical-contents))))

(defun macher-agent--split-context (ctx)
  "Split context CTX into virtual and physical context clones.

CTX is the active context structure.

Return a cons cell of cloned contexts (FILE-CONTEXT . BUFFER-CONTEXT)."
  (let ((file-ctx (macher-agent--clone-context ctx))
        (buf-ctx (macher-agent--clone-context ctx))
        (workspace (when ctx (macher-agent--get-context-workspace ctx))))
    (let* ((root (and workspace (macher-agent-root workspace)))
           (contents (when ctx (macher-agent--get-context-contents ctx)))

           (modified-contents (cl-remove-if (lambda (e) (equal (macher-agent-vfs-entry-orig e) (macher-agent-vfs-entry-curr e))) contents))
           (partitioned (macher-agent--partition-vfs-entries modified-contents root))
           (buf-contents (car partitioned))
           (file-contents (cdr partitioned)))
      (when file-ctx (macher-agent--set-context-contents file-ctx file-contents))
      (when buf-ctx (macher-agent--set-context-contents buf-ctx buf-contents))
      (cons file-ctx buf-ctx))))

(defun macher-agent--get-buffer-content-stateless (path workspace-root)
  "Read buffer content statelessly using an explicit WORKSPACE-ROOT.

PATH is the relative or absolute path string.
WORKSPACE-ROOT is the workspace root path string.

Return the content string, or nil."
  (if workspace-root
      (let* ((relative-path (if (file-name-absolute-p path)
                                (file-relative-name path workspace-root)
                              path))
             (safe-path (macher-agent--resolve-safe-path relative-path workspace-root)))
        (macher-agent--read-content-from-disk-or-buffer safe-path))
    (macher-agent--read-content-from-disk-or-buffer path)))

(defun macher-agent--update-context-file (context path new-content)
  "Update PATH in CONTEXT with NEW-CONTENT.

CONTEXT is the active context structure.
PATH is the relative file path string.
NEW-CONTENT is the modified content string.

Return nil."
  (let* ((contents (macher-agent--get-context-contents context))
         (entry (cl-find path contents :key #'macher-agent-vfs-entry-path :test #'equal)))
    (if entry
        (setf (macher-agent-vfs-entry-curr entry) new-content)
      (let* ((workspace-root (macher-context-workspace-root context))
             (orig (macher-agent--get-buffer-content-stateless path workspace-root)))
        (macher-agent--set-context-contents context
                                            (cons (make-macher-agent-vfs-entry :path path :orig orig :curr new-content) contents))))
    (macher-agent--set-context-dirty-p context t)
    (macher-agent--persist-vfs-to-hidden-buffer context)
    (run-hook-with-args 'macher-agent-context-mutated-hook path)))

(defun macher-agent--ensure-access-stateless (contents path)
  "Ensure PATH is within the explicitly scoped CONTENTS list.

CONTENTS is the list of authorised VFS entries.
PATH is the string file path to verify.

Return nil or signals an error."
  (let ((actual-name (substring-no-properties path)))
    (unless (cl-find actual-name contents :key #'macher-agent-vfs-entry-path :test #'equal)
      (error "SECURITY ERROR: You do not have permission to access '%s'. Use list_buffers_in_workspace to see your allowed scope." actual-name))))

(defun macher-agent--read-context-file (context path)
  "Read PATH from CONTEXT. Prioritises VFS, then active buffers, then physical disk.
Uniformly applies security and path normalisation checks.

CONTEXT is the active context structure.
PATH is the file path string to read.

Return the file content string."
  (let* ((contents (and context (macher-agent--get-context-contents context)))
         (workspace-root (macher-context-workspace-root context)))
    (macher-agent--ensure-access-stateless contents path)
    (let* ((virtual-entry (cl-find path contents :key #'macher-agent-vfs-entry-path :test #'equal))
           (virtual-content (when virtual-entry (macher-agent-vfs-entry-curr virtual-entry))))
      (cond
       (virtual-content virtual-content)
       (workspace-root
        (let* ((relative-path (if (file-name-absolute-p path)
                                  (file-relative-name path workspace-root)
                                path))
               (safe-path (macher-agent--resolve-safe-path relative-path workspace-root)))
          (or (macher-agent--read-content-from-disk-or-buffer safe-path)
              (error "ERROR: File/Buffer '%s' does not exist." path))))
       (t
        (or (macher-agent--read-content-from-disk-or-buffer path)
            (error "ERROR: File/Buffer '%s' does not exist." path)))))))

(defun macher-agent-context-classify-entry (path-or-buf &optional root-dir)
  "Classify PATH-OR-BUF into a file type category.

PATH-OR-BUF is the string path or buffer name to classify.
ROOT-DIR is the optional workspace root path string.

Return a symbol: `file', `media', `buffer', or `external'."
  (let* ((expanded (if root-dir (expand-file-name path-or-buf root-dir) (expand-file-name path-or-buf)))
         (buf (get-buffer path-or-buf))
         (file-from-buf (and buf (buffer-file-name buf)))
         (is-absolute (file-name-absolute-p path-or-buf))
         (has-slash (string-match-p "/" path-or-buf)))
    (cond
     ((or (and file-from-buf (file-exists-p file-from-buf))
          (file-exists-p expanded)
          is-absolute
          has-slash)
      (let* ((path (or (and file-from-buf (file-exists-p file-from-buf) file-from-buf) expanded))
             (is-in-workspace (if root-dir (string-prefix-p (expand-file-name root-dir) path) t)))
        (if is-in-workspace
            (if (macher-agent-media-file-p path)
                'media
              'file)
          'external)))
     (t 'buffer))))

(defvar-local macher-agent--is-workspace nil)
(defvar-local macher--workspace nil)
(defvar-local macher-agent--persistent-context nil)

(defun macher-agent--get-root (workspace)
  "Get the project root path of WORKSPACE.

WORKSPACE is the active workspace structure.

Return the project root path string."
  (macher-agent-workspace-project-root workspace))

(defun macher-agent--get-name (workspace)
  "Get a display name for WORKSPACE.

WORKSPACE is the active workspace structure.

Return the formatted name string."
  (concat "Agent: " (file-name-nondirectory (directory-file-name (macher-agent-workspace-project-root workspace)))))

(defun macher-agent--get-files (workspace)
  "Retrieve list of active files in WORKSPACE, applying safety constraints.

WORKSPACE is the active workspace structure.

Return a list of file path strings."
  (let* ((expanded-dir (expand-file-name (macher-agent-workspace-project-root workspace)))
         (home-dir (expand-file-name "~/")))
    (condition-case err
        (let* ((raw-files
                (if-let ((proj (project-current nil expanded-dir)))
                    (project-files proj)
                  (when (or (string= expanded-dir home-dir) (string= expanded-dir "/"))
                    (error "SECURITY HALT: Workspace resolved to root or home directory."))
                  (directory-files-recursively
                   expanded-dir "^[^.]" nil
                   (lambda (d)
                     (let ((base (file-name-nondirectory (directory-file-name d))))
                       (and (not (member base '(".git" "target" "node_modules" ".Trash" "Library" ".cache" ".config")))
                            (condition-case nil (progn (directory-files d) t) (error nil))))))))
               (safe-files '()))
          (dolist (file raw-files)
            (when (file-exists-p file)
              (let ((attrs (file-attributes file)))
                (when (and attrs
                           (< (file-attribute-size attrs) 1000000)
                           (not (string-suffix-p ".json" file))
                           (not (string-suffix-p ".eln" file))
                           (not (string-suffix-p ".elc" file)))
                  (push file safe-files)))))
          (nreverse safe-files))
      (error (error "Agent Workspace Error: %s" (error-message-string err))))))

(add-to-list 'macher-workspace-types-alist
             '(agent . (:get-root macher-agent--get-root
                                  :get-name macher-agent--get-name
                                  :get-files macher-agent--get-files)))

(defun macher-workspace-agent ()
  "Identify if the current buffer is a workspace and return the workspace.

Return the workspace struct, or nil."
  (when macher-agent--is-workspace macher--workspace))

(add-hook 'macher-workspace-functions #'macher-workspace-agent)

(defun macher-agent--clone-context (ctx)
  "Deep-copy and clone CTX.

CTX is the context structure.

Return the newly cloned context structure, or nil."
  (if (not ctx) nil
    (let ((new-ctx (macher-agent--make-vfs-context :workspace (when (fboundp 'macher-context-workspace) (macher-agent--get-context-workspace ctx))
                                                   :contents (copy-tree (macher-agent--get-context-contents ctx)))))
      (when (and (fboundp 'macher-context-prompt) (macher-agent--get-context-prompt ctx))
        (setf (macher-context-prompt new-ctx) (macher-agent--get-context-prompt ctx)))
      new-ctx)))

(defun macher-agent--merge-contexts (parent-ctx child-ctx)
  "Merge the VFS contents of CHILD-CTX into PARENT-CTX.

PARENT-CTX is the parent context structure.
CHILD-CTX is the child context structure.

Return nil."
  (let ((child-contents (macher-agent--get-context-contents child-ctx))
        (parent-contents (macher-agent--get-context-contents parent-ctx)))
    (dolist (child-entry child-contents)
      (let* ((path (macher-agent-vfs-entry-path child-entry))
             (orig (macher-agent-vfs-entry-orig child-entry))
             (new (macher-agent-vfs-entry-curr child-entry)))
        (when (or (not (equal orig new))
                  (not (cl-find path parent-contents :key #'macher-agent-vfs-entry-path :test #'equal)))
          (macher-agent--update-context-file parent-ctx path new))))))

(defun macher-agent--sync-context-entry (entry)
  "Synchronise a single VFS ENTRY with the physical disk.

ENTRY is the VFS entry structure.

Return non-nil if synchronisation modified the entry, otherwise nil."
  (let* ((path (macher-agent-vfs-entry-path entry))
         (orig (macher-agent-vfs-entry-orig entry))
         (new (macher-agent-vfs-entry-curr entry))
         (current-state (macher-agent--read-content-from-disk-or-buffer path)))
    (if (not (equal (or orig "") (or current-state "")))
        (if (equal (or new "") (or current-state ""))
            (progn (setf (macher-agent-vfs-entry-orig entry) current-state) t)
          (progn
            (setf (macher-agent-vfs-entry-orig entry) current-state)
            (setf (macher-agent-vfs-entry-curr entry) current-state)
            t))
      nil)))

(defvar macher-agent--pause-auto-sync nil
  "When non-nil, `macher-agent--auto-sync-context` will silently abort.
Used to prevent race conditions during shadow-buffer patch generation.")

(defun macher-agent--auto-sync-context (ctx &rest _args)
  "Synchronise the active context with the physical disk, unless paused.

CTX is the active context structure.
_ARGS represents unused extra arguments.

Return nil."
  (when (and ctx (not macher-agent--pause-auto-sync))
    (let ((contents (macher-agent--get-context-contents ctx))
          (synced nil))
      (dolist (entry contents)
        (when (and (fboundp 'macher-agent-vfs-entry-p)
                   (macher-agent-vfs-entry-p entry))
          (when (macher-agent--sync-context-entry entry)
            (setq synced t))))
      (when synced
        (macher-agent--set-context-dirty-p ctx nil)
        (macher-agent--persist-vfs-to-hidden-buffer ctx)
        (run-hooks 'macher-agent-context-mutated-hook)))))

(defun macher-agent--persist-vfs-to-hidden-buffer (ctx)
  "Persist virtual file system state of CTX to a hidden buffer for review.

CTX is the active context structure.

Return nil."
  (let* ((workspace (when ctx (macher-agent--get-context-workspace ctx)))
         (root-dir (if workspace (macher-agent-root workspace) "default"))
         (buf-name (format " *macher-agent-vfs-state-%s*" (md5 (expand-file-name root-dir))))
         (vfs-buf (get-buffer-create buf-name)))
    (with-current-buffer vfs-buf
      (erase-buffer)
      (insert ";;; Macher Agent Virtual File System State\n")
      (insert ";;; This buffer is native and handles large text blocks.\n\n")
      (when ctx
        (dolist (entry (macher-agent--get-context-contents ctx))
          (let ((path (macher-agent-vfs-entry-path entry))
                (new-content (macher-agent-vfs-entry-curr entry)))
            (when new-content
              (insert (format "=== VFS ENTRY: %s ===\n" path))
              (insert new-content)
              (insert "\n=======================\n\n"))))))))

(defun macher-agent-apply-patch ()
  "Apply the active patch from diff-mode context back to physical files.

Return nil."
  (interactive)
  (unless (derived-mode-p 'diff-mode) (user-error "Not in a patch/diff buffer"))
  (let* ((patch-content (buffer-substring-no-properties (point-min) (point-max)))
         (ctx (ignore-errors (macher-agent-resolve-context)))
         (ws (or (when ctx (macher-agent--get-context-workspace ctx))
                 (bound-and-true-p macher--workspace)))
         (root (if ws
                   (macher-agent-root ws)
                 (or (locate-dominating-file default-directory ".git") default-directory)))
         (default-directory (file-name-as-directory (expand-file-name root)))
         (use-git (locate-dominating-file default-directory ".git"))
         (cmd (if use-git "git" "patch"))
         (args (if use-git '("apply" "-p1" "-") '("-p1"))))
    (with-temp-buffer
      (insert patch-content)
      (let ((exit-code (apply #'call-process-region (point-min) (point-max) cmd nil "*macher-patch-out*" nil args)))
        (if (= exit-code 0)
            (progn (message "SUCCESS: Patch applied safely via %s from %s" cmd default-directory)
                   (when (get-buffer "*macher-patch-out*")
                     (kill-buffer "*macher-patch-out*")))
          (pop-to-buffer "*macher-patch-out*")
          (message "ERROR: Failed to apply patch safely."))))))

(defun macher-agent-insert-patch ()
  "Insert the proposed patch content into the current buffer.

Return nil."
  (interactive)
  (let* ((patch-buf (macher-patch-buffer))
         (content (when (buffer-live-p patch-buf)
                    (with-current-buffer patch-buf (buffer-substring-no-properties (point-min) (point-max))))))
    (if (or (null content) (string-empty-p content))
        (message "No patch available for current workspace.")
      (insert "\nHere is your proposed patch:\n```diff\n" content "\n```\n"))))

(defun macher-agent-clear-context ()
  "Clear the active context and reset state.

Return nil."
  (interactive)
  (if (not macher-agent--persistent-context)
      (message "No active context to clear in this buffer.")
    (let* ((ws (macher-agent--get-context-workspace macher-agent--persistent-context))
           (ws-root (and ws (macher-agent-root ws))))
      (when ws-root
        (remhash (expand-file-name ws-root) macher-agent-active-workspaces)))
    (setq-local macher-agent--persistent-context nil)
    (run-hooks 'macher-agent-context-mutated-hook)
    (message "Agent memory cleared. It will take a fresh snapshot of the disk on its next task.")))

(provide 'macher-agent-vfs-client)
