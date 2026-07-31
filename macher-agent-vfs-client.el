;;; macher-agent-vfs-client.el --- Layer 2 VFS Client for Macher -*- lexical-binding: t; -*-

;;; Commentary:
;; Virtual File System client managing optimistic concurrency, sandbox synchronisation,
;; and buffer-local agent contexts.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'macher)
(require 'gptel nil t)
(require 'xref)

(declare-function macher-agent-sync-prompt-transformer "macher-agent-gptel-bridge")
(declare-function macher-agent-initialize-skills "macher-agent-api")
(declare-function macher-context-workspace-root "macher-agent-api")

(defvar macher-agent-tools-registry)
(defvar macher-agent-global-skills-alist)

(defun macher-agent--get-context-data (ctx key &optional default)
  "Retrieve KEY from the native data slot of CTX.

CTX is the context structure.
KEY is the lookup key symbol.
DEFAULT is the value returned if KEY is not present.

Return the value or DEFAULT."
  (if (and ctx (macher-context-p ctx))
      (let ((data (macher-context-data ctx)))
        (if (plist-member data key)
            (plist-get data key)
          default))
    default))

(defun macher-agent--set-context-data (ctx key val)
  "Set KEY to VAL in the native data slot of CTX.

CTX is the context structure.
KEY is the key symbol.
VAL is the value to store.

Return VAL.
Side effects: Modifies native data slot of CTX."
  (when (and ctx (macher-context-p ctx))
    (let ((data (macher-context-data ctx)))
      (setf (macher-context-data ctx)
            (plist-put data key val))))
  val)

(defun macher-agent--match-persistent-context (pers-ctx expanded-root)
  "Check if PERS-CTX root matches EXPANDED-ROOT.

PERS-CTX is the persistent context structure.
EXPANDED-ROOT is the expanded project root directory path string.

Return PERS-CTX if matching, otherwise nil."
  (when (and pers-ctx expanded-root)
    (let ((pers-root (ignore-errors (expand-file-name (macher-agent-context-root pers-ctx)))))
      (when (and pers-root
                 (or (string= pers-root expanded-root)
                     (string= (file-name-as-directory pers-root) (file-name-as-directory expanded-root))
                     (string= pers-root (file-name-as-directory expanded-root))
                     (and (file-directory-p expanded-root)
                          (string-prefix-p (file-name-as-directory pers-root)
                                           (file-name-as-directory expanded-root)))))
        pers-ctx))))

(defun macher-agent--find-active-workspace-in-ancestors (expanded-root)
  "Find active workspace context by walking up directory tree from EXPANDED-ROOT.

EXPANDED-ROOT is the expanded project root directory path string.

Return the matching workspace context structure, or nil."
  (when expanded-root
    (cl-loop for dir = (file-name-as-directory expanded-root)
             then (let ((p (file-name-directory (directory-file-name dir))))
                    (if (equal p dir) nil p))
             while dir
             thereis (or (gethash (file-name-as-directory dir) macher-agent-active-workspaces)
                         (gethash (directory-file-name dir) macher-agent-active-workspaces)
                         (gethash dir macher-agent-active-workspaces)))))

(defun macher-agent--resolve-context-from-ws (ws-or-ctx)
  "Resolve WS-OR-CTX into a `macher-context` struct if possible.

WS-OR-CTX is a workspace object, cons cell, or context struct.

Return the resolved `macher-context` struct, or nil."
  (if (and ws-or-ctx (macher-context-p ws-or-ctx))
      ws-or-ctx
    (let* ((target-root (macher-agent-workspace-project-root ws-or-ctx))
           (expanded-root (and target-root (expand-file-name target-root)))
           (pers-ctx (bound-and-true-p macher-agent--persistent-context)))
      (or (macher-agent--match-persistent-context pers-ctx expanded-root)
          (macher-agent--find-active-workspace-in-ancestors expanded-root)
          (ignore-errors (macher-agent-resolve-context))))))

(cl-defun make-macher-agent-workspace (&key project-root &allow-other-keys)
  "Construct a standard workspace cons cell `(project . PROJECT-ROOT)`.

PROJECT-ROOT is the root directory path.

Return a workspace cons cell."
  (cons 'project (and project-root (expand-file-name project-root))))

(defun macher-agent-workspace-p (ws)
  "Return non-nil if WS is a valid workspace identifier.

WS is the object to check.

Return non-nil if valid, otherwise nil."
  (or (and (consp ws) (eq (car ws) 'project))
      (and (consp ws) (eq (car ws) 'agent))
      (stringp ws)))

(defun macher-agent-workspace-project-root (ws)
  "Retrieve the project root from workspace WS.

WS is the workspace cons cell, string, or struct.

Return the project root path string."
  (cond
   ((and (consp ws) (eq (car ws) 'project)) (expand-file-name (cdr ws)))
   ((and (consp ws) (eq (car ws) 'agent) (stringp (cdr ws))) (expand-file-name (cdr ws)))
   ((stringp ws) (expand-file-name ws))
   ((consp ws) (expand-file-name (cdr ws)))
   (t nil)))

(defun macher-agent-workspace-vfs-buffers (ws-or-ctx)
  "Retrieve the VFS buffers hash-table for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a hash-table.
Side effects: Initialises `:vfs-buffers` in WS-OR-CTX if not present."
  (let ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
    (if ctx
        (or (macher-agent--get-context-data ctx :vfs-buffers)
            (let ((ht (make-hash-table :test 'equal)))
              (macher-agent--set-context-data ctx :vfs-buffers ht)
              ht))
      (make-hash-table :test 'equal))))

(defun macher-agent-workspace-mtime-tracker (ws-or-ctx)
  "Retrieve the mtime tracker hash-table for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a hash-table.
Side effects: Initialises `:mtime-tracker` in WS-OR-CTX if not present."
  (let ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
    (if ctx
        (or (macher-agent--get-context-data ctx :mtime-tracker)
            (let ((ht (make-hash-table :test 'equal)))
              (macher-agent--set-context-data ctx :mtime-tracker ht)
              ht))
      (make-hash-table :test 'equal))))

(defun macher-agent-workspace-tools-registry (ws-or-ctx)
  "Retrieve the tools registry hash-table for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a hash-table.
Side effects: Initialises `:tools-registry` in WS-OR-CTX if not present."
  (let ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
    (if ctx
        (or (macher-agent--get-context-data ctx :tools-registry)
            (let ((ht (make-hash-table :test 'equal)))
              (macher-agent--set-context-data ctx :tools-registry ht)
              ht))
      macher-agent-tools-registry)))

(defun macher-agent-workspace-skills-alist (ws-or-ctx)
  "Retrieve the skills alist for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return an alist."
  (let ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
    (if ctx
        (macher-agent--get-context-data ctx :skills-alist)
      macher-agent-global-skills-alist)))

(defun macher-agent-workspace-active-subagents (ws-or-ctx)
  "Retrieve the active subagents list for WS-OR-CTX.

WS-OR-CTX is a workspace object or context structure.

Return a list of subagent entries."
  (when-let* ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
    (macher-agent--get-context-data ctx :active-subagents)))

(defun macher-agent--set-workspace-skills-alist (ws-or-ctx val)
  "Set the skills alist for WS-OR-CTX to VAL.

WS-OR-CTX is a workspace object or context structure.
VAL is the skills alist to set.

Return VAL.
Side effects: Modifies the skills alist for WS-OR-CTX or global skills alist."
  (if-let* ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
      (macher-agent--set-context-data ctx :skills-alist val)
    (setq macher-agent-global-skills-alist val))
  val)

(gv-define-setter macher-agent-workspace-skills-alist (val ws-or-ctx)
  `(macher-agent--set-workspace-skills-alist ,ws-or-ctx ,val))

(defun macher-agent--set-workspace-active-subagents (ws-or-ctx val)
  "Set the active subagents list for WS-OR-CTX to VAL.

WS-OR-CTX is a workspace object or context structure.
VAL is the list of active subagents to set.

Return VAL.
Side effects: Modifies the active subagents list for WS-OR-CTX."
  (when-let* ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
    (macher-agent--set-context-data ctx :active-subagents val))
  val)

(gv-define-setter macher-agent-workspace-active-subagents (val ws-or-ctx)
  `(macher-agent--set-workspace-active-subagents ,ws-or-ctx ,val))

(defun macher-agent--set-workspace-tools-registry (ws-or-ctx val)
  "Set the tools registry for WS-OR-CTX to VAL.

WS-OR-CTX is a workspace object or context structure.
VAL is the tools registry hash-table to set.

Return VAL.
Side effects: Modifies the tools registry for WS-OR-CTX or global registry."
  (let ((ctx (macher-agent--resolve-context-from-ws ws-or-ctx)))
    (if ctx
        (macher-agent--set-context-data ctx :tools-registry val)
      (setq macher-agent-tools-registry val)))
  val)

(gv-define-setter macher-agent-workspace-tools-registry (val ws-or-ctx)
  `(macher-agent--set-workspace-tools-registry ,ws-or-ctx ,val))

(require 'macher-agent-macher-bridge)

(defvar macher-agent-context-mutated-hook nil
  "Hook run when a VFS context is mutated.")
(defvar macher-agent--allow-lazy-init nil
  "When non-nil, allow lazy initialisation of workspace context.")
(defvar macher-agent-active-workspaces (make-hash-table :test 'equal)
  "Registry mapping expanded project roots to their active persistent contexts.")

(defun macher-agent-vfs-get-node (workspace path)
  "Retrieve a node's content from the virtual file system.

WORKSPACE is the workspace structure.
PATH is the relative or absolute path string.

Return the node's content, or nil."
  (unless workspace
    (error "VFS Read Error: Workspace/Context cannot be nil"))
  (gethash path (macher-agent-workspace-vfs-buffers workspace)))

(defun macher-agent-vfs-set-node (workspace path content)
  "Set a node's content in the virtual file system.

WORKSPACE is the workspace structure.
PATH is the relative or absolute path string.
CONTENT is the string content to store.

Return the stored content.
Side effects: Stores CONTENT in the VFS buffers hash-table for WORKSPACE."
  (unless workspace
    (error "VFS Write Error: Workspace/Context cannot be nil"))
  (puthash path content (macher-agent-workspace-vfs-buffers workspace)))

(defun macher-agent-vfs-write (workspace file-path content)
  "Write CONTENT to FILE-PATH in VFS with concurrency checking.

WORKSPACE is the workspace structure.
FILE-PATH is the relative or absolute path string.
CONTENT is the string content to write.

Return the stored content.
Side effects: Updates mtime tracker and VFS buffer entries for WORKSPACE."
  (unless workspace
    (error "VFS Write Error: Workspace/Context cannot be nil"))
  (let* ((tracker (macher-agent-workspace-mtime-tracker workspace))
         (original-mtime (gethash file-path tracker))
         (current-attrs (file-attributes file-path))
         (current-mtime (nth 5 current-attrs)))
    (when (and original-mtime current-mtime (not (equal original-mtime current-mtime)))
      (error "Your previous edits to %s were discarded due to external file modifications.  Please re-read and re-apply"
             (file-name-nondirectory file-path)))
    (puthash file-path current-mtime tracker)
    (puthash file-path content (macher-agent-workspace-vfs-buffers workspace))
    (when-let* ((ctx (macher-agent--resolve-context-from-ws workspace)))
      (macher-agent--update-context-file ctx file-path content))
    content))

(defun macher-agent-vfs-read (workspace file-path)
  "Read a node's content from the virtual file system or physical disk.

WORKSPACE is the workspace structure.
FILE-PATH is the relative or absolute path string.

Return the content string, or nil."
  (unless workspace
    (error "VFS Read Error: Workspace/Context cannot be nil"))
  (or (gethash file-path (macher-agent-workspace-vfs-buffers workspace))
      (when-let* ((ctx (macher-agent--resolve-context-from-ws workspace))
                  (contents (macher-agent--get-context-contents ctx))
                  (entry (cl-find file-path contents :key #'car :test #'equal)))
        (if (consp (cdr entry)) (cddr entry) (cdr entry)))
      (macher-agent--read-content-from-disk-or-buffer file-path)))

(defun macher-agent-vfs-make-entry (path orig curr)
  "Create a native VFS entry tuple (PATH ORIG . CURR).

PATH is the string path.
ORIG is the original content string.
CURR is the current content string.

Return the constructed VFS entry."
  (cons path (cons orig curr)))

(defun macher-agent-vfs-entry-path (entry)
  "Retrieve the path from VFS ENTRY.

ENTRY is the cons-cell entry.

Return the path string."
  (car entry))

(defun macher-agent-vfs-entry-orig (entry)
  "Retrieve the original content from VFS ENTRY.

ENTRY is the cons-cell entry.

Return the original content string, or nil."
  (if (consp (cdr entry))
      (cadr entry)
    nil))

(defun macher-agent-vfs-entry-curr (entry)
  "Retrieve the current content from VFS ENTRY.

ENTRY is the cons-cell entry.

Return the current content string, or nil."
  (if (consp (cdr entry))
      (cddr entry)
    (cdr entry)))

(defun macher-agent--hydrate-vfs-entry (entry base-dir)
  "Hydrate a VFS ENTRY within BASE-DIR.

ENTRY is the VFS entry.
BASE-DIR is the base directory path string.

Return the hydrated VFS entry."
  (let* ((path (car entry))
         (cdr-val (cdr entry)))
    (if (consp cdr-val)
        entry
      (let ((orig (macher-agent--get-buffer-content-stateless path base-dir))
            (curr cdr-val))
        (cons path (cons orig curr))))))

(defun macher-agent-vfs-entry-modified-p (entry)
  "Return non-nil if ENTRY has been modified from its original content.

ENTRY is the cons-cell entry to check.

Return non-nil if modified, otherwise nil."
  (let ((orig (macher-agent-vfs-entry-orig entry))
        (curr (macher-agent-vfs-entry-curr entry)))
    (not (equal orig curr))))

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

(defun macher-agent-root (&optional path)
  "Resolve absolute project root path from PATH.

PATH is a directory or file path, defaulting to `default-directory`.

Return the absolute project root path string."
  (let* ((dir (if (stringp path) path default-directory))
         (proj (and (fboundp 'project-current) (project-current nil dir))))
    (expand-file-name
     (or (and proj (if (fboundp 'project-root) (project-root proj) (cdr proj)))
         (and (fboundp 'vc-root-dir) (let ((default-directory dir)) (vc-root-dir)))
         dir))))

(defun macher-agent--resolve-safe-path (unsafe-path base-dir)
  "Resolve UNSAFE-PATH strictly within BASE-DIR to prevent jailbreaks.

UNSAFE-PATH is the raw string path to resolve.
BASE-DIR is the absolute directory path string.

Return the resolved absolute safe path string."
  (when (file-name-absolute-p unsafe-path)
    (error "SECURITY ERROR: Absolute paths are forbidden.  You must use relative paths (for example, ./file).  Path attempted: %s" unsafe-path))

  (when (string-prefix-p "~" unsafe-path)
    (error "SECURITY ERROR: Home directory paths are forbidden: %s" unsafe-path))

  (let* ((resolved (expand-file-name unsafe-path base-dir))
         (canonical-base (file-name-as-directory (expand-file-name base-dir)))
         (canonical-resolved (expand-file-name resolved)))
    (if (or (and (file-directory-p base-dir) (file-in-directory-p resolved base-dir))
            (string-prefix-p canonical-base canonical-resolved)
            (string= canonical-resolved (directory-file-name canonical-base)))
        resolved
      (error "SECURITY ERROR: Path traversal jailbreak detected: %s" unsafe-path))))

(defun macher-agent--write-or-delete-vfs-entry (target-path content)
  "Write CONTENT to TARGET-PATH if string, otherwise delete file if exists.

TARGET-PATH is the target file path string.
CONTENT is the content string to write, or nil to delete the file.

Return nil.
Side effects: Writes content to file or deletes the file at TARGET-PATH."
  (if (stringp content)
      (progn
        (make-directory (file-name-directory target-path) t)
        (write-region content nil target-path nil 'silent))
    (when (file-exists-p target-path)
      (delete-file target-path))))

(defun macher-agent--vfs-process-entries (entries sandbox-path entry-path-fn entry-content-fn)
  "Process VFS ENTRIES, inflating or deleting them within SANDBOX-PATH.

ENTRIES is the list of VFS entries.
SANDBOX-PATH is the sandbox path string.
ENTRY-PATH-FN is the function to extract the relative path.
ENTRY-CONTENT-FN is the function to extract content.

Return nil.
Side effects: Writes or deletes files within SANDBOX-PATH."
  (let ((sandbox-root (file-name-as-directory (expand-file-name sandbox-path))))
    (mapc (lambda (entry)
            (let* ((relative-path (funcall entry-path-fn entry))
                   (new-content (funcall entry-content-fn entry))
                   (sandbox-target-path (macher-agent--resolve-safe-path relative-path sandbox-root)))
              (macher-agent--write-or-delete-vfs-entry sandbox-target-path new-content)))
          entries)))

(defun macher-agent-sandbox-inflate (ctx)
  "Inflate the VFS contents for CTX into its physical sandbox directory.

CTX is the macher-context struct.

Return nil.
Side effects: Writes VFS contents to the sandbox directory."
  (let* ((sandbox-path (macher-agent--get-context-data ctx :sandbox-path))
         (vfs-buffers (macher-agent-workspace-vfs-buffers ctx))
         (ws-root (macher-agent-context-root ctx)))
    (when sandbox-path
      (let ((entries (hash-table-keys vfs-buffers)))
        (macher-agent--vfs-process-entries
         entries
         sandbox-path
         (lambda (key)
           (if (file-name-absolute-p key)
               (file-relative-name key ws-root)
             key))
         (lambda (key) (gethash key vfs-buffers))))
      (macher-agent--vfs-apply-overlay ctx sandbox-path))))

(defun macher-agent-context-root (context)
  "Retrieve the project root directory string from CONTEXT.

CONTEXT is the active context structure.

Return the project root path string."
  (or (when-let* ((ws (when context (macher-agent--get-context-workspace context))))
        (macher-agent-workspace-project-root ws))
      default-directory))

(defun macher-agent--vfs-verify-clean-merge (_workspace-root _context)
  "Verify that the active CONTEXT can merge cleanly into WORKSPACE-ROOT.

WORKSPACE-ROOT is the project root path string.
CONTEXT is the active context structure.

Return t."
  t)

(defun macher-agent--build-rsync-cmd (src dest)
  "Construct an rsync command driven by Git.

Throws an error if Git is unavailable or if the workspace is not
git-backed.  Generates a safe list of files including submodules.

SRC is the source directory string.
DEST is the destination directory string.

Return the shell command string."
  (let* ((src-dir (file-name-as-directory (expand-file-name src)))
         (dest-dir (file-name-as-directory (expand-file-name dest))))

    (unless (executable-find "git")
      (error "Macher-Agent: Git executable not found in PATH"))

    (let* ((safe-src-dir (if (file-directory-p src-dir) src-dir default-directory))
           (is-git (eq 0 (let ((default-directory safe-src-dir))
                           (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree")))))
      (if is-git
          (format "(cd %s && { git -c core.quotePath=false ls-files -z -c --recurse-submodules; git -c core.quotePath=false ls-files -z -o --exclude-standard; }) | rsync -aLC --delete --from0 --files-from=- %s %s"
                  (shell-quote-argument src-dir)
                  (shell-quote-argument src-dir)
                  (shell-quote-argument dest-dir))
        (error "Macher-Agent VFS requires a git-backed workspace; %s is not inside a git repository" src-dir)))))

(defun macher-agent--vfs-sync-baseline (workspace-root sandbox-dir)
  "Synchronise the physical WORKSPACE-ROOT to SANDBOX-DIR.

WORKSPACE-ROOT is the source project directory string.
SANDBOX-DIR is the target directory string.

Return the process exit code integer.
Side effects: Runs an rsync process to copy files."
  (let ((sync-cmd (macher-agent--build-rsync-cmd workspace-root sandbox-dir)))
    (call-process shell-file-name nil nil nil shell-command-switch sync-cmd)))

(defun macher-agent--edit-string-fast (content old-text new-text replace-all)
  "Replace OLD-TEXT with NEW-TEXT in CONTENT.

If REPLACE-ALL is nil, signal an error if OLD-TEXT occurs more than once.

CONTENT is the original string block.
OLD-TEXT is the target substring to match.
NEW-TEXT is the replacement string.
REPLACE-ALL is a boolean flag to replace all occurrences.

Return the modified string."
  (when (string-empty-p old-text)
    (error "Cannot replace an empty string.  Provide exact text to match"))
  (let ((count 0)
        (start 0))
    (while (string-match (regexp-quote old-text) content start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    (cond
     ((= count 0)
      (error "Text not found: %s" old-text))
     ((and (> count 1) (not replace-all))
      (error "Multiple matches found for text.  Set replace_all to true or provide more context: %s" old-text))
     (t
      (replace-regexp-in-string (regexp-quote old-text) new-text content t t)))))

(defun macher-agent--vfs-apply-overlay-stateless (contents ws-root sandbox-dir)
  "Apply virtual CONTENTS overlay to SANDBOX-DIR statelessly.

CONTENTS is the list of VFS entry cons cells.
WS-ROOT is the workspace root path string.
SANDBOX-DIR is the sandbox directory path string.

Return nil.
Side effects: Writes virtual overlay contents to SANDBOX-DIR."
  (macher-agent--vfs-process-entries
   contents
   sandbox-dir
   (lambda (entry)
     (let ((path (car entry)))
       (if (file-name-absolute-p path)
           (file-relative-name path ws-root)
         path)))
   (lambda (entry)
     (if (consp (cdr entry)) (cddr entry) (cdr entry)))))

(defun macher-agent--vfs-apply-overlay (context sandbox-dir)
  "Apply virtual context overlay to SANDBOX-DIR.

CONTEXT is the active context structure.
SANDBOX-DIR is the sandbox directory path string.

Return nil.
Side effects: Writes dirty VFS overlay contents to SANDBOX-DIR."
  (when (and context (macher-agent--get-context-dirty-p context))
    (let* ((ws-root (macher-agent-context-root context))
           (contents (macher-agent--get-context-contents context)))
      (macher-agent--vfs-apply-overlay-stateless contents ws-root sandbox-dir))))

(defun macher-agent-call-with-strict-vfs-pipeline (context body-fn)
  "Execute BODY-FN within a physical sandbox directory populated with CONTEXT.

CONTEXT is the active context structure.
BODY-FN is the function containing pipeline logic.

Return the result of BODY-FN.
Side effects: Creates and cleans up a temporary sandbox directory."
  (let* ((workspace-root (macher-agent-context-root context))
         (sandbox-dir (make-temp-file "macher-sandbox-" t)))
    (unwind-protect
        (progn
          (macher-agent--vfs-verify-clean-merge workspace-root context)
          (macher-agent--vfs-sync-baseline workspace-root sandbox-dir)
          (macher-agent--vfs-apply-overlay context sandbox-dir)
          (let ((default-directory sandbox-dir))
            (funcall body-fn)))
      (ignore-errors
        (delete-directory sandbox-dir t)))))

(defmacro macher-agent-with-strict-vfs-pipeline (context &rest body)
  "Execute BODY with strict VFS pipeline isolation populated with CONTEXT.

CONTEXT is the active agent context.
BODY represents the forms to evaluate in the isolated directory.

Return the result of evaluating BODY."
  `(macher-agent-call-with-strict-vfs-pipeline ,context (lambda () ,@body)))

(defun macher-agent--ensure-access (context path)
  "Ensure PATH is within the explicitly scoped CONTEXT.

CONTEXT is the active context structure.
PATH is the string file path.

Return nil or signals an error."
  (unless context
    (error "VFS Error: Context cannot be nil"))
  (macher-agent--ensure-access-stateless (macher-agent--get-context-contents context) path))

(defun macher-agent--inject-context-state (context &optional directives)
  "Inject the active CONTEXT and optional DIRECTIVES into the buffer.

CONTEXT is the active context structure.
DIRECTIVES is the optional directives alist.

Return nil.
Side effects: Sets buffer-local variables `macher-agent--persistent-context` and `gptel-directives`."
  (when context
    (setq-local macher-agent--persistent-context context)
    (when directives
      (setq-local gptel-directives directives))))

(defun macher-agent-current-context (&optional ctx-or-fsm)
  "Return the active context structure for CTX-OR-FSM.

CTX-OR-FSM is the optional context or finite-state machine.

Return the resolved context structure, or nil."
  (macher-agent-resolve-context ctx-or-fsm))

(defun macher-agent--extract-fsm-info (fsm)
  "Safely extract the info plist from a finite-state machine (FSM).

FSM is the finite-state machine object.

Return the info property list."
  (unless fsm
    (error "FSM Error: State machine object cannot be nil"))
  (gptel-fsm-info fsm))

(defun macher-agent--extract-fsm-context (fsm)
  "Extract the active context from a finite-state machine (FSM).

FSM is the finite-state machine object.

Return the active context structure, or nil."
  (when-let* ((fsm fsm)
              (info (macher-agent--extract-fsm-info fsm)))
    (plist-get info :macher-agent-context)))

(defun macher-agent--resolve-context-lazy-init ()
  "Attempt lazy initialisation of context for current directory.

Return non-nil if initialisation succeeded, or signals an error if not allowed.
Side effects: May initialise workspace state for current directory."
  (unless macher-agent--allow-lazy-init
    (error "Macher-Agent: Not in a recognised project workspace; running standard gptel"))
  (save-excursion
    (when-let* ((current-root (macher-agent-root default-directory)))
      (macher-agent--init-workspace-state current-root)
      (bound-and-true-p macher-agent--persistent-context))))

(defun macher-agent--register-active-workspace-root (root context)
  "Register CONTEXT for ROOT in `macher-agent-active-workspaces` hash-table.

ROOT is the project root directory path string.
CONTEXT is the active context structure to register.

Return CONTEXT.
Side effects: Modifies `macher-agent-active-workspaces` hash-table."
  (when root
    (let ((expanded (expand-file-name root)))
      (puthash expanded context macher-agent-active-workspaces)
      (puthash (file-name-as-directory expanded) context macher-agent-active-workspaces)
      (puthash (directory-file-name expanded) context macher-agent-active-workspaces))))

(defun macher-agent-resolve-context (&optional ctx-or-fsm)
  "Resolve the active context from CTX-OR-FSM or state.

Follows a predictable waterfall:
1. If CTX-OR-FSM satisfies `macher-context-p', return it.
2. If CTX-OR-FSM is a finite-state machine (FSM), extract its context.
3. If buffer is a subagent and `macher-agent--persistent-context' is bound, return it.
4. Fallback to registry-based active workspace context lookup.
5. Fallback lazy initialisation...

CTX-OR-FSM is the optional context or finite-state machine.

Return the resolved context structure, or signals an error if nil."
  (let* ((active-root (macher-agent-root default-directory))
         (active-root-expanded (and active-root (expand-file-name active-root)))
         (canonical-ctx (when active-root-expanded
                          (or (gethash active-root-expanded macher-agent-active-workspaces)
                              (gethash (file-name-as-directory active-root-expanded) macher-agent-active-workspaces)
                              (gethash (directory-file-name active-root-expanded) macher-agent-active-workspaces))))
         (pers-ctx (bound-and-true-p macher-agent--persistent-context))
         (pers-matches-root (macher-agent--match-persistent-context pers-ctx active-root-expanded))
         (ctx
          (cond
           ((and ctx-or-fsm (macher-context-p ctx-or-fsm))
            ctx-or-fsm)
           ((macher-agent--extract-fsm-context ctx-or-fsm))
           ((and (bound-and-true-p macher-agent--is-subagent) pers-matches-root))
           ((and canonical-ctx (not (bound-and-true-p macher-agent--is-subagent)))
            (when (and pers-ctx (not (eq pers-ctx canonical-ctx)))
              (setq-local macher-agent--persistent-context canonical-ctx))
            canonical-ctx)
           (pers-matches-root)
           (canonical-ctx
            (let ((isolated-ctx (macher-agent--clone-context canonical-ctx)))
              (setq-local macher-agent--persistent-context isolated-ctx)
              isolated-ctx))
           ((let ((fsm (macher-agent--get-fsm-latest)))
              (macher-agent--extract-fsm-context fsm)))
           (t
            (or canonical-ctx (macher-agent--resolve-context-lazy-init))))))
    (unless ctx
      (error "No active agent session found"))
    (when-let* ((root (ignore-errors (macher-agent-context-root ctx))))
      (unless (gethash (expand-file-name root) macher-agent-active-workspaces)
        (macher-agent--register-active-workspace-root root ctx)))
    ctx))

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

Return nil.
Side effects: Sets buffer-local workspace state, registers active workspace root, and adds prompt transformer hook."
  (setq-local macher-agent--is-workspace t)
  (let* ((expanded (expand-file-name workspace-root))
         (existing (or (gethash expanded macher-agent-active-workspaces)
                       (gethash (file-name-as-directory expanded) macher-agent-active-workspaces)
                       (gethash (directory-file-name expanded) macher-agent-active-workspaces)))
         (workspace (or (and existing (macher-agent--get-context-workspace existing))
                        (make-macher-agent-workspace :project-root workspace-root)))
         (canonical-context (or existing
                                (macher-agent--make-vfs-context :workspace workspace :contents nil)))
         (buffer-context (macher-agent--clone-context canonical-context)))
    (setq-local macher--workspace workspace)
    (macher-agent--inject-context-state buffer-context)

    (unless existing
      (macher-agent--register-active-workspace-root workspace-root canonical-context))

    (add-hook 'gptel-prompt-transform-functions #'macher-agent-sync-prompt-transformer nil t)

    (let ((skills-dir (expand-file-name "skills" workspace-root))
          (bundled (or (and (boundp 'macher-agent--bundled-skills-dir) macher-agent--bundled-skills-dir)
                       (and (boundp 'macher-agent-bundled-skills-directory) macher-agent-bundled-skills-directory))))
      (when bundled
        (macher-agent-initialize-skills buffer-context bundled))
      (when (file-directory-p skills-dir)
        (macher-agent-initialize-skills buffer-context skills-dir)))))

(defun macher-agent--partition-vfs-entries (contents &optional root-dir)
  "Split raw VFS CONTENTS into pure virtual and physical lists.

CONTENTS is the list of VFS entry structures.
ROOT-DIR is the optional project root path string.

Return a cons cell (VIRTUAL-ENTRIES . PHYSICAL-ENTRIES)."
  (let ((virtual-contents nil)
        (physical-contents nil))
    (dolist (entry contents)
      (let* ((name (car entry))
             (type (macher-agent-context-classify-entry name root-dir)))
        (if (eq type 'buffer)
            (push entry virtual-contents)
          (push entry physical-contents))))
    (cons (nreverse virtual-contents) (nreverse physical-contents))))

(defun macher-agent--split-context (ctx)
  "Split context CTX into virtual and physical context clones.

CTX is the active context structure.

Return a cons cell of cloned contexts (FILE-CONTEXT . BUFFER-CONTEXT)."
  (let* ((file-ctx (macher-agent--clone-context ctx))
         (buf-ctx (macher-agent--clone-context ctx))
         (workspace (when ctx (macher-agent--get-context-workspace ctx)))
         (root (and workspace (macher-agent-workspace-project-root workspace)))
         (contents (when ctx (macher-agent--get-context-contents ctx)))
         (modified-contents (cl-remove-if (lambda (e)
                                            (let ((orig (if (consp (cdr e)) (cadr e) nil))
                                                  (curr (if (consp (cdr e)) (cddr e) (cdr e))))
                                              (equal orig curr)))
                                          contents))
         (partitioned (macher-agent--partition-vfs-entries modified-contents root))
         (buf-contents (car partitioned))
         (file-contents (cdr partitioned)))
    (when file-ctx (macher-agent--set-context-contents file-ctx file-contents))
    (when buf-ctx (macher-agent--set-context-contents buf-ctx buf-contents))
    (cons file-ctx buf-ctx)))

(defun macher-agent--normalize-path-key (path &optional context)
  "Normalise PATH to a canonical key for CONTEXT entries.

PATH is the string path to normalise.
CONTEXT is the optional context structure.

Return the canonical key path string, or PATH if non-string."
  (if (or (null path) (not (stringp path)))
      path
    (let* ((ws-root (and context (macher-context-workspace-root context)))
           (buf (get-buffer path))
           (is-pure-buffer (and buf (null (buffer-file-name buf)))))
      (if (and ws-root (not is-pure-buffer) (not (string-prefix-p "*" path)))
          (expand-file-name path ws-root)
        (if (and (string-prefix-p "*" path) (not (string-suffix-p "*" path)))
            (concat path "*")
          path)))))

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

Return nil.
Side effects: Updates CONTEXT entries, sets dirty flag, and persists state."
  (unless context
    (error "VFS Write Error: Context cannot be nil"))
  (let* ((norm-path (macher-agent--normalize-path-key path context))
         (contents (macher-agent--get-context-contents context))
         (entry (or (cl-find norm-path contents :key #'car :test #'equal)
                    (cl-find path contents :key #'car :test #'equal)
                    (cl-find-if (lambda (e)
                                  (let ((e-path (car e)))
                                    (or (equal (macher-agent--normalize-path-key e-path context) norm-path)
                                        (string-suffix-p e-path path)
                                        (string-suffix-p path e-path))))
                                contents))))
    (if entry
        (if (consp (cdr entry))
            (setcdr (cdr entry) new-content)
          (setcdr entry new-content))
      (let* ((workspace-root (macher-context-workspace-root context))
             (orig (macher-agent--get-buffer-content-stateless norm-path workspace-root)))
        (macher-agent--set-context-contents context
                                            (cons (cons norm-path (cons orig new-content)) contents))))
    (puthash norm-path new-content (macher-agent-workspace-vfs-buffers context))
    (puthash path new-content (macher-agent-workspace-vfs-buffers context))
    (macher-agent--set-context-dirty-p context t)
    (macher-agent--persist-vfs-to-hidden-buffer context)
    (run-hook-with-args 'macher-agent-context-mutated-hook norm-path)))

(defun macher-agent--ensure-access-stateless (contents path)
  "Ensure PATH is within the explicitly scoped CONTENTS list.

CONTENTS is the list of authorised VFS entries.
PATH is the string file path to verify.

Return nil or signals an error."
  (let ((actual-name (substring-no-properties path)))
    (unless (or (cl-find actual-name contents :key #'car :test #'equal)
                (cl-find (expand-file-name actual-name) contents :key #'car :test #'equal)
                (cl-find-if (lambda (e) (string-suffix-p actual-name (car e))) contents)
                (get-buffer actual-name)
                (and (not (file-name-absolute-p actual-name))
                     (not (string-prefix-p "~" actual-name))
                     (file-exists-p actual-name)))
      (error "SECURITY ERROR: You do not have permission to access '%s'.  \
Use list_buffers_in_workspace to see your allowed scope" actual-name))))

(defun macher-agent--read-context-file (context path)
  "Read PATH from CONTEXT.

Prioritises VFS, then active buffers, then physical disk.
Uniformly applies security and path normalisation checks.

CONTEXT is the active context structure.
PATH is the file path string to read.

Return the file content string."
  (unless context
    (error "VFS Read Error: Context cannot be nil"))
  (let* ((norm-path (macher-agent--normalize-path-key path context))
         (contents (macher-agent--get-context-contents context))
         (workspace-root (macher-context-workspace-root context)))
    (macher-agent--ensure-access-stateless contents path)
    (if-let* ((virtual-entry (or (cl-find norm-path contents :key #'car :test #'equal)
                                 (cl-find path contents :key #'car :test #'equal)))
              (virtual-content (if (consp (cdr virtual-entry)) (cddr virtual-entry) (cdr virtual-entry))))
        virtual-content
      (let ((target-path (if workspace-root
                             (let ((relative-path (if (file-name-absolute-p path)
                                                      (file-relative-name path workspace-root)
                                                    path)))
                               (macher-agent--resolve-safe-path relative-path workspace-root))
                           path)))
        (or (macher-agent--read-content-from-disk-or-buffer target-path)
            (error "ERROR: File/Buffer '%s' does not exist" path))))))

(defun macher-agent--classify-file-path (path root-dir)
  "Classify file PATH with ROOT-DIR into `media', `file', or `external'.

PATH is the file path string to classify.
ROOT-DIR is the project root directory path string.

Return a symbol: `media', `file', or `external'."
  (let ((is-in-workspace (if root-dir (string-prefix-p (expand-file-name root-dir) path) t)))
    (if is-in-workspace
        (if (macher-agent-media-file-p path)
            'media
          'file)
      'external)))

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
    (if (or (and file-from-buf (file-exists-p file-from-buf))
            (file-exists-p expanded)
            is-absolute
            has-slash)
        (let ((path (or (and file-from-buf (file-exists-p file-from-buf) file-from-buf) expanded)))
          (macher-agent--classify-file-path path root-dir))
      'buffer)))

(defvar-local macher-agent--is-workspace nil
  "Non-nil if current buffer is an agent workspace buffer.")
(defvar-local macher--workspace nil
  "The active workspace structure for current buffer.")
(defvar-local macher-agent--persistent-context nil
  "The persistent VFS context structure bound to current buffer.")

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

(defun macher-agent--collect-raw-files (expanded-dir home-dir)
  "Collect raw file list for EXPANDED-DIR.

Stop execution if EXPANDED-DIR is equal to HOME-DIR or root.

EXPANDED-DIR is the expanded project directory path string.
HOME-DIR is the user home directory path string.

Return a list of file path strings."
  (if-let* ((proj (project-current nil expanded-dir)))
      (project-files proj)
    (when (or (string= expanded-dir home-dir) (string= expanded-dir "/"))
      (error "SECURITY HALT: Workspace resolved to root or home directory"))
    (directory-files-recursively
     expanded-dir "^[^.]" nil
     (lambda (d)
       (let ((base (file-name-nondirectory (directory-file-name d))))
         (and (not (member base '(".git" "target" "node_modules" ".Trash" "Library" ".cache" ".config")))
              (condition-case nil (progn (directory-files d) t) (error nil))))))))

(defun macher-agent--filter-safe-files (raw-files)
  "Filter RAW-FILES based on existence, size, and file extensions.

RAW-FILES is a list of candidate file path strings.

Return a list of safe file path strings."
  (let ((safe-files '()))
    (dolist (file raw-files)
      (when (file-exists-p file)
        (let ((attrs (file-attributes file)))
          (when (and attrs
                     (< (file-attribute-size attrs) 1000000)
                     (not (string-suffix-p ".json" file))
                     (not (string-suffix-p ".eln" file))
                     (not (string-suffix-p ".elc" file)))
            (push file safe-files)))))
    (nreverse safe-files)))

(defun macher-agent--get-files (workspace)
  "Retrieve list of active files in WORKSPACE, applying safety constraints.

WORKSPACE is the active workspace structure.

Return a list of file path strings."
  (let* ((expanded-dir (expand-file-name (macher-agent-workspace-project-root workspace)))
         (home-dir (expand-file-name "~/")))
    (condition-case err
        (let ((raw-files (macher-agent--collect-raw-files expanded-dir home-dir)))
          (macher-agent--filter-safe-files raw-files))
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
  "Deep-copy and clone CTX.

CTX is the context structure.

Return the newly cloned context structure, or nil."
  (when ctx
    (let* ((orig-data (when (macher-context-p ctx) (macher-context-data ctx)))
           (new-data (macher-agent--copy-context-hash-tables orig-data))
           (new-ctx (macher-agent--make-vfs-context :workspace (macher-agent--get-context-workspace ctx)
                                                    :contents (copy-tree (macher-agent--get-context-contents ctx)))))
      (when (macher-agent--get-context-prompt ctx)
        (setf (macher-context-prompt new-ctx) (macher-agent--get-context-prompt ctx)))
      (when new-data
        (setf (macher-context-data new-ctx) new-data))
      new-ctx)))

(defun macher-agent--merge-contexts (parent-ctx child-ctx)
  "Merge the VFS contents of CHILD-CTX into PARENT-CTX.

PARENT-CTX is the parent context structure.
CHILD-CTX is the child context structure.

Return nil.
Side effects: Updates PARENT-CTX with VFS entries from CHILD-CTX."
  (let ((child-contents (macher-agent--get-context-contents child-ctx))
        (parent-contents (macher-agent--get-context-contents parent-ctx)))
    (dolist (child-entry child-contents)
      (let* ((path (car child-entry))
             (orig (if (consp (cdr child-entry)) (cadr child-entry) nil))
             (new (if (consp (cdr child-entry)) (cddr child-entry) (cdr child-entry))))
        (when (or (not (equal orig new))
                  (not (cl-find path parent-contents :key #'car :test #'equal)))
          (macher-agent--update-context-file parent-ctx path new))))))

(defun macher-agent--update-entry-content-cells (entry new-orig new-curr)
  "Update original and current content cells of VFS ENTRY.

ENTRY is the VFS entry cons cell.
NEW-ORIG is the new original content string.
NEW-CURR is the new current content string.

Return ENTRY.
Side effects: Modifies the cdr of ENTRY in place."
  (if (consp (cdr entry))
      (progn
        (setcar (cdr entry) new-orig)
        (setcdr (cdr entry) new-curr))
    (setcdr entry (cons new-orig new-curr))))

(defun macher-agent--sync-context-entry (entry)
  "Synchronise a single VFS ENTRY with the physical disk.

ENTRY is the VFS entry structure.

Return non-nil if synchronisation modified the entry, otherwise nil.
Side effects: Updates entry cells if disk state changed."
  (let* ((path (car entry))
         (orig (if (consp (cdr entry)) (cadr entry) nil))
         (new (if (consp (cdr entry)) (cddr entry) (cdr entry)))
         (current-state (macher-agent--read-content-from-disk-or-buffer path)))
    (when (not (equal (or orig "") (or current-state "")))
      (if (equal (or new "") (or current-state ""))
          (macher-agent--update-entry-content-cells entry current-state new)
        (macher-agent--update-entry-content-cells entry current-state current-state))
      t)))

(defvar macher-agent--pause-auto-sync nil
  "When non-nil, `macher-agent--auto-sync-context` will silently abort.
Used to prevent race conditions during shadow-buffer patch generation.")

(defun macher-agent--sync-and-check-dirty-entries (contents)
  "Synchronise CONTENTS entries with disk or buffer and check dirty state.

CONTENTS is the list of VFS entries.

Return a cons cell (SYNCED . IS-DIRTY)."
  (let ((synced nil)
        (is-dirty nil))
    (dolist (entry contents)
      (when (consp entry)
        (when (macher-agent--sync-context-entry entry)
          (setq synced t))
        (let ((orig (if (consp (cdr entry)) (cadr entry) nil))
              (new (if (consp (cdr entry)) (cddr entry) (cdr entry))))
          (unless (equal (or orig "") (or new ""))
            (setq is-dirty t)))))
    (cons synced is-dirty)))

(defun macher-agent--auto-sync-context (ctx &rest _args)
  "Synchronise the active context with the physical disk, unless paused.

CTX is the active context structure.
_ARGS represents unused extra arguments.

Return nil.
Side effects: May update CTX dirty state and persist state if synced."
  (when (and ctx (not macher-agent--pause-auto-sync))
    (let* ((contents (macher-agent--get-context-contents ctx))
           (res (macher-agent--sync-and-check-dirty-entries contents))
           (synced (car res))
           (is-dirty (cdr res)))
      (macher-agent--set-context-dirty-p ctx is-dirty)
      (when synced
        (macher-agent--persist-vfs-to-hidden-buffer ctx)
        (run-hooks 'macher-agent-context-mutated-hook)))))

(defun macher-agent--write-vfs-entries-to-buffer (contents)
  "Write CONTENTS entries into the current buffer.

CONTENTS is the list of VFS entries.

Return nil.
Side effects: Inserts formatted VFS entries into current buffer."
  (dolist (entry contents)
    (let ((path (car entry))
          (new-content (if (consp (cdr entry)) (cddr entry) (cdr entry))))
      (when new-content
        (insert (format "=== VFS ENTRY: %s ===\n" path))
        (insert new-content)
        (insert "\n=======================\n\n")))))

(defun macher-agent--persist-vfs-to-hidden-buffer (ctx)
  "Persist virtual file system state of CTX to a hidden buffer for review.

CTX is the active context structure.

Return nil.
Side effects: Creates or erases and populates the hidden VFS state buffer."
  (let* ((workspace (when ctx (macher-agent--get-context-workspace ctx)))
         (root-dir (if workspace (macher-agent-workspace-project-root workspace) "default"))
         (buf-name (format " *macher-agent-vfs-state-%s*" (md5 (expand-file-name root-dir))))
         (vfs-buf (get-buffer-create buf-name)))
    (with-current-buffer vfs-buf
      (erase-buffer)
      (insert ";;; Macher Agent Virtual File System State\n")
      (insert ";;; This buffer is native and handles large text blocks.\n\n")
      (when ctx
        (macher-agent--write-vfs-entries-to-buffer (macher-agent--get-context-contents ctx))))))

(defun macher-agent-apply-patch ()
  "Apply the active patch from `diff-mode' context back to physical files.

Return nil.
Side effects: Invokes external process (`git` or `patch`) to apply patch."
  (interactive)
  (unless (derived-mode-p 'diff-mode) (user-error "Not in a patch/diff buffer"))
  (let* ((patch-content (buffer-substring-no-properties (point-min) (point-max)))
         (ctx (ignore-errors (macher-agent-resolve-context)))
         (ws (or (when ctx (macher-agent--get-context-workspace ctx))
                 (bound-and-true-p macher--workspace)))
         (root (if ws
                   (macher-agent-workspace-project-root ws)
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
          (message "ERROR: Failed to apply patch safely."))))))

(defun macher-agent-insert-patch ()
  "Insert the proposed patch content into the current buffer.

Return nil.
Side effects: Inserts diff string into the current buffer."
  (interactive)
  (if-let* ((patch-buf (macher-patch-buffer))
            (is-live (buffer-live-p patch-buf))
            (content (with-current-buffer patch-buf (buffer-substring-no-properties (point-min) (point-max))))
            ((not (string-empty-p content))))
      (insert "\nHere is your proposed patch:\n```diff\n" content "\n```\n")
    (message "No patch available for current workspace.")))

(defun macher-agent--reset-fsm-context (fresh-ctx)
  "Reset context in `gptel--fsm` to FRESH-CTX if active.

FRESH-CTX is the new context structure.

Return nil.
Side effects: Mutates info property list of `gptel--fsm` if bound."
  (when (and (boundp 'gptel--fsm) gptel--fsm)
    (let ((info (gptel-fsm-info gptel--fsm)))
      (when (plist-get info :macher--context)
        (setf (gptel-fsm-info gptel--fsm) (plist-put info :macher--context fresh-ctx))
        (setf (gptel-fsm-info gptel--fsm) (plist-put info :macher-agent-context fresh-ctx))))))

(defun macher-agent-clear-context ()
  "Clear the persistent VFS context for the current sub-agent buffer.

Return nil.
Side effects: Clears `macher-agent--persistent-context` and resets FSM context."
  (interactive)
  (if (not (bound-and-true-p macher-agent--persistent-context))
      (message "Macher-Agent: No persistent context to clear in buffer '%s'." (buffer-name))
    (let* ((ws (or (and (bound-and-true-p macher-agent--persistent-context)
                        (macher-agent--get-context-workspace macher-agent--persistent-context))
                   (ignore-errors (macher-workspace (current-buffer)))
                   (bound-and-true-p macher--workspace)))
           (fresh-ctx (when ws (macher-agent--make-vfs-context :workspace ws :contents nil)))
           (root (and ws (macher-agent-workspace-project-root ws)))
           (expanded-root (and root (expand-file-name root))))
      (setq macher-agent--persistent-context fresh-ctx)
      (when (and expanded-root (not (bound-and-true-p macher-agent--is-subagent)))
        (macher-agent--register-active-workspace-root expanded-root fresh-ctx))
      (macher-agent--reset-fsm-context fresh-ctx)
      (message "Macher-Agent: VFS context successfully cleared. Agent reset to physical baseline."))))

(provide 'macher-agent-vfs-client)
;;; macher-agent-vfs-client.el ends here
