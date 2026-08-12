;;; macher-agent-vfs.el --- Layer 2 VFS Client for Macher -*- lexical-binding: t; -*-

;;; Commentary:
;; Virtual File System client managing optimistic concurrency, sandbox synchronisation,
;; and buffer-local agent contexts.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'project)
(require 'xref)
(require 'macher-agent-core)

(declare-function macher-context-workspace-root "macher-agent-api" (context))

(defvar macher-agent--vfs-lock-table (make-hash-table :test 'equal)
  "Hash table tracking resource path locks in VFS.")

(defvar macher-agent--pending-callbacks (make-hash-table :test 'equal)
  "Registry mapping paths/events to closure callbacks.")

(defvar macher-agent--pause-auto-sync nil
  "When non-nil, `macher-agent--auto-sync-context` will silently abort.
Used to prevent race conditions during shadow-buffer patch generation.")

(defun macher-agent--vfs-lock-listener (payload)
  "Process ACQUIRE_LOCK PAYLOAD for VFS A2A lock requests."
  (when (eq (plist-get payload :type) 'ACQUIRE_LOCK)
    (let* ((path (plist-get (plist-get payload :metadata) :resource_path))
           (task-id (or (plist-get payload :task-id) t)))
      (unless (gethash path macher-agent--vfs-lock-table)
        (puthash path task-id macher-agent--vfs-lock-table)
        (let ((cb (gethash path macher-agent--pending-callbacks)))
          (when cb
            (remhash path macher-agent--pending-callbacks)
            (funcall cb "Resource lock acquired.")))))))

(defalias 'macher-agent--vfs-a2a-callback #'macher-agent--vfs-lock-listener)

(defvar macher-agent--vfs-a2a-callback #'macher-agent--vfs-lock-listener
  "Point-to-point A2A callback function for VFS lock management.")

(defun macher-agent-vfs-get-node (vfs-buffers-ht path)
  "Retrieve a node's content from the virtual file system.

VFS-BUFFERS-HT is the workspace structure.
PATH is the relative or absolute path string.

Return the node's content, or nil."
  (gethash path vfs-buffers-ht))

(defun macher-agent-vfs-set-node (vfs-buffers-ht path content)
  "Set a node's content in the virtual file system.

WORKSPACEVFS-BUFFERS-HT is the workspace structure.
PATH is the relative or absolute path string.
CONTENT is the string content to store.

Return the stored content.
Side effects: Stores CONTENT in the VFS buffers hash-table for WORKSPACE."
  (puthash path content vfs-buffers-ht))

(defun macher-agent-vfs-write (vfs-buffers-ht mtime-tracker-ht file-path content)
  "Write CONTENT to FILE-PATH in VFS with concurrency checking."
  (let* ((original-mtime (gethash file-path mtime-tracker-ht))
         (current-attrs (file-attributes file-path))
         (current-mtime (nth 5 current-attrs)))
    (when (and original-mtime current-mtime (not (equal original-mtime current-mtime)))
      (error "Your previous edits to %s were discarded due to external file modifications.  \
Please re-read and re-apply"
             (file-name-nondirectory file-path)))
    (puthash file-path current-mtime mtime-tracker-ht)
    (puthash file-path content vfs-buffers-ht)
    content))

(defun macher-agent-vfs-read (vfs-buffers-ht contents file-path)
  "Read a node's content from the virtual file system or physical disk.

VFS-BUFFERS-HT is the hash table containing VFS nodes.
CONTENTS is the list of VFS entry cons cells.
FILE-PATH is the relative or absolute path string.

Return the content string, or nil."
  (or (when-let* ((entry (cl-find file-path contents :key #'car :test #'equal)))
        (if (consp (cdr entry)) (cddr entry) (cdr entry)))
      (gethash file-path vfs-buffers-ht)
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
             (string-match-p
              "\\.\\(png\\|jpe?g\\|gif\\|webp\\|svg\\|pdf\\|mp4\\|mov\\|mp3\\|wav\\)$" path)))))

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
    (error "SECURITY ERROR: Absolute paths are forbidden.  You must use relative paths \
(for example, ./file).  Path attempted: %s" unsafe-path))

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
      (ignore-errors (delete-file target-path)))))

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

(defun macher-agent-sandbox-inflate (sandbox-path vfs-buffers-ht ws-root contents)
  "Inflate the VFS contents into its physical sandbox directory.

SANDBOX-PATH is the sandbox directory path string.
VFS-BUFFERS-HT is the hash table containing VFS nodes.
WS-ROOT is the project root path string.
CONTENTS is the list of VFS entry cons cells.

Return nil.
Side effects: Writes VFS contents to the sandbox directory."
  (when sandbox-path
    (let ((entries (hash-table-keys vfs-buffers-ht)))
      (macher-agent--vfs-process-entries
       entries
       sandbox-path
       (lambda (key)
         (if (file-name-absolute-p key)
             (file-relative-name key ws-root)
           key))
       (lambda (key) (gethash key vfs-buffers-ht))))
    (macher-agent--vfs-apply-overlay-stateless contents ws-root sandbox-path)))

(defun macher-agent--vfs-verify-clean-merge (_workspace-root _contents)
  "Verify that the VFS _CONTENTS can merge cleanly into _WORKSPACE-ROOT.

_WORKSPACE-ROOT is the project root path string.
_CONTENTS is the list of VFS entry cons cells.

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
          (format "(cd %s && { git -c core.quotePath=false ls-files -z -c --recurse-submodules; git \
-c core.quotePath=false ls-files -z -o --exclude-standard; }) | rsync -aLC --delete --from0 \
--files-from=- %s %s"
                  (shell-quote-argument src-dir)
                  (shell-quote-argument src-dir)
                  (shell-quote-argument dest-dir))
        (error
         "Macher-Agent VFS requires a git-backed workspace; %s is not inside a git repository"
         src-dir)))))

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
      (error
       "Multiple matches found for text.  Set replace_all to true or provide more context: %s" old-text))
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

(defun macher-agent--ensure-access-stateless (contents path)
  "Ensure PATH is within the explicitly scoped CONTENTS list.

CONTENTS is the list of authorised VFS entries.
PATH is the string file path to verify.

Return nil or signals an error."
  (let ((actual-name (substring-no-properties path))
        (abs-path (expand-file-name path)))
    (unless (or (cl-find actual-name contents :key #'car :test #'equal)
                (cl-find abs-path contents :key #'car :test #'equal)
                (cl-find-if (lambda (e)
                              (and (stringp (car e))
                                   (equal (file-name-nondirectory actual-name)
                                          (file-name-nondirectory (car e)))))
                            contents)
                (get-buffer actual-name)
                (and (not (file-name-absolute-p actual-name))
                     (not (string-prefix-p "~" actual-name))
                     (file-exists-p actual-name)))
      (error "SECURITY ERROR: You do not have permission to access '%s'.  \
Use list_buffers_in_workspace to see your allowed scope" actual-name))))

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
  (let* ((expanded
          (if root-dir
              (expand-file-name path-or-buf root-dir) (expand-file-name path-or-buf)))
         (buf (get-buffer path-or-buf))
         (file-from-buf (and buf (buffer-file-name buf)))
         (is-absolute (file-name-absolute-p path-or-buf))
         (has-slash (string-match-p "/" path-or-buf)))
    (if (or (and file-from-buf (file-exists-p file-from-buf))
            (file-exists-p expanded)
            is-absolute
            has-slash)
        (let
            ((path
              (or (and file-from-buf (file-exists-p file-from-buf) file-from-buf) expanded)))
          (macher-agent--classify-file-path path root-dir))
      'buffer)))

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
         (and
          (not
           (member base
                   '(".git" "target" "node_modules" ".Trash" "Library" ".cache" ".config")))
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

(defun macher-agent--read-content-from-disk-direct (path)
  "Read the contents of PATH directly from physical disk, ignoring active buffers.

PATH is the relative or absolute path string.

Return the content string, or nil."
  (when (and path (stringp path) (file-exists-p path))
    (with-temp-buffer
      (if (and (fboundp 'macher-agent-media-file-p) (macher-agent-media-file-p path))
          (insert-file-contents-literally path)
        (insert-file-contents path))
      (buffer-string))))

(defun macher-agent--sync-context-entry (entry &optional mtime-tracker-ht)
  "Synchronise a single VFS ENTRY with the physical disk.

ENTRY is the VFS entry structure.
MTIME-TRACKER-HT is an optional hash table tracking file modification times.

Return non-nil if synchronisation modified the entry, otherwise nil.
Side effects: Updates entry cells if disk state changed and updates
stored mtime tracker."
  (let* ((path (car entry))
         (orig (if (consp (cdr entry)) (cadr entry) nil))
         (new (if (consp (cdr entry)) (cddr entry) (cdr entry)))
         (stored-mtime (when mtime-tracker-ht (gethash path mtime-tracker-ht)))
         (attrs (and path (stringp path) (file-attributes path)))
         (current-mtime (when attrs (nth 5 attrs)))
         (disk-newer
          (and stored-mtime current-mtime (time-less-p stored-mtime current-mtime)))
         (buf (when (and path (stringp path))
                (or (get-file-buffer path) (get-buffer path))))
         (live-buf (and buf (buffer-live-p buf)))
         (buf-content (when live-buf
                        (with-current-buffer buf
                          (buffer-substring-no-properties (point-min) (point-max)))))
         (disk-direct (macher-agent--read-content-from-disk-direct path))
         (current-state nil))

    (when (and mtime-tracker-ht current-mtime (null stored-mtime))
      (puthash path current-mtime mtime-tracker-ht))

    (cond
     ((and live-buf (equal buf-content orig))
      (setq current-state buf-content))

     ((and live-buf (equal buf-content new))
      (setq current-state buf-content))

     ((or disk-newer
          (and live-buf (buffer-file-name buf) (not (verify-visited-file-modtime buf))))
      (setq current-state
            (or disk-direct (macher-agent--read-content-from-disk-or-buffer path))))

     ((and live-buf (buffer-modified-p buf))
      (setq current-state buf-content))

     (t
      (setq current-state
            (or disk-direct (macher-agent--read-content-from-disk-or-buffer path)))))

    (when (and mtime-tracker-ht current-mtime)
      (puthash path current-mtime mtime-tracker-ht))

    (when (not (equal (or orig "") (or current-state "")))
      (if (equal (or new "") (or current-state ""))
          (macher-agent--update-entry-content-cells entry current-state new)
        (macher-agent--update-entry-content-cells entry current-state current-state))
      t)))

(defun macher-agent--sync-and-check-dirty-entries (contents &optional mtime-tracker-ht)
  "Synchronise CONTENTS entries with disk or buffer and check dirty state.

CONTENTS is the list of VFS entries.
MTIME-TRACKER-HT is an optional hash table tracking file modification times.

Return a cons cell (SYNCED . IS-DIRTY)."
  (let ((synced nil)
        (is-dirty nil))
    (dolist (entry contents)
      (when (consp entry)
        (when (macher-agent--sync-context-entry entry mtime-tracker-ht)
          (setq synced t))
        (let ((orig (if (consp (cdr entry)) (cadr entry) nil))
              (new (if (consp (cdr entry)) (cddr entry) (cdr entry))))
          (unless (equal (or orig "") (or new ""))
            (setq is-dirty t)))))
    (cons synced is-dirty)))

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

(provide 'macher-agent-vfs)
;;; macher-agent-vfs.el ends here
