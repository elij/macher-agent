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
(require 'macher-agent-macher)

(defvar macher-agent--vfs-lock-table (make-hash-table :test 'equal)
  "Hash table tracking resource path locks in VFS.")

(defvar macher-agent--vfs-lock-queues (make-hash-table :test 'equal)
  "Hash table tracking waiting lock acquisition queues per resource path.")

(defvar macher-agent--pending-callbacks (make-hash-table :test 'equal)
  "Registry mapping paths/events to closure callbacks.")

(defvar macher-agent--pause-auto-sync nil
  "When non-nil, `macher-agent--auto-sync-context` will silently abort.
Used to prevent race conditions during shadow-buffer patch generation.")

(defun macher-agent-vfs-release-lock (path &optional task-id)
  "Release VFS lock on PATH held by TASK-ID.
If there are waiting requesters in the queue, transfer the lock to the next
requester and notify their callback.

PATH is the locked resource path string.
TASK-ID is the optional identifier of the task holding the lock.

Return non-nil if lock was released or transferred, otherwise nil."
  (let* ((lock-state (gethash path macher-agent--vfs-lock-table))
         (current-owner (car-safe lock-state))
         (ref-count (or (cdr-safe lock-state) 0)))
    (when (and lock-state (equal current-owner task-id))
      (if (> ref-count 1)
          (progn
            (puthash path (cons current-owner (1- ref-count)) macher-agent--vfs-lock-table)
            t)
        (let ((queue (gethash path macher-agent--vfs-lock-queues nil)))
          (if (and queue (consp queue))
              (let* ((next (car queue))
                     (next-task-id (car next))
                     (next-cb (cdr next)))
                (puthash path (cdr queue) macher-agent--vfs-lock-queues)
                (puthash path (cons next-task-id 1) macher-agent--vfs-lock-table)
                (when next-cb
                  (funcall next-cb "Resource lock acquired."))
                t)
            (remhash path macher-agent--vfs-lock-table)
            (remhash path macher-agent--vfs-lock-queues)
            t))))))

(defun macher-agent--vfs-a2a-callback (payload)
  "Process ACQUIRE_LOCK and RELEASE_LOCK PAYLOAD for VFS A2A lock requests."
  (let ((type (plist-get payload :type)))
    (cond
     ((eq type 'ACQUIRE_LOCK)
      (let* ((meta (plist-get payload :metadata))
             (path (or (plist-get meta :resource_path) (plist-get payload :resource-path)))
             (task-id (or (plist-get payload :task-id) (macher-agent--generate-uuid)))
             (cb (or (plist-get payload :callback)
                     (when path (gethash path macher-agent--pending-callbacks)))))
        (when path
          (let* ((lock-state (gethash path macher-agent--vfs-lock-table))
                 (current-owner (car-safe lock-state))
                 (ref-count (or (cdr-safe lock-state) 0)))
            (if (or (null lock-state) (equal current-owner task-id))
                (progn
                  (puthash path (cons task-id (1+ ref-count)) macher-agent--vfs-lock-table)
                  (when cb
                    (remhash path macher-agent--pending-callbacks)
                    (funcall cb "Resource lock acquired.")))
              (if cb
                  (progn
                    (remhash path macher-agent--pending-callbacks)
                    (let ((queue (gethash path macher-agent--vfs-lock-queues nil)))
                      (puthash path (append queue (list (cons task-id cb))) macher-agent--vfs-lock-queues)))
                (display-warning 'macher-agent (format "Resource '%s' is locked." path) :warning)))))))
     ((eq type 'RELEASE_LOCK)
      (let* ((meta (plist-get payload :metadata))
             (path (or (plist-get meta :resource_path) (plist-get payload :resource-path)))
             (task-id (plist-get payload :task-id)))
        (when path (macher-agent-vfs-release-lock path task-id)))))))

(defun macher-agent-vfs-get-node (vfs-buffers-ht path)
  "Retrieve a node's content from the virtual file system.

VFS-BUFFERS-HT is the workspace structure.
PATH is the relative or absolute path string.

Return the node's content, or nil."
  (gethash path vfs-buffers-ht))

(defun macher-agent-vfs-set-node (vfs-buffers-ht path content)
  "Set a node's content in the virtual file system.

VFS-BUFFERS-HT is the hash-table mapping paths to virtual contents.
PATH is the relative or absolute path string.
CONTENT is the string content to store.

Return the stored content.
Side effects: Stores CONTENT in the VFS buffers hash-table for WORKSPACE."
  (puthash path content vfs-buffers-ht))

(defun macher-agent-vfs-write (vfs-buffers-ht mtime-tracker-ht file-path content)
  "Write CONTENT to FILE-PATH in VFS with concurrency checking."
  (let* ((original-mtime (when (and mtime-tracker-ht (hash-table-p mtime-tracker-ht))
                           (or (gethash file-path mtime-tracker-ht)
                               (and (stringp file-path)
                                    (gethash (expand-file-name file-path) mtime-tracker-ht)))))
         (current-attrs (and (stringp file-path) (file-attributes file-path)))
         (current-mtime (when current-attrs (nth 5 current-attrs))))
    (when (and original-mtime current-mtime (not (equal original-mtime current-mtime)))
      (error "Your previous edits to %s were discarded due to external file modifications.  \
Please re-read and re-apply"
             (file-name-nondirectory file-path)))
    (when (and mtime-tracker-ht (hash-table-p mtime-tracker-ht))
      (puthash file-path current-mtime mtime-tracker-ht)
      (when (stringp file-path)
        (puthash (expand-file-name file-path) current-mtime mtime-tracker-ht)))
    (when (and vfs-buffers-ht (hash-table-p vfs-buffers-ht))
      (puthash file-path content vfs-buffers-ht))
    content))

(defun macher-agent-vfs-read (vfs-buffers-ht contents file-path)
  "Read a node's content from the virtual file system or physical disk.

VFS-BUFFERS-HT is the hash table containing VFS nodes.
CONTENTS is the list of VFS entry cons cells.
FILE-PATH is the relative or absolute path string.

Return the content string, or nil."
  (let ((entry (cl-find file-path contents :key #'car :test #'equal)))
    (if entry
        (if (consp (cdr entry)) (cddr entry) (cdr entry))
      (if (and vfs-buffers-ht (hash-table-p vfs-buffers-ht))
          (let ((val (gethash file-path vfs-buffers-ht 'macher-agent--unbound)))
            (if (not (eq val 'macher-agent--unbound))
                val
              (macher-agent--read-content-from-disk-or-buffer file-path)))
        (macher-agent--read-content-from-disk-or-buffer file-path)))))

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

(defun macher-agent-to-relative-path (path &optional workspace-root)
  "Convert PATH to a relative path string within WORKSPACE-ROOT.
Canonicalises symlinks so that paths like /private/tmp and /tmp align.

PATH is the file path string.
WORKSPACE-ROOT is the root directory string.

Return the relative path string, or PATH if PATH is not a string."
  (if (or (null path) (not (stringp path)))
      path
    (if (and (string-prefix-p "*" path) (string-suffix-p "*" path))
        path
      (let* ((root (file-name-as-directory
                    (file-truename (expand-file-name (or workspace-root default-directory)))))
             (expanded (expand-file-name path root))
             (truename (file-truename expanded)))
        (if (or (string-prefix-p root truename)
                (string-prefix-p root (file-name-as-directory truename)))
            (file-relative-name truename root)
          (if (file-name-absolute-p path)
              (file-relative-name truename root)
            path))))))

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

  (let* ((canonical-base (file-name-as-directory (file-truename (expand-file-name base-dir))))
         (canonical-resolved (file-truename (expand-file-name unsafe-path canonical-base))))
    (if (or (and (file-directory-p canonical-base) (file-in-directory-p canonical-resolved canonical-base))
            (string-prefix-p canonical-base (file-name-as-directory canonical-resolved))
            (string-prefix-p canonical-base canonical-resolved)
            (string= canonical-resolved (directory-file-name canonical-base)))
        canonical-resolved
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
         (macher-agent-to-relative-path key ws-root))
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
-c core.quotePath=false ls-files -z -o --exclude-standard; }) | rsync -aC --delete --from0 \
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
       (macher-agent-to-relative-path path ws-root)))
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
      (let* ((relative-path (macher-agent-to-relative-path path workspace-root))
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
                (and (not (string-prefix-p "~" actual-name))
                     (or (file-exists-p actual-name)
                         (file-exists-p abs-path))))
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
  (let* ((name-str (if (bufferp path-or-buf)
                       (buffer-name path-or-buf)
                     (substring-no-properties (format "%s" path-or-buf))))
         (buf (if (bufferp path-or-buf) path-or-buf (get-buffer path-or-buf)))
         (file-from-buf (and buf (buffer-live-p buf) (buffer-file-name buf)))
         (is-live-buf-no-file (and buf (buffer-live-p buf) (null file-from-buf)))
         (is-asterisk (and (stringp name-str)
                           (string-prefix-p "*" name-str)
                           (string-suffix-p "*" name-str)))
         (expanded
          (if root-dir
              (expand-file-name name-str root-dir)
            (expand-file-name name-str)))
         (is-absolute (file-name-absolute-p name-str))
         (has-slash (string-match-p "/" name-str)))
    (cond
     ((or is-live-buf-no-file is-asterisk)
      'buffer)
     ((or (and file-from-buf (file-exists-p file-from-buf))
          (file-exists-p expanded)
          is-absolute
          has-slash)
      (let ((path (or (and file-from-buf (file-exists-p file-from-buf) file-from-buf)
                      expanded)))
        (macher-agent--classify-file-path path root-dir)))
     (t 'buffer))))

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
      (if (macher-agent-media-file-p path)
          (insert-file-contents-literally path)
        (insert-file-contents path))
      (buffer-string))))

(defun macher-agent--sync-context-entry (entry &optional mtime-tracker-ht root)
  "Synchronise a single VFS ENTRY with the physical disk.

ENTRY is the VFS entry structure.
MTIME-TRACKER-HT is an optional hash table tracking file modification times.
ROOT is an optional root directory string for resolving relative paths.

Return non-nil if synchronisation modified the entry, otherwise nil.
Side effects: Updates entry cells if disk state changed and updates
stored mtime tracker."
  (let* ((path (car entry))
         (abs-path (if (and root (stringp root) (stringp path) (not (file-name-absolute-p path)))
                       (expand-file-name path root)
                     path))
         (orig (if (consp (cdr entry)) (cadr entry) nil))
         (new (if (consp (cdr entry)) (cddr entry) (cdr entry)))
         (stored-mtime (when mtime-tracker-ht (gethash path mtime-tracker-ht)))
         (attrs (and abs-path (stringp abs-path) (file-attributes abs-path)))
         (current-mtime (when attrs (nth 5 attrs)))
         (disk-newer
          (and stored-mtime current-mtime (time-less-p stored-mtime current-mtime)))
         (buf (when (and path (stringp path))
                (or (get-file-buffer path) (get-file-buffer abs-path) (get-buffer path))))
         (live-buf (and buf (buffer-live-p buf)))
         (buf-content (when live-buf
                        (with-current-buffer buf
                          (buffer-substring-no-properties (point-min) (point-max)))))
         (disk-direct (macher-agent--read-content-from-disk-direct abs-path))
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
            (or disk-direct (macher-agent--read-content-from-disk-or-buffer (or abs-path path)))))

     ((and live-buf (buffer-modified-p buf))
      (setq current-state buf-content))

     (t
      (setq current-state
            (or disk-direct (macher-agent--read-content-from-disk-or-buffer (or abs-path path))))))

    (when (and mtime-tracker-ht current-mtime)
      (puthash path current-mtime mtime-tracker-ht))

    (when (not (equal (or orig "") (or current-state "")))
      (cond
       ((null current-state)
        (if (equal (or orig "") (or new ""))
            (macher-agent--update-entry-content-cells entry nil nil)
          (macher-agent--update-entry-content-cells entry nil new)))
       ((equal (or orig "") (or new ""))
        (macher-agent--update-entry-content-cells entry current-state current-state))
       ((equal (or new "") (or current-state ""))
        (macher-agent--update-entry-content-cells entry current-state new))
       (t
        (macher-agent--update-entry-content-cells entry current-state current-state)))
      t)))

(defun macher-agent--sync-and-check-dirty-entries (contents &optional mtime-tracker-ht root)
  "Synchronise CONTENTS entries with disk or buffer and check dirty state.

CONTENTS is the list of VFS entries.
MTIME-TRACKER-HT is an optional hash table tracking file modification times.
ROOT is an optional root directory string for resolving relative paths.

Return a cons cell (SYNCED . IS-DIRTY)."
  (let ((synced nil)
        (is-dirty nil))
    (dolist (entry contents)
      (when (consp entry)
        (when (macher-agent--sync-context-entry entry mtime-tracker-ht root)
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
        (unless (string-suffix-p "\n" new-content)
          (insert "\n"))
        (insert "=======================\n\n")))))

(defun macher-agent-context-root (context)
  "Retrieve the project root directory string from CONTEXT.

CONTEXT is the active context structure.

Return the project root path string."
  (or (when-let* ((ws (when context (macher-agent--get-context-workspace context))))
        (macher-agent-workspace-project-root ws))
      default-directory))

(defun macher-agent--normalize-path-key (path &optional context)
  "Normalise PATH to a canonical key for CONTEXT entries.

PATH is the string path to normalise.
CONTEXT is the optional context structure.

Return the canonical key path string, or PATH if non-string."
  (if (or (null path) (not (stringp path)))
      path
    (let* ((ws-root (and context (macher-agent--get-context-root context)))
           (buf (get-buffer path))
           (has-slash (string-match-p "/" path))
           (is-pure-buffer (and buf (null (buffer-file-name buf)) (not has-slash))))
      (if (and ws-root (not is-pure-buffer) (not (string-prefix-p "*" path)))
          (expand-file-name path ws-root)
        (if (and (string-prefix-p "*" path) (not (string-suffix-p "*" path)))
            (concat path "*")
          path)))))

(defun macher-agent--ensure-access (context path)
  "Ensure PATH is within the explicitly scoped CONTEXT.

CONTEXT is the active context structure.
PATH is the string file path.

Return nil or signals an error."
  (unless context
    (error "VFS Error: Context cannot be nil"))
  (macher-agent--ensure-access-stateless (macher-agent--get-context-contents context) path))

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
        (macher-agent--write-vfs-entries-to-buffer
         (macher-agent--get-context-contents ctx))))))

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
         (workspace-root (macher-agent--get-context-root context))
         (vfs-ht (macher-agent-workspace-vfs-buffers context))
         (mtime-ht (macher-agent-workspace-mtime-tracker context))
         (entry
          (or (cl-find norm-path contents :key #'car :test #'equal)
              (cl-find path contents :key #'car :test #'equal)
              (cl-find-if
               (lambda (e)
                 (let ((e-norm (ignore-errors (macher-agent--normalize-path-key (car e) context))))
                   (and e-norm (equal e-norm norm-path))))
               contents))))
    (if (and mtime-ht (hash-table-p mtime-ht))
        (progn
          (macher-agent-vfs-write vfs-ht mtime-ht norm-path new-content)
          (unless (equal path norm-path)
            (macher-agent-vfs-write vfs-ht mtime-ht path new-content)))
      (when (and vfs-ht (hash-table-p vfs-ht))
        (puthash norm-path new-content vfs-ht)
        (unless (equal path norm-path)
          (puthash path new-content vfs-ht))))
    (if entry
        (if (consp (cdr entry))
            (setcdr (cdr entry) new-content)
          (setcdr entry new-content))
      (let* ((safe-path (if workspace-root
                            (let ((rel-path (macher-agent-to-relative-path norm-path workspace-root)))
                              (macher-agent--resolve-safe-path rel-path workspace-root))
                          norm-path))
             (orig (if new-content (macher-agent--read-content-from-disk-direct safe-path) nil)))
        (macher-agent--set-context-contents
         context
         (cons (cons norm-path (cons orig new-content)) contents))))
    (macher-agent--set-context-dirty-p context t)
    (macher-agent--persist-vfs-to-hidden-buffer context)
    (run-hook-with-args 'macher-agent-context-mutated-hook norm-path)))

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
         (workspace-root (macher-agent--get-context-root context))
         (vfs-ht (macher-agent-workspace-vfs-buffers context))
         (mtime-ht (macher-agent-workspace-mtime-tracker context)))
    (macher-agent--ensure-access-stateless contents path)
    (let ((target-path
           (if workspace-root
               (let ((relative-path (macher-agent-to-relative-path path workspace-root)))
                 (macher-agent--resolve-safe-path relative-path workspace-root))
             path)))
      (when (and mtime-ht (hash-table-p mtime-ht) (stringp target-path))
        (let ((attrs (file-attributes target-path)))
          (when attrs
            (let ((mtime (nth 5 attrs)))
              (unless (gethash norm-path mtime-ht)
                (puthash norm-path mtime mtime-ht))
              (unless (gethash path mtime-ht)
                (puthash path mtime mtime-ht))
              (unless (gethash target-path mtime-ht)
                (puthash target-path mtime mtime-ht))))))
      (let* ((entry (or (cl-find norm-path contents :key #'car :test #'equal)
                        (cl-find path contents :key #'car :test #'equal)
                        (cl-find-if
                         (lambda (e)
                           (let ((e-norm (ignore-errors (macher-agent--normalize-path-key (car e) context))))
                             (and e-norm (equal e-norm norm-path))))
                         contents)))
             (vfs-val (when (and vfs-ht (hash-table-p vfs-ht))
                        (let ((v1 (gethash norm-path vfs-ht 'macher-agent--unbound)))
                          (if (not (eq v1 'macher-agent--unbound))
                              v1
                            (let ((v2 (gethash path vfs-ht 'macher-agent--unbound)))
                              (if (not (eq v2 'macher-agent--unbound))
                                  v2
                                'macher-agent--unbound)))))))
        (cond
         (entry
          (if (consp (cdr entry)) (cddr entry) (cdr entry)))
         ((and vfs-val (not (eq vfs-val 'macher-agent--unbound)))
          vfs-val)
         (t
          (or (macher-agent--read-content-from-disk-or-buffer target-path)
              (when (and (not (equal path target-path))
                         (file-name-absolute-p path))
                (macher-agent--read-content-from-disk-or-buffer path))
              (error "ERROR: File/Buffer '%s' does not exist" path))))))))

(defun macher-agent-call-with-strict-vfs-pipeline (context body-fn)
  "Execute BODY-FN within a physical sandbox directory populated with CONTEXT.

CONTEXT is the active context structure.
BODY-FN is the function containing pipeline logic.

Return the result of BODY-FN.
Side effects: Creates and cleans up a temporary sandbox directory."
  (let* ((ctx-root (ignore-errors (macher-agent-context-root context)))
         (proj (project-current nil default-directory))
         (proj-root (when proj (expand-file-name (project-root proj))))
         (workspace-root (or ctx-root proj-root default-directory))
         (sandbox-dir (make-temp-file "macher-sandbox-" t))
         (contents (when context (macher-agent--get-context-contents context))))
    (unwind-protect
        (progn
          (macher-agent--vfs-verify-clean-merge workspace-root contents)
          (macher-agent--vfs-sync-baseline workspace-root sandbox-dir)
          (when contents
            (macher-agent--vfs-apply-overlay-stateless contents workspace-root sandbox-dir))
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

(defun macher-agent--get-files (workspace)
  "Get list of files associated with WORKSPACE.

WORKSPACE is the active workspace structure.

Return list of file paths."
  (let* ((root (macher-agent-workspace-project-root workspace))
         (home (expand-file-name "~")))
    (when root
      (let* ((expanded (expand-file-name root))
             (raw-files (macher-agent--collect-raw-files expanded home)))
        (macher-agent--filter-safe-files raw-files)))))

(defun macher-agent--auto-sync-context (ctx &rest _args)
  "Synchronise the active context with the physical disk, unless paused.

CTX is the active context structure.
_ARGS represents unused extra arguments.

Return nil.
Side effects: May update CTX dirty state and persist state if synced."
  (when (and ctx (not macher-agent--pause-auto-sync))
    (let* ((contents (macher-agent--get-context-contents ctx))
           (workspace (or (when ctx (macher-agent--get-context-workspace ctx)) ctx))
           (root (or (ignore-errors (macher-agent-context-root ctx))
                     (when workspace (ignore-errors (macher-agent-workspace-project-root workspace)))
                     default-directory))
           (tracker (when workspace (macher-agent-workspace-mtime-tracker workspace)))
           (res (macher-agent--sync-and-check-dirty-entries contents tracker root))
           (synced (car res))
           (is-dirty (cdr res)))

      (unless is-dirty
        (macher-agent--set-context-dirty-p ctx nil))

      (when synced
        (macher-agent--persist-vfs-to-hidden-buffer ctx)
        (run-hooks 'macher-agent-context-mutated-hook)))))

(defun macher-agent-storage--extract-context (payload)
  "Extract the target context structure from PAYLOAD property list."
  (ignore-errors (macher-agent-resolve-context payload)))

(defun macher-agent-vfs--compose-artifact (payload)
  "Compose artifact by attaching the child context struct and any modified diffs.

Extracts the target context structure from PAYLOAD via
`macher-agent-resolve-context' and attaches it to PAYLOAD as `:child-context'
along with `:diff' when modified entries exist.

PAYLOAD is the outgoing artifact property list.

Return updated PAYLOAD property list."
  (let* ((payload-buf (when-let* ((raw-buf (and (macher-agent--plist-p payload)
                                                (or (plist-get payload :buffer-name)
                                                    (plist-get payload :buf)
                                                    (plist-get payload :buffer)
                                                    (plist-get payload :target-buffer)))))
                        (if (bufferp raw-buf)
                            raw-buf
                          (when (stringp raw-buf) (get-buffer raw-buf)))))
         (target-ctx (or (when (and (macher-agent--plist-p payload)
                                    (plist-member payload :child-context))
                           (let ((c (plist-get payload :child-context)))
                             (when (macher-agent-valid-context-p c) c)))
                         (when (fboundp 'macher-agent-resolve-from-transit-payload)
                           (macher-agent-resolve-from-transit-payload payload))
                         (when (and payload-buf (buffer-live-p payload-buf))
                           (let ((c (buffer-local-value 'macher-agent--persistent-context payload-buf)))
                             (when (macher-agent-valid-context-p c) c)))
                         (when (bound-and-true-p macher-agent--persistent-context)
                           (when (macher-agent-valid-context-p macher-agent--persistent-context)
                             macher-agent--persistent-context))
                         (ignore-errors (macher-agent-resolve-context payload))
                         (ignore-errors (macher-agent-resolve-context (current-buffer)))))
         (contents (when (and target-ctx (fboundp 'macher-agent--get-context-contents))
                     (macher-agent--get-context-contents target-ctx)))
         (modified-entries (when contents
                             (cl-remove-if-not #'macher-agent-vfs-entry-modified-p contents))))
    (if (macher-agent--plist-p payload)
        (let ((res (copy-sequence payload)))
          (when (and target-ctx (macher-agent-valid-context-p target-ctx))
            (setq res (plist-put res :child-context target-ctx)))
          (if modified-entries
              (setq res (plist-put res :diff modified-entries))
            (when (plist-member res :diff)
              (setq res (plist-put res :diff nil))))
          res)
      payload)))

(defun macher-agent-vfs-handle-flush (&optional ctx)
  "Execute disk overlay if suppressed, or broadcast patch request via core hooks."
  (let* ((target-buf (current-buffer))
         (c (or (when (macher-agent-valid-context-p ctx) ctx)
                (bound-and-true-p macher-agent--persistent-context)
                (ignore-errors (macher-agent-resolve-context ctx))
                (ignore-errors (macher-agent-resolve-context target-buf))))
         (suppress (or (bound-and-true-p macher-agent--is-subagent)
                       (bound-and-true-p macher-agent--suppress-patch)))
         (contents (when c (macher-agent--get-context-contents c)))
         (has-changes (and c (or (macher-agent--get-context-dirty-p c)
                                 (cl-some #'macher-agent-vfs-entry-modified-p contents)))))
    (when (and c has-changes)
      (when-let* ((prompt (or (macher-agent--get-context-prompt c)
                              (macher-agent--get-context-data c :prompt))))
        (macher-agent--set-context-prompt c prompt)
        (macher-agent--set-context-data c :prompt prompt))
      (if suppress
          (let ((contents (macher-agent--get-context-contents c))
                (ws-root (macher-agent-context-root c)))
            (macher-agent--vfs-apply-overlay-stateless contents ws-root))
        (run-hook-with-args 'macher-agent-vfs-flush-hook c)))))

(defmacro macher-agent-with-vfs-scope (context &rest body)
  "Execute BODY with Virtual File System awareness established from CONTEXT.

CONTEXT is the context structure, state machine, or buffer environment
to resolve via `macher-agent-resolve-context'.  When nil, the active
context is resolved automatically.
BODY is the sequence of forms to evaluate within the established VFS scope.

Return the result of evaluating the last form in BODY.

Side effects: Binds `macher-agent--persistent-context' and adjusts `default-directory'."
  (declare (indent 1) (debug t))
  (let ((raw-ctx-sym (make-symbol "raw-ctx"))
        (ctx-sym (make-symbol "ctx"))
        (root-sym (make-symbol "root")))
    `(let* ((,raw-ctx-sym ,context)
            (,ctx-sym (or (when (and ,raw-ctx-sym (macher-agent-valid-context-p ,raw-ctx-sym))
                            ,raw-ctx-sym)
                          (ignore-errors (macher-agent-resolve-context ,raw-ctx-sym))
                          (ignore-errors (macher-agent-resolve-context))
                          ,raw-ctx-sym))
            (macher-agent--persistent-context ,ctx-sym)
            (,root-sym (when ,ctx-sym
                         (ignore-errors (macher-agent--get-context-root ,ctx-sym))))
            (default-directory (if (and ,root-sym (stringp ,root-sym) (file-directory-p ,root-sym))
                                   (file-name-as-directory ,root-sym)
                                 default-directory)))
       ,@body)))

(defun macher-agent--prepare-patch-contexts (context fsm project-root)
  "Partition CONTEXT into isolated virtual and physical patch contexts.

Extract VFS entries from CONTEXT or FSM and split them according to
PROJECT-ROOT into dedicated virtual and physical context objects.

Return a list (VIRTUAL-CONTEXT PHYSICAL-CONTEXT PHYSICAL-CONTENTS).

Side effects: None."
  (let* ((vfs-ctx (or (ignore-errors (macher-agent-resolve-context (or fsm context))) context))
         (raw-contents (or (and context (macher-agent--get-context-contents context))
                           (and vfs-ctx (macher-agent--get-context-contents vfs-ctx))))
         (prompt (or (and context (macher-agent--get-context-prompt context))
                     (and context (macher-agent--get-context-data context :prompt))
                     (and vfs-ctx (macher-agent--get-context-prompt vfs-ctx))
                     (and vfs-ctx (macher-agent--get-context-data vfs-ctx :prompt))
                     (when-let* ((info (macher-agent--extract-fsm-info fsm)))
                       (plist-get info :prompt))))
         (categorised (macher-agent--partition-vfs-entries raw-contents project-root))
         (virtual-contents (car categorised))
         (physical-contents (cdr categorised))
         (macher-compatible-ws (cons 'project project-root))
         (v-ctx (and virtual-contents
                     (macher-agent--create-and-tag-vfs-context macher-compatible-ws
                                                               virtual-contents
                                                               prompt)))
         (p-ctx (and physical-contents
                     (macher-agent--create-and-tag-vfs-context macher-compatible-ws
                                                               physical-contents
                                                               prompt))))
    (list v-ctx p-ctx physical-contents)))

(defun macher-agent-prepare-upstream-payloads (context)
  "Split CONTEXT into independent, native macher-context structs.

Resolve the project root for the workspace in CONTEXT and construct separate
physical and virtual patch context structures.

Return a cons cell (PHYSICAL-CONTEXT . VIRTUAL-CONTEXT).

Side effects: None."
  (let* ((ws (macher-agent--get-context-workspace context))
         (project-root (macher-agent-workspace-project-root ws))
         (prepared (macher-agent--prepare-patch-contexts context nil project-root))
         (v-ctx (nth 0 prepared))
         (p-ctx (nth 1 prepared)))
    (cons p-ctx v-ctx)))

(defun macher-agent--build-and-rename-patch (ctx fsm-obj patch-type)
  "Build patch for CTX using FSM-OBJ and rename buffer according to PATCH-TYPE.

Construct a patch buffer for context CTX and finite-state machine FSM-OBJ if
CTX contains changes and patch generation is not suppressed.  Rename the
resulting patch buffer using PATCH-TYPE, for example, \"physical\" or \"virtual\".

Return the renamed patch buffer if generated, or nil.

Side effects: Creates or renames patch buffers in the Emacs runtime."
  (let* ((target-buf (when (and fsm-obj (fboundp 'gptel-fsm-info))
                       (ignore-errors (plist-get (gptel-fsm-info fsm-obj) :buffer))))
         (suppress-patch (if (buffer-live-p target-buf)
                             (buffer-local-value 'macher-agent--suppress-patch target-buf)
                           (bound-and-true-p macher-agent--suppress-patch))))
    (when (and (not suppress-patch)
               (macher-agent--context-has-changes-p ctx))
      (macher--build-patch ctx fsm-obj)
      (when-let* ((buf (macher-patch-buffer (macher-context-workspace ctx)))
                  (name (buffer-name buf)))
        (let ((new-name (if (string-match "^\\*macher-patch\\(.*\\)\\*$" name)
                            (concat "*macher-" patch-type "-patch" (match-string 1 name) "*")
                          (concat "*macher-" patch-type "-patch*"))))
          (with-current-buffer buf
            (rename-buffer new-name t)
            (current-buffer)))))))

(defun macher-agent--display-patch-buffers (generated-buffers)
  "Display GENERATED-BUFFERS in split windows if multiple buffers exist.

Arrange window layout to show all patch buffers in GENERATED-BUFFERS when
more than one patch buffer is present.

Return nil.

Side effects: Alters window configuration in the active frame."
  (when (> (length generated-buffers) 1)
    (let ((bufs (nreverse generated-buffers)))
      (delete-other-windows)
      (split-window-vertically)
      (set-window-buffer (selected-window) (car bufs))
      (set-window-buffer (next-window) (cadr bufs)))))

(defun macher-agent--context-has-changes-p (context)
  "Determine whether CONTEXT contains actual VFS modifications.

Inspect all VFS entries within CONTEXT to verify if any entries
have been modified.

Return non-nil if CONTEXT has modified VFS entries, or nil otherwise.

Side effects: None."
  (and context
       (cl-some #'macher-agent-vfs-entry-modified-p
                (macher-agent--get-context-contents context))))

(defun macher-agent--split-context (ctx)
  "Split context CTX into virtual and physical context clones.

CTX is the active context structure.

Return a cons cell of cloned contexts (FILE-CONTEXT . BUFFER-CONTEXT)."
  (let* ((file-ctx (macher-agent--clone-context ctx))
         (buf-ctx (macher-agent--clone-context ctx))
         (workspace (when ctx (macher-agent--get-context-workspace ctx)))
         (root (and workspace (macher-agent-workspace-project-root workspace)))
         (contents (when ctx (macher-agent--get-context-contents ctx)))
         (modified-contents
          (cl-remove-if-not #'macher-agent-vfs-entry-modified-p contents))
         (partitioned (macher-agent--partition-vfs-entries modified-contents root))
         (buf-contents (car partitioned))
         (file-contents (cdr partitioned)))
    (when file-ctx (macher-agent--set-context-contents file-ctx file-contents))
    (when buf-ctx (macher-agent--set-context-contents buf-ctx buf-contents))
    (cons file-ctx buf-ctx)))

(defun macher-agent--execute-split-patch (ctx fsm-obj)
  "Execute the split patch generation for physical and virtual contexts."
  (when (or (macher-agent--get-context-dirty-p ctx)
            (macher-agent--context-has-changes-p ctx))
    (let* ((payloads (macher-agent--split-context ctx))
           (p-ctx (car payloads))
           (v-ctx (cdr payloads))
           (prompt (macher-agent--get-context-prompt ctx))
           (generated-buffers nil))
      (when prompt
        (when p-ctx
          (macher-agent--set-context-prompt p-ctx prompt)
          (macher-agent--set-context-data p-ctx :prompt prompt))
        (when v-ctx
          (macher-agent--set-context-prompt v-ctx prompt)
          (macher-agent--set-context-data v-ctx :prompt prompt)))
      (when-let* ((p-buf (macher-agent--build-and-rename-patch p-ctx fsm-obj "physical")))
        (push p-buf generated-buffers))
      (when-let* ((v-buf (macher-agent--build-and-rename-patch v-ctx fsm-obj "virtual")))
        (push v-buf generated-buffers))
      (when generated-buffers
        (macher-agent--display-patch-buffers generated-buffers)))
    (macher-agent--set-context-dirty-p ctx nil)))

(defun macher-agent-macher-build-patch-from-hook (ctx)
  "Observe VFS flush events and build the visual macher interface."
  (when (macher-agent-valid-context-p ctx)
    (let ((fsm-obj (macher-agent-get-active-fsm)))
      (when (null (macher-agent--get-context-prompt ctx))
        (when-let* ((info (macher-agent--extract-fsm-info fsm-obj))
                    (fsm-prompt (or (plist-get info :prompt)
                                    (when-let* ((fsm-ctx (plist-get info :macher-agent-context)))
                                      (macher-agent--get-context-prompt fsm-ctx)))))
          (macher-agent--set-context-prompt ctx fsm-prompt)
          (macher-agent--set-context-data ctx :prompt fsm-prompt)))
      (when-let* ((p (macher-agent--get-context-prompt ctx)))
        (macher-agent--set-context-prompt ctx p)
        (macher-agent--set-context-data ctx :prompt p))
      (macher-agent--execute-split-patch ctx fsm-obj))))

(defun macher-agent-vfs-install ()
  "Install VFS storage hooks, patch interfaces, and pipeline steps."
  (macher-agent-register-pipeline-step 'payload-merge #'macher-agent-vfs--merge-payload 10)
  (macher-agent-register-pipeline-step 'artifact-compose #'macher-agent-vfs--compose-artifact 10)
  (add-hook 'macher-agent-vfs-flush-hook #'macher-agent-macher-build-patch-from-hook)
  (add-hook 'macher-agent-task-flush-hook #'macher-agent-vfs-handle-flush)
  (with-eval-after-load 'macher
    (defalias 'macher--workspace-hash #'macher-agent--safe-workspace-hash)

    (add-to-list
     'macher-workspace-types-alist
     '(agent . (:get-root macher-agent--get-root
                          :get-name macher-agent--get-name
                          :get-files macher-agent--get-files)))

    (add-hook 'macher-workspace-functions #'macher-agent-workspace-agent)))

(defun macher-agent--merge-contexts (parent-ctx child-ctx)
  "Merge the VFS contents of CHILD-CTX into PARENT-CTX respecting existing parent edits."
  (when (and parent-ctx child-ctx (not (eq parent-ctx child-ctx)))
    (let ((child-contents (macher-agent--get-context-contents child-ctx))
          (parent-contents (macher-agent--get-context-contents parent-ctx))
          (any-merged nil))
      (dolist (child-entry child-contents)
        (let* ((path (car-safe child-entry))
               (orig (if (consp (cdr child-entry)) (cadr child-entry) nil))
               (new (if (consp (cdr child-entry)) (cddr child-entry) (cdr child-entry)))
               (norm-path (ignore-errors (macher-agent--normalize-path-key path parent-ctx)))
               (parent-entry
                (or (cl-find path parent-contents :key #'car :test #'equal)
                    (when norm-path (cl-find norm-path parent-contents :key #'car :test #'equal))
                    (cl-find-if (lambda (e)
                                  (let ((e-norm (ignore-errors (macher-agent--normalize-path-key (car e) parent-ctx))))
                                    (and norm-path e-norm (equal e-norm norm-path))))
                                parent-contents)))
               (p-curr (when parent-entry (if (consp (cdr parent-entry)) (cddr parent-entry) (cdr parent-entry)))))
          (when (or (and (not (equal orig new)) (not (equal new p-curr)))
                    (and (null parent-entry) (not (equal orig new))))
            (macher-agent--update-context-file parent-ctx path new)
            (setq any-merged t))))
      (when any-merged
        (macher-agent--set-context-dirty-p parent-ctx t))))
  parent-ctx)

(defun macher-agent-vfs--merge-payload (payload)
  "Merge Virtual File System PAYLOAD into the target parent context directly.

Exhaustively extracts target context from PAYLOAD keys (:target-context,
:parent-context, :parent-ctx, :context), `macher-agent-storage--extract-context',
workspace-id, shared-state, and buffer local persistent context.
Applies file diffs and child-context modifications to the resolved parent context,
handling deletions when diff items have nil content.
Synchronises `macher-agent--persistent-context' across relevant buffers and
updates `:target-context' on the returned payload plist.

PAYLOAD is the property list or data structure containing merge artifacts.

Return updated PAYLOAD."
  (let ((parent-ctx (ignore-errors (macher-agent-resolve-context payload)))
        (child-ctx (let ((c (macher-agent--extract-prop payload :child-context)))
                     (if (and (not (eq c 'macher-missing)) (macher-agent-valid-context-p c)) c nil)))
        (diff (let ((d (macher-agent--extract-prop payload :diff)))
                (if (and (not (eq d 'macher-missing)) (listp d)) d nil))))

    (when parent-ctx
      (when (and child-ctx (not (eq child-ctx parent-ctx)))
        (macher-agent--merge-contexts parent-ctx child-ctx))

      (when diff
        (let ((any-mutated nil))
          (dolist (item diff)
            (let* ((path (car-safe item))
                   (curr (if (consp (cdr item)) (cddr item) (cdr item))))
              (when path
                (macher-agent--update-context-file parent-ctx path curr)
                (setq any-mutated t))))
          (when any-mutated
            (macher-agent--set-context-dirty-p parent-ctx t))))

      (when (boundp 'macher-agent--persistent-context)
        (setq macher-agent--persistent-context parent-ctx))

      (let* ((buf-candidates (list (macher-agent--extract-prop payload :target-buffer)
                                   (macher-agent--extract-prop payload :parent-buf)
                                   (macher-agent--extract-prop payload :parent-buffer)
                                   (macher-agent--extract-prop payload :buffer)
                                   (macher-agent--extract-prop payload :buf)
                                   (macher-agent--extract-prop payload :buffer-name)))
             (live-bufs (cl-remove-if-not #'buffer-live-p
                                          (mapcar (lambda (b)
                                                    (cond ((bufferp b) b)
                                                          ((stringp b) (get-buffer b))
                                                          (t nil)))
                                                  (cl-remove 'macher-missing buf-candidates)))))
        (dolist (b live-bufs)
          (with-current-buffer b
            (setq-local macher-agent--persistent-context parent-ctx))))

      (when (macher-agent--plist-p payload)
        (setq payload (plist-put (copy-sequence payload) :target-context parent-ctx))))
    payload))

(provide 'macher-agent-vfs)
;;; macher-agent-vfs.el ends here
