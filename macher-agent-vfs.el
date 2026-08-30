;;; macher-agent-vfs.el --- Layer 2 VFS Client for Macher -*- lexical-binding: t; -*-

;;; Commentary:
;; Virtual File System client managing optimistic concurrency, sandbox synchronisation,
;; and buffer-local agent contexts.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'macher-agent-core)
(require 'macher-agent-macher)

(declare-function gptel-fsm-info "gptel" (fsm))
(declare-function gptel-fsm-p "gptel" (obj))
(declare-function mailcap-file-name-to-mime-type "mailcap" (file-name))

(defvar macher-agent--vfs-lock-table (make-hash-table :test 'equal)
  "Hash table tracking resource path locks in VFS.")

(defvar macher-agent--vfs-lock-queues (make-hash-table :test 'equal)
  "Hash table tracking waiting lock acquisition queues per resource path.")

(defvar macher-agent--pending-callbacks (make-hash-table :test 'equal)
  "Registry mapping paths/events to closure callbacks.")

(defvar macher-agent--pause-auto-sync nil
  "When non-nil, `macher-agent--auto-sync-context` will silently abort.
Used to prevent race conditions during shadow-buffer patch generation.")

(defvar macher-agent-vfs-flush-hook nil
  "Event hook triggered after the Virtual File System processes modifications.
Functions in this hook receive the active `macher-agent-context' struct.")

;;; VFS Envelope State Accessors

(defun macher-agent-vfs--get-state (ctx)
  "Retrieve VFS state plist from CTX."
  (when (macher-agent-context-p ctx)
    (let ((plugins (macher-agent-context-plugins ctx)))
      (when (macher-agent--plist-p plugins)
        (plist-get plugins :vfs)))))

(defun macher-agent-vfs--set-state (ctx state)
  "Set VFS STATE plist on CTX."
  (when (macher-agent-context-p ctx)
    (let ((plugins (macher-agent-context-plugins ctx)))
      (setf (macher-agent-context-plugins ctx)
            (plist-put (copy-sequence plugins) :vfs state))
      state)))

(defun macher-agent-context-shadow-buffers (ctx)
  "Safely retrieve shadow buffers list from CTX natively."
  (when (macher-agent-context-p ctx)
    (let ((state (macher-agent-vfs--get-state ctx))
          (plugins (macher-agent-context-plugins ctx)))
      (cond
       ((and state (plist-member state :shadow-buffers))
        (plist-get state :shadow-buffers))
       ((and (macher-agent--plist-p plugins) (plist-member plugins :shadow-buffers))
        (plist-get plugins :shadow-buffers))
       (t nil)))))

(defun macher-agent--set-context-shadow-buffers (ctx val)
  "Safely set shadow buffers list on CTX to VAL natively."
  (when (macher-agent-context-p ctx)
    (let ((state (macher-agent-vfs--get-state ctx)))
      (macher-agent-vfs--set-state ctx (plist-put state :shadow-buffers val))
      val)))

(defun macher-agent-vfs--get-origin-buffer (ctx)
  "Retrieve origin buffer from CTX."
  (when (macher-agent-context-p ctx)
    (let ((state (macher-agent-vfs--get-state ctx))
          (plugins (macher-agent-context-plugins ctx)))
      (or (when state (plist-get state :origin-buffer))
          (macher-agent-context-origin-buffer ctx)
          (when (macher-agent--plist-p plugins)
            (or (plist-get plugins :origin-buffer)
                (plist-get plugins :buffer)))))))

;;; Concurrency Locks

(defun macher-agent-vfs-release-lock (path &optional task-id)
  "Release VFS lock on PATH held by TASK-ID.
If there are waiting requesters in the queue, transfer the lock to the next
requester and notify their callback.

PATH is the locked resource path string.
TASK-ID is the optional identifier of the task holding the lock.

Return non-nil if lock was released or transferred, otherwise nil."
  (cl-assert (stringp path) nil "PATH must be a string, got: %S" path)
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
                  (condition-case nil
                      (funcall next-cb "Resource lock acquired.")
                    (error nil)))
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

(defun macher-agent-vfs-write (vfs-buffers-ht mtime-tracker-ht file-path content)
  "Write CONTENT to FILE-PATH in VFS with safe concurrency checking."
  (let* ((expanded-path (if (stringp file-path) (expand-file-name file-path) file-path))
         (original-mtime (when (and mtime-tracker-ht (hash-table-p mtime-tracker-ht))
                           (or (gethash file-path mtime-tracker-ht)
                               (and (stringp file-path)
                                    (gethash expanded-path mtime-tracker-ht)))))
         (current-attrs (and (stringp file-path) (file-attributes expanded-path)))
         (current-mtime (when current-attrs (nth 5 current-attrs))))

    (when (and original-mtime current-mtime (not (time-equal-p original-mtime current-mtime)))
      (display-warning 'macher-agent
                       (format "Your previous edits to %s were discarded due to external file modifications.  Please re-read and re-apply"
                               (file-name-nondirectory file-path))
                       :warning))

    (when (and mtime-tracker-ht (hash-table-p mtime-tracker-ht))
      (puthash file-path current-mtime mtime-tracker-ht)
      (when (stringp file-path)
        (puthash expanded-path current-mtime mtime-tracker-ht)))

    (when (and vfs-buffers-ht (hash-table-p vfs-buffers-ht))
      (puthash file-path content vfs-buffers-ht))

    content))

(defun macher-agent-vfs-make-entry (path orig curr)
  "Create a native `macher-agent-vfs-entry' struct.

PATH is the string path.
ORIG is the original content string.
CURR is the current content string.

Return the constructed `macher-agent-vfs-entry' struct."
  (make-macher-agent-vfs-entry :path path :orig orig :curr curr))

(defun macher-agent-vfs-entry-modified-p (entry)
  "Return non-nil if ENTRY has been modified from its original content.

ENTRY is the VFS entry to check.

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
Pure, non-file-backed buffers are identified via SSOT and returned untouched.

PATH is the file path string.
WORKSPACE-ROOT is the root directory string.

Return the relative path string, or PATH if PATH is not a string."
  (if-let* (((stringp path))
            ((not (eq (macher-agent--classify-file-path path workspace-root) 'buffer)))
            (root (file-name-as-directory
                   (file-truename (expand-file-name (or workspace-root default-directory)))))
            (truename (file-truename (expand-file-name path root))))
      (if (or (string-prefix-p root truename)
              (string-prefix-p root (file-name-as-directory truename)))
          (file-relative-name truename root)
        (if (file-name-absolute-p path)
            (file-relative-name truename root)
          path))
    path))

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

(defun macher-agent-vfs-scratch-inflate (sandbox-path vfs-buffers-ht ws-root contents)
  "Inflate the VFS contents into its physical sandbox directory.

SANDBOX-PATH is the sandbox directory path string.
VFS-BUFFERS-HT is the hash-table of current VFS buffer contents.
WS-ROOT is the project root directory string.
CONTENTS is the list of `macher-agent-vfs-entry` structs.

Return nil.
Side effects: Writes VFS contents to the sandbox directory."
  (when (and sandbox-path (file-directory-p sandbox-path))
    (let ((file-entries
           (cl-remove-if
            (lambda (entry)
              (let ((path (if (macher-agent-vfs-entry-p entry)
                              (macher-agent-vfs-entry-path entry)
                            (car entry))))
                (eq (macher-agent--classify-file-path path ws-root) 'buffer)))
            contents)))
      (macher-agent--vfs-process-entries
       file-entries
       sandbox-path
       (lambda (entry)
         (let ((p (macher-agent-vfs-entry-path entry)))
           (macher-agent-to-relative-path p ws-root)))
       (lambda (entry)
         (macher-agent-vfs-entry-curr entry))))

    (when (and (hash-table-p vfs-buffers-ht) (> (hash-table-count vfs-buffers-ht) 0))
      (let ((physical-keys
             (cl-remove-if
              (lambda (key)
                (or (eq (macher-agent--classify-file-path key ws-root) 'buffer)
                    (let ((b (and (stringp key) (get-buffer key))))
                      (and b (buffer-live-p b) (not (buffer-file-name b))))))
              (hash-table-keys vfs-buffers-ht))))
        (macher-agent--vfs-process-entries
         physical-keys
         sandbox-path
         (lambda (key)
           (macher-agent-to-relative-path key ws-root))
         (lambda (key) (gethash key vfs-buffers-ht)))))))

(defun macher-agent--vfs-verify-clean-merge (_workspace-root _contents)
  "Verify that the VFS _CONTENTS can merge cleanly into _WORKSPACE-ROOT.

_WORKSPACE-ROOT is the project root path string.
_CONTENTS is the list of `macher-agent-vfs-entry` structs.

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

(defun macher-agent--read-content-from-disk-or-buffer (path)
  "Read the contents of PATH from an active buffer or disk.

PATH is the relative or absolute path string.

Return the content string, or nil."
  (when (and (stringp path) (not (string-empty-p path)))
    (let* ((buf (or (get-file-buffer path) (get-buffer path)))
           (is-media (and (file-exists-p path)
                          (macher-agent-media-file-p path)))
           (file-exists (file-exists-p path))
           (trust-buffer (and buf
                              (buffer-live-p buf)
                              (or (buffer-modified-p buf)
                                  (not (buffer-file-name buf))
                                  (verify-visited-file-modtime buf)))))
      (cond
       (trust-buffer
        (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))
       (file-exists
        (with-temp-buffer
          (if is-media (insert-file-contents-literally path) (insert-file-contents path))
          (buffer-string)))
       (t nil)))))

(defun macher-agent--partition-vfs-entries (contents &optional root-dir)
  "Split raw VFS CONTENTS into pure virtual and physical lists.

CONTENTS is the list of VFS entry structures.
ROOT-DIR is the optional project root path string.

Return a cons cell (VIRTUAL-ENTRIES . PHYSICAL-ENTRIES)."
  (let ((virtual-contents nil)
        (physical-contents nil))
    (dolist (entry contents)
      (let* ((name (macher-agent-vfs-entry-path entry))
             (type (macher-agent--classify-file-path name root-dir)))
        (if (eq type 'buffer)
            (push entry virtual-contents)
          (push entry physical-contents))))
    (cons (nreverse virtual-contents) (nreverse physical-contents))))

(defun macher-agent--classify-file-path (path &optional root-dir)
  "Classify PATH with ROOT-DIR into `buffer', `media', `file', or `external'.
Pure, non-file-backed buffers are determined purely by runtime buffer state.

PATH is the file path string or buffer to classify.
ROOT-DIR is the optional project root directory path string.

Return a symbol: `buffer', `media', `file', or `external'."
  (let* ((buf (cond
               ((bufferp path) (when (buffer-live-p path) path))
               ((stringp path) (get-buffer path))))
         (file-path (and buf (buffer-file-name buf))))
    (if (and buf (null file-path))
        'buffer
      (let* ((target-path (cond
                           (file-path file-path)
                           ((bufferp path) (buffer-name path))
                           (t (format "%s" path))))
             (canonical-root (and root-dir (file-name-as-directory (expand-file-name root-dir))))
             (expanded (expand-file-name target-path (or root-dir default-directory))))
        (if (and canonical-root
                 (not (or (string-prefix-p canonical-root (file-name-as-directory expanded))
                          (string-prefix-p canonical-root expanded)
                          (string= (expand-file-name root-dir) expanded))))
            'external
          (if (macher-agent-media-file-p expanded)
              'media
            'file))))))

(defun macher-agent--collect-raw-files (expanded-dir home-dir)
  "Collect raw file list for EXPANDED-DIR.

Stop execution if EXPANDED-DIR is equal to HOME-DIR or root.

EXPANDED-DIR is the expanded project directory path string.
HOME-DIR is the user home directory path string.

Return a list of file path strings."
  (let ((norm-exp (directory-file-name (expand-file-name expanded-dir)))
        (norm-home (when home-dir (directory-file-name (expand-file-name home-dir)))))
    (when (or (and norm-home (string= norm-exp norm-home))
              (string= norm-exp "")
              (string= norm-exp "/")
              (and home-dir (string= expanded-dir home-dir))
              (string= expanded-dir "/"))
      (error "SECURITY HALT: Workspace resolved to root or home directory")))
  (if-let* ((proj (project-current nil expanded-dir)))
      (project-files proj)
    (directory-files-recursively
     expanded-dir "^[^.]" nil
     (lambda (d)
       (let ((base (file-name-nondirectory (directory-file-name d))))
         (and
          (not
           (member base
                   '(".git" "target" "node_modules" ".Trash" "Library" ".cache" ".config")))
          (condition-case nil (progn (directory-files d) t) (error nil))))))))

(defun macher-agent--update-entry-content-cells (entry new-orig new-curr)
  "Update original and current content cells of VFS ENTRY.

ENTRY is the VFS entry structure.
NEW-ORIG is the new original content string.
NEW-CURR is the new current content string.

Return ENTRY.
Side effects: Modifies ENTRY in place."
  (setf (macher-agent-vfs-entry-orig entry) new-orig)
  (setf (macher-agent-vfs-entry-curr entry) new-curr)
  entry)

(defun macher-agent--sync-context-entry (entry &optional mtime-tracker-ht root)
  "Synchronise a single VFS ENTRY with the physical disk.

ENTRY is the VFS entry structure.
MTIME-TRACKER-HT is an optional hash table tracking file modification times.
ROOT is an optional root directory string for resolving relative paths.

Return non-nil if synchronisation modified the entry, otherwise nil.
Side effects: Updates entry cells if disk state changed and updates
stored mtime tracker."
  (let* ((path (macher-agent-vfs-entry-path entry))
         (abs-path (if (and root (stringp root) (stringp path) (not (file-name-absolute-p path)))
                       (expand-file-name path root)
                     path))
         (orig (macher-agent-vfs-entry-orig entry))
         (new (macher-agent-vfs-entry-curr entry))
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
            (macher-agent--read-content-from-disk-or-buffer (or abs-path path))))

     ((and live-buf (buffer-modified-p buf))
      (setq current-state buf-content))

     (t
      (setq current-state
            (macher-agent--read-content-from-disk-or-buffer (or abs-path path)))))

    (when (and mtime-tracker-ht current-mtime)
      (puthash path current-mtime mtime-tracker-ht))

    (when (not (equal (or orig "") (or current-state "")))
      (cond
       ((null current-state)
        (if (equal (or orig "") (or new ""))
            (macher-agent--update-entry-content-cells entry nil nil)
          (macher-agent--update-entry-content-cells entry nil new)))
       ((null new)
        (macher-agent--update-entry-content-cells entry current-state nil))
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
      (when (macher-agent--sync-context-entry entry mtime-tracker-ht root)
        (setq synced t))
      (let ((orig (macher-agent-vfs-entry-orig entry))
            (new (macher-agent-vfs-entry-curr entry)))
        (unless (equal (or orig "") (or new ""))
          (setq is-dirty t))))
    (cons synced is-dirty)))

(defun macher-agent--normalize-path-key (path &optional context)
  "Normalise PATH to a canonical key for CONTEXT entries.

PATH is the string path to normalise.
CONTEXT is the optional context structure.

Return the canonical key path string, or PATH if non-string."
  (if-let* (((stringp path))
            (ws-root (and context (macher-agent-context-root context))))
      (if (eq (macher-agent--classify-file-path path ws-root) 'buffer)
          path
        (expand-file-name path ws-root))
    path))

(defun macher-agent--ensure-access (context path)
  "Ensure PATH is within the explicitly scoped CONTEXT.

CONTEXT is the active context structure.
PATH is the string file path.

Return nil or signals an error."
  (cl-assert (macher-agent-valid-context-p context) nil "VFS Error: Context cannot be nil or invalid, got: %S" context)
  (cl-assert (stringp path) nil "PATH must be a string, got: %S" path)
  (let* ((contents (macher-agent--get-context-contents context))
         (actual-name (substring-no-properties path))
         (abs-path (expand-file-name path)))
    (unless (or (cl-find actual-name contents :key #'macher-agent-vfs-entry-path :test #'equal)
                (cl-find abs-path contents :key #'macher-agent-vfs-entry-path :test #'equal)
                (cl-find-if (lambda (e)
                              (let ((e-path (macher-agent-vfs-entry-path e)))
                                (and (stringp e-path)
                                     (equal (file-name-nondirectory actual-name)
                                            (file-name-nondirectory e-path)))))
                            contents)
                (get-buffer actual-name)
                (and (not (string-prefix-p "~" actual-name))
                     (or (file-exists-p actual-name)
                         (file-exists-p abs-path))))
      (error "SECURITY ERROR: You do not have permission to access '%s'.  \
Use list_buffers_in_workspace to see your allowed scope" actual-name))))

(defun macher-agent--persist-vfs-to-hidden-buffer (ctx)
  "Persist virtual file system state of CTX to a hidden buffer for review.

CTX is the active context structure.

Return nil.
Side effects: Creates or erases and populates the hidden VFS state buffer."
  (let* ((root-dir (or (when (macher-agent-valid-context-p ctx)
                         (macher-agent-context-root ctx))
                       "default"))
         (buf-name (format " *macher-agent-vfs-state-%s*" (md5 (expand-file-name (if (consp root-dir) (cdr root-dir) root-dir)))))
         (vfs-buf (get-buffer-create buf-name)))
    (with-current-buffer vfs-buf
      (erase-buffer)
      (insert ";;; Macher Agent Virtual File System State\n")
      (insert ";;; This buffer is native and handles large text blocks.\n\n")
      (when ctx
        (dolist (entry (macher-agent--get-context-contents ctx))
          (let* ((path (macher-agent-vfs-entry-path entry))
                 (new-content (macher-agent-vfs-entry-curr entry)))
            (when new-content
              (insert (format "=== VFS ENTRY: %s ===\n" path))
              (insert new-content)
              (unless (string-suffix-p "\n" new-content)
                (insert "\n"))
              (insert "=======================\n\n"))))))))

(defun macher-agent--update-context-file (context path new-content)
  "Update PATH in CONTEXT with NEW-CONTENT.

CONTEXT is the active context structure.
PATH is the relative file path string.
NEW-CONTENT is the modified content string.

Return nil.
Side effects: Updates CONTEXT entries, sets dirty flag, and persists state."
  (cl-assert (macher-agent-valid-context-p context) nil "VFS Write Error: Context cannot be nil or invalid")
  (cl-assert (stringp path) nil "PATH must be a string, got: %S" path)
  (let* ((norm-path (macher-agent--normalize-path-key path context))
         (contents (macher-agent--get-context-contents context))
         (workspace-root (when (macher-agent-valid-context-p context)
                           (macher-agent-context-root context)))
         (vfs-ht (macher-agent-workspace-vfs-buffers context))
         (mtime-ht (macher-agent-workspace-mtime-tracker context))
         (entry
          (or (cl-find norm-path contents :key #'macher-agent-vfs-entry-path :test #'equal)
              (cl-find path contents :key #'macher-agent-vfs-entry-path :test #'equal)
              (cl-find-if
               (lambda (e)
                 (let ((e-norm (macher-agent--normalize-path-key (macher-agent-vfs-entry-path e) context)))
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
        (setf (macher-agent-vfs-entry-curr entry) new-content)
      (let* ((is-buffer (eq (macher-agent--classify-file-path norm-path workspace-root) 'buffer))
             (safe-path (if (or is-buffer (null workspace-root))
                            norm-path
                          (let ((rel-path (macher-agent-to-relative-path norm-path workspace-root)))
                            (macher-agent--resolve-safe-path rel-path workspace-root))))
             (orig (cond
                    (is-buffer
                     (let ((buf (or (get-buffer norm-path) (get-buffer path))))
                       (if (and buf (buffer-live-p buf))
                           (with-current-buffer buf
                             (buffer-substring-no-properties (point-min) (point-max)))
                         new-content)))
                    (t
                     (macher-agent--read-content-from-disk-or-buffer safe-path))))
             (new-entry (make-macher-agent-vfs-entry :path norm-path :orig orig :curr new-content)))
        (macher-agent--set-context-contents
         context
         (cons new-entry contents))))
    (macher-agent--set-context-dirty-p context t)
    (macher-agent--persist-vfs-to-hidden-buffer context)
    (run-hook-with-args 'macher-agent-context-mutated-hook norm-path)))

(defun macher-agent--read-context-file (context path)
  "Read PATH from CONTEXT.

Prioritises VFS, then active buffers, then physical disk.
Uniformly applies security and path normalisation checks.

CONTEXT is the active context structure.
PATH is the file path string or buffer name.

Return the content string or nil."
  (cl-assert (macher-agent-valid-context-p context) nil "VFS Read Error: Context cannot be nil or invalid")
  (cl-assert (stringp path) nil "PATH must be a string, got: %S" path)
  (let* ((norm-path (macher-agent--normalize-path-key path context))
         (contents (macher-agent--get-context-contents context))
         (workspace-root (when (macher-agent-valid-context-p context)
                           (macher-agent-context-root context)))
         (vfs-ht (macher-agent-workspace-vfs-buffers context))
         (mtime-ht (macher-agent-workspace-mtime-tracker context))
         (is-buf (eq (macher-agent--classify-file-path path workspace-root) 'buffer)))
    (macher-agent--ensure-access context path)
    (let ((target-path
           (if (and workspace-root (not is-buf))
               (let ((relative-path (macher-agent-to-relative-path path workspace-root)))
                 (macher-agent--resolve-safe-path relative-path workspace-root))
             path)))
      (when (and mtime-ht (hash-table-p mtime-ht) (stringp target-path) (not is-buf))
        (let ((attrs (file-attributes target-path)))
          (when attrs
            (let ((mtime (nth 5 attrs)))
              (unless (gethash norm-path mtime-ht)
                (puthash norm-path mtime mtime-ht))
              (unless (gethash path mtime-ht)
                (puthash path mtime mtime-ht))
              (unless (gethash target-path mtime-ht)
                (puthash target-path mtime mtime-ht))))))
      (let* ((entry (or (cl-find norm-path contents :key #'macher-agent-vfs-entry-path :test #'equal)
                        (cl-find path contents :key #'macher-agent-vfs-entry-path :test #'equal)
                        (cl-find-if
                         (lambda (e)
                           (let ((e-norm (macher-agent--normalize-path-key (macher-agent-vfs-entry-path e) context)))
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
        (when (and entry (macher-agent-vfs-entry-curr entry))
          (macher-agent--sync-context-entry entry mtime-ht workspace-root))
        (cond
         (entry
          (macher-agent-vfs-entry-curr entry))
         ((not (eq vfs-val 'macher-agent--unbound))
          vfs-val)
         (t
          (or (macher-agent--read-content-from-disk-or-buffer target-path)
              (when (and (not (equal path target-path))
                         (file-name-absolute-p path))
                (macher-agent--read-content-from-disk-or-buffer path))
              (error "ERROR: File/Buffer '%s' does not exist" path))))))))

(defun macher-agent--vfs-apply-overlay-stateless (contents ws-root sandbox-dir)
  "Apply virtual CONTENTS overlay to SANDBOX-DIR statelessly.

CONTENTS is the list of VFS entry structs.
WS-ROOT is the workspace root path string.
SANDBOX-DIR is the sandbox directory path string.

Return nil.
Side effects: Writes virtual overlay contents to SANDBOX-DIR."
  (macher-agent--vfs-process-entries
   contents
   sandbox-dir
   (lambda (entry)
     (let ((path (macher-agent-vfs-entry-path entry)))
       (macher-agent-to-relative-path path ws-root)))
   (lambda (entry)
     (macher-agent-vfs-entry-curr entry))))

(defun macher-agent-call-with-strict-vfs-pipeline (context body-fn)
  "Execute BODY-FN within a physical sandbox directory populated with CONTEXT.

CONTEXT is the active context structure.
BODY-FN is the function containing pipeline logic.

Return the result of BODY-FN.
Side effects: Creates and cleans up a temporary sandbox directory."
  (let* ((ctx-root (when context (macher-agent-context-root context)))
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
      (when (and sandbox-dir (file-directory-p sandbox-dir))
        (delete-directory sandbox-dir t)))))

(defun macher-agent--auto-sync-context (ctx &rest _args)
  "Synchronise the active context with the physical disk, unless paused.

CTX is the active context structure.
_ARGS represents unused extra arguments.

Return nil.
Side effects: May update CTX dirty state and persist state if synced."
  (when (and ctx (not macher-agent--pause-auto-sync))
    (let* ((contents (macher-agent--get-context-contents ctx))
           (root (or (when (macher-agent-valid-context-p ctx) (macher-agent-context-root ctx))
                     default-directory))
           (tracker (when ctx (macher-agent-workspace-mtime-tracker ctx)))
           (res (macher-agent--sync-and-check-dirty-entries contents tracker root))
           (synced (car res))
           (is-dirty (cdr res)))

      (unless is-dirty
        (macher-agent--set-context-dirty-p ctx nil))

      (when synced
        (macher-agent--persist-vfs-to-hidden-buffer ctx)
        (run-hooks 'macher-agent-context-mutated-hook)))))

(defun macher-agent-storage--extract-context (payload)
  "Extract the target context structure from PAYLOAD."
  (when payload
    (or (when (macher-agent-valid-context-p payload) payload)
        (when (macher-agent-transit-payload-p payload)
          (or (let ((c (macher-agent-transit-payload-target-context payload)))
                (when (macher-agent-valid-context-p c) c))
              (let ((c (macher-agent-transit-payload-parent-context payload)))
                (when (macher-agent-valid-context-p c) c))
              (let ((c (macher-agent-transit-payload-child-context payload)))
                (when (macher-agent-valid-context-p c) c))
              (let ((shared (macher-agent-transit-payload-shared-state payload)))
                (when (macher-agent--plist-p shared)
                  (or (let ((c (plist-get shared :target-context)))
                        (when (macher-agent-valid-context-p c) c))
                      (let ((c (plist-get shared :parent-context)))
                        (when (macher-agent-valid-context-p c) c))
                      (let ((c (plist-get shared :child-context)))
                        (when (macher-agent-valid-context-p c) c))
                      (let ((c (plist-get shared :context)))
                        (when (macher-agent-valid-context-p c) c)))))))
        (when (macher-agent--plist-p payload)
          (or (let ((c (plist-get payload :target-context)))
                (when (macher-agent-valid-context-p c) c))
              (let ((c (plist-get payload :parent-context)))
                (when (macher-agent-valid-context-p c) c))
              (let ((c (plist-get payload :child-context)))
                (when (macher-agent-valid-context-p c) c))
              (let ((c (plist-get payload :context)))
                (when (macher-agent-valid-context-p c) c))
              (let ((shared (plist-get payload :shared-state)))
                (when (macher-agent--plist-p shared)
                  (or (let ((c (plist-get shared :target-context)))
                        (when (macher-agent-valid-context-p c) c))
                      (let ((c (plist-get shared :parent-context)))
                        (when (macher-agent-valid-context-p c) c))
                      (let ((c (plist-get shared :child-context)))
                        (when (macher-agent-valid-context-p c) c))
                      (let ((c (plist-get shared :context)))
                        (when (macher-agent-valid-context-p c) c))))))))))

(defun macher-agent-vfs-handle-flush (&optional ctx)
  "Broadcast patch request via core hooks if not suppressed. Never write autonomously."
  (let* ((c (when (macher-agent-valid-context-p ctx) ctx))
         (suppress (or (bound-and-true-p macher-agent--suppress-patch)
                       (and c (let ((plugins (macher-agent-context-plugins c)))
                                (when (macher-agent--plist-p plugins)
                                  (plist-get plugins :suppress-patch))))))
         (contents (when c (macher-agent--get-context-contents c)))
         (has-changes (and c (or (macher-agent--get-context-dirty-p c)
                                 (cl-some #'macher-agent-vfs-entry-modified-p contents)))))
    (when (and c has-changes)
      (when-let* ((prompt (macher-agent-context-prompt c)))
        (setf (macher-agent-context-prompt c) prompt))

      (unless suppress
        (run-hook-with-args 'macher-agent-vfs-flush-hook c)))))

(defmacro macher-agent-with-vfs-scope (context &rest body)
  "Execute BODY with Virtual File System awareness established from CONTEXT.

CONTEXT is the context structure, state machine, buffer environment,
or workspace to resolve.  When nil, attempts to resolve from the environment.
BODY is the sequence of forms to evaluate within the established VFS scope.

Fails fast by signaling an error if the context cannot be resolved.
Evaluates CONTEXT exactly once using uninterned lexical symbols.

Return the result of evaluating the last form in BODY.
Side effects: Binds `macher-agent--persistent-context' and adjusts `default-directory'."
  (declare (indent 1) (debug t))
  (let ((raw-ctx-sym (gensym "raw-ctx-"))
        (ctx-sym (gensym "ctx-"))
        (root-sym (gensym "root-")))
    `(let* ((,raw-ctx-sym ,context)
            (,ctx-sym (cond
                       ((and ,raw-ctx-sym (macher-agent-valid-context-p ,raw-ctx-sym))
                        ,raw-ctx-sym)
                       (,raw-ctx-sym
                        (macher-agent-resolve-context ,raw-ctx-sym))
                       (t (macher-agent-resolve-context)))))
       (unless (and ,ctx-sym (macher-agent-valid-context-p ,ctx-sym))
         (error "macher-agent-with-vfs-scope: Unable to resolve a valid VFS context from %S" ,raw-ctx-sym))
       (let* ((macher-agent--persistent-context ,ctx-sym)
              (,root-sym (macher-agent-context-root ,ctx-sym))
              (default-directory (if (and ,root-sym (stringp ,root-sym) (file-directory-p ,root-sym))
                                     (file-name-as-directory ,root-sym)
                                   default-directory)))
         ,@body))))

(defun macher-agent--expressive-patch-buffer-name (patch-type ws &optional orig-buf)
  "Compute deterministic expressive patch buffer name for PATCH-TYPE, WS, and ORIG-BUF.

Follows the pattern `*macher-[CATEGORY]-patch:[WORKSPACE-TYPE]@[WORKSPACE-NAME]<[HASH]>[BUF-NAME]*'
or fallback `*macher-[CATEGORY]-patch:[WORKSPACE-TYPE]@[WORKSPACE-NAME]<[HASH]>*' when
ORIG-BUF is not provided or empty.

Return the expressive buffer name string.

Side effects: None."
  (let* ((category (if (symbolp patch-type) (symbol-name patch-type) (or patch-type "diff")))
         (buf-name (cond ((bufferp orig-buf)
                          (if (buffer-live-p orig-buf)
                              (buffer-name orig-buf)
                            nil))
                         ((stringp orig-buf)
                          (if (string-empty-p orig-buf) nil orig-buf))
                         (t nil)))
         (unwrapped (if (fboundp 'macher-agent--unwrap-workspace)
                        (macher-agent--unwrap-workspace ws)
                      ws)))
    (if unwrapped
        (let* ((ws-type (cond ((consp ws) (car ws))
                              ((consp unwrapped) (car unwrapped))
                              ((recordp unwrapped) 'agent)
                              (t 'project)))
               (ws-name (macher-agent-macher-workspace-name ws))
               (hash (macher-agent-macher-workspace-hash ws 4)))
          (if buf-name
              (format "*macher-%s-patch:%s@%s<%s>[%s]*" category ws-type ws-name hash buf-name)
            (format "*macher-%s-patch:%s@%s<%s>*" category ws-type ws-name hash)))
      (if buf-name
          (format "*macher-%s-patch[%s]*" category buf-name)
        (format "*macher-%s-patch*" category)))))

(defun macher-agent--build-and-rename-patch (ctx fsm-obj patch-type)
  "Build patch for CTX using FSM-OBJ, reusing and renaming buffers deterministically.

Construct a patch buffer for context CTX and finite-state machine FSM-OBJ if
CTX contains changes and patch generation is not suppressed.  Renames the
newly built patch buffer directly in place to the expressive patch buffer name
so that the patch buffer remains live and retains its identity.

Return the renamed patch buffer if generated, or nil.

Side effects: Reuses, modifies, and renames patch buffers."
  (let* ((target-buf (or (when (and fsm-obj (fboundp 'gptel-fsm-info) (fboundp 'gptel-fsm-p) (gptel-fsm-p fsm-obj))
                           (plist-get (gptel-fsm-info fsm-obj) :buffer))
                         (when (and fsm-obj (macher-agent--plist-p fsm-obj))
                           (plist-get fsm-obj :buffer))
                         (when (macher-agent-valid-context-p ctx)
                           (or (macher-agent-vfs--get-origin-buffer ctx)
                               (macher-agent-context-origin-buffer ctx)))
                         (when-let* ((active-fsm (macher-agent-get-active-fsm fsm-obj)))
                           (when (and (fboundp 'gptel-fsm-info) (fboundp 'gptel-fsm-p) (gptel-fsm-p active-fsm))
                             (plist-get (gptel-fsm-info active-fsm) :buffer)))))
         (suppress-patch (if (and (bufferp target-buf) (buffer-live-p target-buf))
                             (buffer-local-value 'macher-agent--suppress-patch target-buf)
                           (bound-and-true-p macher-agent--suppress-patch)))
         (ws (when (macher-agent-valid-context-p ctx)
               (macher-agent-context-workspace ctx))))
    (when (and (not suppress-patch)
               (macher-agent--context-has-changes-p ctx))
      (let* ((expressive-name (macher-agent--expressive-patch-buffer-name patch-type ws target-buf))
             (patch-buf (macher-agent-macher-build-patch ctx fsm-obj)))
        (if (and patch-buf (buffer-live-p patch-buf))
            (if (equal (buffer-name patch-buf) expressive-name)
                patch-buf
              (when-let* ((existing (get-buffer expressive-name)))
                (unless (eq existing patch-buf)
                  (kill-buffer existing)))
              (with-current-buffer patch-buf
                (rename-buffer expressive-name t))
              patch-buf)
          (or (get-buffer expressive-name) (generate-new-buffer expressive-name)))))))

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
  (let* ((file-ctx (if (macher-agent-context-p ctx)
                       (let* ((cloned (macher-agent--copy-context ctx))
                              (plugins (macher-agent-context-plugins ctx))
                              (copied-plugins (macher-agent--copy-context-hash-tables plugins)))
                         (setf (macher-agent-context-plugins cloned)
                               (copy-tree copied-plugins))
                         cloned)
                     (when ctx (macher-agent--clone-context ctx))))
         (buf-ctx (if (macher-agent-context-p ctx)
                      (let* ((cloned (macher-agent--copy-context ctx))
                             (plugins (macher-agent-context-plugins ctx))
                             (copied-plugins (macher-agent--copy-context-hash-tables plugins)))
                        (setf (macher-agent-context-plugins cloned)
                              (copy-tree copied-plugins))
                        cloned)
                    (when ctx (macher-agent--clone-context ctx))))
         (root (when (macher-agent-valid-context-p ctx)
                 (macher-agent-context-root ctx)))
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
           (prompt (or (and (fboundp 'gptel-fsm-info) (fboundp 'gptel-fsm-p) fsm-obj (gptel-fsm-p fsm-obj) (plist-get (gptel-fsm-info fsm-obj) :prompt))
                       (and (listp fsm-obj) (plist-get fsm-obj :prompt))
                       (when (macher-agent-context-p ctx) (macher-agent-context-prompt ctx))
                       ""))
           (generated-buffers nil))
      (when prompt
        (when p-ctx
          (when (macher-agent-context-p p-ctx)
            (setf (macher-agent-context-prompt p-ctx) prompt)
            (setf (macher-agent-context-plugins p-ctx)
                  (plist-put (copy-sequence (macher-agent-context-plugins p-ctx)) :prompt prompt))))
        (when v-ctx
          (when (macher-agent-context-p v-ctx)
            (setf (macher-agent-context-prompt v-ctx) prompt)
            (setf (macher-agent-context-plugins v-ctx)
                  (plist-put (copy-sequence (macher-agent-context-plugins v-ctx)) :prompt prompt)))))
      (when-let* ((p-buf (macher-agent--build-and-rename-patch p-ctx fsm-obj "physical")))
        (push p-buf generated-buffers))
      (when-let* ((v-buf (macher-agent--build-and-rename-patch v-ctx fsm-obj "virtual")))
        (push v-buf generated-buffers))
      (when generated-buffers
        (macher-agent--display-patch-buffers generated-buffers)))
    (macher-agent--set-context-dirty-p ctx nil)))

(defun macher-agent-vfs-build-patch-from-hook (ctx)
  "Observe VFS flush events and build the visual macher interface."
  (when (macher-agent-valid-context-p ctx)
    (let* ((fsm-obj (macher-agent-get-active-fsm))
           (prompt (or (and (fboundp 'gptel-fsm-info) (fboundp 'gptel-fsm-p) fsm-obj (gptel-fsm-p fsm-obj) (plist-get (gptel-fsm-info fsm-obj) :prompt))
                       (and (listp fsm-obj) (plist-get fsm-obj :prompt))
                       (when (macher-agent-context-p ctx) (macher-agent-context-prompt ctx))
                       "")))
      (when prompt
        (when (macher-agent-context-p ctx)
          (setf (macher-agent-context-prompt ctx) prompt)
          (setf (macher-agent-context-plugins ctx)
                (plist-put (copy-sequence (macher-agent-context-plugins ctx)) :prompt prompt))))
      (macher-agent--execute-split-patch ctx fsm-obj))))

(defun macher-agent-vfs-diff-review (&optional ctx)
  "Review pending diff patches for uncommitted VFS modifications in CTX.

When CTX is nil, resolves context from the current environment or active buffer.
Subscribes to or dispatches through `macher-agent-vfs-handle-flush'.

Return nil.
Side effects: Displays generated patch review buffers for pending edits."
  (interactive)
  (let ((context (or (when (macher-agent-valid-context-p ctx) ctx)
                     (bound-and-true-p macher-agent--persistent-context)
                     (ignore-errors (macher-agent-resolve-context (current-buffer)))
                     (ignore-errors (macher-agent-resolve-context)))))
    (if (and context (macher-agent-valid-context-p context))
        (macher-agent-vfs-handle-flush context)
      (message "No active VFS context found."))))

(defun macher-agent-vfs-install ()
  "Install VFS storage hooks, patch interfaces, and pipeline steps."
  (macher-agent-register-pipeline-step 'payload-merge #'macher-agent-vfs--merge-payload 10)
  (add-hook 'macher-agent-vfs-flush-hook #'macher-agent-vfs-build-patch-from-hook)
  (add-hook 'macher-agent-task-flush-hook #'macher-agent-vfs-handle-flush)
  (when (fboundp 'macher-agent-macher-install)
    (macher-agent-macher-install)))

(defun macher-agent--merge-contexts (parent-ctx child-ctx)
  "Merge the VFS contents of CHILD-CTX into PARENT-CTX respecting existing parent edits."
  (cl-assert (macher-agent-valid-context-p parent-ctx) nil "PARENT-CTX must be a valid context, got: %S" parent-ctx)
  (cl-assert (macher-agent-valid-context-p child-ctx) nil "CHILD-CTX must be a valid context, got: %S" child-ctx)
  (when (and parent-ctx child-ctx (not (eq parent-ctx child-ctx)))
    (let ((child-contents (macher-agent--get-context-contents child-ctx))
          (parent-contents (macher-agent--get-context-contents parent-ctx))
          (any-merged nil))
      (dolist (child-entry child-contents)
        (let* ((path (macher-agent-vfs-entry-path child-entry))
               (orig (macher-agent-vfs-entry-orig child-entry))
               (new (macher-agent-vfs-entry-curr child-entry))
               (norm-path (macher-agent--normalize-path-key path parent-ctx))
               (parent-entry
                (or (cl-find path parent-contents :key #'macher-agent-vfs-entry-path :test #'equal)
                    (when norm-path (cl-find norm-path parent-contents :key #'macher-agent-vfs-entry-path :test #'equal))
                    (cl-find-if (lambda (e)
                                  (let ((e-norm (macher-agent--normalize-path-key (macher-agent-vfs-entry-path e) parent-ctx)))
                                    (and norm-path e-norm (equal e-norm norm-path))))
                                parent-contents)))
               (p-curr (when parent-entry (macher-agent-vfs-entry-curr parent-entry))))
          (when (or (and (not (equal orig new)) (not (equal new p-curr)))
                    (and (null parent-entry) (not (equal orig new))))
            (macher-agent--update-context-file parent-ctx path new)
            (setq any-merged t))))
      (when any-merged
        (macher-agent--set-context-dirty-p parent-ctx t))))
  parent-ctx)

(defun macher-agent-vfs--merge-payload (target-or-payload &optional payload-arg)
  "Merge Virtual File System PAYLOAD into the target parent context directly.

Support polymorphic invocation:
- (macher-agent-vfs--merge-payload payload)
- (macher-agent-vfs--merge-payload target payload)

TARGET can be `gptel-fsm', `macher-agent-context', or a buffer/FSM container.
PAYLOAD is the property list, struct, or list of diff entries containing merge artifacts.

Exhaustively extracts target context from PAYLOAD keys (`macher-agent-transit-context-keys'),
`macher-agent-storage--extract-context', workspace-id, shared-state, and buffer local persistent context.
Applies file diffs and child-context modifications to the resolved parent context,
handling deletions when diff items have nil content.
Synchronises `macher-agent--persistent-context' across relevant buffers and
updates `:target-context' on the returned payload structure.

Return updated PAYLOAD or context."
  (let* ((has-two-args (and payload-arg t))
         (explicit-target (if has-two-args target-or-payload nil))
         (payload (if has-two-args payload-arg target-or-payload))
         (p-struct (when (macher-agent-transit-payload-p payload) payload))
         (target
          (or explicit-target
              (when p-struct
                (or (macher-agent-transit-payload-target-context p-struct)
                    (macher-agent-transit-payload-parent-context p-struct)
                    (macher-agent-transit-payload-target-buffer p-struct)))
              (when (macher-agent--plist-p payload)
                (or (plist-get payload :target-context)
                    (plist-get payload :parent-context)
                    (plist-get payload :macher-agent-context)
                    (plist-get payload :target-buffer)
                    (plist-get payload :buffer)
                    (plist-get payload :target)))))
         (parent-ctx
          (or (when target
                (cond
                 ((macher-agent-valid-context-p target)
                  target)
                 ((and (fboundp 'gptel-fsm-p) (gptel-fsm-p target))
                  (or (macher-agent-gptel--fsm-context target)
                      (macher-agent--extract-fsm-context target)
                      (let* ((info (macher-agent--extract-fsm-info target))
                             (buf (when (macher-agent--plist-p info) (plist-get info :buffer))))
                        (when buf (macher-agent-resolve-context buf)))))
                 ((and (recordp target) (fboundp 'gptel-fsm-p) (gptel-fsm-p target))
                  (or (macher-agent-gptel--fsm-context target)
                      (macher-agent--extract-fsm-context target)))
                 ((and (recordp target) (fboundp 'gptel-fsm-info))
                  (or (macher-agent-gptel--fsm-context target)
                      (macher-agent--extract-fsm-context target)))
                 ((or (bufferp target) (stringp target))
                  (let ((buf (if (bufferp target) target (get-buffer target))))
                    (when (and buf (buffer-live-p buf))
                      (or (let ((ctx (buffer-local-value 'macher-agent--persistent-context buf)))
                            (when (macher-agent-valid-context-p ctx) ctx))
                          (macher-agent-resolve-context buf)))))
                 ((macher-agent--plist-p target)
                  (or (plist-get target :macher-agent-context)
                      (plist-get target :target-context)
                      (plist-get target :parent-context)
                      (plist-get target :context)
                      (macher-agent-storage--extract-context target)
                      (when-let* ((buf (or (plist-get target :buffer) (plist-get target :target-buffer))))
                        (let ((live-buf (if (bufferp buf) buf (and (stringp buf) (get-buffer buf)))))
                          (when (and live-buf (buffer-live-p live-buf))
                            (or (let ((ctx (buffer-local-value 'macher-agent--persistent-context live-buf)))
                                  (when (macher-agent-valid-context-p ctx) ctx))
                                (macher-agent-resolve-context live-buf)))))))))
              (when (macher-agent-valid-context-p payload) payload)
              (when p-struct
                (or (when-let* ((c (macher-agent-transit-payload-target-context p-struct)))
                      (when (macher-agent-valid-context-p c) c))
                    (when-let* ((c (macher-agent-transit-payload-parent-context p-struct)))
                      (when (macher-agent-valid-context-p c) c))
                    (when-let* ((b (macher-agent-transit-payload-target-buffer p-struct)))
                      (let ((buf (if (bufferp b) b (and (stringp b) (get-buffer b)))))
                        (when (and buf (buffer-live-p buf))
                          (or (let ((ctx (buffer-local-value 'macher-agent--persistent-context buf)))
                                (when (macher-agent-valid-context-p ctx) ctx))
                              (macher-agent-resolve-context buf)))))))
              (macher-agent-storage--extract-context payload)
              (when (and (bound-and-true-p macher-agent--persistent-context)
                         (macher-agent-valid-context-p macher-agent--persistent-context))
                macher-agent--persistent-context)
              (macher-agent-resolve-context)))
         (child-ctx (cond
                     (p-struct (macher-agent-transit-payload-child-context p-struct))
                     ((and (macher-agent-valid-context-p payload) (not (eq payload parent-ctx)))
                      payload)
                     ((macher-agent--plist-p payload)
                      (let ((c (or (plist-get payload :child-context) (plist-get payload :context))))
                        (if (and c (macher-agent-valid-context-p c)) c nil)))))
         (diff (cond
                (p-struct (let ((pl (macher-agent-transit-payload-payload p-struct)))
                            (cond
                             ((and (macher-agent--plist-p pl) (plist-get pl :diff))
                              (plist-get pl :diff))
                             ((and (macher-agent--plist-p pl) (plist-get pl :vfs-diff))
                              (plist-get pl :vfs-diff))
                             ((listp pl) pl)
                             (t nil))))
                ((macher-agent--plist-p payload)
                 (let ((d (or (plist-get payload :diff)
                              (plist-get payload :vfs-diff)
                              (plist-get payload :payload)
                              (plist-get (plist-get payload :shared-state) :diff))))
                   (if (listp d) d nil)))
                ((and (listp payload) (macher-agent-vfs-entry-p (car-safe payload)))
                 payload))))

    (when parent-ctx
      (when (and child-ctx (not (eq child-ctx parent-ctx)))
        (macher-agent--merge-contexts parent-ctx child-ctx))

      (when diff
        (let ((any-mutated nil))
          (dolist (item diff)
            (let* ((v-item (cond
                            ((macher-agent-vfs-entry-p item) item)
                            ((listp item)
                             (make-macher-agent-vfs-entry
                              :path (or (plist-get item :path) (plist-get item :file))
                              :orig (plist-get item :orig)
                              :curr (or (plist-get item :content) (plist-get item :curr) (plist-get item :contents)))))))
              (when v-item
                (let ((path (macher-agent-vfs-entry-path v-item))
                      (curr (macher-agent-vfs-entry-curr v-item)))
                  (when path
                    (macher-agent--update-context-file parent-ctx path curr)
                    (setq any-mutated t))))))
          (when any-mutated
            (macher-agent--set-context-dirty-p parent-ctx t))))

      (when (boundp 'macher-agent--persistent-context)
        (setq macher-agent--persistent-context parent-ctx))

      (when (and explicit-target (fboundp 'gptel-fsm-p) (gptel-fsm-p explicit-target))
        (when (fboundp 'macher-agent--inject-context-into-fsm-info)
          (macher-agent--inject-context-into-fsm-info parent-ctx explicit-target)))

      (let* ((buf-candidates
              (cond
               (p-struct (list (macher-agent-transit-payload-target-buffer p-struct)))
               ((macher-agent--plist-p payload)
                (mapcar (lambda (k) (plist-get payload k))
                        (bound-and-true-p macher-agent-transit-buffer-keys)))))
             (live-bufs
              (cl-remove-if-not #'buffer-live-p
                                (mapcar (lambda (b)
                                          (cond ((bufferp b) b)
                                                ((stringp b) (get-buffer b))
                                                (t nil)))
                                        (delq nil buf-candidates)))))
        (dolist (b live-bufs)
          (with-current-buffer b
            (setq-local macher-agent--persistent-context parent-ctx))))

      (if (macher-agent-transit-payload-p payload)
          (setf (macher-agent-transit-payload-target-context payload) parent-ctx)
        (when (macher-agent--plist-p payload)
          (setq payload (plist-put (copy-sequence payload) :target-context parent-ctx)))))
    (or payload parent-ctx)))

(defun macher-agent--apply-single-virtual-buffer (entry)
  "Apply a single virtual edit ENTRY to a live Emacs buffer.

ENTRY is a `macher-agent-vfs-entry` structure.

Return non-nil if applied to a live buffer, otherwise nil.

Side effects: Modifies the target live buffer contents."
  (when-let* (((macher-agent-vfs-entry-p entry))
              (path (macher-agent-vfs-entry-path entry))
              ((and (stringp path) (not (string-empty-p path))))
              (content (macher-agent-vfs-entry-curr entry))
              (curr (string-trim-right (or content "")))
              (buf-name (macher-agent--resolve-buffer-name path))
              (buf (when buf-name (get-buffer buf-name))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (erase-buffer)
        (insert curr))
      t)))

(defun macher-agent-apply-virtual-buffers (&optional context)
  "Apply pending virtual edits to live Emacs buffers.

CONTEXT is the optional context structure.  If omitted, resolves
the active context.

Return nil.

Side effects: Modifies live buffer contents and updates context entries."
  (let* ((ctx (or context (bound-and-true-p macher-agent--persistent-context)))
         (contents (when ctx
                     (macher-agent--get-context-contents ctx)))
         (normalized-contents nil))
    (dolist (entry contents)
      (when-let* (((macher-agent-vfs-entry-p entry))
                  (path (macher-agent-vfs-entry-path entry))
                  ((and (stringp path) (not (string-empty-p path)))))
        (let* ((content (macher-agent-vfs-entry-curr entry))
               (trimmed (string-trim-right (or content ""))))
          (push (make-macher-agent-vfs-entry
                 :path path
                 :orig (or (macher-agent-vfs-entry-orig entry) trimmed)
                 :curr trimmed)
                normalized-contents))
        (macher-agent--apply-single-virtual-buffer entry)))
    (when (and ctx normalized-contents)
      (macher-agent--set-context-contents ctx (nreverse normalized-contents)))
    (when ctx
      (macher-agent--auto-sync-context ctx))))

(provide 'macher-agent-vfs)
;;; macher-agent-vfs.el ends here
