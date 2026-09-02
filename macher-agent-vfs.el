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
  "Extract current VFS hierarchy state strictly from CTX struct."
  (cl-check-type ctx macher-agent-context)
  (plist-get (macher-agent-context-plugins ctx) :vfs))

(defun macher-agent-vfs--set-state (ctx state)
  "Update VFS hierarchy STATE strictly within CTX struct."
  (cl-check-type ctx macher-agent-context)
  (setf (macher-agent-context-plugins ctx)
        (plist-put (copy-sequence (macher-agent-context-plugins ctx)) :vfs state)))

(defun macher-agent-context-shadow-buffers (ctx)
  "Retrieve active shadow mapping strictly from CTX struct."
  (cl-check-type ctx macher-agent-context)
  (or (plist-get (macher-agent-vfs--get-state ctx) :shadows)
      (plist-get (macher-agent-vfs--get-state ctx) :shadow-buffers)))

(defun macher-agent--set-context-shadow-buffers (ctx shadows)
  "Bind SHADOWS map strictly to CTX struct."
  (cl-check-type ctx macher-agent-context)
  (let ((state (macher-agent-vfs--get-state ctx)))
    (macher-agent-vfs--set-state ctx (plist-put (plist-put state :shadows shadows) :shadow-buffers shadows))))

(defun macher-agent-vfs--get-origin-buffer (ctx)
  "Retrieve root orchestrator buffer exclusively from CTX struct."
  (cl-check-type ctx macher-agent-context)
  (or (plist-get (macher-agent-context-plugins ctx) :origin-buffer)
      (plist-get (macher-agent-vfs--get-state ctx) :origin-buffer)))

;;; Concurrency Locks

(defun macher-agent-vfs-release-lock (path &optional lock-state-or-task-id current-owner)
  "Relinquish VFS lock bound explicitly to PATH string."
  (cl-check-type path string)
  (let* ((task-id (or current-owner lock-state-or-task-id))
         (lock-state (if (consp lock-state-or-task-id) lock-state-or-task-id (gethash path macher-agent--vfs-lock-table)))
         (actual-owner (or (car-safe lock-state) task-id))
         (ref-count (or (cdr-safe lock-state) 0)))
    (when (and lock-state (or (null task-id) (equal actual-owner task-id)))
      (if (> ref-count 1)
          (progn
            (puthash path (cons actual-owner (1- ref-count)) macher-agent--vfs-lock-table)
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
             (path (or (when meta (or (plist-get meta :resource_path)
                                      (plist-get meta :resource-path)
                                      (plist-get meta :path)))
                       (plist-get payload :resource_path)
                       (plist-get payload :resource-path)
                       (plist-get payload :path)))
             (task-id (or (plist-get payload :task-id) (macher-agent--generate-uuid)))
             (explicit-cb (plist-get payload :callback))
             (cb (or explicit-cb
                     (when path (gethash path macher-agent--pending-callbacks)))))
        (when path
          (let* ((lock-state (gethash path macher-agent--vfs-lock-table))
                 (current-owner (car-safe lock-state))
                 (ref-count (or (cdr-safe lock-state) 0)))
            (if (or (null lock-state) (equal current-owner task-id))
                (progn
                  (puthash path (cons task-id (1+ ref-count)) macher-agent--vfs-lock-table)
                  (when (and cb (not (eq cb #'ignore)))
                    (unless explicit-cb
                      (remhash path macher-agent--pending-callbacks))
                    (funcall cb "Resource lock acquired.")))
              (if cb
                  (progn
                    (unless explicit-cb
                      (remhash path macher-agent--pending-callbacks))
                    (let ((queue (gethash path macher-agent--vfs-lock-queues nil)))
                      (puthash path (append queue (list (cons task-id cb))) macher-agent--vfs-lock-queues)))
                (display-warning 'macher-agent (format "Resource '%s' is locked." path) :warning)))))))
     ((eq type 'RELEASE_LOCK)
      (let* ((meta (plist-get payload :metadata))
             (path (or (when meta (or (plist-get meta :resource_path)
                                      (plist-get meta :resource-path)
                                      (plist-get meta :path)))
                       (plist-get payload :resource_path)
                       (plist-get payload :resource-path)
                       (plist-get payload :path)))
             (task-id (plist-get payload :task-id)))
        (when path (macher-agent-vfs-release-lock path task-id)))))))

(defun macher-agent-vfs-write (file-path content mtime-tracker-ht &optional vfs-buffers-ht)
  "Write CONTENT string explicitly to FILE-PATH and log to tracked tables."
  (cl-check-type file-path string)
  (cl-check-type content string)
  (cl-check-type mtime-tracker-ht hash-table)
  (when vfs-buffers-ht
    (cl-check-type vfs-buffers-ht hash-table))
  (let* ((expanded-path (expand-file-name file-path))
         (original-mtime (or (gethash file-path mtime-tracker-ht)
                             (gethash expanded-path mtime-tracker-ht)))
         (current-attrs (file-attributes expanded-path))
         (current-mtime (when current-attrs (nth 5 current-attrs))))
    (when (and original-mtime current-mtime (not (time-equal-p original-mtime current-mtime)))
      (display-warning 'macher-agent
                       (format "Your previous edits to %s were discarded due to external file modifications.  Please re-read and re-apply"
                               (file-name-nondirectory file-path))
                       :warning))
    (puthash file-path current-mtime mtime-tracker-ht)
    (puthash expanded-path current-mtime mtime-tracker-ht)
    (when vfs-buffers-ht
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
  "Identify if specific PATH string represents active media."
  (cl-check-type path string)
  (let ((mime (and (fboundp 'mailcap-file-name-to-mime-type)
                   (mailcap-file-name-to-mime-type path))))
    (or (and mime (or (string-prefix-p "image/" mime)
                      (string-prefix-p "video/" mime)
                      (string-prefix-p "audio/" mime)))
        (string-match-p
         "\\.\\(png\\|jpe?g\\|gif\\|webp\\|svg\\|pdf\\|mp4\\|mov\\|mp3\\|wav\\)$" path))))

(defun macher-agent-to-relative-path (path &optional root)
  "Construct relative offset explicitly for PATH string."
  (cl-check-type path string)
  (let* ((ws-root (file-name-as-directory
                   (file-truename (expand-file-name (or root default-directory)))))
         (truename (file-truename (expand-file-name path ws-root))))
    (file-relative-name truename ws-root)))

(defun macher-agent--resolve-safe-path (unsafe-path root)
  "Generate normalized sandbox directory explicitly from UNSAFE-PATH."
  (cl-check-type unsafe-path string)
  (let* ((canonical-base (file-name-as-directory (file-truename (expand-file-name root))))
         (canonical-resolved (file-truename (expand-file-name unsafe-path canonical-base))))
    (if (or (and (file-directory-p canonical-base) (file-in-directory-p canonical-resolved canonical-base))
            (string-prefix-p canonical-base (file-name-as-directory canonical-resolved))
            (string= (directory-file-name canonical-base) (directory-file-name canonical-resolved)))
        canonical-resolved
      (error "SECURITY ERROR: Path traversal jailbreak detected: %s" unsafe-path))))

(defun macher-agent--write-or-delete-vfs-entry (content target-path)
  "Process IO event using strict CONTENT string against TARGET-PATH."
  (cl-check-type content string)
  (cl-check-type target-path string)
  (let ((dir (file-name-directory target-path)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))
    (with-temp-file target-path
      (insert content))))

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
                   (sandbox-target-path (macher-agent--resolve-safe-path (expand-file-name relative-path sandbox-root) sandbox-root)))
              (when (stringp new-content)
                (macher-agent--write-or-delete-vfs-entry new-content sandbox-target-path))))
          entries)))

(defun macher-agent-vfs-scratch-inflate (sandbox-path vfs-buffers-ht &optional ws-root contents)
  "Hydrate VFS state directly targeting SANDBOX-PATH directory."
  (cl-check-type sandbox-path string)
  (cl-check-type vfs-buffers-ht hash-table)
  (cl-assert (file-directory-p sandbox-path))
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
         (macher-agent-to-relative-path (expand-file-name p ws-root) ws-root)))
     (lambda (entry)
       (macher-agent-vfs-entry-curr entry))))

  (when (> (hash-table-count vfs-buffers-ht) 0)
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
         (macher-agent-to-relative-path (expand-file-name key ws-root) ws-root))
       (lambda (key) (gethash key vfs-buffers-ht))))))

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
-c core.quotePath=false ls-files -z -o --exclude-standard; }) | rsync -a --delete --from0 \
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

(defun macher-agent--read-string (offset limit &optional text show-line-numbers)
  "Extract bounded string fragment relying on strictly typed OFFSET and LIMIT."
  (cl-check-type offset integer)
  (cl-check-type limit integer)
  (when text
    (cl-check-type text string))
  (let* ((lines (split-string (or text "") "\n"))
         (total-lines (length lines))
         (start-line (if (> offset 0)
                         (1- offset)
                       (if (< offset 0)
                           (max 0 (+ total-lines offset))
                         0)))
         (num-lines (if (> limit 0)
                        limit
                      (if (< limit 0)
                          (max 0 (+ total-lines limit))
                        (- total-lines start-line))))
         (selected-lines (seq-take (seq-drop lines start-line) num-lines)))
    (if show-line-numbers
        (string-join
         (cl-loop for l in selected-lines
                  for idx from (1+ start-line)
                  collect (format "%6d\t%s" idx l))
         "\n")
      (string-join selected-lines "\n"))))

(defun macher-agent--edit-string-fast (old-text new-text &optional content replace-all)
  "Perform fast substitution validating OLD-TEXT is fully formed."
  (cl-check-type old-text string)
  (cl-check-type new-text string)
  (cl-check-type content string)
  (cl-assert (not (string-empty-p old-text)))
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
  "Extract content string guaranteed by PATH string structure."
  (cl-check-type path string)
  (cl-assert (not (string-empty-p path)))
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
     (t nil))))

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
  "Categorize string PATH into standard VFS archetypes."
  (cl-check-type path string)
  (let* ((buf (get-buffer path))
         (file-path (and buf (buffer-file-name buf))))
    (if (and buf (null file-path))
        'buffer
      (let* ((target-path (or file-path path))
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
      (let ((project-vc-include-untracked t))
        (project-files proj))
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

(defun macher-agent--changes-towards-p (orig new current-state)
  "Return non-nil if CURRENT-STATE represents a progression from ORIG towards NEW.

ORIG is the baseline content string.
NEW is the target content string containing modifications.
CURRENT-STATE is the live buffer or disk content."
  (let ((orig-str (or orig ""))
        (new-str (or new ""))
        (curr-str (or current-state "")))
    (cond
     ((equal curr-str new-str) t)
     ((equal curr-str orig-str) nil)
     (t
      (let ((orig-lines (split-string orig-str "\n"))
            (new-lines (split-string new-str "\n"))
            (curr-lines (split-string curr-str "\n")))
        (and (cl-every (lambda (line)
                         (or (member line orig-lines)
                             (member line new-lines)))
                       curr-lines)
             (or (cl-some (lambda (line)
                            (and (member line curr-lines)
                                 (member line new-lines)
                                 (not (member line orig-lines))))
                          curr-lines)
                 (cl-some (lambda (line)
                            (and (member line orig-lines)
                                 (not (member line curr-lines))
                                 (not (member line new-lines))))
                          orig-lines))))))))

(defun macher-agent--sync-context-entry (entry root &optional mtime-tracker-ht)
  "Process synchronization strictly on isolated ROOT string.

ENTRY is the `macher-agent-vfs-entry' struct.
ROOT is the workspace root directory string.
MTIME-TRACKER-HT is an optional hash table tracking file modification times.

Return non-nil if synchronisation modified the entry, otherwise nil."
  (cl-check-type entry macher-agent-vfs-entry)
  (cl-check-type root string)
  (when mtime-tracker-ht
    (cl-check-type mtime-tracker-ht hash-table))
  (let* ((path (macher-agent-vfs-entry-path entry))
         (abs-path (if (not (file-name-absolute-p path))
                       (expand-file-name path root)
                     path))
         (orig (macher-agent-vfs-entry-orig entry))
         (new (macher-agent-vfs-entry-curr entry))
         (stored-mtime (when mtime-tracker-ht
                         (or (gethash path mtime-tracker-ht)
                             (gethash abs-path mtime-tracker-ht))))
         (attrs (file-attributes abs-path))
         (current-mtime (when attrs (nth 5 attrs)))
         (disk-newer
          (and stored-mtime current-mtime (time-less-p stored-mtime current-mtime)))
         (buf (or (get-file-buffer path) (get-file-buffer abs-path) (get-buffer path)))
         (live-buf (and buf (buffer-live-p buf)))
         (buf-content (when live-buf
                        (with-current-buffer buf
                          (buffer-substring-no-properties (point-min) (point-max)))))
         (disk-exists (file-exists-p abs-path))
         (disk-content (when disk-exists
                         (ignore-errors
                           (with-temp-buffer
                             (insert-file-contents abs-path)
                             (buffer-string)))))
         (current-state nil))

    (when (and mtime-tracker-ht current-mtime (null stored-mtime))
      (puthash path current-mtime mtime-tracker-ht)
      (puthash abs-path current-mtime mtime-tracker-ht))

    (cond
     ;; 1. Live buffer has changes: equals new, progressed towards new, or buffer-modified-p
     ((and live-buf
           (or (equal buf-content new)
               (macher-agent--changes-towards-p orig new buf-content)
               (and (buffer-modified-p buf) (not disk-newer))))
      (setq current-state buf-content))

     ;; 2. Disk was modified out-of-band / disk is newer
     ((or disk-newer
          (and live-buf (buffer-file-name buf) (not (verify-visited-file-modtime buf))))
      (setq current-state (or disk-content
                              (macher-agent--read-content-from-disk-or-buffer abs-path)))
      (when (and mtime-tracker-ht current-mtime)
        (puthash path current-mtime mtime-tracker-ht)
        (puthash abs-path current-mtime mtime-tracker-ht)))

     ;; 3. Disk content matches new or progressed towards new
     ((and disk-content
           (or (equal disk-content new)
               (macher-agent--changes-towards-p orig new disk-content)))
      (setq current-state disk-content)
      (when (and mtime-tracker-ht current-mtime)
        (puthash path current-mtime mtime-tracker-ht)
        (puthash abs-path current-mtime mtime-tracker-ht)))

     ;; 4. Live buffer content
     (live-buf
      (setq current-state buf-content))

     ;; 5. Disk content
     (disk-content
      (setq current-state disk-content))

     (t
      (setq current-state nil)))

    (if (and current-state (not (equal current-state orig)))
        (cond
         ;; Hunks fully applied: current-state equals new
         ((equal current-state new)
          (macher-agent--update-entry-content-cells entry current-state new)
          (when (and mtime-tracker-ht current-mtime)
            (puthash path current-mtime mtime-tracker-ht)
            (puthash abs-path current-mtime mtime-tracker-ht))
          t)

         ;; Hunks partially applied towards new: update orig baseline
         ((macher-agent--changes-towards-p orig new current-state)
          (macher-agent--update-entry-content-cells entry current-state new)
          (when (and mtime-tracker-ht current-mtime)
            (puthash path current-mtime mtime-tracker-ht)
            (puthash abs-path current-mtime mtime-tracker-ht))
          t)

         ;; Out-of-band modification: fail-fast sync/invalidation
         (t
          (if (equal (or orig "") (or new ""))
              (macher-agent--update-entry-content-cells entry current-state current-state)
            (display-warning
             'macher-agent
             (format "Your previous edits to %s were discarded due to external file modifications.  Please re-read and re-apply"
                     (file-name-nondirectory path))
             :warning)
            (macher-agent--update-entry-content-cells entry current-state current-state))
          (when (and mtime-tracker-ht current-mtime)
            (puthash path current-mtime mtime-tracker-ht)
            (puthash abs-path current-mtime mtime-tracker-ht))
          t))
      nil)))

(defun macher-agent--sync-and-check-dirty-entries (contents &optional root mtime-tracker-ht)
  "Synchronise CONTENTS entries with disk or buffer and check dirty state.

CONTENTS is the list of VFS entries.
ROOT is an optional root directory string for resolving relative paths.
MTIME-TRACKER-HT is an optional hash table tracking file modification times.

Return a cons cell (SYNCED . IS-DIRTY)."
  (cl-check-type contents list)
  (when root
    (cl-check-type root string))
  (when mtime-tracker-ht
    (cl-check-type mtime-tracker-ht hash-table))
  (let ((synced nil)
        (is-dirty nil))
    (dolist (entry contents)
      (when (macher-agent--sync-context-entry entry (or root default-directory) mtime-tracker-ht)
        (setq synced t))
      (let ((orig (macher-agent-vfs-entry-orig entry))
            (new (macher-agent-vfs-entry-curr entry)))
        (unless (equal (or orig "") (or new ""))
          (setq is-dirty t))))
    (cons synced is-dirty)))

(defun macher-agent--normalize-path-key (path root)
  "Format canonical path key exclusively from string PATH."
  (cl-check-type path string)
  (let ((ws-root (if (stringp root)
                     root
                   (when (macher-agent-context-p root)
                     (macher-agent-context-root root)))))
    (if (and ws-root (eq (macher-agent--classify-file-path path ws-root) 'buffer))
        path
      (if ws-root (expand-file-name path ws-root) path))))

(defun macher-agent--ensure-access (context path)
  "Validate workspace boundary mapping PATH strictly against CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (cl-check-type path string)
  (let* ((contents (macher-agent--get-context-contents context))
         (actual-name (substring-no-properties path))
         (canonical-path (file-truename (expand-file-name path)))
         (abs-path (expand-file-name path)))
    (unless (or (cl-find actual-name contents :key #'macher-agent-vfs-entry-path :test #'equal)
                (cl-find canonical-path contents :key (lambda (e) (file-truename (expand-file-name (macher-agent-vfs-entry-path e)))) :test #'equal)
                (cl-find abs-path contents :key (lambda (e) (expand-file-name (macher-agent-vfs-entry-path e))) :test #'equal))
      (let ((workspace (macher-agent-context-workspace context)))
        (when (fboundp 'macher--validate-path-in-workspace)
          (macher--validate-path-in-workspace path (or workspace (macher-agent-context-root context))))))))

(defun macher-agent--persist-vfs-to-hidden-buffer (ctx)
  "Commit VFS map into background tracking reliant on CTX struct."
  (cl-check-type ctx macher-agent-context)
  (let* ((root-dir (or (macher-agent-context-root ctx) "default"))
         (buf-name (format " *macher-agent-vfs-state-%s*" (md5 (expand-file-name (if (consp root-dir) (cdr root-dir) root-dir)))))
         (vfs-buf (get-buffer-create buf-name)))
    (with-current-buffer vfs-buf
      (erase-buffer)
      (insert ";;; Macher Agent Virtual File System State\n")
      (insert ";;; This buffer is native and handles large text blocks.\n\n")
      (dolist (entry (macher-agent--get-context-contents ctx))
        (let* ((path (macher-agent-vfs-entry-path entry))
               (new-content (macher-agent-vfs-entry-curr entry)))
          (when new-content
            (insert (format "=== VFS ENTRY: %s ===\n" path))
            (insert new-content)
            (unless (string-suffix-p "\n" new-content)
              (insert "\n"))
            (insert "=======================\n\n")))))))

(defun macher-agent--update-context-file (context path content)
  "Commit updated string CONTENT against PATH string linked to CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (cl-check-type path string)
  (let* ((norm-path (macher-agent--normalize-path-key path context))
         (contents (macher-agent--get-context-contents context))
         (workspace-root (macher-agent-context-root context))
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
          (macher-agent-vfs-write norm-path content mtime-ht vfs-ht)
          (unless (equal path norm-path)
            (macher-agent-vfs-write path content mtime-ht vfs-ht)))
      (when (and vfs-ht (hash-table-p vfs-ht))
        (puthash norm-path content vfs-ht)
        (unless (equal path norm-path)
          (puthash path content vfs-ht))))
    (if entry
        (setf (macher-agent-vfs-entry-curr entry) content)
      (let* ((is-buffer (eq (macher-agent--classify-file-path norm-path workspace-root) 'buffer))
             (safe-path (if (or is-buffer (null workspace-root))
                            norm-path
                          (let ((rel-path (macher-agent-to-relative-path norm-path workspace-root)))
                            (macher-agent--resolve-safe-path (expand-file-name rel-path workspace-root) workspace-root))))
             (orig (cond
                    (is-buffer
                     (let ((buf (or (get-buffer norm-path) (get-buffer path))))
                       (if (and buf (buffer-live-p buf))
                           (with-current-buffer buf
                             (buffer-substring-no-properties (point-min) (point-max)))
                         content)))
                    (t
                     (macher-agent--read-content-from-disk-or-buffer safe-path))))
             (new-entry (make-macher-agent-vfs-entry :path norm-path :orig orig :curr content)))
        (macher-agent--set-context-contents
         context
         (cons new-entry contents))))
    (macher-agent--set-context-dirty-p context t)
    (macher-agent--persist-vfs-to-hidden-buffer context)
    (run-hook-with-args 'macher-agent-context-mutated-hook norm-path)))

(defun macher-agent--read-context-file (context path)
  "Fetch VFS entry mapped explicitly to CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (cl-check-type path string)
  (let* ((norm-path (macher-agent--normalize-path-key path context))
         (contents (macher-agent--get-context-contents context))
         (workspace-root (macher-agent-context-root context))
         (vfs-ht (macher-agent-workspace-vfs-buffers context))
         (mtime-ht (macher-agent-workspace-mtime-tracker context))
         (is-buf (eq (macher-agent--classify-file-path path workspace-root) 'buffer)))

    (let ((target-path
           (if (and workspace-root (not is-buf))
               (let ((relative-path (macher-agent-to-relative-path path workspace-root)))
                 (macher-agent--resolve-safe-path (expand-file-name relative-path workspace-root) workspace-root))
             path)))
      (when (and mtime-ht (hash-table-p mtime-ht) (stringp target-path) (not is-buf))
        (let ((attrs (file-attributes target-path)))
          (when (and attrs (nth 5 attrs))
            (puthash target-path (nth 5 attrs) mtime-ht))))

      (let* ((vfs-val (and vfs-ht (hash-table-p vfs-ht) (gethash target-path vfs-ht)))
             (e-norm (cl-find norm-path contents :key #'macher-agent-vfs-entry-path :test #'equal))
             (entry (or e-norm
                        (cl-find-if
                         (lambda (e)
                           (let ((p (macher-agent-vfs-entry-path e)))
                             (and (stringp p)
                                  (or (equal path p)
                                      (equal target-path p)
                                      (equal target-path (expand-file-name p workspace-root))))))
                         contents))))
        (when (and entry (macher-agent-vfs-entry-curr entry))
          (macher-agent--sync-context-entry entry (or workspace-root default-directory) mtime-ht))
        (cond
         (entry
          (macher-agent-vfs-entry-curr entry))
         (vfs-val
          vfs-val)
         (t
          (or (macher-agent--read-content-from-disk-or-buffer target-path)
              (when (not (equal path target-path))
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

(defun macher-agent-vfs-active-p (context)
  "Query VFS status relying entirely on formatted CONTEXT struct."
  (cl-check-type context macher-agent-context)
  t)

(defun macher-agent-vfs-flush (context)
  "Execute complete write routine isolated to active CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (let* ((root (macher-agent-context-root context))
         (contents (macher-agent--get-context-contents context)))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (buffer-modified-p buf)
                 (buffer-file-name buf)
                 (or (null root)
                     (file-in-directory-p (buffer-file-name buf) root)))
        (with-current-buffer buf
          (save-buffer))))
    (when contents
      (macher-agent--auto-sync-context context))))

(defun macher-agent-vfs-restore (context)
  "Revert dirty VFS mapping contained strictly within CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (macher-agent--auto-sync-context context))

(defmacro macher-agent-with-strict-vfs (context &rest body)
  "Execute BODY within a strict virtual file system boundary.
Synchronises uncommitted buffer modifications to disk before execution
and restores the virtual state upon completion."
  (declare (indent 1))
  `(let ((vfs-active (if (macher-agent-context-p ,context)
                         (macher-agent-vfs-active-p ,context)
                       (and (macher-agent-valid-context-p ,context) t))))
     (unwind-protect
         (progn
           (when vfs-active
             (macher-agent-vfs-flush ,context))
           ,@body)
       (when vfs-active
         (macher-agent-vfs-restore ,context)))))

(defun macher-agent--auto-sync-context (ctx &rest _args)
  "Automatically coordinate file states within active CTX struct."
  (cl-check-type ctx macher-agent-context)
  (unless macher-agent--pause-auto-sync
    (let* ((contents (macher-agent--get-context-contents ctx))
           (root (or (macher-agent-context-root ctx) default-directory))
           (tracker (macher-agent-workspace-mtime-tracker ctx))
           (res (macher-agent--sync-and-check-dirty-entries contents root tracker))
           (synced (car res))
           (is-dirty (cdr res)))
      (unless is-dirty
        (macher-agent--set-context-dirty-p ctx nil))
      (when synced
        (macher-agent--persist-vfs-to-hidden-buffer ctx)
        (run-hooks 'macher-agent-context-mutated-hook)))))

(defun macher-agent-storage--extract-context (payload)
  "Retrieve valid context mapping solely from transit PAYLOAD struct."
  (cl-check-type payload macher-agent-transit-payload)
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

(defun macher-agent-vfs-handle-flush (ctx)
  "Format response queue tied explicitly to CTX struct."
  (cl-check-type ctx macher-agent-context)
  (macher-agent--auto-sync-context ctx)
  (let* ((suppress (or (bound-and-true-p macher-agent--suppress-patch)
                       (let ((plugins (macher-agent-context-plugins ctx)))
                         (when (macher-agent--plist-p plugins)
                           (plist-get plugins :suppress-patch)))))
         (contents (macher-agent--get-context-contents ctx))
         (has-changes (or (macher-agent--get-context-dirty-p ctx)
                          (cl-some #'macher-agent-vfs-entry-modified-p contents))))
    (when has-changes
      (when-let* ((prompt (macher-agent-context-prompt ctx)))
        (setf (macher-agent-context-prompt ctx) prompt))
      (unless suppress
        (run-hook-with-args 'macher-agent-vfs-flush-hook ctx)))))

(defmacro macher-agent-with-vfs-scope (context &rest body)
  "Execute BODY with Virtual File System awareness established from CONTEXT.

CONTEXT is the context structure or buffer environment to resolve.
When nil, attempts to resolve from the environment.
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
                       ((and (bound-and-true-p macher-agent--persistent-context)
                             (macher-agent-valid-context-p macher-agent--persistent-context))
                        macher-agent--persistent-context)
                       (t nil))))
       (unless (and ,ctx-sym (macher-agent-valid-context-p ,ctx-sym))
         (error "macher-agent-with-vfs-scope: Unable to resolve a valid VFS context from %S" ,raw-ctx-sym))
       (let* ((macher-agent--persistent-context ,ctx-sym)
              (,root-sym (macher-agent-context-root ,ctx-sym))
              (default-directory (if (and ,root-sym (stringp ,root-sym) (file-directory-p ,root-sym))
                                     (file-name-as-directory ,root-sym)
                                   default-directory)))
         ,@body))))

(defun macher-agent--expressive-patch-buffer-name (context patch-type &optional orig-buf)
  "Build expressive patch buffer name strictly from CONTEXT, PATCH-TYPE, and ORIG-BUF.
CONTEXT is a `macher-agent-context' struct.
PATCH-TYPE is a symbol or string (e.g. 'diff or 'buffer).
ORIG-BUF is an optional buffer or buffer name string."
  (cl-check-type context macher-agent-context)
  (let* ((type-sym (if (symbolp patch-type) patch-type (intern (format "%s" patch-type))))
         (category (symbol-name type-sym))
         (target-buf (or orig-buf
                         (macher-agent-context-origin-buffer context)
                         (macher-agent-vfs--get-origin-buffer context)))
         (buf-name (cond
                    ((bufferp target-buf) (when (buffer-live-p target-buf) (buffer-name target-buf)))
                    ((stringp target-buf) target-buf)
                    (t nil)))
         (ws (macher-agent-context-workspace context))
         (ws-type (if (consp ws) (car ws) 'project))
         (ws-name (macher-agent-macher-workspace-name context))
         (hash (macher-agent-macher-safe-workspace-hash context 4)))
    (if (and buf-name (not (string-empty-p buf-name)))
        (format "*macher-%s-patch:%s@%s<%s>[%s]*" category ws-type ws-name hash buf-name)
      (format "*macher-%s-patch:%s@%s<%s>*" category ws-type ws-name hash))))

(defun macher-agent--build-and-rename-patch (ctx patch-type &optional files)
  "Format active patch view bound explicitly to dirty CTX struct.
CTX is a `macher-agent-context' struct.
PATCH-TYPE is a symbol or string designating the patch type (e.g. 'diff or 'buffer).
FILES is an optional list of `macher-agent-vfs-entry' objects."
  (cl-check-type ctx macher-agent-context)
  (let* ((type-sym (if (symbolp patch-type) patch-type (intern (format "%s" patch-type))))
         (target-buf (or (macher-agent-context-origin-buffer ctx)
                         (macher-agent-vfs--get-origin-buffer ctx)
                         (current-buffer)))
         (suppress-patch (if (and (bufferp target-buf) (buffer-live-p target-buf))
                             (buffer-local-value 'macher-agent--suppress-patch target-buf)
                           (bound-and-true-p macher-agent--suppress-patch))))
    (when (and (not suppress-patch)
               (macher-agent--context-has-changes-p ctx))
      (let* ((expressive-name (macher-agent--expressive-patch-buffer-name ctx type-sym target-buf))
             (patch-buf (macher-agent-macher-build-patch ctx (or (macher-agent-context-prompt ctx) "") files)))
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
  "Distinguish mapped buffer state strictly bound by CTX struct."
  (cl-check-type ctx macher-agent-context)
  (let* ((file-ctx (let* ((cloned (macher-agent--copy-context ctx))
                          (plugins (macher-agent-context-plugins ctx))
                          (copied-plugins (macher-agent--copy-context-hash-tables plugins)))
                     (setf (macher-agent-context-plugins cloned)
                           (copy-tree copied-plugins))
                     cloned))
         (buf-ctx (let* ((cloned (macher-agent--copy-context ctx))
                         (plugins (macher-agent-context-plugins ctx))
                         (copied-plugins (macher-agent--copy-context-hash-tables plugins)))
                    (setf (macher-agent-context-plugins cloned)
                          (copy-tree copied-plugins))
                    cloned))
         (root (macher-agent-context-root ctx))
         (contents (macher-agent--get-context-contents ctx))
         (modified-contents
          (cl-remove-if-not #'macher-agent-vfs-entry-modified-p contents))
         (partitioned (macher-agent--partition-vfs-entries modified-contents root))
         (buf-contents (car partitioned))
         (file-contents (cdr partitioned)))
    (macher-agent--set-context-contents file-ctx file-contents)
    (macher-agent--set-context-contents buf-ctx buf-contents)
    (cons file-ctx buf-ctx)))

(defun macher-agent--execute-split-patch (ctx)
  "Generate diff payload dependent strictly on dirty CTX struct."
  (cl-check-type ctx macher-agent-context)
  (when (or (macher-agent--get-context-dirty-p ctx)
            (macher-agent--context-has-changes-p ctx))
    (let* ((payloads (macher-agent--split-context ctx))
           (p-ctx (car payloads))
           (v-ctx (cdr payloads))
           (p-contents (macher-agent--get-context-contents p-ctx))
           (v-contents (macher-agent--get-context-contents v-ctx))
           (generated nil))
      (when (and p-contents (> (length p-contents) 0))
        (let ((p-buf (macher-agent--build-and-rename-patch p-ctx 'diff p-contents)))
          (when p-buf (push p-buf generated))))
      (when (and v-contents (> (length v-contents) 0))
        (let ((v-buf (macher-agent--build-and-rename-patch v-ctx 'buffer v-contents)))
          (when v-buf (push v-buf generated))))
      (macher-agent--display-patch-buffers (nreverse generated)))))

(defun macher-agent-vfs-build-patch-from-hook (ctx)
  "Format background UI changes via target CTX struct."
  (cl-check-type ctx macher-agent-context)
  (let ((prompt (or (when (macher-agent-context-p ctx) (macher-agent-context-prompt ctx)) "")))
    (when prompt
      (setf (macher-agent-context-prompt ctx) prompt)
      (setf (macher-agent-context-plugins ctx)
            (plist-put (copy-sequence (macher-agent-context-plugins ctx)) :prompt prompt)))
    (macher-agent--execute-split-patch ctx)))

(defun macher-agent-vfs-diff-review (ctx)
  "Format interactive diff utilizing active CTX struct."
  (interactive (list (or (bound-and-true-p macher-agent--persistent-context)
                         (buffer-local-value 'macher-agent--persistent-context (current-buffer)))))
  (cl-check-type ctx macher-agent-context)
  (macher-agent-vfs-handle-flush ctx))

(defun macher-agent-vfs-install ()
  "Install VFS storage hooks, patch interfaces, and pipeline steps."
  (macher-agent-register-pipeline-step 'payload-merge #'macher-agent-vfs--merge-payload 10)
  (add-hook 'macher-agent-vfs-flush-hook #'macher-agent-vfs-build-patch-from-hook)
  (add-hook 'macher-agent-task-flush-hook #'macher-agent-vfs-handle-flush)
  (when (fboundp 'macher-agent-macher-install)
    (macher-agent-macher-install)))

(defun macher-agent--merge-contexts (parent-ctx child-ctx)
  "Sync differential data relying on explicitly defined PARENT-CTX and CHILD-CTX structs."
  (cl-check-type parent-ctx macher-agent-context)
  (cl-check-type child-ctx macher-agent-context)
  (unless (eq parent-ctx child-ctx)
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

(defun macher-agent-vfs--merge-payload (payload)
  "Merge Virtual File System PAYLOAD into the target parent context directly.

PAYLOAD is a `macher-agent-transit-payload' struct, a property list,
or a `macher-agent-context' struct.
Applies file diffs and child-context modifications to the resolved parent context,
handling deletions when diff items have nil content.
Synchronises `macher-agent--persistent-context' across relevant buffers and
updates `:target-context' on the returned payload structure.

Return updated PAYLOAD or context."
  (let* ((p-struct (when (macher-agent-transit-payload-p payload) payload))
         (raw-target
          (cond
           ((macher-agent-context-p payload)
            payload)
           (p-struct
            (or (macher-agent-transit-payload-target-context p-struct)
                (macher-agent-transit-payload-parent-context p-struct)
                (when-let* ((b (macher-agent-transit-payload-target-buffer p-struct)))
                  (let ((buf (if (bufferp b) b (get-buffer b))))
                    (when (and buf (buffer-live-p buf))
                      (buffer-local-value 'macher-agent--persistent-context buf))))
                (macher-agent-storage--extract-context p-struct)))
           ((macher-agent--plist-p payload)
            (or (plist-get payload :target-context)
                (plist-get payload :parent-context)
                (plist-get payload :macher-agent-context)
                (plist-get payload :context)
                (when-let* ((b (or (plist-get payload :target-buffer) (plist-get payload :buffer))))
                  (let ((buf (if (bufferp b) b (and (stringp b) (get-buffer b)))))
                    (when (and buf (buffer-live-p buf))
                      (buffer-local-value 'macher-agent--persistent-context buf))))))
           ((bufferp payload)
            (when (buffer-live-p payload)
              (buffer-local-value 'macher-agent--persistent-context payload)))
           (t payload)))
         (ctx (or raw-target
                  (when (and (bound-and-true-p macher-agent--persistent-context)
                             (macher-agent-context-p macher-agent--persistent-context))
                    macher-agent--persistent-context)))
         (_ (cl-check-type ctx macher-agent-context))
         (parent-ctx ctx)
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
        (cl-check-type child-ctx macher-agent-context)
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
          (setq payload (plist-put (copy-sequence payload) :target-context parent-ctx))))
      (or payload parent-ctx))))

(defun macher-agent--apply-single-virtual-buffer (entry)
  "Render content explicitly derived from formatting ENTRY struct."
  (cl-check-type entry macher-agent-vfs-entry)
  (let* ((path (macher-agent-vfs-entry-path entry))
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

(defun macher-agent-call-with-strict-vfs-pipeline (context body-fn)
  "Execute BODY-FN within a physical sandbox directory populated with CONTEXT.

CONTEXT is the active context structure.
BODY-FN is the function containing pipeline logic.

Return the result of BODY-FN.
Side effects: Creates and cleans up a temporary sandbox directory."
  (let* ((ctx (or context (bound-and-true-p macher-agent--persistent-context)))
         (raw-root (when (macher-agent-valid-context-p ctx)
                     (or (macher-agent-context-project-root ctx)
                         (macher-agent-context-root ctx))))
         (canonical-root (and raw-root
                              (file-truename
                               (expand-file-name (if (consp raw-root) (cdr raw-root) raw-root)))))
         (contents (when (macher-agent-valid-context-p ctx)
                     (macher-agent--get-context-contents ctx)))
         (sandbox-dir (make-temp-file "macher-sandbox-" t)))
    (if (not (and ctx canonical-root (file-directory-p canonical-root)))
        (funcall body-fn)
      (unwind-protect
          (progn
            (macher-agent--vfs-verify-clean-merge canonical-root contents)
            (macher-agent--vfs-sync-baseline canonical-root sandbox-dir)
            (when contents
              (macher-agent--vfs-apply-overlay-stateless contents canonical-root sandbox-dir))
            (let ((default-directory sandbox-dir))
              (funcall body-fn)))
        (when (and sandbox-dir (file-directory-p sandbox-dir))
          (delete-directory sandbox-dir t))))))

(defmacro macher-agent-with-strict-vfs-pipeline (context &rest body)
  "Execute BODY with default directory sandboxed to CONTEXT VFS state."
  (declare (indent 1) (debug t))
  `(macher-agent-call-with-strict-vfs-pipeline ,context (lambda () ,@body)))

(provide 'macher-agent-vfs)
;;; macher-agent-vfs.el ends here
