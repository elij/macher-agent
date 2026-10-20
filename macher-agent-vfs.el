;;; macher-agent-vfs.el --- Layer 2 VFS Client for Macher -*- lexical-binding: t; -*-

;;; Commentary:
;; Virtual File System client managing optimistic concurrency, non-blocking
;; asynchronous sandbox hydration, and buffer-local agent contexts without
;; physical disk mutation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(eval-and-compile
  (require 'gv)
  (let* ((root (file-name-directory (or load-file-name buffer-file-name default-directory)))
         (macher-dir (expand-file-name "macher" root))
         (gptel-dir (expand-file-name "gptel" root)))
    (when (file-directory-p macher-dir)
      (add-to-list 'load-path macher-dir))
    (when (file-directory-p gptel-dir)
      (add-to-list 'load-path gptel-dir))))
(require 'gptel)
(require 'macher-agent-core)
(require 'macher-agent-presets)
(require 'macher-agent-gptel)
(require 'macher-agent-macher)

(declare-function mailcap-file-name-to-mime-type "mailcap" (file-name))

(cl-defstruct (macher-agent-vfs-entry
               (:constructor macher-agent-make-vfs-entry)
               (:copier copy-macher-agent-vfs-entry))
  "Virtual file system entry tuple representing a managed file or buffer."
  (path nil :type (or null string))
  (orig nil :type (or null string))
  (curr nil :type (or null string))
  (pending-patch t :type boolean))

(defalias 'make-macher-agent-vfs-entry #'macher-agent-make-vfs-entry)

(defvar macher-agent--vfs-lock-table (make-hash-table :test 'equal)
  "Hash table tracking resource path locks in VFS.")

(defvar macher-agent--vfs-lock-queues (make-hash-table :test 'equal)
  "Hash table tracking waiting lock acquisition queues per resource path.")

(defvar macher-agent--pause-auto-sync nil
  "When non-nil, `macher-agent--auto-sync-context' will silently abort.")

(defvar macher-agent-vfs-flush-hook nil
  "Event hook triggered after Virtual File System processes modifications.")

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

(defun macher-agent-vfs--get-origin-buffer (ctx)
  "Retrieve owner or root orchestrator buffer from CTX struct."
  (cl-check-type ctx macher-agent-context)
  (or (macher-agent-context-owner-buffer ctx)
      (plist-get (macher-agent-context-plugins ctx) :origin-buffer)
      (plist-get (macher-agent-vfs--get-state ctx) :origin-buffer)))

(defun macher-agent--get-context-contents (ctx)
  "Extract file contents strictly from CTX struct."
  (cl-check-type ctx macher-agent-context)
  (plist-get (macher-agent-vfs--get-state ctx) :contents))

(defun macher-agent--set-context-contents (ctx val)
  "Set file contents VAL strictly on CTX struct."
  (cl-check-type ctx macher-agent-context)
  (cl-check-type val list)
  (let ((state (macher-agent-vfs--get-state ctx)))
    (macher-agent-vfs--set-state ctx (plist-put state :contents val))))

(defalias 'macher-agent-context-contents #'macher-agent--get-context-contents)

(gv-define-setter macher-agent-context-contents (val ctx)
  `(macher-agent--set-context-contents ,ctx ,val))

(defun macher-agent--get-context-dirty-p (ctx)
  "Check dirty flag strictly on CTX struct."
  (cl-check-type ctx macher-agent-context)
  (or (plist-get (macher-agent-vfs--get-state ctx) :dirty)
      (plist-get (macher-agent-vfs--get-state ctx) :dirty-p)))

(defun macher-agent--set-context-dirty-p (ctx is-dirty)
  "Set dirty flag IS-DIRTY strictly on CTX struct."
  (cl-check-type ctx macher-agent-context)
  (let ((state (macher-agent-vfs--get-state ctx)))
    (macher-agent-vfs--set-state ctx (plist-put (plist-put state :dirty is-dirty) :dirty-p is-dirty))))

(defun macher-agent-workspace-vfs-buffers (ctx)
  "Retrieve VFS buffers hash-table for CTX."
  (cl-check-type ctx macher-agent-context)
  (macher-agent--workspace-get-hash-table ctx :vfs-buffers))

(defun macher-agent-vfs-entry-to-macher-entry (entry &optional root)
  "Convert VFS ENTRY to a Macher content cell using optional ROOT."
  (cond
   ((macher-agent-vfs-entry-p entry)
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
   ((macher-agent-context-p vfs)
    (macher-agent--get-context-contents vfs))
   ((listp vfs)
    (if (plist-member vfs :contents)
        (plist-get vfs :contents)
      vfs))
   (t nil)))

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

(defun macher-agent-vfs-release-actor-locks (actor-ids)
  "Release all VFS locks and queued lock requests held by ACTOR-IDS.
ACTOR-IDS is a list of actor identifiers (such as buffer names or task IDs).
Queued waiters matching ACTOR-IDS are removed from `macher-agent--vfs-lock-queues'.
For paths currently locked by any actor in ACTOR-IDS, the lock is transferred
to the next queued waiter (invoking its callback) or released."
  (let ((actors (cl-remove-if #'null (if (listp actor-ids) actor-ids (list actor-ids)))))
    (when (and actors (hash-table-p macher-agent--vfs-lock-queues))
      (maphash
       (lambda (path queue)
         (when (listp queue)
           (let ((filtered (cl-remove-if (lambda (item)
                                           (member (car item) actors))
                                         queue)))
             (if filtered
                 (puthash path filtered macher-agent--vfs-lock-queues)
               (remhash path macher-agent--vfs-lock-queues)))))
       macher-agent--vfs-lock-queues))
    (when (and actors (hash-table-p macher-agent--vfs-lock-table))
      (let ((paths-to-release nil))
        (maphash
         (lambda (path lock-state)
           (let ((owner (car-safe lock-state)))
             (when (member owner actors)
               (push path paths-to-release))))
         macher-agent--vfs-lock-table)
        (dolist (path paths-to-release)
          (let ((queue (gethash path macher-agent--vfs-lock-queues nil)))
            (if (and queue (consp queue))
                (let* ((next (car queue))
                       (next-task-id (car next))
                       (next-cb (cdr next)))
                  (if (cdr queue)
                      (puthash path (cdr queue) macher-agent--vfs-lock-queues)
                    (remhash path macher-agent--vfs-lock-queues))
                  (puthash path (cons next-task-id 1) macher-agent--vfs-lock-table)
                  (when (and next-cb (not (eq next-cb #'ignore)))
                    (condition-case nil
                        (funcall next-cb "Resource lock acquired.")
                      (error nil))))
              (remhash path macher-agent--vfs-lock-table)
              (remhash path macher-agent--vfs-lock-queues))))))))

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
             (task-id (or (plist-get payload :task-id)
                          (md5 (format "%s%s%s%s" (emacs-pid) (current-time) (random) (recent-keys)))))
             (explicit-cb (plist-get payload :callback))
             (cb (or explicit-cb
                     (when path
                       (or (gethash path macher-agent--vfs-callbacks)
                           (gethash path macher-agent--pending-callbacks))))))
        (when path
          (let* ((lock-state (gethash path macher-agent--vfs-lock-table))
                 (current-owner (car-safe lock-state))
                 (ref-count (or (cdr-safe lock-state) 0)))
            (if (or (null lock-state) (equal current-owner task-id))
                (progn
                  (puthash path (cons task-id (1+ ref-count)) macher-agent--vfs-lock-table)
                  (when (and cb (not (eq cb #'ignore)))
                    (unless explicit-cb
                      (remhash path macher-agent--vfs-callbacks)
                      (remhash path macher-agent--pending-callbacks))
                    (funcall cb "Resource lock acquired.")))
              (if cb
                  (progn
                    (unless explicit-cb
                      (remhash path macher-agent--vfs-callbacks)
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

(defun macher-agent-vfs-make-entry (path orig curr &optional pending-patch)
  "Create a native `macher-agent-vfs-entry' struct."
  (macher-agent-make-vfs-entry :path path :orig orig :curr curr
                               :pending-patch (if (null pending-patch) t pending-patch)))

(defun macher-agent-vfs-entry-modified-p (entry)
  "Return non-nil if ENTRY has been modified from its original content."
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
  "Generate normalised sandbox directory explicitly from UNSAFE-PATH."
  (cl-check-type unsafe-path string)
  (let* ((canonical-base (file-name-as-directory (file-truename (expand-file-name root))))
         (canonical-resolved (file-truename (expand-file-name unsafe-path canonical-base))))
    (if (or (and (file-directory-p canonical-base) (file-in-directory-p canonical-resolved canonical-base))
            (string-prefix-p canonical-base (file-name-as-directory canonical-resolved))
            (string= (directory-file-name canonical-base) (directory-file-name canonical-resolved)))
        canonical-resolved
      (error "SECURITY ERROR: Path traversal jailbreak detected: %s" unsafe-path))))

(defun macher-agent--read-content-from-disk-or-buffer (path)
  "Extract content string guaranteed by PATH string structure."
  (cl-check-type path string)
  (cl-assert (not (string-empty-p path)))
  (let* ((buf (or (get-file-buffer path) (get-buffer path)))
         (is-media (and (file-exists-p path) (macher-agent-media-file-p path)))
         (file-exists (file-exists-p path))
         (trust-buffer (and buf (buffer-live-p buf)
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

(defun macher-agent--read-string (offset limit &optional text show-line-numbers)
  "Extract bounded string fragment relying on strictly typed OFFSET and LIMIT."
  (cl-check-type offset integer)
  (cl-check-type limit integer)
  (when text (cl-check-type text string))
  (let* ((lines (split-string (or text "") "\n"))
         (total-lines (length lines))
         (start-line (if (> offset 0) (1- offset) (if (< offset 0) (max 0 (+ total-lines offset)) 0)))
         (num-lines (if (> limit 0) limit (if (< limit 0) (max 0 (+ total-lines limit)) (- total-lines start-line))))
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
      (error "Multiple matches found for text. Set replace_all to true or provide more context: %s" old-text))
     (t
      (replace-regexp-in-string (regexp-quote old-text) new-text content t t)))))

(defun macher-agent--build-rsync-cmd (src dest)
  "Construct an rsync command driven by Git that includes untracked files in submodules."
  (let* ((src-dir (file-name-as-directory (expand-file-name src)))
         (dest-dir (file-name-as-directory (expand-file-name dest))))
    (unless (executable-find "git")
      (error "Macher-Agent: Git executable not found in PATH"))
    (format "cd %s && git rev-parse --is-inside-work-tree >/dev/null 2>&1 && { git -c core.quotePath=false ls-files -z -c --recurse-submodules; git -c core.quotePath=false ls-files -z -o --exclude-standard; git submodule foreach --quiet --recursive 'git -c core.quotePath=false ls-files -z -o --exclude-standard | perl -0ne \"print \\\"\\$ENV{displaypath}/\\$_\\\"\"'; } | perl -0ne 'print if -e $_' | rsync -a --delete --from0 --files-from=- %s %s"
            (shell-quote-argument src-dir)
            (shell-quote-argument src-dir)
            (shell-quote-argument dest-dir))))

(defun macher-agent--partition-vfs-entries (contents &optional root-dir)
  "Split raw VFS CONTENTS into pure virtual and physical lists."
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
  "Categorise string PATH into standard VFS archetypes."
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
  "Collect raw file list for EXPANDED-DIR."
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
          (not (member base '(".git" "target" "node_modules" ".Trash" "Library" ".cache" ".config")))
          (condition-case nil (progn (directory-files d) t) (error nil))))))))

(defun macher-agent--update-entry-content-cells (entry new-orig new-curr)
  "Update original and current content cells of VFS ENTRY."
  (setf (macher-agent-vfs-entry-orig entry) new-orig)
  (setf (macher-agent-vfs-entry-curr entry) new-curr)
  entry)

(defun macher-agent--sync-context-entry (entry root &optional context-or-mtime)
  "Synchronise ENTRY with live buffer or disk under ROOT, replacing both orig and curr on external edit.
When physical file was deleted externally, removes stale entry and returns non-nil."
  (cl-check-type entry macher-agent-vfs-entry)
  (cl-check-type root string)
  (let* ((path (macher-agent-vfs-entry-path entry))
         (abs-path (if (not (file-name-absolute-p path)) (expand-file-name path root) path))
         (orig (macher-agent-vfs-entry-orig entry))
         (new (macher-agent-vfs-entry-curr entry))
         (buf (or (get-file-buffer path) (get-file-buffer abs-path) (get-buffer path)))
         (live-buf (and buf (buffer-live-p buf)))
         (buf-content (when live-buf
                        (with-current-buffer buf
                          (buffer-substring-no-properties (point-min) (point-max)))))
         (disk-content (when (file-exists-p abs-path)
                         (ignore-errors
                           (with-temp-buffer
                             (insert-file-contents abs-path)
                             (buffer-string)))))
         (current-state (cond
                         ((and live-buf (buffer-modified-p buf)) buf-content)
                         (disk-content disk-content)
                         (live-buf buf-content)
                         (t nil)))
         (is-buf (eq (macher-agent--classify-file-path path root) 'buffer))
         (ctx (cond
               ((macher-agent-context-p context-or-mtime) context-or-mtime)
               ((bound-and-true-p macher-agent--persistent-context)
                macher-agent--persistent-context)
               ((macher-agent-context-p root) root)
               (t (or (macher-agent-resolve-context root)
                      (macher-agent-resolve-context (current-buffer)))))))
    (cond
     ((and current-state (not (equal current-state orig)))
      (unless (or (equal current-state new)
                  (equal (or orig "") (or new "")))
        (message "Macher-Agent VFS: Replaced entry '%s' due to external edits."
                 (file-name-nondirectory path)))
      (macher-agent--update-entry-content-cells entry current-state current-state)
      t)
     ((and (not is-buf) orig (null current-state))
      (let ((has-pending (and (fboundp 'macher-agent-vfs-entry-pending-patch)
                              (macher-agent-vfs-entry-pending-patch entry)
                              (not (equal orig new)))))
        (if has-pending
            nil
          (when (and ctx (macher-agent-valid-context-p ctx))
            (let ((contents (macher-agent--get-context-contents ctx)))
              (macher-agent--set-context-contents
               ctx (cl-remove entry contents :test #'eq))))
          t)))
     (t nil))))

(defun macher-agent--sync-and-check-dirty-entries (contents &optional root context)
  "Synchronise CONTENTS entries with disk or buffer and check dirty state."
  (let ((synced nil)
        (is-dirty nil))
    (dolist (entry contents)
      (when (macher-agent--sync-context-entry entry (or root default-directory) context)
        (setq synced t))
      (let ((orig (macher-agent-vfs-entry-orig entry))
            (new (macher-agent-vfs-entry-curr entry)))
        (unless (equal (or orig "") (or new ""))
          (setq is-dirty t))))
    (cons synced is-dirty)))

(defun macher-agent--normalize-path-key (path root)
  "Format canonical path key exclusively from string PATH."
  (cl-check-type path string)
  (let ((ws-root (if (stringp root) root (when (macher-agent-context-p root) (macher-agent-context-project-root root)))))
    (if (and ws-root (eq (macher-agent--classify-file-path path ws-root) 'buffer))
        path
      (if ws-root (expand-file-name path ws-root) path))))

(defun macher-agent--ensure-access (context path)
  "Validate workspace boundary mapping PATH strictly against CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (cl-check-type path string)
  (let* ((contents (macher-agent--get-context-contents context))
         (vfs-ht (macher-agent-workspace-vfs-buffers context))
         (actual-name (substring-no-properties path))
         (canonical-path (file-truename (expand-file-name path)))
         (abs-path (expand-file-name path)))
    (unless (or (and vfs-ht (gethash actual-name vfs-ht))
                (cl-find actual-name contents :key #'macher-agent-vfs-entry-path :test #'equal)
                (cl-find canonical-path contents :key (lambda (e) (file-truename (expand-file-name (macher-agent-vfs-entry-path e)))) :test #'equal)
                (cl-find abs-path contents :key (lambda (e) (expand-file-name (macher-agent-vfs-entry-path e))) :test #'equal))
      (let ((live-buf (get-buffer actual-name))
            (workspace (or (macher-agent-context-workspace context)
                           (macher-agent-context-root context))))
        (if (and live-buf (not (buffer-file-name live-buf)))
            (error "SECURITY ERROR: Buffer '%s' is not in the agent's authorised scope" actual-name)
          (macher-agent-macher-validate-path
           (or (and live-buf (buffer-file-name live-buf)) actual-name)
           workspace))))))

;;;###autoload
(defun macher-agent-add-buffer-to-scope (buffer)
  "Add explicitly resolved BUFFER to active VFS scope."
  (interactive "bAdd buffer to agent scope: ")
  (let* ((buf (if (stringp buffer) (get-buffer buffer) buffer))
         (ctx (macher-agent-resolve-context (current-buffer))))
    (cl-check-type buf buffer)
    (if (not (macher-agent-valid-context-p ctx))
        (user-error "No active Macher Agent context found")
      (let ((buf-name (buffer-name buf))
            (content (with-current-buffer buf
                       (buffer-substring-no-properties (point-min) (point-max)))))
        (macher-agent--update-context-file ctx buf-name content)
        (message "SUCCESS: Added '%s' to the agent's authorised scope." buf-name)))))

(defun macher-agent--update-context-file (context path content)
  "Commit updated string CONTENT (or nil for deletion) against PATH in CONTEXT."
  (cl-check-type context macher-agent-context)
  (cl-check-type path string)
  (when content (cl-check-type content string))
  (let* ((norm-path (macher-agent--normalize-path-key path context))
         (ws-root (macher-agent-context-project-root context))
         (is-buf (eq (macher-agent--classify-file-path norm-path ws-root) 'buffer))
         (vfs-ht (macher-agent-workspace-vfs-buffers context))
         (contents (macher-agent--get-context-contents context))
         (entry (or (cl-find norm-path contents :key #'macher-agent-vfs-entry-path :test #'equal)
                    (cl-find path contents :key #'macher-agent-vfs-entry-path :test #'equal)
                    (cl-find-if
                     (lambda (e)
                       (let ((e-norm (macher-agent--normalize-path-key (macher-agent-vfs-entry-path e) context)))
                         (and e-norm (equal e-norm norm-path))))
                     contents)))
         (orig (if entry
                   (macher-agent-vfs-entry-orig entry)
                 (if is-buf
                     (let ((buf (or (get-buffer norm-path) (get-buffer path))))
                       (if (and buf (buffer-live-p buf))
                           (with-current-buffer buf
                             (buffer-substring-no-properties (point-min) (point-max)))
                         content))
                   (let ((safe-path (if (or is-buf (null ws-root))
                                        norm-path
                                      (macher-agent--resolve-safe-path
                                       (expand-file-name (macher-agent-to-relative-path norm-path ws-root) ws-root)
                                       ws-root))))
                     (macher-agent--read-content-from-disk-or-buffer safe-path)))))
         (prior (if entry (macher-agent-vfs-entry-curr entry) orig))
         (did-mutate (not (equal prior content))))
    (cond
     ((null content)
      (when (hash-table-p vfs-ht)
        (remhash norm-path vfs-ht)
        (unless (equal path norm-path)
          (remhash path vfs-ht)))
      (if (and orig (not is-buf))
          (progn
            (if entry
                (progn
                  (setf (macher-agent-vfs-entry-curr entry) nil)
                  (setf (macher-agent-vfs-entry-pending-patch entry) t))
              (let ((new-entry (macher-agent-make-vfs-entry
                                :path norm-path
                                :orig orig
                                :curr nil
                                :pending-patch t)))
                (macher-agent--set-context-contents
                 context (cons new-entry (macher-agent--get-context-contents context)))))
            (macher-agent--set-context-dirty-p context t))
        (when entry
          (macher-agent--set-context-contents
           context (cl-remove entry (macher-agent--get-context-contents context) :test #'eq)))
        (let* ((updated-contents (macher-agent--get-context-contents context))
               (has-dirty (cl-some #'macher-agent-vfs-entry-modified-p updated-contents)))
          (macher-agent--set-context-dirty-p context (and has-dirty t)))))

     ((not did-mutate)
      (if (and entry (equal content orig) (or orig (not (file-exists-p norm-path))))
          (progn
            (when (and is-buf (hash-table-p vfs-ht))
              (remhash norm-path vfs-ht)
              (unless (equal path norm-path) (remhash path vfs-ht)))
            (macher-agent--set-context-contents
             context (cl-remove entry (macher-agent--get-context-contents context) :test #'eq))
            (let* ((updated-contents (macher-agent--get-context-contents context))
                   (has-dirty (cl-some (lambda (e)
                                         (not (equal (macher-agent-vfs-entry-orig e)
                                                     (macher-agent-vfs-entry-curr e))))
                                       updated-contents)))
              (macher-agent--set-context-dirty-p context (and has-dirty t))))))

     (t
      (if (and (equal content orig) (or orig (not (file-exists-p norm-path))))
          (progn
            (when (and is-buf (hash-table-p vfs-ht))
              (remhash norm-path vfs-ht)
              (unless (equal path norm-path) (remhash path vfs-ht)))
            (when entry
              (macher-agent--set-context-contents
               context (cl-remove entry (macher-agent--get-context-contents context) :test #'eq)))
            (let* ((updated-contents (macher-agent--get-context-contents context))
                   (has-dirty (cl-some (lambda (e)
                                         (not (equal (macher-agent-vfs-entry-orig e)
                                                     (macher-agent-vfs-entry-curr e))))
                                       updated-contents)))
              (macher-agent--set-context-dirty-p context (and has-dirty t))))
        (when (and is-buf (hash-table-p vfs-ht))
          (puthash norm-path content vfs-ht)
          (unless (equal path norm-path)
            (puthash path content vfs-ht)))
        (if entry
            (progn
              (setf (macher-agent-vfs-entry-curr entry) content)
              (setf (macher-agent-vfs-entry-pending-patch entry) t))
          (let ((new-entry (macher-agent-make-vfs-entry
                            :path norm-path
                            :orig orig
                            :curr content
                            :pending-patch t)))
            (macher-agent--set-context-contents
             context (cons new-entry (macher-agent--get-context-contents context)))))
        (macher-agent--set-context-dirty-p context t))))

    (when did-mutate
      (dolist (key (delete-dups (list norm-path path)))
        (when-let* ((cb (or (gethash key macher-agent--vfs-callbacks)
                            (gethash key macher-agent--pending-callbacks))))
          (remhash key macher-agent--vfs-callbacks)
          (remhash key macher-agent--pending-callbacks)
          (funcall cb (or content ""))))
      (run-hook-with-args 'macher-agent-context-mutated-hook norm-path context))))

(defun macher-agent--read-context-file (context path)
  "Fetch VFS entry mapped explicitly to CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (cl-check-type path string)
  (let* ((norm-path (macher-agent--normalize-path-key path context))
         (workspace-root (macher-agent-context-project-root context))
         (vfs-ht (macher-agent-workspace-vfs-buffers context))
         (contents (macher-agent--get-context-contents context))
         (is-buf (eq (macher-agent--classify-file-path path workspace-root) 'buffer))
         (entry (or (cl-find norm-path contents :key #'macher-agent-vfs-entry-path :test #'equal)
                    (cl-find path contents :key #'macher-agent-vfs-entry-path :test #'equal))))
    (if is-buf
        (or (and (hash-table-p vfs-ht)
                 (or (gethash norm-path vfs-ht) (gethash path vfs-ht)))
            (when entry (macher-agent-vfs-entry-curr entry))
            (macher-agent--read-content-from-disk-or-buffer path)
            (error "ERROR: Buffer '%s' does not exist" path))
      (let ((target-path (if workspace-root
                             (macher-agent--resolve-safe-path
                              (expand-file-name (macher-agent-to-relative-path path workspace-root) workspace-root)
                              workspace-root)
                           path)))
        (when (and entry (macher-agent-vfs-entry-curr entry))
          (macher-agent--sync-context-entry entry (or workspace-root default-directory) context))
        (or (when entry (macher-agent-vfs-entry-curr entry))
            (and (hash-table-p vfs-ht)
                 (or (gethash norm-path vfs-ht) (gethash path vfs-ht)))
            (macher-agent--read-content-from-disk-or-buffer target-path)
            (error "ERROR: File/Buffer '%s' does not exist" path))))))

(defun macher-agent--vfs-apply-overlay-stateless (contents ws-root sandbox-dir)
  "Apply virtual CONTENTS overlay to SANDBOX-DIR statelessly."
  (let ((sandbox-root (file-name-as-directory (expand-file-name sandbox-dir))))
    (mapc (lambda (entry)
            (let* ((relative-path (macher-agent-to-relative-path (macher-agent-vfs-entry-path entry) ws-root))
                   (new-content (macher-agent-vfs-entry-curr entry))
                   (target-path (macher-agent--resolve-safe-path (expand-file-name relative-path sandbox-root) sandbox-root)))
              (if (stringp new-content)
                  (progn
                    (let ((dir (file-name-directory target-path)))
                      (when (and dir (not (file-directory-p dir))) (make-directory dir t)))
                    (with-temp-file target-path (insert new-content)))
                (when (file-exists-p target-path)
                  (delete-file target-path)))))
          contents)))

(defun macher-agent-vfs-active-p (context)
  "Query VFS status relying entirely on formatted CONTEXT struct."
  (cl-check-type context macher-agent-context)
  t)

(defun macher-agent--auto-sync-context (ctx &optional path)
  "Automatically coordinate file states within active CTX struct."
  (cl-check-type ctx macher-agent-context)
  (unless macher-agent--pause-auto-sync
    (let* ((contents (macher-agent--get-context-contents ctx))
           (root (or (macher-agent-context-project-root ctx) default-directory))
           (res (macher-agent--sync-and-check-dirty-entries contents root ctx))
           (synced (car res))
           (is-dirty (cdr res)))
      (unless is-dirty
        (macher-agent--set-context-dirty-p ctx nil))
      (when (and synced
                 (or (null path)
                     (string-match-p "/skills/" path)
                     (string-match-p "/\\.skills/" path)
                     (string-match-p "/commands/" path)
                     (string-match-p "/scripts/" path)))
        (run-hook-with-args 'macher-agent-context-mutated-hook path ctx)))))

(defun macher-agent-storage--extract-context (payload)
  "Retrieve valid context mapping solely from transit PAYLOAD struct."
  (cl-check-type payload macher-agent-transit-payload)
  (macher-agent-context-from-payload payload))

(defun macher-agent-vfs-handle-flush (ctx)
  "Format response queue tied explicitly to CTX struct."
  (cl-check-type ctx macher-agent-context)
  (macher-agent--auto-sync-context ctx)
  (let* ((owner-buf (macher-agent-context-owner-buffer ctx))
         (suppress (or (bound-and-true-p macher-agent--suppress-patch)
                       (and (bufferp owner-buf)
                            (buffer-live-p owner-buf)
                            (buffer-local-value 'macher-agent--suppress-patch owner-buf))
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
  "Execute BODY with Virtual File System awareness established from CONTEXT."
  (declare (indent 1) (debug t))
  (let ((raw-ctx-sym (gensym "raw-ctx-"))
        (ctx-sym (gensym "ctx-"))
        (root-sym (gensym "root-")))
    `(let* ((,raw-ctx-sym ,context)
            (,ctx-sym (macher-agent-resolve-context ,raw-ctx-sym)))
       (unless (and ,ctx-sym (macher-agent-valid-context-p ,ctx-sym))
         (error "macher-agent-with-vfs-scope: Unable to resolve a valid VFS context from %S" ,raw-ctx-sym))
       (let* ((macher-agent--persistent-context ,ctx-sym)
              (,root-sym (macher-agent-context-project-root ,ctx-sym))
              (default-directory (if (and ,root-sym (stringp ,root-sym) (file-directory-p ,root-sym))
                                     (file-name-as-directory ,root-sym)
                                   default-directory)))
         ,@body))))

(defun macher-agent--expressive-patch-buffer-name (context patch-type &optional orig-buf)
  "Build expressive patch buffer name from CONTEXT, PATCH-TYPE, and ORIG-BUF."
  (cl-check-type context macher-agent-context)
  (let* ((type-sym (if (symbolp patch-type) patch-type (intern (format "%s" patch-type))))
         (category (symbol-name type-sym))
         (target-buf (or orig-buf
                         (macher-agent-context-owner-buffer context)
                         (macher-agent-vfs--get-origin-buffer context)))
         (buf-name (cond ((bufferp target-buf) (when (buffer-live-p target-buf) (buffer-name target-buf)))
                         ((stringp target-buf) target-buf)
                         (t nil)))
         (ws (macher-agent-context-workspace context))
         (ws-type (if (consp ws) (car ws) 'project))
         (ws-name (macher-agent-macher-workspace-name context))
         (hash (macher-agent-macher-safe-workspace-hash context)))
    (if (and buf-name (not (string-empty-p buf-name)))
        (format "*macher-%s-patch:%s@%s<%s>[%s]*" category ws-type ws-name hash buf-name)
      (format "*macher-%s-patch:%s@%s<%s>*" category ws-type ws-name hash))))

(defun macher-agent--build-and-rename-patch (ctx patch-type &optional files)
  "Format active patch view bound explicitly to dirty CTX struct."
  (cl-check-type ctx macher-agent-context)
  (let* ((type-sym (if (symbolp patch-type) patch-type (intern (format "%s" patch-type))))
         (target-buf (or (macher-agent-context-owner-buffer ctx)
                         (macher-agent-vfs--get-origin-buffer ctx)
                         (current-buffer)))
         (suppress-patch (if (and (bufferp target-buf) (buffer-live-p target-buf))
                             (buffer-local-value 'macher-agent--suppress-patch target-buf)
                           (bound-and-true-p macher-agent--suppress-patch))))
    (when (and (not suppress-patch)
               (macher-agent--context-has-changes-p ctx))
      (let* ((expressive-name (macher-agent--expressive-patch-buffer-name ctx type-sym target-buf))
             (entries (macher-agent--gather-vfs-entries ctx files))
             (patch-buf (macher-agent-macher-build-patch ctx (or (macher-agent-context-prompt ctx) "") entries)))
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
  "Display GENERATED-BUFFERS in split windows if multiple buffers exist."
  (when (> (length generated-buffers) 1)
    (let ((bufs (nreverse generated-buffers)))
      (delete-other-windows)
      (split-window-vertically)
      (set-window-buffer (selected-window) (car bufs))
      (set-window-buffer (next-window) (cadr bufs)))))

(defun macher-agent--context-has-changes-p (context)
  "Determine whether CONTEXT contains actual VFS modifications."
  (and context
       (cl-some #'macher-agent-vfs-entry-modified-p
                (macher-agent--get-context-contents context))))

(defun macher-agent--split-context (ctx)
  "Distinguish mapped buffer state strictly bound by CTX struct."
  (cl-check-type ctx macher-agent-context)
  (let* ((file-ctx (macher-agent--clone-context ctx))
         (buf-ctx (macher-agent--clone-context ctx))
         (root (macher-agent-context-project-root ctx))
         (contents (macher-agent--get-context-contents ctx))
         (modified-contents (cl-remove-if-not #'macher-agent-vfs-entry-modified-p contents))
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
  (let ((prompt (or (macher-agent-context-prompt ctx) "")))
    (when prompt
      (setf (macher-agent-context-prompt ctx) prompt)
      (setf (macher-agent-context-plugins ctx)
            (plist-put (copy-sequence (macher-agent-context-plugins ctx)) :prompt prompt)))
    (macher-agent--execute-split-patch ctx)))

(defun macher-agent-vfs-diff-review (ctx)
  "Format interactive diff utilising active CTX struct."
  (interactive (list (macher-agent-resolve-context (current-buffer))))
  (cl-check-type ctx macher-agent-context)
  (macher-agent-vfs-handle-flush ctx))

(defun macher-agent--gather-vfs-entries (ctx &optional files)
  "Gather VFS entries from CTX, optionally restricted to FILES."
  (cl-check-type ctx macher-agent-context)
  (or files
      (macher-agent--get-context-contents ctx)
      (macher-agent-vfs-modified-files ctx)))

(defun macher-agent-vfs--workspace-read-file (context path &optional only-staged)
  "Read PATH from CONTEXT VFS.
When ONLY-STAGED is non-nil, only return content from staged VFS entries."
  (when (and (macher-agent-valid-context-p context) (stringp path))
    (if only-staged
        (let* ((contents (macher-agent--get-context-contents context))
               (vfs-buffers (macher-agent-workspace-vfs-buffers context))
               (entry (cl-find path contents :key #'macher-agent-vfs-entry-path :test #'equal)))
          (cond
           (entry (macher-agent-vfs-entry-curr entry))
           ((hash-table-p vfs-buffers) (gethash path vfs-buffers))
           (t nil)))
      (ignore-errors (macher-agent--read-context-file context path)))))

(defun macher-agent-vfs--workspace-list-files (context)
  "Return list of non-deleted file paths tracked in CONTEXT VFS."
  (when (macher-agent-valid-context-p context)
    (cl-loop for entry in (macher-agent--get-context-contents context)
             for path = (macher-agent-vfs-entry-path entry)
             for curr = (macher-agent-vfs-entry-curr entry)
             when (and (stringp path) curr)
             collect path)))

(defun macher-agent-vfs--clone-context-step (new-ctx)
  "Deep copy VFS entries in NEW-CTX and reset `:pending-patch' to nil."
  (when (macher-agent-context-p new-ctx)
    (let* ((orig-contents (macher-agent--get-context-contents new-ctx))
           (cloned-contents
            (mapcar (lambda (entry)
                      (let ((copy (copy-macher-agent-vfs-entry entry)))
                        (setf (macher-agent-vfs-entry-pending-patch copy) nil)
                        copy))
                    orig-contents)))
      (macher-agent--set-context-contents new-ctx cloned-contents)
      (macher-agent--set-context-dirty-p new-ctx nil)))
  new-ctx)

(defvar macher-agent--wrapped-tools-hash (make-hash-table :test 'eq)
  "Track wrapped `gptel-tool' instances in a hash table.")

(defun macher-agent--extract-tool-name (tool)
  "Extract canonical name string from TOOL."
  (let ((actual-tool (if (consp tool) (cdr tool) tool)))
    (cond
     ((and (fboundp 'gptel-tool-p) (gptel-tool-p actual-tool))
      (gptel-tool-name actual-tool))
     ((stringp tool) tool)
     ((symbolp tool) (symbol-name tool))
     (t (error "Invalid tool representation: %S" tool)))))

(defun macher-agent--get-search-in-workspace-tool ()
  "Return the tool struct defined in skills/scripts/search_in_workspace.el."
  (let ((tool (when (boundp 'macher-agent-search-in-workspace-tool)
                (symbol-value 'macher-agent-search-in-workspace-tool))))
    (unless (macher-tool-valid-p tool)
      (let* ((script-candidates
              (delq nil
                    (list
                     (when (bound-and-true-p macher-agent--bundled-skills-dir)
                       (expand-file-name "scripts/search_in_workspace.el" macher-agent--bundled-skills-dir))
                     (locate-file "skills/scripts/search_in_workspace.el" load-path)
                     (locate-file "search_in_workspace.el" load-path)
                     (expand-file-name "skills/scripts/search_in_workspace.el" default-directory)
                     (let ((root (locate-dominating-file default-directory "skills")))
                       (when root
                         (expand-file-name "skills/scripts/search_in_workspace.el" root))))))
             (found (cl-find-if #'file-exists-p script-candidates)))
        (when found
          (load found nil t))
        (setq tool (when (boundp 'macher-agent-search-in-workspace-tool)
                     (symbol-value 'macher-agent-search-in-workspace-tool)))))
    (or tool
        (when (bound-and-true-p macher-agent-tools-registry)
          (gethash "search_in_workspace" macher-agent-tools-registry))
        (when (bound-and-true-p gptel--known-tools)
          (or (alist-get "search_in_workspace" (alist-get "perception" gptel--known-tools nil nil #'equal) nil nil #'equal)
              (ignore-errors (gptel-get-tool "search_in_workspace")))))))

(defun macher-agent--wrap-single-tool (tool)
  "Wrap single TOOL to intercept upstream macher context and route to VFS."
  (when-let* (((not (gethash tool macher-agent--wrapped-tools-hash)))
              (tool-name (macher-agent--extract-tool-name tool)))
    (if-let* (((string= (format "%s" tool-name) "search_in_workspace"))
              (replacement (macher-agent--get-search-in-workspace-tool)))
        (progn
          (setf (gptel-tool-name tool) (gptel-tool-name replacement)
                (gptel-tool-function tool) (gptel-tool-function replacement)
                (gptel-tool-description tool) (gptel-tool-description replacement)
                (gptel-tool-args tool) (gptel-tool-args replacement)
                (gptel-tool-category tool) (gptel-tool-category replacement)
                (gptel-tool-async tool) (gptel-tool-async replacement)
                (gptel-tool-confirm tool) (gptel-tool-confirm replacement)
                (gptel-tool-include tool) (gptel-tool-include replacement))
          (puthash tool t macher-agent--wrapped-tools-hash))
      (when-let* ((category (gptel-tool-category tool))
                  ((string= (format "%s" category) "macher"))
                  (orig-fn (gptel-tool-function tool)))
        (setf (gptel-tool-function tool)
              (lambda (orphaned-context callback &rest args)
                (let* ((fsm (macher-agent-get-active-fsm))
                       (target-buf (or (when (and fsm (fboundp 'gptel-fsm-info))
                                         (ignore-errors (plist-get (gptel-fsm-info fsm) :buffer)))
                                       (current-buffer)))
                       (agent-ctx (or (when fsm
                                        (macher-agent-gptel-context-from-fsm fsm target-buf))
                                      (macher-agent-resolve-context target-buf))))
                  (unless agent-ctx
                    (error "Macher Agent: Could not resolve master agent context from buffer %s" target-buf))
                  (let* ((before-contents (macher-agent--get-context-contents agent-ctx))
                         (root (or (macher-agent-context-project-root agent-ctx)
                                   default-directory))
                         (partitioned (macher-agent--partition-vfs-entries before-contents root))
                         (buffer-entries (car partitioned))
                         (file-entries (cdr partitioned))
                         (marshalled-files
                          (mapcar (lambda (entry)
                                    (let ((path (if (macher-agent-vfs-entry-p entry) (macher-agent-vfs-entry-path entry) (car-safe entry)))
                                          (orig (if (macher-agent-vfs-entry-p entry) (macher-agent-vfs-entry-orig entry) (when (consp (cdr-safe entry)) (cadr entry))))
                                          (curr (if (macher-agent-vfs-entry-p entry) (macher-agent-vfs-entry-curr entry) (if (consp (cdr-safe entry)) (cddr entry) (cdr-safe entry)))))
                                      (cons path (cons orig curr))))
                                  file-entries))
                         (ws (macher-agent-context-workspace agent-ctx))
                         (proxy-outcome
                          (macher-agent-macher-run-with-proxy-context
                           orphaned-context ws marshalled-files
                           (macher-agent-context-prompt agent-ctx)
                           target-buf orig-fn callback args))
                         (res (car proxy-outcome))
                         (raw-after-files (cdr proxy-outcome))
                         (after-files
                          (if (listp raw-after-files)
                              (mapcar (lambda (item)
                                        (cond
                                         ((macher-agent-vfs-entry-p item) item)
                                         ((consp item)
                                          (let ((path (car item))
                                                (rest (cdr item)))
                                            (if (consp rest)
                                                (make-macher-agent-vfs-entry :path path :orig (car rest) :curr (cdr rest))
                                              (make-macher-agent-vfs-entry :path path :orig nil :curr rest))))
                                         (t item)))
                                      raw-after-files)
                            raw-after-files))
                         (after-contents (append buffer-entries after-files)))
                    (macher-agent--set-context-contents agent-ctx after-contents)
                    (dolist (entry after-files)
                      (let* ((path (macher-agent-vfs-entry-path entry))
                             (orig (macher-agent-vfs-entry-orig entry))
                             (new (macher-agent-vfs-entry-curr entry))
                             (prev (cl-find path file-entries :key #'macher-agent-vfs-entry-path :test #'equal))
                             (p-curr (when prev (macher-agent-vfs-entry-curr prev))))
                        (when (or (and (not (equal orig new)) (not (equal new p-curr)))
                                  (and (null prev) (not (equal orig new))))
                          (macher-agent--update-context-file agent-ctx path new))))
                    (unless (equal before-contents after-contents)
                      (macher-agent--set-context-dirty-p agent-ctx t))
                    res))))
        (puthash tool t macher-agent--wrapped-tools-hash)))))

(defun macher-agent--wrap-macher-tools ()
  "Wrap all Macher tools to inject persistent VFS context."
  (when-let* ((tools-entry (assoc "macher" (bound-and-true-p gptel--known-tools))))
    (let* ((macher-tools (cdr tools-entry))
           (filtered (cl-remove-if
                      (lambda (item)
                        (let ((name (macher-agent--extract-tool-name item)))
                          (and name (string= (format "%s" name) "search_in_workspace"))))
                      macher-tools)))
      (setcdr tools-entry filtered)
      (dolist (item filtered)
        (let ((tool (if (consp item) (cdr item) item)))
          (when (macher-tool-valid-p tool)
            (macher-agent--wrap-single-tool tool)))))))

(defun macher-agent-vfs-install ()
  "Install VFS storage hooks, patch interfaces, and pipeline steps."
  (setq macher-agent-macher-entry-converter-function #'macher-agent-vfs-entry-to-macher-entry)
  (setq macher-agent-workspace-read-file-function #'macher-agent-vfs--workspace-read-file)
  (setq macher-agent-workspace-list-files-function #'macher-agent-vfs--workspace-list-files)
  (macher-agent-register-pipeline-step 'payload-merge #'macher-agent-vfs--merge-payload 10)
  (macher-agent-register-pipeline-step 'context-clone #'macher-agent-vfs--clone-context-step 10)
  (add-hook 'macher-agent-vfs-flush-hook #'macher-agent-vfs-build-patch-from-hook)
  (add-hook 'macher-agent-task-flush-hook #'macher-agent-vfs-handle-flush)
  (add-hook 'macher-agent-context-sync-hook #'macher-agent--auto-sync-context)
  (add-hook 'macher-agent-peer-sync-hook #'macher-agent--sync-baseline-context)
  (add-hook 'macher-agent-actor-reap-hook #'macher-agent-vfs-release-actor-locks)
  (macher-agent-macher-install #'macher-agent--collect-raw-files))

(defun macher-agent-vfs-uninstall ()
  "Uninstall VFS storage hooks, patch interfaces, and pipeline steps."
  (macher-agent-unregister-pipeline-step 'payload-merge #'macher-agent-vfs--merge-payload)
  (macher-agent-unregister-pipeline-step 'context-clone #'macher-agent-vfs--clone-context-step)
  (remove-hook 'macher-agent-vfs-flush-hook #'macher-agent-vfs-build-patch-from-hook)
  (remove-hook 'macher-agent-task-flush-hook #'macher-agent-vfs-handle-flush)
  (remove-hook 'macher-agent-context-sync-hook #'macher-agent--auto-sync-context)
  (remove-hook 'macher-agent-peer-sync-hook #'macher-agent--sync-baseline-context)
  (remove-hook 'macher-agent-actor-reap-hook #'macher-agent-vfs-release-actor-locks))

(macher-agent-vfs-install)

(defun macher-agent--sync-baseline-context (source-ctx target-ctx)
  "Copy entries from SOURCE-CTX into TARGET-CTX with `:pending-patch' nil.
Never overwrites entries in TARGET-CTX that already have `pending-patch' set to t."
  (cl-check-type source-ctx macher-agent-context)
  (cl-check-type target-ctx macher-agent-context)
  (let ((src-contents (macher-agent--get-context-contents source-ctx))
        (tgt-contents (macher-agent--get-context-contents target-ctx))
        (modified nil))
    (dolist (src-entry src-contents)
      (let* ((path (macher-agent-vfs-entry-path src-entry))
             (norm-path (macher-agent--normalize-path-key path target-ctx))
             (existing (or (cl-find path tgt-contents :key #'macher-agent-vfs-entry-path :test #'equal)
                           (when norm-path
                             (cl-find norm-path tgt-contents :key #'macher-agent-vfs-entry-path :test #'equal))
                           (cl-find-if
                            (lambda (e)
                              (let* ((p (macher-agent-vfs-entry-path e))
                                     (p-norm (and p (macher-agent--normalize-path-key p target-ctx))))
                                (or (and norm-path p-norm (equal p-norm norm-path))
                                    (file-equal-p p path)
                                    (and norm-path (file-equal-p p norm-path)))))
                            tgt-contents))))
        (cond
         ((and existing (macher-agent-vfs-entry-pending-patch existing))
          nil)
         (existing
          (setf (macher-agent-vfs-entry-orig existing) (macher-agent-vfs-entry-orig src-entry))
          (setf (macher-agent-vfs-entry-curr existing) (macher-agent-vfs-entry-curr src-entry))
          (setf (macher-agent-vfs-entry-pending-patch existing) nil)
          (setq modified t))
         (t
          (let ((new-entry (copy-macher-agent-vfs-entry src-entry)))
            (setf (macher-agent-vfs-entry-pending-patch new-entry) nil)
            (push new-entry tgt-contents)
            (setq modified t))))))
    (when modified
      (macher-agent--set-context-contents target-ctx tgt-contents))
    target-ctx))

(defun macher-agent--merge-contexts (parent-ctx child-ctx)
  "Sync differential data between PARENT-CTX and CHILD-CTX structs."
  (cl-check-type parent-ctx macher-agent-context)
  (cl-check-type child-ctx macher-agent-context)
  (unless (eq parent-ctx child-ctx)
    (let ((child-contents (macher-agent--get-context-contents child-ctx))
          (any-merged nil))
      (dolist (child-entry child-contents)
        (let* ((path (macher-agent-vfs-entry-path child-entry))
               (new (macher-agent-vfs-entry-curr child-entry))
               (pending (macher-agent-vfs-entry-pending-patch child-entry))
               (norm-path (macher-agent--normalize-path-key path parent-ctx))
               (parent-contents (macher-agent--get-context-contents parent-ctx))
               (parent-entry
                (or (cl-find path parent-contents :key #'macher-agent-vfs-entry-path :test #'equal)
                    (when norm-path
                      (cl-find norm-path parent-contents :key #'macher-agent-vfs-entry-path :test #'equal))
                    (cl-find-if
                     (lambda (e)
                       (let* ((p (macher-agent-vfs-entry-path e))
                              (p-norm (and p (macher-agent--normalize-path-key p parent-ctx))))
                         (or (and norm-path p-norm (equal p-norm norm-path))
                             (file-equal-p p path)
                             (and norm-path (file-equal-p p norm-path)))))
                     parent-contents))))
          (if (not pending)
              ;; Baseline entry: only copy to parent if parent has no entry for this path
              (unless parent-entry
                (let ((copy (copy-macher-agent-vfs-entry child-entry)))
                  (setf (macher-agent-vfs-entry-pending-patch copy) nil)
                  (macher-agent--set-context-contents
                   parent-ctx
                   (cons copy (macher-agent--get-context-contents parent-ctx)))))
            ;; Child actively modified this file (pending-patch is non-nil)
            (macher-agent--update-context-file parent-ctx path new)
            (setq any-merged t))))
      (when any-merged
        (macher-agent--set-context-dirty-p parent-ctx t))))
  parent-ctx)

(defun macher-agent-vfs--merge-payload (payload)
  "Merge Virtual File System PAYLOAD into target parent context directly."
  (let* ((p-struct (when (macher-agent-transit-payload-p payload) payload))
         (ctx (macher-agent-resolve-context payload))
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
                             ((and (macher-agent--plist-p pl) (plist-get pl :diff)) (plist-get pl :diff))
                             ((and (macher-agent--plist-p pl) (plist-get pl :vfs-diff)) (plist-get pl :vfs-diff))
                             ((listp pl) pl)
                             (t nil))))
                ((macher-agent--plist-p payload)
                 (let ((d (or (plist-get payload :diff) (plist-get payload :vfs-diff) (plist-get payload :payload))))
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
                             (macher-agent-make-vfs-entry
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
         (buf (cond ((bufferp path) (when (buffer-live-p path) path))
                    ((stringp path) (or (get-buffer path) (get-file-buffer path))))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (erase-buffer)
        (insert curr))
      t)))

(defun macher-agent-apply-virtual-buffers (&optional context)
  "Apply pending virtual edits to live Emacs buffers."
  (let* ((ctx (macher-agent-resolve-context context))
         (contents (when ctx (macher-agent--get-context-contents ctx)))
         (normalized-contents nil))
    (dolist (entry contents)
      (when-let* (((macher-agent-vfs-entry-p entry))
                  (path (macher-agent-vfs-entry-path entry))
                  ((and (stringp path) (not (string-empty-p path)))))
        (let* ((content (macher-agent-vfs-entry-curr entry))
               (trimmed (string-trim-right (or content ""))))
          (push (macher-agent-make-vfs-entry
                 :path path
                 :orig (or (macher-agent-vfs-entry-orig entry) trimmed)
                 :curr trimmed)
                normalized-contents))
        (macher-agent--apply-single-virtual-buffer entry)))
    (when (and ctx normalized-contents)
      (macher-agent--set-context-contents ctx (nreverse normalized-contents)))))

;;; Asynchronous VFS Sandbox Pipeline

(defun macher-agent-call-with-strict-vfs-pipeline (context-or-resolve callback-or-body &optional maybe-body-fn)
  "Hydrate a temporary VFS sandbox asynchronously and invoke BODY-FN.
Supports both 2 arguments (RESOLVE-FN BODY-FN) resolving context
from `(current-buffer)', and 3 arguments (CTX RESOLVE-FN BODY-FN).
CALLBACK must be a function receiving the final result."
  (let* ((three-args-p (not (null maybe-body-fn)))
         (raw-context (if three-args-p context-or-resolve (current-buffer)))
         (callback (if three-args-p callback-or-body context-or-resolve))
         (body-fn (if three-args-p maybe-body-fn callback-or-body)))
    (cl-check-type callback function)
    (cl-check-type body-fn function)
    (let* ((ctx (or (macher-agent-resolve-context raw-context)
                    (macher-agent-resolve-context (current-buffer))
                    (macher-agent-resolve-context nil)))
           (raw-root (when (macher-agent-valid-context-p ctx)
                       (macher-agent-context-project-root ctx)))
           (canonical-root (and raw-root (file-truename (expand-file-name raw-root))))
           (contents (when (macher-agent-valid-context-p ctx)
                       (macher-agent--get-context-contents ctx))))
      (if (not (and ctx canonical-root (file-directory-p canonical-root)))
          (funcall callback "ERROR: VFS sandbox requires a valid workspace context and existing root directory.")
        (condition-case err
            (let* ((sandbox-dir (make-temp-file "macher-sandbox-" t))
                   (sync-cmd (macher-agent--build-rsync-cmd canonical-root sandbox-dir))
                   (cleaned nil)
                   (cleanup-and-resolve
                    (lambda (result)
                      (unless cleaned
                        (setq cleaned t)
                        (when (and sandbox-dir (file-directory-p sandbox-dir))
                          (make-process :name "macher-sandbox-rm"
                                        :noquery t
                                        :command (list "rm" "-rf" sandbox-dir))))
                      (funcall callback result))))
              (make-process
               :name "macher-vfs-rsync"
               :noquery t
               :command (list shell-file-name shell-command-switch sync-cmd)
               :sentinel
               (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (if (not (= (process-exit-status proc) 0))
                       (funcall cleanup-and-resolve
                                (format "ERROR: Macher-Agent VFS requires a git-backed workspace or rsync failed (exit code %d): %s"
                                        (process-exit-status proc) canonical-root))
                     (condition-case overlay-err
                         (progn
                           (when contents
                             (macher-agent--vfs-apply-overlay-stateless contents canonical-root sandbox-dir))
                           (let ((default-directory (macher-agent--ensure-trailing-slash sandbox-dir))
                                 (arity (ignore-errors (func-arity body-fn))))
                             (if (or (eq (cdr-safe arity) 'many)
                                     (and (numberp (cdr-safe arity)) (>= (cdr-safe arity) 2)))
                                 (condition-case _
                                     (funcall body-fn sandbox-dir cleanup-and-resolve)
                                   (wrong-number-of-arguments
                                    (funcall body-fn cleanup-and-resolve)))
                               (condition-case _
                                   (funcall body-fn cleanup-and-resolve)
                                 (wrong-number-of-arguments
                                  (funcall body-fn sandbox-dir cleanup-and-resolve))))))
                       (error
                        (funcall cleanup-and-resolve
                                 (format "ERROR: VFS overlay failed: %s"
                                         (error-message-string overlay-err))))))))))
          (error
           (funcall callback (format "ERROR: %s" (error-message-string err)))))))))

(defun macher-agent-run-in-vfs-sandbox (ctx-or-command command-or-resolve &optional resolve-or-format maybe-format-fn)
  "Hydrate a VFS sandbox and execute COMMAND asynchronously.
Automatically manages the temporary output buffer, `make-process', sentinel
cleanup, and output truncation so `skills/scripts/*.el' never need manual
process boilerplate.
Supports both:
  (COMMAND RESOLVE-FN &optional FORMAT-FN)
  (CTX COMMAND RESOLVE-FN &optional FORMAT-FN)
COMMAND may be a shell command string or a list of program and arguments.
FORMAT-FN, when non-nil, is called as `(funcall format-fn exit-code output)'."
  (let* ((has-ctx (not (functionp command-or-resolve)))
         (ctx (if has-ctx ctx-or-command (current-buffer)))
         (command (if has-ctx command-or-resolve ctx-or-command))
         (resolve-fn (if has-ctx resolve-or-format command-or-resolve))
         (format-fn (if has-ctx maybe-format-fn resolve-or-format)))
    (cl-check-type resolve-fn function)
    (macher-agent-call-with-strict-vfs-pipeline
     ctx resolve-fn
     (lambda (sandbox-dir done)
       (let* ((default-directory (macher-agent--ensure-trailing-slash sandbox-dir))
              (out-buf (generate-new-buffer " *macher-sandbox-cmd*"))
              (cmd-list (if (stringp command)
                            (list shell-file-name shell-command-switch command)
                          command)))
         (condition-case err
             (make-process
              :name "macher-sandbox-cmd"
              :buffer out-buf
              :noquery t
              :command cmd-list
              :sentinel
              (lambda (proc _event)
                (when (memq (process-status proc) '(exit signal))
                  (let ((raw-out (if (buffer-live-p out-buf)
                                     (with-current-buffer out-buf
                                       (buffer-string))
                                   ""))
                        (code (process-exit-status proc)))
                    (when (buffer-live-p out-buf)
                      (kill-buffer out-buf))
                    (let ((formatted (if (functionp format-fn)
                                         (funcall format-fn code raw-out)
                                       (if (= code 0)
                                           raw-out
                                         (format "ERROR (exit %d):\n%s" code raw-out)))))
                      (funcall done (macher-agent--truncate-string formatted)))))))
           (error
            (when (buffer-live-p out-buf)
              (kill-buffer out-buf))
            (funcall done (format "ERROR: Failed to launch sandbox process: %s"
                                  (error-message-string err))))))))))

(defmacro macher-agent-with-strict-vfs-pipeline (context callback &rest body)
  "Hydrate CONTEXT VFS sandbox asynchronously, exposing `done' inside BODY."
  (declare (indent 2) (debug t))
  `(macher-agent-call-with-strict-vfs-pipeline
    ,context
    ,callback
    (lambda (done)
      ,@body)))

(provide 'macher-agent-vfs)
;;; macher-agent-vfs.el ends here
