;;; macher-agent-vfs.el --- Virtual File System plugin for Macher Agent -*- lexical-binding: t; -*-

;; Author: Elijah Charles
;; Version: 0.8.3.66
;; Package-Requires: ((emacs "30.1") (gptel "0.9.9.6") (macher "0.5.2") (macher-agent "0.8.3.66") (macher-agent-sandbox "0.8.3.66"))
;; Keywords: convenience, gptel, llm
;; URL: https://github.com/elij/macher-agent
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Virtual File System (VFS) plugin for Macher Agent managing optimistic
;; concurrency, external-write reconciliation, non-blocking asynchronous
;; Git/rsync sandbox hydration, upstream `macher' tool wrapping, and
;; buffer-local agent diff contexts without physical disk mutation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(eval-and-compile
  (require 'gv))
(require 'gptel)
(require 'macher-agent)
(require 'macher-agent-sandbox)
(require 'macher-agent-macher)

(declare-function mailcap-file-name-to-mime-type "mailcap" (file-name))

(defvar gptel-prompt-transform-functions)

(defconst macher-agent-vfs--package-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing the `macher-agent-vfs' library.")

(defun macher-agent-vfs-skills-directory ()
  "Return the bundled skills directory for `macher-agent-vfs', or nil."
  (let ((dir (expand-file-name "skills" macher-agent-vfs--package-dir)))
    (when (file-directory-p dir)
      dir)))

(cl-defstruct (macher-agent-vfs-entry
               (:constructor macher-agent-vfs-create-entry)
               (:copier macher-agent-vfs-copy-entry))
  "Virtual file system entry tuple representing a managed file or buffer."
  (path nil :type (or null string))
  (orig nil :type (or null string))
  (curr nil :type (or null string))
  (pending-patch t :type boolean)
  (stat-mtime nil)
  (stat-size nil)
  (stat-inode nil))

(cl-defstruct (macher-agent-vfs-layer
               (:constructor macher-agent-vfs--make-layer))
  "Copy-on-write VFS entry layer with an immutable parent chain."
  (parent nil)
  (entries (make-hash-table :test #'equal))
  (tombstones (make-hash-table :test #'equal))
  (owner nil)
  (frozen nil :type boolean))

(defvar macher-agent-vfs--raw-files-cache (make-hash-table :test #'equal)
  "Cache mapping workspace root strings to `(index-mtime timestamp . files)'.")

(defvar macher-agent-vfs--canon-root-cache (make-hash-table :test #'equal)
  "Cache mapping workspace root strings to canonical `file-truename' dirs.")

(defun macher-agent-vfs--canonical-root (root)
  "Return cached canonical `file-truename' directory string for ROOT."
  (when (and (stringp root) (not (string-empty-p root)))
    (or (and (hash-table-p macher-agent-vfs--canon-root-cache)
             (gethash root macher-agent-vfs--canon-root-cache))
        (let ((canon (ignore-errors
                       (file-name-as-directory
                        (file-truename (expand-file-name root))))))
          (when (and canon (hash-table-p macher-agent-vfs--canon-root-cache))
            (puthash root canon macher-agent-vfs--canon-root-cache))
          canon))))

(defvar macher-agent-vfs--raw-files-cache-ttl 2.0
  "Time-to-live in seconds for cached workspace file listings.")

(defvar macher-tool-output-max-length)

(defvar macher-agent-vfs-lock-table (make-hash-table :test 'equal)
  "Hash table tracking resource path locks in VFS.")

(defvar macher-agent-vfs-lock-queues (make-hash-table :test 'equal)
  "Hash table tracking waiting lock acquisition queues per resource path.")

(defvar macher-agent-vfs-pause-auto-sync nil
  "When non-nil, `macher-agent-vfs-auto-sync-context' will silently abort.")

(defvar macher-agent-vfs-flush-hook nil
  "Event hook triggered after Virtual File System processes modifications.")

(defvar macher-agent-vfs--wrapped-tools-hash (make-hash-table :test 'eq)
  "Hash table mapping wrapped `gptel-tool' instances to original slot plists.")

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

(defun macher-agent-vfs--owner-token (ctx)
  "Return or allocate the unique layer-ownership token for CTX."
  (let* ((state (macher-agent-vfs--get-state ctx))
         (tok (plist-get state :owner-token)))
    (or tok
        (let ((new-tok (list 'vfs-owner))
              (new-state (copy-sequence state)))
          (macher-agent-vfs--set-state
           ctx (plist-put new-state :owner-token new-tok))
          new-tok))))

(defun macher-agent-vfs--invalidate-contents-cache (ctx)
  "Clear cached materialised `:contents' projection on CTX."
  (let ((state (macher-agent-vfs--get-state ctx)))
    (when (plist-get state :contents-cache)
      (macher-agent-vfs--set-state
       ctx (plist-put (copy-sequence state) :contents-cache nil)))))

(defun macher-agent-vfs--ensure-layer (ctx)
  "Return the active `macher-agent-vfs-layer' for CTX, migrating legacy lists."
  (let* ((state (macher-agent-vfs--get-state ctx))
         (layer (plist-get state :layer)))
    (if (macher-agent-vfs-layer-p layer)
        layer
      (let* ((tok (or (plist-get state :owner-token) (list 'vfs-owner)))
             (legacy (plist-get state :contents))
             (entries (make-hash-table :test #'equal :size (max 16 (length legacy))))
             (new-layer (macher-agent-vfs--make-layer
                         :parent nil
                         :entries entries
                         :tombstones (make-hash-table :test #'equal)
                         :owner tok
                         :frozen nil)))
        (dolist (e legacy)
          (when (and (macher-agent-vfs-entry-p e)
                     (stringp (macher-agent-vfs-entry-path e)))
            (puthash (macher-agent-vfs-entry-path e) e entries)))
        (let ((new-state (copy-sequence state)))
          (setq new-state (plist-put new-state :owner-token tok))
          (setq new-state (plist-put new-state :layer new-layer))
          (setq new-state (plist-put new-state :contents nil))
          (setq new-state (plist-put new-state :contents-cache nil))
          (macher-agent-vfs--set-state ctx new-state))
        new-layer))))

(defun macher-agent-vfs--ensure-writable-layer (ctx)
  "Return a writable `macher-agent-vfs-layer' for CTX, CoW-wrapping if frozen."
  (let ((layer (macher-agent-vfs--ensure-layer ctx)))
    (if (not (macher-agent-vfs-layer-frozen layer))
        layer
      (let* ((tok (macher-agent-vfs--owner-token ctx))
             (child-layer (macher-agent-vfs--make-layer
                           :parent layer
                           :entries (make-hash-table :test #'equal)
                           :tombstones (make-hash-table :test #'equal)
                           :owner tok
                           :frozen nil))
             (state (copy-sequence (macher-agent-vfs--get-state ctx))))
        (setq state (plist-put state :layer child-layer))
        (setq state (plist-put state :contents-cache nil))
        (macher-agent-vfs--set-state ctx state)
        child-layer))))

(defun macher-agent-vfs--layer-lookup-pair (ctx key &optional alt-key)
  "Look up KEY (or ALT-KEY) in CTX's CoW layer chain.
Return `(ENTRY . LAYER)' or nil when absent or tombstoned."
  (let ((cur (macher-agent-vfs--ensure-layer ctx))
        (found nil)
        (done nil))
    (while (and cur (not done))
      (let ((tombs (macher-agent-vfs-layer-tombstones cur))
            (ents (macher-agent-vfs-layer-entries cur)))
        (cond
         ((or (and key (gethash key tombs))
              (and alt-key (not (equal alt-key key)) (gethash alt-key tombs)))
          (setq done t))
         ((and key (gethash key ents))
          (setq found (cons (gethash key ents) cur)
                done t))
         ((and alt-key (not (equal alt-key key)) (gethash alt-key ents))
          (setq found (cons (gethash alt-key ents) cur)
                done t))
         (t
          (setq cur (macher-agent-vfs-layer-parent cur))))))
    found))

(defun macher-agent-vfs--layer-has-ancestor-key-p (layer key)
  "Return non-nil if any ancestor of LAYER contains KEY."
  (let ((cur (macher-agent-vfs-layer-parent layer))
        (found nil))
    (while (and cur (not found))
      (if (gethash key (macher-agent-vfs-layer-tombstones cur))
          (setq cur nil)
        (if (gethash key (macher-agent-vfs-layer-entries cur))
            (setq found t)
          (setq cur (macher-agent-vfs-layer-parent cur)))))
    found))

(defun macher-agent-vfs--layer-put (ctx key entry)
  "Store ENTRY under KEY in CTX's writable CoW layer."
  (let ((layer (macher-agent-vfs--ensure-writable-layer ctx)))
    (remhash key (macher-agent-vfs-layer-tombstones layer))
    (puthash key entry (macher-agent-vfs-layer-entries layer))
    (macher-agent-vfs--invalidate-contents-cache ctx)
    entry))

(defun macher-agent-vfs--layer-remove (ctx key)
  "Remove KEY from CTX's CoW layer, inserting a tombstone when shadowed."
  (let ((layer (macher-agent-vfs--ensure-writable-layer ctx)))
    (remhash key (macher-agent-vfs-layer-entries layer))
    (if (macher-agent-vfs--layer-has-ancestor-key-p layer key)
        (puthash key t (macher-agent-vfs-layer-tombstones layer))
      (remhash key (macher-agent-vfs-layer-tombstones layer)))
    (macher-agent-vfs--invalidate-contents-cache ctx)))

(defun macher-agent-vfs--layer-remove-entry (ctx entry)
  "Remove ENTRY by path from CTX's CoW layer."
  (let ((p (and (macher-agent-vfs-entry-p entry) (macher-agent-vfs-entry-path entry))))
    (when (stringp p)
      (macher-agent-vfs--layer-remove ctx p))))

(defun macher-agent-vfs--layer-owned-entries (ctx)
  "Return list of active entries stored in layers owned by CTX.
Respects shadows and tombstones in newer layers; runs in O(|owned-delta|)."
  (let ((cur (macher-agent-vfs--ensure-layer ctx))
        (tok (macher-agent-vfs--owner-token ctx))
        (seen (make-hash-table :test #'equal))
        (res nil))
    (while (and cur (eq (macher-agent-vfs-layer-owner cur) tok))
      (maphash (lambda (k _v) (puthash k t seen))
               (macher-agent-vfs-layer-tombstones cur))
      (maphash (lambda (k v)
                 (unless (gethash k seen)
                   (puthash k t seen)
                   (push v res)))
               (macher-agent-vfs-layer-entries cur))
      (setq cur (macher-agent-vfs-layer-parent cur)))
    res))

(defun macher-agent-vfs--layer-all-pairs (ctx)
  "Return list of `(ENTRY . LAYER)' for every active entry visible in CTX."
  (let ((cur (macher-agent-vfs--ensure-layer ctx))
        (seen (make-hash-table :test #'equal))
        (res nil))
    (while cur
      (maphash (lambda (k _v) (puthash k t seen))
               (macher-agent-vfs-layer-tombstones cur))
      (maphash (lambda (k v)
                 (unless (gethash k seen)
                   (puthash k t seen)
                   (push (cons v cur) res)))
               (macher-agent-vfs-layer-entries cur))
      (setq cur (macher-agent-vfs-layer-parent cur)))
    res))

(defun macher-agent-vfs-context-contents (ctx)
  "Extract VFS file entry list from CTX struct's CoW layer chain."
  (cl-check-type ctx macher-agent-context)
  (let* ((state (macher-agent-vfs--get-state ctx))
         (cached (plist-get state :contents-cache)))
    (or cached
        (let ((tok (macher-agent-vfs--owner-token ctx))
              (pairs (macher-agent-vfs--layer-all-pairs ctx))
              (res nil))
          (dolist (pair pairs)
            (let ((entry (car pair))
                  (layer (cdr pair)))
              (if (eq (macher-agent-vfs-layer-owner layer) tok)
                  (push entry res)
                (let ((proj (macher-agent-vfs-copy-entry entry)))
                  (setf (macher-agent-vfs-entry-pending-patch proj) nil)
                  (push proj res)))))
          (when res
            (macher-agent-vfs--set-state
             ctx (plist-put (copy-sequence (macher-agent-vfs--get-state ctx))
                            :contents-cache res)))
          res))))

(defun macher-agent-vfs-set-context-contents (ctx val)
  "Set VFS file contents VAL strictly on CTX struct as a fresh writable layer."
  (cl-check-type ctx macher-agent-context)
  (cl-check-type val list)
  (let* ((state (copy-sequence (macher-agent-vfs--get-state ctx)))
         (tok (or (plist-get state :owner-token) (list 'vfs-owner)))
         (entries (make-hash-table :test #'equal :size (max 16 (length val))))
         (new-layer (macher-agent-vfs--make-layer
                     :parent nil
                     :entries entries
                     :tombstones (make-hash-table :test #'equal)
                     :owner tok
                     :frozen nil)))
    (dolist (e val)
      (when (and (macher-agent-vfs-entry-p e)
                 (stringp (macher-agent-vfs-entry-path e)))
        (puthash (macher-agent-vfs-entry-path e) e entries)))
    (setq state (plist-put state :owner-token tok))
    (setq state (plist-put state :layer new-layer))
    (setq state (plist-put state :contents nil))
    (setq state (plist-put state :contents-cache (when val (copy-sequence val))))
    (macher-agent-vfs--set-state ctx state)))

(gv-define-setter macher-agent-vfs-context-contents (val ctx)
  `(macher-agent-vfs-set-context-contents ,ctx ,val))

(defun macher-agent-vfs-context-dirty-p (ctx)
  "Check VFS dirty flag strictly on CTX struct."
  (cl-check-type ctx macher-agent-context)
  (or (plist-get (macher-agent-vfs--get-state ctx) :dirty)
      (plist-get (macher-agent-vfs--get-state ctx) :dirty-p)))

(defun macher-agent-vfs-set-context-dirty-p (ctx is-dirty)
  "Set VFS dirty flag IS-DIRTY strictly on CTX struct."
  (cl-check-type ctx macher-agent-context)
  (let ((state (copy-sequence (macher-agent-vfs--get-state ctx))))
    (macher-agent-vfs--set-state
     ctx (plist-put (plist-put state :dirty is-dirty) :dirty-p is-dirty))))

(defun macher-agent-vfs-scoped-buffers (ctx)
  "Retrieve workspace scoped buffers hash-table for CTX."
  (cl-check-type ctx macher-agent-context)
  (macher-agent-workspace-get-hash-table ctx :scoped-buffers))

(defun macher-agent-vfs-buffers (ctx)
  "Retrieve VFS buffers diff hash-table for CTX."
  (cl-check-type ctx macher-agent-context)
  (macher-agent-workspace-get-hash-table ctx :vfs-buffers))

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
    (macher-agent-vfs-context-contents vfs))
   ((listp vfs)
    (if (plist-member vfs :contents)
        (plist-get vfs :contents)
      vfs))
   (t nil)))

;;; Concurrency Locks

(defun macher-agent-vfs-release-lock (path &optional lock-state-or-task-id current-owner)
  "Relinquish VFS lock bound to PATH.
Uses optional LOCK-STATE-OR-TASK-ID and CURRENT-OWNER to verify ownership."
  (cl-check-type path string)
  (let* ((task-id (or current-owner lock-state-or-task-id))
         (lock-state (if (consp lock-state-or-task-id)
                         lock-state-or-task-id
                       (gethash path macher-agent-vfs-lock-table)))
         (actual-owner (or (car-safe lock-state) task-id))
         (ref-count (or (cdr-safe lock-state) 0)))
    (when (and lock-state (or (null task-id) (equal actual-owner task-id)))
      (if (> ref-count 1)
          (progn
            (puthash path (cons actual-owner (1- ref-count)) macher-agent-vfs-lock-table)
            t)
        (let ((queue (gethash path macher-agent-vfs-lock-queues nil)))
          (if (and queue (consp queue))
              (let* ((next (car queue))
                     (next-task-id (car next))
                     (next-cb (cdr next)))
                (if (cdr queue)
                    (puthash path (cdr queue) macher-agent-vfs-lock-queues)
                  (remhash path macher-agent-vfs-lock-queues))
                (puthash path (cons next-task-id 1) macher-agent-vfs-lock-table)
                (when next-cb
                  (condition-case nil
                      (funcall next-cb "Resource lock acquired.")
                    (error nil)))
                t)
            (remhash path macher-agent-vfs-lock-table)
            (remhash path macher-agent-vfs-lock-queues)
            t))))))

(defun macher-agent-vfs-release-actor-locks (actor-ids)
  "Release all VFS locks and queued lock requests held by ACTOR-IDS.
ACTOR-IDS is a list of actor identifiers (such as buffer names or task IDs).
Queued waiters matching ACTOR-IDS are removed from
`macher-agent-vfs-lock-queues'.  For paths currently locked by any actor in
ACTOR-IDS, the lock is transferred to the next queued waiter (invoking its
callback) or released."
  (let ((actors (cl-remove-if #'null (if (listp actor-ids) actor-ids (list actor-ids)))))
    (when (and actors (hash-table-p macher-agent-vfs-lock-queues))
      (maphash
       (lambda (path queue)
         (when (listp queue)
           (let ((filtered (cl-remove-if (lambda (item)
                                           (member (car item) actors))
                                         queue)))
             (if filtered
                 (puthash path filtered macher-agent-vfs-lock-queues)
               (remhash path macher-agent-vfs-lock-queues)))))
       macher-agent-vfs-lock-queues))
    (when (and actors (hash-table-p macher-agent-vfs-lock-table))
      (let ((paths-to-release nil))
        (maphash
         (lambda (path lock-state)
           (let ((owner (car-safe lock-state)))
             (when (member owner actors)
               (push path paths-to-release))))
         macher-agent-vfs-lock-table)
        (dolist (path paths-to-release)
          (let ((queue (gethash path macher-agent-vfs-lock-queues nil)))
            (if (and queue (consp queue))
                (let* ((next (car queue))
                       (next-task-id (car next))
                       (next-cb (cdr next)))
                  (if (cdr queue)
                      (puthash path (cdr queue) macher-agent-vfs-lock-queues)
                    (remhash path macher-agent-vfs-lock-queues))
                  (puthash path (cons next-task-id 1) macher-agent-vfs-lock-table)
                  (when (and next-cb (not (eq next-cb #'ignore)))
                    (condition-case nil
                        (funcall next-cb "Resource lock acquired.")
                      (error nil))))
              (remhash path macher-agent-vfs-lock-table)
              (remhash path macher-agent-vfs-lock-queues))))))))

(defun macher-agent-vfs-a2a-callback (payload)
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
                       (or (gethash path macher-agent-vfs-callbacks)
                           (gethash path macher-agent-pending-callbacks))))))
        (when path
          (let* ((lock-state (gethash path macher-agent-vfs-lock-table))
                 (current-owner (car-safe lock-state))
                 (ref-count (or (cdr-safe lock-state) 0)))
            (if (or (null lock-state) (equal current-owner task-id))
                (progn
                  (puthash path (cons task-id (1+ ref-count)) macher-agent-vfs-lock-table)
                  (when (and cb (not (eq cb #'ignore)))
                    (unless explicit-cb
                      (remhash path macher-agent-vfs-callbacks)
                      (remhash path macher-agent-pending-callbacks))
                    (funcall cb "Resource lock acquired.")))
              (if cb
                  (progn
                    (unless explicit-cb
                      (remhash path macher-agent-vfs-callbacks)
                      (remhash path macher-agent-pending-callbacks))
                    (let ((queue (gethash path macher-agent-vfs-lock-queues nil)))
                      (puthash path (append queue (list (cons task-id cb))) macher-agent-vfs-lock-queues)))
                (display-warning 'macher-agent-vfs (format "Resource '%s' is locked." path) :warning)))))))
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

;;; Entry, Path, and Canonical Content Helpers

(defun macher-agent-vfs-make-entry (path orig curr &optional pending-patch)
  "Create a `macher-agent-vfs-entry' struct for PATH.
Initialises ORIG, CURR, and optional PENDING-PATCH."
  (macher-agent-vfs-create-entry
   :path path :orig orig :curr curr
   :pending-patch (if (null pending-patch) t pending-patch)))

(defun macher-agent-vfs-entry-modified-p (entry)
  "Return non-nil if ENTRY has been modified from its original content."
  (let ((orig (macher-agent-vfs-entry-orig entry))
        (curr (macher-agent-vfs-entry-curr entry)))
    (not (equal orig curr))))

(defun macher-agent-vfs-media-file-p (path)
  "Identify if specific PATH string represents active media."
  (cl-check-type path string)
  (let ((mime (and (fboundp 'mailcap-file-name-to-mime-type)
                   (mailcap-file-name-to-mime-type path))))
    (or (and mime (or (string-prefix-p "image/" mime)
                      (string-prefix-p "video/" mime)
                      (string-prefix-p "audio/" mime)))
        (string-match-p
         "\\.\\(png\\|jpe?g\\|gif\\|webp\\|svg\\|pdf\\|mp4\\|mov\\|mp3\\|wav\\)$" path))))

(defun macher-agent-vfs-to-relative-path (path &optional root)
  "Construct relative path for PATH under optional ROOT."
  (cl-check-type path string)
  (let* ((ws-root (file-name-as-directory
                   (file-truename (expand-file-name (or root default-directory)))))
         (truename (file-truename (expand-file-name path ws-root))))
    (file-relative-name truename ws-root)))

(defun macher-agent-vfs-resolve-safe-path (unsafe-path root &optional canon-root)
  "Resolve UNSAFE-PATH safely within ROOT directory.
Optional CANON-ROOT supplies a precomputed canonical `file-truename'
directory string (with trailing slash) for ROOT."
  (cl-check-type unsafe-path string)
  (let* ((canonical-base (or canon-root
                             (file-name-as-directory (file-truename (expand-file-name root)))))
         (canonical-resolved (file-truename (expand-file-name unsafe-path canonical-base))))
    (if (or (string-prefix-p canonical-base (file-name-as-directory canonical-resolved))
            (string= (directory-file-name canonical-base) (directory-file-name canonical-resolved))
            (and (file-directory-p canonical-base) (file-in-directory-p canonical-resolved canonical-base)))
        canonical-resolved
      (error "SECURITY ERROR: Path traversal jailbreak detected: %s" unsafe-path))))

(defun macher-agent-vfs--file-stat (abs-path)
  "Return `(MTIME SIZE INODE)' for regular file ABS-PATH, or nil if absent."
  (when (stringp abs-path)
    (let ((attrs (file-attributes abs-path 'integer)))
      (when (and attrs (not (eq t (file-attribute-type attrs))))
        (list (file-attribute-modification-time attrs)
              (file-attribute-size attrs)
              (file-attribute-inode-number attrs))))))

(defun macher-agent-vfs--stat-unchanged-p (entry cur-stat)
  "Return non-nil if ENTRY's cached disk stat provably matches CUR-STAT.
Guards against coarse 1-second filesystems using a 2-second racy-stat check."
  (when (and (macher-agent-vfs-entry-p entry)
             cur-stat
             (macher-agent-vfs-entry-stat-mtime entry))
    (let ((cur-mtime (nth 0 cur-stat))
          (cur-size (nth 1 cur-stat))
          (cur-inode (nth 2 cur-stat)))
      (and (equal cur-mtime (macher-agent-vfs-entry-stat-mtime entry))
           (eql cur-size (macher-agent-vfs-entry-stat-size entry))
           (equal cur-inode (macher-agent-vfs-entry-stat-inode entry))
           (or (>= (or (cdr-safe cur-mtime) 0) 1000000)
               (>= (abs (float-time (time-subtract nil cur-mtime))) 2.0))))))

(defun macher-agent-vfs--stamp-entry-stat (entry stat)
  "Record disk STAT tuple `(MTIME SIZE INODE)' on ENTRY."
  (when (macher-agent-vfs-entry-p entry)
    (setf (macher-agent-vfs-entry-stat-mtime entry) (nth 0 stat)
          (macher-agent-vfs-entry-stat-size entry) (nth 1 stat)
          (macher-agent-vfs-entry-stat-inode entry) (nth 2 stat)))
  entry)

(defun macher-agent-vfs--read-canonical-content (path &optional is-buf)
  "Read canonical baseline string for PATH.
When IS-BUF is non-nil (or PATH names a live non-file virtual buffer),
read from that Emacs buffer.  Physical files are strictly read from disk
and return nil when absent on disk, never falling back to a file-visiting
buffer."
  (cl-check-type path string)
  (cl-assert (not (string-empty-p path)))
  (if is-buf
      (let ((buf (get-buffer path)))
        (when (and buf (buffer-live-p buf))
          (with-current-buffer buf
            (buffer-substring-no-properties (point-min) (point-max)))))
    (let* ((buf (get-buffer path))
           (virtual-buf (and buf (buffer-live-p buf) (not (buffer-file-name buf)))))
      (cond
       (virtual-buf
        (with-current-buffer buf
          (buffer-substring-no-properties (point-min) (point-max))))
       ((file-exists-p path)
        (let ((is-media (macher-agent-vfs-media-file-p path)))
          (ignore-errors
            (with-temp-buffer
              (if is-media
                  (progn
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally path))
                (insert-file-contents path))
              (buffer-substring-no-properties (point-min) (point-max))))))
       (t nil)))))

(defun macher-agent-vfs-read-content-from-disk-or-buffer (path)
  "Extract canonical content string for virtual buffer or physical file PATH."
  (macher-agent-vfs--read-canonical-content path))

(defun macher-agent-vfs-read-string (offset limit &optional text show-line-numbers)
  "Extract bounded slice from TEXT using OFFSET and LIMIT.
When SHOW-LINE-NUMBERS is non-nil, prefix each line with its number."
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

(defun macher-agent-vfs-edit-string-fast (old-text new-text &optional content replace-all)
  "Replace OLD-TEXT with NEW-TEXT in CONTENT.
When REPLACE-ALL is non-nil, replace every occurrence."
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
      (error "Multiple matches found for text.  Set replace_all to true or provide more context: %s" old-text))
     (t
      (replace-regexp-in-string (regexp-quote old-text) new-text content t t)))))

(defun macher-agent-vfs--build-rsync-cmd (src dest)
  "Construct a Git-driven rsync command from SRC to DEST.
Includes untracked files inside submodules."
  (let* ((src-dir (file-name-as-directory (expand-file-name src)))
         (dest-dir (file-name-as-directory (expand-file-name dest))))
    (unless (executable-find "git")
      (error "Macher-Agent: Git executable not found in PATH"))
    (format "cd %s && git rev-parse --is-inside-work-tree >/dev/null 2>&1 && { git -c core.quotePath=false ls-files -z -c --recurse-submodules; git -c core.quotePath=false ls-files -z -o --exclude-standard; git submodule foreach --quiet --recursive 'git -c core.quotePath=false ls-files -z -o --exclude-standard | perl -0ne \"print \\\"\\$ENV{displaypath}/\\$_\\\"\"'; } | perl -0ne 'print if -e $_' | rsync -a --delete --from0 --files-from=- %s %s"
            (shell-quote-argument src-dir)
            (shell-quote-argument src-dir)
            (shell-quote-argument dest-dir))))

(defun macher-agent-vfs--classify-file-path (path &optional root-dir context)
  "Categorise PATH into a VFS archetype relative to ROOT-DIR and CONTEXT."
  (cl-check-type path string)
  (let* ((buf (get-buffer path))
         (file-path (and buf (buffer-file-name buf)))
         (vfs-ht (when (and context (macher-agent-valid-context-p context))
                   (macher-agent-vfs-buffers context)))
         (in-vfs-bufs (and (hash-table-p vfs-ht)
                           (gethash path vfs-ht))))
    (if (or (and buf (null file-path))
            in-vfs-bufs)
        'buffer
      (let* ((target-path (or file-path path))
             (canonical-root (and root-dir (file-name-as-directory (expand-file-name root-dir))))
             (expanded (expand-file-name target-path (or root-dir default-directory))))
        (if (and canonical-root
                 (not (or (string-prefix-p canonical-root (file-name-as-directory expanded))
                          (string-prefix-p canonical-root expanded)
                          (string= (expand-file-name root-dir) expanded))))
            'external
          (if (macher-agent-vfs-media-file-p expanded)
              'media
            'file))))))

(defun macher-agent-vfs--invalidate-raw-files-cache (&optional root)
  "Invalidate cached workspace file list for ROOT, or all entries when nil."
  (when (hash-table-p macher-agent-vfs--raw-files-cache)
    (if root
        (remhash (directory-file-name (expand-file-name root))
                 macher-agent-vfs--raw-files-cache)
      (clrhash macher-agent-vfs--raw-files-cache))))

(defun macher-agent-vfs--collect-raw-files (expanded-dir home-dir)
  "Collect raw file list for EXPANDED-DIR, guarding against HOME-DIR.
Caches results briefly keyed by workspace root and `.git/index' mtime."
  (let ((norm-exp (directory-file-name (expand-file-name expanded-dir)))
        (norm-home (when home-dir (directory-file-name (expand-file-name home-dir)))))
    (when (or (and norm-home (string= norm-exp norm-home))
              (string= norm-exp "")
              (string= norm-exp "/")
              (and home-dir (string= expanded-dir home-dir))
              (string= expanded-dir "/"))
      (error "SECURITY HALT: Workspace resolved to root or home directory"))
    (let* ((idx-file (expand-file-name ".git/index" norm-exp))
           (idx-mtime (when (file-exists-p idx-file)
                        (file-attribute-modification-time (file-attributes idx-file 'integer))))
           (now (float-time))
           (cached (and (hash-table-p macher-agent-vfs--raw-files-cache)
                        (gethash norm-exp macher-agent-vfs--raw-files-cache))))
      (if (and cached
               (equal (nth 0 cached) idx-mtime)
               (< (- now (nth 1 cached)) macher-agent-vfs--raw-files-cache-ttl))
          (nth 2 cached)
        (let ((files
               (if-let* ((proj (project-current nil expanded-dir)))
                   (let ((project-vc-include-untracked t))
                     (project-files proj))
                 (directory-files-recursively
                  expanded-dir "^[^.]" nil
                  (lambda (d)
                    (let ((base (file-name-nondirectory (directory-file-name d))))
                      (and
                       (not (member base '(".git" "target" "node_modules" ".Trash" "Library" ".cache" ".config")))
                       (condition-case nil (progn (directory-files d) t) (error nil)))))))))
          (when (hash-table-p macher-agent-vfs--raw-files-cache)
            (puthash norm-exp (list idx-mtime now files) macher-agent-vfs--raw-files-cache))
          files)))))

(defun macher-agent-vfs--update-entry-content-cells (entry new-orig new-curr &optional pending-patch)
  "Update ENTRY content cells with NEW-ORIG, NEW-CURR, and PENDING-PATCH."
  (setf (macher-agent-vfs-entry-orig entry) new-orig)
  (setf (macher-agent-vfs-entry-curr entry) new-curr)
  (setf (macher-agent-vfs-entry-pending-patch entry) (and pending-patch t))
  entry)

(defun macher-agent-vfs--entry-in-frozen-layer-p (ctx entry)
  "Return non-nil if ENTRY lives in a frozen layer of CTX."
  (when (and ctx (macher-agent-valid-context-p ctx) (macher-agent-vfs-entry-p entry))
    (let* ((pair (macher-agent-vfs--layer-lookup-pair
                  ctx (macher-agent-vfs-entry-path entry)))
           (layer (cdr pair)))
      (and layer
           (eq (car pair) entry)
           (macher-agent-vfs-layer-frozen layer)))))

(defun macher-agent-vfs-sync-context-entry (entry root &optional context-or-mtime)
  "Synchronise ENTRY with live buffer or disk under ROOT.
Uses optional CONTEXT-OR-MTIME to resolve context.  Uses an O(1) disk
stat guard (`mtime', `size', `inode') to avoid redundant file reads
when unchanged, and CoW-isolates mutations when ENTRY belongs to a
frozen ancestor layer."
  (cl-check-type entry macher-agent-vfs-entry)
  (cl-check-type root string)
  (let* ((path (macher-agent-vfs-entry-path entry))
         (ctx (cond
               ((macher-agent-context-p context-or-mtime) context-or-mtime)
               ((bound-and-true-p macher-agent-persistent-context)
                macher-agent-persistent-context)
               ((macher-agent-context-p root) root)
               (t (or (macher-agent-resolve-context root)
                      (macher-agent-resolve-context (current-buffer))))))
         (vfs-ht (when (and ctx (macher-agent-valid-context-p ctx))
                   (macher-agent-vfs-buffers ctx)))
         (in-vfs-buffers-p
          (and (hash-table-p vfs-ht)
               (or (eq (gethash path vfs-ht) entry)
                   (cl-block macher-agent-vfs--found-buf-entry
                     (maphash (lambda (_k v)
                                (when (eq v entry)
                                  (cl-return-from macher-agent-vfs--found-buf-entry t)))
                              vfs-ht)
                     nil))))
         (is-buf (or in-vfs-buffers-p
                     (eq (macher-agent-vfs--classify-file-path path root ctx) 'buffer)))
         (abs-path (if (or is-buf (file-name-absolute-p path))
                       path
                     (expand-file-name path root)))
         (orig (macher-agent-vfs-entry-orig entry))
         (new (macher-agent-vfs-entry-curr entry))
         (disk-stat (unless is-buf (macher-agent-vfs--file-stat abs-path))))
    (if (and (not is-buf)
             orig
             disk-stat
             (macher-agent-vfs--stat-unchanged-p entry disk-stat))
        nil
      (if (and (not is-buf) (null orig) (null disk-stat))
          nil
        (let ((current-state (macher-agent-vfs--read-canonical-content
                              (if is-buf path abs-path) is-buf))
              (frozen-p (and (not is-buf)
                             (macher-agent-vfs--entry-in-frozen-layer-p ctx entry))))
          (cond
           ((and current-state (not (equal current-state orig)))
            (unless (or (equal current-state new)
                        (equal orig new))
              (message "Macher-Agent VFS: Replaced entry '%s' due to external edits."
                       (file-name-nondirectory path)))
            (if frozen-p
                (let ((fresh (macher-agent-vfs-create-entry
                              :path path
                              :orig current-state
                              :curr current-state
                              :pending-patch nil)))
                  (macher-agent-vfs--stamp-entry-stat fresh disk-stat)
                  (macher-agent-vfs--layer-put ctx path fresh))
              (macher-agent-vfs--update-entry-content-cells entry current-state current-state nil)
              (unless is-buf
                (macher-agent-vfs--stamp-entry-stat entry disk-stat))
              (when (and ctx (macher-agent-valid-context-p ctx) (not is-buf))
                (macher-agent-vfs--invalidate-contents-cache ctx)))
            (when (and is-buf (hash-table-p vfs-ht))
              (remhash path vfs-ht)
              (let ((stale-keys nil))
                (maphash (lambda (k v)
                           (when (eq v entry)
                             (push k stale-keys)))
                         vfs-ht)
                (dolist (k stale-keys)
                  (remhash k vfs-ht))))
            t)
           ((and is-buf (null current-state))
            (unless (or (null new)
                        (equal orig new))
              (message "Macher-Agent VFS: Replaced entry '%s' due to external edits."
                       (file-name-nondirectory path)))
            (macher-agent-vfs--update-entry-content-cells entry nil nil nil)
            (when (hash-table-p vfs-ht)
              (remhash path vfs-ht)
              (let ((stale-keys nil))
                (maphash (lambda (k v)
                           (when (eq v entry)
                             (push k stale-keys)))
                         vfs-ht)
                (dolist (k stale-keys)
                  (remhash k vfs-ht))))
            t)
           ((and (not is-buf) orig (null current-state))
            (unless (or (null new)
                        (equal orig new))
              (message "Macher-Agent VFS: Replaced entry '%s' due to external edits."
                       (file-name-nondirectory path)))
            (unless frozen-p
              (macher-agent-vfs--update-entry-content-cells entry nil nil nil)
              (macher-agent-vfs--stamp-entry-stat entry nil))
            (when (and ctx (macher-agent-valid-context-p ctx))
              (macher-agent-vfs--layer-remove ctx path))
            t)
           (t
            (when (and (not is-buf) orig disk-stat (not frozen-p))
              (macher-agent-vfs--stamp-entry-stat entry disk-stat))
            nil)))))))

(defun macher-agent-vfs--sync-and-check-dirty-entries (contents &optional root context)
  "Synchronise CONTENTS under ROOT using CONTEXT and check dirty state."
  (let ((synced nil)
        (is-dirty nil))
    (dolist (entry (copy-sequence contents))
      (when (macher-agent-vfs-sync-context-entry entry (or root default-directory) context)
        (setq synced t))
      (when (macher-agent-vfs-entry-modified-p entry)
        (setq is-dirty t)))
    (cons synced is-dirty)))

(defun macher-agent-vfs--strip-leading-dot-slash (path)
  "Strip leading `./' prefixes from relative PATH."
  (let ((res path))
    (while (string-prefix-p "./" res)
      (setq res (substring res 2)))
    res))

(defun macher-agent-vfs--strip-sandbox-prefix (path)
  "If `macher-agent-active-sandbox-dir' is bound and prefixes PATH, strip it."
  (if (and (stringp macher-agent-active-sandbox-dir)
           (not (string-empty-p macher-agent-active-sandbox-dir))
           (file-name-absolute-p path))
      (let* ((sbx-true (file-name-as-directory (file-truename macher-agent-active-sandbox-dir)))
             (sbx-exp (file-name-as-directory (expand-file-name macher-agent-active-sandbox-dir)))
             (p-exp (expand-file-name path))
             (p-true (file-truename p-exp)))
        (cond
         ((string-prefix-p sbx-true p-true)
          (let ((rel (substring p-true (length sbx-true))))
            (if (string-empty-p rel) "." rel)))
         ((string-prefix-p sbx-exp p-exp)
          (let ((rel (substring p-exp (length sbx-exp))))
            (if (string-empty-p rel) "." rel)))
         (t path)))
    path))

(defun macher-agent-vfs-normalize-path-key (path root &optional canon-root)
  "Format canonical workspace-relative key or buffer name from PATH and ROOT.
Optional CANON-ROOT supplies a precomputed canonical `file-truename'
directory string (with trailing slash) for ROOT."
  (cl-check-type path string)
  (let* ((ws-root (cond
                   ((stringp root) root)
                   ((macher-agent-context-p root)
                    (or (macher-agent-context-project-root root)
                        (macher-agent-context-workspace-root root)))
                   (t nil)))
         (clean-path (macher-agent-vfs--strip-sandbox-prefix path))
         (ctx-arg (when (macher-agent-context-p root) root)))
    (cond
     ((and ws-root (eq (macher-agent-vfs--classify-file-path clean-path ws-root ctx-arg) 'buffer))
      clean-path)
     (ws-root
      (let* ((canonical-base (or canon-root
                                 (file-name-as-directory (file-truename (expand-file-name ws-root)))))
             (safe-path (macher-agent-vfs-resolve-safe-path clean-path ws-root canonical-base)))
        (if (string= (directory-file-name safe-path) (directory-file-name canonical-base))
            "."
          (macher-agent-vfs--strip-leading-dot-slash
           (substring safe-path (length canonical-base))))))
     (t
      (macher-agent-vfs--strip-leading-dot-slash clean-path)))))

(defun macher-agent-vfs-ensure-access (context path)
  "Validate workspace boundary mapping PATH strictly against CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (cl-check-type path string)
  (let* ((actual-name (substring-no-properties path))
         (live-buf (get-buffer actual-name))
         (is-virtual-buf (and live-buf (not (buffer-file-name live-buf)))))
    (if is-virtual-buf
        (let ((scoped-ht (macher-agent-vfs-scoped-buffers context))
              (vfs-ht (macher-agent-vfs-buffers context)))
          (unless (or (and (hash-table-p scoped-ht) (gethash actual-name scoped-ht))
                      (and (hash-table-p vfs-ht) (gethash actual-name vfs-ht)))
            (error "SECURITY ERROR: Buffer '%s' is not in the agent's authorised scope" actual-name)))
      (let* ((contents (macher-agent-vfs-context-contents context))
             (norm-path (ignore-errors (macher-agent-vfs-normalize-path-key actual-name context))))
        (unless (and norm-path
                     (cl-find norm-path contents :key #'macher-agent-vfs-entry-path :test #'equal))
          (let ((workspace (or (macher-agent-context-workspace context)
                               (macher-agent-context-root context))))
            (macher-agent-macher-validate-path
             (or (and live-buf (buffer-file-name live-buf)) actual-name)
             workspace)))))))

;;;###autoload
(defun macher-agent-vfs-add-buffer-to-scope (buffer)
  "Add explicitly resolved BUFFER to active workspace buffer scope."
  (interactive "bAdd buffer to agent scope: ")
  (let* ((buf (if (stringp buffer) (get-buffer buffer) buffer))
         (ctx (or (macher-agent-resolve-context (current-buffer))
                  (macher-agent-resolve-context nil)
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (or (macher-agent-resolve-context buf)
                          (macher-agent-resolve-context nil))))
                  (cl-some (lambda (w)
                             (macher-agent-resolve-context (window-buffer w)))
                           (window-list))
                  (cl-some (lambda (b)
                             (when (and (buffer-local-value 'macher-agent-persistent-context b)
                                        (not (buffer-local-value 'macher-agent-is-ephemeral b)))
                               (buffer-local-value 'macher-agent-persistent-context b)))
                           (buffer-list)))))
    (cl-check-type buf buffer)
    (if (not (macher-agent-valid-context-p ctx))
        (user-error "No active Macher Agent context found")
      (let* ((buf-name (buffer-name buf))
             (scoped-ht (macher-agent-vfs-scoped-buffers ctx)))
        (when (hash-table-p scoped-ht)
          (puthash buf-name t scoped-ht))
        (message "SUCCESS: Added '%s' to the agent's authorised scope." buf-name)))))

(defun macher-agent-vfs--buffers-entries (context)
  "Return `macher-agent-vfs-entry' structs stored in CONTEXT `:vfs-buffers'."
  (let ((vfs-ht (macher-agent-vfs-buffers context))
        (entries nil))
    (when (hash-table-p vfs-ht)
      (maphash (lambda (_k v)
                 (when (and (macher-agent-vfs-entry-p v)
                            (not (memq v entries)))
                   (push v entries)))
               vfs-ht))
    (nreverse entries)))

(defun macher-agent-vfs--find-file-entry (norm-path contents context &optional raw-path)
  "Find VFS entry matching NORM-PATH or RAW-PATH in CONTENTS for CONTEXT.
Uses O(1) CoW hash lookup when CONTEXT is a valid `macher-agent-context'."
  (or (when (macher-agent-context-p context)
        (car (macher-agent-vfs--layer-lookup-pair context norm-path raw-path)))
      (and norm-path
           (cl-find norm-path contents :key #'macher-agent-vfs-entry-path :test #'equal))
      (and raw-path (not (equal raw-path norm-path))
           (cl-find raw-path contents :key #'macher-agent-vfs-entry-path :test #'equal))
      (when contents
        (let* ((ws-root (cond
                         ((stringp context) context)
                         ((macher-agent-context-p context)
                          (or (macher-agent-context-project-root context)
                              (macher-agent-context-workspace-root context)))
                         (t nil)))
               (canon-root (and (stringp ws-root)
                                (ignore-errors
                                  (file-name-as-directory
                                   (file-truename (expand-file-name ws-root)))))))
          (cl-find-if
           (lambda (e)
             (when-let* ((p (macher-agent-vfs-entry-path e)))
               (let ((p-norm (ignore-errors
                               (macher-agent-vfs-normalize-path-key p context canon-root))))
                 (if (and norm-path p-norm)
                     (equal p-norm norm-path)
                   (or (and raw-path (file-equal-p p raw-path))
                       (and norm-path (file-equal-p p norm-path)))))))
           contents)))))

(defun macher-agent-vfs--recompute-dirty-flag (context)
  "Recompute and set dirty flag on CONTEXT from owned file and buffer diff pools."
  (let ((has-dirty (or (cl-some #'macher-agent-vfs-entry-modified-p
                                (macher-agent-vfs--layer-owned-entries context))
                       (cl-some #'macher-agent-vfs-entry-modified-p
                                (macher-agent-vfs--buffers-entries context)))))
    (macher-agent-vfs-set-context-dirty-p context (and has-dirty t))))

(defun macher-agent-vfs-update-context-file (context path content)
  "Commit updated string CONTENT (or nil for deletion) against PATH in CONTEXT.
Reconciles any pre-existing entry against live disk or buffer state before
recording the edit, using O(1) Copy-on-Write layer storage."
  (cl-check-type context macher-agent-context)
  (cl-check-type path string)
  (when content (cl-check-type content string))
  (let* ((ws-root (macher-agent-context-project-root context))
         (sync-root (or ws-root default-directory))
         (is-buf (eq (macher-agent-vfs--classify-file-path path ws-root context) 'buffer))
         (canon-root (when (and (not is-buf) (stringp ws-root))
                       (file-name-as-directory (file-truename (expand-file-name ws-root)))))
         (norm-path (if is-buf path (macher-agent-vfs-normalize-path-key path context canon-root)))
         (disk-path (if (or is-buf (null ws-root))
                        norm-path
                      (macher-agent-vfs-resolve-safe-path norm-path ws-root canon-root)))
         (scoped-ht (macher-agent-vfs-scoped-buffers context))
         (vfs-ht (macher-agent-vfs-buffers context))
         (lookup-pair (unless is-buf
                        (macher-agent-vfs--layer-lookup-pair context norm-path path)))
         (entry (if is-buf
                    (when (hash-table-p vfs-ht)
                      (let ((v (or (gethash norm-path vfs-ht) (gethash path vfs-ht))))
                        (when (macher-agent-vfs-entry-p v) v)))
                  (car lookup-pair)))
         (entry-layer (cdr lookup-pair))
         (entry-frozen-p (and entry-layer (macher-agent-vfs-layer-frozen entry-layer))))
    (when entry
      (when (and (not is-buf) (not entry-frozen-p))
        (setf (macher-agent-vfs-entry-path entry) norm-path))
      (when (macher-agent-vfs-sync-context-entry entry sync-root context)
        (if is-buf
            (unless (and (hash-table-p vfs-ht)
                         (or (eq (gethash norm-path vfs-ht) entry)
                             (eq (gethash path vfs-ht) entry)))
              (setq entry nil))
          (let ((refetched (macher-agent-vfs--layer-lookup-pair context norm-path path)))
            (setq entry (car refetched)
                  entry-layer (cdr refetched)
                  entry-frozen-p (and entry-layer (macher-agent-vfs-layer-frozen entry-layer)))))))
    (let* ((fresh-stat (unless (or is-buf entry)
                         (macher-agent-vfs--file-stat disk-path)))
           (orig (if entry
                     (macher-agent-vfs-entry-orig entry)
                   (if is-buf
                       (or (macher-agent-vfs--read-canonical-content norm-path t)
                           (macher-agent-vfs--read-canonical-content path t)
                           content)
                     (when fresh-stat
                       (macher-agent-vfs--read-canonical-content disk-path nil)))))
           (prior (if entry (macher-agent-vfs-entry-curr entry) orig))
           (did-mutate (not (equal prior content))))
      (if is-buf
          (cond
           ((null content)
            (when (hash-table-p scoped-ht)
              (remhash norm-path scoped-ht)
              (unless (equal path norm-path) (remhash path scoped-ht)))
            (when (hash-table-p vfs-ht)
              (remhash norm-path vfs-ht)
              (unless (equal path norm-path) (remhash path vfs-ht)))
            (macher-agent-vfs--recompute-dirty-flag context))
           ((not did-mutate)
            (when (hash-table-p scoped-ht)
              (puthash norm-path t scoped-ht)
              (unless (equal path norm-path) (puthash path t scoped-ht)))
            (when (and entry (equal content orig))
              (when (hash-table-p vfs-ht)
                (remhash norm-path vfs-ht)
                (unless (equal path norm-path) (remhash path vfs-ht)))
              (macher-agent-vfs--recompute-dirty-flag context)))
           (t
            (when (hash-table-p scoped-ht)
              (puthash norm-path t scoped-ht)
              (unless (equal path norm-path) (puthash path t scoped-ht)))
            (if (equal content orig)
                (progn
                  (when (hash-table-p vfs-ht)
                    (remhash norm-path vfs-ht)
                    (unless (equal path norm-path) (remhash path vfs-ht)))
                  (macher-agent-vfs--recompute-dirty-flag context))
              (let ((updated-entry
                     (if entry
                         (progn
                           (setf (macher-agent-vfs-entry-curr entry) content)
                           (setf (macher-agent-vfs-entry-pending-patch entry) t)
                           entry)
                       (macher-agent-vfs-create-entry
                        :path norm-path
                        :orig orig
                        :curr content
                        :pending-patch t))))
                (when (hash-table-p vfs-ht)
                  (puthash norm-path updated-entry vfs-ht)
                  (unless (equal path norm-path)
                    (puthash path updated-entry vfs-ht))))
              (macher-agent-vfs-set-context-dirty-p context t))))
        ;; Physical file diff pool (O(1) CoW layer)
        (when (and (or (null orig) (null content)) ws-root)
          (macher-agent-vfs--invalidate-raw-files-cache ws-root))
        (cond
         ((null content)
          (if orig
              (progn
                (if (and entry (not entry-frozen-p))
                    (progn
                      (setf (macher-agent-vfs-entry-curr entry) nil)
                      (setf (macher-agent-vfs-entry-pending-patch entry) t)
                      (macher-agent-vfs--invalidate-contents-cache context))
                  (let ((new-entry (if entry
                                       (let ((c (macher-agent-vfs-copy-entry entry)))
                                         (setf (macher-agent-vfs-entry-path c) norm-path
                                               (macher-agent-vfs-entry-curr c) nil
                                               (macher-agent-vfs-entry-pending-patch c) t)
                                         c)
                                     (macher-agent-vfs--stamp-entry-stat
                                      (macher-agent-vfs-create-entry
                                       :path norm-path
                                       :orig orig
                                       :curr nil
                                       :pending-patch t)
                                      fresh-stat))))
                    (macher-agent-vfs--layer-put context norm-path new-entry)))
                (macher-agent-vfs-set-context-dirty-p context t))
            (when entry
              (macher-agent-vfs--layer-remove context norm-path))
            (macher-agent-vfs--recompute-dirty-flag context)))
         ((not did-mutate)
          (when (and entry (equal content orig) (or orig (not (file-exists-p disk-path))))
            (macher-agent-vfs--layer-remove context norm-path)
            (macher-agent-vfs--recompute-dirty-flag context)))
         (t
          (if (and (equal content orig) (or orig (not (file-exists-p disk-path))))
              (progn
                (when entry
                  (macher-agent-vfs--layer-remove context norm-path))
                (macher-agent-vfs--recompute-dirty-flag context))
            (if (and entry (not entry-frozen-p))
                (progn
                  (setf (macher-agent-vfs-entry-curr entry) content)
                  (setf (macher-agent-vfs-entry-pending-patch entry) t)
                  (macher-agent-vfs--invalidate-contents-cache context))
              (let ((new-entry (if entry
                                   (let ((c (macher-agent-vfs-copy-entry entry)))
                                     (setf (macher-agent-vfs-entry-path c) norm-path
                                           (macher-agent-vfs-entry-curr c) content
                                           (macher-agent-vfs-entry-pending-patch c) t)
                                     c)
                                 (macher-agent-vfs--stamp-entry-stat
                                  (macher-agent-vfs-create-entry
                                   :path norm-path
                                   :orig orig
                                   :curr content
                                   :pending-patch t)
                                  fresh-stat))))
                (macher-agent-vfs--layer-put context norm-path new-entry)))
            (macher-agent-vfs-set-context-dirty-p context t)))))

      (when did-mutate
        (let ((invoked-cbs nil)
              (keys (delete-dups (delq nil (list norm-path path disk-path)))))
          (dolist (key keys)
            (let ((vfs-cb (when (hash-table-p macher-agent-vfs-callbacks)
                            (gethash key macher-agent-vfs-callbacks)))
                  (pend-cb (when (hash-table-p macher-agent-pending-callbacks)
                             (gethash key macher-agent-pending-callbacks))))
              (when (hash-table-p macher-agent-vfs-callbacks)
                (remhash key macher-agent-vfs-callbacks))
              (when (hash-table-p macher-agent-pending-callbacks)
                (remhash key macher-agent-pending-callbacks))
              (dolist (cb (delq nil (list vfs-cb pend-cb)))
                (unless (memq cb invoked-cbs)
                  (push cb invoked-cbs)))))
          (when invoked-cbs
            (dolist (ht (list macher-agent-vfs-callbacks macher-agent-pending-callbacks))
              (when (hash-table-p ht)
                (let ((stale-keys nil))
                  (maphash (lambda (k v)
                             (when (memq v invoked-cbs)
                               (push k stale-keys)))
                           ht)
                  (dolist (k stale-keys)
                    (remhash k ht)))))
            (dolist (cb (nreverse invoked-cbs))
              (funcall cb (or content "")))))
        (run-hook-with-args 'macher-agent-context-mutated-hook norm-path context)))))

(defun macher-agent-vfs-read-context-file (context path)
  "Fetch VFS entry for PATH mapped to CONTEXT struct in O(1)."
  (cl-check-type context macher-agent-context)
  (cl-check-type path string)
  (let* ((workspace-root (macher-agent-context-project-root context))
         (sync-root (or workspace-root default-directory))
         (is-buf (eq (macher-agent-vfs--classify-file-path path workspace-root context) 'buffer))
         (canon-root (when (and (not is-buf) (stringp workspace-root))
                       (file-name-as-directory (file-truename (expand-file-name workspace-root)))))
         (norm-path (if is-buf path (macher-agent-vfs-normalize-path-key path context canon-root))))
    (if is-buf
        (let* ((vfs-ht (macher-agent-vfs-buffers context))
               (raw (and (hash-table-p vfs-ht)
                         (or (gethash norm-path vfs-ht) (gethash path vfs-ht))))
               (entry (when (macher-agent-vfs-entry-p raw) raw)))
          (when entry
            (when (macher-agent-vfs-sync-context-entry entry sync-root context)
              (macher-agent-vfs--recompute-dirty-flag context)
              (unless (and (hash-table-p vfs-ht)
                           (or (eq (gethash norm-path vfs-ht) entry)
                               (eq (gethash path vfs-ht) entry)))
                (setq entry nil))))
          (or (when entry (macher-agent-vfs-entry-curr entry))
              (when (stringp raw) raw)
              (macher-agent-vfs--read-canonical-content path t)
              (error "ERROR: Buffer '%s' does not exist" path)))
      (let* ((lookup (macher-agent-vfs--layer-lookup-pair context norm-path path))
             (entry (car lookup))
             (target-path (if workspace-root
                              (macher-agent-vfs-resolve-safe-path norm-path workspace-root canon-root)
                            norm-path)))
        (when entry
          (when (macher-agent-vfs-sync-context-entry entry sync-root context)
            (macher-agent-vfs--recompute-dirty-flag context)
            (setq entry (car (macher-agent-vfs--layer-lookup-pair context norm-path path)))))
        (cond
         ((and entry (macher-agent-vfs-entry-curr entry))
          (macher-agent-vfs-entry-curr entry))
         ((and entry (null (macher-agent-vfs-entry-curr entry)))
          (error "ERROR: File '%s' has been deleted in workspace" path))
         (t
          (or (macher-agent-vfs--read-canonical-content target-path nil)
              (error "ERROR: File/Buffer '%s' does not exist" path))))))))

(defun macher-agent-vfs--apply-overlay-stateless (contents ws-root sandbox-dir)
  "Apply virtual CONTENTS from WS-ROOT onto SANDBOX-DIR statelessly."
  (let ((sandbox-root (file-name-as-directory (expand-file-name sandbox-dir))))
    (mapc (lambda (entry)
            (when (macher-agent-vfs-entry-modified-p entry)
              (let* ((relative-path (macher-agent-vfs-to-relative-path
                                     (macher-agent-vfs-entry-path entry) ws-root))
                     (new-content (macher-agent-vfs-entry-curr entry))
                     (target-path (macher-agent-vfs-resolve-safe-path
                                   (expand-file-name relative-path sandbox-root) sandbox-root)))
                (if (stringp new-content)
                    (progn
                      (let ((dir (file-name-directory target-path)))
                        (when (and dir (not (file-directory-p dir))) (make-directory dir t)))
                      (if (not (multibyte-string-p new-content))
                          (let ((coding-system-for-write 'no-conversion))
                            (with-temp-file target-path
                              (set-buffer-multibyte nil)
                              (insert new-content)))
                        (with-temp-file target-path
                          (insert new-content))))
                  (when (file-exists-p target-path)
                    (delete-file target-path))))))
          contents)))

(defun macher-agent-vfs-active-p (context)
  "Query VFS status relying entirely on formatted CONTEXT struct."
  (cl-check-type context macher-agent-context)
  t)

(defun macher-agent-vfs-auto-sync-context (ctx &optional path)
  "Coordinate file states for optional PATH within active CTX struct."
  (cl-check-type ctx macher-agent-context)
  (unless macher-agent-vfs-pause-auto-sync
    (let* ((pairs (macher-agent-vfs--layer-all-pairs ctx))
           (tok (macher-agent-vfs--owner-token ctx))
           (buf-entries (macher-agent-vfs--buffers-entries ctx))
           (root (or (macher-agent-context-project-root ctx) default-directory))
           (synced nil)
           (is-dirty nil))
      (dolist (pair pairs)
        (let ((entry (car pair))
              (layer (cdr pair)))
          (when (macher-agent-vfs-sync-context-entry entry root ctx)
            (setq synced t))
          (when (and (eq (macher-agent-vfs-layer-owner layer) tok)
                     (macher-agent-vfs-entry-modified-p entry))
            (setq is-dirty t))))
      (let ((res-bufs (macher-agent-vfs--sync-and-check-dirty-entries buf-entries root ctx)))
        (when (car res-bufs) (setq synced t))
        (when (cdr res-bufs) (setq is-dirty t)))
      (unless is-dirty
        (macher-agent-vfs-set-context-dirty-p ctx nil))
      (when (and synced
                 (or (null path)
                     (macher-agent-managed-path-p path)))
        (run-hook-with-args 'macher-agent-context-mutated-hook path ctx)))))

(defun macher-agent-vfs--extract-context (payload)
  "Retrieve valid context mapping solely from transit PAYLOAD struct."
  (cl-check-type payload macher-agent-transit-payload)
  (macher-agent-context-from-payload payload))

(defun macher-agent-vfs-handle-flush (ctx)
  "Format response queue tied explicitly to CTX struct."
  (cl-check-type ctx macher-agent-context)
  (macher-agent-vfs-auto-sync-context ctx)
  (let* ((owner-buf (macher-agent-context-owner-buffer ctx))
         (suppress (or (bound-and-true-p macher-agent-suppress-patch)
                       (and (bufferp owner-buf)
                            (buffer-live-p owner-buf)
                            (buffer-local-value 'macher-agent-suppress-patch owner-buf))
                       (let ((plugins (macher-agent-context-plugins ctx)))
                         (when (macher-agent-plist-p plugins)
                           (plist-get plugins :suppress-patch)))))
         (has-changes (or (macher-agent-vfs-context-dirty-p ctx)
                          (macher-agent-vfs--context-has-changes-p ctx))))
    (when has-changes
      (when-let* ((prompt (macher-agent-context-prompt ctx)))
        (setf (macher-agent-context-prompt ctx) prompt))
      (unless suppress
        (run-hook-with-args 'macher-agent-vfs-flush-hook ctx)))))

(defmacro macher-agent-vfs-with-scope (context &rest body)
  "Execute BODY with Virtual File System awareness established from CONTEXT."
  (declare (indent 1) (debug t))
  (let ((raw-ctx-sym (gensym "raw-ctx-"))
        (ctx-sym (gensym "ctx-"))
        (root-sym (gensym "root-")))
    `(let* ((,raw-ctx-sym ,context)
            (,ctx-sym (macher-agent-resolve-context ,raw-ctx-sym)))
       (unless (and ,ctx-sym (macher-agent-valid-context-p ,ctx-sym))
         (error "Macher-agent-vfs-with-scope: Unable to resolve a valid VFS context from %S" ,raw-ctx-sym))
       (let* ((macher-agent-persistent-context ,ctx-sym)
              (,root-sym (macher-agent-context-project-root ,ctx-sym))
              (default-directory (if (and ,root-sym (stringp ,root-sym) (file-directory-p ,root-sym))
                                     (file-name-as-directory ,root-sym)
                                   default-directory)))
         ,@body))))

;;; Patch Generation & Split Review

(defun macher-agent-vfs--expressive-patch-buffer-name (context patch-type &optional orig-buf)
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

(defun macher-agent-vfs--build-and-rename-patch (ctx patch-type &optional files)
  "Build and rename a PATCH-TYPE patch buffer for dirty CTX.
Optionally restricts diffing to FILES."
  (cl-check-type ctx macher-agent-context)
  (let* ((type-sym (if (symbolp patch-type) patch-type (intern (format "%s" patch-type))))
         (target-buf (or (macher-agent-context-owner-buffer ctx)
                         (macher-agent-vfs--get-origin-buffer ctx)
                         (current-buffer)))
         (suppress-patch (if (and (bufferp target-buf) (buffer-live-p target-buf))
                             (buffer-local-value 'macher-agent-suppress-patch target-buf)
                           (bound-and-true-p macher-agent-suppress-patch))))
    (when (and (not suppress-patch)
               (macher-agent-vfs--context-has-changes-p ctx))
      (let* ((expressive-name (macher-agent-vfs--expressive-patch-buffer-name ctx type-sym target-buf))
             (entries (macher-agent-vfs--gather-entries ctx files))
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

(defun macher-agent-vfs--display-patch-buffers (generated-buffers)
  "Display GENERATED-BUFFERS in split windows if multiple buffers exist."
  (when (> (length generated-buffers) 1)
    (let ((bufs (nreverse generated-buffers)))
      (delete-other-windows)
      (split-window-vertically)
      (set-window-buffer (selected-window) (car bufs))
      (set-window-buffer (next-window) (cadr bufs)))))

(defun macher-agent-vfs--context-has-changes-p (context)
  "Determine whether CONTEXT contains actual VFS modifications."
  (and context
       (or (cl-some #'macher-agent-vfs-entry-modified-p
                    (macher-agent-vfs--layer-owned-entries context))
           (cl-some #'macher-agent-vfs-entry-modified-p
                    (macher-agent-vfs--buffers-entries context)))))

(defun macher-agent-vfs--split-context (ctx)
  "Distinguish mapped buffer state strictly bound by CTX struct."
  (cl-check-type ctx macher-agent-context)
  (let* ((file-ctx (macher-agent-clone-context ctx))
         (buf-ctx (macher-agent-clone-context ctx))
         (file-contents (cl-remove-if-not #'macher-agent-vfs-entry-modified-p
                                          (macher-agent-vfs-context-contents ctx)))
         (buf-contents (cl-remove-if-not #'macher-agent-vfs-entry-modified-p
                                         (macher-agent-vfs--buffers-entries ctx)))
         (file-vfs-ht (macher-agent-vfs-buffers file-ctx)))
    (when (hash-table-p file-vfs-ht)
      (clrhash file-vfs-ht))
    (macher-agent-vfs-set-context-contents file-ctx file-contents)
    (macher-agent-vfs-set-context-contents buf-ctx buf-contents)
    (cons file-ctx buf-ctx)))

(defun macher-agent-vfs--execute-split-patch (ctx)
  "Generate diff payload dependent strictly on dirty CTX struct."
  (cl-check-type ctx macher-agent-context)
  (when (or (macher-agent-vfs-context-dirty-p ctx)
            (macher-agent-vfs--context-has-changes-p ctx))
    (let* ((payloads (macher-agent-vfs--split-context ctx))
           (p-ctx (car payloads))
           (v-ctx (cdr payloads))
           (p-contents (macher-agent-vfs-context-contents p-ctx))
           (v-contents (macher-agent-vfs-context-contents v-ctx))
           (generated nil))
      (when (and p-contents (> (length p-contents) 0))
        (let ((p-buf (macher-agent-vfs--build-and-rename-patch p-ctx 'diff p-contents)))
          (when p-buf (push p-buf generated))))
      (when (and v-contents (> (length v-contents) 0))
        (let ((v-buf (macher-agent-vfs--build-and-rename-patch v-ctx 'buffer v-contents)))
          (when v-buf (push v-buf generated))))
      (macher-agent-vfs--display-patch-buffers (nreverse generated)))))

(defun macher-agent-vfs-build-patch-from-hook (ctx)
  "Format background UI changes via target CTX struct."
  (cl-check-type ctx macher-agent-context)
  (let ((prompt (or (macher-agent-context-prompt ctx) "")))
    (when prompt
      (setf (macher-agent-context-prompt ctx) prompt)
      (setf (macher-agent-context-plugins ctx)
            (plist-put (copy-sequence (macher-agent-context-plugins ctx)) :prompt prompt)))
    (macher-agent-vfs--execute-split-patch ctx)))

(defun macher-agent-vfs-diff-review (ctx)
  "Format interactive diff utilising active CTX struct."
  (interactive (list (macher-agent-resolve-context (current-buffer))))
  (cl-check-type ctx macher-agent-context)
  (macher-agent-vfs-handle-flush ctx))

(defun macher-agent-vfs--gather-entries (ctx &optional files)
  "Gather VFS entries from CTX, optionally restricted to FILES."
  (cl-check-type ctx macher-agent-context)
  (or files
      (macher-agent-vfs-context-contents ctx)
      (macher-agent-vfs-modified-files ctx)))

;;; Workspace Storage Callbacks & Context Cloning

(defun macher-agent-vfs--fast-rel-key (path ws-root canon-root)
  "Return canonical relative key for PATH under WS-ROOT or CANON-ROOT.
Avoids `file-truename' when PATH is already a clean relative or prefixed path."
  (cond
   ((not (stringp path)) nil)
   ((and (not (file-name-absolute-p path))
         (not (string-prefix-p "./" path))
         (not (string-match-p "\\.\\." path)))
    path)
   ((and canon-root (string-prefix-p canon-root path)
         (not (string-match-p "\\.\\." path)))
    (macher-agent-vfs--strip-leading-dot-slash (substring path (length canon-root))))
   ((and ws-root
         (let ((exp-root (file-name-as-directory (expand-file-name ws-root))))
           (and (string-prefix-p exp-root path)
                (not (string-match-p "\\.\\." path)))))
    (let ((exp-root (file-name-as-directory (expand-file-name ws-root))))
      (macher-agent-vfs--strip-leading-dot-slash (substring path (length exp-root)))))
   (t nil)))

(defun macher-agent-vfs-workspace-read-file (context path &optional only-staged)
  "Read PATH from CONTEXT VFS or disk without redundant `file-truename' calls.
When ONLY-STAGED is non-nil, only return content from staged VFS entries."
  (when (and (macher-agent-valid-context-p context) (stringp path))
    (let* ((ws-root (or (macher-agent-context-project-root context)
                        (macher-agent-context-workspace-root context)))
           (sync-root (or ws-root default-directory))
           (canon-root (macher-agent-vfs--canonical-root ws-root))
           (fast-rel (macher-agent-vfs--fast-rel-key path ws-root canon-root))
           (norm-path (or fast-rel
                          (ignore-errors (macher-agent-vfs-normalize-path-key path context canon-root))))
           (vfs-buffers (macher-agent-vfs-buffers context))
           (file-entry (car (macher-agent-vfs--layer-lookup-pair context norm-path path)))
           (buf-val (and (hash-table-p vfs-buffers)
                         (> (hash-table-count vfs-buffers) 0)
                         (or (and norm-path (gethash norm-path vfs-buffers))
                             (gethash path vfs-buffers)))))
      (cond
       (file-entry
        (when (macher-agent-vfs-sync-context-entry file-entry sync-root context)
          (macher-agent-vfs--recompute-dirty-flag context)
          (setq file-entry (car (macher-agent-vfs--layer-lookup-pair context norm-path path))))
        (when file-entry
          (macher-agent-vfs-entry-curr file-entry)))
       ((macher-agent-vfs-entry-p buf-val)
        (when (macher-agent-vfs-sync-context-entry buf-val sync-root context)
          (macher-agent-vfs--recompute-dirty-flag context)
          (unless (and (hash-table-p vfs-buffers)
                       (or (and norm-path (eq (gethash norm-path vfs-buffers) buf-val))
                           (eq (gethash path vfs-buffers) buf-val)))
            (setq buf-val nil)))
        (when (macher-agent-vfs-entry-p buf-val)
          (macher-agent-vfs-entry-curr buf-val)))
       ((stringp buf-val) buf-val)
       (only-staged nil)
       (t
        (let ((candidate (if (and ws-root (not (file-name-absolute-p path)))
                             (expand-file-name path ws-root)
                           path)))
          (when (and (stringp candidate)
                     (file-exists-p candidate)
                     (not (file-directory-p candidate)))
            (let ((safe (if ws-root
                            (ignore-errors
                              (macher-agent-vfs-resolve-safe-path path ws-root canon-root))
                          candidate)))
              (when safe
                (macher-agent-vfs--read-canonical-content safe nil))))))))))

(defun macher-agent-vfs-workspace-list-files (context)
  "Return non-deleted workspace-relative paths tracked in CONTEXT VFS."
  (when (macher-agent-valid-context-p context)
    (let* ((ws-root (or (macher-agent-context-project-root context)
                        (macher-agent-context-workspace-root context)))
           (canon-root (macher-agent-vfs--canonical-root ws-root))
           (res nil))
      (dolist (pair (macher-agent-vfs--layer-all-pairs context))
        (let* ((entry (car pair))
               (curr (macher-agent-vfs-entry-curr entry))
               (raw-path (and curr (macher-agent-vfs-entry-path entry))))
          (when (stringp raw-path)
            (let ((rel (or (macher-agent-vfs--fast-rel-key raw-path ws-root canon-root)
                           (ignore-errors
                             (macher-agent-vfs-normalize-path-key raw-path context canon-root)))))
              (when (stringp rel)
                (push rel res))))))
      res)))

(defun macher-agent-vfs--clone-context-step (new-ctx)
  "Fork VFS state in NEW-CTX in O(1) using an immutable CoW parent layer."
  (when (macher-agent-context-p new-ctx)
    (let* ((parent-layer (macher-agent-vfs--ensure-layer new-ctx))
           (_ (setf (macher-agent-vfs-layer-frozen parent-layer) t))
           (new-tok (list 'vfs-owner))
           (child-layer (macher-agent-vfs--make-layer
                         :parent parent-layer
                         :entries (make-hash-table :test #'equal)
                         :tombstones (make-hash-table :test #'equal)
                         :owner new-tok
                         :frozen nil))
           (new-state (copy-sequence (macher-agent-vfs--get-state new-ctx)))
           (vfs-ht (macher-agent-vfs-buffers new-ctx)))
      (setq new-state (plist-put new-state :owner-token new-tok))
      (setq new-state (plist-put new-state :layer child-layer))
      (setq new-state (plist-put new-state :contents nil))
      (setq new-state (plist-put new-state :contents-cache nil))
      (setq new-state (plist-put new-state :dirty nil))
      (setq new-state (plist-put new-state :dirty-p nil))
      (macher-agent-vfs--set-state new-ctx new-state)
      (when (and (hash-table-p vfs-ht) (> (hash-table-count vfs-ht) 0))
        (maphash (lambda (k v)
                   (when (macher-agent-vfs-entry-p v)
                     (let ((copy (macher-agent-vfs-copy-entry v)))
                       (setf (macher-agent-vfs-entry-pending-patch copy) nil)
                       (puthash k copy vfs-ht))))
                 vfs-ht))))
  new-ctx)

;;; Upstream Macher Tool Wrapping & Search Replacement

(defun macher-agent-vfs--extract-tool-name (tool)
  "Extract canonical name string from TOOL."
  (let ((actual-tool (if (consp tool) (cdr tool) tool)))
    (cond
     ((and (fboundp 'gptel-tool-p) (gptel-tool-p actual-tool))
      (gptel-tool-name actual-tool))
     ((stringp tool) tool)
     ((symbolp tool) (symbol-name tool))
     (t (error "Invalid tool representation: %S" tool)))))

(defun macher-agent-vfs-get-search-in-workspace-tool ()
  "Return the tool struct defined in `skills/scripts/search_in_workspace.el'."
  (let ((tool (or (and (boundp 'macher-agent-search-in-workspace-tool)
                       (macher-agent-tool-valid-p (symbol-value 'macher-agent-search-in-workspace-tool))
                       (symbol-value 'macher-agent-search-in-workspace-tool))
                  (and (bound-and-true-p macher-agent-tools-registry)
                       (let ((reg-tl (gethash "search_in_workspace" macher-agent-tools-registry)))
                         (and (macher-agent-tool-valid-p reg-tl)
                              (not (member (gptel-tool-category reg-tl)
                                           '("macher" "macher__processed")))
                              reg-tl))))))
    (unless (macher-agent-tool-valid-p tool)
      (let* ((vfs-skills-dir (macher-agent-vfs-skills-directory))
             (bundled-dir (macher-agent-resolve-bundled-skills-dir))
             (script-candidates
              (delq nil
                    (list
                     (when vfs-skills-dir
                       (expand-file-name "scripts/search_in_workspace.el" vfs-skills-dir))
                     (when bundled-dir
                       (expand-file-name "scripts/search_in_workspace.el" bundled-dir))
                     (locate-file "skills/scripts/search_in_workspace.el" load-path)
                     (locate-file "search_in_workspace.el" load-path)
                     (expand-file-name "skills/scripts/search_in_workspace.el" default-directory)
                     (let ((root (locate-dominating-file default-directory "skills")))
                       (when root
                         (expand-file-name "skills/scripts/search_in_workspace.el" root))))))
             (found (cl-find-if #'file-exists-p script-candidates)))
        (when found
          (macher-agent-load-script-isolated found))
        (setq tool (and (boundp 'macher-agent-search-in-workspace-tool)
                        (symbol-value 'macher-agent-search-in-workspace-tool)))))
    (or (and (macher-agent-tool-valid-p tool) tool)
        (when (fboundp 'gptel-get-tool)
          (let ((perc (ignore-errors (gptel-get-tool '("perception" "search_in_workspace")))))
            (and (macher-agent-tool-valid-p perc) perc))))))

(defun macher-agent-vfs--search-tool-names ()
  "Return candidate names for the Macher workspace search tool."
  (delete-dups
   (list (macher-agent-macher-search-tool-name)
         "search_in_workspace"
         "macher_search")))

(defun macher-agent-vfs--search-tool-name-p (name)
  "Return non-nil when NAME identifies a Macher workspace search tool."
  (and name
       (member (if (stringp name) name (format "%s" name))
               (macher-agent-vfs--search-tool-names))
       t))

(defun macher-agent-vfs--named-tool-copy (tool target-name &optional force-copy)
  "Return TOOL or a shallow copy renamed to TARGET-NAME.
When FORCE-COPY is non-nil, always allocate a distinct struct copy.
Preserves wrapped status and workspace PTC override mappings."
  (if (or (not (macher-agent-tool-valid-p tool))
          (and (not force-copy)
               (or (not (stringp target-name))
                   (string-empty-p target-name)
                   (equal (gptel-tool-name tool) target-name))))
      tool
    (let ((copy (macher-agent-copy-tool tool)))
      (when (and (stringp target-name) (not (string-empty-p target-name)))
        (setf (gptel-tool-name copy) target-name))
      (puthash copy (list :injected t) macher-agent-vfs--wrapped-tools-hash)
      (when (and (boundp 'macher-agent-sandbox-workspace-tool-overrides)
                 (hash-table-p macher-agent-sandbox-workspace-tool-overrides))
        (when-let* ((ws-fn (gethash tool macher-agent-sandbox-workspace-tool-overrides)))
          (puthash copy ws-fn macher-agent-sandbox-workspace-tool-overrides)))
      copy)))

(defun macher-agent-vfs--snapshot-tool-slots (tool)
  "Capture original mutable slots of TOOL for clean unwrapping."
  (list :function (gptel-tool-function tool)
        :description (gptel-tool-description tool)
        :args (gptel-tool-args tool)
        :category (gptel-tool-category tool)
        :async (gptel-tool-async tool)
        :confirm (gptel-tool-confirm tool)
        :include (gptel-tool-include tool)))

(defconst macher-agent-vfs--single-path-tool-names
  '("edit_file_in_workspace" "write_file_in_workspace" "delete_file_in_workspace"
    "multi_edit_file_in_workspace" "insert_in_file_in_workspace"
    "replace_in_file_in_workspace")
  "Upstream Macher file-mutation tools operating on a single relative path.")

(defun macher-agent-vfs--format-read-content (content offset limit show-line-numbers)
  "Slice and format CONTENT using optional OFFSET, LIMIT, and SHOW-LINE-NUMBERS.
Enforces `macher-tool-output-max-length' when bound."
  (let* ((processed
          (if (and (null offset) (null limit) (null show-line-numbers))
              content
            (let* ((lines (split-string content "\n"))
                   (num-lines (length lines))
                   (parsed-offset (when offset (round offset)))
                   (parsed-limit (when limit (round limit)))
                   (start-idx
                    (if parsed-offset
                        (if (< parsed-offset 0)
                            (max 0 (+ num-lines parsed-offset))
                          (max 0 (min (1- (max 1 parsed-offset)) num-lines)))
                      0))
                   (actual-limit
                    (when parsed-limit
                      (if (< parsed-limit 0)
                          (max 0 (+ num-lines parsed-limit))
                        parsed-limit)))
                   (sliced (if actual-limit
                               (seq-take (nthcdr start-idx lines) actual-limit)
                             (nthcdr start-idx lines))))
              (if show-line-numbers
                  (string-join
                   (cl-loop for line in sliced
                            for n from (1+ start-idx)
                            collect (format "%6d\t%s" n line))
                   "\n")
                (string-join sliced "\n")))))
         (max-len (and (boundp 'macher-tool-output-max-length)
                       (integerp macher-tool-output-max-length)
                       macher-tool-output-max-length)))
    (when (and max-len (> (length processed) max-len))
      (let* ((len (length processed))
             (preview (min 256 (/ len 2))))
        (error "File content too large: %d characters exceeds maximum tool output length of %d characters\nFirst %d characters:\n%s\n...\nLast %d characters:\n%s"
               len max-len preview (substring processed 0 preview)
               preview (substring processed (- len preview)))))
    processed))

(defun macher-agent-vfs--fast-read-file (agent-ctx ws-root canon-root path &optional offset limit show-line-numbers)
  "Read PATH directly from AGENT-CTX's CoW VFS or disk in O(1).
Uses WS-ROOT and CANON-ROOT for containment validation without spawning
`git ls-files' or marshalling unrelated VFS entries.  Honours optional
OFFSET, LIMIT, and SHOW-LINE-NUMBERS."
  (cl-check-type path string)
  (let* ((exp-root (file-name-as-directory (expand-file-name ws-root)))
         (canon-base (or canon-root (file-name-as-directory (file-truename exp-root))))
         (expanded (expand-file-name path exp-root))
         (symlink-target (file-symlink-p expanded)))
    (if symlink-target
        (progn
          (macher-agent-vfs-resolve-safe-path
           (file-name-directory (directory-file-name expanded)) exp-root canon-base)
          (format "Symlink target: %s" symlink-target))
      (let* ((safe-path (macher-agent-vfs-resolve-safe-path path exp-root canon-base))
             (norm-rel (if (string= (directory-file-name safe-path) (directory-file-name canon-base))
                           "."
                         (macher-agent-vfs--strip-leading-dot-slash
                          (substring safe-path (length canon-base)))))
             (lookup (macher-agent-vfs--layer-lookup-pair agent-ctx norm-rel path))
             (entry (car lookup)))
        (when entry
          (when (macher-agent-vfs-sync-context-entry entry exp-root agent-ctx)
            (macher-agent-vfs--recompute-dirty-flag agent-ctx)
            (setq entry (car (macher-agent-vfs--layer-lookup-pair agent-ctx norm-rel path)))))
        (let ((content
               (cond
                ((and entry (macher-agent-vfs-entry-curr entry))
                 (macher-agent-vfs-entry-curr entry))
                ((and entry (null (macher-agent-vfs-entry-curr entry)))
                 (error "File '%s' not found in workspace" path))
                ((and (file-exists-p safe-path) (not (file-directory-p safe-path)))
                 (macher-agent-vfs--read-canonical-content safe-path nil))
                (t
                 (error "File '%s' not found in workspace" path)))))
          (macher-agent-vfs--format-read-content content offset limit show-line-numbers))))))

(defun macher-agent-vfs-wrap-single-tool (tool)
  "Wrap single TOOL to intercept upstream macher context and route to VFS."
  (when-let* (((not (gethash tool macher-agent-vfs--wrapped-tools-hash)))
              (tool-name (macher-agent-vfs--extract-tool-name tool)))
    (if-let* (((macher-agent-vfs--search-tool-name-p tool-name))
              ((member (format "%s" (gptel-tool-category tool))
                       '("macher" "macher__processed")))
              (replacement (macher-agent-vfs-get-search-in-workspace-tool)))
        (progn
          (puthash tool (macher-agent-vfs--snapshot-tool-slots tool)
                   macher-agent-vfs--wrapped-tools-hash)
          (unless (eq tool replacement)
            (setf (gptel-tool-function tool) (gptel-tool-function replacement)
                  (gptel-tool-description tool) (gptel-tool-description replacement)
                  (gptel-tool-args tool) (gptel-tool-args replacement)
                  (gptel-tool-category tool) (gptel-tool-category replacement)
                  (gptel-tool-async tool) (gptel-tool-async replacement)
                  (gptel-tool-confirm tool) (gptel-tool-confirm replacement)
                  (gptel-tool-include tool) (gptel-tool-include replacement))))
      (when-let* ((category (gptel-tool-category tool))
                  ((string= (format "%s" category) "macher"))
                  (orig-fn (gptel-tool-function tool)))
        (let ((is-read-tool (string= tool-name "read_file_in_workspace"))
              (is-single-path-tool (member tool-name macher-agent-vfs--single-path-tool-names))
              (is-async (gptel-tool-async tool)))
          (puthash tool (macher-agent-vfs--snapshot-tool-slots tool)
                   macher-agent-vfs--wrapped-tools-hash)
          (setf (gptel-tool-function tool)
                (lambda (&rest raw-args)
                  (let* ((has-upstream-ctx (and raw-args
                                                (fboundp 'macher-context-p)
                                                (macher-context-p (car raw-args))))
                         (orphaned-context (when has-upstream-ctx (car raw-args)))
                         (callback (if has-upstream-ctx (cadr raw-args) (car raw-args)))
                         (args (if has-upstream-ctx (cddr raw-args) (cdr raw-args)))
                         (fsm (macher-agent-get-active-fsm))
                         (target-buf (or (when (and fsm (fboundp 'gptel-fsm-info))
                                           (ignore-errors (plist-get (gptel-fsm-info fsm) :buffer)))
                                         (current-buffer)))
                         (agent-ctx (or (when fsm
                                          (macher-agent-gptel-context-from-fsm fsm target-buf))
                                        (macher-agent-resolve-context target-buf))))
                    (unless agent-ctx
                      (error "Macher Agent: Could not resolve master agent context from buffer %s" target-buf))
                    (let* ((ws (or (macher-agent-context-workspace agent-ctx)
                                   (when-let* ((pr (or (macher-agent-context-project-root agent-ctx)
                                                       (macher-agent-context-workspace-root agent-ctx))))
                                     (cons 'project (expand-file-name pr)))))
                           (ws-root (or (and (consp ws) (stringp (cdr ws)) (cdr ws))
                                        (macher-agent-context-project-root agent-ctx)
                                        (macher-agent-context-workspace-root agent-ctx)
                                        default-directory))
                           (ctx-root (macher-agent-context-project-root agent-ctx))
                           (norm-root (or ctx-root
                                          (macher-agent-context-workspace-root agent-ctx)))
                           (sync-root (or norm-root ws-root default-directory))
                           (canon-root (and (stringp norm-root)
                                            (ignore-errors
                                              (file-name-as-directory
                                               (file-truename (expand-file-name norm-root))))))
                           (ws-canon (and (stringp ws-root)
                                          (if (and norm-root (equal ws-root norm-root))
                                              canon-root
                                            (ignore-errors
                                              (file-name-as-directory
                                               (file-truename (expand-file-name ws-root)))))))
                           (same-root-p (or (null canon-root) (equal ws-canon canon-root))))
                      (if is-read-tool
                          (let* ((tool-args (if is-async args (cons callback args)))
                                 (res (apply #'macher-agent-vfs--fast-read-file
                                             agent-ctx ws-root canon-root tool-args)))
                            (when (and is-async (functionp callback))
                              (funcall callback res))
                            res)
                        (let* ((target-arg (when is-single-path-tool
                                             (if is-async (car args) callback)))
                               (scoped-rel (when (and (stringp target-arg) same-root-p)
                                             (ignore-errors
                                               (macher-agent-vfs-normalize-path-key
                                                target-arg agent-ctx canon-root))))
                               (before-contents
                                (if scoped-rel
                                    (let ((entry (car (macher-agent-vfs--layer-lookup-pair
                                                       agent-ctx scoped-rel target-arg))))
                                      (when entry
                                        (when (macher-agent-vfs-sync-context-entry entry sync-root agent-ctx)
                                          (macher-agent-vfs--recompute-dirty-flag agent-ctx)
                                          (setq entry (car (macher-agent-vfs--layer-lookup-pair
                                                            agent-ctx scoped-rel target-arg)))))
                                      (when entry (list entry)))
                                  (let ((any-synced nil))
                                    (dolist (pair (macher-agent-vfs--layer-all-pairs agent-ctx))
                                      (when (macher-agent-vfs-sync-context-entry (car pair) sync-root agent-ctx)
                                        (setq any-synced t)))
                                    (when any-synced
                                      (macher-agent-vfs--recompute-dirty-flag agent-ctx)))
                                  (copy-sequence (macher-agent-vfs-context-contents agent-ctx))))
                               (before-by-rel (make-hash-table :test #'equal
                                                               :size (max 1 (length before-contents))))
                               (marshalled-rel-by-abs (make-hash-table :test #'equal
                                                                       :size (max 1 (length before-contents))))
                               (before-records nil)
                               (marshalled-files
                                (mapcar (lambda (entry)
                                          (let* ((raw-p (macher-agent-vfs-entry-path entry))
                                                 (norm-rel (if scoped-rel
                                                               scoped-rel
                                                             (and (stringp raw-p)
                                                                  (ignore-errors
                                                                    (macher-agent-vfs-normalize-path-key
                                                                     raw-p agent-ctx canon-root)))))
                                                 (abs-p (if (and (stringp raw-p) ws-root)
                                                            (expand-file-name raw-p ws-root)
                                                          raw-p)))
                                            (when norm-rel
                                              (unless (gethash norm-rel before-by-rel)
                                                (puthash norm-rel entry before-by-rel))
                                              (when (and same-root-p (stringp abs-p))
                                                (puthash abs-p norm-rel marshalled-rel-by-abs)))
                                            (push (cons entry (or norm-rel raw-p)) before-records)
                                            (cons abs-p
                                                  (cons (macher-agent-vfs-entry-orig entry)
                                                        (macher-agent-vfs-entry-curr entry)))))
                                        before-contents))
                               (_ (setq before-records (nreverse before-records)))
                               (proxy-outcome
                                (macher-agent-macher-run-with-proxy-context
                                 orphaned-context ws marshalled-files
                                 (macher-agent-context-prompt agent-ctx)
                                 target-buf orig-fn callback args))
                               (res (car-safe proxy-outcome))
                               (raw-after-files (cdr-safe proxy-outcome)))
                          (when (and (consp proxy-outcome) (listp raw-after-files))
                            (let ((seen-paths (make-hash-table :test #'equal
                                                                :size (max 1 (length raw-after-files)))))
                              (dolist (item raw-after-files)
                                (let* ((raw-path (cond
                                                  ((macher-agent-vfs-entry-p item) (macher-agent-vfs-entry-path item))
                                                  ((consp item) (car item))
                                                  (t nil)))
                                       (orig (cond
                                              ((macher-agent-vfs-entry-p item) (macher-agent-vfs-entry-orig item))
                                              ((and (consp item) (consp (cdr item))) (cadr item))
                                              (t nil)))
                                       (new (cond
                                             ((macher-agent-vfs-entry-p item) (macher-agent-vfs-entry-curr item))
                                             ((and (consp item) (consp (cdr item))) (cddr item))
                                             ((consp item) (cdr item))
                                             (t nil)))
                                       (rel-path (and (stringp raw-path)
                                                      (or (gethash raw-path marshalled-rel-by-abs)
                                                          (ignore-errors
                                                            (macher-agent-vfs-normalize-path-key
                                                             raw-path agent-ctx canon-root)))))
                                       (prev (and rel-path (gethash rel-path before-by-rel))))
                                  (when rel-path
                                    (puthash rel-path t seen-paths)
                                    (when (if prev
                                              (not (equal (macher-agent-vfs-entry-curr prev) new))
                                            (not (equal orig new)))
                                      (macher-agent-vfs-update-context-file agent-ctx rel-path new)))))
                              (dolist (rec before-records)
                                (let ((prev (car rec))
                                      (prev-rel-path (cdr rec)))
                                  (unless (and (stringp prev-rel-path)
                                               (gethash prev-rel-path seen-paths))
                                    (let ((kind (and (stringp prev-rel-path)
                                                     (macher-agent-vfs--classify-file-path prev-rel-path ctx-root))))
                                      (if (and (stringp prev-rel-path) (memq kind '(file media)))
                                          (macher-agent-vfs-update-context-file
                                           agent-ctx prev-rel-path (macher-agent-vfs-entry-orig prev))
                                        (macher-agent-vfs--layer-remove-entry agent-ctx prev)
                                        (macher-agent-vfs--recompute-dirty-flag agent-ctx))))))))
                          res)))))))))))

(defun macher-agent-vfs--unwrap-all-tools ()
  "Restore original slots on all tools wrapped by `macher-agent-vfs'.
Removes any `:injected' search tools added to known Macher tool alists."
  (when (hash-table-p macher-agent-vfs--wrapped-tools-hash)
    (let ((injected-tools nil))
      (maphash
       (lambda (tool orig-slots)
         (cond
          ((and (listp orig-slots) (plist-get orig-slots :injected))
           (push tool injected-tools))
          ((and (macher-agent-tool-valid-p tool)
                (listp orig-slots))
           (setf (gptel-tool-function tool) (plist-get orig-slots :function)
                 (gptel-tool-description tool) (plist-get orig-slots :description)
                 (gptel-tool-args tool) (plist-get orig-slots :args)
                 (gptel-tool-category tool) (plist-get orig-slots :category)
                 (gptel-tool-async tool) (plist-get orig-slots :async)
                 (gptel-tool-confirm tool) (plist-get orig-slots :confirm)
                 (gptel-tool-include tool) (plist-get orig-slots :include)))))
       macher-agent-vfs--wrapped-tools-hash)
      (when injected-tools
        (macher-agent-map-known-tool-alists
         (lambda (known-tools)
           (when-let* ((tools-entry (assoc "macher" known-tools)))
             (setcdr tools-entry
                     (cl-remove-if
                      (lambda (item)
                        (let ((tool (if (consp item) (cdr item) item)))
                          (memq tool injected-tools)))
                      (cdr tools-entry)))))))
      (clrhash macher-agent-vfs--wrapped-tools-hash))))

(defun macher-agent-vfs-wrap-macher-tools-in-alist (known-tools)
  "Wrap Macher tools in KNOWN-TOOLS and ensure the search tool is present."
  (when-let* ((tools-entry (assoc "macher" known-tools)))
    (unless (cl-some (lambda (item)
                       (macher-agent-vfs--search-tool-name-p
                        (if (consp item) (car item) (macher-agent-vfs--extract-tool-name item))))
                     (cdr tools-entry))
      (let ((expected-name (macher-agent-macher-search-tool-name)))
        (when-let* ((replacement (macher-agent-vfs-get-search-in-workspace-tool))
                    ((macher-agent-tool-valid-p replacement)))
          (let ((injected (macher-agent-vfs--named-tool-copy replacement expected-name t)))
            (setcdr tools-entry
                    (cons (cons expected-name injected)
                          (cdr tools-entry)))))))
    (dolist (item (cdr tools-entry))
      (let ((tool (if (consp item) (cdr item) item)))
        (when (macher-agent-tool-valid-p tool)
          (macher-agent-vfs-wrap-single-tool tool))))))

(defun macher-agent-vfs-wrap-macher-tools ()
  "Wrap all Macher tools across default and buffer-local known tool alists."
  (macher-agent-map-known-tool-alists #'macher-agent-vfs-wrap-macher-tools-in-alist))

(defun macher-agent-vfs-normalize-tool-list (tools &optional context)
  "Wrap Macher structs in TOOLS and replace Macher search tools.
Uses optional CONTEXT to prefer workspace-local script overrides."
  (let* ((ctx (or (and (macher-agent-valid-context-p context) context)
                  (macher-agent-resolve-context)))
         (local-reg (and (macher-agent-valid-context-p ctx)
                         (macher-agent-context-tools ctx)))
         (ws-reg-p (and (hash-table-p local-reg)
                        (not (eq local-reg macher-agent-tools-registry))))
         (search-tool (and ws-reg-p
                           (cl-some (lambda (key)
                                      (let ((cand (gethash key local-reg)))
                                        (and (macher-agent-tool-valid-p cand)
                                             (gethash cand macher-agent-sandbox-workspace-tool-overrides)
                                             cand)))
                                    (macher-agent-vfs--search-tool-names)))))
    (mapcar
     (lambda (tool)
       (if (not (macher-agent-tool-valid-p tool))
           tool
         (let ((name (gptel-tool-name tool))
               (cat (gptel-tool-category tool))
               (was-ws-override (and (boundp 'macher-agent-sandbox-workspace-tool-overrides)
                                     (hash-table-p macher-agent-sandbox-workspace-tool-overrides)
                                     (gethash tool macher-agent-sandbox-workspace-tool-overrides))))
           (cond
            ((and (macher-agent-vfs--search-tool-name-p name)
                  search-tool
                  (or (member cat '("macher" "macher__processed" "perception"))
                      was-ws-override
                      (member name '("search_in_workspace" "macher_search"))))
             (macher-agent-vfs--named-tool-copy
              (or (and ws-reg-p
                       (let ((exact (gethash name local-reg)))
                         (and (macher-agent-tool-valid-p exact)
                              (gethash exact macher-agent-sandbox-workspace-tool-overrides)
                              exact)))
                  search-tool)
              name))
            ((and (macher-agent-vfs--search-tool-name-p name)
                  (or (member cat '("macher" "macher__processed"))
                      was-ws-override))
             (or (macher-agent-vfs--named-tool-copy
                  (macher-agent-vfs-get-search-in-workspace-tool) name)
                 tool))
            ((equal cat "macher")
             (macher-agent-vfs-wrap-single-tool tool)
             tool)
            (t tool)))))
     tools)))

(defun macher-agent-vfs--wrap-composed-tools (state)
  "Wrap Macher tools and replace `search_in_workspace' in composed STATE."
  (cl-check-type state list)
  (if-let* ((tools (plist-get state :tools)))
      (plist-put state :tools (macher-agent-vfs-normalize-tool-list tools))
    state))

(defun macher-agent-vfs--prepare-transmission (state)
  "Wrap Macher tools and expand workspace placeholders in transmission STATE."
  (cl-check-type state macher-agent-transmission-state)
  (let* ((target-buf (or (macher-agent-transmission-state-target-buffer state)
                         (current-buffer)))
         (ctx (or (macher-agent-transmission-state-context state)
                  (macher-agent-resolve-context target-buf)))
         (tools (macher-agent-transmission-state-tools state))
         (base-sys (macher-agent-transmission-state-base-prompt state)))
    (when tools
      (setf (macher-agent-transmission-state-tools state)
            (macher-agent-vfs-normalize-tool-list tools ctx)))
    (when (stringp base-sys)
      (let ((expanded (if (and (bufferp target-buf) (buffer-live-p target-buf))
                          (with-current-buffer target-buf
                            (macher-agent-macher-expand-system-placeholder base-sys target-buf))
                        (macher-agent-macher-expand-system-placeholder base-sys))))
        (unless (equal expanded base-sys)
          (setf (macher-agent-transmission-state-base-prompt state) expanded)
          (setf (macher-agent-transmission-state-compiled-prompt state) expanded)))))
  state)

;;; Baseline Sync & Context Merging

(defun macher-agent-vfs--entry-canonical-baseline (path root is-buf)
  "Return canonical disk or live-buffer baseline for PATH under ROOT.
When IS-BUF is non-nil, reads from the live virtual buffer."
  (let ((abs-path (if (or is-buf (file-name-absolute-p path))
                      path
                    (expand-file-name path root))))
    (macher-agent-vfs--read-canonical-content
     (if is-buf path abs-path) is-buf)))

(defun macher-agent-vfs--entry-disk-fresh-p (entry abs-path)
  "Return non-nil if ENTRY's `orig' matches disk at ABS-PATH.
Uses cached disk stat first to avoid reading unchanged files."
  (let ((orig (macher-agent-vfs-entry-orig entry))
        (cur-stat (macher-agent-vfs--file-stat abs-path)))
    (cond
     ((and orig cur-stat (macher-agent-vfs--stat-unchanged-p entry cur-stat))
      t)
     ((and (null orig) (null cur-stat))
      t)
     ((null cur-stat)
      nil)
     (t
      (equal orig (macher-agent-vfs--read-canonical-content abs-path nil))))))

(defun macher-agent-vfs-sync-baseline-context (source-ctx target-ctx)
  "Synchronise baseline entries from SOURCE-CTX into TARGET-CTX.
Uses O(1) CoW hash lookups and cached disk stats so unchanged files avoid
disk reads and quadratic scans.  Never overwrites TARGET-CTX entries with
`pending-patch' set to t or fresher `orig' baselines matching disk."
  (cl-check-type source-ctx macher-agent-context)
  (cl-check-type target-ctx macher-agent-context)
  (unless (eq source-ctx target-ctx)
    (let ((src-scoped (macher-agent-vfs-scoped-buffers source-ctx))
          (tgt-scoped (macher-agent-vfs-scoped-buffers target-ctx)))
      (when (and (hash-table-p src-scoped) (hash-table-p tgt-scoped))
        (maphash (lambda (k v) (puthash k v tgt-scoped)) src-scoped)))
    (let* ((src-pairs (macher-agent-vfs--layer-all-pairs source-ctx))
           (tgt-root (or (macher-agent-context-project-root target-ctx)
                         (macher-agent-context-project-root source-ctx)
                         default-directory))
           (tgt-tok (macher-agent-vfs--owner-token target-ctx))
           (canon-root (ignore-errors
                         (file-name-as-directory (file-truename (expand-file-name tgt-root)))))
           (modified nil))
      (dolist (pair src-pairs)
        (let* ((src-entry (car pair))
               (path (macher-agent-vfs-entry-path src-entry))
               (norm-path (or (when (and (stringp path) (not (file-name-absolute-p path))
                                         (not (string-prefix-p "./" path))
                                         (not (string-match-p "\\.\\." path)))
                                path)
                              (macher-agent-vfs-normalize-path-key path target-ctx canon-root)))
               (abs-path (if (file-name-absolute-p (or norm-path path))
                             (or norm-path path)
                           (expand-file-name (or norm-path path) tgt-root)))
               (tgt-lookup (macher-agent-vfs--layer-lookup-pair target-ctx norm-path path))
               (existing (car tgt-lookup))
               (existing-layer (cdr tgt-lookup))
               (existing-pending (and existing
                                      existing-layer
                                      (eq (macher-agent-vfs-layer-owner existing-layer) tgt-tok)
                                      (macher-agent-vfs-entry-pending-patch existing)))
               (src-fresh-p (macher-agent-vfs--entry-disk-fresh-p src-entry abs-path)))
          (cond
           (existing-pending
            nil)
           ((and existing
                 (not src-fresh-p)
                 (macher-agent-vfs--entry-disk-fresh-p existing abs-path))
            nil)
           (existing
            (if src-fresh-p
                (let ((fresh (macher-agent-vfs-copy-entry src-entry)))
                  (setf (macher-agent-vfs-entry-path fresh) (or norm-path path)
                        (macher-agent-vfs-entry-pending-patch fresh) nil)
                  (macher-agent-vfs--layer-put target-ctx (or norm-path path) fresh))
              (let ((cur-stat (macher-agent-vfs--file-stat abs-path)))
                (if (null cur-stat)
                    (macher-agent-vfs--layer-remove target-ctx (or norm-path path))
                  (let* ((current-state (macher-agent-vfs--read-canonical-content abs-path nil))
                         (fresh (macher-agent-vfs-create-entry
                                 :path (or norm-path path)
                                 :orig current-state
                                 :curr current-state
                                 :pending-patch nil)))
                    (macher-agent-vfs--stamp-entry-stat fresh cur-stat)
                    (macher-agent-vfs--layer-put target-ctx (or norm-path path) fresh)))))
            (setq modified t))
           (src-fresh-p
            (let ((new-entry (macher-agent-vfs-copy-entry src-entry)))
              (setf (macher-agent-vfs-entry-path new-entry) (or norm-path path)
                    (macher-agent-vfs-entry-pending-patch new-entry) nil)
              (macher-agent-vfs--layer-put target-ctx (or norm-path path) new-entry)
              (setq modified t))))))
      (when modified
        (macher-agent-vfs--recompute-dirty-flag target-ctx)))
    (let ((src-vfs-ht (macher-agent-vfs-buffers source-ctx))
          (tgt-vfs-ht (macher-agent-vfs-buffers target-ctx))
          (tgt-root (or (macher-agent-context-project-root target-ctx)
                        (macher-agent-context-project-root source-ctx)
                        default-directory)))
      (when (and (hash-table-p src-vfs-ht)
                 (hash-table-p tgt-vfs-ht)
                 (> (hash-table-count src-vfs-ht) 0))
        (maphash
         (lambda (path src-entry)
           (when (macher-agent-vfs-entry-p src-entry)
             (let* ((existing (gethash path tgt-vfs-ht))
                    (current-state (macher-agent-vfs--entry-canonical-baseline path tgt-root t))
                    (src-orig (macher-agent-vfs-entry-orig src-entry))
                    (src-fresh-p (equal src-orig current-state)))
               (cond
                ((and (macher-agent-vfs-entry-p existing)
                      (macher-agent-vfs-entry-pending-patch existing))
                 nil)
                ((and (macher-agent-vfs-entry-p existing)
                      (not src-fresh-p)
                      (equal (macher-agent-vfs-entry-orig existing) current-state))
                 nil)
                ((macher-agent-vfs-entry-p existing)
                 (if src-fresh-p
                     (progn
                       (setf (macher-agent-vfs-entry-orig existing) src-orig)
                       (setf (macher-agent-vfs-entry-curr existing) (macher-agent-vfs-entry-curr src-entry))
                       (setf (macher-agent-vfs-entry-pending-patch existing) nil))
                   (remhash path tgt-vfs-ht)))
                (src-fresh-p
                 (let ((new-entry (macher-agent-vfs-copy-entry src-entry)))
                   (setf (macher-agent-vfs-entry-pending-patch new-entry) nil)
                   (puthash path new-entry tgt-vfs-ht)))))))
         src-vfs-ht))))
  target-ctx)

(defun macher-agent-vfs-merge-contexts (parent-ctx child-ctx)
  "Merge only CHILD-CTX's own delta entries into PARENT-CTX in O(|delta|).
Never re-introduces stale pre-external-write baseline entries from
CHILD-CTX into PARENT-CTX."
  (cl-check-type parent-ctx macher-agent-context)
  (cl-check-type child-ctx macher-agent-context)
  (unless (eq parent-ctx child-ctx)
    (let ((child-scoped (macher-agent-vfs-scoped-buffers child-ctx))
          (parent-scoped (macher-agent-vfs-scoped-buffers parent-ctx)))
      (when (and (hash-table-p child-scoped) (hash-table-p parent-scoped))
        (maphash (lambda (k v) (puthash k v parent-scoped)) child-scoped)))
    (let* ((child-owned (macher-agent-vfs--layer-owned-entries child-ctx))
           (child-vfs-ht (macher-agent-vfs-buffers child-ctx))
           (parent-vfs-ht (macher-agent-vfs-buffers parent-ctx))
           (parent-root (or (macher-agent-context-project-root parent-ctx)
                            (macher-agent-context-project-root child-ctx)
                            default-directory))
           (canon-root (ignore-errors
                         (file-name-as-directory (file-truename (expand-file-name parent-root)))))
           (any-merged nil))
      (dolist (child-entry child-owned)
        (let* ((path (macher-agent-vfs-entry-path child-entry))
               (new (macher-agent-vfs-entry-curr child-entry))
               (pending (macher-agent-vfs-entry-pending-patch child-entry))
               (norm-path (or (when (and (stringp path) (not (file-name-absolute-p path))
                                         (not (string-prefix-p "./" path))
                                         (not (string-match-p "\\.\\." path)))
                                path)
                              (macher-agent-vfs-normalize-path-key path parent-ctx canon-root)))
               (parent-entry (car (macher-agent-vfs--layer-lookup-pair parent-ctx norm-path path))))
          (if (not pending)
              (unless parent-entry
                (let ((abs-path (if (file-name-absolute-p (or norm-path path))
                                    (or norm-path path)
                                  (expand-file-name (or norm-path path) parent-root))))
                  (when (macher-agent-vfs--entry-disk-fresh-p child-entry abs-path)
                    (let ((copy (macher-agent-vfs-copy-entry child-entry)))
                      (setf (macher-agent-vfs-entry-path copy) (or norm-path path)
                            (macher-agent-vfs-entry-pending-patch copy) nil)
                      (macher-agent-vfs--layer-put parent-ctx (or norm-path path) copy)))))
            (macher-agent-vfs-update-context-file parent-ctx (or norm-path path) new)
            (setq any-merged t))))
      (when (and (hash-table-p child-vfs-ht)
                 (hash-table-p parent-vfs-ht)
                 (> (hash-table-count child-vfs-ht) 0))
        (maphash
         (lambda (path child-entry)
           (when (macher-agent-vfs-entry-p child-entry)
             (let ((new (macher-agent-vfs-entry-curr child-entry))
                   (pending (macher-agent-vfs-entry-pending-patch child-entry))
                   (parent-entry (gethash path parent-vfs-ht)))
               (if (not pending)
                   (unless parent-entry
                     (let ((current-state (macher-agent-vfs--entry-canonical-baseline
                                           path parent-root t)))
                       (when (equal (macher-agent-vfs-entry-orig child-entry) current-state)
                         (let ((copy (macher-agent-vfs-copy-entry child-entry)))
                           (setf (macher-agent-vfs-entry-pending-patch copy) nil)
                           (puthash path copy parent-vfs-ht)))))
                 (macher-agent-vfs-update-context-file parent-ctx path new)
                 (setq any-merged t)))))
         child-vfs-ht))
      (when any-merged
        (macher-agent-vfs--recompute-dirty-flag parent-ctx))))
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
                     ((macher-agent-plist-p payload)
                      (let ((c (or (plist-get payload :child-context) (plist-get payload :context))))
                        (if (and c (macher-agent-valid-context-p c)) c nil)))))
         (diff (cond
                (p-struct (let ((pl (macher-agent-transit-payload-payload p-struct)))
                            (cond
                             ((and (macher-agent-plist-p pl) (plist-get pl :diff)) (plist-get pl :diff))
                             ((and (macher-agent-plist-p pl) (plist-get pl :vfs-diff)) (plist-get pl :vfs-diff))
                             ((listp pl) pl)
                             (t nil))))
                ((macher-agent-plist-p payload)
                 (let ((d (or (plist-get payload :diff) (plist-get payload :vfs-diff) (plist-get payload :payload))))
                   (if (listp d) d nil)))
                ((and (listp payload) (macher-agent-vfs-entry-p (car-safe payload)))
                 payload))))

    (when parent-ctx
      (when (and child-ctx (not (eq child-ctx parent-ctx)))
        (cl-check-type child-ctx macher-agent-context)
        (macher-agent-vfs-merge-contexts parent-ctx child-ctx))

      (when diff
        (let ((any-mutated nil))
          (dolist (item diff)
            (let* ((v-item (cond
                            ((macher-agent-vfs-entry-p item) item)
                            ((listp item)
                             (macher-agent-vfs-create-entry
                              :path (or (plist-get item :path) (plist-get item :file))
                              :orig (plist-get item :orig)
                              :curr (or (plist-get item :content) (plist-get item :curr) (plist-get item :contents)))))))
              (when v-item
                (let ((path (macher-agent-vfs-entry-path v-item))
                      (curr (macher-agent-vfs-entry-curr v-item)))
                  (when path
                    (macher-agent-vfs-update-context-file parent-ctx path curr)
                    (setq any-mutated t))))))
          (when any-mutated
            (macher-agent-vfs--recompute-dirty-flag parent-ctx))))

      (when (boundp 'macher-agent-persistent-context)
        (setq macher-agent-persistent-context parent-ctx))

      (if (macher-agent-transit-payload-p payload)
          (setf (macher-agent-transit-payload-target-context payload) parent-ctx)
        (when (macher-agent-plist-p payload)
          (setq payload (plist-put (copy-sequence payload) :target-context parent-ctx))))
      (or payload parent-ctx))))

;;; Virtual Buffer Rendering

(defun macher-agent-vfs--apply-single-virtual-buffer (entry)
  "Render content explicitly derived from formatting ENTRY struct."
  (cl-check-type entry macher-agent-vfs-entry)
  (let* ((path (macher-agent-vfs-entry-path entry))
         (content (macher-agent-vfs-entry-curr entry))
         (curr (string-trim-right (or content "")))
         (buf (cond ((bufferp path) (when (buffer-live-p path) path))
                    ((stringp path) (or (get-buffer path) (get-file-buffer path))))))
    (when (and buf (buffer-live-p buf))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert curr)))
      t)))

(defun macher-agent-vfs-apply-virtual-buffers (&optional context)
  "Apply pending virtual edits from CONTEXT to live Emacs buffers."
  (let* ((ctx (macher-agent-resolve-context context))
         (vfs-ht (when ctx (macher-agent-vfs-buffers ctx))))
    (when (hash-table-p vfs-ht)
      (maphash
       (lambda (path entry)
         (when (and (macher-agent-vfs-entry-p entry)
                    (stringp path)
                    (not (string-empty-p path)))
           (let* ((content (macher-agent-vfs-entry-curr entry))
                  (trimmed (string-trim-right (or content ""))))
             (when (macher-agent-vfs--apply-single-virtual-buffer entry)
               (puthash path
                        (macher-agent-vfs-create-entry
                         :path path
                         :orig trimmed
                         :curr trimmed
                         :pending-patch nil)
                        vfs-ht)))))
       vfs-ht)
      (macher-agent-vfs--recompute-dirty-flag ctx))))

;;; Asynchronous VFS Sandbox Pipeline

(defun macher-agent-vfs-call-with-strict-pipeline (context-or-resolve callback-or-body &optional maybe-body-fn)
  "Hydrate a temporary VFS sandbox asynchronously and invoke the body.
Dispatches CONTEXT-OR-RESOLVE, CALLBACK-OR-BODY, and MAYBE-BODY-FN to
support both 2 arguments (callback body-fn), resolving context from
`(current-buffer)', and 3 arguments (ctx callback body-fn).  The
callback receives the final result."
  (let* ((three-args-p (not (null maybe-body-fn)))
         (raw-context (if three-args-p context-or-resolve (current-buffer)))
         (callback (if three-args-p callback-or-body context-or-resolve))
         (body-fn (if three-args-p maybe-body-fn callback-or-body)))
    (cl-check-type callback function)
    (cl-check-type body-fn function)
    (let* ((ctx (or (macher-agent-resolve-context raw-context)
                    (macher-agent-resolve-context (current-buffer))
                    (macher-agent-resolve-context nil)))
           (_ (when (macher-agent-valid-context-p ctx)
                (macher-agent-vfs-auto-sync-context ctx)))
           (raw-root (when (macher-agent-valid-context-p ctx)
                       (macher-agent-context-project-root ctx)))
           (canonical-root (and raw-root (file-truename (expand-file-name raw-root))))
           (contents (when (macher-agent-valid-context-p ctx)
                       (macher-agent-vfs-context-contents ctx))))
      (if (not (and ctx canonical-root (file-directory-p canonical-root)))
          (funcall callback "ERROR: VFS sandbox requires a valid workspace context and existing root directory.")
        (condition-case err
            (let* ((raw-sandbox-dir (make-temp-file "macher-sandbox-" t))
                   (sandbox-dir (file-name-as-directory (file-truename raw-sandbox-dir)))
                   (scrub-roots
                    (delete-dups
                     (delq nil
                           (list sandbox-dir
                                 (file-name-as-directory (expand-file-name raw-sandbox-dir))
                                 (file-name-as-directory (abbreviate-file-name sandbox-dir))
                                 (file-name-as-directory (abbreviate-file-name raw-sandbox-dir))))))
                   (scrub-output
                    (lambda (val)
                      (if (not (stringp val))
                          val
                        (let ((s val))
                          (dolist (root-slash scrub-roots s)
                            (setq s (replace-regexp-in-string
                                     (regexp-quote root-slash) "" s t t))
                            (let ((bare (directory-file-name root-slash)))
                              (unless (or (string-empty-p bare) (string= bare "/"))
                                (setq s (replace-regexp-in-string
                                         (regexp-quote bare) "." s t t)))))))))
                   (sync-cmd (macher-agent-vfs--build-rsync-cmd canonical-root sandbox-dir))
                   (cleaned nil)
                   (cleanup-and-resolve
                    (lambda (result)
                      (unless cleaned
                        (setq cleaned t)
                        (when (and sandbox-dir (file-directory-p sandbox-dir))
                          (make-process :name "macher-sandbox-rm"
                                        :noquery t
                                        :command (list "rm" "-rf" sandbox-dir))))
                      (funcall callback (funcall scrub-output result)))))
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
                             (macher-agent-vfs--apply-overlay-stateless contents canonical-root sandbox-dir))
                           (let ((default-directory sandbox-dir)
                                 (macher-agent-active-sandbox-dir sandbox-dir)
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

(defun macher-agent-vfs-run-in-sandbox (ctx-or-command command-or-resolve &optional resolve-or-format maybe-format-fn)
  "Hydrate a VFS sandbox and execute a command asynchronously.
Manages the output buffer, `make-process', sentinel cleanup, and
output truncation for `skills/scripts/*.el'.  Accepts CTX-OR-COMMAND,
COMMAND-OR-RESOLVE, RESOLVE-OR-FORMAT, and MAYBE-FORMAT-FN, supporting
both `(command resolve-fn &optional format-fn)' and
`(ctx command resolve-fn &optional format-fn)'.  The command may be a
shell string or list of program and arguments.  The format function,
when non-nil, is called as `(funcall format-fn exit-code output)'."
  (let* ((has-ctx (not (functionp command-or-resolve)))
         (ctx (if has-ctx ctx-or-command (current-buffer)))
         (command (if has-ctx command-or-resolve ctx-or-command))
         (resolve-fn (if has-ctx resolve-or-format command-or-resolve))
         (format-fn (if has-ctx maybe-format-fn resolve-or-format)))
    (cl-check-type resolve-fn function)
    (macher-agent-vfs-call-with-strict-pipeline
     ctx resolve-fn
     (lambda (sandbox-dir done)
       (let* ((default-directory (macher-agent-ensure-trailing-slash sandbox-dir))
              (out-buf (generate-new-buffer " *macher-sandbox-cmd*"))
              (cmd-list (if (stringp command)
                            (list shell-file-name shell-command-switch command)
                          command))
              (timeout-secs (bound-and-true-p macher-agent-sandbox-command-timeout))
              (timed-out nil)
              (resolved nil)
              (timeout-timer nil))
         (condition-case err
             (let ((proc
                    (make-process
                     :name "macher-sandbox-cmd"
                     :buffer out-buf
                     :noquery t
                     :command cmd-list
                     :sentinel
                     (lambda (proc event)
                       (when (and (not resolved)
                                  (memq (process-status proc) '(exit signal)))
                         (setq resolved t)
                         (when (timerp timeout-timer)
                           (cancel-timer timeout-timer)
                           (setq timeout-timer nil))
                         (let ((raw-out (if (buffer-live-p out-buf)
                                            (with-current-buffer out-buf
                                              (buffer-string))
                                          ""))
                               (status (process-status proc))
                               (code (process-exit-status proc)))
                           (when (buffer-live-p out-buf)
                             (kill-buffer out-buf))
                           (let ((formatted
                                  (cond
                                   (timed-out
                                    (format "ERROR: Sandbox command timed out after %ss.%s"
                                            timeout-secs
                                            (if (string-empty-p (string-trim raw-out))
                                                ""
                                              (concat "\n" raw-out))))
                                   ((functionp format-fn)
                                    (let ((custom-out (funcall format-fn code raw-out)))
                                      (if (and (eq status 'signal)
                                               (string-empty-p (string-trim (or custom-out ""))))
                                          (format "ERROR: Sandbox process terminated by signal (%s)."
                                                  (string-trim (or event "killed")))
                                        custom-out)))
                                   ((eq status 'signal)
                                    (format "ERROR: Sandbox process terminated by signal (%s).%s"
                                            (string-trim (or event "killed"))
                                            (if (string-empty-p (string-trim raw-out))
                                                ""
                                              (concat "\n" raw-out))))
                                   ((= code 0)
                                    raw-out)
                                   (t
                                    (format "ERROR (exit %d):\n%s" code raw-out)))))
                             (funcall done (macher-agent-truncate-string formatted)))))))))
               (when (and (numberp timeout-secs) (> timeout-secs 0))
                 (setq timeout-timer
                       (run-at-time
                        timeout-secs nil
                        (lambda ()
                          (when (process-live-p proc)
                            (setq timed-out t)
                            (ignore-errors (delete-process proc))))))))
           (error
            (when (timerp timeout-timer)
              (cancel-timer timeout-timer))
            (when (buffer-live-p out-buf)
              (kill-buffer out-buf))
            (funcall done (format "ERROR: Failed to launch sandbox process: %s"
                                  (error-message-string err))))))))))

(defmacro macher-agent-vfs-with-strict-pipeline (context callback &rest body)
  "Hydrate CONTEXT VFS sandbox with CALLBACK, exposing `done' in BODY."
  (declare (indent 2) (debug t))
  `(macher-agent-vfs-call-with-strict-pipeline
    ,context
    ,callback
    (lambda (done)
      ,@body)))

;;; Installation & Lifecycle

;;;###autoload
(defun macher-agent-vfs-install ()
  "Install VFS storage hooks, skills, tool wrappers, and pipeline steps."
  (interactive)
  (when-let* ((skills-dir (macher-agent-vfs-skills-directory)))
    (macher-agent-register-skill-directory skills-dir))
  (setq macher-agent-macher-entry-converter-function #'macher-agent-vfs-entry-to-macher-entry)
  (setq macher-agent-workspace-read-file-function #'macher-agent-vfs-workspace-read-file)
  (setq macher-agent-workspace-list-files-function #'macher-agent-vfs-workspace-list-files)
  (macher-agent-register-pipeline-step 'preset-composition #'macher-agent-vfs--wrap-composed-tools 5)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-vfs--prepare-transmission 25)
  (macher-agent-register-pipeline-step 'payload-merge #'macher-agent-vfs--merge-payload 10)
  (macher-agent-register-pipeline-step 'context-clone #'macher-agent-vfs--clone-context-step 10)
  (add-hook 'macher-agent-vfs-flush-hook #'macher-agent-vfs-build-patch-from-hook)
  (add-hook 'macher-agent-task-flush-hook #'macher-agent-vfs-handle-flush)
  (add-hook 'macher-agent-context-sync-hook #'macher-agent-vfs-auto-sync-context)
  (add-hook 'macher-agent-peer-sync-hook #'macher-agent-vfs-sync-baseline-context)
  (add-hook 'macher-agent-actor-reap-hook #'macher-agent-vfs-release-actor-locks)
  (add-hook 'macher-agent-wrap-tools-hook #'macher-agent-vfs-wrap-macher-tools)
  (macher-agent-macher-install #'macher-agent-vfs--collect-raw-files
                               #'macher-agent-vfs-wrap-macher-tools))

;;;###autoload
(defun macher-agent-vfs-uninstall ()
  "Uninstall VFS storage hooks, skills, tool wrappers, and pipeline steps."
  (interactive)
  (when-let* ((skills-dir (macher-agent-vfs-skills-directory)))
    (macher-agent-unregister-skill-directory skills-dir))
  (when (eq macher-agent-workspace-read-file-function #'macher-agent-vfs-workspace-read-file)
    (setq macher-agent-workspace-read-file-function nil))
  (when (eq macher-agent-workspace-list-files-function #'macher-agent-vfs-workspace-list-files)
    (setq macher-agent-workspace-list-files-function nil))
  (macher-agent-unregister-pipeline-step 'preset-composition #'macher-agent-vfs--wrap-composed-tools)
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-vfs--prepare-transmission)
  (macher-agent-unregister-pipeline-step 'payload-merge #'macher-agent-vfs--merge-payload)
  (macher-agent-unregister-pipeline-step 'context-clone #'macher-agent-vfs--clone-context-step)
  (remove-hook 'macher-agent-vfs-flush-hook #'macher-agent-vfs-build-patch-from-hook)
  (remove-hook 'macher-agent-task-flush-hook #'macher-agent-vfs-handle-flush)
  (remove-hook 'macher-agent-context-sync-hook #'macher-agent-vfs-auto-sync-context)
  (remove-hook 'macher-agent-peer-sync-hook #'macher-agent-vfs-sync-baseline-context)
  (remove-hook 'macher-agent-actor-reap-hook #'macher-agent-vfs-release-actor-locks)
  (remove-hook 'macher-agent-wrap-tools-hook #'macher-agent-vfs-wrap-macher-tools)
  (macher-agent-vfs--unwrap-all-tools)
  (macher-agent-vfs--invalidate-raw-files-cache)
  (when (hash-table-p macher-agent-vfs--canon-root-cache)
    (clrhash macher-agent-vfs--canon-root-cache))
  (macher-agent-macher-uninstall))

(add-hook 'macher-agent-install-hook #'macher-agent-vfs-install)
(add-hook 'macher-agent-uninstall-hook #'macher-agent-vfs-uninstall)
(when (and (boundp 'gptel-prompt-transform-functions)
           (memq #'macher-agent-fsm-hijack-transform
                 (default-value 'gptel-prompt-transform-functions)))
  (macher-agent-vfs-install))

(provide 'macher-agent-vfs)
;;; macher-agent-vfs.el ends here
