;;; macher-agent-macher-bridge.el --- Bridge to Macher Core -*- lexical-binding: t; -*-

;;; Commentary:

;; Bridge implementation for Macher Agent to interact with Macher Core.

;;; Code:

(require 'macher)
(require 'cl-lib)
(require 'subr-x)

(declare-function macher-agent-workspace-project-root "macher-agent-vfs-client")
(declare-function macher-agent-workspace-p "macher-agent-vfs-client")
(declare-function macher-agent-resolve-context "macher-agent-vfs-client")

(defun macher-agent--get-workspace-root (ws)
  "Resolve the absolute project root of WS.

WS is the workspace object.

Return the absolute path string."
  (macher-agent-workspace-project-root ws))

(defun macher-agent--get-workspace-name (ws)
  "Retrieve a human-readable name for WS.

WS is the workspace object.

Return the workspace name string."
  (cond
   ((macher-agent-workspace-p ws)
    (file-name-nondirectory (directory-file-name (macher-agent-workspace-project-root ws))))
   ((and (consp ws) (eq (car ws) 'agent) (not (stringp (cdr ws))))
    (file-name-nondirectory (directory-file-name (macher-agent-workspace-project-root (cdr ws)))))
   (t (ignore-errors (macher--workspace-name ws)))))

(defun macher-agent--split-vfs-contents (contents)
  "Split raw VFS contents into pure virtual and physical lists.

CONTENTS is the list of VFS entry structures.

Return a cons cell (VIRTUAL-CONTENTS . PHYSICAL-CONTENTS)."
  (let ((virtual-contents nil)
        (physical-contents nil))
    (dolist (entry contents)
      (let* ((name (car entry))
             (live-buf (get-buffer name))
             (is-pure-virtual (and live-buf (null (buffer-file-name live-buf)))))
        (if is-pure-virtual
            (push entry virtual-contents)
          (push entry physical-contents))))
    (cons (nreverse virtual-contents) (nreverse physical-contents))))

(defun macher-agent--safe-workspace-hash (workspace &rest _args)
  "Nuke the core recursive hashing function which causes depth crashes.

WORKSPACE is the workspace object.
_ARGS represents extra arguments passed.

Return the MD5 hash string."
  (let ((path (cond
               ((and (recordp workspace) (eq (type-of workspace) 'macher-agent-workspace))
                (macher-agent-workspace-project-root workspace))
               ((and (consp workspace) (eq (car workspace) 'agent) (recordp (cdr workspace)))
                (macher-agent-workspace-project-root (cdr workspace)))
               ((and (consp workspace) (eq (car workspace) 'project))
                (cdr workspace))
               (t (format "%s" workspace)))))
    (md5 (or path "unknown-workspace"))))

(advice-add 'macher--workspace-hash :override #'macher-agent--safe-workspace-hash)

(defvar-local macher-agent--persistent-context nil)

(cl-defun macher-agent--make-vfs-context (&key workspace contents)
  "Create a native `macher-context` struct using `macher--make-context`.

WORKSPACE is the workspace object.
CONTENTS is the list of VFS entries.

Return the context struct."
  (let ((macher-agent--persistent-context nil))
    (macher--make-context :workspace workspace :contents contents)))

(defun macher-agent--prepare-patch-contexts (context fsm project-root)
  "Calculate and return isolated contexts for virtual buffers and physical files.

CONTEXT is the active context structure.
FSM is the finite-state machine object.
PROJECT-ROOT is the project root directory string.

Return a list containing virtual context, physical context, and physical contents."
  (let* ((vfs-ctx (or (ignore-errors (macher-agent-resolve-context (or fsm context))) context))
         (raw-contents (or (and context (macher-agent--get-context-contents context))
                           (and vfs-ctx (macher-agent--get-context-contents vfs-ctx))))
         (categorised (macher-agent--partition-vfs-entries raw-contents project-root))
         (virtual-contents (car categorised))
         (physical-contents (cdr categorised))
         (macher-compatible-ws (cons 'project project-root))
         (v-ctx (and virtual-contents
                     (let ((ctx (macher-agent--make-vfs-context :workspace macher-compatible-ws
                                                                :contents virtual-contents)))
                       (setf (macher-context-prompt ctx) (macher-agent--get-context-prompt context))
                       (macher-agent--set-context-dirty-p ctx t)
                       ctx)))
         (p-ctx (and physical-contents
                     (let ((ctx (macher-agent--make-vfs-context :workspace macher-compatible-ws
                                                                :contents physical-contents)))
                       (setf (macher-context-prompt ctx) (macher-agent--get-context-prompt context))
                       (macher-agent--set-context-dirty-p ctx t)
                       ctx))))
    (list v-ctx p-ctx physical-contents)))

(defun macher-agent-prepare-upstream-payloads (context)
  "Split the VFS context into independent, native macher-context structs.

CONTEXT is the active context structure.

Return a cons cell of (PHYSICAL-CONTEXT . VIRTUAL-CONTEXT)."
  (let* ((ws (macher-agent--get-context-workspace context))
         (project-root (macher-agent-workspace-project-root ws))
         (prepared (macher-agent--prepare-patch-contexts context nil project-root))
         (v-ctx (nth 0 prepared))
         (p-ctx (nth 1 prepared)))
    (cons p-ctx v-ctx)))

(defun macher-agent-process-request (&optional status context fsm)
  "Process request STATUS for CONTEXT and optional FSM.
Split the context into physical and virtual parts and call `macher--build-patch` natively on both."
  (let* ((ctx (cond
               ((and status (macher-context-p status)) status)
               (context context)
               (t (ignore-errors (macher-agent-resolve-context)))))
         (fsm-obj (cond
                   ((and context (not (macher-context-p context))) context)
                   (fsm fsm)
                   (t nil))))
    (when ctx
      (let* ((payloads (macher-agent-prepare-upstream-payloads ctx))
             (p-ctx (car payloads))
             (v-ctx (cdr payloads))
             (generated-buffers nil)
             (has-changes-p (lambda (c)
                              (and c
                                   (cl-some (lambda (entry)
                                              (let ((orig (if (consp (cdr entry)) (cadr entry) nil))
                                                    (curr (if (consp (cdr entry)) (cddr entry) (cdr entry))))
                                                (not (equal (or orig "") (or curr "")))))
                                            (macher-agent--get-context-contents c))))))
        (when (and (not (bound-and-true-p macher-agent--suppress-patch))
                   (funcall has-changes-p p-ctx))
          (macher--build-patch p-ctx fsm-obj)
          (when-let* ((buf (macher-patch-buffer (macher-context-workspace p-ctx)))
                      (name (buffer-name buf)))
            (let ((new-name (if (string-match "^\\*macher-patch\\(.*\\)\\*$" name)
                                (concat "*macher-physical-patch" (match-string 1 name) "*")
                              "*macher-physical-patch*")))
              (with-current-buffer buf
                (rename-buffer new-name t)
                (push (current-buffer) generated-buffers)))))
        (when (and (not (bound-and-true-p macher-agent--suppress-patch))
                   (funcall has-changes-p v-ctx))
          (macher--build-patch v-ctx fsm-obj)
          (when-let* ((buf (macher-patch-buffer (macher-context-workspace v-ctx)))
                      (name (buffer-name buf)))
            (let ((new-name (if (string-match "^\\*macher-patch\\(.*\\)\\*$" name)
                                (concat "*macher-virtual-patch" (match-string 1 name) "*")
                              "*macher-virtual-patch*")))
              (with-current-buffer buf
                (rename-buffer new-name t)
                (push (current-buffer) generated-buffers)))))
        (unless generated-buffers
          (message "Macher-Agent: No pending physical or virtual edits to review."))))))

(setq macher-process-request-function #'macher-agent-process-request)

(defun macher-agent--get-context-workspace (ctx)
  "Retrieve the workspace from context CTX.

CTX is the context structure.

Return the workspace struct, or nil."
  (cond
   ((and ctx (macher-context-p ctx))
    (let ((ws (macher-context-workspace ctx)))
      (if (and (consp ws) (eq (car ws) 'agent))
          (cdr ws)
        ws)))
   ((and (consp ctx) (eq (car ctx) 'agent))
    (cdr ctx))
   ((and ctx (macher-agent-workspace-p ctx))
    ctx)
   (t nil)))

(defun macher-agent--set-context-workspace (ctx ws)
  "Set the workspace on CTX to WS.

CTX is the context structure.
WS is the new workspace struct.

Return nil."
  (when (and ctx (macher-context-p ctx))
    (setf (macher-context-workspace ctx) ws)))

(defmacro macher-agent--def-context-accessor (name accessor &optional setter-name docstring)
  "Define a safe accessor and optional setter for a Macher context struct.

NAME is the symbol of the getter to define.
ACCESSOR is the field name symbol on the underlying struct.
SETTER-NAME is the optional symbol of the setter to define.
DOCSTRING is the optional documentation string to use."
  (let ((getter `(defun ,name (ctx)
                   ,(or docstring (format "Safely access `%s` on CTX." accessor))
                   (and ctx
                        (macher-context-p ctx)
                        (,accessor ctx)))))
    (if setter-name
        `(progn
           ,getter
           (defun ,setter-name (ctx val)
             ,(format "Safely set `%s` on CTX." accessor)
             (when (and ctx
                        (macher-context-p ctx))
               (setf (,accessor ctx) val))))
      getter)))

(macher-agent--def-context-accessor macher-agent--get-context-contents macher-context-contents macher-agent--set-context-contents)
(macher-agent--def-context-accessor macher-agent--get-context-dirty-p macher-context-dirty-p macher-agent--set-context-dirty-p)
(macher-agent--def-context-accessor macher-agent--get-context-prompt macher-context-prompt)
(macher-agent--def-context-accessor macher-agent--get-context-shadow-buffers macher-context-shadow-buffers macher-agent--set-context-shadow-buffers "Safely assign shadow buffers to the struct if the accessor is defined upstream.")

(defun macher-agent--get-fsm-latest ()
  "Get the active finite-state machine (FSM) if bound.

Return the active finite-state machine struct, or nil."
  (or (bound-and-true-p macher--fsm-latest)
      (bound-and-true-p gptel--fsm)
      (bound-and-true-p gptel--fsm-last)))

(provide 'macher-agent-macher-bridge)
;;; macher-agent-macher-bridge.el ends here
