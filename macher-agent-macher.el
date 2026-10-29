;;; macher-agent-macher.el --- Bridge to Macher Core -*- lexical-binding: t; -*-

;;; Commentary:

;; Tier 4 bridge implementation for Macher Agent VFS to interact with Macher Core.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(eval-and-compile
  (let* ((root (file-name-directory (or load-file-name buffer-file-name default-directory)))
         (macher-dir (expand-file-name "macher" root))
         (gptel-dir (expand-file-name "gptel" root)))
    (when (file-directory-p macher-dir)
      (add-to-list 'load-path macher-dir))
    (when (file-directory-p gptel-dir)
      (add-to-list 'load-path gptel-dir))))
(require 'macher)
(require 'macher-agent-core)

(defvar macher-workspace-types-alist)
(defvar macher-workspace-functions)
(defvar macher--workspace)
(defvar macher-agent--is-workspace)

(defvar macher-agent-macher-entry-converter-function nil
  "Function called with (ENTRY ROOT) to convert an entry into a Macher cell.")

(defun macher-agent-macher-safe-workspace-hash (context)
  "Generate a safe 16-character hash strictly from the CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (let* ((ws-id (or (macher-agent-context-project-root context) ""))
         (hash-input (secure-hash 'sha256 (concat "project" ws-id)))
         (chars "abcdefghijklmnopqrstuvwxyz0123456789")
         (result ""))
    (dotimes (i 16)
      (let* ((hex-char (aref hash-input i))
             (idx (mod (if (>= hex-char ?a) (- hex-char ?a -10) (- hex-char ?0)) (length chars))))
        (setq result (concat result (substring chars idx (1+ idx))))))
    result))

(defun macher-agent-macher-workspace-name (context)
  "Extract a concise project name strictly from the CONTEXT struct."
  (cl-check-type context macher-agent-context)
  (let ((root (macher-agent-context-project-root context)))
    (if root
        (file-name-nondirectory (directory-file-name root))
      "workspace")))

(defun macher-agent-macher-patch-buffer (ws-cons)
  "Return the patch buffer for upstream WS-CONS."
  (cl-check-type ws-cons cons)
  (if (fboundp 'macher-patch-buffer)
      (macher-patch-buffer ws-cons t)
    (get-buffer "*macher-patch*")))

(defun macher-agent-macher-build-patch (ctx prompt &optional files)
  "Build patch buffer via upstream macher using CTX, PROMPT, and FILES."
  (cl-check-type ctx macher-agent-context)
  (let* ((raw-root (macher-agent-context-project-root ctx))
         (canonical-root (when raw-root
                           (file-truename (expand-file-name raw-root))))
         (ws (cons 'project canonical-root))
         (mapped-contents
          (mapcar (lambda (entry)
                    (if (functionp macher-agent-macher-entry-converter-function)
                        (funcall macher-agent-macher-entry-converter-function entry canonical-root)
                      entry))
                  files))
         (ephemeral-ctx (when (fboundp 'macher--make-context)
                          (funcall 'macher--make-context
                                   :contents mapped-contents
                                   :workspace ws
                                   :prompt prompt
                                   :dirty-p t)))
         (patch-result (when (fboundp 'macher--build-patch)
                         (let ((default-directory (if canonical-root
                                                      (file-name-as-directory canonical-root)
                                                    default-directory)))
                           (macher--build-patch ephemeral-ctx nil)))))
    (if (buffer-live-p patch-result)
        patch-result
      (macher-agent-macher-patch-buffer ws))))

(defun macher-agent-macher-validate-path (path workspace)
  "Validate that PATH belongs to WORKSPACE."
  (when (and (stringp path) (stringp workspace) (not (string-empty-p workspace)))
    (let* ((canonical-ws (file-name-as-directory (file-truename workspace)))
           (resolved (expand-file-name path canonical-ws))
           (truename (file-truename resolved)))
      (unless (string-prefix-p canonical-ws truename)
        (error "SECURITY ERROR: Access denied. '%s' is outside the workspace '%s' and has not been explicitly added to the agent's scope"
               path canonical-ws))
      t)))

(defun macher-agent-macher-run-with-proxy-context (orphaned-context ws marshalled-files prompt target-buf orig-fn callback args)
  "Execute upstream tool ORIG-FN in TARGET-BUF using a proxy Macher context.
Validates ORPHANED-CONTEXT, constructs a proxy with WS, MARSHALLED-FILES, and
PROMPT, invokes ORIG-FN with CALLBACK and ARGS, and returns (RESULT . FILES)."
  (unless (and (fboundp 'macher-context-p) (macher-context-p orphaned-context))
    (error "Macher Agent: Tool called without an upstream context"))
  (let* ((effective-ws (or ws
                           (when (fboundp 'macher-context-workspace)
                             (ignore-errors (macher-context-workspace orphaned-context)))))
         (proxy (when (fboundp 'macher--make-context)
                  (let ((macher-agent--persistent-context nil))
                    (macher--make-context
                     :workspace effective-ws
                     :contents marshalled-files
                     :prompt prompt)))))
    (unless proxy
      (error "Macher Agent: Failed to construct upstream proxy context"))
    (let* ((res (with-current-buffer target-buf
                  (apply orig-fn proxy callback args)))
           (raw-after-files (if (fboundp 'macher-context-contents)
                                (with-no-warnings (macher-context-contents proxy))
                              marshalled-files)))
      (cons res raw-after-files))))

(defun macher-agent-macher-install (&optional get-files-fn)
  "Install Macher Core integration and workspace hooks idempotently.
Optional GET-FILES-FN provides the `:get-files' workspace callback."
  (let ((register-fn
         (lambda ()
           (when (boundp 'macher-workspace-types-alist)
             (let ((merged (copy-sequence (alist-get 'agent macher-workspace-types-alist))))
               (setq merged (plist-put merged :get-root 'macher-agent-workspace-project-root))
               (setq merged (plist-put merged :get-name 'macher-agent--get-name))
               (when (and get-files-fn (not (plist-get merged :get-files)))
                 (setq merged (plist-put merged :get-files get-files-fn)))
               (setf (alist-get 'agent macher-workspace-types-alist) merged)))
           (when (boundp 'macher-workspace-functions)
             (add-hook 'macher-workspace-functions #'macher-agent-workspace-agent)))))
    (funcall register-fn)
    (with-eval-after-load 'macher
      (funcall register-fn))))

(defun macher-agent--get-active-workspace ()
  "Retrieve globally active workspace."
  (bound-and-true-p macher--workspace))

(defun macher-agent-trigger-patch (&optional ctx)
  "Retrieve underlying patch buffer for CTX."
  (let* ((ws (if ctx
                 (macher-agent-context-workspace ctx)
               (macher-agent--get-active-workspace)))
         (patch-buf (and ws
                         (fboundp 'macher-patch-buffer)
                         (macher-patch-buffer ws))))
    patch-buf))

(defun macher-agent-workspace-agent ()
  "Identify if current buffer is a workspace and return workspace."
  (when (bound-and-true-p macher-agent--is-workspace)
    (bound-and-true-p macher--workspace)))

(defun macher-agent-apply-patch ()
  "Apply active patch from `diff-mode' context back to physical files."
  (interactive)
  (unless (derived-mode-p 'diff-mode) (user-error "Not in a patch/diff buffer"))
  (let* ((patch-content (buffer-substring-no-properties (point-min) (point-max)))
         (ctx (macher-agent-resolve-context (current-buffer)))
         (root (if ctx
                   (macher-agent-context-project-root ctx)
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
