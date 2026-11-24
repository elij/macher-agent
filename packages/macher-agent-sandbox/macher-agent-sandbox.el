;;; macher-agent-sandbox.el --- Sandboxed PTC evaluator for Macher Agent -*- lexical-binding: t; -*-

;; Author: Elijah Charles
;; Version: 0.8.3.66
;; Package-Requires: ((emacs "30.1") (gptel "0.9.9.6") (macher-agent "0.8.3.66"))
;; Keywords: convenience, gptel, llm
;; URL: https://github.com/elij/macher-agent
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Sandboxed Programmatic Tool Calling (PTC) evaluator plugin for
;; Macher Agent.  Provides a generator-based Emacs Lisp interpreter
;; that suspends on declared tool primitives, isolates workspace-local
;; `ptc-function' overrides, injects PTC prompt instructions and the
;; `ptc_execution' tool via the `macher-agent' pipeline, and manages
;; buffer-local PTC primitive state across skill transitions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'generator)
(require 'byte-opt)
(require 'gptel)
(require 'macher-agent)

(declare-function org-cycle "org" (&optional arg))

(defvar gptel-tools)
(defvar gptel-model)
(defvar gptel-pre-tool-call-functions)
(defvar gptel-post-tool-call-functions)
(defvar gptel-include-tool-results)
(defvar gptel-prompt-transform-functions)
(defvar macher-agent-ptc-execution-tool)

(defconst macher-agent-sandbox--package-dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing the `macher-agent-sandbox' library.")

(defun macher-agent-sandbox-skills-directory ()
  "Return the bundled skills directory for `macher-agent-sandbox', or nil."
  (let ((dir (expand-file-name "skills" macher-agent-sandbox--package-dir)))
    (when (file-directory-p dir)
      dir)))

;;;; Owned PTC State & Workspace Override Isolation

(defvar-local macher-agent-sandbox-active-ptc-primitives nil
  "List of active Programmatic Tool Calling primitive symbols or names.")
(put 'macher-agent-sandbox-active-ptc-primitives 'permanent-local t)

(defvar macher-agent-sandbox-active-ptc-execution nil
  "Dynamic flag bound non-nil while executing inside a PTC script.")

(defvar-local macher-agent-sandbox--cached-baseline-ptc-primitives nil
  "Cached `macher-agent-sandbox-active-ptc-primitives' before `use_skill'.")
(put 'macher-agent-sandbox--cached-baseline-ptc-primitives 'permanent-local t)

(defvar-local macher-agent-sandbox--cached-baseline-set nil
  "Non-nil when `macher-agent-sandbox--cached-baseline-ptc-primitives' is set.")
(put 'macher-agent-sandbox--cached-baseline-set 'permanent-local t)

(defvar macher-agent-sandbox-workspace-tool-overrides
  (make-hash-table :test 'eq :weakness 'key)
  "Weak table mapping workspace tools to isolated `ptc-function' closures.")

(defun macher-agent-sandbox--collect-put-ptc-symbols (forms)
  "Collect symbols targeted by `put' for `ptc-function' in FORMS."
  (let ((acc nil))
    (cl-labels
        ((walk (form)
           (when (consp form)
             (let ((head (car form)))
               (cond
                ((eq head 'quote) nil)
                ((and (eq head 'put)
                      (consp (cdr form))
                      (consp (cddr form)))
                 (let ((sym-expr (cadr form))
                       (prop-expr (caddr form)))
                   (when (and (consp sym-expr) (eq (car sym-expr) 'quote)
                              (symbolp (cadr sym-expr))
                              (consp prop-expr) (eq (car prop-expr) 'quote)
                              (memq (cadr prop-expr) '(ptc-function command-fn)))
                     (cl-pushnew (cadr sym-expr) acc :test #'eq)))
                 (let ((cur (cdr form)))
                   (while (consp cur)
                     (walk (car cur))
                     (setq cur (cdr cur)))))
                (t
                 (let ((cur form))
                   (while (consp cur)
                     (walk (car cur))
                     (setq cur (cdr cur))))))))))
      (dolist (f forms)
        (walk f)))
    acc))

(defun macher-agent-sandbox-evaluate-tool-forms (forms tool-name &optional workspace-override-p)
  "Evaluate trusted tool FORMS for TOOL-NAME with `ptc-function' isolation.
When WORKSPACE-OVERRIDE-P is non-nil, snapshot and restore global
`ptc-function' and `command-fn' symbol properties around evaluation and
record any workspace-defined `ptc-function' (or `:none') in
`macher-agent-sandbox-workspace-tool-overrides'."
  (if (not workspace-override-p)
      (macher-agent-evaluate-trusted-tool-ast forms tool-name nil)
    (let* ((under-name (replace-regexp-in-string "-" "_" tool-name))
           (hyphen-name (replace-regexp-in-string "_" "-" tool-name))
           (put-syms (macher-agent-sandbox--collect-put-ptc-symbols forms))
           (derived-syms
            (delq nil
                  (mapcar #'intern-soft
                          (delete-dups
                           (list (format "macher-agent-%s-tool" hyphen-name)
                                 (format "macher-agent-%s-tool" under-name)
                                 tool-name
                                 hyphen-name
                                 under-name)))))
           (ptc-syms (delete-dups (append put-syms derived-syms)))
           (sentinel (make-symbol "sandbox-ptc-unset"))
           (snapshots
            (mapcar (lambda (sym)
                      (list sym
                            (and (plist-member (symbol-plist sym) 'ptc-function) t)
                            (get sym 'ptc-function)
                            (and (plist-member (symbol-plist sym) 'command-fn) t)
                            (get sym 'command-fn)))
                    ptc-syms))
           (ws-ptc-fn nil)
           (captured-tool nil))
      (unwind-protect
          (progn
            (dolist (sym ptc-syms)
              (put sym 'ptc-function sentinel)
              (put sym 'command-fn sentinel))
            (setq captured-tool
                  (macher-agent-evaluate-trusted-tool-ast forms tool-name workspace-override-p))
            (dolist (sym ptc-syms)
              (unless ws-ptc-fn
                (let ((cur-ptc (get sym 'ptc-function))
                      (cur-cmd (get sym 'command-fn)))
                  (cond
                   ((and (not (eq cur-ptc sentinel)) (functionp cur-ptc))
                    (setq ws-ptc-fn cur-ptc))
                   ((and (not (eq cur-cmd sentinel)) (functionp cur-cmd))
                    (setq ws-ptc-fn cur-cmd)))))))
        (dolist (snap snapshots)
          (pcase-let ((`(,sym ,had-ptc ,old-ptc ,had-cmd ,old-cmd) snap))
            (if had-ptc
                (put sym 'ptc-function old-ptc)
              (cl-remprop sym 'ptc-function))
            (if had-cmd
                (put sym 'command-fn old-cmd)
              (cl-remprop sym 'command-fn)))))
      (when (macher-agent-tool-valid-p captured-tool)
        (puthash captured-tool (or ws-ptc-fn :none) macher-agent-sandbox-workspace-tool-overrides))
      captured-tool)))

;;;; Plugin State Accessors

(defun macher-agent-sandbox--state-buf-key (buf-or-name)
  "Normalise BUF-OR-NAME into a buffer name string, or nil."
  (cond
   ((stringp buf-or-name) buf-or-name)
   ((and (bufferp buf-or-name) (buffer-live-p buf-or-name))
    (buffer-name buf-or-name))
   (t nil)))

(defun macher-agent-sandbox-get-state (context &optional buf-name)
  "Extract sandbox configuration explicitly from CONTEXT struct.
When optional BUF-NAME is non-nil (or when `:sandbox' stores a per-buffer
hash table), return only the state owned by BUF-NAME (defaulting to the
current buffer's name).  Direct plists stored in `:sandbox' without a
hash table are returned as-is for backward compatibility."
  (cl-check-type context macher-agent-context)
  (let ((raw (plist-get (macher-agent-context-plugins context) :sandbox))
        (key (or (macher-agent-sandbox--state-buf-key buf-name)
                 (when (buffer-live-p (current-buffer))
                   (buffer-name (current-buffer))))))
    (cond
     ((hash-table-p raw)
      (when key (gethash key raw)))
     ((listp raw) raw)
     (t nil))))

(defun macher-agent-sandbox-set-state (context state &optional buf-name)
  "Bind sandbox STATE explicitly to CONTEXT struct.
When optional BUF-NAME is non-nil, update only the entry for BUF-NAME in
the per-buffer `:sandbox' table without disturbing sibling buffers
sharing CONTEXT."
  (cl-check-type context macher-agent-context)
  (let ((key (macher-agent-sandbox--state-buf-key buf-name)))
    (if (null key)
        (progn
          (setf (macher-agent-context-plugins context)
                (plist-put (copy-sequence (macher-agent-context-plugins context))
                           :sandbox state))
          state)
      (let ((raw (plist-get (macher-agent-context-plugins context) :sandbox)))
        (if state
            (let ((table (if (hash-table-p raw)
                             raw
                           (let ((ht (make-hash-table :test 'equal)))
                             (when (and (listp raw) raw)
                               (let ((owner (macher-agent-context-owner-buffer context)))
                                 (when (and (bufferp owner) (buffer-live-p owner))
                                   (puthash (buffer-name owner) raw ht))))
                             (setf (macher-agent-context-plugins context)
                                   (plist-put (copy-sequence (macher-agent-context-plugins context))
                                              :sandbox ht))
                             ht))))
              (puthash key state table)
              state)
          (cond
           ((hash-table-p raw)
            (remhash key raw))
           ((and (listp raw) raw)
            (let ((owner (macher-agent-context-owner-buffer context)))
              (if (and (bufferp owner) (buffer-live-p owner)
                       (not (equal (buffer-name owner) key)))
                  (let ((ht (make-hash-table :test 'equal)))
                    (puthash (buffer-name owner) raw ht)
                    (setf (macher-agent-context-plugins context)
                          (plist-put (copy-sequence (macher-agent-context-plugins context))
                                     :sandbox ht)))
                (setf (macher-agent-context-plugins context)
                      (plist-put (copy-sequence (macher-agent-context-plugins context))
                                 :sandbox nil))))))
          nil)))))

(defun macher-agent-sandbox--clone-context-step (new-ctx)
  "Clear inherited `:sandbox' plugin state on cloned NEW-CTX."
  (when (macher-agent-context-p new-ctx)
    (let ((plugins (copy-sequence (macher-agent-context-plugins new-ctx))))
      (setq plugins (plist-put plugins :sandbox nil))
      (setf (macher-agent-context-plugins new-ctx) plugins)))
  new-ctx)

(defun macher-agent-sandbox--ptc-primitive-p (sym)
  "Check whether SYM is an active Programmatic Tool Calling primitive."
  (let* ((sym-name (symbol-name sym))
         (norm-sym (replace-regexp-in-string "_" "-" sym-name))
         (active (bound-and-true-p macher-agent-sandbox-active-ptc-primitives))
         (tools (bound-and-true-p gptel-tools)))
    (and (or (memq sym active)
             (cl-some (lambda (prim)
                        (let* ((prim-name (if (symbolp prim) (symbol-name prim) prim))
                               (norm-prim (replace-regexp-in-string "_" "-" prim-name)))
                          (string= norm-sym norm-prim)))
                      active)
             (and (null active)
                  (not (string= norm-sym "ptc-execution"))
                  (cl-some (lambda (tool)
                             (let* ((resolved (if (gptel-tool-p tool)
                                                  tool
                                                (when (fboundp 'gptel-get-tool)
                                                  (ignore-errors
                                                    (gptel-get-tool
                                                     (if (stringp tool) tool
                                                       (format "%s" tool)))))))
                                    (t-name (if (gptel-tool-p resolved)
                                                (gptel-tool-name resolved)
                                              (if (stringp tool) tool
                                                (format "%s" tool))))
                                    (norm-tool (replace-regexp-in-string "_" "-" t-name))
                                    (cat (when (gptel-tool-p resolved)
                                           (gptel-tool-category resolved))))
                               (and (not (string= norm-tool "ptc-execution"))
                                    (not (equal cat "meta"))
                                    (string= norm-sym norm-tool))))
                           tools)))
         t)))

(defvar-local macher-agent-sandbox--globals nil
  "Store global variable environment for guest execution.")

(defvar-local macher-agent-sandbox--primitives nil
  "Store allowed host primitive operations.")

(defvar-local macher-agent-sandbox--functions nil
  "Store guest function definitions.")

(defconst macher-agent-sandbox--unbound (make-symbol "unbound")
  "Mark unbound variables in the sandbox environment.")

(defun macher-agent-sandbox--populate-pure-primitives (&optional primitives)
  "Populate PRIMITIVES table with pure host operations."
  (let ((table (or primitives macher-agent-sandbox--primitives)))
    (dolist
        (sym '(not null eq eql equal = + - * / < > <= >= max min
                   length list vector vconcat aref cons car cdr nth reverse
                   split-string plist-get string= string-match-p format))
      (when (fboundp sym)
        (puthash sym sym table)))
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (or (get sym 'pure)
                      (get sym 'side-effect-free)))
         (puthash sym sym table))))))

(defun macher-agent-sandbox--init (&optional extra-operations)
  "Initialise sandbox environment applying specific EXTRA-OPERATIONS."
  (when extra-operations
    (cl-check-type extra-operations (or hash-table list)))
  (unless (and macher-agent-sandbox--primitives
               (hash-table-p macher-agent-sandbox--primitives)
               (> (hash-table-count macher-agent-sandbox--primitives) 0))
    (setq macher-agent-sandbox--primitives (make-hash-table :test 'eq)
          macher-agent-sandbox--functions (make-hash-table :test 'eq)
          macher-agent-sandbox--globals (make-hash-table :test 'eq))
    (macher-agent-sandbox--populate-pure-primitives macher-agent-sandbox--primitives))
  (when extra-operations
    (if (hash-table-p extra-operations)
        (maphash (lambda (op _)
                   (puthash op op macher-agent-sandbox--primitives))
                 extra-operations)
      (dolist (op extra-operations)
        (when (symbolp op)
          (puthash op op macher-agent-sandbox--primitives))))))

(iter-defun macher-agent-sandbox--eval-args-iter (args env)
  "Evaluate a list of ARGS in ENV using the generator evaluator."
  (let ((evaled nil))
    (dolist (arg args (nreverse evaled))
      (push (iter-yield-from (macher-agent-sandbox--eval-iter arg env)) evaled))))

(iter-defun macher-agent-sandbox--apply-closure-iter (closure arguments)
  "Execute CLOSURE with ARGUMENTS as a generator."
  (let ((new-env (cadddr closure))
        (params (cadr closure))
        (result nil))
    (while params
      (let ((param (pop params)))
        (cond
         ((eq param '&rest)
          (push (cons (pop params) arguments) new-env)
          (setq arguments nil))
         ((eq param '&optional)
          nil)
         (t
          (push (cons param (pop arguments)) new-env)))))
    (dolist (form (caddr closure) result)
      (setq result (iter-yield-from (macher-agent-sandbox--eval-iter form new-env))))))

(iter-defun macher-agent-sandbox--funcall-symbol-iter (func eval-args)
  "Execute function symbol FUNC with EVAL-ARGS in the sandbox."
  (let ((prim (and macher-agent-sandbox--primitives
                   (gethash func macher-agent-sandbox--primitives)))
        (closure (and macher-agent-sandbox--functions
                      (gethash func macher-agent-sandbox--functions))))
    (cond
     ((macher-agent-sandbox--ptc-primitive-p func)
      (iter-yield (make-macher-agent-tool-call
                   :name func
                   :args (macher-agent-sandbox--normalize-args-to-plist eval-args))))
     (prim (apply prim eval-args))
     (closure (iter-yield-from (macher-agent-sandbox--apply-closure-iter closure eval-args)))
     (t (error "Void or forbidden function: %s" func)))))

(iter-defun macher-agent-sandbox--funcall-iter (func eval-args)
  "Execute function FUNC with EVAL-ARGS in the sandbox."
  (cond
   ((and (consp func) (eq (car func) 'lambda))
    (iter-yield-from
     (macher-agent-sandbox--apply-closure-iter
      (list 'closure (cadr func) (cddr func) nil) eval-args)))
   ((and (consp func) (eq (car func) 'closure))
    (iter-yield-from (macher-agent-sandbox--apply-closure-iter func eval-args)))
   ((symbolp func)
    (iter-yield-from (macher-agent-sandbox--funcall-symbol-iter func eval-args)))
   (t (error "Invalid function target: %s" func))))

(defun macher-agent-sandbox--normalize-args-to-plist (args)
  "Format explicit ARGS list to property sequence."
  (cl-check-type args list)
  (cond
   ((keywordp (car args)) args)
   ((and (listp args) (consp (car args)) (symbolp (caar args)) (not (keywordp (caar args))))
    (let (result)
      (dolist (pair args)
        (let* ((sym-name (symbol-name (car pair)))
               (key (if (string-prefix-p ":" sym-name)
                        (car pair)
                      (intern (concat ":" sym-name))))
               (val (cdr pair)))
          (push key result)
          (push val result)))
      (nreverse result)))
   ((and (listp args) (= (length args) 1) (listp (car args))
         (consp (caar args)) (symbolp (car (caar args))) (not (keywordp (car (caar args)))))
    (let (result)
      (dolist (pair (car args))
        (let* ((sym-name (symbol-name (car pair)))
               (key (if (string-prefix-p ":" sym-name)
                        (car pair)
                      (intern (concat ":" sym-name))))
               (val (cdr pair)))
          (push key result)
          (push val result)))
      (nreverse result)))
   (t args)))

(iter-defun macher-agent-sandbox--eval-single-binding-iter (binding eval-env)
  "Evaluate a single BINDING specification in EVAL-ENV."
  (let ((var (if (consp binding) (car binding) binding))
        (val (when (consp binding)
               (iter-yield-from (macher-agent-sandbox--eval-iter (cadr binding) eval-env)))))
    (cons var val)))

(iter-defun macher-agent-sandbox--bind-iter (bindings env is-star)
  "Process BINDINGS to extend ENV sequentially when IS-STAR is non-nil."
  (let ((new-env env))
    (dolist (binding bindings new-env)
      (let* ((eval-env (if is-star new-env env))
             (pair (iter-yield-from
                    (macher-agent-sandbox--eval-single-binding-iter binding eval-env))))
        (setq new-env (cons pair new-env))))))

(iter-defun macher-agent-sandbox--eval-and-or-iter (args env is-or)
  "Evaluate boolean ARGS in ENV, treating as `or' when IS-OR is non-nil."
  (let ((result (not is-or)))
    (while (and args (if is-or (not result) result))
      (setq result (iter-yield-from (macher-agent-sandbox--eval-iter (pop args) env))))
    result))

(iter-defun macher-agent-sandbox--eval-let-iter (args env is-star)
  "Evaluate `let' or `let*' ARGS in ENV, sequential when IS-STAR is non-nil."
  (let ((new-env (iter-yield-from (macher-agent-sandbox--bind-iter (car args) env is-star)))
        (result nil))
    (dolist (form (cdr args) result)
      (setq result (iter-yield-from (macher-agent-sandbox--eval-iter form new-env))))))

(iter-defun macher-agent-sandbox--eval-setq-iter (args env)
  "Evaluate `setq' form ARGS in ENV."
  (let ((val nil))
    (while args
      (let* ((var (pop args))
             (rhs (pop args))
             (evaled (iter-yield-from (macher-agent-sandbox--eval-iter rhs env)))
             (binding (assoc var env)))
        (setq val evaled)
        (if binding
            (setcdr binding val)
          (puthash var val macher-agent-sandbox--globals))))
    val))

(iter-defun macher-agent-sandbox--eval-mapcar-iter (args env)
  "Evaluate `mapcar' form ARGS in ENV over any sequence."
  (let ((func (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
        (seq (iter-yield-from (macher-agent-sandbox--eval-iter (cadr args) env)))
        (result nil))
    (unless (sequencep seq)
      (error "Execution error (mapcar): Wrong type argument: sequencep, %s" seq))
    (dolist (item (append seq nil) (nreverse result))
      (push (iter-yield-from (macher-agent-sandbox--funcall-iter func (list item))) result))))

(iter-defun macher-agent-sandbox--eval-mapc-iter (args env)
  "Evaluate `mapc' form ARGS in ENV over any sequence."
  (let ((func (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
        (seq (iter-yield-from (macher-agent-sandbox--eval-iter (cadr args) env))))
    (unless (sequencep seq)
      (error "Execution error (mapc): Wrong type argument: sequencep, %s" seq))
    (dolist (item (append seq nil) seq)
      (iter-yield-from (macher-agent-sandbox--funcall-iter func (list item))))))

(defvar macher-agent-sandbox--special-forms nil
  "Store mapping of special form symbols to handler functions.")

(setq
 macher-agent-sandbox--special-forms
 `((quote . ,(iter-lambda (args _env) (car args)))
   (mapcar . ,(iter-lambda (args env)
                (iter-yield-from (macher-agent-sandbox--eval-mapcar-iter args env))))
   (mapc . ,(iter-lambda (args env)
              (iter-yield-from (macher-agent-sandbox--eval-mapc-iter args env))))
   (function . ,(iter-lambda (args env)
                  (let ((fn-target (car args)))
                    (if (symbolp fn-target)
                        fn-target
                      (iter-yield-from (macher-agent-sandbox--eval-iter fn-target env))))))
   (lambda . ,(iter-lambda (args env)
                (list 'closure (car args) (cdr args) env)))
   (progn . ,(iter-lambda (args env)
               (let ((result nil))
                 (dolist (form args result)
                   (setq result (iter-yield-from
                                 (macher-agent-sandbox--eval-iter form env)))))))
   (if . ,(iter-lambda (args env)
            (if (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env))
                (iter-yield-from (macher-agent-sandbox--eval-iter (cadr args) env))
              (iter-yield-from (macher-agent-sandbox--eval-iter (caddr args) env)))))
   (and . ,(iter-lambda (args env)
             (iter-yield-from (macher-agent-sandbox--eval-and-or-iter args env nil))))
   (or . ,(iter-lambda (args env)
            (iter-yield-from (macher-agent-sandbox--eval-and-or-iter args env t))))
   (cond . ,(iter-lambda (args env)
              (let ((result nil)
                    (matched nil))
                (while (and args (not matched))
                  (let* ((clause (pop args))
                         (cond-val (iter-yield-from
                                    (macher-agent-sandbox--eval-iter (car clause) env))))
                    (when cond-val
                      (setq matched t)
                      (if (cdr clause)
                          (dolist (form (cdr clause))
                            (setq result (iter-yield-from
                                          (macher-agent-sandbox--eval-iter form env))))
                        (setq result cond-val)))))
                result)))
   (setq . ,(iter-lambda (args env)
              (iter-yield-from (macher-agent-sandbox--eval-setq-iter args env))))
   (while . ,(iter-lambda (args env)
               (while (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env))
                 (dolist (form (cdr args))
                   (iter-yield-from (macher-agent-sandbox--eval-iter form env))))
               nil))
   (let . ,(iter-lambda (args env)
             (iter-yield-from (macher-agent-sandbox--eval-let-iter args env nil))))
   (let* . ,(iter-lambda (args env)
              (iter-yield-from (macher-agent-sandbox--eval-let-iter args env t))))
   (defalias . ,(iter-lambda (args env)
                  (let ((func-name (iter-yield-from
                                    (macher-agent-sandbox--eval-iter (car args) env)))
                        (func-body (iter-yield-from
                                    (macher-agent-sandbox--eval-iter (cadr args) env))))
                    (puthash func-name func-body macher-agent-sandbox--functions)
                    func-name)))
   (funcall . ,(iter-lambda (args env)
                 (let ((func (iter-yield-from
                              (macher-agent-sandbox--eval-iter (car args) env)))
                       (eval-args (iter-yield-from
                                   (macher-agent-sandbox--eval-args-iter (cdr args) env))))
                   (iter-yield-from (macher-agent-sandbox--funcall-iter func eval-args)))))
   (apply . ,(iter-lambda (args env)
               (let* ((func (iter-yield-from
                             (macher-agent-sandbox--eval-iter (car args) env)))
                      (eval-args (iter-yield-from
                                  (macher-agent-sandbox--eval-args-iter (cdr args) env)))
                      (final-args (append (butlast eval-args) (car (last eval-args)))))
                 (iter-yield-from (macher-agent-sandbox--funcall-iter func final-args)))))
   (condition-case . ,(iter-lambda (args env)
                        (let ((var (car args))
                              (bodyform (cadr args))
                              (handlers (cddr args)))
                          (condition-case err
                              (iter-yield-from (macher-agent-sandbox--eval-iter bodyform env))
                            (error
                             (let* ((err-sym (car err))
                                    (err-conditions (get err-sym 'error-conditions))
                                    (matched-handler nil))
                               (dolist (handler handlers)
                                 (let ((cond-spec (car handler)))
                                   (when (or (eq cond-spec t)
                                             (memq cond-spec err-conditions)
                                             (and (listp cond-spec)
                                                  (cl-intersection cond-spec err-conditions)))
                                     (setq matched-handler handler))))
                               (if matched-handler
                                   (let ((new-env (if var (cons (cons var err) env) env))
                                         (result nil))
                                     (dolist (form (cdr matched-handler) result)
                                       (setq result (iter-yield-from (macher-agent-sandbox--eval-iter form new-env)))))
                                 (signal (car err) (cdr err)))))))))
   (unwind-protect . ,(iter-lambda (args env)
                        (let ((body-result nil))
                          (unwind-protect
                              (setq body-result (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
                            (dolist (form (cdr args))
                              (let ((iter (macher-agent-sandbox--eval-iter form env)))
                                (condition-case nil
                                    (while t
                                      (iter-next iter nil))
                                  (iter-end-of-sequence nil)))))
                          body-result)))
   (catch . ,(iter-lambda (args env)
               (catch (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env))
                 (let ((result nil))
                   (dolist (form (cdr args) result)
                     (setq result (iter-yield-from (macher-agent-sandbox--eval-iter form env))))))))
   (throw . ,(iter-lambda (args env)
               (throw (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env))
                      (iter-yield-from (macher-agent-sandbox--eval-iter (cadr args) env)))))
   (fboundp . ,(iter-lambda (args env)
                 (let ((sym (iter-yield-from
                             (macher-agent-sandbox--eval-iter (car args) env))))
                   (when (symbolp sym)
                     (if (or (gethash sym macher-agent-sandbox--primitives)
                             (gethash sym macher-agent-sandbox--functions)
                             (macher-agent-sandbox--ptc-primitive-p sym)
                             (assoc sym macher-agent-sandbox--special-forms))
                         t nil)))))
   (boundp . ,(iter-lambda (args env)
                (let ((sym (iter-yield-from
                            (macher-agent-sandbox--eval-iter (car args) env))))
                  (when (symbolp sym)
                    (if (or (assoc sym env)
                            (not (eq (gethash sym macher-agent-sandbox--globals
                                              macher-agent-sandbox--unbound)
                                     macher-agent-sandbox--unbound)))
                        t nil)))))
   (macroexpand . ,(iter-lambda (args env)
                     (let* ((form (iter-yield-from
                                   (macher-agent-sandbox--eval-iter (car args) env)))
                            (macro-def (when (and (consp form) (symbolp (car form)))
                                         (gethash (car form)
                                                  macher-agent-sandbox--functions))))
                       (if (and (consp macro-def) (eq (car macro-def) 'macro))
                           (iter-yield-from
                            (macher-agent-sandbox--funcall-iter (cdr macro-def) (cdr form)))
                         form))))
   (functionp . ,(iter-lambda (args env)
                   (let ((val (iter-yield-from
                               (macher-agent-sandbox--eval-iter (car args) env))))
                     (or (and (consp val) (memq (car val) '(closure lambda macro)))
                         (and (symbolp val)
                              (or (gethash val macher-agent-sandbox--primitives)
                                  (gethash val macher-agent-sandbox--functions)
                                  (macher-agent-sandbox--ptc-primitive-p val)))
                         (functionp val)))))))

(defun macher-agent-sandbox--eval-symbol (expression &optional environment)
  "Evaluate symbol EXPRESSION against lexical ENVIRONMENT and sandbox globals."
  (if (keywordp expression)
      expression
    (let ((binding (assoc expression environment)))
      (if binding
          (cdr binding)
        (let ((val (gethash expression macher-agent-sandbox--globals macher-agent-sandbox--unbound)))
          (when (eq val macher-agent-sandbox--unbound)
            (error "Unbound variable: %s" expression))
          val)))))

(iter-defun macher-agent-sandbox--eval-compound-iter (expression environment)
  "Evaluate compound form EXPRESSION in ENVIRONMENT."
  (let* ((operator (car expression))
         (arguments (cdr expression))
         (special-handler (and (symbolp operator)
                               (cdr (assoc operator macher-agent-sandbox--special-forms))))
         (macro-def (and (symbolp operator)
                         (gethash operator macher-agent-sandbox--functions))))
    (cond
     ((and (symbolp operator)
           (macher-agent-sandbox--ptc-primitive-p operator))
      (let ((evaled-args (iter-yield-from
                          (macher-agent-sandbox--eval-args-iter arguments environment))))
        (iter-yield
         (make-macher-agent-tool-call
          :name operator
          :args (macher-agent-sandbox--normalize-args-to-plist evaled-args)))))
     (special-handler
      (iter-yield-from (funcall special-handler arguments environment)))
     ((and (consp macro-def) (eq (car macro-def) 'macro))
      (let* ((macro-fn (cdr macro-def))
             (expanded-form (iter-yield-from
                             (macher-agent-sandbox--funcall-iter macro-fn arguments))))
        (iter-yield-from (macher-agent-sandbox--eval-iter expanded-form environment))))
     (t
      (let ((evaled-args (iter-yield-from
                          (macher-agent-sandbox--eval-args-iter arguments environment))))
        (iter-yield-from (macher-agent-sandbox--funcall-iter operator evaled-args)))))))

(iter-defun macher-agent-sandbox--eval-iter (expression environment)
  "Evaluate EXPRESSION in ENVIRONMENT yielding on PTC tool calls."
  (cond
   ((or (numberp expression) (stringp expression) (vectorp expression) (memq expression '(t nil)))
    expression)
   ((symbolp expression)
    (macher-agent-sandbox--eval-symbol expression environment))
   ((consp expression)
    (iter-yield-from (macher-agent-sandbox--eval-compound-iter expression environment)))))

(defun macher-agent-sandbox--resolve-ptc-execution-tool (&optional ctx)
  "Resolve the `ptc_execution' tool struct using optional CTX.
Return a valid `gptel-tool' satisfying `macher-agent-tool-valid-p', or nil."
  (let ((resolved (ignore-errors (macher-agent-resolve-tool "ptc_execution" nil nil ctx))))
    (cond
     ((macher-agent-tool-valid-p resolved) resolved)
     ((and (boundp 'macher-agent-ptc-execution-tool)
           (macher-agent-tool-valid-p (symbol-value 'macher-agent-ptc-execution-tool)))
      (symbol-value 'macher-agent-ptc-execution-tool))
     ((fboundp 'gptel-get-tool)
      (let ((gt (ignore-errors (gptel-get-tool "ptc_execution"))))
        (when (macher-agent-tool-valid-p gt) gt)))
     (t nil))))

(defun macher-agent-sandbox--inject-preset-ptc-tool (state)
  "Extract PTC primitives from STATE presets and inject `ptc_execution'."
  (cl-check-type state list)
  (if (plist-get state :disable-local-tools)
      (progn
        (setq state (plist-put state :ptc-primitives nil))
        (setq state (plist-put state :tools nil))
        state)
    (dolist (item (plist-get state :resolved-items))
      (pcase item
        ((or `(preset ,_sym ,spec)
             `(exclusive-reset ,_sym ,spec))
         (when (plist-get spec :exclusive)
           (setq state (plist-put state :ptc-primitives nil)))
         (when-let* ((ptc-spec (plist-get spec :ptc-primitives)))
           (let ((current-ptc (plist-get state :ptc-primitives))
                 (ptc-list (cond
                            ((vectorp ptc-spec) (append ptc-spec nil))
                            ((listp ptc-spec) ptc-spec)
                            (t (list ptc-spec)))))
             (setq state (plist-put state :ptc-primitives
                                    (cl-delete-duplicates (append current-ptc ptc-list)
                                                          :test #'equal))))))))
    (let* ((state-prims (plist-get state :ptc-primitives))
           (active-prims (if (plist-member state :ptc-primitives)
                             state-prims
                           (bound-and-true-p macher-agent-sandbox-active-ptc-primitives)))
           (ctx (or (let ((c (plist-get state :context)))
                      (when (macher-agent-valid-context-p c) c))
                    (macher-agent-resolve-context (current-buffer)))))
      (when active-prims
        (let* ((tools (plist-get state :tools))
               (already-present (cl-some (lambda (tl)
                                           (let ((name (macher-agent-canonical-tool-name tl)))
                                             (and name (member name '("ptc_execution" "ptc-execution")))))
                                         tools))
               (ptc-tool (unless already-present
                           (macher-agent-sandbox--resolve-ptc-execution-tool ctx))))
          (when (and (not already-present) (macher-agent-tool-valid-p ptc-tool))
            (setq state (plist-put state :tools (append tools (list ptc-tool)))))))
      state)))

;;; Programmatic Tool Calling (PTC) Prompt Generation

(defun macher-agent-sandbox--build-tool-arg-type-spec (arg lisp-name lisp-arg-name)
  "Build type spec and deftype string for ARG.
Uses LISP-NAME and LISP-ARG-NAME to name the generated type."
  (let* ((arg-type (plist-get arg :type))
         (items (plist-get arg :items))
         (props (when items (plist-get items :properties))))
    (cond
     ((and (member arg-type '(array "array")) props)
      (let ((type-name (intern (format "%s-%s" lisp-name lisp-arg-name)))
            (prop-strs nil))
        (cl-loop for (p-key p-val) on props by #'cddr
                 for p-type = (plist-get p-val :type)
                 do (push (format ":%s %s"
                                  p-key
                                  (pcase p-type
                                    ((or 'string "string") 'string)
                                    ((or 'number "number") 'number)
                                    ((or 'boolean "boolean") 'boolean)
                                    ((or 'array "array") '(or null (vector string)))
                                    (_ 't)))
                          prop-strs))
        (cons (list 'vector type-name)
              (format "(cl-deftype %s () \n  \"Property list for %s.\"\n  '(plist %s))"
                      type-name lisp-arg-name
                      (string-join (nreverse prop-strs) " ")))))
     ((member arg-type '(array "array"))
      (cons '(or null (vector string)) nil))
     ((member arg-type '(object "object"))
      (cons 'plist nil))
     (t
      (cons (pcase arg-type
              ((or 'string "string") 'string)
              ((or 'number "number") 'number)
              ((or 'boolean "boolean") 'boolean)
              (_ 't))
            nil)))))

(defun macher-agent-sandbox--format-tool-defun (name lisp-name desc arg-names arg-types return-type dummy-return deftypes)
  "Format tool defun string for NAME as LISP-NAME.
Includes DESC, ARG-NAMES, ARG-TYPES, RETURN-TYPE, DUMMY-RETURN, and
optional DEFTYPES."
  (cl-check-type name string)
  (cl-check-type lisp-name symbol)
  (cl-check-type desc (or string null))
  (cl-check-type arg-names list)
  (cl-check-type arg-types list)
  (cl-check-type return-type (or symbol list))
  (cl-check-type dummy-return (or string null))
  (cl-check-type deftypes list)
  (let* ((formatted-arg-types (mapcar (lambda (t-spec)
                                        (if (symbolp t-spec)
                                            (symbol-name t-spec)
                                          (prin1-to-string t-spec)))
                                      (nreverse (copy-sequence arg-types))))
         (ret-type-str (if (symbolp return-type)
                           (symbol-name return-type)
                         (prin1-to-string return-type)))
         (defun-str
          (format "%s -> (defun %s (%s)\n  %S\n  (declare (type (function (%s) %s)))\n  %s)"
                  name
                  lisp-name
                  (string-join (nreverse (copy-sequence arg-names)) " ")
                  (or desc "")
                  (string-join formatted-arg-types " ")
                  ret-type-str
                  (or dummy-return ""))))
    (if deftypes
        (concat (string-join (nreverse deftypes) "\n\n") "\n\n" defun-str)
      defun-str)))

(defun macher-agent-sandbox--format-tool-lisp-docstring (tool-obj)
  "Infer declarative Lisp signature for TOOL-OBJ."
  (let* ((name (gptel-tool-name tool-obj))
         (lisp-name (intern (replace-regexp-in-string "_" "-" name)))
         (desc (gptel-tool-description tool-obj))
         (args (gptel-tool-args tool-obj))
         (deftypes nil)
         (arg-names nil)
         (arg-types nil)
         (is-delegate (or (string-match-p "delegate" name)
                          (string= name "execute_subagents")))
         (return-type (if is-delegate '(list string) 'string))
         (dummy-return (if is-delegate "(list \"\")" "\"\"")))
    (dolist (arg args)
      (let* ((raw-name (if (symbolp (plist-get arg :name))
                           (symbol-name (plist-get arg :name))
                         (plist-get arg :name)))
             (lisp-arg-name (replace-regexp-in-string "_" "-" raw-name))
             (spec-pair (macher-agent-sandbox--build-tool-arg-type-spec arg lisp-name lisp-arg-name))
             (type-spec (car spec-pair))
             (deftype-str (cdr spec-pair)))
        (push lisp-arg-name arg-names)
        (when deftype-str
          (push deftype-str deftypes))
        (push type-spec arg-types)))
    (macher-agent-sandbox--format-tool-defun
     name lisp-name desc arg-names arg-types return-type dummy-return deftypes)))

(defun macher-agent-sandbox--filter-ptc-matching-tools (tools active)
  "Filter TOOLS matching any active primitive in ACTIVE."
  (cl-loop for t-obj in tools
           when (and (gptel-tool-p t-obj)
                     (cl-some
                      (lambda (prim)
                        (string=
                         (replace-regexp-in-string "_" "-"
                                                   (if (symbolp prim)
                                                       (symbol-name prim)
                                                     prim))
                         (replace-regexp-in-string "_" "-" (gptel-tool-name t-obj))))
                      active))
           collect t-obj))

(defun macher-agent-sandbox--tools-require-complex-types-p (tools)
  "Check if any of the TOOLS require complex object or array arguments."
  (cl-some (lambda (tool)
             (cl-some (lambda (arg)
                        (let ((type (plist-get arg :type)))
                          (member type '(array "array" object "object"))))
                      (gptel-tool-args tool)))
           tools))

(defun macher-agent-sandbox--inject-ptc-prompt (existing-prompt &optional active-primitives available-tools)
  "Append Programmatic Tool Calling instructions to EXISTING-PROMPT.
Uses ACTIVE-PRIMITIVES and AVAILABLE-TOOLS when non-nil."
  (let* ((active (or active-primitives (bound-and-true-p macher-agent-sandbox-active-ptc-primitives)))
         (tools (or available-tools (bound-and-true-p gptel-tools)))
         (matching-tools (macher-agent-sandbox--filter-ptc-matching-tools tools active)))
    (if (null matching-tools)
        existing-prompt
      (concat
       (if-let* ((prompt existing-prompt)) prompt "")
       "\n\n=== PROGRAMMATIC TOOL CALLING (PTC) ===\n"
       "You are equipped with a `ptc_execution` tool. Instead of outputting multiple sequential \\\n"
       "JSON tool calls, you MUST use `ptc_execution` to orchestrate complex tasks via an Emacs \\\n"
       "Lisp script.\n\n"
       "lisp tool aliases:\n"
       (string-join
        (mapcar #'macher-agent-sandbox--format-tool-lisp-docstring matching-tools) "\n\n")
       (if (macher-agent-sandbox--tools-require-complex-types-p matching-tools)
           "\n\n;; JSON TO EMACS LISP MAPPING RULES:\n;; 1. JSON Objects MUST translate to flat property lists using keywords (for example, `(:key \"value\")`). DO NOT use alists or dotted pairs.\n;; 2. JSON Arrays MUST translate to Lisp vectors using the `vector` function.\n;; Syntax example for complex arguments: `(tool-name (vector (list :buffer_name \"agent\" :instructions \"Task\")))`"
         "")
       "\n\n;; EXPECTED SCRIPT RETURN TYPE:\n"
       "Keep everything within a let*. Return the output object without formatting.\n"))))

;;; PTC Execution Handlers

(defun macher-agent-sandbox--spoof-tool-ui (target-buf tool-name)
  "Simulate tool UI execution for TOOL-NAME symbol in TARGET-BUF."
  (cl-check-type target-buf buffer)
  (cl-check-type tool-name symbol)
  (let ((name-str (replace-regexp-in-string "_" "-" (symbol-name tool-name))))
    (macher-agent-notify-tool-status target-buf (format "PTC: %s" name-str))))

(defun macher-agent-sandbox--display-ptc-tool-execution (target-buf tool-name args &optional context)
  "Update tool execution UI on live TARGET-BUF for TOOL-NAME.
Runs pre-tool hooks with ARGS and optional CONTEXT."
  (cl-check-type target-buf buffer)
  (let* ((target-lisp-name (if (symbolp tool-name) (symbol-name tool-name) (format "%s" tool-name)))
         (original-name-str (replace-regexp-in-string "-" "_" target-lisp-name))
         (hyphen-name (replace-regexp-in-string "_" "-" target-lisp-name))
         (ctx (or (and (macher-agent-valid-context-p context) context)
                  (macher-agent-resolve-context target-buf)))
         (tool-var (intern-soft (format "macher-agent-%s-tool" hyphen-name)))
         (resolved-tool (or (let ((r1 (macher-agent-resolve-tool original-name-str nil nil ctx)))
                              (when (macher-agent-tool-valid-p r1) r1))
                            (let ((r2 (macher-agent-resolve-tool hyphen-name nil nil ctx)))
                              (when (macher-agent-tool-valid-p r2) r2))
                            (when (buffer-live-p target-buf)
                              (cl-find-if
                               (lambda (tl)
                                 (and (macher-agent-tool-valid-p tl)
                                      (member (gptel-tool-name tl)
                                              (list original-name-str hyphen-name target-lisp-name))))
                               (buffer-local-value 'gptel-tools target-buf)))
                            (when (and tool-var (boundp tool-var)
                                       (macher-agent-tool-valid-p (symbol-value tool-var)))
                              (symbol-value tool-var))))
         (block-reason nil))
    (when (bound-and-true-p gptel-pre-tool-call-functions)
      (run-hook-wrapped
       'gptel-pre-tool-call-functions
       (lambda (f)
         (let ((res (ignore-errors
                      (funcall f (list :name original-name-str :args args
                                       :buffer (buffer-name target-buf)
                                       :model (bound-and-true-p gptel-model))))))
           (when (and res (listp res) (plist-get res :block))
             (setq block-reason (plist-get res :block))
             t)))))
    (if block-reason
        (error "%s" block-reason)
      (message "PTC Executing: %s" original-name-str)
      (macher-agent-sandbox--spoof-tool-ui target-buf (intern hyphen-name))
      (cons original-name-str resolved-tool))))

(defun macher-agent-sandbox--format-ptc-result-block (truncated-call call-str str-res)
  "Format PTC result block for display in buffer.
Includes TRUNCATED-CALL header, CALL-STR, and STR-RES output."
  (if (derived-mode-p 'org-mode)
      (format "\n#+begin_tool %s\n%s\n\n%s\n#+end_tool\n"
              truncated-call call-str str-res)
    (format "\n``` tool %s\n%s\n\n%s\n```\n"
            truncated-call call-str str-res)))

(defun macher-agent-sandbox--cycle-ptc-result-block ()
  "Cycle visibility of the PTC result block at point."
  (ignore-errors
    (if (derived-mode-p 'org-mode)
        (org-cycle)
      (when (fboundp 'gptel-markdown-cycle-block)
        (gptel-markdown-cycle-block)))))

(defun macher-agent-sandbox--insert-ptc-tool-result (target-buf call-str res-data original-name-str &optional resolved-tool)
  "Insert RES-DATA for CALL-STR and ORIGINAL-NAME-STR into TARGET-BUF.
Respects buffer-local `gptel-include-tool-results' and optional
RESOLVED-TOOL `:include' policy."
  (cl-check-type res-data string)
  (when (bound-and-true-p gptel-post-tool-call-functions)
    (run-hook-wrapped
     'gptel-post-tool-call-functions
     (lambda (f)
       (ignore-errors
         (funcall f (list :name original-name-str :call call-str :result res-data
                          :buffer (buffer-name target-buf) :model (bound-and-true-p gptel-model)))))))
  (when (and (bufferp target-buf)
             (buffer-live-p target-buf))
    (let* ((inc-mode (with-current-buffer target-buf
                       (bound-and-true-p gptel-include-tool-results)))
           (tool-inc (if (and resolved-tool (macher-agent-tool-valid-p resolved-tool))
                         (gptel-tool-include resolved-tool)
                       t))
           (eff-inc (if (eq inc-mode 'auto) tool-inc inc-mode)))
      (when eff-inc
        (with-current-buffer target-buf
          (save-excursion
            (let* ((inhibit-read-only t)
                   (fsm (or (bound-and-true-p macher-agent-active-fsm)
                            (macher-agent-get-active-fsm)))
                   (info (and fsm (fboundp 'gptel-fsm-info)
                              (ignore-errors (gptel-fsm-info fsm))))
                   (pos-m (and (listp info) (plist-get info :position)))
                   (track-m (and (listp info) (plist-get info :tracking-marker)))
                   (owns-fsm (and (listp info)
                                  (or (eq (plist-get info :buffer) target-buf)
                                      (and (markerp pos-m) (eq (marker-buffer pos-m) target-buf))
                                      (and (markerp track-m) (eq (marker-buffer track-m) target-buf))))))
              (cond
               ((and owns-fsm (markerp track-m) (eq (marker-buffer track-m) target-buf))
                (set-marker-insertion-type track-m t)
                (goto-char track-m))
               ((and owns-fsm (markerp pos-m) (eq (marker-buffer pos-m) target-buf))
                (goto-char pos-m)
                (setq track-m (set-marker (make-marker) (point) target-buf))
                (set-marker-insertion-type track-m t)
                (setq info (plist-put info :tracking-marker track-m))
                (macher-agent-set-fsm-info fsm info))
               (t
                (goto-char (point-max))))
              (let ((truncated-call (truncate-string-to-width call-str 60 nil nil " ...)"))
                    (display-res (if (eq eff-inc 'call)
                                     "(Cached tool result — available during original generation)"
                                   res-data))
                    (start-pt (point)))
                (insert (macher-agent-sandbox--format-ptc-result-block truncated-call call-str display-res))
                (add-text-properties start-pt (point) '(gptel ignore macher-agent-ptc-log t))
                (when (and owns-fsm (markerp track-m) (eq (marker-buffer track-m) target-buf))
                  (let ((tool-m (plist-get info :tool-marker)))
                    (if (and (markerp tool-m) (eq (marker-buffer tool-m) target-buf))
                        (move-marker tool-m track-m)
                      (setq info (plist-put info :tool-marker (copy-marker track-m nil)))
                      (macher-agent-set-fsm-info fsm info))))
                (goto-char start-pt)
                (forward-line 1)
                (macher-agent-sandbox--cycle-ptc-result-block)))))))))

(defun macher-agent-sandbox--coerce-arg-for-spec (val arg-spec)
  "Coerce VAL to a vector when ARG-SPEC declares `:type \"array\"'."
  (let ((arg-type (plist-get arg-spec :type)))
    (if (and (member arg-type '(array "array"))
             (listp val)
             (not (null val)))
        (vconcat val)
      val)))

(defun macher-agent-sandbox--resolve-positional-args (resolved-tool user-args)
  "Convert USER-ARGS into positional arguments using RESOLVED-TOOL `:args'."
  (let ((arg-specs (gptel-tool-args resolved-tool)))
    (if (and (consp user-args) (keywordp (car user-args)))
        (let ((mapped
               (mapcar (lambda (arg-spec)
                         (let* ((raw-name (plist-get arg-spec :name))
                                (name-str (if (symbolp raw-name) (symbol-name raw-name) raw-name))
                                (under-kw (intern (concat ":" (replace-regexp-in-string "-" "_" name-str))))
                                (hyphen-kw (intern (concat ":" (replace-regexp-in-string "_" "-" name-str))))
                                (raw-val (if (plist-member user-args under-kw)
                                             (plist-get user-args under-kw)
                                           (plist-get user-args hyphen-kw))))
                           (macher-agent-sandbox--coerce-arg-for-spec raw-val arg-spec)))
                       arg-specs)))
          (reverse (seq-drop-while #'null (reverse mapped))))
      (cl-loop for val in user-args
               for tail on arg-specs
               collect (macher-agent-sandbox--coerce-arg-for-spec val (car tail)) into coerced
               finally return (append coerced (nthcdr (length coerced) user-args))))))

(defun macher-agent-sandbox--dispatch-ptc-primitive (context target-buf tool-lisp-sym resolved-tool user-args ptc-callback &optional on-error stop-iter-fn)
  "Dispatch PTC primitive TOOL-LISP-SYM in CONTEXT and TARGET-BUF.
Calls RESOLVED-TOOL with USER-ARGS via `ptc-function' or `gptel-tool',
passing the result to PTC-CALLBACK, or reporting errors via ON-ERROR and
STOP-ITER-FN."
  (cl-check-type context macher-agent-context)
  (cl-check-type target-buf buffer)
  (cl-check-type tool-lisp-sym symbol)
  (cl-check-type resolved-tool gptel-tool)
  (let* ((clean-name (gptel-tool-name resolved-tool))
         (hyphen-name (replace-regexp-in-string "_" "-" clean-name))
         (under-name (replace-regexp-in-string "-" "_" clean-name))
         (tool-var-hyphen (intern-soft (format "macher-agent-%s-tool" hyphen-name)))
         (tool-var-under (intern-soft (format "macher-agent-%s-tool" under-name)))
         (ws-override-entry (and (hash-table-p macher-agent-sandbox-workspace-tool-overrides)
                                 (gethash resolved-tool macher-agent-sandbox-workspace-tool-overrides)))
         (ptc-fn (if ws-override-entry
                     (when (functionp ws-override-entry) ws-override-entry)
                   (or (get tool-lisp-sym 'ptc-function)
                       (get tool-lisp-sym 'command-fn)
                       (when tool-var-hyphen (get tool-var-hyphen 'ptc-function))
                       (when tool-var-under (get tool-var-under 'ptc-function))
                       (get (intern-soft clean-name) 'ptc-function)
                       (when (intern-soft hyphen-name) (get (intern-soft hyphen-name) 'ptc-function))
                       (when (intern-soft under-name) (get (intern-soft under-name) 'ptc-function)))))
         (pos-args (macher-agent-sandbox--resolve-positional-args resolved-tool user-args))
         (run-in-target
          (lambda (fn)
            (with-current-buffer target-buf
              (setq-local macher-agent-persistent-context context)
              (funcall fn)))))
    (macher-agent-log-tool-intent context "ptc" clean-name user-args)
    (cond
     (ptc-fn
      (funcall run-in-target
               (lambda ()
                 (condition-case cmd-err
                     (let ((res (apply ptc-fn pos-args)))
                       (if (functionp res)
                           (funcall res ptc-callback)
                         (funcall ptc-callback res)))
                   (error
                    (if stop-iter-fn
                        (funcall stop-iter-fn (error-message-string cmd-err))
                      (when (functionp on-error)
                        (funcall on-error (list :status 'error :error (error-message-string cmd-err))))))))))
     ((gptel-tool-function resolved-tool)
      (funcall run-in-target
               (lambda ()
                 (condition-case tool-err
                     (let* ((fn (gptel-tool-function resolved-tool))
                            (is-async (gptel-tool-async resolved-tool))
                            (call-args (if is-async
                                           (cons (lambda (res &optional err)
                                                   (if err
                                                       (if stop-iter-fn
                                                           (funcall stop-iter-fn err)
                                                         (when (functionp on-error) (funcall on-error err)))
                                                     (funcall ptc-callback res)))
                                                 pos-args)
                                         pos-args)))
                       (if is-async
                           (apply fn call-args)
                         (funcall ptc-callback (apply fn call-args))))
                   (wrong-number-of-arguments
                    (let* ((fn (gptel-tool-function resolved-tool))
                           (call-args (cons (lambda (res &optional err)
                                              (if err
                                                  (if stop-iter-fn
                                                      (funcall stop-iter-fn err)
                                                    (when (functionp on-error) (funcall on-error err)))
                                                (funcall ptc-callback res)))
                                            pos-args)))
                      (apply fn context call-args)))
                   (error
                    (if stop-iter-fn
                        (funcall stop-iter-fn (error-message-string tool-err))
                      (when (functionp on-error)
                        (funcall on-error (list :status 'error :error (error-message-string tool-err))))))))))
     (t
      (when stop-iter-fn (funcall stop-iter-fn "Tool not accessible"))
      (when (functionp on-error)
        (funcall on-error
                 (list :status 'error
                       :error (format "ERROR: Tool '%s' is not accessible in this context or is no longer available. Please select another tool or approach." clean-name))))))))

(defun macher-agent-sandbox--ptc-handle-yielded-value (yielded-val context target-buf ptc-callback &optional on-error stop-iter-fn)
  "Dispatch YIELDED-VAL tool call in CONTEXT and TARGET-BUF.
Resumes via PTC-CALLBACK or reports failures to ON-ERROR and
STOP-ITER-FN."
  (cl-check-type context macher-agent-context)
  (cl-check-type target-buf buffer)
  (if (or (macher-agent-tool-call-p yielded-val) (listp yielded-val))
      (let* ((tool-lisp-sym (if (macher-agent-tool-call-p yielded-val)
                                (macher-agent-tool-call-name yielded-val)
                              (car yielded-val)))
             (args (if (macher-agent-tool-call-p yielded-val)
                       (macher-agent-tool-call-args yielded-val)
                     (cdr yielded-val)))
             (tool-info (macher-agent-sandbox--display-ptc-tool-execution target-buf tool-lisp-sym args context))
             (original-name-str (car tool-info))
             (resolved-tool (cdr tool-info))
             (call-str (prin1-to-string `(:name ,original-name-str :args ,args)))
             (cb (lambda (res-data)
                   (let ((str-res (if (stringp res-data) res-data (prin1-to-string res-data))))
                     (macher-agent-sandbox--insert-ptc-tool-result
                      target-buf call-str str-res original-name-str resolved-tool)
                     (funcall ptc-callback res-data)))))
        (if (and resolved-tool (macher-agent-tool-valid-p resolved-tool))
            (macher-agent-sandbox--dispatch-ptc-primitive
             context target-buf tool-lisp-sym resolved-tool args
             cb on-error stop-iter-fn)
          (let ((reason (format "ERROR: Tool '%s' is not accessible in this context or is no longer available." original-name-str)))
            (when stop-iter-fn (funcall stop-iter-fn reason))
            (if (functionp on-error)
                (funcall on-error (list :status 'error :error reason))
              (funcall cb (list :status 'error :error reason))))))
    (let ((reason (format "Unexpected yield from PTC sandbox: %S" yielded-val)))
      (when stop-iter-fn (funcall stop-iter-fn reason))
      (when (functionp on-error)
        (funcall on-error (list :status 'error :error reason))))))

(defun macher-agent-sandbox--ptc-resume-step (iterator resolved-ctx resolved-buf active-ptc-prims on-success on-error stop-iter-fn input)
  "Advance PTC ITERATOR iteratively via a trampoline starting with INPUT.
Runs inside RESOLVED-BUF with RESOLVED-CTX and ACTIVE-PTC-PRIMS,
routing outcomes to ON-SUCCESS, ON-ERROR, and STOP-ITER-FN without
growing the Elisp call stack across synchronous yields or tool calls."
  (let ((macher-agent-sandbox-active-ptc-execution t)
        (macher-agent-sandbox-active-ptc-primitives active-ptc-prims)
        (cur-input input)
        (running t))
    (while running
      (setq running nil)
      (condition-case iter-err
          (let ((step (with-current-buffer resolved-buf
                        (iter-next iterator cur-input))))
            (if (not (macher-agent-tool-call-p step))
                (setq cur-input step
                      running t)
              (let ((in-dispatch t)
                    (sync-resolved nil)
                    (sync-val nil)
                    (resumed nil))
                (let ((resume-cb
                       (lambda (next-input)
                         (unless resumed
                           (setq resumed t)
                           (if in-dispatch
                               (setq sync-resolved t
                                     sync-val next-input)
                             (macher-agent-sandbox--ptc-resume-step
                              iterator resolved-ctx resolved-buf active-ptc-prims
                              on-success on-error stop-iter-fn next-input))))))
                  (macher-agent-sandbox--ptc-handle-yielded-value
                   step resolved-ctx resolved-buf resume-cb on-error stop-iter-fn)
                  (setq in-dispatch nil)
                  (when sync-resolved
                    (setq cur-input sync-val
                          running t))))))
        (iter-end-of-sequence
         (when (functionp on-success)
           (if (buffer-live-p resolved-buf)
               (with-current-buffer resolved-buf
                 (funcall on-success (cdr iter-err)))
             (funcall on-success (cdr iter-err)))))
        (error
         (when (functionp on-error)
           (if (buffer-live-p resolved-buf)
               (with-current-buffer resolved-buf
                 (funcall on-error (list :status 'error :error (error-message-string iter-err))))
             (funcall on-error (list :status 'error :error (error-message-string iter-err))))))))))

(defun macher-agent-sandbox--ptc-resume-loop-start (iterator resolved-ctx resolved-buf active-ptc-prims on-success on-error stop-iter-fn)
  "Start PTC execution loop for ITERATOR with ACTIVE-PTC-PRIMS.
Evaluates inside RESOLVED-BUF with RESOLVED-CTX, routing outcomes to
ON-SUCCESS, ON-ERROR, and STOP-ITER-FN."
  (macher-agent-sandbox--ptc-resume-step
   iterator resolved-ctx resolved-buf active-ptc-prims
   on-success on-error stop-iter-fn nil))

(defun macher-agent-sandbox-execute-ptc-script (script context target-buf on-success on-error &optional extra-operations)
  "Execute PTC SCRIPT using CONTEXT in TARGET-BUF.
Invokes ON-SUCCESS or ON-ERROR, optionally allowing EXTRA-OPERATIONS."
  (cl-check-type script string)
  (cl-check-type target-buf (or buffer string null))
  (cl-check-type on-success (or function null))
  (cl-check-type on-error (or function null))
  (cl-check-type extra-operations (or hash-table list null))
  (let* ((caller-buf (current-buffer))
         (caller-prims (bound-and-true-p macher-agent-sandbox-active-ptc-primitives))
         (resolved-buf (cond
                        ((bufferp target-buf) target-buf)
                        ((stringp target-buf) (get-buffer-create target-buf))
                        (t caller-buf)))
         (resolved-ctx (or (macher-agent-resolve-context context)
                           (macher-agent-resolve-context resolved-buf)
                           (macher-agent-make-context
                            :id "ptc-ctx"
                            :project-root (macher-agent-root default-directory)
                            :origin-buffer resolved-buf
                            :tools (make-hash-table :test 'equal))))
         (ctx-prims (let ((sb (macher-agent-sandbox-get-state
                               resolved-ctx
                               (when (buffer-live-p resolved-buf)
                                 (buffer-name resolved-buf)))))
                      (when (listp sb) (plist-get sb :ptc-primitives))))
         (buf-prims (with-current-buffer resolved-buf
                      (bound-and-true-p macher-agent-sandbox-active-ptc-primitives)))
         (prims (append
                 '(nreverse sort delete delq nconc plist-put aset puthash remhash error signal message random emacs-version)
                 extra-operations))
         (active-ptc-prims
          (delete-dups (append extra-operations
                               ctx-prims
                               buf-prims
                               (when (or (eq caller-buf resolved-buf)
                                         (and (null extra-operations)
                                              (null ctx-prims)
                                              (null buf-prims)))
                                 caller-prims))))
         (macher-agent-sandbox-active-ptc-execution t))
    (cl-check-type resolved-ctx macher-agent-context)
    (condition-case err
        (progn
          (with-current-buffer resolved-buf
            (setq-local macher-agent-persistent-context resolved-ctx)
            (setq macher-agent-sandbox--globals (make-hash-table :test 'eq)
                  macher-agent-sandbox--primitives (make-hash-table :test 'eq)
                  macher-agent-sandbox--functions (make-hash-table :test 'eq))
            (macher-agent-sandbox--init prims))
          (let* ((ast (macroexpand-all (read (format "(progn\n%s\n)" script))))
                 (iterator (with-current-buffer resolved-buf
                             (macher-agent-sandbox--eval-iter ast nil)))
                 (stop-iter-fn (lambda (reason)
                                 (when (functionp on-error)
                                   (funcall on-error (list :status 'error :error reason))))))
            (macher-agent-sandbox--ptc-resume-loop-start
             iterator resolved-ctx resolved-buf active-ptc-prims
             on-success on-error stop-iter-fn)))
      (error
       (when (functionp on-error)
         (funcall on-error (list :status 'error :error (error-message-string err))))))))

(defun macher-agent-sandbox-run (expression extra-operations context target-buf)
  "Execute Lisp EXPRESSION in sandbox with EXTRA-OPERATIONS.
Evaluates inside TARGET-BUF using CONTEXT."
  (let* ((target-name (if (and (bufferp target-buf) (buffer-live-p target-buf))
                          (buffer-name target-buf)
                        (buffer-name (current-buffer))))
         (caller-prims (or (when (macher-agent-valid-context-p context)
                             (let ((sb (macher-agent-sandbox-get-state context target-name)))
                               (when (listp sb) (plist-get sb :ptc-primitives))))
                           (bound-and-true-p macher-agent-sandbox-active-ptc-primitives)))
         (run-eval
          (lambda ()
            (let ((macher-agent-sandbox-active-ptc-primitives
                   (or (bound-and-true-p macher-agent-sandbox-active-ptc-primitives)
                       caller-prims))
                  (macher-agent-sandbox--primitives (make-hash-table :test 'eq))
                  (macher-agent-sandbox--functions (make-hash-table :test 'eq))
                  (macher-agent-sandbox--globals (make-hash-table :test 'eq)))
              (macher-agent-sandbox--init extra-operations)
              (let* ((iterator (macher-agent-sandbox--eval-iter (macroexpand-all expression) nil))
                     (yield-val nil)
                     (next-yield nil))
                (condition-case err
                    (while t
                      (setq next-yield (iter-next iterator yield-val))
                      (let ((tc (cond
                                 ((macher-agent-tool-call-p next-yield) next-yield)
                                 ((and (listp next-yield)
                                       (or (eq (plist-get next-yield :interrupt) 'tool-call)
                                           (plist-get next-yield :name)))
                                  (make-macher-agent-tool-call
                                   :name (or (plist-get next-yield :name) (plist-get next-yield :target))
                                   :args (or (plist-get next-yield :args) (plist-get next-yield :payload))))
                                 (t nil))))
                        (when (and tc (macher-agent-tool-call-name tc))
                          (let ((target (macher-agent-tool-call-name tc))
                                (args (macher-agent-tool-call-args tc)))
                            (when (and context target)
                              (macher-agent-log-tool-intent context "ptc" target args)))))
                      (setq yield-val next-yield))
                  (iter-end-of-sequence (cdr err))))))))
    (if (and (bufferp target-buf) (buffer-live-p target-buf))
        (with-current-buffer target-buf
          (funcall run-eval))
      (funcall run-eval))))

(defun macher-agent-sandbox--normalize-ptc-list (ptc-spec)
  "Normalise PTC-SPEC into a list of primitive symbols or names."
  (cond
   ((null ptc-spec) nil)
   ((vectorp ptc-spec) (append ptc-spec nil))
   ((listp ptc-spec) ptc-spec)
   (t (list ptc-spec))))

(defun macher-agent-sandbox--flatten-skill-specs (items ctx target-buf known)
  "Flatten skill/preset ITEMS and `:parents' chains for PTC resolution.
Looks up symbols via `macher-agent-lookup-skill-spec' using CTX,
TARGET-BUF, and KNOWN without passing `strict-known' when KNOWN is nil."
  (let ((result nil)
        (strict (and known t)))
    (cl-labels
        ((walk (item ancestors)
           (cond
            ((null item) nil)
            ((and (consp item) (keywordp (car item)))
             (let ((parents (plist-get item :parents))
                   (exclusive (plist-get item :exclusive)))
               (if (and exclusive parents)
                   (progn
                     (push (list :exclusive t) result)
                     (dolist (parent (if (listp parents) parents (list parents)))
                       (walk parent ancestors))
                     (let ((clean (copy-sequence item)))
                       (setq clean (plist-put clean :exclusive nil))
                       (push clean result)))
                 (when parents
                   (dolist (parent (if (listp parents) parents (list parents)))
                     (walk parent ancestors)))
                 (push item result))))
            (t
             (let ((sym (macher-agent-normalise-preset-name item)))
               (when (and sym (not (memq sym ancestors)))
                 (let ((next-ancestors (cons sym ancestors)))
                   (when-let* ((spec (macher-agent-lookup-skill-spec
                                      sym ctx target-buf known strict)))
                     (let ((parents (plist-get spec :parents))
                           (exclusive (plist-get spec :exclusive)))
                       (if (and exclusive parents)
                           (progn
                             (push (list :exclusive t) result)
                             (dolist (parent (if (listp parents) parents (list parents)))
                               (walk parent next-ancestors))
                             (let ((clean (copy-sequence spec)))
                               (setq clean (plist-put clean :exclusive nil))
                               (push clean result)))
                         (when parents
                           (dolist (parent (if (listp parents) parents (list parents)))
                             (walk parent next-ancestors)))
                         (push spec result)))))))))))
      (dolist (item items)
        (walk item nil))
      (nreverse result))))

(defun macher-agent-sandbox--resolve-transmission-prims (state)
  "Resolve active PTC primitives from STATE, target buffer, skills, or context."
  (let* ((target-buf (macher-agent-transmission-state-target-buffer state))
         (target-buf-name (when (and target-buf (buffer-live-p target-buf))
                            (buffer-name target-buf)))
         (ctx (or (macher-agent-transmission-state-context state)
                  (macher-agent-resolve-context target-buf)))
         (known (macher-agent-transmission-state-known-presets state))
         (skills (macher-agent-transmission-state-skills state))
         (redirected (macher-agent-transmission-state-redirected-skill state))
         (tx-plugins (macher-agent-transmission-state-plugins state))
         (tx-sandbox (when (macher-agent-plist-p tx-plugins)
                       (plist-get tx-plugins :sandbox)))
         (tx-prims (when (macher-agent-plist-p tx-sandbox)
                     (plist-get tx-sandbox :ptc-primitives)))
         (buf-prims (when (and target-buf (buffer-live-p target-buf))
                      (with-current-buffer target-buf
                        (bound-and-true-p macher-agent-sandbox-active-ptc-primitives))))
         (ctx-prims (when (macher-agent-valid-context-p ctx)
                      (let ((sb (macher-agent-sandbox-get-state ctx target-buf-name)))
                        (when (macher-agent-plist-p sb) (plist-get sb :ptc-primitives)))))
         (skill-prims nil)
         (apply-specs
          (lambda (specs)
            (dolist (spec specs)
              (when (plist-get spec :exclusive)
                (setq tx-prims nil
                      buf-prims nil
                      ctx-prims nil
                      skill-prims nil))
              (if (plist-get spec :disable-local-tools)
                  (setq tx-prims nil
                        buf-prims nil
                        ctx-prims nil
                        skill-prims nil)
                (when-let* ((p (plist-get spec :ptc-primitives)))
                  (setq skill-prims
                        (append skill-prims
                                (macher-agent-sandbox--normalize-ptc-list p)))))))))
    (if skills
        (funcall apply-specs
                 (macher-agent-sandbox--flatten-skill-specs skills ctx target-buf known))
      (when-let* ((active-preset (when (and target-buf (buffer-live-p target-buf))
                                   (macher-agent-current-gptel-preset target-buf)))
                  (active-spec (macher-agent-lookup-skill-spec
                                active-preset ctx target-buf known (and known t)))
                  ((not (plist-get active-spec :is-command))))
        (funcall apply-specs
                 (macher-agent-sandbox--flatten-skill-specs
                  (list active-preset) ctx target-buf known))))
    (when redirected
      (funcall apply-specs
               (macher-agent-sandbox--flatten-skill-specs
                (list redirected) ctx target-buf known)))
    (cl-delete-duplicates
     (append tx-prims
             skill-prims
             buf-prims
             ctx-prims)
     :test #'equal)))

(defun macher-agent-sandbox--sync-fsm-ptc-primitives (target-buf prims)
  "Sync PRIMS onto the active FSM info when it owns TARGET-BUF."
  (when-let* ((fsm (macher-agent-get-active-fsm)))
    (when (fboundp 'gptel-fsm-info)
      (let* ((info (gptel-fsm-info fsm))
             (fsm-buf (when (macher-agent-plist-p info) (plist-get info :buffer))))
        (when (and (macher-agent-plist-p info)
                   (or (null target-buf) (null fsm-buf) (eq fsm-buf target-buf)))
          (macher-agent-set-fsm-info
           fsm (plist-put (copy-sequence info) :ptc-primitives prims)))))))

(defun macher-agent-sandbox-append-ptc-to-transmission (state)
  "Hydrate PTC primitives and append PTC instructions to STATE."
  (cl-check-type state macher-agent-transmission-state)
  (let ((target-buf (macher-agent-transmission-state-target-buffer state)))
    (if (macher-agent-transmission-state-disable-local-tools state)
        (progn
          (let* ((tx-plugins (copy-sequence (macher-agent-transmission-state-plugins state)))
                 (sb (when (macher-agent-plist-p tx-plugins)
                       (plist-get tx-plugins :sandbox))))
            (when (and (macher-agent-plist-p sb) (plist-member sb :ptc-primitives))
              (setf (macher-agent-transmission-state-plugins state)
                    (plist-put tx-plugins :sandbox
                               (plist-put (copy-sequence sb) :ptc-primitives nil)))))
          (setf (macher-agent-transmission-state-tools state) nil)
          (macher-agent-sandbox--sync-fsm-ptc-primitives target-buf nil))
      (let ((prims (macher-agent-sandbox--resolve-transmission-prims state))
            (ctx (or (macher-agent-transmission-state-context state)
                     (macher-agent-resolve-context target-buf))))
        (let ((tx-plugins (copy-sequence (macher-agent-transmission-state-plugins state))))
          (setf (macher-agent-transmission-state-plugins state)
                (plist-put tx-plugins :sandbox (list :ptc-primitives prims))))
        (macher-agent-sandbox--sync-fsm-ptc-primitives target-buf prims)
        (when prims
          (let* ((tools (macher-agent-transmission-state-tools state))
                 (already-present (cl-some (lambda (tl)
                                             (let ((name (macher-agent-canonical-tool-name tl)))
                                               (and name (member name '("ptc_execution" "ptc-execution")))))
                                           tools))
                 (ptc-tool (unless already-present
                             (macher-agent-sandbox--resolve-ptc-execution-tool ctx))))
            (when (and (not already-present) (macher-agent-tool-valid-p ptc-tool))
              (setq tools (append tools (list ptc-tool)))
              (setf (macher-agent-transmission-state-tools state) tools))
            (let ((compiled-ptc-block (macher-agent-sandbox--inject-ptc-prompt "" prims tools)))
              (unless (string-empty-p compiled-ptc-block)
                (push (string-trim compiled-ptc-block)
                      (macher-agent-transmission-state-directives state))))))))
    state))

;;;###autoload
(cl-defmacro macher-agent-sandbox-execute-detached-ptc ((&key (buffer-name "*workspace-root*")
                                                              (reap t)
                                                              primitives
                                                              presets
                                                              on-success
                                                              on-error)
                                                        &rest script-body)
  "Execute SCRIPT-BODY as a sandboxed PTC script in a detached buffer.
BUFFER-NAME is the target workspace buffer (default \"*workspace-root*\").
When REAP is non-nil (default t), kill the buffer after completion.
PRIMITIVES lists allowed tool names, or nil to inherit from PRESETS.
PRESETS is a preset or list of presets to apply to the detached buffer.
ON-SUCCESS and ON-ERROR are single-argument callbacks."
  (declare (indent 1) (debug t))
  `(let ((root-buf (get-buffer-create ,buffer-name))
         (allowed-primitives ,primitives)
         (active-presets ,presets)
         (reap-p ,reap)
         (caller-buf (current-buffer)))
     (with-current-buffer root-buf
       (if (fboundp 'markdown-mode)
           (unless (eq major-mode 'markdown-mode)
             (markdown-mode))
         (unless (eq major-mode 'text-mode)
           (text-mode)))
       (unless (bound-and-true-p macher-agent-persistent-context)
         (setq-local macher-agent-persistent-context
                     (or (macher-agent-context-from-buffer caller-buf)
                         (macher-agent-make-context
                          :id (format "detached-%s" (macher-agent-generate-uuid))
                          :project-root default-directory))))
       (when (fboundp 'gptel-mode)
         (unless (bound-and-true-p gptel-mode)
           (gptel-mode 1)))
       (macher-agent-copy-gptel-session-vars caller-buf root-buf)
       (macher-agent-initialize-skills macher-agent-persistent-context)
       (when active-presets
         (setq-local macher-agent-presets (ensure-list active-presets))
         (macher-agent-apply-preset macher-agent-presets))
       (when allowed-primitives
         (setq-local macher-agent-sandbox-active-ptc-primitives allowed-primitives))
       (let ((script-str (prin1-to-string ',(if (> (length script-body) 1)
                                                `(progn ,@script-body)
                                              (car script-body))))
             (success-fn (or ,on-success (lambda (res) (message "PTC Success: %s" res))))
             (error-fn (or ,on-error (lambda (err) (message "PTC Error: %s" err))))
             (cleanup-fn (lambda ()
                           (when (and reap-p (buffer-live-p root-buf))
                             (macher-agent-reap-buffer root-buf)))))
         (macher-agent-sandbox-execute-ptc-script
          script-str
          macher-agent-persistent-context
          root-buf
          (lambda (res)
            (unwind-protect
                (funcall success-fn res)
              (funcall cleanup-fn)))
          (lambda (err)
            (unwind-protect
                (funcall error-fn err)
              (funcall cleanup-fn)))
          nil)))))

;;;; Lifecycle & Core Hook Handlers

(defun macher-agent-sandbox--transient-prompt-buffer-p (buf ctx)
  "Return non-nil when BUF is a transient prompt-copy buffer for CTX."
  (let* ((bname (and (bufferp buf) (buffer-live-p buf) (buffer-name buf)))
         (fsm (macher-agent-get-active-fsm))
         (fsm-info (when (and fsm (fboundp 'gptel-fsm-info))
                     (ignore-errors (gptel-fsm-info fsm))))
         (fsm-buf (when (listp fsm-info) (plist-get fsm-info :buffer)))
         (owner (when (macher-agent-valid-context-p ctx)
                  (macher-agent-context-owner-buffer ctx))))
    (or (and (stringp bname) (string-prefix-p " " bname))
        (and (bufferp fsm-buf) (buffer-live-p fsm-buf) (not (eq fsm-buf buf)))
        (and (bufferp owner) (buffer-live-p owner) (not (eq owner buf))
             (not (local-variable-p 'macher-agent-persistent-context buf))))))

(defun macher-agent-sandbox--on-apply-payload (payload)
  "Apply composed `:ptc-primitives' from PAYLOAD to current buffer."
  (when (listp payload)
    (let* ((cur-buf (current-buffer))
           (cur-name (buffer-name cur-buf))
           (ctx (macher-agent-resolve-context cur-buf))
           (persist-p (and (macher-agent-valid-context-p ctx)
                           (not (macher-agent-sandbox--transient-prompt-buffer-p cur-buf ctx)))))
      (cond
       ((plist-get payload :disable-local-tools)
        (setq-local macher-agent-sandbox-active-ptc-primitives nil)
        (when persist-p
          (let ((sb (copy-sequence (macher-agent-sandbox-get-state ctx cur-name))))
            (macher-agent-sandbox-set-state ctx (plist-put sb :ptc-primitives nil) cur-name))))
       ((plist-member payload :ptc-primitives)
        (let ((prims (plist-get payload :ptc-primitives)))
          (setq-local macher-agent-sandbox-active-ptc-primitives prims)
          (when persist-p
            (let ((sb (copy-sequence (macher-agent-sandbox-get-state ctx cur-name))))
              (macher-agent-sandbox-set-state ctx (plist-put sb :ptc-primitives prims) cur-name)))))))))

(defun macher-agent-sandbox--on-preset-cleared (buf ctx)
  "Clear buffer-local and CTX cached `:ptc-primitives' after preset reset.
Resets `macher-agent-sandbox-active-ptc-primitives' on live BUF."
  (let ((buf-name (when (and (bufferp buf) (buffer-live-p buf))
                    (buffer-name buf))))
    (when (and (bufferp buf) (buffer-live-p buf))
      (with-current-buffer buf
        (setq-local macher-agent-sandbox-active-ptc-primitives nil)))
    (when (macher-agent-valid-context-p ctx)
      (let ((sb (macher-agent-sandbox-get-state ctx buf-name)))
        (when (and (listp sb) (plist-member sb :ptc-primitives))
          (macher-agent-sandbox-set-state
           ctx (plist-put (copy-sequence sb) :ptc-primitives nil) buf-name))))))

(defun macher-agent-sandbox--on-skill-baseline-capture (target-buf)
  "Capture buffer-local PTC primitives on TARGET-BUF before skill switch."
  (when (and (bufferp target-buf) (buffer-live-p target-buf))
    (with-current-buffer target-buf
      (unless macher-agent-sandbox--cached-baseline-set
        (setq-local macher-agent-sandbox--cached-baseline-ptc-primitives
                    (copy-sequence macher-agent-sandbox-active-ptc-primitives))
        (setq-local macher-agent-sandbox--cached-baseline-set t)))))

(defun macher-agent-sandbox--on-skill-baseline-restore (target-buf)
  "Restore cached PTC primitives on TARGET-BUF after `use_skill' completes."
  (when (and (bufferp target-buf) (buffer-live-p target-buf))
    (with-current-buffer target-buf
      (when macher-agent-sandbox--cached-baseline-set
        (let ((restored (copy-sequence macher-agent-sandbox--cached-baseline-ptc-primitives))
              (tname (buffer-name target-buf)))
          (setq-local macher-agent-sandbox-active-ptc-primitives restored)
          (when-let* ((ctx (macher-agent-resolve-context target-buf))
                      ((macher-agent-valid-context-p ctx)))
            (let ((sb (copy-sequence (macher-agent-sandbox-get-state ctx tname))))
              (macher-agent-sandbox-set-state ctx (plist-put sb :ptc-primitives restored) tname)))
          (kill-local-variable 'macher-agent-sandbox--cached-baseline-ptc-primitives)
          (kill-local-variable 'macher-agent-sandbox--cached-baseline-set))))))

;;;###autoload
(defun macher-agent-sandbox-install ()
  "Install sandbox execution capabilities, hooks, and pipeline steps."
  (interactive)
  (when-let* ((skills-dir (macher-agent-sandbox-skills-directory)))
    (macher-agent-register-skill-directory skills-dir))
  (add-hook 'macher-agent-apply-payload-hook #'macher-agent-sandbox--on-apply-payload)
  (add-hook 'macher-agent-preset-cleared-hook #'macher-agent-sandbox--on-preset-cleared)
  (add-hook 'macher-agent-skill-baseline-capture-hook #'macher-agent-sandbox--on-skill-baseline-capture)
  (add-hook 'macher-agent-skill-baseline-restore-hook #'macher-agent-sandbox--on-skill-baseline-restore)
  (setq macher-agent-evaluate-tool-forms-function #'macher-agent-sandbox-evaluate-tool-forms)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-sandbox-append-ptc-to-transmission 60)
  (macher-agent-register-pipeline-step 'preset-composition #'macher-agent-sandbox--inject-preset-ptc-tool 40)
  (macher-agent-register-pipeline-step 'context-clone #'macher-agent-sandbox--clone-context-step 20))

;;;###autoload
(defun macher-agent-sandbox-uninstall ()
  "Uninstall sandbox execution capabilities, hooks, and pipeline steps."
  (interactive)
  (when-let* ((skills-dir (macher-agent-sandbox-skills-directory)))
    (macher-agent-unregister-skill-directory skills-dir))
  (remove-hook 'macher-agent-apply-payload-hook #'macher-agent-sandbox--on-apply-payload)
  (remove-hook 'macher-agent-preset-cleared-hook #'macher-agent-sandbox--on-preset-cleared)
  (remove-hook 'macher-agent-skill-baseline-capture-hook #'macher-agent-sandbox--on-skill-baseline-capture)
  (remove-hook 'macher-agent-skill-baseline-restore-hook #'macher-agent-sandbox--on-skill-baseline-restore)
  (when (eq macher-agent-evaluate-tool-forms-function #'macher-agent-sandbox-evaluate-tool-forms)
    (setq macher-agent-evaluate-tool-forms-function #'macher-agent-evaluate-trusted-tool-ast))
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-sandbox-append-ptc-to-transmission)
  (macher-agent-unregister-pipeline-step 'preset-composition #'macher-agent-sandbox--inject-preset-ptc-tool)
  (macher-agent-unregister-pipeline-step 'context-clone #'macher-agent-sandbox--clone-context-step)
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (local-variable-p 'macher-agent-sandbox--cached-baseline-set buf)
          (kill-local-variable 'macher-agent-sandbox--cached-baseline-set))
        (when (local-variable-p 'macher-agent-sandbox--cached-baseline-ptc-primitives buf)
          (kill-local-variable 'macher-agent-sandbox--cached-baseline-ptc-primitives))))))

(add-hook 'macher-agent-install-hook #'macher-agent-sandbox-install)
(add-hook 'macher-agent-uninstall-hook #'macher-agent-sandbox-uninstall)
(when (and (boundp 'gptel-prompt-transform-functions)
           (memq #'macher-agent-fsm-hijack-transform
                 (default-value 'gptel-prompt-transform-functions)))
  (macher-agent-sandbox-install))

(provide 'macher-agent-sandbox)
;;; macher-agent-sandbox.el ends here
