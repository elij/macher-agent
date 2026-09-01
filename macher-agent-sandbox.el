;;; macher-agent-sandbox.el --- Sandboxed Lisp evaluator for macher-agent  -*- lexical-binding: t; -*-

;; Author: Elijah Charles
;; Keywords: convenience, gptel, llm, macher
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; This file provides an isolated, sandboxed evaluator for Emacs Lisp
;; forms, limiting host operations and custom function executions.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'generator)
(require 'byte-opt)
(require 'gptel)
(require 'macher-agent-core)
(require 'macher-agent-tools)

(declare-function cps-internal-yield "generator")
(declare-function gptel-tool-p "gptel" (tool))
(declare-function gptel-tool-name "gptel" (tool))
(declare-function gptel-tool-description "gptel" (tool))
(declare-function gptel-tool-args "gptel" (tool))
(declare-function gptel-tool-function "gptel" (tool))
(declare-function gptel-tool-async "gptel" (tool))
(declare-function gptel-get-tool "gptel" (name))
(declare-function gptel--update-status "gptel" (status))
(declare-function gptel-markdown-cycle-block "gptel" ())
(declare-function org-cycle "org" (&optional arg))

(defvar gptel-tools)
(defvar gptel-model)
(defvar gptel-pre-tool-call-functions)
(defvar gptel-post-tool-call-functions)
(defvar gptel-include-tool-results)
(defvar macher-agent-allowed-tools)
(defvar macher-agent-ptc-execution-tool nil)

;;;; Plugin State Accessors

(defun macher-agent-sandbox-get-state (context)
  "Get the sandbox plugin state from CONTEXT."
  (when (macher-agent-context-p context)
    (plist-get (macher-agent-context-plugins context) :sandbox)))

(defun macher-agent-sandbox-set-state (context state)
  "Set the sandbox plugin STATE in CONTEXT."
  (when (macher-agent-context-p context)
    (setf (macher-agent-context-plugins context)
          (plist-put (macher-agent-context-plugins context) :sandbox state))
    state))

(defun macher-agent--ptc-primitive-p (sym)
  "Check whether SYM is an active Programmatic Tool Calling primitive.

Matches both direct symbol equivalence and dynamic translation between
hyphens and underscores for SYM.

SYM is the symbol to test.

Return t if SYM is an active primitive, otherwise nil.

Side effects: None."
  (let* ((sym-name (symbol-name sym))
         (norm-sym (replace-regexp-in-string "_" "-" sym-name))
         (active (bound-and-true-p macher-agent--active-ptc-primitives))
         (tools (bound-and-true-p gptel-tools)))
    (and (or (memq sym active)
             (cl-some (lambda (prim)
                        (let* ((prim-name (if (symbolp prim) (symbol-name prim) prim))
                               (norm-prim (replace-regexp-in-string "_" "-" prim-name)))
                          (string= norm-sym norm-prim)))
                      active)
             (and (null active)
                  (cl-some (lambda (tool)
                             (let* ((t-name (if (gptel-tool-p tool)
                                                (gptel-tool-name tool)
                                              (if (stringp tool) tool
                                                (format "%s" tool))))
                                    (norm-tool (replace-regexp-in-string "_" "-" t-name)))
                               (string= norm-sym norm-tool)))
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
  "Populate PRIMITIVES table with pure host operations.

Including a number of hardcoded defaults."
  (let ((table (or primitives macher-agent-sandbox--primitives)))
    (dolist
        (sym '(not null eq eql equal = + - * / < > <= >= max min
                   length list cons car cdr nth reverse split-string
                   plist-get string= string-match-p format))
      (when (fboundp sym)
        (puthash sym sym table)))

    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (or (get sym 'pure)
                      (get sym 'side-effect-free)))
         (puthash sym sym table))))))

(defun macher-agent-sandbox--init (&optional extra-operations)
  "Initialise the sandbox environment with EXTRA-OPERATIONS."
  (unless (and macher-agent-sandbox--primitives
               (hash-table-p macher-agent-sandbox--primitives)
               (> (hash-table-count macher-agent-sandbox--primitives) 0))
    (setq macher-agent-sandbox--primitives (make-hash-table :test 'eq)
          macher-agent-sandbox--functions (make-hash-table :test 'eq)
          macher-agent-sandbox--globals (make-hash-table :test 'eq))
    (macher-agent-sandbox--populate-pure-primitives macher-agent-sandbox--primitives))
  (when extra-operations
    (dolist (op extra-operations)
      (puthash op op macher-agent-sandbox--primitives))))

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
            (let ((prim (and
                         macher-agent-sandbox--primitives
                         (gethash func macher-agent-sandbox--primitives)))
                  (closure (and
                            macher-agent-sandbox--functions
                            (gethash func macher-agent-sandbox--functions))))
              (cond
               ((macher-agent--ptc-primitive-p func)
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
  "Normalise ARGS into a standard plist for tool calls."
  (cond
   ((keywordp (car args)) args)
   ((and (listp args) (consp (car args)) (symbolp (caar args)))
    (let (result)
      (dolist (pair args)
        (let* ((sym-name (symbol-name (car pair)))
               (key (if
                        (string-prefix-p ":" sym-name)
                        (car pair) (intern (concat ":" sym-name))))
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
                        (car pair) (intern (concat ":" sym-name))))
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
            "Process BINDINGS to extend lexical environment ENV.

If IS-STAR is non-nil, evaluate each binding in the updated environment."
            (let ((new-env env))
              (dolist (binding bindings new-env)
                (let*
                    ((eval-env (if is-star new-env env))
                     (pair
                      (iter-yield-from
                       (macher-agent-sandbox--eval-single-binding-iter binding eval-env))))
                  (setq new-env (cons pair new-env))))))

(iter-defun macher-agent-sandbox--eval-and-or-iter (args env is-or)
            "Evaluate boolean logic forms ARGS in ENV.

If IS-OR is non-nil, evaluate as `or', otherwise evaluate as `and'."
            (let ((result (not is-or)))
              (while (and args (if is-or (not result) result))
                (setq result (iter-yield-from (macher-agent-sandbox--eval-iter (pop args) env))))
              result))

(iter-defun macher-agent-sandbox--eval-let-iter (args env is-star)
            "Evaluate let or let* form ARGS in ENV.

If IS-STAR is non-nil, evaluate as `let*', otherwise evaluate as `let'."
            (let ((new-env (iter-yield-from (macher-agent-sandbox--bind-iter (car args) env is-star)))
                  (result nil))
              (dolist (form (cdr args) result)
                (setq result (iter-yield-from (macher-agent-sandbox--eval-iter form new-env))))))

(iter-defun macher-agent-sandbox--eval-setq-iter (args env)
            "Evaluate setq form ARGS in ENV."
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
            "Evaluate mapcar form ARGS in ENV."
            (let ((func (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
                  (seq (iter-yield-from (macher-agent-sandbox--eval-iter (cadr args) env)))
                  (result nil))
              (unless (listp seq) (error "Execution error (mapcar): Wrong type argument: listp, %s" seq))
              (dolist (item seq (nreverse result))
                (push (iter-yield-from (macher-agent-sandbox--funcall-iter func (list item))) result))))

(iter-defun macher-agent-sandbox--eval-mapc-iter (args env)
            "Evaluate mapc form ARGS in ENV."
            (let ((func (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
                  (seq (iter-yield-from (macher-agent-sandbox--eval-iter (cadr args) env))))
              (unless (listp seq)
                (error "Execution error (mapc): Wrong type argument: listp, %s" seq))
              (dolist (item seq seq)
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
                                    (cond-val
                                     (iter-yield-from
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
                                  (eval-args
                                   (iter-yield-from
                                    (macher-agent-sandbox--eval-args-iter (cdr args) env))))
                              (iter-yield-from (macher-agent-sandbox--funcall-iter func eval-args)))))
   (apply . ,(iter-lambda (args env)
                          (let* ((func (iter-yield-from
                                        (macher-agent-sandbox--eval-iter (car args) env)))
                                 (eval-args
                                  (iter-yield-from
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
                                        (macher-agent--ptc-primitive-p sym)
                                        (assoc sym macher-agent-sandbox--special-forms))
                                    t nil)))))
   (boundp . ,(iter-lambda (args env)
                           (let ((sym (iter-yield-from
                                       (macher-agent-sandbox--eval-iter (car args) env))))
                             (when (symbolp sym)
                               (if
                                   (or (assoc sym env)
                                       (not (eq
                                             (gethash sym macher-agent-sandbox--globals
                                                      macher-agent-sandbox--unbound)
                                             macher-agent-sandbox--unbound)))
                                   t nil)))))
   (macroexpand . ,(iter-lambda (args env)
                                (let* ((form (iter-yield-from
                                              (macher-agent-sandbox--eval-iter (car args) env)))
                                       (macro-def
                                        (when (and (consp form) (symbolp (car form)))
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
                                             (macher-agent--ptc-primitive-p val)))
                                    (functionp val)))))))

(defun macher-agent-sandbox--eval-symbol (expression environment)
  "Evaluate symbol EXPRESSION in ENVIRONMENT."
  (let ((binding (assoc expression environment)))
    (cond
     ((keywordp expression) expression)
     (binding (cdr binding))
     (t
      (let
          ((val
            (gethash expression macher-agent-sandbox--globals macher-agent-sandbox--unbound)))
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
                     (macher-agent--ptc-primitive-p operator))
                (let
                    ((evaled-args (iter-yield-from
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
            (macher-agent-sandbox--init)
            (cond
             ((or (numberp expression) (stringp expression) (memq expression '(t nil)))
              expression)
             ((symbolp expression)
              (macher-agent-sandbox--eval-symbol expression environment))
             ((consp expression)
              (iter-yield-from (macher-agent-sandbox--eval-compound-iter expression environment)))))

(defun macher-agent-ptc--inject-tool (state &optional item)
  "Assess Programmatic Tool Calling primitives in ITEM or STATE and attach execution tool.

STATE is the accumulated payload property list.
ITEM is the optional resolved preset or tool item tuple.

Return the updated STATE property list with the execution tool attached
when primitives are present.

Side effects: None."
  (let* ((item-spec (when (and (consp item) (eq (car item) 'preset)) (nth 2 item)))
         (item-prims (when item-spec (plist-get item-spec :ptc-primitives)))
         (state-prims (when (listp state) (plist-get state :ptc-primitives)))
         (active-prims (or item-prims state-prims (bound-and-true-p macher-agent--active-ptc-primitives))))
    (when active-prims
      (let* ((tools (when (listp state) (plist-get state :tools)))
             (ptc-tool (or (when (boundp 'macher-agent-ptc-execution-tool)
                             (symbol-value 'macher-agent-ptc-execution-tool))
                           (when (fboundp 'macher-agent-resolve-tool)
                             (ignore-errors (macher-agent-resolve-tool "ptc_execution" nil)))
                           (when (fboundp 'gptel-get-tool)
                             (ignore-errors (gptel-get-tool "ptc_execution")))
                           'ptc_execution))
             (already-present (cl-some (lambda (tl)
                                         (let ((name (cond
                                                      ((symbolp tl) (symbol-name tl))
                                                      ((stringp tl) tl)
                                                      ((and (fboundp 'gptel-tool-p)
                                                            (gptel-tool-p tl)
                                                            (fboundp 'gptel-tool-name))
                                                       (gptel-tool-name tl))
                                                      ((consp tl) (plist-get tl :name))
                                                      (t nil))))
                                           (and name (member name '("ptc_execution" "ptc-execution")))))
                                       tools)))
        (unless already-present
          (setq state (plist-put state :tools (append tools (list ptc-tool)))))))
    state))

;;; Programmatic Tool Calling (PTC) Prompt Generation

(defun macher-agent--build-tool-arg-type-spec (arg lisp-name lisp-arg-name)
  "Build type spec and optional deftype string for ARG.

Construct type spec and optional deftype definition for tool parameter ARG.
LISP-NAME is the Lisp function symbol for the tool.
LISP-ARG-NAME is the string name of the argument.

Return a cons cell (TYPE-SPEC . DEFTYPE-STR).
Side effects: None."
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
                                    ((or 'array "array") '(or null (list string)))
                                    (_ 't)))
                          prop-strs))
        (cons (list 'list type-name)
              (format "(cl-deftype %s () \n  \"Property list for %s.\"\n  '(plist %s))"
                      type-name lisp-arg-name
                      (string-join (nreverse prop-strs) " ")))))

     ((member arg-type '(array "array"))
      (cons '(or null (list string)) nil))

     ((member arg-type '(object "object"))
      (cons 'plist nil))

     (t
      (cons (pcase arg-type
              ((or 'string "string") 'string)
              ((or 'number "number") 'number)
              ((or 'boolean "boolean") 'boolean)
              (_ 't))
            nil)))))

(defun macher-agent--format-tool-defun
    (name lisp-name desc arg-names arg-types return-type dummy-return &optional deftypes)
  "Format tool defun string for tool NAME.

Construct Lisp function definition string for tool NAME using given specs.
LISP-NAME is the function symbol.
DESC is the tool description string.
ARG-NAMES is the list of argument name strings.
ARG-TYPES is the list of argument type specifications.
RETURN-TYPE is the declared return type specification.
DUMMY-RETURN is the dummy return value string representation.
DEFTYPES is an optional list of deftype strings.

Return a formatted defun string representation.
Side effects: None."
  (let* ((formatted-arg-types (mapcar (lambda (t-spec)
                                        (if (symbolp t-spec)
                                            (symbol-name t-spec)
                                          (prin1-to-string t-spec)))
                                      (nreverse arg-types)))
         (defun-str
          (format "%s -> (defun %s (%s)\n  %S\n  (declare (type (function (%s) %s)))\n  %s)"
                  name
                  lisp-name
                  (string-join (nreverse arg-names) " ")
                  desc
                  (string-join formatted-arg-types " ")
                  (if (symbolp return-type)
                      (symbol-name return-type) (prin1-to-string return-type))
                  dummy-return)))
    (if deftypes
        (concat (string-join (nreverse deftypes) "\n\n") "\n\n" defun-str)
      defun-str)))

(defun macher-agent--format-tool-lisp-docstring (tool-obj)
  "Infer declarative Lisp signature for TOOL-OBJ.

Extract schema and metadata from TOOL-OBJ to construct a declarative
Lisp signature string with type hints.

Return formatted tool Lisp signature string.
Side effects: None."
  (let* ((name (gptel-tool-name tool-obj))
         (lisp-name (intern (replace-regexp-in-string "_" "-" name)))
         (desc (gptel-tool-description tool-obj))
         (args (gptel-tool-args tool-obj))
         (deftypes nil)
         (arg-names nil)
         (arg-types nil)
         (is-delegate (string-match-p "delegate" name))
         (return-type (if is-delegate '(list string) 'string))
         (dummy-return (if is-delegate "(list \"\")" "\"\"")))

    (dolist (arg args)
      (let* ((raw-name (if (symbolp (plist-get arg :name))
                           (symbol-name (plist-get arg :name))
                         (plist-get arg :name)))
             (lisp-arg-name (replace-regexp-in-string "_" "-" raw-name))
             (spec-pair (macher-agent--build-tool-arg-type-spec arg lisp-name lisp-arg-name))
             (type-spec (car spec-pair))
             (deftype-str (cdr spec-pair)))
        (push lisp-arg-name arg-names)
        (when deftype-str
          (push deftype-str deftypes))
        (push type-spec arg-types)))

    (macher-agent--format-tool-defun
     name lisp-name desc arg-names arg-types return-type dummy-return deftypes)))

(defun macher-agent--filter-ptc-matching-tools (tools active)
  "Filter TOOLS matching any active primitive in ACTIVE.

Iterate through TOOLS list and collect tool objects matching active
primitive symbols or names present in ACTIVE.

Return list of matching gptel tool objects.
Side effects: None."
  (cl-loop for t-obj in tools
           when
           (and (gptel-tool-p t-obj)
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

(defun macher-agent--tools-require-complex-types-p (tools)
  "Check if any of the TOOLS require complex object or array arguments."
  (cl-some (lambda (tool)
             (cl-some (lambda (arg)
                        (let ((type (plist-get arg :type)))
                          (member type '(array "array" object "object"))))
                      (gptel-tool-args tool)))
           tools))

(defun macher-agent--inject-ptc-prompt
    (existing-prompt &optional active-primitives available-tools)
  "Append Programmatic Tool Calling instructions to EXISTING-PROMPT.

Dynamically append Programmatic Tool Calling instructions with compiler hints
to EXISTING-PROMPT.
ACTIVE-PRIMITIVES is an optional list of active primitive symbols or names.
AVAILABLE-TOOLS is an optional list of gptel tool objects.

Return modified prompt string with PTC instructions, or EXISTING-PROMPT.
Side effects: None."
  (let* ((active (or active-primitives (bound-and-true-p macher-agent--active-ptc-primitives)))
         (tools (or available-tools (bound-and-true-p gptel-tools)))
         (matching-tools (macher-agent--filter-ptc-matching-tools tools active)))
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
        (mapcar #'macher-agent--format-tool-lisp-docstring matching-tools) "\n\n")
       (if (macher-agent--tools-require-complex-types-p matching-tools)
           "\n\n;; JSON TO EMACS LISP MAPPING RULES:\n;; 1. JSON Objects MUST translate to flat property lists using keywords (for example, `(:key \"value\")`). DO NOT use alists or dotted pairs.\n;; 2. JSON Arrays MUST translate to standard Lisp lists. Construct them using the `list` function rather than quoting.\n;; Syntax example for complex arguments: `(tool-name (list (list :buffer_name \"agent\" :instructions \"Task\")))`"
         "")
       "\n\n;; EXPECTED SCRIPT RETURN TYPE:\n"
       "Keep everything within a let*. Return the output object without formatting.\n"))))

;;; PTC Execution Handlers and Coercion

(defun macher-agent--coerce-val (val spec)
  "Coerce VAL to match expected schema type in SPEC.

VAL is the input value to coerce.
SPEC is the JSON schema property specification plist.

Return coerced value matching expected type structure.
Side effects: None."
  (if (null spec) val
    (let ((type (plist-get spec :type)))
      (cond
       ((and (member type '(string "string")) (bufferp val))
        (buffer-name val))
       ((and (member type '(array "array")) (listp val))
        (vconcat (cl-loop for item in val
                          collect (macher-agent--coerce-val item (plist-get spec :items)))))
       ((and (member type '(object "object")) (listp val))
        (let* ((normalized-val
                (if (and val (consp (car val)))
                    (cl-loop for cell in val append (list (car cell) (cdr cell)))
                  val))
               (props (plist-get spec :properties)))
          (if props
              (cl-loop for (k v) on normalized-val by #'cddr
                       for p-name = (if (keywordp k) (substring (symbol-name k) 1) (format "%s" k))
                       for p-spec = (plist-get props (intern (concat ":" p-name)))
                       append (list k (macher-agent--coerce-val v p-spec)))
            normalized-val)))
       (t val)))))

(defun macher-agent--coerce-plist-args (args schema)
  "Coerce plist ARGS using target SCHEMA list.

ARGS is the plist of arguments to coerce.
SCHEMA is the argument schema specification list.

Return coerced plist of arguments.
Side effects: None."
  (let ((coerced-args nil))
    (cl-loop
     for (k v) on args by #'cddr
     for spec =
     (cl-find-if (lambda (s)
                   (let ((n (plist-get s :name)))
                     (string= (replace-regexp-in-string "_" "-" (if (symbolp n) (symbol-name n) n))
                              (substring (symbol-name k) 1))))
                 schema)
     do (progn (push k coerced-args)
               (push (if spec (macher-agent--coerce-val v spec) v) coerced-args)))
    (nreverse coerced-args)))

(defun macher-agent--coerce-positional-args (args schema)
  "Coerce positional ARGS using target SCHEMA list.

ARGS is the positional argument list to coerce.
SCHEMA is the argument schema specification list.

Return coerced list of positional arguments.
Side effects: None."
  (let ((coerced-args nil))
    (cl-loop for arg in args
             for spec in schema
             do (push (macher-agent--coerce-val arg spec) coerced-args))
    (nreverse coerced-args)))

(defun macher-agent--coerce-ptc-args (args tool)
  "Coerce evaluated Lisp ARGS to strictly match TOOL schema types.

ARGS is the evaluated argument list or plist.
TOOL is the resolved gptel tool structure.

Return coerced arguments list matching tool schema types.
Side effects: None."
  (if (not (macher-tool-valid-p tool))
      args
    (let ((schema (gptel-tool-args tool)))
      (if (and args (keywordp (car args)))
          (macher-agent--coerce-plist-args args schema)
        (macher-agent--coerce-positional-args args schema)))))

(defun macher-agent--display-ptc-tool-execution (tool-sym args context target-buf)
  "Display mode-line status and run pre-execution hooks for PTC tool.

TOOL-SYM is the symbol name of the target PTC tool.
ARGS is the list of arguments supplied to the tool call.
CONTEXT is the active agent context structure.
TARGET-BUF is the target buffer for tool execution.

Return cons cell of (ORIGINAL-NAME-STRING . RESOLVED-TOOL).
Side effects: Updates mode-line status and executes pre-tool-call hooks."
  (let* ((target-lisp-name (symbol-name tool-sym))
         (original-name-str (replace-regexp-in-string "-" "_" target-lisp-name))
         (tool-name original-name-str)
         (resolved-tool (if (fboundp 'macher-agent-resolve-tool)
                            (macher-agent-resolve-tool original-name-str nil nil context)
                          (when (and (boundp 'macher-agent-tools-registry)
                                     (hash-table-p macher-agent-tools-registry))
                            (gethash original-name-str macher-agent-tools-registry))))
         (desc (if (macher-tool-valid-p resolved-tool)
                   (gptel-tool-description resolved-tool)
                 original-name-str))
         (block-reason nil))

    (when (bound-and-true-p gptel-pre-tool-call-functions)
      (run-hook-wrapped
       'gptel-pre-tool-call-functions
       (lambda (f)
         (let ((res (ignore-errors
                      (funcall f (list :name original-name-str :args args
                                       :buffer (buffer-name (if (bufferp target-buf) target-buf (current-buffer)))
                                       :model (bound-and-true-p gptel-model))))))
           (when (and res (listp res) (plist-get res :block))
             (setq block-reason (plist-get res :block))
             t)))))

    (if block-reason
        (error "%s" block-reason)
      (message "PTC Executing: %s" (or desc original-name-str))
      (when (fboundp 'macher-agent-gptel-spoof-tool-ui)
        (macher-agent-gptel-spoof-tool-ui target-buf tool-name))
      (cons original-name-str resolved-tool))))

(defun macher-agent--format-ptc-result-block (truncated-call call-str str-res)
  "Format PTC result block string for display in buffer.

TRUNCATED-CALL is the truncated call header string.
CALL-STR is the full call string representation.
STR-RES is the formatted string response payload.

Return formatted result block string matching mode syntax.
Side effects: None."
  (if (derived-mode-p 'org-mode)
      (format "\n#+begin_tool %s\n%s\n\n%s\n#+end_tool\n"
              truncated-call call-str str-res)
    (format "\n``` tool %s\n%s\n\n%s\n```\n"
            truncated-call call-str str-res)))

(defun macher-agent--cycle-ptc-result-block ()
  "Cycle visibility of the PTC result block at point.

Return result of cycling command evaluation or nil on error.
Side effects: Toggles block visibility in Org mode or Markdown mode."
  (ignore-errors
    (if (derived-mode-p 'org-mode)
        (org-cycle)
      (when (fboundp 'gptel-markdown-cycle-block)
        (gptel-markdown-cycle-block)))))

(defun macher-agent--insert-ptc-tool-result (original-name-str args res-data)
  "Insert collapsible tool result block into current buffer.

ORIGINAL-NAME-STR is the original tool name string.
ARGS is the list of arguments passed to tool.
RES-DATA is the response data payload or string.

Return nil.
Side effects: Inserts formatted result block text into buffer at buffer max."
  (when-let* ((str-res (if (stringp res-data) res-data (prin1-to-string res-data)))
              (call-str (prin1-to-string `(:name ,original-name-str :args ,args))))
    (when (bound-and-true-p gptel-post-tool-call-functions)
      (run-hook-wrapped
       'gptel-post-tool-call-functions
       (lambda (f)
         (ignore-errors
           (funcall f (list :name original-name-str :args args :result str-res
                            :buffer (buffer-name) :model (bound-and-true-p gptel-model)))))))
    (when (bound-and-true-p gptel-include-tool-results)
      (save-excursion
        (goto-char (point-max))
        (let* ((inhibit-read-only t)
               (arg-str (string-trim (prin1-to-string args) "(" ")"))
               (raw-call (format "(%s %s)" original-name-str arg-str))
               (truncated-call (truncate-string-to-width raw-call 60 nil nil " ...)"))
               (start-pt (point)))
          (insert (macher-agent--format-ptc-result-block truncated-call call-str str-res))
          (goto-char start-pt)
          (forward-line 1)
          (macher-agent--cycle-ptc-result-block))))))

(defun macher-agent--dispatch-ptc-primitive
    (tool-lisp-sym args resolved-tool original-name-str ptc-callback on-error stop-iter-fn &optional target-buf context)
  "Dispatch TOOL-LISP-SYM with ARGS or RESOLVED-TOOL for ORIGINAL-NAME-STR.

TOOL-LISP-SYM is the symbol of the tool function.
ARGS is the list of arguments passed to the tool.
RESOLVED-TOOL is the resolved gptel tool structure.
ORIGINAL-NAME-STR is the string name of the original tool.
PTC-CALLBACK is the callback function upon successful execution.
ON-ERROR is the callback function invoked upon error.
STOP-ITER-FN is the function to halt the iterator.
TARGET-BUF is the target buffer for tool execution.
CONTEXT is the active agent context structure.

Return result of dispatching tool execution.
Side effects: Invokes tool function and callbacks."
  (let ((run-in-target
         (lambda (fn)
           (if (and target-buf (buffer-live-p target-buf))
               (with-current-buffer target-buf
                 (when (and context (macher-agent-valid-context-p context))
                   (setq macher-agent--persistent-context context))
                 (funcall fn))
             (funcall fn)))))
    (cond
     ((and (macher-tool-valid-p resolved-tool)
           (gptel-tool-function resolved-tool))
      (let ((coerced-args (macher-agent--coerce-ptc-args args resolved-tool)))
        (funcall run-in-target
                 (lambda ()
                   (if (gptel-tool-async resolved-tool)
                       (let ((called nil))
                         (apply (gptel-tool-function resolved-tool)
                                (lambda (res &optional err)
                                  (unless called
                                    (setq called t)
                                    (if err
                                        (funcall stop-iter-fn err)
                                      (funcall ptc-callback res))))
                                coerced-args))
                     (let ((sync-result (apply (gptel-tool-function resolved-tool) coerced-args)))
                       (funcall ptc-callback sync-result)))))))
     ((and (symbolp tool-lisp-sym)
           (fboundp tool-lisp-sym)
           (or (macher-agent--ptc-primitive-p tool-lisp-sym)
               (and (boundp 'macher-agent-sandbox--primitives)
                    (hash-table-p macher-agent-sandbox--primitives)
                    (gethash tool-lisp-sym macher-agent-sandbox--primitives))
               (and (boundp 'macher-agent-allowed-tools)
                    (listp macher-agent-allowed-tools)
                    (memq tool-lisp-sym macher-agent-allowed-tools))))
      (funcall run-in-target
               (lambda ()
                 (let ((lisp-res (apply tool-lisp-sym args)))
                   (funcall ptc-callback lisp-res)))))
     (t
      (when stop-iter-fn (funcall stop-iter-fn "Tool not accessible"))
      (funcall on-error
               (list :status 'error
                     :error (format "ERROR: Tool '%s' is not accessible." original-name-str)))))))

(defun macher-agent--ptc-handle-yielded-value (yielded-val context target-buf ptc-resume-loop on-error stop-iter-fn)
  "Handle YIELDED-VAL returned during PTC execution.

YIELDED-VAL is the value yielded from sandbox evaluation iterator.
CONTEXT is the active agent context structure.
TARGET-BUF is the target buffer for tool execution.
PTC-RESUME-LOOP is the loop continuation callback function.
ON-ERROR is the error callback function on failure.
STOP-ITER-FN is the callback function halting iterator.

Return result of dispatching yielded value handling.
Side effects: Triggers tool execution and schedules loop resumption."
  (if (and (macher-agent-tool-call-p yielded-val) (macher-agent-tool-call-name yielded-val))
      (let* ((tool-lisp-sym (macher-agent-tool-call-name yielded-val))
             (args (macher-agent-tool-call-args yielded-val))
             (tool-info (macher-agent--display-ptc-tool-execution tool-lisp-sym args context target-buf))
             (original-name-str (car tool-info))
             (resolved-tool (cdr tool-info))
             (ptc-callback (lambda (res-data)
                             (macher-agent--insert-ptc-tool-result original-name-str args res-data)
                             (funcall ptc-resume-loop res-data))))
        (macher-agent--dispatch-ptc-primitive
         tool-lisp-sym args resolved-tool original-name-str
         ptc-callback on-error stop-iter-fn target-buf context))
    (let ((reason (format "Unexpected yield from PTC sandbox: %S" yielded-val)))
      (when stop-iter-fn (funcall stop-iter-fn reason))
      (funcall on-error (list :status 'error :error reason)))))

(defun macher-agent-execute-ptc-script
    (script-string context target-buf on-success on-error &optional extra-primitives)
  "Execute Programmatic Tool Calling SCRIPT-STRING using CONTEXT in TARGET-BUF.

SCRIPT-STRING is the string containing Lisp code to evaluate.
CONTEXT is the active context structure.
TARGET-BUF is the target buffer for execution.
ON-SUCCESS is the callback function invoked with success result.
ON-ERROR is the callback function invoked with error result.
EXTRA-PRIMITIVES is an optional list of allowed primitive host functions.

Return nil.
Side effects: Evaluates Lisp code in a sandboxed environment."
  (let* ((prims (append
                 '(nreverse sort delete delq nconc plist-put aset puthash remhash error signal message random emacs-version)
                 extra-primitives))
         (macher-agent--active-ptc-execution t))
    (condition-case err
        (progn
          (with-current-buffer target-buf
            (setq macher-agent--persistent-context context)
            (when (boundp 'macher-agent-sandbox--globals)
              (setq macher-agent-sandbox--globals (make-hash-table :test 'eq)))
            (when (boundp 'macher-agent-sandbox--primitives)
              (setq macher-agent-sandbox--primitives (make-hash-table :test 'eq)))
            (when (boundp 'macher-agent-sandbox--functions)
              (setq macher-agent-sandbox--functions (make-hash-table :test 'eq)))
            (macher-agent-sandbox--init prims))
          (let* ((ast (macroexpand-all (let ((read-eval nil))
                                         (read (format "(progn\n%s\n)" script-string)))))
                 (iterator (with-current-buffer target-buf
                             (macher-agent-sandbox--eval-iter ast nil)))
                 (stop-iter-fn (lambda (reason)
                                 (funcall on-error (list :status 'error :error reason)))))
            (letrec
                ((ptc-resume-loop
                  (lambda (input)
                    (condition-case iter-err
                        (let ((step (with-current-buffer target-buf (iter-next iterator input))))
                          (if (macher-agent-tool-call-p step)
                              (macher-agent--ptc-handle-yielded-value
                               step context target-buf ptc-resume-loop on-error stop-iter-fn)
                            (funcall ptc-resume-loop step)))
                      (iter-end-of-sequence
                       (funcall on-success (cdr iter-err)))
                      (error
                       (funcall on-error (list :status 'error :error (error-message-string iter-err))))))))
              (funcall ptc-resume-loop nil))))
      (error
       (funcall on-error (list :status 'error :error (error-message-string err)))))))

(defun macher-agent-sandbox-run (expression extra-operations context target-buf)
  "Execute Lisp EXPRESSION in a sandboxed environment with EXTRA-OPERATIONS.

EXPRESSION is the Lisp expression form to evaluate inside sandbox.
EXTRA-OPERATIONS is a list of host function symbols allowed in sandbox.
CONTEXT is the active agent context structure to use for logging tool intent.
TARGET-BUF is the target buffer of the execution.

Return the result of evaluating EXPRESSION.

Side effects: Evaluates sandboxed Lisp expression."
  (declare (ftype (function (t list t t) t)))
  (let ((macher-agent-sandbox--primitives (make-hash-table :test 'eq))
        (macher-agent-sandbox--functions (make-hash-table :test 'eq))
        (macher-agent-sandbox--globals (make-hash-table :test 'eq)))
    (macher-agent-sandbox--init extra-operations)
    (let* ((iterator (macher-agent-sandbox--eval-iter (macroexpand-all expression) nil))
           (yield-val nil)
           (next-yield nil))
      (if (null iterator)
          (error "macher-agent-sandbox--eval-iter is unmapped")
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
                      (when (fboundp 'macher-agent-log-tool-intent)
                        (macher-agent-log-tool-intent context "ptc" target args))))))
              (setq yield-val next-yield))
          (iter-end-of-sequence (cdr err)))))))

(defun macher-agent-sandbox-append-ptc-directive (state _orig-buf _presets _skills _redirect)
  "Append Programmatic Tool Calling hint directives and aliases to STATE."
  (let ((prims (if (and (fboundp 'macher-agent-transmission-state-p)
                        (macher-agent-transmission-state-p state))
                   (macher-agent-transmission-state-ptc-primitives state)
                 (when (listp state) (plist-get state :ptc-primitives)))))
    (when prims
      (let* ((tools (if (and (fboundp 'macher-agent-transmission-state-p)
                             (macher-agent-transmission-state-p state))
                        (macher-agent-transmission-state-tools state)
                      (when (listp state) (plist-get state :tools))))
             (compiled-ptc-block (macher-agent--inject-ptc-prompt "" prims tools)))
        (unless (string-empty-p compiled-ptc-block)
          (if (and (fboundp 'macher-agent-transmission-state-p)
                   (macher-agent-transmission-state-p state))
              (push (string-trim compiled-ptc-block)
                    (macher-agent-transmission-state-directives state))
            (when (listp state)
              (setq state (plist-put state :directives
                                     (cons (string-trim compiled-ptc-block)
                                           (plist-get state :directives))))))))))
  state)

(defun macher-agent-sandbox-install ()
  "Install Programmatic Tool Calling hooks, tools, and pipeline steps."
  (macher-agent-register-pipeline-step 'preset-composition #'macher-agent-ptc--inject-tool 50)
  (macher-agent-register-pipeline-step 'transmission #'macher-agent-sandbox-append-ptc-directive 80))

(defun macher-agent-sandbox-uninstall ()
  "Uninstall Programmatic Tool Calling hooks, tools, and pipeline steps."
  (macher-agent-unregister-pipeline-step 'preset-composition #'macher-agent-ptc--inject-tool)
  (macher-agent-unregister-pipeline-step 'transmission #'macher-agent-sandbox-append-ptc-directive)
  (setq macher-agent-ptc-execution-tool nil))

(provide 'macher-agent-sandbox)
;;; macher-agent-sandbox.el ends here
