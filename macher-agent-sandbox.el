;;; macher-agent-sandbox.el --- Sandboxed Lisp evaluator for macher-agent -*- lexical-binding: t; -*-

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
(require 'generator)
(require 'byte-opt)

(declare-function cps-internal-yield "generator")

(defvar-local macher-agent-sandbox--globals nil
  "Store global variable environment for guest execution.

This hash-table maps variable symbols to their values during sandboxed
evaluation.

Return current hash-table or nil.
Side effects: Buffer-local variable.")

(defvar-local macher-agent-sandbox--primitives nil
  "Store allowed host primitive operations.

This hash-table maps primitive function symbols to their host definitions
permitted in the sandbox environment.

Return current hash-table or nil.
Side effects: Buffer-local variable.")

(defvar-local macher-agent-sandbox--functions nil
  "Store guest function definitions.

This hash-table maps guest function symbols to their closure definitions
or function bodies within the sandbox environment.

Return current hash-table or nil.
Side effects: Buffer-local variable.")

(defconst macher-agent-sandbox--unbound (make-symbol "unbound")
  "Mark unbound variables in the sandbox environment.

This unique uninterned symbol serves as a sentinel marker to distinguish
unbound variables from variables bound to nil.

Return sentinel symbol marking unbound variables.
Side effects: None.")

(declare-function macher-agent--ptc-primitive-p "macher-agent-orchestration" (sym))

(defun macher-agent-sandbox--populate-pure-primitives (&optional primitives)
  "Populate PRIMITIVES table with pure host operations.

Scan internal host symbols and add those marked as pure or side-effect-free
to PRIMITIVES.

PRIMITIVES is an optional hash-table to populate, defaulting to
`macher-agent-sandbox--primitives`.

Return the populated hash-table PRIMITIVES.
Side effects: Mutates the PRIMITIVES hash-table."
  (let ((table (or primitives macher-agent-sandbox--primitives)))
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (or (get sym 'pure)
                      (get sym 'side-effect-free)))
         (puthash sym sym table))))))

(defun macher-agent-sandbox--init (&optional extra-operations)
  "Initialise the sandbox environment.

Set up primitives, functions, and globals hash-tables if not already
initialised, and populate them with standard pure host operations.

EXTRA-OPERATIONS is an optional list of additional primitive symbols to permit.

Return nil.
Side effects: Initialises and mutates buffer-local sandbox state variables."
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
            "Evaluate a list of ARGS in ENV using the generator evaluator.

Evaluate each argument expression in ARGS sequentially using the
sandboxed generator evaluator within environment ENV.

ARGS is a list of expressions to evaluate.
ENV is the current lexical environment alist.

Return a list of evaluated argument values.
Side effects: May mutate global sandbox state during evaluation."
            (let ((evaled nil))
              (dolist (arg args (nreverse evaled))
                (push (iter-yield-from (macher-agent-sandbox--eval-iter arg env)) evaled))))

(iter-defun macher-agent-sandbox--apply-closure-iter (closure arguments)
            "Execute CLOSURE with ARGUMENTS as a generator.

Bind CLOSURE parameters to evaluated ARGUMENTS in a new lexical environment
and evaluate the CLOSURE body forms sequentially.

CLOSURE is a closure structure (closure PARAMS BODY ENV).
ARGUMENTS is a list of evaluated argument values to bind.

Return the result of evaluating the final body form in CLOSURE.
Side effects: May mutate global sandbox state during execution."
            (let ((new-env (cadddr closure))
                  (params (cadr closure))
                  (result nil))
              (while params
                (push (cons (pop params) (pop arguments)) new-env))
              (dolist (form (caddr closure) result)
                (setq result (iter-yield-from (macher-agent-sandbox--eval-iter form new-env))))))

(defun macher-agent-sandbox--normalize-args-to-plist (args)
  "Normalise ARGS into a standard plist for tool calls.

Convert alist representations of arguments into a flat plist with
keyword symbols.  If ARGS is already a plist, return it unchanged.

ARGS is a list representing function arguments.

Return a normalised plist of arguments.
Side effects: None."
  (cond
   ((keywordp (car args))
    args)
   ((and (listp args)
         (consp (car args))
         (symbolp (caar args)))
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
   ((and (listp args)
         (= (length args) 1)
         (listp (car args))
         (consp (caar args))
         (symbolp (car (caar args)))
         (not (keywordp (car (caar args)))))
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

(iter-defun macher-agent-sandbox--funcall-symbol-iter (func eval-args)
            "Execute function symbol FUNC with EVAL-ARGS in the sandbox.

Look up function symbol FUNC in PTC primitives, allowed primitive
table, or defined guest closures, and execute or yield as appropriate.

FUNC is the function symbol to invoke.
EVAL-ARGS is the list of evaluated argument values.

Return the result of the function call or yield a tool call interrupt.
Side effects: May yield an interrupt or mutate global sandbox state."
            (let ((prim (and macher-agent-sandbox--primitives (gethash func macher-agent-sandbox--primitives)))
                  (closure (and macher-agent-sandbox--functions (gethash func macher-agent-sandbox--functions))))

              (cond
               ((and (fboundp 'macher-agent--ptc-primitive-p)
                     (macher-agent--ptc-primitive-p func))
                (iter-yield (list :interrupt 'tool-call
                                  :name func
                                  :args (macher-agent-sandbox--normalize-args-to-plist eval-args))))
               (prim
                (apply prim eval-args))
               (closure
                (iter-yield-from (macher-agent-sandbox--apply-closure-iter closure eval-args)))
               (t
                (error "Void or forbidden function: %s" func)))))

(iter-defun macher-agent-sandbox--funcall-iter (func eval-args)
            "Execute function FUNC with EVAL-ARGS in the sandbox.

Dispatch execution of FUNC based on whether it is a closure structure
or a function symbol.

FUNC is a function symbol or closure structure.
EVAL-ARGS is the list of evaluated argument values.

Return the result of invoking FUNC with EVAL-ARGS.
Side effects: May yield an interrupt or mutate global sandbox state."
            ;;(message func)
            (cond
             ((and (consp func) (eq (car func) 'closure))
              (iter-yield-from (macher-agent-sandbox--apply-closure-iter func eval-args)))
             ((symbolp func)
              (iter-yield-from (macher-agent-sandbox--funcall-symbol-iter func eval-args)))
             (t
              (error "Invalid function target: %s" func))))

(iter-defun macher-agent-sandbox--eval-single-binding-iter (binding eval-env)
            "Evaluate a single BINDING specification in EVAL-ENV.

Evaluate the value expression of BINDING within environment EVAL-ENV
and construct a variable-value pair cons cell.

BINDING is a variable symbol or a (VAR VAL) list.
EVAL-ENV is the lexical environment alist used to evaluate VAL.

Return a (VAR . VAL) cons cell.
Side effects: May mutate global sandbox state during evaluation."
            (let ((var (if (consp binding) (car binding) binding))
                  (val (when (consp binding)
                         (iter-yield-from
                          (macher-agent-sandbox--eval-iter (cadr binding) eval-env)))))
              (cons var val)))

(iter-defun macher-agent-sandbox--bind-iter (bindings env is-star)
            "Process BINDINGS to extend lexical environment ENV.

Evaluate each binding in BINDINGS and extend ENV, supporting sequential
lexical binding if IS-STAR is non-nil.

BINDINGS is a list of binding specifications.
ENV is the base lexical environment alist.
IS-STAR is non-nil for sequential let* binding, or nil for parallel let binding.

Return the extended lexical environment alist.
Side effects: May mutate global sandbox state during binding evaluation."
            (let ((new-env env))
              (dolist (binding bindings new-env)
                (let* ((eval-env (if is-star new-env env))
                       (pair (iter-yield-from
                              (macher-agent-sandbox--eval-single-binding-iter binding eval-env))))
                  (setq new-env (cons pair new-env))))))

(iter-defun macher-agent-sandbox--eval-and-or-iter (args env is-or)
            "Evaluate boolean logic forms ARGS in ENV.

Evaluate expressions in ARGS sequentially in environment ENV with short-circuit
logic, implementing logical OR if IS-OR is non-nil, or logical AND otherwise.

ARGS is a list of conditional expressions.
ENV is the lexical environment alist.
IS-OR is non-nil for or forms, or nil for and forms.

Return the result of short-circuit evaluation.
Side effects: May mutate global sandbox state during evaluation."
            (let ((result (not is-or)))
              (while (and args (if is-or (not result) result))
                (setq result (iter-yield-from (macher-agent-sandbox--eval-iter (pop args) env))))
              result))

(iter-defun macher-agent-sandbox--eval-let-iter (args env is-star)
            "Evaluate let or let* form ARGS in ENV.

Bind variables in (car ARGS) using environment ENV and evaluate body
expressions in (cdr ARGS) sequentially.

ARGS is a list starting with bindings list followed by body forms.
ENV is the current lexical environment alist.
IS-STAR is non-nil for let* binding, or nil for standard let binding.

Return the value of the final body form.
Side effects: May mutate global sandbox state during evaluation."
            (let ((new-env (iter-yield-from (macher-agent-sandbox--bind-iter (car args) env is-star)))
                  (result nil))
              (dolist (form (cdr args) result)
                (setq result (iter-yield-from (macher-agent-sandbox--eval-iter form new-env))))))

(iter-defun macher-agent-sandbox--eval-setq-iter (args env)
            "Evaluate setq form ARGS in ENV.

Assign values to variable symbols pair by pair in ARGS, updating lexical
binding in ENV if present, or setting global sandbox variable otherwise.

ARGS is a list of alternating VAR RHS expressions.
ENV is the current lexical environment alist.

Return the value assigned to the final variable.
Side effects: Mutates variable bindings in ENV or `macher-agent-sandbox--globals`."
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
            "Evaluate mapcar form ARGS in ENV.

Evaluate function and sequence expressions in ARGS within environment ENV,
then map the function over sequence items sequentially.

ARGS is a list starting with function expression followed by sequence.
ENV is the current lexical environment alist.

Return a list of mapped elements.
Side effects: May yield tool call interrupts or mutate global sandbox state."
            (let ((func (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
                  (seq (iter-yield-from (macher-agent-sandbox--eval-iter (cadr args) env)))
                  (result nil))
              (unless (listp seq)
                (error "Execution error (mapcar): Wrong type argument: listp, %s" seq))
              (dolist (item seq (nreverse result))
                (push (iter-yield-from (macher-agent-sandbox--funcall-iter func (list item))) result))))

(iter-defun macher-agent-sandbox--eval-mapc-iter (args env)
            "Evaluate mapc form ARGS in ENV.

Evaluate function and sequence expressions in ARGS within environment ENV,
then apply the function to sequence items sequentially.

ARGS is a list starting with function expression followed by sequence.
ENV is the current lexical environment alist.

Return the sequence argument SEQ.
Side effects: May yield tool call interrupts or mutate global sandbox state."
            (let ((func (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
                  (seq (iter-yield-from (macher-agent-sandbox--eval-iter (cadr args) env))))
              (unless (listp seq)
                (error "Execution error (mapc): Wrong type argument: listp, %s" seq))
              (dolist (item seq seq)
                (iter-yield-from (macher-agent-sandbox--funcall-iter func (list item))))))

(defvar-local macher-agent-sandbox--special-forms
  `((quote . ,(iter-lambda (args _env) (car args)))
    (mapcar . ,(iter-lambda (args env) (iter-yield-from (macher-agent-sandbox--eval-mapcar-iter args env))))
    (mapc . ,(iter-lambda (args env) (iter-yield-from (macher-agent-sandbox--eval-mapc-iter args env))))
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
                               (setq result (iter-yield-from (macher-agent-sandbox--eval-iter form env)))))))
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
                                     (cond-val (iter-yield-from (macher-agent-sandbox--eval-iter (car clause) env))))
                                (when cond-val
                                  (setq matched t)
                                  (if (cdr clause)
                                      (dolist (form (cdr clause))
                                        (setq result (iter-yield-from (macher-agent-sandbox--eval-iter form env))))
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
                              (let ((func-name (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
                                    (func-body (iter-yield-from (macher-agent-sandbox--eval-iter (cadr args) env))))
                                (puthash func-name func-body macher-agent-sandbox--functions)
                                func-name)))
    (funcall . ,(iter-lambda (args env)
                             (let ((func (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
                                   (eval-args (iter-yield-from (macher-agent-sandbox--eval-args-iter (cdr args) env))))
                               (iter-yield-from (macher-agent-sandbox--funcall-iter func eval-args)))))
    (apply . ,(iter-lambda (args env)
                           (let* ((func (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
                                  (eval-args (iter-yield-from (macher-agent-sandbox--eval-args-iter (cdr args) env)))
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
                                                (matched-handler nil))
                                           (dolist (handler handlers)
                                             (let ((cond-spec (car handler)))
                                               (when (or (eq cond-spec t)
                                                         (eq cond-spec err-sym)
                                                         (and (listp cond-spec) (memq err-sym cond-spec)))
                                                 (setq matched-handler handler))))
                                           (if matched-handler
                                               (let ((new-env (if var (cons (cons var err) env) env))
                                                     (result nil))
                                                 (dolist (form (cdr matched-handler) result)
                                                   (setq result (iter-yield-from (macher-agent-sandbox--eval-iter form new-env)))))
                                             (signal (car err) (cdr err)))))))))
    (unwind-protect . ,(iter-lambda (args env)
                                    (unwind-protect
                                        (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env))
                                      (dolist (form (cdr args))
                                        (iter-yield-from (macher-agent-sandbox--eval-iter form env))))))
    (catch . ,(iter-lambda (args env)
                           (catch (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env))
                             (let ((result nil))
                               (dolist (form (cdr args) result)
                                 (setq result (iter-yield-from (macher-agent-sandbox--eval-iter form env))))))))
    (throw . ,(iter-lambda (args env)
                           (throw (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env))
                                  (iter-yield-from (macher-agent-sandbox--eval-iter (cadr args) env)))))
    (fboundp . ,(iter-lambda (args env)
                             (let ((sym (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env))))
                               (when (symbolp sym)
                                 (or (gethash sym macher-agent-sandbox--primitives)
                                     (gethash sym macher-agent-sandbox--functions)
                                     (and (fboundp 'macher-agent--ptc-primitive-p)
                                          (macher-agent--ptc-primitive-p sym))
                                     (assoc sym macher-agent-sandbox--special-forms))))))
    (boundp . ,(iter-lambda (args env)
                            (let ((sym (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env))))
                              (when (symbolp sym)
                                (or (assoc sym env)
                                    (not (eq (gethash sym macher-agent-sandbox--globals macher-agent-sandbox--unbound)
                                             macher-agent-sandbox--unbound)))))))
    (macroexpand . ,(iter-lambda (args env)
                                 (let* ((form (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
                                        (macro-def (when (and (consp form) (symbolp (car form)))
                                                     (gethash (car form) macher-agent-sandbox--functions))))
                                   (if (and (consp macro-def) (eq (car macro-def) 'macro))
                                       (iter-yield-from (macher-agent-sandbox--funcall-iter (cdr macro-def) (cdr form)))
                                     form))))
    (functionp . ,(iter-lambda (args env)
                               (let ((val (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env))))
                                 (or (and (consp val) (memq (car val) '(closure lambda macro)))
                                     (and (symbolp val)
                                          (or (gethash val macher-agent-sandbox--primitives)
                                              (gethash val macher-agent-sandbox--functions)
                                              (and (fboundp 'macher-agent--ptc-primitive-p)
                                                   (macher-agent--ptc-primitive-p val))))
                                     (functionp val))))))
  "Store mapping of special form symbols to handler functions.

This alist maps special form symbols (such as quote, let, if, cond, and so on)
to generator handler functions used by the sandboxed evaluator.

Return the alist mapping special form symbols to handler functions.
Side effects: Buffer-local variable.")

(defun macher-agent-sandbox--eval-symbol (expression environment)
  "Evaluate symbol EXPRESSION in ENVIRONMENT.

Resolve symbol EXPRESSION by checking keywords, lexical bindings in
ENVIRONMENT, or global sandbox variables.

EXPRESSION is the symbol to evaluate.
ENVIRONMENT is the current lexical environment alist.

Return the value bound to EXPRESSION.
Side effects: Signals an error if EXPRESSION is an unbound variable."
  (let ((binding (assoc expression environment)))
    (cond
     ((keywordp expression) expression)
     (binding (cdr binding))
     (t
      (let ((val (gethash expression macher-agent-sandbox--globals macher-agent-sandbox--unbound)))
        (when (eq val macher-agent-sandbox--unbound)
          (error "Unbound variable: %s" expression))
        val)))))

(iter-defun macher-agent-sandbox--eval-compound-iter (expression environment)
            "Evaluate compound form EXPRESSION in ENVIRONMENT.

Evaluate Lisp list expression EXPRESSION by dispatching to PTC tool calls,
special forms, macros, or standard function invocations.

EXPRESSION is the non-empty compound form list to evaluate.
ENVIRONMENT is the current lexical environment alist.

Return the evaluated result of the compound form.
Side effects: May yield tool call interrupts or mutate global sandbox state."
            (let* ((operator (car expression))
                   (arguments (cdr expression))
                   (special-handler (and (symbolp operator)
                                         (cdr (assoc operator macher-agent-sandbox--special-forms))))
                   (macro-def (and (symbolp operator)
                                   (gethash operator macher-agent-sandbox--functions))))
              (cond
               ((and (symbolp operator)
                     (fboundp 'macher-agent--ptc-primitive-p)
                     (macher-agent--ptc-primitive-p operator))
                (let ((evaled-args (iter-yield-from (macher-agent-sandbox--eval-args-iter arguments environment))))
                  (iter-yield (list :interrupt 'tool-call
                                    :name operator
                                    :args (macher-agent-sandbox--normalize-args-to-plist evaled-args)))))
               (special-handler
                (iter-yield-from (funcall special-handler arguments environment)))
               ((and (consp macro-def) (eq (car macro-def) 'macro))
                (let* ((macro-fn (cdr macro-def))
                       (expanded-form (iter-yield-from (macher-agent-sandbox--funcall-iter macro-fn arguments))))
                  (iter-yield-from (macher-agent-sandbox--eval-iter expanded-form environment))))
               (t
                (let ((evaled-args (iter-yield-from (macher-agent-sandbox--eval-args-iter arguments environment))))
                  (iter-yield-from (macher-agent-sandbox--funcall-iter operator evaled-args)))))))

(iter-defun macher-agent-sandbox--eval-iter (expression environment)
            "Evaluate EXPRESSION in ENVIRONMENT yielding on PTC tool calls.

Initialise the sandbox environment and evaluate EXPRESSION based on whether
it is a literal atom, symbol, or compound form.

EXPRESSION is the Lisp form to evaluate.
ENVIRONMENT is the current lexical environment alist.

Return the evaluated result of EXPRESSION.
Side effects: May initialise sandbox state and yield on tool interrupts."
            (macher-agent-sandbox--init)
            (cond
             ((or (numberp expression) (stringp expression) (memq expression '(t nil)))
              expression)
             ((symbolp expression)
              (macher-agent-sandbox--eval-symbol expression environment))
             ((consp expression)
              (iter-yield-from (macher-agent-sandbox--eval-compound-iter expression environment)))))

(provide 'macher-agent-sandbox)
;;; macher-agent-sandbox.el ends here
