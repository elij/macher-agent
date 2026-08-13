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
(require 'generator)
(require 'byte-opt)
(require 'macher-agent-core)

(declare-function cps-internal-yield "generator")

(defvar-local macher-agent-sandbox--globals nil
  "Store global variable environment for guest execution.")

(defvar-local macher-agent-sandbox--primitives nil
  "Store allowed host primitive operations.")

(defvar-local macher-agent-sandbox--functions nil
  "Store guest function definitions.")

(defconst macher-agent-sandbox--unbound (make-symbol "unbound")
  "Mark unbound variables in the sandbox environment.")

(declare-function macher-agent--ptc-primitive-p "macher-agent-orchestration" (sym))

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

(defun macher-agent-sandbox--eval-sync (expression env)
  "Synchronously evaluate EXPRESSION in ENV to bridge C-stack and CPS."
  (let ((iter (macher-agent-sandbox--eval-iter expression env))
        (yield-val nil))
    (condition-case err
        (while t
          (let ((step (iter-next iter yield-val)))
            (if (and (listp step) (eq (plist-get step :interrupt) 'tool-call))
                (error "Sandbox error: PTC tool calls are not allowed inside non-local exit blocks")
              (setq yield-val step))))
      (iter-end-of-sequence (cdr err)))))

(defun macher-agent-sandbox--condition-case-sync (args env)
  "Synchronous `condition-case' for ARGS in ENV hidden from generator compiler."
  (let ((var (car args))
        (bodyform (cadr args))
        (handlers (cddr args)))
    (condition-case err
        (macher-agent-sandbox--eval-sync bodyform env)
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
                 (setq result (macher-agent-sandbox--eval-sync form new-env))))
           (signal (car err) (cdr err))))))))

(defun macher-agent-sandbox--unwind-protect-sync (args env)
  "Synchronous `unwind-protect' for ARGS in ENV hidden from generator compiler."
  (unwind-protect
      (macher-agent-sandbox--eval-sync (car args) env)
    (dolist (form (cdr args))
      (macher-agent-sandbox--eval-sync form env))))

(defun macher-agent-sandbox--catch-sync (args env)
  "Synchronous catch for ARGS in ENV hidden from generator compiler."
  (catch (macher-agent-sandbox--eval-sync (car args) env)
    (let ((result nil))
      (dolist (form (cdr args) result)
        (setq result (macher-agent-sandbox--eval-sync form env))))))

(defun macher-agent-sandbox--throw-sync (args env)
  "Synchronous throw for ARGS in ENV hidden from generator compiler."
  (throw (macher-agent-sandbox--eval-sync (car args) env)
         (macher-agent-sandbox--eval-sync (cadr args) env)))

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
               ((and (fboundp 'macher-agent--ptc-primitive-p)
                     (macher-agent--ptc-primitive-p func))
                (iter-yield (list :interrupt 'tool-call
                                  :name func
                                  :args (macher-agent-sandbox--normalize-args-to-plist eval-args))))
               (prim (apply prim eval-args))
               (closure (iter-yield-from (macher-agent-sandbox--apply-closure-iter closure eval-args)))
               (t (error "Void or forbidden function: %s" func)))))

(iter-defun macher-agent-sandbox--funcall-iter (func eval-args)
            "Execute function FUNC with EVAL-ARGS in the sandbox."
            (cond
             ((and (consp func) (eq (car func) 'closure))
              (iter-yield-from (macher-agent-sandbox--apply-closure-iter func eval-args)))
             ((and (consp func) (eq (car func) 'lambda))
              (iter-yield-from
               (macher-agent-sandbox--apply-closure-iter
                (list 'closure (cadr func) (cddr func) nil) eval-args)))
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
                                   (macher-agent-sandbox--condition-case-sync args env)))
   (unwind-protect . ,(iter-lambda (args env)
                                   (macher-agent-sandbox--unwind-protect-sync args env)))
   (catch . ,(iter-lambda (args env)
                          (macher-agent-sandbox--catch-sync args env)))
   (throw . ,(iter-lambda (args env)
                          (macher-agent-sandbox--throw-sync args env)))
   (fboundp . ,(iter-lambda (args env)
                            (let ((sym (iter-yield-from
                                        (macher-agent-sandbox--eval-iter (car args) env))))
                              (when (symbolp sym)
                                (if (or (gethash sym macher-agent-sandbox--primitives)
                                        (gethash sym macher-agent-sandbox--functions)
                                        (and (fboundp 'macher-agent--ptc-primitive-p)
                                             (macher-agent--ptc-primitive-p sym))
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
                                             (and (fboundp 'macher-agent--ptc-primitive-p)
                                                  (macher-agent--ptc-primitive-p val))))
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
                     (fboundp 'macher-agent--ptc-primitive-p)
                     (macher-agent--ptc-primitive-p operator))
                (let
                    ((evaled-args (iter-yield-from
                                   (macher-agent-sandbox--eval-args-iter arguments environment))))
                  (iter-yield
                   (list :interrupt 'tool-call
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

(provide 'macher-agent-sandbox)
;;; macher-agent-sandbox.el ends here
