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

(defvar-local macher-agent-sandbox--globals nil
  "Global variable environment for guest execution.")

(defvar-local macher-agent-sandbox--primitives nil
  "Allowed host operations.")

(defvar-local macher-agent-sandbox--functions nil
  "Guest functions.")

(defconst macher-agent-sandbox--unbound (make-symbol "unbound")
  "Marker for unbound variables.")

(declare-function macher-agent--ptc-primitive-p "macher-agent-orchestration" (sym))

(defun macher-agent-sandbox--init (&optional extra-operations)
  "Initialize the sandbox environment if not already set up.
EXTRA-OPERATIONS is an optional list of additional primitive symbols to permit."
  (unless (and macher-agent-sandbox--primitives
               (hash-table-p macher-agent-sandbox--primitives)
               (> (hash-table-count macher-agent-sandbox--primitives) 0))
    (setq macher-agent-sandbox--primitives (make-hash-table :test 'eq)
          macher-agent-sandbox--functions (make-hash-table :test 'eq)
          macher-agent-sandbox--globals (make-hash-table :test 'eq))
    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym)
                  (or (get sym 'pure)
                      (get sym 'side-effect-free)))
         (puthash sym sym macher-agent-sandbox--primitives))))
    (puthash 'mapcar (lambda (f seq)
                       (mapcar (lambda (item) (macher-agent-sandbox--funcall f (list item))) seq))
             macher-agent-sandbox--primitives)
    (puthash 'mapc (lambda (f seq)
                     (mapc (lambda (item) (macher-agent-sandbox--funcall item (list item))) seq) seq)
             macher-agent-sandbox--primitives))
  (when extra-operations
    (dolist (op extra-operations)
      (puthash op op macher-agent-sandbox--primitives))))

(defun macher-agent-sandbox--run-sync (iterator)
  "Pump ITERATOR to completion synchronously.
Catches iter-end-of-sequence to return the final value safely.
Throws an error if the script attempts an async PTC yield."
  (condition-case err
      (let ((yield-val nil))
        (while t
          (let ((res (iter-next iterator yield-val)))
            (if (and (consp res) (eq (plist-get res :interrupt) 'tool-call))
                (error "Execution Error: Script attempted to call async PTC tool '%s' inside a synchronous evaluation context" (plist-get res :name))
              (setq yield-val res)))))
    (iter-end-of-sequence (cdr err))))

(iter-defun macher-agent-sandbox--eval-args-iter (args env)
	    "Evaluate a list of ARGS in ENV using the generator evaluator."
	    (let ((evaled nil))
	      (dolist (arg args (nreverse evaled))
		(push (iter-yield-from (macher-agent-sandbox--eval-iter arg env)) evaled))))

(defun macher-agent-sandbox--eval-args (args env)
  "Evaluate a list of ARGS in ENV synchronously."
  (macher-agent-sandbox--run-sync (macher-agent-sandbox--eval-args-iter args env)))

(iter-defun macher-agent-sandbox--apply-closure-iter (closure arguments)
	    "Execute CLOSURE with ARGUMENTS."
	    (let ((new-env (cadddr closure))
		  (params (cadr closure))
		  (result nil))
	      (while params
		(push (cons (pop params) (pop arguments)) new-env))
	      (dolist (form (caddr closure) result)
		(setq result (iter-yield-from (macher-agent-sandbox--eval-iter form new-env))))))

(defun macher-agent-sandbox--apply-closure (closure arguments)
  "Execute CLOSURE with ARGUMENTS synchronously."
  (macher-agent-sandbox--run-sync (macher-agent-sandbox--apply-closure-iter closure arguments)))

(iter-defun macher-agent-sandbox--funcall-iter (func eval-args)
	    "Execute FUNC with EVAL-ARGS in the sandbox."
	    (cond
	     ((and (consp func) (eq (car func) 'closure))
	      (iter-yield-from (macher-agent-sandbox--apply-closure-iter func eval-args)))
	     ((symbolp func)
	      (if (and (fboundp 'macher-agent--ptc-primitive-p)
		       (macher-agent--ptc-primitive-p func))
		  (iter-yield (list :interrupt 'tool-call
				    :name func
				    :args eval-args))
		(if-let* ((prim (and macher-agent-sandbox--primitives (gethash func macher-agent-sandbox--primitives))))
		    (apply prim eval-args)
		  (if-let* ((closure (and macher-agent-sandbox--functions (gethash func macher-agent-sandbox--functions))))
		      (iter-yield-from (macher-agent-sandbox--apply-closure-iter closure eval-args))
		    (error "Void or forbidden function: %s" func)))))
	     (t (error "Invalid function target: %s" func))))

(defun macher-agent-sandbox--funcall (func eval-args)
  "Execute FUNC with EVAL-ARGS in the sandbox synchronously."
  (macher-agent-sandbox--run-sync (macher-agent-sandbox--funcall-iter func eval-args)))

(iter-defun macher-agent-sandbox--bind-iter (bindings env is-star)
	    "Process BINDINGS to extend ENV, supporting let* if IS-STAR is non-nil."
	    (let ((new-env env))
	      (dolist (binding bindings new-env)
		(let ((var (if (consp binding) (car binding) binding))
		      (val (when (consp binding)
			     (iter-yield-from
			      (macher-agent-sandbox--eval-iter (cadr binding) (if is-star new-env env))))))
		  (setq new-env (cons (cons var val) new-env))))))

(defun macher-agent-sandbox--bind (bindings env is-star)
  "Process BINDINGS to extend ENV synchronously, supporting let* if IS-STAR is non-nil."
  (macher-agent-sandbox--run-sync (macher-agent-sandbox--bind-iter bindings env is-star)))

(iter-defun macher-agent-sandbox--eval-and-or-iter (args env is-or)
	    "Evaluate and or ARGS in ENV, behaviour driven by IS-OR."
	    (let ((result (not is-or)))
	      (while (and args (if is-or (not result) result))
		(setq result (iter-yield-from (macher-agent-sandbox--eval-iter (pop args) env))))
	      result))

(defun macher-agent-sandbox--eval-and-or (args env is-or)
  "Evaluate and or ARGS in ENV synchronously, behaviour driven by IS-OR."
  (macher-agent-sandbox--run-sync (macher-agent-sandbox--eval-and-or-iter args env is-or)))

(iter-defun macher-agent-sandbox--eval-let-iter (args env is-star)
	    "Evaluate let or let* ARGS in ENV, behaviour driven by IS-STAR."
	    (let ((new-env (iter-yield-from (macher-agent-sandbox--bind-iter (car args) env is-star)))
		  (result nil))
	      (dolist (form (cdr args) result)
		(setq result (iter-yield-from (macher-agent-sandbox--eval-iter form new-env))))))

(defun macher-agent-sandbox--eval-let (args env is-star)
  "Evaluate let or let* ARGS in ENV synchronously, behaviour driven by IS-STAR."
  (macher-agent-sandbox--run-sync (macher-agent-sandbox--eval-let-iter args env is-star)))

(defvar-local macher-agent-sandbox--special-forms
  `((quote . ,(iter-lambda (args _env) (car args)))
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
			      (let ((clause (pop args)))
				(when-let* ((condition-result (iter-yield-from (macher-agent-sandbox--eval-iter (car clause) env))))
				  (setq matched t)
				  (if (cdr clause)
				      (dolist (form (cdr clause))
					(setq result (iter-yield-from (macher-agent-sandbox--eval-iter form env))))
				    (setq result condition-result)))))
			    result)))
    (setq . ,(iter-lambda (args env)
			  (let ((val nil))
			    (while args
			      (let ((var (pop args)))
				(setq val (iter-yield-from (macher-agent-sandbox--eval-iter (pop args) env)))
				(if-let* ((binding (assoc var env)))
				    (setcdr binding val)
				  (puthash var val macher-agent-sandbox--globals))))
			    val)))
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
					 (let ((matched-handler nil)
					       (err-sym (car err)))
					   (dolist (handler handlers)
					     (when (or (eq (car handler) t)
						       (eq (car handler) err-sym)
						       (and (listp (car handler)) (memq err-sym (car handler))))
					       (setq matched-handler handler)))
					   (if-let* ((handler matched-handler))
					       (let ((new-env (if var (cons (cons var err) env) env))
						     (result nil))
						 (dolist (form (cdr handler) result)
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
			     (when-let* ((sym (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
					 (_ (symbolp sym)))
			       (or (gethash sym macher-agent-sandbox--primitives)
				   (gethash sym macher-agent-sandbox--functions)
				   (and (fboundp 'macher-agent--ptc-primitive-p)
					(macher-agent--ptc-primitive-p sym))
				   (assoc sym macher-agent-sandbox--special-forms)))))
    (boundp . ,(iter-lambda (args env)
			    (when-let* ((sym (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env)))
					(_ (symbolp sym)))
			      (or (assoc sym env)
				  (not (eq (gethash sym macher-agent-sandbox--globals macher-agent-sandbox--unbound)
					   macher-agent-sandbox--unbound))))))
    (macroexpand . ,(iter-lambda (args env)
				 (let ((form (iter-yield-from (macher-agent-sandbox--eval-iter (car args) env))))
				   (if-let* ((_ (and (consp form) (symbolp (car form))))
					     (macro-def (gethash (car form) macher-agent-sandbox--functions))
					     (_ (and (consp macro-def) (eq (car macro-def) 'macro))))
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
  "Alist mapping special form symbols to their handler functions.")

(iter-defun macher-agent-sandbox--eval-iter (expression environment)
	    "Evaluate EXPRESSION in ENVIRONMENT, yielding on PTC tool calls."
	    (macher-agent-sandbox--init)
	    (cond
	     ((or (numberp expression) (stringp expression) (memq expression '(t nil)))
	      expression)

	     ((symbolp expression)
	      (if (keywordp expression)
		  expression
		(if-let* ((binding (assoc expression environment)))
		    (cdr binding)
		  (let ((val (gethash expression macher-agent-sandbox--globals macher-agent-sandbox--unbound)))
		    (when (eq val macher-agent-sandbox--unbound)
		      (error "Unbound variable: %s" expression))
		    val))))

	     ((consp expression)
	      (let ((operator (car expression))
		    (arguments (cdr expression)))
		(if (and (symbolp operator) (macher-agent--ptc-primitive-p operator))
		    (let ((evaled-args (iter-yield-from (macher-agent-sandbox--eval-args-iter arguments environment))))
		      (iter-yield (list :interrupt 'tool-call
					:name operator
					:args evaled-args)))
		  (if-let* ((special-handler (and (symbolp operator) (cdr (assoc operator macher-agent-sandbox--special-forms)))))
		      (iter-yield-from (funcall special-handler arguments environment))
		    (let ((macro-def (and (symbolp operator) (gethash operator macher-agent-sandbox--functions))))
		      (if (and (consp macro-def) (eq (car macro-def) 'macro))
			  (let* ((macro-fn (cdr macro-def))
				 (expanded-form (iter-yield-from (macher-agent-sandbox--funcall-iter macro-fn arguments))))
			    (iter-yield-from (macher-agent-sandbox--eval-iter expanded-form environment)))
			(let ((evaled-args (iter-yield-from (macher-agent-sandbox--eval-args-iter arguments environment))))
			  (iter-yield-from (macher-agent-sandbox--funcall-iter operator evaled-args)))))))))))

(defun macher-agent-sandbox--eval (expression environment)
  "Evaluate EXPRESSION in ENVIRONMENT synchronously."
  (macher-agent-sandbox--run-sync (macher-agent-sandbox--eval-iter expression environment)))

(defun macher-agent--sandbox-run (expression extra-operations)
  "Expand EXPRESSION and execute in a sandboxed environment.

EXPRESSION is the Lisp expression to expand and evaluate.
EXTRA-OPERATIONS is a list of additional primitive symbols to permit."
  (let ((expanded-expression (macroexpand-all expression)))
    (setq macher-agent-sandbox--primitives (make-hash-table :test 'eq)
          macher-agent-sandbox--functions (make-hash-table :test 'eq)
          macher-agent-sandbox--globals (make-hash-table :test 'eq))
    (macher-agent-sandbox--init extra-operations)
    (macher-agent-sandbox--eval expanded-expression nil)))

(provide 'macher-agent-sandbox)
;;; macher-agent-sandbox.el ends here
