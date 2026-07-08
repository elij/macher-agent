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

(defvar-local macher-agent-sandbox--primitives nil
  "Allowed host operations.")

(defvar-local macher-agent-sandbox--functions nil
  "Guest functions.")

(defun macher-agent-sandbox--apply-closure (closure arguments)
  "Execute a CLOSURE with ARGUMENTS.

CLOSURE is the closure form to execute.
ARGUMENTS is the list of arguments to pass.

Return the evaluated result."
  (declare (ftype (function (list list) t)))
  (let ((new-env (cadddr closure))
        (params (cadr closure))
        (result nil))
    (while params
      (setq new-env (cons (cons (car params) (car arguments)) new-env)
            params (cdr params)
            arguments (cdr arguments)))
    (dolist (form (caddr closure) result)
      (setq result (macher-agent-sandbox--eval form new-env)))))

(defun macher-agent-sandbox--eval (expression environment)
  "Eval EXPRESSION in ENVIRONMENT.

EXPRESSION is the Lisp form to evaluate.
ENVIRONMENT is the association list of current variable bindings.

Return the evaluated result of the expression."
  (declare (ftype (function (t list) t)))
  (cond
   ((or (numberp expression) (stringp expression) (memq expression '(t nil)))
    expression)

   ((symbolp expression)
    (let ((binding (assoc expression environment)))
      (if binding
          (cdr binding)
        (error "Unbound variable: %s" expression))))

   ((consp expression)
    (let ((operator (car expression))
          (arguments (cdr expression)))
      (cond
       ((eq operator 'quote)
        (car arguments))

       ((memq operator '(lambda function))
        (if (eq operator 'function)
            (macher-agent-sandbox--eval (car arguments) environment)
          (list 'closure (car arguments) (cdr arguments) environment)))

       ((eq operator 'progn)
        (let ((result nil))
          (dolist (form arguments result)
            (setq result (macher-agent-sandbox--eval form environment)))))

       ((eq operator 'if)
        (if (macher-agent-sandbox--eval (car arguments) environment)
            (macher-agent-sandbox--eval (cadr arguments) environment)
          (macher-agent-sandbox--eval (caddr arguments) environment)))

       ((eq operator 'setq)
        (let ((args arguments)
              (val nil))
          (while args
            (let* ((var (car args))
                   (binding (assoc var environment)))
              (setq val (macher-agent-sandbox--eval (cadr args) environment))
              (if binding
                  (setcdr binding val)
                (error "Cannot setq unbound variable: %s" var)))
            (setq args (cddr args)))
          val))

       ((eq operator 'while)
        (let ((condition (car arguments))
              (body (cdr arguments)))
          (while (macher-agent-sandbox--eval condition environment)
            (dolist (form body)
              (macher-agent-sandbox--eval form environment)))
          nil))

       ((memq operator '(let let*))
        (let ((new-env environment)
              (result nil))
          (dolist (binding (car arguments))
            (let ((var (if (consp binding) (car binding) binding))
                  (val (if (consp binding) 
                           (macher-agent-sandbox--eval (cadr binding)
                                                       (if (eq operator 'let*) new-env environment)) 
                         nil)))
              (setq new-env (cons (cons var val) new-env))))
          (dolist (form (cdr arguments) result)
            (setq result (macher-agent-sandbox--eval form new-env)))))

       ;; allow inline funcs
       ((eq operator 'defalias)
        (let ((func-name (macher-agent-sandbox--eval (car arguments) environment))
              (func-body (macher-agent-sandbox--eval (cadr arguments) environment)))
          (puthash func-name func-body macher-agent-sandbox--functions)
          func-name))

       ((eq operator 'funcall)
        (let ((func (macher-agent-sandbox--eval (car arguments) environment))
              (eval-args (mapcar (lambda (arg) (macher-agent-sandbox--eval arg environment))
                                 (cdr arguments))))
          (cond
           ((and (consp func) (eq (car func) 'closure))
            (macher-agent-sandbox--apply-closure func eval-args))
           ((symbolp func)
            (cond
             ((gethash func macher-agent-sandbox--primitives)
              (apply (gethash func macher-agent-sandbox--primitives) eval-args))
             ((gethash func macher-agent-sandbox--functions)
              (macher-agent-sandbox--apply-closure (gethash func macher-agent-sandbox--functions) eval-args))
             (t (error "Void function: %s" func))))
           (t (error "Invalid funcall target: %s" func)))))

       (t
        (let ((eval-args (mapcar (lambda (arg) (macher-agent-sandbox--eval arg environment))
                                 arguments)))
          (cond
           ((and (symbolp operator) (gethash operator macher-agent-sandbox--primitives))
            (apply (gethash operator macher-agent-sandbox--primitives) eval-args))
           
           ((and (symbolp operator) (gethash operator macher-agent-sandbox--functions))
            (macher-agent-sandbox--apply-closure (gethash operator macher-agent-sandbox--functions) eval-args))
           
           ((consp operator)
            (let ((func (macher-agent-sandbox--eval operator environment)))
              (if (and (consp func) (eq (car func) 'closure))
                  (macher-agent-sandbox--apply-closure func eval-args)
                (error "Invalid operator evaluation: %s" operator))))
           
           (t (error "Void function: %s" operator))))))))))

(defun macher-agent-sandbox--run (expression extra-operations)
  "Expand EXPRESSION and execute in a sandboxed environment.

EXPRESSION is the Lisp form to evaluate.
EXTRA-OPERATIONS is a list of allowed host function symbols.

Return the evaluated result."
  (declare (ftype (function (t list) t)))
  (let ((expanded-expression (macroexpand-all expression))
        (macher-agent-sandbox--primitives (make-hash-table :test 'eq))
        (macher-agent-sandbox--functions (make-hash-table :test 'eq)))

    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym) (get sym 'pure))
         (puthash sym sym macher-agent-sandbox--primitives))))

    (dolist (op extra-operations)
      (puthash op op macher-agent-sandbox--primitives))

    (macher-agent-sandbox--eval expanded-expression nil)))

(defun macher-agent--sandbox-run (expression extra-operations)
  "Expand EXPRESSION and execute in a sandboxed environment.

EXPRESSION is the Lisp form to evaluate.
EXTRA-OPERATIONS is a list of allowed host function symbols.

Return the evaluated result."
  (declare (ftype (function (t list) t)))
  (macher-agent-sandbox--run expression extra-operations))

(provide 'macher-agent-sandbox)
;;; macher-agent-sandbox.el ends here
