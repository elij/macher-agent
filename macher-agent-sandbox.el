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

(defvar-local macher-agent-sandbox--globals nil
  "Global variable environment for guest execution.")

(defvar-local macher-agent-sandbox--primitives nil
  "Allowed host operations.")

(defvar-local macher-agent-sandbox--functions nil
  "Guest functions.")

(defun macher-agent-sandbox--apply-closure (closure arguments)
  "Execute a CLOSURE with ARGUMENTS.

CLOSURE is the closure form to execute.
ARGUMENTS is the list of arguments to pass"
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
  (cond
   ((or (numberp expression) (stringp expression) (memq expression '(t nil)))
    expression)

   ((symbolp expression)
    (let ((binding (assoc expression environment)))
      (if binding
          (cdr binding)
        (let* ((unbound-marker (make-symbol "unbound"))
               (val (gethash expression macher-agent-sandbox--globals unbound-marker)))
          (if (eq val unbound-marker)
              (error "Unbound variable: %s" expression)
            val)))))

   ((consp expression)
    (let ((operator (car expression))
          (arguments (cdr expression)))
      (cond
       ((eq operator 'quote)
        (car arguments))

       ((memq operator '(lambda function))
        (if (eq operator 'function)
            (let ((fn-target (car arguments)))
              (if (symbolp fn-target)
                  fn-target
                (macher-agent-sandbox--eval fn-target environment)))
          (list 'closure (car arguments) (cdr arguments) environment)))

       ((eq operator 'progn)
        (let ((result nil))
          (dolist (form arguments result)
            (setq result (macher-agent-sandbox--eval form environment)))))

       ((eq operator 'if)
        (if (macher-agent-sandbox--eval (car arguments) environment)
            (macher-agent-sandbox--eval (cadr arguments) environment)
          (macher-agent-sandbox--eval (caddr arguments) environment)))

       ((memq operator '(and or))
        (let ((args arguments)
              (is-and (eq operator 'and))
              (result (eq operator 'and))) 
          (while (and args (if is-and result (not result)))
            (setq result (macher-agent-sandbox--eval (car args) environment))
            (setq args (cdr args)))
          result))

       ((eq operator 'cond)
        (let ((clauses arguments)
              (result nil)
              (matched nil))
          (while (and clauses (not matched))
            (let* ((clause (car clauses))
                   (condition-result (macher-agent-sandbox--eval (car clause) environment)))
              (when condition-result
                (setq matched t)
                (if (cdr clause)
                    (dolist (form (cdr clause))
                      (setq result (macher-agent-sandbox--eval form environment)))
                  (setq result condition-result))))
            (setq clauses (cdr clauses)))
          result))

       ((eq operator 'setq)
        (let ((args arguments)
              (val nil))
          (while args
            (let* ((var (car args))
                   (binding (assoc var environment)))
              (setq val (macher-agent-sandbox--eval (cadr args) environment))
              (if binding
                  (setcdr binding val)
                (puthash var val macher-agent-sandbox--globals)))
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

       ((eq operator 'defalias)
        (let ((func-name (macher-agent-sandbox--eval (car arguments) environment))
              (func-body (macher-agent-sandbox--eval (cadr arguments) environment)))
          (puthash func-name func-body macher-agent-sandbox--functions)
          func-name))

       ((eq operator 'mapcar)
        (let ((func (macher-agent-sandbox--eval (car arguments) environment))
              (seq (macher-agent-sandbox--eval (cadr arguments) environment)))
          (mapcar (lambda (item)
                    (cond
                     ((and (consp func) (eq (car func) 'closure))
                      (macher-agent-sandbox--apply-closure func (list item)))
                     ((symbolp func)
                      (cond
                       ((gethash func macher-agent-sandbox--primitives)
                        (funcall (gethash func macher-agent-sandbox--primitives) item))
                       ((gethash func macher-agent-sandbox--functions)
                        (macher-agent-sandbox--apply-closure (gethash func macher-agent-sandbox--functions) (list item)))
                       (t (error "Void function in mapcar: %s" func))))
                     (t (error "Invalid function in mapcar: %s" func))))
                  seq)))

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
  (let ((expanded-expression (macroexpand-all expression))
        (macher-agent-sandbox--primitives (make-hash-table :test 'eq))
        (macher-agent-sandbox--functions (make-hash-table :test 'eq))
        (macher-agent-sandbox--globals (make-hash-table :test 'eq)))

    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym) (get sym 'pure))
         (puthash sym sym macher-agent-sandbox--primitives))))

    (dolist (op (append '(cons list string-to-list substring
                               expt string-upcase string= string< random format
                               string-match string-to-number number-to-string
                               int-to-string make-vector vector make-list append)
                        extra-operations))
      (puthash op op macher-agent-sandbox--primitives))

    (macher-agent-sandbox--eval expanded-expression nil)))

(provide 'macher-agent-sandbox)
;;; macher-agent-sandbox.el ends here
