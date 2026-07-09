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

(defconst macher-agent-sandbox--unbound (make-symbol "unbound")
  "Marker for unbound variables.")

(defun macher-agent-sandbox--eval-args (args env)
  "Evaluate a list of ARGS in ENV."
  (mapcar (lambda (arg) (macher-agent-sandbox--eval arg env)) args))

(defun macher-agent-sandbox--apply-closure (closure arguments)
  "Execute a CLOSURE with ARGUMENTS."
  (let ((new-env (cadddr closure))
        (params (cadr closure))
        (result nil))
    (while params
      (push (cons (pop params) (pop arguments)) new-env))
    (dolist (form (caddr closure) result)
      (setq result (macher-agent-sandbox--eval form new-env)))))

(defun macher-agent-sandbox--funcall (func eval-args)
  "Execute FUNC with EVAL-ARGS in the sandbox."
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
   (t (error "Invalid function target: %s" func))))

(defun macher-agent-sandbox--bind (bindings env is-star)
  "Process BINDINGS to extend ENV, supporting let* if IS-STAR is non-nil."
  (let ((new-env env))
    (dolist (binding bindings new-env)
      (let* ((is-cons (consp binding))
             (var (if is-cons (car binding) binding))
             (val (if is-cons (macher-agent-sandbox--eval (cadr binding) (if is-star new-env env)) nil)))
        (setq new-env (cons (cons var val) new-env))))))

(defun macher-agent-sandbox--eval-and-or (args env is-or)
  "Evaluate and/or ARGS in ENV, behaviour driven by IS-OR."
  (let ((result (not is-or)))
    (while (and args (if is-or (not result) result))
      (setq result (macher-agent-sandbox--eval (pop args) env)))
    result))

(defun macher-agent-sandbox--eval-let (args env is-star)
  "Evaluate let/let* ARGS in ENV, behaviour driven by IS-STAR."
  (let ((new-env (macher-agent-sandbox--bind (car args) env is-star))
        (result nil))
    (dolist (form (cdr args) result)
      (setq result (macher-agent-sandbox--eval form new-env)))))

(defvar-local macher-agent-sandbox--special-forms
  `((quote . ,(lambda (args _env) (car args)))
    (function . ,(lambda (args env)
                   (let ((fn-target (car args)))
                     (if (symbolp fn-target) fn-target (macher-agent-sandbox--eval fn-target env)))))
    (lambda . ,(lambda (args env)
                 (list 'closure (car args) (cdr args) env)))
    (progn . ,(lambda (args env)
                (let ((result nil))
                  (dolist (form args result)
                    (setq result (macher-agent-sandbox--eval form env))))))
    (if . ,(lambda (args env)
             (if (macher-agent-sandbox--eval (car args) env)
                 (macher-agent-sandbox--eval (cadr args) env)
               (macher-agent-sandbox--eval (caddr args) env))))
    (and . ,(lambda (args env) (macher-agent-sandbox--eval-and-or args env nil)))
    (or . ,(lambda (args env) (macher-agent-sandbox--eval-and-or args env t)))
    (cond . ,(lambda (args env)
               (let ((result nil) (matched nil))
                 (while (and args (not matched))
                   (let* ((clause (pop args))
                          (condition-result (macher-agent-sandbox--eval (car clause) env)))
                     (when condition-result
                       (setq matched t)
                       (if (cdr clause)
                           (dolist (form (cdr clause))
                             (setq result (macher-agent-sandbox--eval form env)))
                         (setq result condition-result)))))
                 result)))
    (setq . ,(lambda (args env)
               (let ((val nil))
                 (while args
                   (let* ((var (pop args))
                          (binding (assoc var env)))
                     (setq val (macher-agent-sandbox--eval (pop args) env))
                     (if binding
                         (setcdr binding val)
                       (puthash var val macher-agent-sandbox--globals))))
                 val)))
    (while . ,(lambda (args env)
                (while (macher-agent-sandbox--eval (car args) env)
                  (dolist (form (cdr args))
                    (macher-agent-sandbox--eval form env)))
                nil))
    (let . ,(lambda (args env) (macher-agent-sandbox--eval-let args env nil)))
    (let* . ,(lambda (args env) (macher-agent-sandbox--eval-let args env t)))
    (defalias . ,(lambda (args env)
                   (let ((func-name (macher-agent-sandbox--eval (car args) env))
                         (func-body (macher-agent-sandbox--eval (cadr args) env)))
                     (puthash func-name func-body macher-agent-sandbox--functions)
                     func-name)))
    (funcall . ,(lambda (args env)
                  (let ((func (macher-agent-sandbox--eval (car args) env))
                        (eval-args (macher-agent-sandbox--eval-args (cdr args) env)))
                    (macher-agent-sandbox--funcall func eval-args))))
    (apply . ,(lambda (args env)
                (let* ((func (macher-agent-sandbox--eval (car args) env))
                       (eval-args (macher-agent-sandbox--eval-args (cdr args) env))
                       (final-args (append (butlast eval-args) (car (last eval-args)))))
                  (macher-agent-sandbox--funcall func final-args))))
    (condition-case . ,(lambda (args env)
                         (let ((var (car args))
                               (bodyform (cadr args))
                               (handlers (cddr args)))
                           (condition-case err
                               (macher-agent-sandbox--eval bodyform env)
                             (error
                              (let ((matched-handler nil)
                                    (err-sym (car err)))
                                (dolist (handler handlers)
                                  (when (or (eq (car handler) t)
                                            (eq (car handler) err-sym)
                                            (and (listp (car handler)) (memq err-sym (car handler))))
                                    (setq matched-handler handler)))
                                (if matched-handler
                                    (let ((new-env (if var (cons (cons var err) env) env))
                                          (result nil))
                                      (dolist (form (cdr matched-handler) result)
                                        (setq result (macher-agent-sandbox--eval form new-env))))
                                  (signal (car err) (cdr err)))))))))
    (unwind-protect . ,(lambda (args env)
                         (unwind-protect
                             (macher-agent-sandbox--eval (car args) env)
                           (dolist (form (cdr args))
                             (macher-agent-sandbox--eval form env)))))
    (catch . ,(lambda (args env)
                (catch (macher-agent-sandbox--eval (car args) env)
                  (let ((result nil))
                    (dolist (form (cdr args) result)
                      (setq result (macher-agent-sandbox--eval form env)))))))
    (throw . ,(lambda (args env)
                (throw (macher-agent-sandbox--eval (car args) env)
                       (macher-agent-sandbox--eval (cadr args) env))))
    (fboundp . ,(lambda (args env)
                  (let ((sym (macher-agent-sandbox--eval (car args) env)))
                    (and (symbolp sym)
                         (or (gethash sym macher-agent-sandbox--primitives)
                             (gethash sym macher-agent-sandbox--functions)
                             (assoc sym macher-agent-sandbox--special-forms))))))
    (boundp . ,(lambda (args env)
                 (let ((sym (macher-agent-sandbox--eval (car args) env)))
                   (and (symbolp sym)
                        (or (assoc sym env)
                            (not (eq (gethash sym macher-agent-sandbox--globals macher-agent-sandbox--unbound)
                                     macher-agent-sandbox--unbound)))))))
    (macroexpand . ,(lambda (args env)
                      (let ((form (macher-agent-sandbox--eval (car args) env)))
                        (if (and (consp form) (symbolp (car form)))
                            (let ((macro-def (gethash (car form) macher-agent-sandbox--functions)))
                              (if (and (consp macro-def) (eq (car macro-def) 'macro))
                                  (macher-agent-sandbox--funcall (cdr macro-def) (cdr form))
                                form))
                          form))))
    (functionp . ,(lambda (args env)
                    (let ((val (macher-agent-sandbox--eval (car args) env)))
                      (or (and (consp val) (memq (car val) '(closure lambda macro)))
                          (and (symbolp val)
                               (or (gethash val macher-agent-sandbox--primitives)
                                   (gethash val macher-agent-sandbox--functions)))
                          (functionp val))))))
  "Alist mapping special form symbols to their handler functions.")

(defun macher-agent-sandbox--eval (expression environment)
  "Eval EXPRESSION in ENVIRONMENT."
  (cond
   ((or (numberp expression) (stringp expression) (memq expression '(t nil)))
    expression)

   ((symbolp expression)
    (if (keywordp expression)
        expression
      (let* ((binding (assoc expression environment))
             (val (unless binding (gethash expression macher-agent-sandbox--globals macher-agent-sandbox--unbound))))
        (when-let* ((_ (not binding))
                    (_ (eq val macher-agent-sandbox--unbound)))
          (error "Unbound variable: %s" expression))
        (if binding (cdr binding) val))))

   ((consp expression)
    (let* ((operator (car expression))
           (arguments (cdr expression))
           (special-handler (and (symbolp operator) (cdr (assoc operator macher-agent-sandbox--special-forms)))))
      (if special-handler
          (funcall special-handler arguments environment)
        (let ((macro-def (and (symbolp operator) (gethash operator macher-agent-sandbox--functions))))
          (if (and (consp macro-def) (eq (car macro-def) 'macro))
              (let* ((macro-fn (cdr macro-def))
                     (expanded-form (macher-agent-sandbox--funcall macro-fn arguments)))
                (macher-agent-sandbox--eval expanded-form environment))
            (let ((eval-args (macher-agent-sandbox--eval-args arguments environment)))
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

               (t (error "Void function: %s" operator)))))))))))

(defun macher-agent--sandbox-run (expression extra-operations)
  "Expand EXPRESSION and execute in a sandboxed environment."
  (let ((expanded-expression (macroexpand-all expression)))
    (setq macher-agent-sandbox--primitives (make-hash-table :test 'eq)
          macher-agent-sandbox--functions (make-hash-table :test 'eq)
          macher-agent-sandbox--globals (make-hash-table :test 'eq))

    (mapatoms
     (lambda (sym)
       (when (and (fboundp sym) (get sym 'pure))
         (puthash sym sym macher-agent-sandbox--primitives))))

    (puthash 'mapcar (lambda (f seq)
                       (mapcar (lambda (item) (macher-agent-sandbox--funcall f (list item))) seq))
             macher-agent-sandbox--primitives)
    
    (puthash 'mapc (lambda (f seq)
                     (mapc (lambda (item) (macher-agent-sandbox--funcall f (list item))) seq) seq)
             macher-agent-sandbox--primitives)

    (dolist (op extra-operations)
      (puthash op op macher-agent-sandbox--primitives))

    (macher-agent-sandbox--eval expanded-expression nil)))

(provide 'macher-agent-sandbox)
;;; macher-agent-sandbox.el ends here
