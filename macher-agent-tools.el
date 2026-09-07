;;; macher-agent-tools.el --- Pure gptel orchestration tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Pure gptel orchestration tools for Macher Agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'macher-agent-core)
(require 'macher-agent-gptel)

;;; Customisation Variables

(put 'macher-agent--pending-instructions-queue 'permanent-local t)

;;; Execution Helpers

;;; Presentation Macro

(defmacro macher-agent-with-presentation-context (args &rest body)
  "Execute BODY with named ARGS and an injected `context` variable.
Strictly asserts the presence of an active gptel FSM and extracts
context directly."
  (declare (indent 1))
  (let ((cb-sym (gensym "cb-"))
        (fsm-sym (gensym "fsm-")))
    `(lambda (,cb-sym ,@args)
       (let* ((,fsm-sym (macher-agent-get-active-fsm))
              (context (if ,fsm-sym
                           (macher-agent-gptel-context-from-fsm ,fsm-sym)
                         (error "STRICT CONTRACT VIOLATION: Tool invoked outside of active gptel FSM")))
              (target-buf (when ,fsm-sym
                            (or (ignore-errors (plist-get (gptel-fsm-info ,fsm-sym) :buffer))
                                (when (fboundp 'gptel-fsm-buffer)
                                  (ignore-errors (gptel-fsm-buffer ,fsm-sym)))))))
         (condition-case err
             (if (and target-buf (buffer-live-p target-buf))
                 (with-current-buffer target-buf
                   (funcall ,cb-sym (progn ,@body)))
               (funcall ,cb-sym (progn ,@body)))
           (error
            (funcall ,cb-sym (format "Error executing tool: %s" (error-message-string err)))))))))

;;; Instruction Queue

(defun macher-agent-add-pending-instruction (instruction)
  "Format dispatch queue targeting exclusive INSTRUCTION string parameter."
  (cl-check-type instruction string)
  (setq-local macher-agent--pending-instructions-queue
              (append macher-agent--pending-instructions-queue
                      (list (format "USER OVERRIDE DIRECTIVE:\n%s" instruction)))))

(provide 'macher-agent-tools)
;;; macher-agent-tools.el ends here
