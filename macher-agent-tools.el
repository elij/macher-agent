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
  "Wrap tool execution to provide CONTEXT and safely route errors to the LLM via callback.
Extracts context from active FSM or falls back to persistent context for detached PTC execution."
  (declare (indent 1))
  (let ((fsm-sym (gensym "fsm-")))
    `(lambda (callback ,@args)
       (let* ((,fsm-sym (macher-agent-get-active-fsm))
              (context (or (when ,fsm-sym
                             (macher-agent-gptel-context-from-fsm ,fsm-sym))
                           (bound-and-true-p macher-agent--persistent-context)))
              (target-buf (when ,fsm-sym
                            (or (ignore-errors (plist-get (gptel-fsm-info ,fsm-sym) :buffer))
                                (when (fboundp 'gptel-fsm-buffer)
                                  (ignore-errors (gptel-fsm-buffer ,fsm-sym)))))))
         (condition-case err
             (if (and target-buf (buffer-live-p target-buf))
                 (with-current-buffer target-buf
                   (let ((res (progn ,@body)))
                     (when (and res (functionp callback))
                       (funcall callback res))
                     res))
               (let ((res (progn ,@body)))
                 (when (and res (functionp callback))
                   (funcall callback res))
                 res))
           (error
            (let ((err-msg (format "ERROR: %s" (error-message-string err))))
              (if (and target-buf (buffer-live-p target-buf))
                  (with-current-buffer target-buf
                    (when (functionp callback)
                      (funcall callback err-msg)))
                (when (functionp callback)
                  (funcall callback err-msg)))
              err-msg)))))))

;;; Instruction Queue

(defun macher-agent-add-pending-instruction (instruction)
  "Format dispatch queue targeting exclusive INSTRUCTION string parameter."
  (cl-check-type instruction string)
  (setq-local macher-agent--pending-instructions-queue
              (append macher-agent--pending-instructions-queue
                      (list (format "USER OVERRIDE DIRECTIVE:\n%s" instruction)))))

(provide 'macher-agent-tools)
;;; macher-agent-tools.el ends here
