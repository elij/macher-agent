;; -*- lexical-binding: t; -*-
(macher-agent-make-tool
    macher-agent-ptc-execution-tool
    "Execute an Emacs Lisp orchestration script. Use this to orchestrate multiple tools, \
spawn sub-agents, and handle complex asynchronous workflows in a single step."
  :category "execution"
  :include nil
  :args
  '((
     :name "script"
     :type string
     :description "The Emacs Lisp script to execute. Use standard let*, mapcar, dolist, \
and permitted tool primitives."))
  :command-fn
  (lambda (payload context _root callback)
    (if-let*
        ((script (plist-get payload :script)))
        (let ((target-buf (or (when (macher-agent-context-p context)
                                (macher-agent-context-origin-buffer context))
                              (current-buffer))))
          (macher-agent-execute-ptc-script
           script context target-buf
           (lambda (res) (funcall callback res))
           (lambda (err) (funcall callback err))))
      (error "No script provided for PTC execution"))))
