;;; ptc_execution.el --- Programmatic tool calling execution tool -*- lexical-binding: t; -*-
(setq macher-agent-ptc-execution-tool
      (gptel-make-tool
       :name "ptc_execution"
       :description "Execute an Emacs Lisp orchestration script. Use this to orchestrate multiple tools, spawn sub-agents, and handle complex asynchronous workflows in a single step."
       :category "execution"
       :include nil
       :args '((:name "script" :type "string" :description "The Emacs Lisp script to execute. Use standard let*, mapcar, dolist, and permitted tool primitives."))
       :async t
       :function (lambda (callback script)
                   (funcall (get 'macher-agent-ptc-execution-tool 'ptc-function)
                            script
                            macher-agent--persistent-context
                            callback))))

(put 'macher-agent-ptc-execution-tool 'ptc-function
     (lambda (script context callback)
       (macher-agent-execute-ptc-script
        script
        context
        (current-buffer)
        callback
        callback
        (plist-get (gptel-fsm-info (macher-agent-get-active-fsm)) :ptc-primitives))))
