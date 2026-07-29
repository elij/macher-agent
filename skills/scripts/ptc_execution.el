(macher-agent-make-tool
 macher-agent-ptc-execution-tool
 "Execute an Emacs Lisp orchestration script. Use this to orchestrate multiple tools, spawn agents, and handle \
complex asynchronous workflows in a single step."
 :category "execution"
 :args
 '((
    :name "script"
    :type string
    :description "The Emacs Lisp script to execute. Use standard let*, mapcar, dolist, and permitted tool \
primitives."))
 :command-fn
 (lambda (payload context root)
   (if-let* ((script (plist-get payload :script)))
       (make-macher-agent-ptc-response :payload script)
     (error "No script provided for PTC execution"))))
