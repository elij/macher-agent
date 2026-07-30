(macher-agent-make-tool
    macher-agent-ptc-execution-tool
    "Execute an Emacs Lisp orchestration script. Use this to orchestrate multiple tools, \
spawn sub-agents, and handle complex asynchronous workflows in a single step."
  :category "execution"
  :args
  '((
     :name "script"
     :type string
     :description "The Emacs Lisp script to execute. Use standard let*, mapcar, dolist, \
and permitted tool primitives."))
  :command-fn
  (lambda (payload _context _root)
    (if-let*
        ((script (plist-get payload :script)))
        (make-macher-agent-ptc-response
         :payload script
         :primitives
         '(
           nreverse sort delete delq nconc plist-put aset puthash remhash
           error signal message random emacs-version))
      (error "No script provided for PTC execution"))))
