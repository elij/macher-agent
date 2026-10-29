;;; ptc_execution.el --- Programmatic tool calling execution tool -*- lexical-binding: t; -*-

(setq macher-agent-ptc-execution-tool
      (gptel-make-tool
       :name "ptc_execution"
       :description "Execute an Emacs Lisp orchestration script."
       :category "execution"
       :include nil
       :args '((:name "script" :type "string" :description "The Emacs Lisp script to execute."))
       :async t
       :function (lambda (callback script)
                   (let* ((fsm (macher-agent-get-active-fsm))
                          (info (when fsm (ignore-errors (gptel-fsm-info fsm))))
                          (primitives (when info (plist-get info :ptc-primitives)))
                          (origin-buf (or (when info (plist-get info :buffer))
                                          (current-buffer)))
                          (ctx (or (macher-agent-resolve-context origin-buf)
                                   macher-agent--persistent-context))
                          (deliver-to-gptel
                           (lambda (str-out)
                             (if (buffer-live-p origin-buf)
                                 (with-current-buffer origin-buf
                                   (funcall callback str-out))
                               (funcall callback str-out)))))
                     (funcall (get 'macher-agent-ptc-execution-tool 'ptc-function)
                              script
                              ctx
                              deliver-to-gptel
                              primitives)))))

(put 'macher-agent-ptc-execution-tool 'ptc-function
     (lambda (script context callback &optional primitives)
       (let ((target-buf (or (and (macher-agent-valid-context-p context)
                                  (macher-agent-context-owner-buffer context))
                             (current-buffer))))
         (macher-agent-execute-ptc-script
          script
          context
          target-buf
          (lambda (res)
            (funcall callback
                     (cond
                      ((stringp res) res)
                      ((null res) "nil")
                      ((and (listp res) (cl-every #'stringp res))
                       (string-join res "\n\n"))
                      ((vectorp res)
                       (string-join (mapcar (lambda (x) (format "%s" x)) (append res nil)) "\n\n"))
                      (t (prin1-to-string res)))))
          (lambda (err)
            (funcall callback
                     (cond
                      ((stringp err) (format "ERROR: %s" err))
                      ((and (listp err) (plist-get err :error))
                       (format "ERROR: %s" (plist-get err :error)))
                      (t (format "ERROR: %S" err)))))
          primitives))))
