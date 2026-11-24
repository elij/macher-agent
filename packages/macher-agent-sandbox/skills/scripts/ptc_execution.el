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
                   (unless (fboundp 'macher-agent-sandbox-execute-ptc-script)
                     (error "Macher-Agent Sandbox plugin (`macher-agent-sandbox') is not loaded"))
                   (let* ((fsm (macher-agent-get-active-fsm))
                          (info (when fsm (ignore-errors (gptel-fsm-info fsm))))
                          (primitives (when info (plist-get info :ptc-primitives)))
                          (origin-buf (or (when info (plist-get info :buffer))
                                          (current-buffer)))
                          (ctx (or (macher-agent-resolve-context origin-buf)
                                   (bound-and-true-p macher-agent-persistent-context)))
                          (target-buf (or (when (and (bufferp origin-buf) (buffer-live-p origin-buf))
                                            origin-buf)
                                          (when (macher-agent-valid-context-p ctx)
                                            (let ((ob (macher-agent-context-owner-buffer ctx)))
                                              (when (and (bufferp ob) (buffer-live-p ob)) ob)))
                                          (current-buffer)))
                          (deliver-to-gptel
                           (lambda (str-out)
                             (if (buffer-live-p target-buf)
                                 (with-current-buffer target-buf
                                   (funcall callback str-out))
                               (funcall callback str-out)))))
                     (funcall (symbol-function 'macher-agent-sandbox-execute-ptc-script)
                              script
                              ctx
                              target-buf
                              (lambda (res)
                                (funcall deliver-to-gptel
                                         (cond
                                          ((stringp res) res)
                                          ((null res) "nil")
                                          ((and (listp res) (cl-every #'stringp res))
                                           (string-join res "\n\n"))
                                          ((vectorp res)
                                           (string-join (mapcar (lambda (x) (format "%s" x)) (append res nil)) "\n\n"))
                                          (t (prin1-to-string res)))))
                              (lambda (err)
                                (funcall deliver-to-gptel
                                         (cond
                                          ((stringp err) (format "ERROR: %s" err))
                                          ((and (listp err) (plist-get err :error))
                                           (format "ERROR: %s" (plist-get err :error)))
                                          (t (format "ERROR: %S" err)))))
                              primitives)))))
