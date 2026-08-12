;;; macher-agent-test-harness.el --- Shared testing utilities for Macher Agent -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'gptel)
(require 'macher-agent-core)
(require 'macher-agent-vfs)
(require 'macher-agent-skills)
(require 'macher-agent-gptel)
(require 'macher-agent-orchestration)
(require 'macher-agent-api)

(defmacro with-macher-agent-mock-fsm (ctx &rest body)
  "Execute BODY synchronously while pretending an FSM is active with CTX."
  (declare (indent 1))
  `(let* ((mock-info (list :macher-agent-context ,ctx))
          (mock-fsm (if (fboundp 'gptel-make-fsm)
                        (gptel-make-fsm :state 'RUNNING :info mock-info)
                      mock-info))
          (macher-agent--active-fsm mock-fsm)
          (gptel--fsm-last mock-fsm))
     ,@body))

(defmacro with-macher-agent-test-context (routing-alist call-counter &rest body)
  "Execute BODY with gptel's network requests mocked using raw string responses.
ROUTING-ALIST maps buffer name substrings to a list of raw string responses.
CALL-COUNTER is a symbol bound in the calling environment that increments on dispatch."
  (declare (indent 2))
  `(let* ((queues (copy-tree ,routing-alist))
          (ws (make-macher-agent-workspace :project-root default-directory))
          (ctx (macher-agent--make-vfs-context :workspace ws :contents nil)))

     (setq-local macher-agent--persistent-context ctx)
     (puthash (expand-file-name default-directory) ctx macher-agent-active-workspaces)
     (puthash (expand-file-name (macher-agent-root default-directory)) ctx macher-agent-active-workspaces)
     (macher-agent-initialize-skills
      ctx (or (bound-and-true-p macher-agent--bundled-skills-dir)
              (bound-and-true-p macher-agent-bundled-skills-directory)))

     (let ((gptel-use-curl t)
           (gptel-confirm-tool-calls nil)
           (gptel-backend (gptel-make-openai "Mock" :key "mock-key" :models '(mock-model)))
           (gptel-model 'mock-model)
           (gptel-tools (hash-table-values (macher-agent-workspace-tools-registry ws))))

       (cl-letf (((symbol-function 'gptel--get-api-key) 
                  (lambda (&rest _) "mock-key"))
                 ((symbol-function 'gptel-curl-get-response)
                  (lambda (info-or-fsm &optional callback)
                    (let* ((fsm (if (and (fboundp 'gptel-fsm-p) (gptel-fsm-p info-or-fsm)) info-or-fsm nil))
                           (info (if fsm (gptel-fsm-info fsm) info-or-fsm))
                           (cb (or callback (plist-get info :callback) #'gptel--insert-response))
                           (target-buf (plist-get info :buffer))
                           (buf-name (if target-buf (buffer-name target-buf) (buffer-name)))
                           (queue-pair (cl-find-if (lambda (pair) (string-match-p (car pair) buf-name)) queues))
                           (resp (and queue-pair (pop (cdr queue-pair)))))
                      
                      (when fsm
                        (plist-put info :macher-agent-context ctx)
                        (setf (gptel-fsm-info fsm) info)
                        (when target-buf
                          (with-current-buffer target-buf
                            (setq-local gptel--fsm-last fsm))))
                      
                      (run-at-time 0.01 nil
                                   (lambda ()
                                     (when (buffer-live-p target-buf)
                                       (when fsm
                                         (plist-put info :http-status "200")
                                         (plist-put info :status "200 OK")
                                         (gptel--fsm-transition fsm))
                                       
                                       (if resp
                                           (progn
                                             (when (and fsm (plist-get resp :tool-use))
                                               (let ((tool-use-list nil))
                                                 (cl-loop for tool-req in (plist-get resp :tool-use)
                                                          for i from 1
                                                          do
                                                          (let* ((tool-name (car tool-req))
                                                                 (tool-vals (cdr tool-req))
                                                                 (tool-spec (or (cl-find-if (lambda (ts) (equal (gptel-tool-name ts) tool-name))
                                                                                            (plist-get info :tools))
                                                                                (ignore-errors (gptel-get-tool tool-name))))
                                                                 (expected-args (when tool-spec (gptel-tool-args tool-spec)))
                                                                 (args-plist nil))
                                                            (when tool-spec
                                                              (unless (cl-find-if (lambda (ts) (equal (gptel-tool-name ts) tool-name)) (plist-get info :tools))
                                                                (plist-put info :tools (append (plist-get info :tools) (list tool-spec)))
                                                                (setf (gptel-fsm-info fsm) info)))
                                                            (cl-loop for arg in expected-args
                                                                     for val in tool-vals
                                                                     do (setq args-plist (plist-put args-plist (intern (concat ":" (if (symbolp (plist-get arg :name)) (symbol-name (plist-get arg :name)) (plist-get arg :name)))) val)))
                                                            (push (list :id (format "call_%s_%d" buf-name i)
                                                                        :name tool-name
                                                                        :args args-plist)
                                                                  tool-use-list)))
                                                 (plist-put info :tool-use (nreverse tool-use-list))))
                                             
                                             (if (plist-get resp :text)
                                                 (funcall cb (plist-get resp :text) info)
                                               (funcall cb nil info)))
                                         (funcall cb "Fallback mock response." info))
                                       
                                       (when fsm
                                         (gptel--fsm-transition fsm))
                                       (cl-incf ,call-counter)))))))
                 ((symbol-function 'gptel--url-get-response)
                  (symbol-function 'gptel-curl-get-response)))
         ,@body))))

(provide 'macher-agent-test-harness)
;;; macher-agent-test-harness.el ends here
