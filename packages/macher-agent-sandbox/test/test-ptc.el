;;; test-ptc.el --- Programmatic tool calling (PTC) sandbox evaluation -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Specification 8: Programmatic tool calling (PTC) sandbox evaluation
;; PTC-01, PTC-02, PTC-03, PTC-04, PTC-05, PTC-06, PTC-07

;;; Code:

(unless (featurep 'macher-agent-sandbox-test-setup)
  (load (expand-file-name "macher-agent-sandbox-test-setup.el"
                          (file-name-directory (or load-file-name buffer-file-name)))
        nil t))

(require 'cl-lib)
(require 'buttercup)
(require 'macher-agent-test-helpers)
(require 'macher-agent-sandbox)

(describe "8. Programmatic tool calling (PTC) sandbox evaluation"

  (before-each
    (macher-agent-test-reset-env)
    (macher-agent-sandbox-install))

  ;; --------------------------------------------------------------------------
  ;; PTC-01
  ;; --------------------------------------------------------------------------
  (it "PTC-01: evaluates let, lambda, mapcar, mapc, vectors accurately and blocks unauthorized host functions"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (cl-flet ((run-ptc (code)
                  (let ((res-val nil)
                        (err-val nil))
                    (macher-agent-sandbox-execute-ptc-script
                     code ctx owner-buf
                     (lambda (val) (setq res-val val))
                     (lambda (err) (setq err-val err)))
                    (or res-val (if err-val (error "%s" (plist-get err-val :error)) nil)))))

        ;; 1. let, let*, lambda
        (let ((val (run-ptc "(let* ((a 10) (b (+ a 5))) ((lambda (x) (* x 2)) b))")))
          (expect val :to-equal 30))

        ;; 2. mapcar and mapc over lists and vectors
        (let ((val-list (run-ptc "(mapcar (lambda (x) (* x 2)) '(1 2 3))"))
              (val-vec (run-ptc "(mapcar (lambda (x) (+ x 1)) [10 20 30])")))
          (expect val-list :to-equal '(2 4 6))
          (expect val-vec :to-equal '(11 21 31)))

        ;; 3. Vector literals [...]
        (let ((vec (run-ptc "[1 2 3 4]")))
          (expect (vectorp vec) :to-be t)
          (expect vec :to-equal [1 2 3 4]))

        ;; 4. condition-case and unwind-protect
        (let ((handled (run-ptc "(condition-case err (/ 1 0) (arith-error \"caught-div-by-zero\"))")))
          (expect handled :to-equal "caught-div-by-zero"))
        (let ((unwound (run-ptc "(let ((cleaned nil)) (unwind-protect (progn 42) (setq cleaned t)))")))
          (expect unwound :to-equal 42))

        ;; 5. Attempt to call unauthorized host function (delete-file)
        (expect (run-ptc "(delete-file \"/tmp/unauthorized-file.txt\")")
                :to-throw 'error))))

  ;; --------------------------------------------------------------------------
  ;; PTC-02
  ;; --------------------------------------------------------------------------
  (it "PTC-02: supplies vector for array args in PTC whether supplied as vector or list"
    (let* ((tool (or (bound-and-true-p macher-agent-delegate-tasks-to-subagents-tool)
                     (gptel-make-tool
                      :name "delegate_tasks_to_subagents"
                      :description "Delegate"
                      :args '((:name "tasks" :type "array")))))
           ;; Calling with vector literal
           (args-from-vector (macher-agent-sandbox--resolve-positional-args
                              tool (list [(:buffer_name "sub-1")])))
           ;; Calling with list literal
           (args-from-list (macher-agent-sandbox--resolve-positional-args
                            tool (list '((:buffer_name "sub-1"))))))
      ;; Both supply a vector matching direct gptel JSON deserialisation
      (expect (vectorp (car args-from-vector)) :to-be t)
      (expect (vectorp (car args-from-list)) :to-be t)
      (expect (length (car args-from-vector)) :to-equal 1)
      (expect (length (car args-from-list)) :to-equal 1)))

  ;; --------------------------------------------------------------------------
  ;; PTC-03
  ;; --------------------------------------------------------------------------
  (it "PTC-03: suspends generator on delegate_tasks_to_subagents and resumes with raw strings"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (let* ((script (concat "(let* ((answers (delegate_tasks_to_subagents\n"
                             "                 (vector (list :buffer_name \"*ptc03-w1*\" :presets [\"default\"] :instructions \"Task 1\")\n"
                             "                         (list :buffer_name \"*ptc03-w2*\" :presets [\"default\"] :instructions \"Task 2\")))))\n"
                             "  (format \"Combined: %s & %s\" (car answers) (cadr answers)))\n"))
             (ptc-result nil)
             (ptc-error nil))
        (with-current-buffer owner-buf
          (setq-local macher-agent-sandbox-active-ptc-primitives '(delegate_tasks_to_subagents))
          (cl-letf (((symbol-function 'gptel-send)
                     (lambda ()
                       (let* ((cur-buf (current-buffer))
                              (tid macher-agent-current-task-id)
                              (ans (if (string-match-p "w1" (buffer-name cur-buf)) "Answer-1" "Answer-2")))
                         (macher-agent-submit-task-result ans tid macher-agent-persistent-context)))))
            (macher-agent-sandbox-execute-ptc-script
             script ctx owner-buf
             (lambda (val) (setq ptc-result val))
             (lambda (err) (setq ptc-error err)))))

        ;; Await script resumption and final combination
        (macher-agent-test-wait-until (lambda () (or ptc-result ptc-error)) 5.0)
        (expect ptc-error :to-be nil)
        (expect ptc-result :to-equal "Combined: Answer-1 & Answer-2"))))

  ;; --------------------------------------------------------------------------
  ;; PTC-04
  ;; --------------------------------------------------------------------------
  (it "PTC-04: compares tool return value in PTC (raw Elisp struct) vs gptel (presentation string)"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (let* ((custom-tool
              (gptel-make-tool
               :name "get_diagnostics_tool"
               :description "Retrieve diagnostics"
               :category "execution"
               :args '((:name "target" :type "string"))
               :function (macher-agent-with-presentation-context (target)
                           (let* ((native-fn (get 'custom-tool 'ptc-function))
                                  (raw (funcall native-fn target context)))
                             (format "PRESENTATION: Found %d issues for %s"
                                     (plist-get raw :count) (plist-get raw :target))))))
             (tool-fn-ptc (lambda (target &optional _context)
                            (list :target target :count 3 :status 'warning))))
        (unwind-protect
            (progn
              (put 'custom-tool 'ptc-function tool-fn-ptc)
              (put 'get_diagnostics_tool 'ptc-function tool-fn-ptc)
              (puthash "get_diagnostics_tool" custom-tool macher-agent-tools-registry)

              ;; 1. Inside PTC execution: caller receives raw Elisp data structure
              (let ((ptc-result nil))
                (with-current-buffer owner-buf
                  (let ((macher-agent-sandbox-active-ptc-primitives '(get_diagnostics_tool)))
                    (macher-agent-sandbox-execute-ptc-script
                     "(get_diagnostics_tool \"src/lib.rs\")" ctx owner-buf
                     (lambda (val) (setq ptc-result val))
                     #'ignore)))
                (expect (listp ptc-result) :to-be t)
                (expect (plist-get ptc-result :target) :to-equal "src/lib.rs")
                (expect (plist-get ptc-result :count) :to-equal 3)
                (expect (plist-get ptc-result :status) :to-equal 'warning))

              ;; 2. Via gptel: callback receives formatted presentation string
              (let ((gptel-result nil)
                    (fsm (gptel-make-fsm :info (list :macher-agent-context ctx :buffer owner-buf))))
                (cl-letf (((symbol-function 'macher-agent-get-active-fsm) (lambda () fsm)))
                  (funcall (gptel-tool-function custom-tool)
                           (lambda (res) (setq gptel-result res))
                           "src/lib.rs"))
                (expect (stringp gptel-result) :to-be t)
                (expect gptel-result :to-equal "PRESENTATION: Found 3 issues for src/lib.rs")))
          (cl-remprop 'custom-tool 'ptc-function)
          (cl-remprop 'get_diagnostics_tool 'ptc-function)
          (remhash "get_diagnostics_tool" macher-agent-tools-registry)))))

  ;; --------------------------------------------------------------------------
  ;; PTC-05
  ;; --------------------------------------------------------------------------
  (it "PTC-05: resolves allowed primitives from explicit context and target-buf despite caller buffer binding"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (let ((caller-buf (generate-new-buffer "*ptc-05-caller*"))
            (run-res nil)
            (exec-res nil))
        (unwind-protect
            (progn
              ;; Target buffer permits custom_math_tool
              (with-current-buffer owner-buf
                (let ((math-tool (gptel-make-tool
                                  :name "custom_math_tool"
                                  :description "Math tool"
                                  :args '((:name "x" :type "integer")))))
                  (puthash "custom_math_tool" math-tool macher-agent-tools-registry))
                (setq-local macher-agent-sandbox-active-ptc-primitives '(custom_math_tool))
                (put 'custom_math_tool 'ptc-function (lambda (x &optional _ctx) (* x 10))))

              ;; Caller buffer has different buffer-local binding (or nil)
              (with-current-buffer caller-buf
                (setq-local macher-agent-sandbox-active-ptc-primitives '(unrelated_prim))

                ;; 1. macher-agent-sandbox-run with explicit context and target-buf
                (setq run-res (macher-agent-sandbox-run
                               '(custom_math_tool 5) '(custom_math_tool) ctx owner-buf))
                (expect (macher-agent-tool-call-p run-res) :to-be t)
                (expect (macher-agent-tool-call-name run-res) :to-equal 'custom_math_tool)
                (expect (macher-agent-tool-call-args run-res) :to-equal '(5))

                ;; 2. macher-agent-sandbox-execute-ptc-script with explicit context and target-buf
                (macher-agent-sandbox-execute-ptc-script
                 "(custom_math_tool 7)" ctx owner-buf
                 (lambda (val) (setq exec-res val))
                 #'ignore
                 '(custom_math_tool)))

              (macher-agent-test-wait-until (lambda () exec-res) 5.0)
              (expect exec-res :to-equal 70))
          (cl-remprop 'custom_math_tool 'ptc-function)
          (remhash "custom_math_tool" macher-agent-tools-registry)
          (when (buffer-live-p caller-buf)
            (kill-buffer caller-buf))))))

  ;; --------------------------------------------------------------------------
  ;; PTC-06
  ;; --------------------------------------------------------------------------
  (it "PTC-06: intercepts out-of-scope tools via gptel-pre-tool-call-functions and halts script"
    (macher-agent-test-with-git-workspace (root ctx owner-buf)
      (let* ((in-scope-tool (gptel-make-tool :name "allowed_math" :description "Math"))
             (gptel-tools (list in-scope-tool))
             (fsm (gptel-make-fsm :info (list :tools (list in-scope-tool) :buffer owner-buf)))
             (macher-agent-active-fsm fsm)
             (error-received nil))
        (unwind-protect
            (progn
              (put 'forbidden_system_tool 'ptc-function (lambda () (error "SHOULD NEVER RUN")))
              (with-current-buffer owner-buf
                (setq-local gptel-pre-tool-call-functions (list #'macher-agent-enforce-tool-scope))
                (setq-local macher-agent-sandbox-active-ptc-primitives '(forbidden_system_tool))

                (macher-agent-sandbox-execute-ptc-script
                 "(forbidden_system_tool)" ctx owner-buf
                 (lambda (_val) (error "Execution should have failed"))
                 (lambda (err) (setq error-received err))
                 '(forbidden_system_tool)))

              (macher-agent-test-wait-until (lambda () error-received) 5.0)
              (expect (plist-get error-received :status) :to-equal 'error)
              (expect (plist-get error-received :error) :to-match "not accessible in this context"))
          (cl-remprop 'forbidden_system_tool 'ptc-function)))))

  ;; --------------------------------------------------------------------------
  ;; PTC-07
  ;; --------------------------------------------------------------------------
  (it "PTC-07: manages PTC state in transmission pipeline during install vs uninstall"
    (with-temp-buffer
      (let* ((orig-buf (current-buffer))
             (tool (gptel-make-tool :name "ptc_worker" :description "Worker"))
             (base-state (make-macher-agent-transmission-state
                          :target-buffer orig-buf
                          :plugins '(:sandbox (:ptc-primitives (ptc_worker)))
                          :tools (list tool))))
        (unwind-protect
            (progn
              ;; 1. Sandbox installed: primitives stored under :sandbox and PTC prompt appended to directives
              (macher-agent-sandbox-install)
              (let ((state-installed (macher-agent-sandbox-append-ptc-to-transmission
                                      (copy-macher-agent-transmission-state base-state))))
                (let ((dirs (macher-agent-transmission-state-directives state-installed))
                      (sb-plugin (plist-get (macher-agent-transmission-state-plugins state-installed) :sandbox)))
                  (expect (plist-get sb-plugin :ptc-primitives) :to-equal '(ptc_worker))
                  (expect (length dirs) :to-be-greater-than 0)
                  (expect (cl-some (lambda (d) (string-match-p "PROGRAMMATIC TOOL CALLING (PTC)" d)) dirs)
                          :to-be-truthy)))

              ;; 2. Sandbox uninstalled: transmission runs with zero sandbox state or directives
              (macher-agent-sandbox-uninstall)
              (expect (member #'macher-agent-sandbox--inject-preset-ptc-tool
                              (macher-agent-get-pipeline-steps 'preset-composition))
                      :to-be nil)
              (expect (member #'macher-agent-sandbox-append-ptc-to-transmission
                              (macher-agent-get-pipeline-steps 'transmission))
                      :to-be nil))
          (macher-agent-sandbox-install)))))

  ;; --------------------------------------------------------------------------
  ;; PTC-08: Iterative trampoline avoids max-lisp-eval-depth across deep loops
  ;; --------------------------------------------------------------------------
  (it "PTC-08: executes 200 synchronous and mixed async tool calls via trampoline without stack overflow"
    (macher-agent-test-with-git-workspace (_root ctx owner-buf)
      (let* ((sync-tool (gptel-make-tool
                         :name "ptc08_sync_inc"
                         :description "Synchronously increment an integer"
                         :category "test"
                         :args '((:name "n" :type "integer" :description "Input"))
                         :function (lambda (n) (1+ n))))
             (async-tool (gptel-make-tool
                          :name "ptc08_async_double"
                          :description "Asynchronously double an integer"
                          :category "test"
                          :async t
                          :args '((:name "n" :type "integer" :description "Input"))
                          :function (lambda (cb n)
                                      (run-at-time 0.005 nil (lambda () (funcall cb (* 2 n)))))))
             (res-sync nil)
             (err-sync nil)
             (res-mixed nil)
             (err-mixed nil))
        (with-current-buffer owner-buf
          (setq-local gptel-tools (list sync-tool async-tool))
          (setq-local macher-agent-sandbox-active-ptc-primitives
                      '(ptc08_sync_inc ptc08_async_double))
          ;; 1. 200 synchronous tool calls in a while loop (previously crashed at ~45)
          (macher-agent-sandbox-execute-ptc-script
           "(let ((i 0) (acc 0)) (while (< i 200) (setq acc (ptc08_sync_inc acc)) (setq i (1+ i))) acc)"
           ctx owner-buf
           (lambda (r) (setq res-sync r))
           (lambda (e) (setq err-sync e)))
          (expect err-sync :to-be nil)
          (expect res-sync :to-equal 200)
          ;; 2. Mixed synchronous burst -> async suspension -> synchronous burst
          (macher-agent-sandbox-execute-ptc-script
           "(let ((i 0) (acc 0)) (while (< i 60) (setq acc (ptc08_sync_inc acc)) (setq i (1+ i))) (setq acc (ptc08_async_double acc)) (setq i 0) (while (< i 60) (setq acc (ptc08_sync_inc acc)) (setq i (1+ i))) acc)"
           ctx owner-buf
           (lambda (r) (setq res-mixed r))
           (lambda (e) (setq err-mixed e)))
          (macher-agent-test-wait-until (lambda () (or res-mixed err-mixed)) 5.0)
          (expect err-mixed :to-be nil)
          (expect res-mixed :to-equal 180)))))
)

(provide 'test-ptc)
;;; test-ptc.el ends here
