;;; macher-agent-api-test.el --- Tests for public API -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'macher-agent-api)

(ert-deftest macher-agent-api-contract-test ()
  "Ensure all public API bridge functions are defined."
  (should (fboundp 'macher-agent-workspace-resolve-path))
  (should (fboundp 'macher-agent-context-read))
  (should (fboundp 'macher-agent-context-update))
  (should (fboundp 'macher-agent-scope-add-file))
  (should (fboundp 'macher-agent-a2a-dispatch))
  (should (fboundp 'macher-agent-prepare-instructions))
  (should (fboundp 'macher-agent-submit-task-result))
  (should (fboundp 'macher-agent-sandbox-run))
  (should (fboundp 'macher-agent-workspace-root))
  (should (fboundp 'macher-agent-api-register-skills-in-directory))
  (should (fboundp 'macher-agent-ui-show)))


(ert-deftest macher-agent-sandbox-basic-test ()
  "Test that the sandboxed evaluator can evaluate basic expressions safely."
  (should (equal (macher-agent-sandbox-run 42 nil) 42))
  (should (equal (macher-agent-sandbox-run "test" nil) "test"))
  (should (equal (macher-agent-sandbox-run t nil) t))
  (should (equal (macher-agent-sandbox-run nil nil) nil))
  (should (equal (macher-agent-sandbox-run '(quote (1 2 3)) nil) '(1 2 3)))
  (should (equal (macher-agent-sandbox-run '(progn 1 2 3) nil) 3))
  (should (equal (macher-agent-sandbox-run '(if t 'yes 'no) nil) 'yes))
  (should (equal (macher-agent-sandbox-run '(if nil 'yes 'no) nil) 'no))
  (should (equal (macher-agent-sandbox-run '(let ((x 10) (y 20)) (progn (setq x 15) (+ x y))) '(+)) 35))
  (should (equal (macher-agent-sandbox-run '(funcall (lambda (x) (* x x)) 5) '(*)) 25)))

(ert-deftest macher-agent-sandbox-comprehensive-test ()
  "Test newly implemented sandboxed features."
  ;; Keyword self-evaluation
  (should (equal (macher-agent-sandbox-run ':foo nil) :foo))
  (should (equal (macher-agent-sandbox-run '(let ((x :bar)) x) nil) :bar))
  
  ;; Built-in and whitelisted functions
  (should (equal (macher-agent-sandbox-run '(not nil) nil) t))
  (should (equal (macher-agent-sandbox-run '(not t) nil) nil))
  (should (equal (macher-agent-sandbox-run '(reverse '(1 2 3)) nil) '(3 2 1)))
  (should (equal (macher-agent-sandbox-run '(split-string "foo bar" " ") nil) '("foo" "bar")))
  (should (equal (macher-agent-sandbox-run '(plist-get '(:a 1 :b 2) :b) nil) 2))
  
  ;; Functional application
  (should (equal (macher-agent-sandbox-run '(apply '+ 1 2 '(3 4)) '(+)) 10))
  (should (equal (macher-agent-sandbox-run '(apply '+ '(1 2 3)) '(+)) 6))
  (should (equal (macher-agent-sandbox-run '(mapcar (lambda (x) (* x 2)) '(1 2 3)) '(*)) '(2 4 6)))
  
  ;; Control flow and Error handling
  (should (equal (macher-agent-sandbox-run '(condition-case err (/ 1 0) (error 'caught)) '(/)) 'caught))
  (should (equal (macher-agent-sandbox-run '(unwind-protect 1 2) nil) 1))
  (should (equal (macher-agent-sandbox-run '(catch 'tag (throw 'tag 42)) nil) 42))
  
  ;; Introspection
  (should (equal (macher-agent-sandbox-run '(fboundp 'car) nil) t))
  (should (equal (macher-agent-sandbox-run '(fboundp 'nonexistent) nil) nil))
  (should (equal (macher-agent-sandbox-run '(let ((x 1)) (boundp 'x)) nil) t))
  (should (equal (macher-agent-sandbox-run '(boundp 'nonexistent) nil) nil))
  
  ;; Macros
  (should (equal (macher-agent-sandbox-run '(progn
                                              (defalias 'my-when '(macro lambda (cond &rest body)
                                                                         (list 'if cond (cons 'progn body))))
                                              (my-when t 42))
                                           nil)
                 42)))

(provide 'macher-agent-api-test)
