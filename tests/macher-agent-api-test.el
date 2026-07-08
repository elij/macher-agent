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
  (should (fboundp 'macher-agent-execute-parallel))
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

(provide 'macher-agent-api-test)
