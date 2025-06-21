(in-package :pro-lisp-critic/tests)

(defun run-test-suite ()
  "Runs the test suite(s) for deviations."
  (fiveam:run! 'deviations-tests))
