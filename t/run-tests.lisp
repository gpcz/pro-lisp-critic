(in-package :pro-lisp-critic/tests)

(defun run-test-suite ()
  "Runs the test suite(s) for deviations."
  (let ((test-results (fiveam:run 'deviations-tests)))
    (fiveam:explain! test-results)
    (when (find-if (lambda (item)
                     (equal (type-of item) 'it.bese.fiveam::test-failure))
                   test-results)
      (uiop:quit 1))))
