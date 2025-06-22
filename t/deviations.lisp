(in-package :pro-lisp-critic/tests)

(fiveam:def-suite deviations-tests
  :description "Tests deviation functionality.")
(fiveam:in-suite deviations-tests)

(fiveam:test apply-function-deviation
  "Tests that a function deviation gets applied or not."
  (fiveam:is
   (equal
    nil
    (pro-lisp-critic:critique-file
      (asdf:system-relative-pathname
       "pro-lisp-critic"
       "t/testdata/apply-function-deviation-work.lisp"))))
  (fiveam:is
   (>
    (length
     (pro-lisp-critic:critique-file
      (asdf:system-relative-pathname
       "pro-lisp-critic"
       "t/testdata/apply-function-deviation-not.lisp")))
    0)))

(fiveam:test apply-macro-deviation
  "Tests that a macro deviation gets applied or not."
  (fiveam:is
   (equal
    nil
    (pro-lisp-critic:critique-file
      (asdf:system-relative-pathname
       "pro-lisp-critic"
       "t/testdata/apply-macro-deviation-work.lisp"))))
  (fiveam:is
   (>
    (length
     (pro-lisp-critic:critique-file
      (asdf:system-relative-pathname
       "pro-lisp-critic"
       "t/testdata/apply-macro-deviation-not.lisp")))
    0)))

(fiveam:test apply-defparameter-deviation
  "Tests that a defparameter deviation gets applied or not."
  (fiveam:is
   (equal
    nil
    (pro-lisp-critic:critique-file
      (asdf:system-relative-pathname
       "pro-lisp-critic"
       "t/testdata/apply-defparameter-deviation-work.lisp"))))
  (fiveam:is
   (>
    (length
     (pro-lisp-critic:critique-file
      (asdf:system-relative-pathname
       "pro-lisp-critic"
       "t/testdata/apply-defparameter-deviation-not.lisp")))
    0)))

(fiveam:test basic-functionality
  "Tests basic functionality."
  (let ((deviation (list 'pro-lisp-critic:deviation
                         :rule 'test
                         :construct-type 'function
                         :construct-name 'testfunc
                         :rationale "test")))
    (fiveam:is (equal (deviations::deviation-p deviation)
                      (list (rest deviation))))))
