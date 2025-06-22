
(pro-lisp-critic:deviation
 :construct-type :defparameter :construct-name "*test*"
 :rule :globals-need-docstring :rationale "test")
(defparameter *test* 3)
