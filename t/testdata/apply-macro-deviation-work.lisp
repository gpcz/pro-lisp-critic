
(pro-lisp-critic:deviation
 :construct-type :macro :construct-name "test"
 :rule :required-docstring :rationale "test")
(defmacro test (a)
  3)
