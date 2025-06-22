
(pro-lisp-critic:deviation
 :construct-type :define-constant :construct-name "+test+"
 :rule :constants-need-docstring :rationale "test")
(alexandria:define-constant +test+ 3 :key #'equal)
