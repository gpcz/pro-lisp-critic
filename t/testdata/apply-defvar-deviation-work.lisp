
(pro-lisp-critic:deviation
 :construct-type :defvar :construct-name "*test*"
 :rule :globals-need-docstring :rationale "test")
(pro-lisp-critic:deviation
 :construct-type :defvar :construct-name "*test*"
 :rule :no-defvars :rationale "test")
(defvar *test* 3)
