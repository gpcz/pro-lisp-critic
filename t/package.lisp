(defpackage :pro-lisp-critic/tests
  #+:genera
  (:shadowing-import-from :common-lisp :lambda :simple-string :string)
  (:use #-:genera :cl #+:genera :future-common-lisp
   :fiveam :pro-lisp-critic)
  (:export run!))
