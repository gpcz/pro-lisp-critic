(in-package #:pro-lisp-critic)

(defmacro oneret (type &body body)
  `(the (values ,type &optional) ,@body))
