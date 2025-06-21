(cl:defpackage #:critique
  (:use #:common-lisp)
  (:export #:critique
           #:new-critique
           #:critique-name
           #:critique-blist
           #:critique-code)
  )

(in-package #:critique)

(defstruct (critique
            (:type list)
            (:constructor new-critique (name blist code)))
  name blist code)
