(cl:defpackage #:critique
  (:use #:common-lisp)
  (:export #:critique
           #:new-critique
           #:critique-name
           #:critique-blist
           #:critique-code
           #:critique-file
           #:critique-construct)
  )

(in-package #:critique)

(defstruct (critique
            (:type list)
            (:constructor new-critique (name blist code file construct)))
  name blist code file construct)
