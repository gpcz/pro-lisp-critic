(cl:defpackage #:pro-lisp-critic
  (:use  #:common-lisp #:tables #:extend-match #:write-wrap)
  (:import-from #:extend-match #:match-and)
  (:export #:deviation
           #:critique-file
           #:print-critique-responses)
  )
