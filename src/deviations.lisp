(cl:defpackage #:deviations
  (:use #:common-lisp)
  (:export #:get-deviations-from-file
           #:filter-critiques)
  )

(in-package #:deviations)

(defun cleanup (item)
  (cond
    ((stringp item) (string-upcase item))
    ((symbolp item) (string item))
    (t (error "Not a string or symbol: ~S" item))))

(defun second-thing-p (critique code deviation construct-type)
  (and (equal (cleanup (getf deviation :construct-type)) construct-type)
          (equal (cleanup (getf deviation :construct-name))
                 (cleanup (second code)))
          (equal (cleanup (critique:critique-name critique))
                 (cleanup (getf deviation :rule)))))

(defun deviation-match-p (critique code deviation)
  (cond
    ((eql 'defun (first code))
     (second-thing-p critique code deviation "FUNCTION"))
    ((eql 'defmacro (first code))
     (second-thing-p critique code deviation "MACRO"))
    (t nil)))

(defun deviations-match-p (critique code deviations)
  (loop for deviation in deviations
        when (deviation-match-p critique code deviation)
          do (return-from deviations-match-p t))
  nil)

(defun filter-critiques (critiques code deviations)
  (remove-if (lambda (critique)
               (deviations-match-p critique code deviations))
             critiques))

(defun deviation-p (line)
  "Given a line from lisp code list LINE, return it if
   it is a deviation statement, nil if not."
  (and (string= "DEVIATION" (string (first line)))
       (list (rest line))))

(defun get-deviations-from-file (pathname)
  "Given simple-string PATHNAME, return a list
   of deviations in the file at pathname."
  (alexandria:mappend #'deviation-p
                      (uiop:read-file-forms pathname)))
