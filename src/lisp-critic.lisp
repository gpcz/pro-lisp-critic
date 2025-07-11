;;;-*- Mode: Lisp; Package: LISP-CRITIC -*-

#|
Copyright (C) 1997-2005 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the "Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
|#

;;; A Lisp code critiquing package.
;;; Author: Chris Riesbeck

(in-package #:pro-lisp-critic)

(defun deviation (&key rule construct-type construct-name rationale)
  "Placeholder for recording a Lisp Critic deviation in a file."
  (declare #.*external-optimize-settings*)
  (declare (ignore rule construct-type construct-name rationale))
  nil)

(defun critique-file
       (file &optional (names (get-pattern-names)))
  "Given pathname or simple-string FILE and list NAMES,
   return a list of all critique responses from the
   code in the given file."
  (declare #.*external-optimize-settings*)
  (check-type file (or pathname simple-string))
  (check-type names list)
  (oneret list
    (let ((deviations (deviations:get-deviations-from-file file))
          (result nil))
      (with-open-file (in file)
        (let ((eof (list nil)))
          (do ((code (read in nil eof) (read in nil eof)))
              ((eq code eof) (values))
            (let ((critiques
                    (critique-definition code deviations file
                                         :names names)))
              (when critiques
                (setf result (append result critiques)))))))
      result)))

(defun print-critique-responses (critiques
                                 &optional (stream *standard-output*))
  "Given list CRITIQUES and stream STREAM, print all
   critique responses in the list."
  (declare #.*external-optimize-settings*)
  (check-type critiques list)
  (check-type stream stream)
  (oneret t
    (let ((*print-pretty* nil))
      (when critiques
        (print-separator stream))
      (dolist (critique critiques)
        (print-critique-response critique stream)))))

(provide "lisp-critic")
