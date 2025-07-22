(in-package #:pro-lisp-critic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables and tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftable get-pattern)
(deftable get-response)

(defparameter *length-threshold* 55)

(deftable get-local-vars-fn)
(deftable get-assigned-vars-fn)

;;; used by ?TOP-LEVEL ,set by FIND-CRITIQUE
(defvar *top-level* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function () (values list &optional)) clear-critique-db))
(defun clear-critique-db ()
  (declare #.*internal-optimize-settings*)
  (clear-table (get-pattern))
  (clear-table (get-response))
  nil)

(defparameter *output-width* 70)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (response
            (:type list)
            (:constructor new-response (format-string args)))
  format-string args)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defining Lisp patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-lisp-pattern (name pattern format-string &rest args)
  (unless (symbolp name)
    (error "Non-symbolic Lisp pattern name ~S" name))
  `(add-lisp-pattern ',name ',pattern ,format-string ',args))

(declaim (ftype (function (symbol list simple-string list)
                          (values symbol &optional))
                add-lisp-pattern))
(defun add-lisp-pattern (name pat format-string args)
  (declare #.*internal-optimize-settings*)
  (setf (get-pattern name) pat)
  (setf (get-response name) (new-response format-string args))
  name)

(declaim (ftype (function () (values list &optional))
                get-pattern-names))
(defun get-pattern-names ()
  (declare #.*internal-optimize-settings*)
  (let ((l nil))
    (map-table #'(lambda (name pat)
                   (declare (ignore pat))
                   (push name l))
               (get-pattern))
    (sort l #'string<)))

(declaim (ftype (function (symbol) (values t &optional))
                remove-lisp-pattern))
(defun remove-lisp-pattern (name)
  (declare #.*internal-optimize-settings*)
  (remove-key name (get-pattern))
  (remove-key name (get-response)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CRITIQUE, -DEFINITION, CRITIQUE-FILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (list list (or simple-string pathname)
                                &key (:names list))
                          (values list &optional))
                critique-definition))
(defun critique-definition
      (defn deviations file &key (names (get-pattern-names)))
  (declare #.*internal-optimize-settings*)
  (cond ((or (atom defn)
             (and (eql (car defn) 'quote)
                  (or (atom (cadr defn))
                      (and  (eql (caadr defn) 'quote)
                            (atom (cadadr defn))))))
         (error "Can't critique ~S -- I need the actual definition~%"
                 defn))
        ((null names)
         (error "No lisp rules have been loaded."))
        (t
         (deviations:filter-critiques
          (generate-critiques defn names file defn)
          defn deviations))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (list list (or simple-string pathname) list)
                          (values list &optional))
                generate-critiques))
(defun generate-critiques (code names file construct)
  (declare #.*internal-optimize-settings*)
  (loop for name of-type symbol in names
        append (apply-critique-rule name code file construct)))

(declaim (ftype (function (symbol list (or pathname simple-string) list)
                          (values list &optional))
                apply-critique-rule))
(defun apply-critique-rule (name code file construct)
  (declare #.*internal-optimize-settings*)
  (find-critiques name (get-pattern name) code file construct
                  :blists '(nil) :top-level t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIND-CRITIQUES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-critiques (name pat code file construct
                       &key (blists '(nil))
                         ((:top-level *top-level*) *top-level*))
  (declare #.*internal-optimize-settings*)
  (let ((new-blists (critique-match pat code blists)))
    (cond ((not (null new-blists))
           (make-critiques
            name new-blists code file construct))
      ((atom code) nil)
       (t
       (or (find-critiques
            name pat (car code) :blists blists)
           (find-critiques
            name pat (cdr code) :blists blists))))))


(defun critique-match (pat code blists)
  (declare #.*internal-optimize-settings*)
  (pat-match pat code blists))

(defun make-critiques (name blists code file construct)
  (declare #.*internal-optimize-settings*)
  (mapcar #'(lambda (blist)
              (critique:new-critique name blist code
                                     file construct))
          blists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Critique message printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (cons &optional stream) *)
                print-critique-response))
(defun print-critique-response (critique
                                &optional (stream *standard-output*))
  (declare #.*internal-optimize-settings*)
  (let ((name (critique:critique-name critique))
        (blist (critique:critique-blist critique))
        (code (critique:critique-code critique))
        (file (critique:critique-file critique))
        (construct (critique:critique-construct critique)))
    (let ((response (get-response name)))
      (cond ((null response)
             (let ((*print-lines* 2) (*print-pretty* t)
                   (*print-right-margin* *output-width*))
               (format stream "~&~A: Code: ~W" name code)))
            (t
             (write-wrap stream
                         (namestring file)
                         *output-width*)
             (princ construct stream)
             (format stream "~%~%")
             (write-wrap stream
                         (make-response-string name response blist)
                         *output-width*)))
      (print-separator stream))))

(declaim (ftype (function (symbol cons list)
                          (values simple-string &optional))
                make-response-string))
(defun make-response-string (name response blist)
  "Given symbol NAME, cons RESPONSE, and list BLIST,
   return a string with the critique response."
  (declare #.*internal-optimize-settings*)
  (format nil "~A:~&~?"
          name
          (response-format-string response)
          (instantiate-pattern (response-args response)
                               blist)))

;;; the following can be done with
;;;
;;;   (format stream "~&~V,,,V<~:*~A~>~%" *output-width* ch)
;;;
;;; but XlispStat 3.50 doesn't handle that and everyone has
;;; to run to Steele to see what it does.


(defun print-separator (&optional (stream *standard-output*)
                                  (ch #\-))
  (declare #.*internal-optimize-settings*)
  (format stream "~&~A~%"
    (make-string *output-width* :initial-element ch)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matcher extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; General extensions -- useful lots of places

;;; (?CONTAINS pat) -- matches anything containing something matching
;;;    pat

(add-extension '?contains :single 'match-contains)

(defun match-contains (args input blists)
  (declare #.*internal-optimize-settings*)
  (destructuring-bind (pat) args
    (find-match pat input blists)))

(defun find-match (pat input blists)
  (declare #.*internal-optimize-settings*)
  (or (pat-match pat input blists)
      (and (consp input)
           (or (find-match pat (first input) blists)
               (find-match pat (rest input) blists)))))


;;; (?REPEAT pat [n]) -- matches N or more occurrences of pat;
;;;   N defaults to 1

(add-extension '?repeat :segment 'match-repeat)

(defun match-repeat (args pats input blists)
  (declare #.*internal-optimize-settings*)
  (and (not (null input))
       (destructuring-bind (pat &optional (n 1)) args
         (match-repeat-pat n pat pats input blists))))

(defun match-repeat-pat (n pat pats input blists)
  (declare #.*internal-optimize-settings*)
  (unless (null input)
    (let ((blists (pat-match pat (first input) blists)))
      (cond ((null blists) nil)
            ((> n 1)
             (match-repeat-pat (1- n) pat pats (rest input) blists))
            (t (append (pat-match pats (rest input) blists)
                       (match-repeat-pat n pat pats (rest input) blists)
                       ))))))


;;; (?OPTIONAL pat) -- matches zero or one occurrences of pat

(add-extension '?optional :segment 'match-optional)

(defun match-optional (args pats input blists)
  (declare #.*internal-optimize-settings*)
  (let ((skip-blists (pat-match pats input blists))
        (no-skip-blists
         (and (not (null input))
              (pat-match pats (rest input)
                         (pat-match (first args) (first input) blists)))))
    (cond ((null skip-blists) no-skip-blists)
          ((null no-skip-blists) skip-blists)
          (t (append skip-blists no-skip-blists)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extensions useful for critiquing Lisp code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (?NAME-CONTAINS string) -- matches a symbol containing
;;; the given string (case is ignored)

(add-extension '?name-contains :single 'match-name-contains)

(defun match-name-contains (args input blists)
  (declare #.*internal-optimize-settings*)
  (destructuring-bind (substring) args
    (and (symbolp input)
         (search substring (symbol-name input)
                 :test #'char-equal)
         blists)))

;;; (?NAME-ENDS-WITH string) -- matches a symbol ending with
;;; the given string (case is ignored)

(add-extension '?name-ends-with :single 'match-name-ends-with)

(defun match-name-ends-with (args input blists)
  (declare #.*internal-optimize-settings*)
  (destructuring-bind (substring) args
    (and (symbolp input)
         (string-ends-with (symbol-name input) substring)
         blists)))

(declaim (ftype (function (simple-string simple-string)
                          (values boolean &optional))
                string-ends-with))
(defun string-ends-with (str substr)
  (declare #.*internal-optimize-settings*)
  (let ((strlen (length str))
        (substrlen (length substr)))
    (and (> strlen substrlen)
         (string-equal str substr :start1 (- strlen substrlen)))))

;;; (?NAME-STARTS-WITH string) -- matches a symbol starting with
;;; the given string (case is ignored)

(add-extension '?name-starts-with :single 'match-name-starts-with)

(declaim (ftype (function (list list list) (values boolean &optional))
                match-name-starts-with))
(defun match-name-starts-with (args input blists)
  (declare #.*internal-optimize-settings*)
  (destructuring-bind (substring) args
    (and (symbolp input)
         (string-starts-with-p (symbol-name input) substring)
         blists)))

(declaim (ftype (function (simple-string simple-string)
                          (values boolean &optional))
                string-starts-with-p))
(defun string-starts-with-p (str substr)
  (declare #.*internal-optimize-settings*)
  (let ((strlen (length str))
        (substrlen (length substr)))
    (and (>= strlen substrlen)
         (string-equal str substr :end1 substrlen))))

(add-extension '?user-defined-name-starts-with :single 'match-user-defined-name-starts-with)

(defun standard-symbolp (sym)
  (declare #.*internal-optimize-settings*)
  (and (symbolp sym)
       (eql (load-time-value (find-package "CL"))
            (symbol-package sym))))

(defun match-user-defined-name-starts-with (args input blists)
  (declare #.*internal-optimize-settings*)
  (destructuring-bind (substring) args
    (and (symbolp input)
         (not (standard-symbolp input))
         (string-starts-with-p (symbol-name input) substring)
         blists)))

;;; (?EQL-PRED [name]) -- matches a Lisp equality predicate
;;;   (except =) and binds name to it, if given

(add-extension '?eql-pred :single 'match-eql-pred)

(defun match-eql-pred (args input blists)
  (declare #.*internal-optimize-settings*)
  (destructuring-bind (&optional name) args
    (and (member input '(eq eql equal equalp))
         (bind-variable name input blists))))


;;; (?TOO-LONG [name]) -- matches if code is too long
;;;   (LIST-COUNT > *LENGTH-THRESHOLD*) and binds name
;;    to LIST-COUNT, if given

(add-extension '?too-long :single 'match-too-long)

(defun match-too-long (args input blists)
  (declare #.*internal-optimize-settings*)
  (destructuring-bind (&optional name) args
    (let ((badness (get-length-badness input)))
      (when (> badness 0)
        (bind-variable name
                       (get-badness-phrase badness)
                       blists)))))


(defun get-length-badness (code)
  (declare #.*internal-optimize-settings*)
  (let ((code-length (list-count code)))
    (/ (- code-length *length-threshold*)
       *length-threshold*)))

#| doesn't handle dotted pairs

(defun list-count (form)
  (cond ((atom form) 0)
        (t (reduce #'+ form
                   :key #'list-count
                   :initial-value (length form)))))
|#

(defun list-count (form)
  (declare #.*internal-optimize-settings*)
  (cond ((null form) 0)
        ((atom form) 1)
        (t (+ (list-count (car form))
              (list-count (cdr form))))))

(defun get-badness-phrase (badness)
  (declare #.*internal-optimize-settings*)
  (cond ((<= badness 1/4) "a little")
        ((<= badness 1/2) "somewhat")
        ((<= badness 3/4) "")
        (t "way")))


;;; (?SETS-FREE-VARS [name]) -- matches any Lisp code containing
;;;    assignments to free variables -- binds name to a list of the
;;;    free variables if given
;;;
;;; For all that there's lot of code here, it's still very crude.
;;; Most of the code is to handle all the ways Common Lisp can
;;; assign and create variables.

(add-extension '?sets-free-vars :single 'match-sets-free-vars)

(defun match-sets-free-vars (args input blists)
  (declare #.*internal-optimize-settings*)
  (destructuring-bind (&optional name) args
    (let ((vars (remove-duplicates (find-assigned-free-vars input))))
      (if (null vars) nil
          (bind-variable name vars blists)))))

;;; Usage: (?top-level pat1 pat2 ...)
;;;
;;; Matches if (pat-match pat input) is true at the top-level
;;; of input, i.e, no nesting.

(add-extension '?top-level :single 'match-top-level)

(defun match-top-level (args input blists)
  (declare #.*internal-optimize-settings*)
  (and *top-level*
       (match-and args input blists)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting assigned free variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Very quick and dirty. Doesn't know real
;;; scope rules, assumes anything nested is scoped, e.g.,
;;;
;;;   (do ((x (setq x 2) ...)) ...)
;;;
;;; is not considered a free variable assignment.

(defun find-assigned-free-vars (code &optional env-stack)
  (declare #.*internal-optimize-settings*)
  (or (code-assigned-free-vars code env-stack)
      (and (consp code)
           (let ((new-stack (cons code env-stack)))
             (loop for l = code then (cdr l)
                   until (atom l)
                   append (find-assigned-free-vars (car l) new-stack))))))

(defun code-assigned-free-vars (code &optional env-stack)
  (declare #.*internal-optimize-settings*)
  (let ((vars (code-assigned-vars code)))
    (cond ((null vars) nil)
          (t (get-free-vars vars env-stack)))))

(defun get-free-vars (vars env-stack)
  (declare #.*internal-optimize-settings*)
  (cond ((null env-stack) vars)
        ((null vars) nil)
        (t (get-free-vars (remove-local-vars vars (first env-stack))
                          (rest env-stack)))))

(defun remove-local-vars (vars code-env)
  (declare #.*internal-optimize-settings*)
  (let ((local-vars (code-vars code-env)))
    (cond ((null local-vars) vars)
          (t (set-difference vars local-vars)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting assigned variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun code-assigned-vars (code)
  (declare #.*internal-optimize-settings*)
  (unless (atom code)
    (let ((fn (get-assigned-vars-fn (first code))))
      (cond ((null fn) nil)
            (t (remove-if-not #'symbolp (funcall fn code)))))))

(dolist (fn '(psetf psetq rotatef setf setq shiftf))
  (setf (get-assigned-vars-fn fn)
        #'(lambda (code)
            (do ((tail (cdr code) (cddr tail))
                 (vars nil (cons (first tail) vars)))
                ((null tail) vars)))))

(dolist (fn '(decf incf pop))
  (setf (get-assigned-vars-fn fn)
        #'(lambda (code) (list (second code)))))

(dolist (fn '(push pushnew))
  (setf (get-assigned-vars-fn fn)
        #'(lambda (code) (list (third code)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting new local variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun code-vars (code)
  (declare #.*internal-optimize-settings*)
  (unless (atom code)
    (let ((fn (get-local-vars-fn (first code))))
      (cond ((null fn) nil)
            (t (funcall fn code))))))

(defun get-vars (vars-list)
  (declare #.*internal-optimize-settings*)
  (loop for var-form in vars-list
        for var = (get-var var-form)
        unless (member var lambda-list-keywords)
          collect var))

(defun get-var (var-form)
  (declare #.*internal-optimize-settings*)
  (cond ((atom var-form) var-form)
        (t (get-var (car var-form)))))


(dolist (fn '(defmacro defun))
  (setf (get-local-vars-fn fn)
        #'(lambda (code) (get-vars (third code)))))

(dolist (fn '(destructuring-bind do do* lambda let let*
              multiple-value-bind))
  (setf (get-local-vars-fn fn)
        #'(lambda (code) (get-vars (second code)))))

(dolist (fn '(dolist dotimes with-open-file with-open-stream))
  (setf (get-local-vars-fn fn)
        #'(lambda (code) (list (get-var (second code))))))

(dolist (fn '(flet labels))
  (setf (get-local-vars-fn fn)
        #'(lambda (code)
            (loop for def in (second code)
                  append (second def)))))

(setf (get-local-vars-fn 'loop) 'get-loop-vars)

(declaim (ftype (function (t) (values list &optional)) get-loop-vars))
(defun get-loop-vars (code)
  (declare #.*internal-optimize-settings*)
  (cond ((atom code) nil)
        (t (let ((tail (member-if #'loop-binder-p code)))
             (cond ((null tail) nil)
                   (t (cons (get-var (second tail))
                            (get-loop-vars (cddr tail)))))))))

(defun loop-binder-p (x)
  (declare #.*internal-optimize-settings*)
  (and (symbolp x)
       (member x '(for with and) :test #'string=)))
