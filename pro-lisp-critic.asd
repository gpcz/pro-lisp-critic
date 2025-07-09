;;;; pro-lisp-critic.asd

(cl:in-package :asdf)

(defsystem "pro-lisp-critic"
  :version "1.1"
  :description "PRO-LISP-CRITIC - A Lisp code critiquing package."
  :long-description "A version of Lisp Critic designed for professional use."
  :author "Chris Riesbeck and Gregory Czerniak"
  :maintainer "Gregory Czerniak"
  :license "MIT Licence"
  :pathname "src/"
  :serial t
  :depends-on ("ckr-tables" "alexandria" "uiop")
  :components ((:file "package")
               (:file "critique")
               (:file "deviations")
               (:file "extend-match")
               (:file "write-wrap")
               (:file "specials")
               (:file "internals")
               (:file "lisp-critic")
               (:file "lisp-rules"))
  :in-order-to ((test-op (test-op "pro-lisp-critic/tests"))))

(defsystem "pro-lisp-critic/tests"
  :author "Greg Czerniak"
  :description "Tests the functionality of uniquifier"
  :license "MIT"
  :pathname "t/"
  :depends-on ("pro-lisp-critic" "fiveam")
  :components ((:file "package")
               (:file "deviations" :depends-on ("package"))
               (:file "run-tests" :depends-on ("deviations")))
  :perform (test-op (o c) (symbol-call :pro-lisp-critic/tests
                                       :run-test-suite)))
