;;;; lisp-critic.asd

(cl:in-package :asdf)

(defsystem :pro-lisp-critic
  :version "1.1"
  :description "PRO-LISP-CRITIC - A Lisp code critiquing package."
  :long-description "A version of Lisp Critic designed for professional use."
  :author "Chris Riesbeck and Gregory Czerniak"
  :maintainer "CHIBA Masaomi"
  :license "MIT Licence"
  :serial t
  :depends-on (#:ckr-tables)
  :components ((:file "extend-match")
               (:file "write-wrap")
               (:file "lisp-critic")
               (:file "lisp-rules")))

