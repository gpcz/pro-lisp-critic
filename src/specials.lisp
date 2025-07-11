(in-package #:pro-lisp-critic)

(defvar *internal-optimize-settings*
  '(optimize
    (speed 2)
    (space 0)
    (debug 1)
    (compilation-speed 0))
  "The standard optimize settings used by internal expressions.")

(defvar *external-optimize-settings*
  '(optimize
    (speed 2)
    (space 0)
    (debug 1)
    (compilation-speed 0))
  "The standard optimize settings used by external expressions.")
