(in-package #:cl-user)

(defpackage #:lbash
  (:shadow "!")
  (:use #:common-lisp #:inferior-shell #:esrap 
        #:optima #:optima.ppcre #:optima.extra))
