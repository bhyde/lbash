;;; -*- mode:common-lisp -*-

(defsystem "lbash"
  :depends-on ("inferior-shell" "esrap-peg" "optima.ppcre" "fare-quasiquote-optima" "fiveam")
  :components ((:file "packages")
               (:file "utilities")
               (:file "parser")
               (:file "repl")))


