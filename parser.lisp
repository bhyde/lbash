(in-package #:lbash)


;;;; Grammar Roots

(defrule <shell-one-liner> (and <pipeline> (? (or #\; #\& #\newline)) (* <ws>))
  (:destructure (a b c)
                (declare (ignore c))
                (ecase (if b (char b 0) :eom)
                  (#\&  `(fork ,a))
                  (#\;       a)
                  (#\newline  a)
                  (:eom       a))))

(define-parse-examples <shell-one-liner> ()
  ("cmd;" (cmd))
  ("cmd&" (fork (cmd)))
  ("cmd;" (cmd))
  ("cmd;  " (cmd))
  ("cmd
" (cmd))
  ("a;b" (progn (a) (b)))
  ("a;b;" (progn (a) (b))))

(defrule <shell-program> (+ <shell-one-liner>)
  (:lambda (x)
    (if (cdr x)
        `(progn ,@x)
        x)))

(define-parse-examples <shell-program> ()
    ("cmd
cmd2 \\
    < f
cmd3"
     (progn (cmd)
            (with-redirects ((:redirect-input "f")) (cmd2))
            (cmd3))))

;;;; Primitives

(defrule <number> (+ (character-ranges (#\0 #\9)))
  (:lambda (x) (parse-integer (flatten-text x))))

(defrule <ws> (or (and #\\ #\newline) #\space #\tab)
  (:constant :whitespace))

(defrule <keyword> (or
                    "!"	"esac"	"select"	"}"
                    "case"	"fi"	"then"	"[["
                    "do"	"for"	"until"	"]]"
                    "done"	"function"	"while"	 
                    "elif"	"if"	"time"	 
                    "else"	"in"	"{"))

;;;; Quoted Strings

;;; I'm not currently trying to support the way shells allow a quoted
;;; string to produce multiple tokens. For example bash's:
;;;    A="a b c" ; for i in $A ; do echo $i ; done
;;; generates three lines of output.

(defrule <single-quoted-string> (and #\' (* (not #\')) #\')
  (:lambda (x) (flatten-text (second x))))

(defrule <double-quoted-string> (and #\" (* (or <variable-ref> (not #\"))) #\")
  (:lambda (x)
    (loop
       with contains-computation
       with result-bits
       with pad = (make-array 100 :element-type 'base-character :fill-pointer 0)
       finally
         (unless (zerop (fill-pointer pad))
           (push (coerce pad 'string) result-bits))
         (return
           (cond
             (contains-computation
              `(concatenate 'string ,@(nreverse result-bits)))
             (result-bits
              (car result-bits))
             (t
              "")))
       for i in (second x) do
         (cond
           ((characterp i)
            (vector-push-extend i pad))
           (t
            (setf contains-computation t)
            (unless (zerop (fill-pointer pad))
              (push (coerce pad 'string) result-bits)
              (setf (fill-pointer pad) 0))
            (push i result-bits))))))

(define-parse-examples <double-quoted-string> ()
  ("\"a\"" "a")
  ("\"a$b\"" (concatenate 'string "a" (fetch-var "b")))
  ("\"a${b}\"" (concatenate 'string "a" (fetch-var "b")))
  ("\"a$b$c\"" (concatenate 'string "a" (fetch-var "b") (fetch-var "c")))
  ("\"a${b}${c}\"" (concatenate 'string "a" (fetch-var "b") (fetch-var "c")))
  ("\"$z${b}\"" (concatenate 'string (fetch-var "z") (fetch-var "b"))))


;;; Pipelines

(defrule <pipeline> <pipeline-1>)

(defrule <pipeline-1> (and <pipeline-2> (* (and #\; <pipeline-2>)))
  (:lambda (x) (list-reduce 'progn x)))

(defrule <pipeline-2> (and <pipeline-3> (* (and #\& <pipeline-3>)))
  (:destructure (a b)
    (cond
      (b
       `(progn (fork ,a)
               ,@(loop
                    with cmds = (nreverse (mapcar #'second b))
                    with result = `(,(pop cmds))
                    for cmd in cmds
                    do (push `(fork ,cmd) result)
                    finally (return result))))
      (t  a))))


(defrule <pipeline-3> (and <pipeline-4> (* (and "&&" <pipeline-4>)))
  (:lambda (x) (list-reduce 'and x)))

(defrule <pipeline-4> (and <pipeline-5> (* (and "||" <pipeline-5>)))
  (:lambda (x) (list-reduce 'or x)))

(defrule <pipeline-5> (and <pipeline-6> (* (and "|" <pipeline-6>)))
  (:lambda (x) (list-reduce 'pipe x)))


(defrule <if-stmt> (and "if" <shell-one-liner> "then" <shell-one-liner> (? (and "else" <shell-one-liner>)) "fi")
  (:destructure (a b c d e f)
                (declare (ignore a c f))
                (tidy-expression
                 (if e
                     `(if ,b ,d ,(second e))
                     `(if ,b ,d)))))

(define-parse-examples <if-stmt> ()
  ("if echo hi; then echo bar; fi"
   (if (echo "hi") (echo "bar")))
  ("if echo hi; then echo bar; echo gum fi"
   (when (echo "hi") (echo "bar") (echo "gum"))))

(defrule <until-stmt> (and "until" <shell-one-liner> "do" <shell-one-liner> "done")
  (:destructure (a pred c body e)
                (declare (ignore a c e))
                `(until ,pred ,body)))

(define-parse-examples <until-stmt> ()
  ("until echo hi ; do echo boo ; done" 
   (until (echo "hi") (echo "boo"))))

(defrule <while-stmt> (and "while" <shell-one-liner> "do" <shell-one-liner> "done")
  (:destructure (a pred c body e)
                (declare (ignore a c e))
                `(while ,pred ,body)))

(define-parse-examples <while-stmt> ()
  ("while echo hi ; do echo boo ; done" 
   (while (echo "hi") (echo "boo"))))

(defrule <for-stmt> (and "for" <name> (? (and "in" (* <word>) #\;) ) "do" <shell-one-liner> "done")
  (:destructure (a name over b body c)
                (declare (ignore a b c))
                (let ((elements (if over (second over) nil)))
                  `(for (,name `,elements) ,@body))))

#+tbd  ;; case statements
(progn
(defrule <case-stmt> (and "case" (+ <ws>) <word> (+ <ws>) "in" (+ <case-clause>) "esca")
  (:destructure (case a name b in clauses esca)
                (declare (ignore case a b in esca))
                `(case ,name
                     ,@clauses)))

(defrule <case-clause>
    (and (+ <ws>) (? #\( ) <pattern> (* (and #\| <pattern>)) #\) <shell-one-liner> ";;" (+ <ws>))
  ;; tbd: ";&" and ";;&" are pending :)
  (:destructure (a b p1 ps c rhs d e)
                (declare (ignore a b c d e))
                (let ((lhs 
                       (if ps
                           (cdr (list-reduce 'f `(,p1 ,ps)))
                           `(,p1))))
                  `(,lhs ,rhs))))

(defrule <pattern> (and (* <ws>) <word> (* <ws>))  ;; that's not right?
  (:lambda (x) (second x)))

(define-parse-examples <case-stmt> ()
  ("case x in x) echo hi ;; esac" (case "x" ("x" echo hi)))
  ("case x in x | y) echo hi ;; esac" (case "x" (("x" "y") echo hi))))
)

(defrule <select-stmt> <tbd>) 
(defrule <arithmetic-stmt> <tbd>) ;; (( ... ))
(defrule <test-stmt> <tbd>) ;; [[ ... ]]
(defrule <subshell-stmt> <tbd>) ;; (...)
(defrule <group-stmt> <tbd>) ;; {...}


(defrule <pipeline-6> (or <if-stmt>
                          <until-stmt>
                          <while-stmt>
                          <for-stmt>
                          <simple-command>
                          <variable-assgnment>))


;;;; Simple commands

;;; e.g. running one program, built-in command, or setting a variable.
;;; including all the redirection

(defrule <simple-command> (and (* <variable-assgnment>)
                               (+ (and (* <ws>) (or <word> <redirection>)))
                               (? <ws>))
  (:lambda (x)
    (let ((vars (pop x))
          (cmd-bits (mapcar #'second (pop x))))
      (flet ((build-command (bits)
               (flet ((tidy-command (cmd)
                        `(,(build-symbol (first cmd))
                           ,@(loop 
                                for bit in (rest cmd)
                                collect (ematch bit
                                          ((ppcre "^--.*")
                                           (build-keyword (subseq bit 2)))
                                          (_
                                           bit))))))
                 (loop
                    with redirects
                    with cmd
                    finally
                      (return
                        (cond
                          (redirects
                           `(with-redirects ,(nreverse redirects) ,(tidy-command (nreverse cmd))))
                          (t
                           (tidy-command (nreverse cmd)))))
                    for bit in bits do
                      (ematch bit
                        (`(:redirect ,@args)
                          (push args redirects))
                        (_
                         (push bit cmd)))))))
        (cond
          ((and vars cmd-bits)
           `(with-env ,(mapcar #'rest vars) ,(build-command cmd-bits)))
          (vars
           `(progn ,@vars))
          (cmd-bits
           (build-command cmd-bits)))))))

(defrule <variable-assgnment> (and (* <ws>) <variable-name> #\= <word>)
  (:lambda (x) `(set ,(second x) ,(fourth x))))

(defrule <variable-name> (+ (character-ranges (#\a #\z) (#\A #\Z) (#\0 #\9) #\_ #\-))
  (:lambda (x) (flatten-text x)))

(defrule <word> (and (esrap:! <keyword>)
                     (+ (or <simple-word> <single-quoted-string> <double-quoted-string> <variable-ref>)))
  (:destructure (a x)
                (declare (ignore a))
                (tidy-expression
                 (if (rest x)
                     `(build-word ,@x)
                     (car x)))))

(defrule <simple-word> (+ (character-ranges (#\a #\z) (#\A #\Z) (#\0 #\9) #\_ #\-))
  (:lambda (x) (flatten-text x)))

(defrule <redirect-op> (or "<<-" ">>" "<<" "<-" ">-" #\<  #\>)
  (:lambda (x)
    (cond
      ((string= x "<<") :here-doc)
      ((string= x "<<-") :indented-here-doc)
      ((string= x ">>") :append)
      ((string= x ">-") :close-output)
      ((string= x "<-") :close-input)
      ((string= x ">") :redirect-output)
      ((string= x "<") :redirect-input))))

(defrule <redirection> (and (? <number>) <redirect-op> (or <number> (and (* <ws>) <word>)))
  (:destructure (prefix-number? op what)
                `(:redirect ,op ,(second what)
                           ,@(when prefix-number? `(:fd ,prefix-number?)))))
                
(defrule <variable-ref> (and #\$ (or <complex-variable-ref> <simple-variable-ref>))
  (:lambda (x)
    `(fetch-var ,(second x))))

(defrule <simple-variable-ref> <simple-word>)

(defrule <complex-variable-ref> (and #\{ <simple-word> #\})
  (:lambda (x) (second x)))



;;; See also:
;;;   http://www.opensource.apple.com/source/bash/bash-23/bash/parse.y?txt
;;;   https://github.com/amitkr/bash/blob/master/y.tab.c
