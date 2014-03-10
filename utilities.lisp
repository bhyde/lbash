(in-package #:lbash)

;;;; Bootstrap 5am testing.

(5am:def-suite tests :description "All the lash tests.")

(5am:in-suite tests)

(5am:test test-zero
  "Test that testing is working."
  (5am:is (= 4 (+ 2 2)) "2 plus 2 wasn't equal to 4, oh dear!"))

;;;; Symbol makers

(defvar *keyword-package* (find-package "KEYWORD"))

(defun build-symbol (&rest stuff)
  (intern (apply #'format nil "~:@(~@{~A~}~)" stuff)))

(defun build-keyword (&rest stuff)
  (intern (apply #'format nil "~:@(~@{~A~}~)" stuff) *keyword-package*))

;;;; Parsing utilities.

(defun flatten-text (x)
  (labels ((recure (x)
             (typecase x
               (character (princ x))
               (string (princ x))
               (cons (recure (car x))
                     (recure (cdr x))))))
    (with-output-to-string (*standard-output*)
      (recure x))))

(5am:test flatten-text
  (5am:is (string= "abc"
               (flatten-text '((1 . "a") "b" (:foo "c"))))))

(defun tidy-expression (x)
  (match x
    (`(or ,a (or ,@b)) `(or ,a ,@b))
    (`(progn ,a (progn ,@b)) `(progn ,a ,@b))
    (`(and ,a (and ,@b)) `(and ,a ,@b))
    (`(if ,a (progn ,@b)) `(when ,a ,@b))
    (`(if ,a (progn ,@b) (progn ,@c)) `(cond (,a ,@b) (t ,@c)))
    (`(if ,a (progn ,@b) ,c) `(cond (,a ,@b) (t ,c)))
    (`(if ,a ,b (progn ,@c)) `(cond (,a ,b) (t ,@c)))
    (_ x)))

(defun list-reduce (f x)
  (tidy-expression
   (if (second x)
       `(,f ,(car x) ,@(mapcar #'second (second x)))
       (car x))))

(5am:test list-reduce
  (5am:is (equal (list-reduce 'progn '(a ((#\; b) (#\; c)))) '(progn a b c)))
  (5am:is (equal (list-reduce 'progn '(a)) 'a)))


(defmacro define-parse-examples (grammar-rule-name () &body examples)
  `(5am:test ,grammar-rule-name
     ,@(loop 
          for (in out) in examples
          collect `(5am:is (equal (parse ',grammar-rule-name ,in) ',out)))))
