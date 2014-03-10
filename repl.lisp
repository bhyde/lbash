(in-package #:lbash)

(defvar *show-parse* nil)
(defvar *show-input* nil)
(defvar *run-eval* nil)

(defun evaluate (lash-sexpr)
  (labels ((recure (x)
             (ematch x
               ((type integer) x)
               ((type string) x)
               (`(,command ,@args)
                 (print x)))))
    (recure lash-sexpr)))


(defun lash-text (text &key (eval *run-eval*) (show-parse *show-parse*) (show-input *show-input*))
  (flet ((parse-lash (text)
           (let ((parse (parse '<line> text)))
             (when show-parse (print parse))
             parse))
         (eval-lash (x)
           (when eval
             (run x :output :string)))
         (print-lash (x)
           (format t "~A" x)))
    (when show-input (print text))
    (print-lash
     (eval-lash
      (parse-lash text)))))

(defun repl-1 (&key (eval *run-eval*) (show-parse *show-parse*) (show-input *show-input*))
  (block nil
    (flet ((read-lash ()
             (format t "~&lash> ")
             (let ((input
                    (or (read-line *standard-input* nil nil)
                        (return :done))))
               input)))
      (lash-text (read-lash)
                 :eval eval
                 :show-parse show-parse
                 :show-input show-input))))

(defun repl (&key (eval *run-eval*) (show-parse *show-parse*) (show-input *show-input*))
  (loop (repl-1 :eval eval :show-parse show-parse :show-input show-input)))

