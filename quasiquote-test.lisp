#+xcvb (module (:depends-on ("packages")))

(uiop:define-package :fare-quasiquote/test
  (:mix :fare-quasiquote :hu.dwim.stefil :common-lisp :optima :optima.extra)
  (:shadowing-import-from :fare-quasiquote
   #:quasiquote #:unquote #:unquote-splicing #:unquote-nsplicing
   #:list #:append #:nconc #:list* #:cons #:quote #:vector
   #:kwote #:quotep #:n-vector #:make-vector
   #:quasiquote-unexpand #:quasiquote-unexpand-0 #:quasiquote-unexpand-1 #:quasiquote-unexpand-2))

(in-package :fare-quasiquote/test)

(defsuite* (fare-quasiquote-test :in root-suite :documentation "All fare-quasiquote tests"))

;; This version of princ allows one to see
;; inside of your implementation's version of quasiquoted expressions...

(defun rprinc (x)
  "hand-made princ that allows to see inside quasiquotes
(results are implementation-dependent)"
  (labels
      ((rprinc-list (x)
         (princ "(")
	 (rprinc-list-contents x)
	 (princ ")"))
       (rprinc-list-contents (x)
         (rprinc (car x))
	 (rprinc-cdr (cdr x)))
       (rprinc-cdr (x)
         (if x (if (consp x)
		   (progn
		     (princ " ")
		     (rprinc-list-contents x))
		 (progn
		   (princ " . ")
		   (rprinc x))))))
    (cond
     ((consp x) (rprinc-list x))
     (t (princ x)))
    x))

;; You can test the quasiquote implementation like this:

(defmacro with-qq-syntax ((&key) &body body)
  `(call-with-qq-syntax #'(lambda () ,@body)))
(defun call-with-qq-syntax (thunk)
  (with-standard-io-syntax
    (let ((*package* (find-package :fare-quasiquote/test))
          (*readtable* *fq-readtable*)
          (*print-pprint-dispatch* *fq-pprint-dispatch*)
          (*print-pretty* t)
          (*print-case* :downcase))
      (funcall thunk))))

(defun rq (s) (with-qq-syntax () (read-from-string s)))
(defun pq (x) (with-qq-syntax () (write-to-string x)))

(defparameter b 11)
(defparameter c (list 22 33))
(defparameter d (list 44 55))
(defmacro q-if-match (pat val then &optional else) ;; avoid pprint rules for if* on SBCL
  `(if-match ,pat ,val ,then ,else))

(defmacro q (x y v &optional (z x))
  `(progn
     (is (equal (rq ,x) ',y))
     (is (equalp ,y ',v))
     ,(unless (eq z t) `(is (equal (pq ',y) ,z)))))

(deftest test-quasiquote ()
  (q "`a" (quote a) a)
  (q "``a" (quote (quote a)) (quote a))
  (q "`(a ,b)" (list (quote a) b) (a 11))
  (q "`(,@b)" b 11 "b")
  (q "`,`a" (quote a) a "`a")
  (q "`(,@`a)" (quote a) a "`a")
  (q "``(a ,b)" (quote (list (quote a) b)) (list (quote a) b))
  (q "`(a ,@b)" (cons (quote a) b) (a . 11))
  (q "`(a ,b ,@c)" (list* (quote a) b c) (a 11 22 33))
  (q "(q-if-match `(a ,x ,@y) '(a b c d) (vector x y))"
      (q-if-match (list* (quote a) x y) '(a b c d) (vector x y))
      #(b (c d)))
  (q "`(1 2 3)" (quote (1 2 3)) (1 2 3))
  (q "`(a ,b ,@c .,d)" (list* (quote a) b (append c d)) (a 11 22 33 44 55) "`(a ,b ,@c ,@d)")
  (q "`(,@c .,d)" (append c d) (22 33 44 55) "`(,@c ,@d)")
  (q "```(,,a ,',',b)" ;; This was a bug in 0.9.0 and earlier, inherited from SBCL
      (list (quote list) (quote (quote list)) (quote a)
            (list (quote list) (quote (quote common-lisp:quote)) (list (quote common-lisp:quote) b)))
      (list (quote list) a (list (quote common-lisp:quote) '11)))
  (signals error (rq "`(foo bar #.(max 5 ,*print-base*))"))
  t)
