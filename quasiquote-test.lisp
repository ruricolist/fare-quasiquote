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
(defun qq (x) (let* ((y (rq x)) (v (eval y)) (z (pq y)))
                `(q ,x ,y ,v ,@(unless (equal x z) (list z)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun f? (x)
    (ecase x
      (t t)
      (r (or #-quasiquote-at-macro-expansion-time t))
      (m (or #+quasiquote-at-macro-expansion-time t))
      (q (or #-quasiquote-passes-literals t))
      (l (or #+quasiquote-passes-literals t))
      (rq (or #-(or quasiquote-at-macro-expansion-time quasiquote-passes-literals) t))
      (mq (or #+(and quasiquote-at-macro-expansion-time (not quasiquote-passes-literals)) t))
      (rl (or #+(and (not quasiquote-at-macro-expansion-time) quasiquote-passes-literals) t))
      (ml (or #+(and quasiquote-at-macro-expansion-time quasiquote-passes-literals) t))))

  (defun u (x)
    (match x
      ((list 't v) v)
      ((type cl:cons)
       (loop :for (f v) :in x :when (f? f) :return v))
      (otherwise x))))

(defmacro q (x y v &optional (z x))
  `(progn
     (is (equalp (rq ,(u x)) ',(u y)))
     (is (equalp ,(u y) ',(u v)))
     (is (equal (pq (rq ,x)) ',(or (u z) (u x))))))

(defmacro qx (&rest tests)
  `(progn
     ,@(loop :for (x y v z) :in tests
             :collect `(q ,x ,y ,v ,z))))

;;; Test values
(defparameter a '(vector 0))
(defparameter b 11)
(defparameter c (list 22 33))
(defparameter d (list 44 55))
(defmacro q-if-match (pat val then &optional else) ;; avoid pprint rules for if* on SBCL
  `(if-match ,pat ,val ,then ,else))

(deftest test-quasiquote ()
  (qx
   ("`a"
    ((r (quote a)) (m (quasiquote a)))
    (t a))
   ("``a"
    ((r (quote (quote a)))
     (m (quasiquote (quasiquote a))))
    (t (quote a)))
   ("`(a ,b)"
    ((r (list (quote a) b))
     (m (quasiquote (a (unquote b)))))
    (t (a 11)))
   ("``(a ,b)"
    ((r (quote (list (quote a) b)))
     (m (quasiquote (quasiquote (a (unquote b))))))
    (t (list (quote a) b)))
   ("`(a ,@b)"
    ((r (cons (quote a) b))
     (m (quasiquote (a (unquote-splicing b)))))
    (t (a . 11)))
   ("`(,@b)"
    ((r b) (m (quasiquote ((unquote-splicing b)))))
    11
    ((r "b")))
   ("`,`a"
    ((r (quote a))
     (m (quasiquote (unquote (quasiquote a)))))
    (t a)
    ((r "`a")))
   ("`(,@`a)"
    ((r (quote a))
     (m (quasiquote ((unquote-splicing (quasiquote a))))))
    (t a)
    ((r "`a")))
   ("`(a . ,b)"
    ((r (cons (quote a) b))
     (m (quasiquote (a unquote b))))
    (t (a . 11))
    ((r "`(a ,@b)")
     (m "`(a unquote b)")))
   ("`(a ,b ,@c)"
    ((r (list* (quote a) b c))
     (m (quasiquote (a (unquote b) (unquote-splicing c)))))
    (t (a 11 22 33)))
   ("(q-if-match `(a ,x ,@y) '(a b c d) (vector x y))"
    ((r (q-if-match (list* (quote a) x y) '(a b c d) (vector x y)))
     (m (q-if-match (quasiquote (a (unquote x) (unquote-splicing y))) '(a b c d) (vector x y))))
    #(b (c d)))
   ("(q-if-match `#(a ,x ,@y) #(a b c d) (vector x y))"
    ((r (q-if-match (make-vector (list* (quote a) x y)) #(a b c d) (vector x y)))
     (m (q-if-match (quasiquote (unquote (make-vector (list* (quote a) x y)))) #(a b c d) (vector x y))))
    #(b (c d)))
   ("(q-if-match `#(a ,x ,y d) #(a b c d) (vector x y))"
    ((r (q-if-match (make-vector (list (quote a) x y (quote d))) #(a b c d) (vector x y)))
     (m (q-if-match (quasiquote (unquote (make-vector (list (quote a) x y (quote d)))))
                    #(a b c d) (vector x y))))
    #(b c))
   ("`(1 2 3)"
    ((r (quote (1 2 3)))
     (m (quasiquote (1 2 3))))
    (t (1 2 3)))
   ("`(a ,@c . 4)"
    ((r (cons (quote a) (append c (quote 4))))
     (m (quasiquote (a (unquote-splicing c) . 4))))
    (t (a 22 33 . 4)))
   ("`(a ,b ,@c .,d)"
    ((r (list* (quote a) b (append c d)))
     (m (quasiquote (a (unquote b) (unquote-splicing c) unquote d))))
    (t (a 11 22 33 44 55))
    ((r "`(a ,b ,@c ,@d)")
     (m "`(a ,b ,@c unquote d)")))
   ("`(,@c .,d)"
    ((r (append c d))
     (m (quasiquote ((unquote-splicing c) unquote d))))
    (t (22 33 44 55))
    ((r "`(,@c ,@d)")
     (m "`(,@c unquote d)")))
   ("```(,,a ,',',b)"
    ;; The pretty-printer in 0.9.0 and earlier, had a bug inherited from SBCL,
    ;; and couldn't pretty-print this form back to its value.
    ((r (list (quote list) (quote (quote list)) (quote a)
              (list (quote list) (quote (quote common-lisp:quote)) (list (quote common-lisp:quote) b))))
     (m (quasiquote (quasiquote (quasiquote ((unquote (unquote a)) (unquote '(unquote '(unquote b)))))))))
    (t (list (quote list) a (list (quote common-lisp:quote) '11))))
   ("`#(a ,b)"
    ((r (make-vector (list (quote a) b)))
     (m (quasiquote (unquote (make-vector (list (quote a) b))))))
    #(a 11))
   ("`#3(a ,b)"
    ((r (n-vector 3 (list (quote a) b)))
     (m (quasiquote (unquote (n-vector 3 (list (quote a) b))))))
    #(a 11 11))
   ("`#5(a ,@c)"
    ((r (n-vector 5 (cons (quote a) c)))
     (m (quasiquote (unquote (n-vector 5 (cons (quote a) c))))))
    #(a 22 33 33 33))
   ("`(foobar a b ,c ,'(e f g) d ,@'(e f g) (h i j) ,@b)"
    ((r (list* (quote foobar) (quote a) (quote b) c '(e f g) (quote d)
               (append '(e f g) (cons (quote (h i j)) b))))
     (m (quasiquote (foobar a b (unquote c) (unquote '(e f g)) d
                            (unquote-splicing '(e f g)) (h i j) (unquote-splicing b)))))
    (t (foobar a b (22 33) (e f g) d e f g (h i j) . 11)))
   ("``(, @c)"
    ((r (quote (list @c)))
     (m (quasiquote (quasiquote ((unquote @c))))))
    (t (list @c)))
   ("``(, .c)"
    ((r (quote (list .c)))
     (m (quasiquote (quasiquote ((unquote .c))))))
    (t (list .c))))
  (let ((c (list 2 3)))
    (q "`(1 ,b ,@a ,.c ,.d)"
       ((rq (list* (quote 1) b (append a (nconc c d))))
        (rl (list* 1 b (append a (nconc c d))))
        (m (quasiquote (1 (unquote b) (unquote-splicing a) (unquote-nsplicing c) (unquote-nsplicing d)))))
       (t (1 11 vector 0 2 3 44 55)))
    ;; NCONC is evil. Use at your own risk!
    (is (equal c '(2 3 44 55))))
  (signals error (rq "`(foo bar #.(max 5 ,*print-base*))"))
  t)

;;; Double-quasiquote test from the SBCL test suite backq.impure.lisp
(defparameter *qq* '(*rr* *ss*))
(defparameter *rr* '(3 5))
(defparameter *ss* '(4 6))
(defun *rr* (x) (reduce #'* x))
(defparameter *x* '(a b))
(defparameter *y* '(c))
(defparameter *p* '(append *x* *y*))
(defparameter *q* '((append *x* *y*) (list 'sqrt 9)))
(defparameter *r* '(append *x* *y*))
(defparameter *s* '((append *x* *y*)))

(defparameter *double-quasiquote-tests*
  '(("``(,,*qq*)" . (24))
    ("``(,@,*qq*)" . 24)
    ("``(,,@*qq*)" . ((3 5) (4 6)))
    ("``(foo ,,*p*)" . (foo (a b c)))
    ("``(foo ,,@*q*)" . (foo (a b c) (sqrt 9)))
    ("``(foo ,',*r*)" . (foo (append *x* *y*)))
    ("``(foo ,',@*s*)" . (foo (append *x* *y*)))
    ("``(foo ,@,*p*)" . (foo a b c))
    ("``(foo ,@',*r*)" . (foo append *x* *y*))
    ;; the following expression produces different result under LW.
    ("``(foo . ,,@*q*)" . (foo a b c sqrt 9))
    ;; these three did not work.
    ("``(foo ,@',@*s*)" . (foo append *x* *y*))
    ("``(foo ,@,@*q*)" . (foo a b c sqrt 9))
    ("``(,@,@*qq*)" . (3 5 4 6))
    ("`(,,@(list 1 2 3)" . (1 2 3 10))))

(deftest test-double-quasiquote ()
  (loop :for (expression . value) :in *double-quasiquote-tests* :do
    (is (equal (eval (eval (rq expression))) value)))
  t)

#|
;; To test this system in all configurations:

cl-launch \
"(asdf:test-system :fare-quasiquote :force '(fare-quasiquote fare-quasiquote-test))"

cl-launch \
"(progn
   (pushnew :quasiquote-at-macro-expansion-time *features*)
   (asdf:test-system :fare-quasiquote :force '(fare-quasiquote fare-quasiquote-test)))"

cl-launch \
"(progn
   (pushnew :quasiquote-passes-literals *features*)
   (asdf:test-system :fare-quasiquote :force '(fare-quasiquote fare-quasiquote-test)))"

cl-launch \
"(progn
   (pushnew :quasiquote-at-macro-expansion-time *features*)
   (pushnew :quasiquote-passes-literals *features*)
   (asdf:test-system :fare-quasiquote :force '(fare-quasiquote fare-quasiquote-test)))"
|#
