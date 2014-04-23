#+xcvb (module (:depends-on ("packages")))

(uiop:define-package :fare-quasiquote/test
  (:mix :fare-quasiquote :hu.dwim.stefil :common-lisp)
  (:shadowing-import-from :fare-quasiquote
   #:quote #:list #:list* #:append))

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
          (*print-pretty* nil))
      (funcall thunk))))

(defun rq (s) (with-qq-syntax () (read-from-string s)))
(defun pq (x) (with-qq-syntax () (write-to-string x)))

(defparameter b 11)

(deftest test-quasiquote ()
  (macrolet ((q (x y)
               `(progn
                  (hu.dwim.stefil:is (equal (rq ,x) ',y))
                  ;;(hu.dwim.stefil:is (equal ,x (pq ',y)))
                  )))
    (q "``a" (quote (quote a)))
    (q "`(a ,b)" (list (quote a) b))
    (q "`(,@a)" a)
    (q "``(a ,b)" (quote (list (quote a) b)))
    (q "`(a ,b)" (list (quote a) b))
    (q "`(a ,x ,@y)" (list* (quote a) x y))
    ;;(is (equal (ifmatch `(a ,x ,@y) '(a b c d) (list x y)) '(b (c d))))
    (q "`(1 2 3)" (quote (1 2 3)))
    (q "`(a ,b ,@c .,d)" (list* (quote a) b (append c d)))
    (q "`(,@c .,d)" (append c d))
    ;;BUG: (q "```(,,a ,',',b)" ...)
    ;;BUG: (q "'```(,',',plet/fast ,',kernel ,@body)" ...)
    (signals error (rq "`(foo bar #.(max 5 ,*print-base*))"))
    t))
