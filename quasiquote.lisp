;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; pattern-matching friendly implementation of Quasiquote
;;; Copyright (c) 2002-2014 Fahree Reedaw <fare@tunes.org>
;;; See README

#+xcvb (module (:depends-on ("packages")))

(in-package :fare-quasiquote)

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
;;;; uncomment some of the lines below to disable according simplifications:
;;(pushnew :quasiquote-passes-literals *features*)
;;(pushnew :quasiquote-at-macro-expansion-time *features*)

;;; Functions that actually build data structures.
;; Note that we want our own tokens for decompilation reasons,
;; but as functions they must evaluate the usual way.
(defsubst list (&rest r) r) ;; (apply #'cl:list r)
(defsubst list* (&rest r) (apply #'cl:list* r))
(defsubst cons (x y) (cl:cons x y))
(defsubst append (&rest r) (apply #'cl:append r))
(defsubst nconc (&rest r) (apply #'cl:nconc r))
;; These supporting functions don't have a standard name
(defsubst make-vector (l) (coerce l 'simple-vector))
(defun n-vector (n contents)
  (if (null n) (make-vector contents)
    (let ((a (make-array n :element-type t)))
      (when (and (null contents) (> n 0))
	(error "non-zero length vector with empty contents"))
      (loop for i below n with x
	do (unless (null contents) (setq x (pop contents)))
	do (setf (aref a i) x))
      (when contents
	(error "provided contents larger than declared vector length"))
      a)))

;;; These functions build the forms that build the data structures.
(make-single-arg-form quote kwote)
(make-single-arg-form quasiquote)
(make-single-arg-form unquote)
(make-single-arg-form unquote-splicing)
(make-single-arg-form unquote-nsplicing)
(defun k-list (&rest r) (cons 'list r))
(defun k-list* (&rest r) (cons 'list* r))
(defun k-cons (x y) (list 'cons x y))
(defun k-append (&rest r) (cons 'append r))
(defun k-nconc (&rest r) (cons 'nconc r))
(make-single-arg-form make-vector k-make-vector)
(defun k-n-vector (n l) (list 'n-vector n l))
(defun n-vector-p (x) (and (length=n-p x 3) (eq (first x) 'n-vector)))

(defun k-literal (literal)
  #+quasiquote-passes-literals literal
  #-quasiquote-passes-literals (kwote literal))
(defun k-vector (list)
  (#+quasiquote-at-macro-expansion-time kwote
   #-quasiquote-at-macro-expansion-time k-literal
   (make-vector list)))

;;; These macros expand into suitable forms
(defmacro quote (x) (list 'cl:quote x))
(defmacro quasiquote (x) (quasiquote-expand x))
(defmacro unquote (x)
  (declare (ignore x))
  (error "unquote only allowed within quasiquote"))
(defmacro unquote-splicing (x)
  (declare (ignore x))
  (error "unquote-splicing disallowed outside quasiquote"))
(defmacro unquote-nsplicing (x)
  (declare (ignore x))
  (error "unquote-nsplicing disallowed outside quasiquote"))


(defparameter *quasiquote-level* 0
  "current depth of quasiquote nesting")

(defun unquote-xsplicing-p (x)
  (or (unquote-splicing-p x) (unquote-nsplicing-p x)))

(defun quasiquote-expand (x)
  (let ((*quasiquote-level* 0))
    (multiple-value-bind (top arg)
        (quasiquote-expand-0 x)
      (when (eq top 'unquote-splicing)
        (error ",@ after backquote in ~S" x))
      (when (eq top 'unquote-nsplicing)
        (error ",. after backquote in ~S" x))
      (quasiquote-expand-1 top arg))))

(defun quasiquote-expand-0 (x)
  "Given an expression x under a backquote, return two values:
1- a token identifying the context: nil quote :literal list list* append nconc
2- a form
When combining backquoted expressions, tokens are used for simplifications."
  (cond
   ((null x)
    (values nil nil))
   #+quasiquote-at-macro-expansion-time
   ((simple-vector-p x)
    (values 'make-vector (list (quasiquote-expand-1 'list (coerce x 'cl:list)))))
   ((literalp x)
    (values #+quasiquote-passes-literals :literal #-quasiquote-passes-literals 'quote x))
   ((or (symbolp x) (quotep x))
    (values 'quote x))
   ((unquote-splicing-p x)
    (values 'unquote-splicing (single-arg x)))
   ((unquote-nsplicing-p x)
    (values 'unquote-nsplicing (single-arg x)))
   ((unquotep x)
    (values 'unquote (single-arg x)))
   #+quasiquote-at-macro-expansion-time
   ((quasiquotep x)
    (quasiquote-expand-0 (quasiquote-expand (single-arg x))))
   ((make-vector-p x)
    (values 'make-vector (cdr x)))
   ((n-vector-p x)
    (values 'n-vector (cdr x)))
   ((consp x)
    (multiple-value-bind (atop a) (quasiquote-expand-0 (car x))
      (multiple-value-bind (dtop d) (quasiquote-expand-0 (cdr x))
        (when (eq dtop 'unquote-splicing)
          (error ",@ after dot"))
        (when (eq dtop 'unquote-nsplicing)
          (error ",. after dot"))
        (cond
          ((eq atop 'unquote-splicing)
           (if (null dtop)
               (if (unquote-xsplicing-p a)
                   (values 'append (list a))
                   (expand-unquote a))
               (values 'append
                       (cond ((eq dtop 'append)
                              (cons a d))
                             (t (list a (quasiquote-expand-1 dtop d)))))))
          ((eq atop 'unquote-nsplicing)
           (if (null dtop)
               (if (unquote-xsplicing-p a)
                   (values 'nconc (list a))
                   (expand-unquote a))
               (values 'nconc
                       (cond ((eq dtop 'nconc)
                              (cons a d))
                             (t (list a (quasiquote-expand-1 dtop d)))))))
          ((null dtop)
           (if (member atop '(quote :literal nil))
               (values 'quote (list a))
               (values 'list (list (quasiquote-expand-1 atop a)))))
          ((member dtop '(quote :literal))
           (cond
             ((member atop '(quote :literal nil))
              (values 'quote (cons a d)))
             ((and (eq dtop 'quote) (consp d) (null (cdr (last d))))
              (values 'list (list* (quasiquote-expand-1 atop a)
                                   (mapcar 'kwote d))))
             (t
              (values 'list* (list (quasiquote-expand-1 atop a)
                                   (quasiquote-expand-1 dtop d))))))
          (t (let ((qa (quasiquote-expand-1 atop a)))
               (if (member dtop '(list list*))
                   (values dtop (cons qa d))
                   (values 'list*
                           (list qa (quasiquote-expand-1 dtop d))))))))))
   (t
    (error "unrecognized object in quasiquote"))))

(defun expand-unquote (x)
  (cond
    ((null x)
     (values nil nil))
    ((literalp x)
     (values #+quasiquote-passes-literals :literal #-quasiquote-passes-literals 'quote x))
    ((symbolp x)
     (values 'unquote x))
    #+quasiquote-at-macro-expansion-time
    ((simple-vector-p x)
     (values 'make-vector (quasiquote-expand (coerce x 'cl:list))))
    ((not (consp x))
     (error "unrecognized object in unquote"))
    ((and (quotep x)
          (not (unquote-xsplicing-p (single-arg x))))
     (values 'quote (single-arg x)))
    ((member (car x) '(list list* append nconc))
     (values (car x) (cdr x)))
    ((eq (car x) 'cons)
     (values 'list* (cdr x)))
    (t (values 'unquote x))))

(defun quasiquote-expand-1 (top x)
  "Given a top token and an expression, give the quasiquoting
of the result of the top operation applied to the expression"
  (cond
    ((member top '(unquote :literal nil))
     x)
    ((eq top 'quote)
     (kwote x))
    ((eq top 'list*)
     (cond ((and (null (cddr x))
                 (not (unquote-xsplicing-p (car x)))
                 (not (unquote-xsplicing-p (cadr x))))
            (k-cons (car x) (cadr x)))
           ((unquote-xsplicing-p (car (last x)))
            (k-append
             (quasiquote-expand-1 'list (butlast x))
             (car (last x))))
           (t
            (apply 'k-list* x))))
    (t
     (cons (ecase top
             ((list) 'list)
             ((cons) 'cons)
             ((append) 'append)
             ((nconc) 'nconc)
             ((make-vector) 'make-vector)
             ((n-vector) 'n-vector))
           x))))

;; Note: it would be a *very bad* idea to use quasiquote:quote
;; in the expansion of the macro-character #\'

(defun call-with-quasiquote-reader (thunk)
  (let ((*quasiquote-level* (1+ *quasiquote-level*)))
    (make-quasiquote (funcall thunk))))

(defun call-with-unquote-reader (thunk)
  (let ((*quasiquote-level* (1- *quasiquote-level*)))
    (unless (>= *quasiquote-level* 0) (error "unquote outside quasiquote"))
    (make-unquote (funcall thunk))))

(defun call-with-unquote-splicing-reader (thunk)
  (let ((*quasiquote-level* (1- *quasiquote-level*)))
    (unless (>= *quasiquote-level* 0) (error "unquote-splicing outside quasiquote"))
    (make-unquote-splicing (funcall thunk))))

(defun call-with-unquote-nsplicing-reader (thunk)
  (let ((*quasiquote-level* (1- *quasiquote-level*)))
    (unless (>= *quasiquote-level* 0) (error "unquote-nsplicing outside quasiquote"))
    (make-unquote-nsplicing (funcall thunk))))

(defun read-quasiquote (stream)
  (call-with-quasiquote-reader (lambda () (read stream t nil t))))

(defun read-unquote (stream)
  (call-with-unquote-reader (lambda () (read stream t nil t))))

(defun read-unquote-splicing (stream)
  (call-with-unquote-splicing-reader (lambda () (read stream t nil t))))

(defun read-unquote-nsplicing (stream)
  (call-with-unquote-nsplicing-reader (lambda () (read stream t nil t))))

(defun read-vector (stream n)
  ;; http://www.lisp.org/HyperSpec/Body/sec_2-4-8-3.html
  (let ((contents (read-delimited-list #\) stream t)))
    (if (> *quasiquote-level* 0)
        (if n
            (make-unquote (k-n-vector n (quasiquote-expand contents)))
            (make-unquote (k-make-vector (quasiquote-expand contents))))
	(n-vector n contents))))

(defun read-read-time-backquote (stream char)
  (declare (ignore char))
  (values (macroexpand-1 (read-quasiquote stream))))
(defun read-macroexpand-time-backquote (stream char)
  (declare (ignore char))
  (read-quasiquote stream))
(defun read-backquote (stream char)
  #-quasiquote-at-macro-expansion-time (read-read-time-backquote stream char)
  #+quasiquote-at-macro-expansion-time (read-macroexpand-time-backquote stream char))
(defun backquote-reader (expansion-time)
  (ecase expansion-time
    ((read) #'read-read-time-backquote)
    ((macroexpand) #'read-macroexpand-time-backquote)
    ((nil) #'read-backquote)))
(defun read-comma (stream char)
  (declare (ignore char))
  (case (peek-char nil stream t nil t)
    ((#\@)
     (read-char stream t nil t)
     (read-unquote-splicing stream))
    ((#\.)
     (read-char stream t nil t)
     (read-unquote-nsplicing stream))
    (otherwise (read-unquote stream))))
(defun read-hash-paren (stream subchar arg)
  (declare (ignore subchar))
  (read-vector stream arg))

(defun enable-quasiquote (&key expansion-time (table *readtable*))
  ;; Note that it is *NOT* OK to enable-quasiquote in the initial readtable,
  ;; as this violates the build contract (see ASDF 3.1 documentation about readtables).
  ;; Please only use it in your own private readtable,
  ;; and/or use system fare-quasiquote-readtable and use
  ;;   (named-readtables:in-readtable :fare-quasiquote)
  (set-macro-character #\` (backquote-reader expansion-time) nil table)
  (set-macro-character #\, #'read-comma nil table)
  (when (member expansion-time '(read #-quasiquote-at-macro-expansion-time nil))
    (set-dispatch-macro-character #\# #\( #'read-hash-paren table))
  t)

(defvar *fq-readtable* (let ((x (copy-readtable nil))) (enable-quasiquote :table x) x))

);eval-when
