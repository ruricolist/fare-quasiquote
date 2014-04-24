;;;; pretty-printing of backquote expansions

#+xcvb (module (:depends-on ("quasiquote")))

;;;; This software is originally derived from the CMU CL system via SBCL.
;;;; CMU CL was written at Carnegie Mellon University and released into
;;;; the public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty.

(in-package :fare-quasiquote)

(defun pprint-starts-with-dot-or-at-p (form)
  (and
   (symbolp form)
   (let ((output (with-output-to-string (s)
                   (write form :stream s
                               :level (min 1 (or *print-level* 1))
                               :length (min 1 (or *print-length* 1))))))
     (and (plusp (length output))
          (or (char= (char output 0) #\.)
              (char= (char output 0) #\@))))))

(defun quasiquote-unexpand (x)
  (assert (and (consp x) (member (car x) '(list list* cons append nconc make-vector n-vector quote))))
  (make-quasiquote (quasiquote-unexpand-00 x)))

(defun quasiquote-unexpand-00 (x)
  (quasiquote-unexpand-0 (car x) (cdr x)))

(defun quasiquote-unexpand-0 (top x)
  (ecase top
    ((quote)
     (assert (length=n-p x 1))
     (car x))
    ((make-vector)
     (assert (length=n-p x 1))
     (list 'x-n-vector nil (quasiquote-unexpand-00 (car x))))
    ((n-vector)
     (assert (length=n-p x 2))
     (list 'x-n-vector (car x) (quasiquote-unexpand-00 (cadr x))))
    ((list)
     (mapcar #'(lambda (el) (quasiquote-unexpand-1 'unquote el)) x))
    ((list* cons)
     ;;(apply 'list* (mapcar #'(lambda (el) (quasiquote-unexpand-1 'unquote el)) x))
     (nconc (mapcar #'(lambda (el) (quasiquote-unexpand-1 'unquote el)) (butlast x))
            (quasiquote-unexpand-1 'unquote-splicing (car (last x)))))
    ((append)
     (apply 'cl:append (mapcar (lambda (el) (quasiquote-unexpand-1 'unquote-splicing el)) x)))
    ((nconc)
     (apply 'cl:append (mapcar (lambda (el) (quasiquote-unexpand-1 'unquote-nsplicing el)) x)))))

(defun quasiquote-unexpand-2 (top form)
  (ecase top
    ((unquote)
     (make-unquote form))
    ((unquote-splicing)
     (list (make-unquote-splicing form)))
    ((unquote-nsplicing)
     (list (make-unquote-nsplicing form)))))

(defun quasiquote-unexpand-1 (top x)
  (cond
    ((literalp x)
     (ecase top
       ((nil) (kwote x))
       ((unquote) x)
       ((unquote-splicing unquote-nsplicing) (list x))))
    ((atom x)
     (quasiquote-unexpand-2 top x))
    ((not (null (cdr (last x))))
     (error "found illegal dotted quasiquote form: ~S" x))
    ((and (eq top 'unquote)
          (member (car x) '(list list* cons append nconc quote make-vector n-vector)))
     (quasiquote-unexpand-00 x))
    ((and (member top '(unquote-splicing unquote-nsplicing))
          (member (car x) '(list list* cons append nconc quote)))
     (quasiquote-unexpand-00 x))
    (t
     (quasiquote-unexpand-2 top x))))

(defun pprint-quasiquote (stream form &rest noise)
  (declare (ignore noise))
  (write (quasiquote-unexpand form) :stream stream))

(defun pprint-unquasiquote (stream form &rest noise)
  (declare (ignore noise))
  (block nil
    (cond
      ((quasiquotep form)
       (write-char #\` stream))
      ((unquotep form)
       (let ((x (second form)))
         (when (or (quotep x) (k-list-p x) (k-list*-p x) (k-cons-p x)
                   (make-vector-p x) (k-n-vector-p x))
           (write (quasiquote-unexpand-00 x) :stream stream)
           (return)))
       (write-char #\, stream)
       (when (pprint-starts-with-dot-or-at-p (cadr form)) (write-char #\space stream)))
      ((unquote-splicing-p form)
       (write-string ",@" stream))
      ((unquote-nsplicing-p form)
       (write-string ",." stream))
      ((and (length=n-p form 3) (eq (car form) 'x-n-vector))
       (write-char #\# stream))
      (t
       ;; Given an invalid form. Instead of erroring out, revert to standard *p-p-d* (ugly).
       ;; Unhappily, there is no "call next method" here.
       (let ((*print-pprint-dispatch* (with-standard-io-syntax *print-pprint-dispatch*)))
         (write form :stream stream)
         (return))))
    (case (car form)
      ((x-n-vector)
       (when (cadr form) (write (cadr form) :stream stream))
       (write (caddr form) :stream stream))
      (otherwise
       (write (cadr form) :stream stream))))
  nil)

(defun enable-qq-pp (&key (priority 0) (table *print-pprint-dispatch*))
  ;; Printing the read-time forms
  (set-pprint-dispatch '(cl:cons (eql list)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql list*)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql cons)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql append)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql nconc)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql make-vector)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql n-vector)) 'pprint-quasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql quote)) 'pprint-quasiquote priority table)
  ;; Printing the macro-expansion-time forms
  (set-pprint-dispatch '(cl:cons (eql quasiquote)) 'pprint-unquasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql unquote)) 'pprint-unquasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql unquote-splicing)) 'pprint-unquasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql unquote-nsplicing)) 'pprint-unquasiquote priority table)
  (set-pprint-dispatch '(cl:cons (eql x-n-vector)) 'pprint-unquasiquote priority table)
  t)

(defvar *fq-pprint-dispatch*
  (let ((table (copy-pprint-dispatch nil)))
    (enable-qq-pp :table table)
    table))
