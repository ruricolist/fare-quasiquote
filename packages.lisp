#+xcvb (module ())

(in-package #:cl)

(defpackage #:fare-quasiquote
  (:use #:fare-utils #:common-lisp)
  (:shadow #:list #:append #:nconc #:list* #:cons #:quote #:vector)
  (:shadow #:kwote #:quotep #:n-vector #:make-vector)
  (:documentation
   "Quasiquote implementation with and for pattern-matching")
  (:export #:quasiquote-expand #:quasiquote #:unquote #:unquote-splicing
	   #:enable-quasiquote #:*fq-readtable*
           #:enable-qq-pp #:*fq-pprint-dispatch*
           #:call-with-quasiquote-reader
           #:call-with-unquote-reader
           #:call-with-unquote-splicing-reader
           #:call-with-unquote-nsplicing-reader))
