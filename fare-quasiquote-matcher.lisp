;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; fare-matcher implementation of fare-quasiquote
;;; Copyright (c) 2002-2012 Fahree Wreido <fare@tunes.org>

#+xcvb (module (:depends-on ("quasiquote" "/fare-matcher")))

(in-package :fare-quasiquote)

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)

;; the below instruction enables pattern-matching for the simplifier.
(fare-matcher:copy-function-matcher
 list  cl:list
 list* cl:list*
 cons  cl:cons
 quote cl:quote
 vector cl:vector)

(fare-matcher:define-symbol-matcher knil
  '#'(lambda (form)
       (or (null form)
	   (and (quotep form) (null (single-arg form)))
	   (m%fail))))

(fare-matcher:define-macro-matcher quasiquote
  #'(lambda (x) (fare-matcher:pattern-matcher (quasiquote-expand x))))

);eval-when
