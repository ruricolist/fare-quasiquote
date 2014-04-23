;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; optima implementation for fare-quasiquote
;;; Copyright (c) 2002-2014 Fahree Wreido <fare@tunes.org>

#+xcvb (module (:depends-on ("quasiquote" (:asdf "optima"))))

(in-package :fare-quasiquote)

(declaim (optimize (speed 1) (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)

;; the below instruction enables pattern-matching for the simplifier.
(macrolet ((def (&rest x)
               `(progn
                  ,@(loop :for (a b) :on x :by #'cddr :collect
                          `(optima:defpattern ,a (&rest r) `(,',b ,@r))))))
  (def
    list  cl:list
    list* cl:list*
    cons  cl:cons
    quote cl:quote
    vector cl:vector))

(optima:defpattern knil () ''nil)

(optima:defpattern quasiquote (x)
  (quasiquote-expand x))

);eval-when
