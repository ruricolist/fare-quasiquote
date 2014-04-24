;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; named-readtables support for fare-quasiquote
;;; Copyright (c) 2011-2014 Fahree Wreido <fare@tunes.org>
;;; See README

#+xcvb (module (:depends-on ("quasiquote" (:asdf "named-readtables"))))

(in-package :fare-quasiquote)

(eval-now
  (named-readtables:defreadtable :fare-quasiquote-mixin
    (:macro-char #\` #'read-read-time-backquote)
    (:macro-char #\, #'read-comma)
    (:macro-char #\# :dispatch)
    #-quasiquote-at-macro-expand-time
    (:dispatch-macro-char #\# #\( #'read-hash-paren))
  (named-readtables:defreadtable :fare-quasiquote
    (:fuze :standard :fare-quasiquote-mixin)))

#| ;; To use it:

(named-readtables:in-readtable :fare-quasiquote)

|#
