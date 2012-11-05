;;; -*- Lisp -*-

(in-package :asdf)

(defsystem :fare-quasiquote-extras
  :description "fare-quasiquote plus extras"
  :depends-on (:fare-quasiquote-optima :fare-quasiquote-readtable)) ;; not including deprecated fare-matcher
