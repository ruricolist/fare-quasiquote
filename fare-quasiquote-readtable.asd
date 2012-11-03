;;; -*- Lisp -*-

(asdf:defsystem :fare-quasiquote-readtable
  :depends-on (:named-readtables :fare-quasiquote)
  :components ((:file "quasiquote-readtable")))
