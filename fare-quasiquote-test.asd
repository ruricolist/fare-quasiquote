;;; -*- Lisp -*-

(asdf:defsystem :fare-quasiquote-test
  :depends-on (:fare-quasiquote-matcher :fare-quasiquote-optima :hu.dwim.stefil)
  :components ((:file "quasiquote-test")))
