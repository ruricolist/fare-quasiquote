;;; -*- Lisp -*-

(in-package :asdf)

(defsystem :fare-quasiquote
  :description "Portable, hackable, matchable implementation of quasiquote"
  :long-description "fare-quasiquote implements a portable quasiquote that you can control."
  :license "MIT"
  :depends-on (:fare-utils)
  :serial t
  :components
  ((:file "packages")
   (:file "quasiquote" :depends-on ("packages"))
   (:file "pp-quasiquote" :depends-on ("quasiquote")))
  :in-order-to ((test-op (test-op :fare-quasiquote-test))))
