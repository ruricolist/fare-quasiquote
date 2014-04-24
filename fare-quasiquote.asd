;;; -*- Lisp -*-

(defsystem :fare-quasiquote
  :description "Portable, matchable implementation of quasiquote"
  :long-description "fare-quasiquote implements
  a portable quasiquote that you can control."
  :license "MIT"
  :version "0.9.1"
  :depends-on (:fare-utils)
  :components
  ((:file "packages")
   (:file "quasiquote" :depends-on ("packages"))
   (:file "pp-quasiquote" :depends-on ("quasiquote")))
  :in-order-to ((test-op (test-op :fare-quasiquote-test)))) ;; Don't try that with ASDF2!
