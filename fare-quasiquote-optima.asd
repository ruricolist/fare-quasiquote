;;; -*- Lisp -*-

(defsystem "fare-quasiquote-optima"
  #+asdf3 :version #+asdf3 (:read-file-line "version.text")
  :depends-on ("fare-quasiquote" (:version "optima" "0.2"))
  :components ((:file "fare-quasiquote-optima")))
