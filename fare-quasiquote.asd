;;; -*- Lisp -*-

(defsystem :fare-quasiquote
  :description "Portable, hackable, matchable implementation of quasiquote"
  :long-description "fare-quasiquote implements a portable quasiquote that you can control."
  :depends-on (:fare-utils)
  :serial t
  :components
  ((:file "packages")
   (:file "quasiquote" :depends-on ("packages"))
   (:file "pp-quasiquote" :depends-on ("quasiquote"))))

(defmethod perform ((op test-op) (sys (eql (find-system :fare-quasiquote))))
  (format t "~&Testing fare-quasiquote")
  (load-system :fare-quasiquote-test)
  (funcall (find-symbol* :test-quasiquote :fare-quasiquote)))
