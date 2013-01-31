;;; -*- Lisp -*-

(defsystem :fare-quasiquote-test
  :depends-on (:fare-quasiquote-matcher :fare-quasiquote-optima :hu.dwim.stefil)
  :components ((:file "quasiquote-test")))

(defmethod perform ((op test-op) (sys (eql (find-system :fare-quasiquote-test))))
  (format t "~&Testing fare-quasiquote")
  (load-system :fare-quasiquote-test)
  (funcall (find-symbol* :test-quasiquote :fare-quasiquote)))
