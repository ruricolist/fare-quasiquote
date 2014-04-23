;;; -*- Lisp -*-

(defsystem :fare-quasiquote-test
  :depends-on ("fare-quasiquote-extras" "hu.dwim.stefil")
  :components ((:file "quasiquote-test"))
  :perform (test-op (o c)
             (format! t "~&Testing fare-quasiquote")
             (symbol-call :fare-quasiquote/test :test-quasiquote)))
