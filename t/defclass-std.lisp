(defpackage defclass-std-test
  (:use cl defclass-std prove))
(in-package defclass-std-test)

;; NOTE: To run this test file, execute `(asdf:test-system :defclass-std)' in your Lisp.

(plan nil)



(run-test-all)
