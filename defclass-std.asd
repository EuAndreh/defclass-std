(in-package cl-user)
(defpackage defclass-std-asd
  (:use cl asdf))
(in-package defclass-std-asd)

(defsystem defclass-std
  :version "0.1"
  :author "André Miranda"
  :license "LLGPL"
  :depends-on (alexandria
               anaphora)
  :components ((:module "src"
                :components
                ((:file "defclass-std"))))
  :description "A shortcut macro to write DEFCLASS forms quickly."
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.org"))
  :in-order-to ((test-op (test-op defclass-std-test))))
