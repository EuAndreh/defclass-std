(defpackage defclass-std-test-asd
  (:use cl asdf))
(in-package defclass-std-test-asd)

(defsystem defclass-std-test
  :author "André Miranda"
  :license "LLGPL"
  :depends-on (defclass-std
               prove)
  :components ((:module "t"
                :components
                ((:test-file "defclass-std"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
