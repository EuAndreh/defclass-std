(defsystem defclass-std-test
  :depends-on (defclass-std
               prove)
  :components ((:module "t"
                :components
                ((:test-file "defclass-std"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
