(defsystem defclass-std
  :version "0.1.1"
  :author "André Miranda"
  :maintainer "André Miranda"
  :mailto "andremiramor@gmail.com"
  :homepage "https://github.com/EuAndreh/defclass-std"
  :bug-tracker "https://github.com/EuAndreh/defclass-std/issues"
  :source-control (:git "git@github.com:EuAndreh/defclass-std.git")
  :license "LLGPL"
  :depends-on (alexandria
               anaphora)
  :components ((:module "src"
                        :components
                        ((:file "defclass-std")))
               (:static-file "README.md"))
  :description "A shortcut macro to write DEFCLASS forms quickly."
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op defclass-std-test))))
