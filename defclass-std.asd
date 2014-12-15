(defpackage defclass-std-asd
  (:use cl asdf))
(in-package defclass-std-asd)

(defsystem defclass-std
  :version "0.1"
  :author "André Miranda"
  :license "LLGPL"
  :depends-on (quickutil
               anaphora)
  :components ((:module "src"
                :components
                ((:file "defclass-std"))))
  :description "A shortcut macro to write DEFCLASS forms quickly."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op defclass-std-test))))
