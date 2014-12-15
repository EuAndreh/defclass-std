(defpackage defclass-std-test
  (:use cl defclass-std prove))
(in-package defclass-std-test)

;; NOTE: To run this test file, execute `(asdf:test-system :defclass-std)' in your Lisp.

(plan 1)

(deftest test-all-keyword-option
  (is-expand (defclass/std computer (gadget)
               ((screen mouse keyboard :a :type string :with-prefix)
                (bluetooth touchpad :wi)
                (speaker microphone :r)
                (place :@@ :with :doc "Where it is" :r)
                (owner :static :std "Me" :w)))
             (DEFCLASS COMPUTER (GADGET)
               ((SCREEN :ACCESSOR COMPUTER-SCREEN :INITFORM NIL :TYPE STRING)
                (MOUSE :ACCESSOR COMPUTER-MOUSE :INITFORM NIL :TYPE STRING)
                (KEYBOARD :ACCESSOR COMPUTER-KEYBOARD :INITFORM NIL :TYPE STRING)
                (BLUETOOTH :WRITER BLUETOOTH :INITARG :BLUETOOTH :INITFORM NIL)
                (TOUCHPAD :WRITER TOUCHPAD :INITARG :TOUCHPAD :INITFORM NIL)
                (SPEAKER :READER SPEAKER :INITFORM NIL)
                (MICROPHONE :READER MICROPHONE :INITFORM NIL)
                (PLACE :READER COMPUTER-PLACE :INITFORM NIL :ALLOCATION :CLASS
                       :DOCUMENTATION "Where it is")
                (OWNER :WRITER OWNER :INITFORM "Me" :ALLOCATION :CLASS)))))

(run-test-all)
