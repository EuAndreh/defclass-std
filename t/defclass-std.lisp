(defpackage defclass-std-test
  (:use cl defclass-std prove))
(in-package defclass-std-test)

;; NOTE: To run this test file, execute `(asdf:test-system :defclass-std)' in your Lisp.

(plan 3)

(deftest class/std-expansion
  (is-expand (class/std stub slot1 slot2 slot3 slot4 slot5)
             (DEFCLASS/STD STUB ()
               ((SLOT1 SLOT2 SLOT3 SLOT4 SLOT5)))
             "CLASS/STD expands correctly."))

(deftest default-accessor-initarg
  (is-expand (DEFCLASS/STD STUB ()
               ((SLOT1 SLOT2 SLOT3 SLOT4 SLOT5)))
             (DEFCLASS STUB ()
               ((SLOT1 :ACCESSOR SLOT1 :INITARG :SLOT1 :INITFORM NIL)
                (SLOT2 :ACCESSOR SLOT2 :INITARG :SLOT2 :INITFORM NIL)
                (SLOT3 :ACCESSOR SLOT3 :INITARG :SLOT3 :INITFORM NIL)
                (SLOT4 :ACCESSOR SLOT4 :INITARG :SLOT4 :INITFORM NIL)
                (SLOT5 :ACCESSOR SLOT5 :INITARG :SLOT5 :INITFORM NIL)))
             "Defaults omitted args (:ai) works correctly."))

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
