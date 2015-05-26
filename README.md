# defclass-std - Standard class writing macro
[![Build Status](https://travis-ci.org/EuAndreh/defclass-std.svg?branch=master)](https://travis-ci.org/EuAndreh/defclass-std)
[![Coverage Status](https://coveralls.io/repos/EuAndreh/defclass-std/badge.svg?branch=master)](https://coveralls.io/r/EuAndreh/defclass-std?branch=master)

Most times, when sketching out a new class, I often commit lots of typos and forget to add an `:initform`.

Also, the throw away class designed in the beginning may thrive and stay the same. If only there was a way to overcome these problems... There is!

This simple macro atempts to give a very DRY and succint interface to the common `DEFCLASS` form. The goal is to offer most of the capabilities of a normal `DEFCLASS`, only in a more compact way.

Everything compiles down to `DEFCLASS`.

## Usage
```lisp
* (ql:quickload :defclass-std)
; => (:DEFCLASS-STD)
* (import 'defclass-std:defclass/std)
; => T
```

A simple class defined with `DEFCLASS/STD` looks like this:
```lisp
(defclass/std example ()
  ((slot1 slot2 slot3)))

; which expands to:

(DEFCLASS EXAMPLE ()
  ((SLOT1 :ACCESSOR SLOT1 :INITARG :SLOT1 :INITFORM NIL)
   (SLOT2 :ACCESSOR SLOT2 :INITARG :SLOT2 :INITFORM NIL)
   (SLOT3 :ACCESSOR SLOT3 :INITARG :SLOT3 :INITFORM NIL)))
```
   As you can see, by default, the macro adds three options:
   1. `:accessor` + the name of the slot
   2. `:initarg` + the name of the slot
   3. `:initform nil`

   If you want to change the `:initform` value, you can use the `:std` option:
```lisp
(defclass std-test ()
  ((slot :std 1)))

; expands to:

(DEFCLASS STD-TEST ()
  ((SLOT :ACCESSOR SLOT :INITARG :SLOT :INITFORM 1)))
```

   If you want to omit the `:initform` option, you have two ways:
   1. Use `:std :unbound` explicitly
   2. Change the value of `*default-std*`. By default it is set to `T`, so, when the `:std` option is omitted, `:initform` is set to nil. When `*default-std*` is set to nil, `:initform` is omitted when `:std` is omitted.
```lisp
(defclass/std omit-std ()
  ((slot :std :unbound)))

; which is (semantically) equivalent to:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *default-std* nil))
(defclass/std omit-std ()
  ((slot)))

; which (both) expands to:

(DEFCLASS OMIT-STD ()
  ((SLOT :ACCESSOR SLOT :INITARG :SLOT)))
```

   `:a`, `:i`, `:r` and `:w` are connected: when all of them are omitted, `:a` and `:i` are inserted by default.

   `:a` stands for `:accessor`, `:i` stands for `:initarg`, `:r` stands for `:reader` and `:w` stands for `:writer`.

   If any of those is present, the default (`:a` and `:i`) is omitted.
```lisp
(defclass/std airw ()
  ((slot1 slot2)
   (slot3 slot4 :r)
   (slot5 :w)
   (slot6 :a)
   (slot7 :ri)))

; which expands to:

(DEFCLASS AIRW ()
  ((SLOT1 :ACCESSOR SLOT1 :INITARG :SLOT1 :INITFORM NIL)
   (SLOT2 :ACCESSOR SLOT2 :INITARG :SLOT2 :INITFORM NIL)
   (SLOT3 :READER SLOT3 :INITFORM NIL)
   (SLOT4 :READER SLOT4 :INITFORM NIL)
   (SLOT5 :WRITER SLOT5 :INITFORM NIL)
   (SLOT6 :ACCESSOR SLOT6 :INITFORM NIL)
   (SLOT7 :READER SLOT7 :INITARG :SLOT7 :INITFORM NIL)))
```
   Note that slot7 has an `:ri` option. That's just `:r` and `:i` together.

   If you want to use `:r` and `:w` together, use `:a` instead, or you'll get an error. The same stands for `:a` + `:r` and `:a` + `:w`.

   You can choose to add the class name as a prefix for the acessor/reader/writer function. Just put `:with` or `:with-prefix` option.

```lisp
(defclass/std example ()
  ((slot1 :with)
   (slot2)))

; which expands to:

(DEFCLASS WITH ()
  ((SLOT1 :ACCESSOR EXAMPLE-SLOT1 :INITARG :SLOT1 :INITFORM NIL)
   (SLOT2 :ACCESSOR SLOT2 :INITARG :SLOT2 :INITFORM NIL)))
```

   To make a slot static (class-allocated), use `:@@` or `:static`.

   To declare the type of a slot or to add documentation to a slot, use `:type` and `:doc`, respectively.

   For real quick, concise, dense and standard class definitions, use `CLASS/STD`:
```lisp
(class/std example slot1 slot2 slot3)

; which expands to:

(DEFCLASS/STD EXAMPLE ()
  ((SLOT1 SLOT2 SLOT3)))

; which expands to:

(DEFCLASS EXAMPLE ()
  ((SLOT1 :ACCESSOR SLOT1 :INITARG :SLOT1 :INITFORM NIL)
   (SLOT2 :ACCESSOR SLOT2 :INITARG :SLOT2 :INITFORM NIL)
   (SLOT3 :ACCESSOR SLOT3 :INITARG :SLOT3 :INITFORM NIL)))
```

   You can also add the prefix by default by changing the value of the `*with-prefix*` special variable (defaults to `nil`):
```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *with-prefix* t))
(defclass/std pre ()
  ((fix)))

; which expands to:

(DEFCLASS PRE ()
  ((FIX :ACCESSOR PRE-FIX :INITARG :FIX)))
```

   Unknown keywords are left intact:
```lisp
(defclass/std unknown ()
  ((slot :unknown :keywords)))

; which expands to:

(DEFCLASS UNKNOWN ()
  ((SLOT :ACCESSOR SLOT :INITARG :SLOT :INITFORM NIL :KEYWORDS :UNKNOWN)))


; Or, even using custom accessors:

(defclass/std unknown ()
  ((slot :unknown :wi :keywords)))

; which expands to:

(DEFCLASS UNKNOWN ()
  ((SLOT :WRITER SLOT :INITARG :SLOT :INITFORM NIL :KEYWORDS :UNKNOWN)))
```
## Examples

```lisp
(defclass/std computer (gadget)
  ((screen mouse keyboard :a :type string :with-prefix)
   (bluetooth touchpad :wi)
   (speaker microphone :r)
   (place :@@ :with :doc "Where it is" :r)
   (owner :static :std "Me" :w)))

  ; expands to:

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
   (OWNER :WRITER OWNER :INITFORM "Me" :ALLOCATION :CLASS)))
```

   Real life examples:

   From [cl-inflector](https://github.com/AccelerationNet/cl-inflector/blob/master/langs.lisp][cl-inflector):
```lisp
(defclass language ()
  ((name :accessor name :initarg :name :initform nil)
   (plurals :accessor plurals :initarg :plurals :initform nil)
   (singulars :accessor singulars :initarg :singulars :initform nil)
   (uncountables :accessor uncountables :initarg :uncountables :initform nil)
   (irregulars :accessor irregulars :initarg :irregulars :initform nil)))

; could be written:

(defclass/std language ()
  ((name plurals singulars uncountables irregulars)))

; or, using CLASS/STD:

(class/std language name plurals singulars uncountables irregulars)
```
   From [clack](https://github.com/fukamachi/clack/blob/9804d0b57350032ebdcf8539bae376b5528ac1f6/src/core/handler.lisp):
```lisp
(defclass <handler> ()
     ((server-name :type keyword
                   :initarg :server-name
                   :accessor server-name)
      (acceptor :initarg :acceptor
                :accessor acceptor)))

; could be written (with *default-std* set to nil)
(defclass/std language ()
  ((server-name :type keyword)
   (acceptor)))
```
   From [RESTAS](https://github.com/archimag/restas/blob/3e37f868141c785d2468fab342d57cca2e2a40dd/src/route.lisp):
```lisp
(defclass route (routes:route)
  ((symbol :initarg :symbol :reader route-symbol)
   (module :initarg :module :initform nil :reader route-module)
   (required-method :initarg :required-method :initform nil
                    :reader route-required-method)
   (arbitrary-requirement :initarg :arbitrary-requirement :initform nil
                          :reader route-arbitrary-requirement)
   (render-method :initarg :render-method :initform #'identity)
   (headers :initarg :headers :initform nil :reader route-headers)
   (variables :initarg :variables :initform nil)
   (additional-variables :initarg :additional-variables :initform nil)))

; could be written
(defclass/std route (routes-route)
  ((symbol :ri :with-prefix :std :unbound)
   (module required-method arbitrary-requirement
           headers variables additional-variables :ri)
   (render-method :i :std #'identity)
   (header :ir)))
```
   From [defclass-star example](http://common-lisp.net/project/defclass-star/configuration.lisp.html):
```lisp
(defclass configuration ()
  ((package-name      :type symbol  :initarg :package-name      :accessor package-name-of)
   (package-nicknames :initform '() :initarg :package-nicknames :accessor package-nicknames-of)
   (included-files    :initform '() :initarg :included-files    :accessor included-files-of)
   (gccxml-path       :initform "gccxml" :initarg :gccxml-path  :accessor gccxml-path-of)
   (gccxml-flags      :initform ""  :initarg :gccxml-flags      :accessor gccxml-flags-of)
   (hidden-symbols    :initform '() :initarg :hidden-symbols    :accessor hidden-symbols-of)
   (output-filename   :initform nil :initarg :output-filename   :accessor output-filename-of)
   (options           :initform (standard-configuration-options)
                      :initarg :options
                      :accessor options-of)
   (symbol-export-filter :initform 'standard-symbol-export-filter
                         :type (or (function (symbol)) symbol)
                         :initarg :symbol-export-filter
                         :accessor symbol-export-filter-of)
   (function-name-transformer :initform 'standard-name-transformer
                              :type (or (function (string)) symbol)
                              :initarg :function-name-transformer
                              :accessor function-name-transformer-of)
   (variable-name-transformer :initform 'standard-name-transformer
                              :type (or (function (string)) symbol)
                              :initarg :variable-name-transformer
                              :accessor variable-name-transformer-of)
   (type-name-transformer :initform 'standard-name-transformer
                          :type (or (function (string)) symbol)
                          :initarg :type-name-transformer
                          :accessor type-name-transformer-of)
   (temp-directory    :initform (make-pathname :directory "/tmp")
                      :initarg :temp-directory
                      :accessor temp-directory-of)
   (working-directory :initform *default-pathname-defaults*
                      :initarg :working-directory
                      :accessor working-directory-of)))

;;; And the equivalent defclass* version (56 tree leaves):
(defclass* configuration ()
  ((package-name
                              :type symbol)
   (package-nicknames         '())
   (included-files            '())
   (gccxml-path               "gccxml")
   (gccxml-flags              "")
   (hidden-symbols            '())
   (output-filename           nil)
   (options                   (standard-configuration-options))
   (symbol-export-filter      'standard-symbol-export-filter
                              :type (or (function (symbol)) symbol))
   (function-name-transformer 'standard-name-transformer
                              :type (or (function (string)) symbol))
   (variable-name-transformer 'standard-name-transformer
                              :type (or (function (string)) symbol))
   (type-name-transformer     'standard-name-transformer
                              :type (or (function (string)) symbol))
   (temp-directory            (make-pathname :directory "/tmp"))
   (working-directory         *default-pathname-defaults*)))

;; And the equivalent defclass/std version (46 tree leaves):
(defclass/std configuration ()
  ((package-name :type symbol :std :unbound)
   (package-nicknames included-files hidden-symbols output-filename)
   (gccxml-path :std "gccxml")
   (gccxml-flags :std "")
   (options :std (standard-configuration-options))
   (symbol-export-filter :std 'standard-symbol-export-filter
                         :type (or (function (symbol)) symbol))
   (function-name-transformer variable-name-transformer type-name-transformer
                              :std 'standard-name-transformer
                              :type (or (function (string)) symbol))
   (temp-directory :std (make-pathname :directory "/tmp"))
   (working-directory :std *default-pathname-defaults*)))
```
   From [cl-hue](https://github.com/jd/cl-hue/blob/master/cl-hue.lisp):
```lisp
(defclass light ()
  ((bridge :initarg :bridge :accessor light-bridge)
   (number :initarg :number :accessor light-number)
   (type :initarg :type :accessor light-type)
   (name :initarg :name :accessor light-name)
   (modelid :initarg :modelid :accessor light-modelid)
   (uniqueid :initarg :uniqueid :accessor light-uniqueid)
   (swversion :initarg :swversion :accessor light-swversion)
   (pointsymbol :initarg :pointsymbol :accessor light-pointsymbol)
   (on :initarg :on :accessor light-on-p)
   (brightness :initarg :brightness :accessor light-brightness)
   (hue :initarg :hue :accessor light-hue)
   (saturation :initarg :saturation :accessor light-saturation)
   (xy :initarg :xy :accessor light-xy)
   (ct :initarg :ct :accessor light-ct)
   (alert :initarg :alert :accessor light-alert)
   (effect :initarg :effect :accessor light-effect)
   (colormode :initarg :colormode :accessor light-colormode)
   (reachable :initarg :reachable :accessor light-reachable-p)))

; could be written:
(defclass/std light ()
  ((bridge number type name modelid uniqueid swversion pointsymbol on brightness
           hue saturation xy ct alert effect colormode reachable
           :with-prefix :std :unbound)))

; or, using class/std:

(class/std light
  bridge number type name modelid uniqueid swversion pointsymbol on brightness
  hue saturation xy ct alert effect colormode reachable
  :std :unbound :with)

; or, with *default-std* set to nil and *with-prefix* set to t:

(class/std light
  bridge number type name modelid uniqueid swversion pointsymbol on brightness
  hue saturation xy ct alert effect colormode reachable)
```

   There's a shortcut to setup a basic printing behaviour of a class, using `printing-unreadably`:
```lisp
(printing-unreadably (field2 field3) (class/std myclass field1 field2 field3))

; which expands to:

(PROGN
  (CLASS/STD MYCLASS FIELD1 FIELD2 FIELD3)
  (DEFMETHOD PRINT-OBJECT ((MYCLASS MYCLASS) #:STREAM1722)
    (PRINT-UNREADABLE-OBJECT (MYCLASS #:STREAM1722 :TYPE T :IDENTITY T)
      (FORMAT #:STREAM1722 "FIELD2: ~s, FIELD3: ~s"
              (FIELD2 MYCLASS) (FIELD3 MYCLASS)))))
```

## Dependencies
This project depends only on [Anaphora](http://common-lisp.net/project/anaphora/) and [Alexandria](https://common-lisp.net/project/alexandria/) libraries. The test package uses the [prove](github.com/fukamachi/prove) test library.

## Installation
Available on [Quicklisp](http://quicklisp.org):
```
(ql:quickload :defclass-std)
```

## Bugs
If you find any bug or inconsistency in the code, or if you find it too hard to use, please, feel free to open an issue.

## Tests
This library is tested under [ABCL](https://common-lisp.net/project/armedbear/), [SBCL](http://www.sbcl.org/), [CCL](http://ccl.clozure.com/), [CLISP](http://www.clisp.org/) and [ECL](https://common-lisp.net/project/ecl/) Common Lisp implementations.

To run all the defined tests, use:
```lisp
* (asdf:test-system :defclass-std)
; prints lots of (colorful) stuff...
; => T
```

Tests are ran with [Travis CI](https://travis-ci.org/EuAndreh/defclass-std) using [cl-travis](https://github.com/luismbo/cl-travis) and [CIM](https://github.com/KeenS/CIM). Check it out!

## Authors
+ [André Miranda](https://github.com/EuAndreh)
+ [Joram Schrijver](https://github.com/jorams)

## License
[LLGPL](https://tldrlegal.com/license/lisp-lesser-general-public-license#fulltext).
