(in-package cl-user)
(defpackage defclass-std
  (:use cl)
  (:import-from alexandria
                make-keyword)
  (:import-from anaphora
                aif
                it)
  (:export defclass/std
           *default-std*
           *with-prefix*
           class/std
           printing-unreadably)
  (:documentation "Main (and only) project package."))
(in-package defclass-std)

(defun extract-slot-names (line)
  "Finds all slot names in the LINE."
  (if (and line
           (not (keywordp (car line))))
      (cons (car line)
            (extract-slot-names (cdr line)))))

(defparameter *options* '(:a :r :w :i :static :with :with-prefix :@@ :static
                          :std :unbound :doc :type)
  "All available keyword options.")

(defparameter *fusioned-keyword-combinations*
  '(:ai :ar :aw :ia :ir :iw :ra :ri :rw :wa :wi :wr)
  "All possible combinations of :a, :i, :r and :w.")

(defparameter *default-std* t
  "Special var that changes the behaviour of the DEFCLASS/STD macro. If true, adds a :initform nil by default to every field, when unespecified. If false, adds nothing.")

(defparameter *with-prefix* nil
  "Special var that changes the behaviour of the DEFCLASS/STD macro. If tru, adds the class name as a prefix to every accessor/reader/writer function. If false, without the :with/:with-prefix slot option, adds nothing.")

(define-condition defclass/std-error (error)
  ((text :initarg text :reader text :type string
         :documentation "The formatting string to print the error message.")))

(define-condition fusioned-keys-error (defclass/std-error)
  ((keys :initarg keys :type keyword :reader keys
         :documentation "Mal-formed fusioned keyword.")))

(defun find-fusioned-keyword-options (line)
  "Should return a singleton list with the only fusioned element. Throws an error otherwise."
  (labels ((first-element (elements list)
           "Returns the element that appears first in the LIST."
           (if elements
               (if (= 1 (length elements))
                   (car elements)
                   (if (< (position (first elements) list)
                          (position (second elements) list))
                       (first-element (cons (first elements)
                                            (cddr elements))
                                      list)
                       (first-element  (cdr elements) list))))))
    (let* ((maybe-unknown-keywords (set-difference (remove-if-not #'keywordp line)
                                                   *options*))
           (fusioned-keywords (intersection *fusioned-keyword-combinations*
                                            maybe-unknown-keywords))
           (unknown-keywords-and-values
            (member (first-element
                     (set-difference maybe-unknown-keywords fusioned-keywords)
                     line)
                    line)))
      (cond ((null fusioned-keywords)
             (unless (or (member :a line)
                         (member :r line)
                         (member :w line)
                         (member :i line))
               (values :ai unknown-keywords-and-values))) ;; defaults to :AI
            ((= 1 (length fusioned-keywords))
             (values (car fusioned-keywords)
                     unknown-keywords-and-values))
            (t (error 'defclass/std-error
                      :text "Too many fusioned keyword options in DEFCLASS/STD: ~s. Invalid keyword option."
                      :keys fusioned-keywords))))))

(defun split-fusioned-keyword (line)
  "Splits the fusioned keyword option, if present."
  (multiple-value-bind (fusioned-keywords-key unknown-keywords)
      (find-fusioned-keyword-options line)
    (values (append (remove-if (lambda (element)
                                 (or (eql element fusioned-keywords-key)
                                     (member element unknown-keywords)))
                               line)
                    (when fusioned-keywords-key
                      (mapcar #'make-keyword
                              (coerce (symbol-name fusioned-keywords-key)
                                      'list))))
            unknown-keywords)))

(define-condition mixed-keyword-error (defclass/std-error)
  ((line :initarg line :reader line :type cons
         :documentation "The original line where the error was thrown."))
  (:report (lambda (error stream)
             (format stream (text error) (line error)))))

(defun check-for-repeated-keywords (line)
  "Verifies if keyword options were repeated. Mainly useful for avoiding things like (:A :AI) together, or (:R :W) instead of (:A)."
  (cond ((and (member :w line)
              (member :r line))
         (error
          'mixed-keyword-error
          :text "Use :A (accessor) instead of :W (writer) and :R (reader): ~s"
          :line line))
        ((and (member :w line)
              (member :a line))
         (error
          'mixed-keyword-error
          :text ":W (writer) and :A (accessor) shouldn't be together: ~s."
          :line line))
        ((and (member :r line)
              (member :a line))
         (error
          'mixed-keyword-error
          :text ":R (reader) and :A (accessor) shouldn't be together: ~s."
          :line line))))

(defun replace-keywords (line prefix unknown-keywords-and-values)
  "Receives a list of slots with keywords and returns a list of lists. Each sublist is a single slot, with all the options appended at the end."
  (flet ((mksym (&rest args)
           "Concatenates all args into one symbol."
           (intern (with-output-to-string (s)
                     (dolist (a args) (princ a s))))))
    (mapcar (lambda (slot)
              (concatenate 'list
                           (list slot)
                           (if (member :a line)
                               (list :accessor (mksym prefix slot)))
                           (if (member :r line)
                               (list :reader (mksym prefix slot)))
                           (if (member :w line)
                               (list :writer (mksym prefix slot)))
                           (if (member :i line)
                               (list :initarg (make-keyword slot)))
                           (aif (member :std line)
                                (if (eq (cadr it) :unbound)
                                    nil
                                    (list :initform (cadr it)))
                                (if *default-std*
                                    (list :initform nil)))
                           (if (or (member :@@ line)
                                   (member :static line))
                               (list :allocation :class))
                           (aif (member :doc line)
                                (list :documentation (cadr it)))
                           (aif (member :type line)
                                (list :type  (cadr it)))
                           unknown-keywords-and-values))
            (extract-slot-names line))))

(defmacro defclass/std (name direct-superclasses direct-slots &rest options)
  "Shortcut macro to the DEFCLASS macro. See README for syntax and usage."
  `(defclass ,name ,direct-superclasses
     ,@(mapcar
        (lambda (line)
          (let ((prefix (if (or (member :with-prefix line)
                                (member :with line)
                                *with-prefix*)
                            (concatenate 'string (string name) "-")
                            "")))
            (multiple-value-bind (split-kws-line unknown-keywords-and-values)
                (split-fusioned-keyword line)
              (check-for-repeated-keywords split-kws-line)
              (replace-keywords split-kws-line
                                prefix
                                unknown-keywords-and-values))))
        direct-slots)
     ,@options))

(defmacro class/std (name &body defaulted-slots)
  "Shortcut macro to the DEFCLASS/STD macro."
  `(defclass/std ,name ()
     ((,@defaulted-slots))))

(defmacro printing-unreadably (fields-list class-std-form
                               &key (type t) (identity t))
  "Automatically generates the unreadable printing boiler plate to print classes and its fields (from FIELDS-LIST)."
  (let ((g!stream (gensym "STREAM"))
        (name (cadr class-std-form)))
    `(progn ,class-std-form
            (defmethod print-object ((,name ,name) ,g!stream)
              (print-unreadable-object (,name ,g!stream
                                        :type ,type
                                        :identity ,identity)
                (format ,g!stream
                        ,(format nil "~{~a: ~~s~^,~^ ~}" fields-list)
                        ,@(mapcar (lambda (a1)
                                    `(,a1 ,name))
                                  fields-list)))))))
