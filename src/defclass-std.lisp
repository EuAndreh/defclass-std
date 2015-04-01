(in-package cl-user)
(defpackage defclass-std
  (:use cl)
  (:import-from alexandria
                make-keyword
                flatten
                symbolicate)
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

(defparameter *fusioned-keyword-combinations*
  '(:ai :ar :aw :ia :ir :iw :ra :ri :rw :wa :wi :wr)
  "All possible combinations of :a, :i, :r and :w.")

(defparameter *default-added-keywords* '(:a :i)
  "Default abbreviated keywords added when none is found.")

(defparameter *fusionable-keywords* '(:a :i :w :r)
  "All abbreviated keywords that can be fusioned.")

(defparameter *standalone-keywords* '(:a :i :w :r :static :with :with-prefix :@@))

(defparameter *paired-keywords* '(:std :unbound :doc :type))

(defparameter *default-std* t
  "Special var that changes the behaviour of the DEFCLASS/STD macro. If true, adds a :initform nil by default to every field, when unespecified. If false, adds nothing.")

(defparameter *with-prefix* nil
  "Special var that changes the behaviour of the DEFCLASS/STD macro. If tru, adds the class name as a prefix to every accessor/reader/writer function. If false, without the :with/:with-prefix slot option, adds nothing.")

(defun remove-all (els list)
  "Applies remove recursively. Serves as a version of apeWEOFJIAOPWEIF  that keeps the original sequence in the same order."
  (if els
      (remove-all (cdr els) (remove (car els) list))
      list))

(defun extract-slot-names (line)
  "Finds all slot names in the LINE."
  (if (and line
           (not (keywordp (car line))))
      (cons (car line)
            (extract-slot-names (cdr line)))))

(defun extract-unkown-keywords (line)
  "Finds pairs of unknown-keywords (and optional values) in LINE."
  (if line
      (let ((slot (car line)))
        (cond ((or (not (keywordp slot))
                   (member slot *standalone-keywords*))
               (extract-unkown-keywords (cdr line)))
              ((member slot *paired-keywords*)
               (extract-unkown-keywords (cddr line)))
              ((or (member (second line) (append *standalone-keywords*
                                                 *paired-keywords*))
                   (null (cdr line)))
               (cons (car line)
                     (extract-unkown-keywords (cdr line))))
              (t (append (subseq line 0 2)
                         (extract-unkown-keywords (cddr line))))))))

(defun split-fusioned-keywords (line)
  "Splits the fusioned keyword option, if present."
  (aif (intersection line *fusioned-keyword-combinations*)
       (append (remove-all it line)
               (mapcar #'make-keyword
                       (flatten (mapcar (lambda (fus-kw)
                                          (coerce (string fus-kw)
                                                  'list))
                                        it))))
       (if (intersection line *fusionable-keywords*)
           line
           (append line *default-added-keywords*))))

(defun check-for-repeated-keywords (line)
  "Verifies if keyword options were repeated. Mainly useful for avoiding things like (:A :AI) together, or (:R :W) instead of (:A)."
  (cond ((and (member :w line)
              (member :r line))
         (error "Use :A (accessor) instead of :W (writer) and :R (reader) in: ~s"
                line))
        ((and (member :w line)
              (member :a line))
         (error ":W (writer) and :A (accessor) shouldn't be together in: ~s."
                line))
        ((and (member :r line)
              (member :a line))
         (error ":R (reader) and :A (accessor) shouldn't be together in: ~s."
                line))))

(defun replace-keywords (line prefix)
  "Receives a list of slots with keywords and returns a list of lists. Each sublist is a single slot, with all the options appended at the end."
  (mapcar (lambda (slot)
            (concatenate 'list
                         (list slot)
                         (if (member :a line)
                             (list :accessor (symbolicate prefix slot)))
                         (if (member :r line)
                             (list :reader (symbolicate prefix slot)))
                         (if (member :w line)
                             (list :writer (symbolicate prefix slot)))
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
                         (extract-unkown-keywords line)))
          (extract-slot-names line)))

(defmacro defclass/std (name direct-superclasses direct-slots &rest options)
  "Shortcut macro to the DEFCLASS macro. See README for syntax and usage."
  `(defclass ,name ,direct-superclasses
     ,@(mapcar
        (lambda (line)
          (let ((prefix (if (or (member :with-prefix line)
                                (member :with line)
                                *with-prefix*)
                            (concatenate 'string (string name) "-")
                            ""))
                (split-kws-line (split-fusioned-keywords line)))
            (check-for-repeated-keywords split-kws-line)
            (replace-keywords split-kws-line
                              prefix)))
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
