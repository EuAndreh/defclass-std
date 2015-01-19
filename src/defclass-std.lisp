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
           class/std)
  (:documentation "Main (and only) project package."))
(in-package defclass-std)

(defun extract-slot-names (line)
  "Finds all slot names in the `line'."
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

(defun find-fusioned-keyword-options (line)
  "Should return a singleton list with the only fusioned element. Throws an error otherwise."
  (let* ((maybe-unknown-keywords (set-difference (remove-if-not #'keywordp line)
                                                 *options*))
         (fusioned-keywords (intersection *fusioned-keyword-combinations*
                                          maybe-unknown-keywords))
         (unknown-keywords (set-difference maybe-unknown-keywords
                                           fusioned-keywords)))
    (cond ((null fusioned-keywords)
           (unless (or (member :a line)
                       (member :r line)
                       (member :w line)
                       (member :i line))
             (values :ai unknown-keywords))) ;; defaults to `:ai'
          ((= 1 (length fusioned-keywords)) (values (car fusioned-keywords)
                                                    unknown-keywords))
          (t (error "Too many fusioned keyword options in DEFCLASS/STD: ~s. Invalid keyword option."
                    fusioned-keywords)))))

(defun split-fusioned-keyword (line)
  "Splits the fusioned keyword option, if present."
  (multiple-value-bind (fusioned-keywords-key unknown-keywords)
      (find-fusioned-keyword-options line)
    (values (append (remove fusioned-keywords-key
                            (set-difference line unknown-keywords))
              (when fusioned-keywords-key
                (mapcar #'make-keyword
                        (coerce (symbol-name fusioned-keywords-key)
                                'list))))
            unknown-keywords)))

(defun check-for-repeated-keywords (line)
  "Verifies if keyword options were repeated. Mainly useful for avoiding things like (`:a' `:ai') together, or (`:r' `:w') instead of (`:a')."
  (cond ((and (member :w line)
              (member :r line))
         (error
          "Use :A (accessor) instead of :W (writer) and :R (reader): ~s"
          line))
        ((and (member :w line)
              (member :a line))
         (error
          ":W (writer) and :A (accessor) shouldn't be together: ~s."
          line))
        ((and (member :r line)
              (member :a line))
         (error
          ":R (reader) and :A (accessor) shouldn't be together: ~s."
          line))))

(defun replace-keywords (line prefix unknown-keywords)
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
                           (sort unknown-keywords
                                 (lambda (k1 k2)
                                   (string< (string k1)
                                            (string k2))))))
            (extract-slot-names line))))

(defmacro defclass/std (name direct-superclasses direct-slots &rest options)
  `(defclass ,name ,direct-superclasses
     ,@(mapcar
        (lambda (line)
          (let ((prefix (if (or (member :with-prefix line)
                                (member :with line)
                                *with-prefix*)
                            (concatenate 'string (string name) "-")
                            "")))
            (multiple-value-bind (split-kws-line unknown-keywords)
                (split-fusioned-keyword line)
              (check-for-repeated-keywords split-kws-line)
              (replace-keywords split-kws-line prefix unknown-keywords))))
        direct-slots)
     ,@options))

(defmacro class/std (name &body defaulted-slots)
  `(defclass/std ,name ()
     ((,@defaulted-slots))))
