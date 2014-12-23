(defpackage defclass-std
  (:use cl)
  (:import-from anaphora
                aif
                it)
  (:export defclass/std
           *default-std*
           *with-prefix*
           class/std)
  (:documentation "Main project package."))
(in-package defclass-std)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (qtlc:utilize-symbols '(:explode :make-keyword :symbolicate)))

(defun extract-slot-names (line)
  "Finds all slot names in the `line'."
  (if (and line
           (not (keywordp (car line))))
      (cons (car line)
            (extract-slot-names (cdr line)))))

(defvar *options* '(:a :r :w :i :static :with :with-prefix :@@ :static
                    :std :unbound :doc :type)
  "All available keyword options.")

(defvar *default-std* t
  "Special var that changes the behaviour of the DEFCLASS/STD macro. If true, adds a :initform nil by default to every field, when unespecified. If false, adds nothing.")

(defvar *with-prefix* nil
  "Special var that changes the behaviour of the DEFCLASS/STD macro. If tru, adds the class name as a prefix to every accessor/reader/writer function. If false, without the :with slot option, adds nothing.")

(defun find-fusioned-keyword-options (line)
  "Should return a singleton list with the only fusioned element. Throws an error otherwise."
  (let ((singleton-list (set-difference (remove-if-not #'keywordp line)
                                        *options*)))
    (cond ((null singleton-list)
           (if (or (member :a line)
                   (member :r line)
                   (member :w line)
                   (member :i line))
               nil
               :ai)) ;; defaults to `:ai'.
          ((= 1 (length singleton-list)) (car singleton-list))
          (t (error "Too many fusioned keyword options in DEFCLASS/STD: ~s. Invalid keyword option."
               singleton-list)))))

(defun split-fusioned-keyword (line)
  "Splits the fusioned keyword option, if present."
  (let ((fusioned-keywords (find-fusioned-keyword-options line)))
    (append (remove fusioned-keywords line)
            (if fusioned-keywords
                (mapcar #'qtl:make-keyword
                        (qtl:explode (symbol-name fusioned-keywords)))))))

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

(defun replace-keywords (line prefix)
  "Receives a list of slots with keywords and returns a list of lists. Each sublist is a single slot, with all the options appended at the end."
  (mapcar (lambda (slot)
            (concatenate 'list
                         (list slot)
                         (if (member :a line)
                             (list :accessor (qtl:symbolicate prefix slot)))
                         (if (member :r line)
                             (list :reader (qtl:symbolicate prefix slot)))
                         (if (member :w line)
                             (list :writer (qtl:symbolicate prefix slot)))
                         (if (member :i line)
                             (list :initarg (qtl:make-keyword slot)))
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
                              (list :type  (cadr it)))))
          (extract-slot-names line)))

(defmacro defclass/std (name direct-superclasses direct-slots &rest options)
  `(defclass ,name ,direct-superclasses
     ,(apply #'concatenate
             'list
             (mapcar
              (lambda (line)
                (let ((prefix (if (or (member :with-prefix line)
                                      (member :with line)
                                      *with-prefix*)
                                  (concatenate 'string (string name) "-")
                                  ""))
                      (split-kws-line (split-fusioned-keyword line)))
                  (check-for-repeated-keywords split-kws-line)
                  (replace-keywords split-kws-line prefix)))
              direct-slots))
     ,@options))

(defmacro class/std (name &body defaulted-slots)
  `(defclass/std ,name ()
     ((,@defaulted-slots))))
