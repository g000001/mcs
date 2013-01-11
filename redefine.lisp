;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;; Copyright 1991 BY GMD, Postfach 1240, D-5205 St. Augustin, FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the Class Redefinition File.
;;; notes:          
;;; contact:        Juergen Kopp, Harry Bretthauer
;;; history:
;;;          date:      author:             comments:
;;;          23.02.90   Harry Bretthauer    initial version
;;; -----------------------------------------------------------------------------------

(in-package "MCS")

;;; -----------------------------------------------------------------------------------
;;;  Define class OBSOLETE-OBJECT, its methods and functions
;;; -----------------------------------------------------------------------------------

;(defclass obsolete-object () ())

(progn
  (ensure-class 'obsolete-object
                'base-class
                (list 'name 'obsolete-object 'superclasses ())))

(setf (%%class-precedence-list (find-class 'obsolete-object))
      (list (find-class 'obsolete-object)))

;;; Accessor Operations on wrappers:

(defmacro wrapper-class (wrapper) `(cadr ,wrapper))
(defmacro wrapper-slots (wrapper) `(caddr ,wrapper))

(defmethod slot-missing ((a_class instantiable) (object obsolete-object) 
                         slot-name operation &rest options)
  (declare (ignore a_class))
  ;; update object:
  (update-object-for-redefined-class object)
  ;; try operation again:
  (case operation
    (slot-value (slot-value object slot-name))
    (setf (setf (slot-value object slot-name) (car options)))
    (slot-makunbound (slot-makunbound object slot-name))
    (slot-boundp (slot-boundp object slot-name))
    (t (call-next-method))))

(defmethod no-applicable-methods ((gfn generic-function) &rest arguments)
  (let ((obsolete-objects nil))
    (dolist (arg arguments)
      (when (obsolete-p arg) 
        (update-object-for-redefined-class arg)
        (setf obsolete-objects 't)))
    (if obsolete-objects
      (dynamic-lookup gfn arguments)
      (error "No applicable methods in ~S for ~S." gfn arguments))))

(defun update-object-for-redefined-class (object)
  (let ((wrapper (mcs%-isit object)))
    (let ((class (wrapper-class wrapper)))
      (unless (finalizedp class)
        (finalize-inheritance class 
                              (class-direct-superclasses class)
                              (class-initial-plist class)))
      (if (%typep class 'instantiable-redefinable)
        (change-structure-and-fill-slots object
                                         (wrapper-slots wrapper)
                                         (%%class-slots class)
                                         class)
        (error "Can't update obsolete instance to new definition of ~S." class))
      object)))

(defun obsolete-p (obj)
  (declare (optimize (speed 3) (safety 1)))
  (and (%object-p obj)
       (mcs%typep obj (find-class 'obsolete-object))))


;;; -----------------------------------------------------------------------------------
;;;  Additional Generic Functions and Methods for OBJECTS and CLASSES:
;;; -----------------------------------------------------------------------------------

(defgeneric finalizedp (class))
(defgeneric change-class (obj new-class))
(defgeneric change-class-using-class (old-class new-class obj))

(defmethod finalizedp ((cl class)) (declare (ignore cl)) 't)

(defmethod reinitialize-instance ((class defined) &rest plist)
  (call-next-method)
  (let ((direct-superclasses (getf plist 'superclasses)))
    (finalize-class class (if direct-superclasses
                            direct-superclasses
                            (list (find-class 'object)))
                    plist)
    class))

(defmethod change-class ((obj object) new-class)
  "Changes the class of obj to new-class. 
   Old slots which are also new slots keep their values.
   New slots are initialized according to the initforms if unbound."
  (change-class-using-class (mcs%class-of obj) 
                            (if (symbolp new-class)
                              (find-class new-class 'signal-error)
                              new-class)
                            obj))

;;; -----------------------------------------------------------------------------------
;;;  Define class REDEFINABLE and its methods
;;; -----------------------------------------------------------------------------------

(defclass redefinable (object)
  ((direct-superclasses :initform (list (find-class 'object))
                        :initarg direct-superclasses
                        :accessor class-direct-superclasses)
   (direct-subclasses :initform ()
                        :accessor class-direct-subclasses)
   (direct-methods :initform () 
                   :accessor class-direct-methods)
   (plist :initform ()
                 :accessor class-initial-plist)
   (finalized-flag :initform nil
                   :accessor finalizedp))
  (:metaclass mixin-class))

(defmethod finalize-class ((class redefinable) direct-superclasses plist)
  ;; store direct information:
  (call-next-method)
  (mapc #'(lambda (superclass)
              (add-direct-subclass superclass class))
          direct-superclasses)
    (setf (class-direct-superclasses class) direct-superclasses)
    (setf (class-initial-plist class) plist))


(defmethod validate-superclass ((subclass redefinable)
                                (superclass redefinable)
                                last-p)
  (declare (ignore last-p))
  (call-next-method)
  (when (member subclass (class-precedence-list superclass) :test #'eq)
    (error "~S can't be a subclass of ~S. This would lead to a circular inheritance graph."
           subclass superclass)))

(defmethod add-direct-subclass ((class redefinable) (subclass redefinable))
  (let ((subclasses (class-direct-subclasses class)))
    (unless (member subclass subclasses :test #'eq)
      (setf (class-direct-subclasses class)
            (cons subclass subclasses)))))

(defmethod add-direct-subclass ((class class) (subclass redefinable))
  (declare (ignore class subclass))
  ())

(defmethod remove-direct-subclass ((class redefinable) (subclass redefinable))
  (let ((subclasses (class-direct-subclasses class)))
    (setf (class-direct-subclasses class)
            (delete subclass subclasses :test #'eq))))

(defmethod remove-direct-subclass ((class class) (subclass redefinable))
  (declare (ignore class subclass))
  ())

(defmethod finalize-inheritance ((class redefinable) direct-superclasses plist)
  (declare (ignore plist))
  ;; check if direct superclasses are finalized:
  (mapc #'(lambda (superclass)
              (unless (finalizedp superclass)
                (finalize-inheritance
                 superclass (class-direct-superclasses superclass)
                 (class-initial-plist superclass))))
          direct-superclasses)
  (call-next-method)
  (setf (finalizedp class) 't)
  class)

(defmethod make-reader ((class redefinable) (slot slot-definition) reader-name)
  ;; Since slots can change their position, the same method must be applied 
  ;; as for multiple-inherited classes!
  ;; An alternative solution would be to update the reader method when
  ;; necessary which requires more space.
  (let ((slot-name (%%slot-name slot)))
    (let ((gfn (ensure-gfn reader-name '(object)
                           (find-method-combination 'standard)
                         'class (find-class 'reader)
                         'method-class (find-class 'reader-method)))
          (method 
           (make-instance-fast 
            'reader-method
            'name reader-name 
            'lambda-list '(object) 
            'specializers (list class)
            'qualifiers ()
            'function #'(lambda (%next-fns object)
                          (declare (ignore %next-fns) (optimize (speed 3) (safety 0)))
                          (mcs%local-slot-indexed object slot-name
                                                  (%slot-location-of object slot-name))))))
      (add-method gfn method)
      #'(lambda (object)
          (declare (optimize (speed 3) (safety 1)))
          (let ((eff-methods 
                 (funcall (%%gfn-discriminating-function gfn)
                          gfn
                          object)))
            (declare (optimize (speed 3) (safety 0)))
            (funcall (car eff-methods)
                     eff-methods
                     object))))))

(defmethod make-writer ((class redefinable) (slot slot-definition) writer-name)
  ;; Since slots can change their position, the same method must be applied 
  ;; as for multiple-inherited classes!
  ;; An alternative solution would be to update the writer method when
  ;; necessary which requires more space.
  (let ((slot-name (%%slot-name slot))
        (gfn (ensure-gfn writer-name '(value object) 
                         (find-method-combination 'standard)
                         'class (find-class 'writer)
                         'method-class (find-class 'writer-method))))
    (let ((method 
           (make-instance-fast
            'writer-method
            'name (if (symbolp writer-name)
                    writer-name
                    (make-setf-name writer-name))
            'lambda-list '(value object) 
            'specializers (list (find-class 't) class)
            'qualifiers ()
            'function #'(lambda (%next-fns value object)
                         (declare (ignore %next-fns) (optimize (speed 3) (safety 0)))
                         (setf (mcs%local-slot-indexed-low 
                                object 
                                (%slot-location-of object slot-name))
                               value)))))
      (add-method gfn method)
      #'(lambda (value object)
          (declare (optimize (speed 3) (safety 1)))
          (let ((eff-methods 
                 (funcall (%%gfn-discriminating-function gfn)
                          gfn
                          value object)))
            (declare (optimize (speed 3) (safety 0)))
            (funcall (car eff-methods)
                     eff-methods
                     value object))))))

(defmethod redefine-class ((class redefinable) metaclass initargs)
  (when *warn-if-redefine-class*
    (warn "Redefining class ~S. 
           Subclasses and instances will be updated. 
           Obsolete direct reader and writer methods will be removed.
           All other direct methods remain applicable."
          class))
  ;; remove old direct reader and writer methods:
  (dolist (method (class-direct-methods class))
    (when (or (mcs%typep method (find-class 'reader-method))
             (mcs%typep method (find-class 'writer-method)))
      (remove-method (find-gfn (%%method-name method)) method)))
  ;; update direct subclasses list of direct superclasses:
  (mapc #'(lambda (superclass)
            (remove-direct-subclass superclass class))
        (class-direct-superclasses class))
  ;; make class and subclasses obsolete:
  (make-class-obsolete class)
  (apply #'reinitialize-instance
         (if (eq (mcs%class-of class) metaclass)
           class
           (change-class class metaclass))
         initargs))

(defun make-class-obsolete (class)
  ;; There is nothing to do if class is not finalized, since it implies
  ;; that all subclasses are not finalized too.
  (when (finalizedp class)
    ;; make subclasses of CLASS obsolete:
    (mapc #'make-class-obsolete (class-direct-subclasses class))
    ;; make CLASS not finalized:
    (setf (finalizedp class) ())
    ;; make instances of CLASS obsolete:
    (when (%typep class 'instantiable)
      ;; let old class wrapper refer to obsolete-object:
      (let ((wrapper (%%class-wrapper class)))
        (setf (cdr wrapper) (list class (%%class-slots class)))
        (setf (car wrapper) (find-class 'obsolete-object))
        (setf (%%class-wrapper class) nil)))))

(defmethod  remove-direct-method ((class redefinable) method)
  (setf (class-direct-methods class)
        (mcs-remq method (class-direct-methods class))))


;;; -----------------------------------------------------------------------------------
;;; Mixin class instantiable-redefinable:
;;; -----------------------------------------------------------------------------------

(defclass instantiable-redefinable (redefinable)
  ((effective-methods :initform nil
                      :accessor class-effective-methods))
  (:metaclass mixin-class))

(defmethod allocate-instance ((class instantiable-redefinable) initargs)
  (declare (ignore initargs))
  (let ((wrapper (%%class-wrapper class)))
    (if wrapper
      ()
      (finalize-inheritance class 
                            (class-direct-superclasses class)
                            (class-initial-plist class)))
    (%make-empty-obj (%%class-wrapper class) ; wrapper *** na so was ***
                     'error ;(%%class-slot-accessor class)
                     (%%class-slots-number class))))


(defmethod change-class-using-class ((old-class instantiable-redefinable)
                                     (new-class instantiable-redefinable) obj)
  (change-structure-and-fill-slots obj
                                   (%%class-slots old-class)
                                   (%%class-slots new-class)
                                   new-class))

(defun change-structure-and-fill-slots (obj old-slots new-slots new-class)
  (let ((new-obj (allocate-instance new-class ())))
    ;; change obj destructively:
    (let ((old-values (mapcan #'(lambda (slot value)
                                  (list (%%slot-name slot) value))
                              old-slots (coerce (mcs%-slots obj) 'list))))
      (setf (mcs%-isit obj)(mcs%-isit new-obj))
      (setf (mcs%-slots obj)(mcs%-slots new-obj))
      (fill-slots obj new-slots old-values)
      obj)))

(defmethod redefine-class ((class instantiable-redefinable) metaclass initargs)
  (declare (ignore metaclass initargs))
  ;; update cache tables of generic functions:
  (dolist (entry (class-effective-methods class))
    (let ((gfn (find-gfn (entry-name entry))))
      (remove-invalid-combined-methods gfn (relevant-method-specializers 
                                            (%%gfn-signature gfn)
                                            (entry-specializers entry)))))
  (setf (class-effective-methods class) nil)
  (call-next-method))

;;; -----------------------------------------------------------------------------------
;;;  Define redefinable base-classes:
;;; -----------------------------------------------------------------------------------

;(defclass dynamic-base-class (instantiable-redefinable base-class) ())

;;; Make self-instantiated class named REDEFINABLE-BASE-CLASS:

(defclass tmp-redefinable-base-class (instantiable-redefinable base-class) ())

(defclass redefinable-base-class (instantiable-redefinable base-class)
  ()
  (:metaclass tmp-redefinable-base-class))

(setf (mcs%-isit (find-class 'redefinable-base-class))
      (class-wrapper (find-class 'redefinable-base-class)))

(remove-class 'tmp-redefinable-base-class)

;;; Make instances of REDEFINABLE-BASE-CLASS:

(defclass redefinable-abstract-base-class (redefinable abstract-base-class)
  ()
  (:metaclass redefinable-base-class))

(defclass redefinable-mixin-class (redefinable mixin-class)
  ()
  (:metaclass redefinable-base-class))

;;; -----------------------------------------------------------------------------------
;;; Reading and Setting the mode concerning redefinition:
;;; -----------------------------------------------------------------------------------

(let ((mode nil))
  (defun redefine-mode (&rest value)
    (if value
      (let ((new-value (car value)))
        (if (eq new-value 't)
          (unless mode
            (setf (find-class 'standard-base-class)
                  (find-class 'redefinable-base-class))
            (setf (find-class 'standard-abstract-base-class)
                  (find-class 'redefinable-abstract-base-class))
            (setf (find-class 'standard-mixin-class)
                  (find-class 'redefinable-mixin-class))
            (setf mode 't))
          (when mode
            (setf (find-class 'standard-base-class)
                  (find-class 'base-class))
            (setf (find-class 'standard-abstract-base-class)
                  (find-class 'abstract-base-class))
            (setf (find-class 'standard-mixin-class) 
                  (find-class 'mixin-class))
            (setf mode nil))))
      mode)))

#|
(redefine-mode nil)
(defclass c0 () ((s0 :initform 11 :accessor s0) (s2 :initform 16) ))
(setf i0 (make-instance 'c0))
(remove-class 'c7)
(defclass c7 () ((s0 :initform 11 :accessor s0) (s2 :initform 16) ))
(obj-describe (find-class 'c7))
(setf i7 (make-instance 'c7))
(obj-describe i7) (s0 i7)
(change-class i0 'c7)
(remove-class 'c8)
(defclass c8 (c7) ((s0 :initform 3) s1))
(obj-describe (find-class 'c8))
(setf i8 (make-instance 'c8))
(obj-describe i8) (s0 i8)  (mcs%-isit i8)
(change-class i8 'c8)

(defclass c19 () ((s0 :initform 11 :accessor s0) s2))
(setf i9 (make-instance 'c9))
(obj-describe i9)

(untrace reinitialize-instance finalize-inheritance redefine-class)

|#

;;; Utilities:

(defmethod class-direct-protocol ((class redefinable))
  (mapcar #'method-name (class-direct-methods class)))




;;; eof

