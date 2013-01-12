;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;; Copyright 1991 BY GMD, Postfach 1240, D-5205 St. Augustin, FRG

;;; -----------------------------------------------------------------------------------
;;; description:    This is the Class Definition File.
;;; notes:          
;;; contact:        Juergen Kopp, Harry Bretthauer
;;; history: date:      author:             comments:
;;;          23.02.90   Harry Bretthauer    initial version
;;; -----------------------------------------------------------------------------------

(in-package "MCS")


;;; Generic Functions for CLASSES:

(defgeneric redefine-class (class metaclass initargs))
(defgeneric validate-superclass (class superclass last-p))
(defgeneric finalize-inheritance (class direct-superclasses initargs))
(defgeneric compute-class-precedence-list (class direct-superclasses))
(defgeneric compute-slots (class direct-superclasses direct-slots))
(defgeneric compute-slot-accessor-lambda (class effective-slots))
(defgeneric compute-initargs (class direct-superclasses direct-initargs))
(defgeneric direct-slot-definition-class (class textual-slot-definition))
(defgeneric effective-slot-definition-class (class slot-definitions))
(defgeneric compute-effective-slot-definition (class slot-name slot-definitions))

;;; -----------------------------------------------------------------------------------
;;; ENSURE-CLASS                                                    Bootstrap Function
;;; -----------------------------------------------------------------------------------

(setf (symbol-function 'ensure-class)
      #'(lambda (class-name metaclass-name initargs)
          (declare (optimize (speed 3) (safety 1)))
          (let ((class (find-class class-name)))
            (if class 
              (redefine-class class (find-class metaclass-name) initargs)
              (progn
                (setf class (apply #'make-instance metaclass-name initargs))
                (insert-class class-name class)
                class)))))

;;; -----------------------------------------------------------------------------------
;;; Methods for CLASSES
;;; -----------------------------------------------------------------------------------

(defmethod finalize-inheritance ((class class) direct-superclasses plist)
  (declare (ignore plist))
  (setf (%%class-precedence-list class)
        (compute-class-precedence-list class direct-superclasses)) 
  class)

(defmethod compute-class-precedence-list ((class class) direct-superclasses)
  (let ((result (list class)))
    (dolist (superclass direct-superclasses)
      (setf result (append result (%%class-precedence-list superclass))))
    (remove-duplicates result :test #'eq :from-end nil)))

;;; -----------------------------------------------------------------------------------
;;; Methods for DEFINED classes
;;; -----------------------------------------------------------------------------------

(defmethod redefine-class ((class class) metaclass initargs)
  "This method is only to be used when compiling and loading in one step." 
  (declare (ignore metaclass initargs))
  (error "Can't redefine ~S." class))

(defmethod initialize-instance ((class defined) &rest plist)
  (call-next-method)
  (let ((direct-superclasses (getf plist 'superclasses)))
    (finalize-class class (if direct-superclasses
                            direct-superclasses
                            (list (find-class 'object)))
                    plist)
    class))

(defmethod finalize-class ((class defined)  direct-superclasses plist)
  ;; create direct slot definitions:
  (setf (getf plist 'slots)
        (mapcar #'(lambda (slot-spec)
                    (apply #'make-instance 
                           (direct-slot-definition-class class slot-spec)
                           slot-spec))
                (getf plist 'slots)))
  ;; do the whole inheritance work:
  (finalize-inheritance class direct-superclasses plist))


(defmethod direct-slot-definition-class ((class defined) canonical-slot-spec)
  (declare (ignore class))
  (find-class (or (getf canonical-slot-spec 'slot-class) 'slot-definition)))

(defmethod finalize-inheritance ((class defined) direct-superclasses plist)
  ;; validate direct-superclasses:
  (let ((local-direct-superclasses direct-superclasses))
    (loop
      (if (null local-direct-superclasses) (return t))
      (validate-superclass class (pop local-direct-superclasses)
                           (if (null local-direct-superclasses) t))))
  (call-next-method)
  (setf (%%class-slots class)
        (compute-slots class direct-superclasses (getf plist 'slots)))
  (setf (%%class-initargs class)
        (compute-initargs class direct-superclasses (getf plist 'initargs)))
  class)

(defmethod compute-slots ((class defined) direct-superclasses direct-slots)
  ;; collect all slots ordered "most general first":
  (let ((inherited-and-direct-slots direct-slots))
    (dolist (superclass direct-superclasses)
      (setf inherited-and-direct-slots 
            (append (%%class-slots superclass) inherited-and-direct-slots)))

    ;; collect slots with the same name ordered "most specific first": 
    (let ((effective-slots nil) (all-slots ()))
      (dolist (slot inherited-and-direct-slots)
        (let ((name (slot-value slot 'name)))
          (if (getf all-slots name)
            (setf (getf all-slots name) (cons slot (getf all-slots name)))
            (setf all-slots (cons name (cons (list slot) all-slots))))))

      ;; compute the effective slots from many ordered "most general first":
      (doplist ((slot-name slots) all-slots)
        (push (compute-effective-slot-definition class slot-name slots)
              effective-slots))
      effective-slots)))

(defmethod compute-effective-slot-definition ((class defined) slot-name slots)
  (let ((initform '<unbound>)
        (initfunction nil)
        (type nil))
    (dolist (slot slots)
      (when (eq initform '<unbound>) 
        (setf initform (%%slot-initform slot))
        (setf initfunction (%%slot-initfunction slot)))
      (setf type (append (rest (slot-definition-type slot)) type)))
    (make-instance (effective-slot-definition-class class slots)
                   'name slot-name
                   'initform initform
                   'initfunction initfunction
                   'type `(and ,@(remove-duplicates type :test #'eq :from-end t)))))

(defmethod effective-slot-definition-class ((class defined) slots)
  (declare (ignore class))
  (class-of (first slots)))

(defmethod compute-initargs ((class defined) direct-superclasses initargs)
  (declare (ignore class))
  (dolist (superclass direct-superclasses)
    (setf initargs (append (%%class-initargs superclass) initargs)))
  (remove-duplicates initargs :test #'eq :from-end t))

;;; -----------------------------------------------------------------------------------
;;; Methods for SINGLE-INHERITED and MULTIPLE-INHERITED classes
;;; -----------------------------------------------------------------------------------

(defmethod validate-superclass ((subclass single-inherited) 
                           (superclass single-inherited)
                           last-p)
  (if last-p
    t
    (error "~S can be inherited only rigthmost by ~S." superclass subclass)))

(defmethod validate-superclass ((subclass single-inherited)
                           (superclass multiple-inherited)
                           last-p)
  (declare (ignore subclass superclass last-p))
  t)

(defmethod validate-superclass ((subclass multiple-inherited)
                                (superclass multiple-inherited)
                                last-p)
  (declare (ignore subclass superclass last-p))
  t)

(defmethod validate-superclass ((subclass multiple-inherited)
                                (superclass single-inherited)
                                last-p)
  (declare (ignore subclass))
  (and (eq superclass (find-class 'object))
       last-p))

;;; -----------------------------------------------------------------------------------
;;; Methods for INSTANTIABLE classes
;;; -----------------------------------------------------------------------------------

(defmethod finalize-inheritance ((class instantiable) direct-superclasses plist)
  (declare (ignore direct-superclasses plist))
  (call-next-method)
  (setf (%%class-slot-accessor class) #'general-slot-position)
  (setf (%%class-slots-number class) (list-length (%%class-slots class)))
  (setf (%%class-wrapper class) (list class))
  class)

(defmethod compute-slot-accessor-lambda ((class instantiable) effective-slots)
  ;; the result is a function which is used in slot-value calls
  ;; applied to instances of class
  (let ((pos (%%class-slots-number class))
        (case-entries (list '(t nil))))
    (dolist (slot (reverse effective-slots))
      (push (list (%%slot-name slot) (decf pos)) case-entries))
    `(lambda (object slot-name)
       (declare (optimize (speed 3) (safety 0))
                (ignore object))
       (case slot-name
         ,@case-entries))))

;;; -----------------------------------------------------------------------------------
;;; Methods for ABSTRACT classes
;;; -----------------------------------------------------------------------------------

(defmethod validate-superclass ((subclass abstract) 
                           (superclass instantiable)
                           last-p)
  (declare (ignore last-p))
  (error "Abstract class: ~S can't specialize an instantiable class: ~S."
         subclass superclass))

;;; -----------------------------------------------------------------------------------
;;; Methods for BUILT-IN classes
;;; -----------------------------------------------------------------------------------

(defmethod redefine-class ((class built-in) metaclass initargs)
  (declare (ignore initargs metaclass))  
  (error "Can't redefine ~S." class))

;;; -----------------------------------------------------------------------------------
;;; Predicates                                                               Functions
;;; -----------------------------------------------------------------------------------

(defun abstract-p (class)
  "Tests if class is an abstract class."
  (%typep class 'abstract))

(defun mixin-p (class)
  "Tests if class is a mixin-class (noninstantiable refinement) class."
  (%typep class 'mixin-class))

;;; -----------------------------------------------------------------------------------
;;; Abstract-Base-Class Constructor                                              Macro
;;; -----------------------------------------------------------------------------------

(defmacro defabstract (class-name direct-superclasses direct-slots &rest options)
  (let ((metaclass-name  (second (assoc ':metaclass options))))
  `(progn
     ,(if metaclass-name
     `(unless (obj-subtypep ',metaclass-name 'abstract)
       (error "Class ~S must be a subclass of mixin-class named abstract." 
              ',metaclass-name)))
     (defclass ,class-name ,direct-superclasses
     ,direct-slots
     ,@(if metaclass-name
         options
         (cons '(:metaclass standard-abstract-base-class) options))))))

;;; -----------------------------------------------------------------------------------
;;; Mixin-Class Constructor                                                      Macro
;;; -----------------------------------------------------------------------------------

(defmacro defmixin (class-name direct-superclasses direct-slots &rest options)
  (let ((metaclass-name  (second (assoc ':metaclass options))))
  `(progn
     ,(if metaclass-name
        `(unless (obj-subtypep ',metaclass-name 'mixin-class)
           (error "Class ~S must be a subclass of class named Mixin-Class." 
                  ',metaclass-name)))
     (defclass ,class-name ,direct-superclasses
     ,direct-slots
     ,@(if metaclass-name
         options
         (cons '(:metaclass standard-mixin-class) options))))))

;;; -----------------------------------------------------------------------------------
;;; Constructor Definition                                                       Macro
;;; -----------------------------------------------------------------------------------

(defmacro defconstructor (fn-name lambda-list &rest body)
  "note: In the body call the function make-instance or macro make-instance-fast!"
  `(defun ,fn-name  ,lambda-list ,@body) )



;;; eof

