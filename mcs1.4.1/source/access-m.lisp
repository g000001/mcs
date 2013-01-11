;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;;           Copyright  1990    BY
;;;           G M D  
;;;           Postfach 1240
;;;           D-5205 St. Augustin
;;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is Accessors File of Metaobjects.
;;;
;;; notes:          
;;;
;;; contact:        Juergen Kopp, Harry Bretthauer
;;;
;;; history:
;;;          date:      author:             comments:
;;;          23.02.90   Harry Bretthauer    initial version
;;; -----------------------------------------------------------------------------------

(in-package "MCS")

;(export '(method-generic-function class-direct-subclasses class-precedence-list
;          class-direct-superclasses))

;;; CLOS: -direct-superclasses, -prototype, -default-initargs, 
;;;       -direct-default-initargs
;;; CLOS: -argument-precedence-order, -declarations, -initial-methods
;;; MCS-CLOS-compatible: description = definition
;;; CLOS: -order, -options, -identity-with-one-argument


;;; Bootstrap completed:
(setf *during-bootstrap* nil)

(defgeneric make-reader (class slot-definition reader-name))
(defgeneric make-writer (class slot-definition writer-name))
(defgeneric class-direct-subclasses (class))
(defgeneric class-initargs (class))
(defgeneric class-slots (class))
(defgeneric class-prototype (class))
(defgeneric method-generic-function (method))

;;; -----------------------------------------------------------------------------------
;;; MAKE-READER                                              Standard Generic Function
;;; -----------------------------------------------------------------------------------

(defmethod make-reader ((class single-inherited) (slot slot-definition) reader-name)
  (let ((gfn (ensure-gfn reader-name '(object) 
                         (find-method-combination 'standard)
                         'class (find-class 'reader)
                         'method-class (find-class 'reader-method)))
        (pos (%find-slot-position class (%%slot-name slot))))
    (add-method gfn
                (make-instance-fast 
                 'reader-method
                 'name          reader-name 
                 'lambda-list   '(object) 
                 'specializers  (list class)
                 'qualifiers    ()
                 'function      (get-accessor-fn 'reader pos)))
    #'(lambda (object)
          (declare (optimize (speed 3) (safety 1)))
          (let ((eff-methods 
                 (funcall (%%gfn-discriminating-function gfn)
                          gfn
                          object)))
            (declare (optimize (speed 3) (safety 0)))
            (funcall (car eff-methods)
                     eff-methods
                     object)))))


(defmethod make-reader ((class multiple-inherited) (slot slot-definition) reader-name)
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


;;; -----------------------------------------------------------------------------------
;;; MAKE-WRITER                                              Standard Generic Function
;;; -----------------------------------------------------------------------------------

(defmethod make-writer ((class single-inherited) (slot slot-definition) writer-name)
  (let ((gfn (ensure-gfn writer-name '(value object) 
                         (find-method-combination 'standard)
                         'class (find-class 'writer)
                         'method-class (find-class 'writer-method)))
        (pos (%find-slot-position class (%%slot-name slot))))
    (add-method gfn
                (make-instance-fast 
                 'writer-method    
                 'name (if (symbolp writer-name)
                         writer-name
                         (make-setf-name writer-name))
                 'lambda-list '(value object) 
                 'specializers (list (find-class 't) class)
                 'qualifiers ()
                 'function (get-accessor-fn 'writer pos)))
    #'(lambda (value object)
        (declare (optimize (speed 3) (safety 1)))
        (let ((eff-methods 
               (funcall (%%gfn-discriminating-function gfn)
                        gfn
                        value object)))
          (declare (optimize (speed 3) (safety 0)))
          (funcall (car eff-methods)
                   eff-methods
                   value object)))))


(defmethod make-writer ((class multiple-inherited) (slot slot-definition) writer-name)
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


;;; -----------------------------------------------------------------------------------
;;; DEFREADER                                                                    Macro
;;; -----------------------------------------------------------------------------------

(defmacro defreader (reader-name class-name slot-name)
  `(define-function ,reader-name
     (let ((class (find-class ',class-name)))
       (make-reader class (%find-slot class ',slot-name) ',reader-name))))

;;; -----------------------------------------------------------------------------------
;;; DEFWRITER                                                                    Macro
;;; -----------------------------------------------------------------------------------

(defmacro defwriter (writer-name class-name slot-name)
  (let ((global-gfn-name writer-name)
        (defsetf-form ()))
    (when (setf-form-p writer-name)
      (setf global-gfn-name (make-setf-name writer-name))
      (setf defsetf-form (%compute-defsetf writer-name '(value object))))
    `(progn
       ,defsetf-form
       (define-function ,global-gfn-name
         (let ((class (find-class ',class-name)))
           (make-writer class 
                        (%find-slot class ',slot-name) 
                        ',global-gfn-name))))))


;;; -----------------------------------------------------------------------------------
;;; Class Reader/Writer
;;; -----------------------------------------------------------------------------------

(defreader class-name class name)
(defreader class-precedence-list class superclasses)
(defreader class-slots class slots)
(defreader class-initargs class initargs)



;;; -----------------------------------------------------------------------------------
;;; Instantiable (Class) Reader/Writer
;;; -----------------------------------------------------------------------------------

(defreader class-slot-accessor instantiable slot-accessor)

(defmethod class-prototype ((class instantiable))
  (let ((prototype (slot-value class 'prototype)))
    (if prototype
      prototype
      (setf (slot-value class 'prototype) (make-instance class)))))

(defreader class-slots-number instantiable slots-number)
(defreader class-wrapper instantiable wrapper)


;;; -----------------------------------------------------------------------------------
;;; Slot Description Reader/Writer
;;; -----------------------------------------------------------------------------------

(defreader slot-definition-name slot-definition name)
(defreader slot-definition-initform slot-definition initform)
(defreader slot-definition-initfunction slot-definition initfunction)
(defreader slot-definition-type slot-definition type)

;;; -----------------------------------------------------------------------------------
;;; Standard Method Reader/Writer
;;; -----------------------------------------------------------------------------------

(defreader method-name method name)
(defreader method-lambda-list method lambda-list)
(defreader method-qualifiers method qualifiers)
(defreader method-specializers method specializers)
(defreader method-function method function)

(defwriter (setf method-function) method function)

(defmethod method-generic-function ((method method))
  (find-gfn (%%method-name method)))

;;; -----------------------------------------------------------------------------------
;;; Generic Function Reader/Writer
;;; -----------------------------------------------------------------------------------

(defreader generic-function-name generic-function name)
(defreader generic-function-lambda-list generic-function lambda-list)
(defreader generic-function-methods generic-function methods)
(defreader generic-function-discriminating-function generic-function 
  discriminating-function)
(defreader generic-function-method-class generic-function method-class)
(defreader generic-function-method-combination generic-function method-combination)

(defreader generic-function-combined-methods generic-function combined-methods)
(defreader generic-function-signature generic-function signature)


;;; -----------------------------------------------------------------------------------
;;; Method Combination Reader/Writer
;;; -----------------------------------------------------------------------------------

(defreader method-combination-name method-combination name)
(defreader method-combination-operator method-combination operator)

;;; -----------------------------------------------------------------------------------
;;; OBJ-DOCUMENTATION, (SETF OBJ-DOCUMENTATION)              Standard Generic Function
;;; -----------------------------------------------------------------------------------

(defgeneric obj-documentation (obj &rest doc-type)
  (:documentation "See CLOS specification for generic function documentation!!!"))

(defgeneric (setf obj-documentation) (value obj &rest doc-type)
  (:documentation "See CLOS specification for generic function (setf documentation)!!!"))

(defmethod obj-documentation ((method method) &rest doc-type)
  "test"
  (declare (ignore doc-type))
  (pe-find 'method-documentation
           (list (method-name method)
                 (method-qualifiers method)
                 (mapcar #'class-name (method-specializers method)))))

(defmethod obj-documentation ((gfn generic-function) &rest doc-type)
  (declare (ignore doc-type))
  (pe-find 'generic-function-documentation 
           (generic-function-name gfn)))

(defmethod obj-documentation ((cl class) &rest doc-type)
  (declare (ignore doc-type))
  (documentation (class-name cl) 'type))

(defmethod obj-documentation ((obj symbol) &rest doc-type)
  (unless doc-type
    (error "The second argument must be supplied for symbol ~S." obj))
  (documentation obj (first doc-type)))

(defmethod (setf obj-documentation) (value (obj symbol) &rest doc-type)
  (unless doc-type
    (error "The second argument must be supplied for symbol ~S." obj))
  (case (first doc-type)
    (type (when (find-class obj)
            (setf (obj-documentation (find-class obj)) value)))
    (function (when (find-gfn obj)
                (setf (obj-documentation (find-gfn obj)) value)))
    (t nil))
    (setf (documentation obj (first doc-type)) value))

(defmethod obj-documentation ((fn-spec cons) &rest doc-type)
  (unless (eq (first doc-type) 'function)
    (error "The second argument must be the symbol 'function instead of ~S." 
           (first doc-type)))
  (unless (setf-form-p fn-spec)
    (error "The first argument must be (SETF a_symbol) instead of ~S." fn-spec))
  (documentation (make-setf-name fn-spec) 'function))


(defmethod (setf obj-documentation) (value (fn-spec cons) &rest doc-type)
  (unless (eq (first doc-type) 'function)
    (error "The second argument must be the symbol 'function instead of ~S." 
           (first doc-type)))
  (unless (setf-form-p fn-spec)
    (error "The first argument must be (SETF a_symbol) instead of ~S." fn-spec))
  (let ((gfn-name (make-setf-name fn-spec)))
    (when (find-gfn gfn-name)
      (setf (obj-documentation (find-gfn gfn-name)) value))
    (setf (documentation gfn-name 'function) value)))



;;; eof

