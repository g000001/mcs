;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the class definition file.
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

;(export '(base-class object built-in-class
;          generic-function standard-method class-method mixin-class-method
;          class-name obj-documentation))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

;(defclass t () 
;  () 
;  (:metaclass abstract-built-in-class)
;  (:documentation "The most common built-in class."))

(progn
  (ensure-class 't
                'abstract-built-in-class
                (list 'name 't 'superclasses ())))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass object (t)
  ()
  (:metaclass abstract-base-class)
  (:documentation 
   "The class Object defines the most common properties of objects."))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass class (object)
  ((name :initform 'Anonymous
         :initarg name 
         :reader class-name)
;   (direct-superclasses :initform (list (find-class 'object))
;                        :initarg direct-superclasses
;                        :reader class-direct-superclasses)
;   (direct-methods :initform () 
;                   :reader class-direct-methods)
;   (direct-slots :initform ()
;                 :initarg direct-slots 
;                 :reader class-direct-slots)
;   (direct-initargs :initform ()
;                    :initarg direct-initargs
;                    :reader class-direct-initargs)
   (superclasses :initform (list (find-class 'object))
                 :initarg superclasses
                 :reader class-precedence-list)
   (slots :initform ()
          :initarg slots
          :reader class-slots)
   (initargs :initform ()
             :initarg initargs
             :reader class-initargs))
  (:metaclass abstract-base-class)
  (:documentation "Class is the most common base class."))

(setf *class-cl* (find-class 'class))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass defined (object)
  ()
  (:metaclass mixin-class) 
  (:documentation "Generalizes the structured property of classes."))

(defclass built-in (object)
  ()
  (:metaclass mixin-class) 
  (:documentation "Generalizes the built-in property of classes."))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass single-inherited (object)
  ()
 ; ((slots :reader class-slots))
  (:metaclass mixin-class) 
  (:documentation "Generalizes the property of classes to be inherited single."))

(defclass multiple-inherited (object)
  ()
  (:metaclass mixin-class) 
  (:documentation "Generalizes the property of classes to be inherited multiple"))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass abstract (object)
  ()
  (:metaclass mixin-class) 
  (:documentation "Generalizes the noninstantiability property of classes."))

(defclass instantiable (object) 
  (
;   (effective-methods :initform ()
;                      :reader class-effective-methods)
;   (initargs :reader class-initargs)
   (slot-accessor :reader class-slot-accessor)
   (prototype :initform ()
              :reader class-prototype)
   (slots-number :reader class-slots-number)
   (wrapper :reader class-wrapper))
  (:metaclass mixin-class) 
  (:documentation "Generalizes the instantiability property of classes."))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass base-class (instantiable single-inherited defined class)
  ()
  (:metaclass base-class)
  (:documentation
   "The metaclass base-class defines the standard properties of classes."))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass abstract-base-class (abstract single-inherited defined class)
  ()
  (:metaclass base-class)
  (:documentation "Class of abstract single-inherited classes."))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass mixin-class (abstract multiple-inherited defined class)
  ()
  (:metaclass base-class)
  (:documentation "Class of abstract multiple-inherited classes (mixin-classs)."))

;;; -----------------------------------------------------------------------------------
;;; ABSTRACT-BUILT-IN-CLASS                                                  Metaclass 
;;; -----------------------------------------------------------------------------------

(defclass abstract-built-in-class (abstract built-in class)
  ()
  (:metaclass base-class)
  (:documentation "Class of all built-in classes."))

;;; -----------------------------------------------------------------------------------
;;; BUILT-IN-CLASS                                                           Metaclass 
;;; -----------------------------------------------------------------------------------

(defclass built-in-class (built-in class)
  ()
  (:metaclass base-class)
  (:documentation "Class of all built-in classes."))

;;; -----------------------------------------------------------------------------------
;;; SLOT-DEFINITION                                                     Standard Class
;;; -----------------------------------------------------------------------------------

(defclass slot-definition (object)
  ((name :initarg name 
         :reader slot-definition-name)
   (initform :initarg initform 
             :reader slot-definition-initform)
   (initfunction :initarg initfunction 
                 :reader slot-definition-initfunction)
   (type :initarg type
         :reader slot-definition-type))
  (:initargs (slot-class))
  (:metaclass base-class)
  (:documentation
   "The class Slot-definition defines the slot description."))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass method (object)
  ((name :initarg name 
         :reader method-name)
   (lambda-list :initarg lambda-list 
                :reader method-lambda-list)
   (specializers :initarg specializers 
                 :reader method-specializers)
   (qualifiers :initarg qualifiers 
               :reader method-qualifiers)
   (function :initarg function 
             :reader method-function))
  (:metaclass base-class)
  (:documentation  "The class Standard-Method."))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass reader-method (method)
  ()
  (:metaclass base-class))

(defclass writer-method (method)
  ()
  (:metaclass base-class))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

;(defclass with-multiple-functions (object)
;  ((multiple-functions :initform nil 
;                       :accessor method-multiple-functions))
;  (:metaclass mixin-class)
;  (:documentation "The multiple compiled functions mixin-class."))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

;(defclass multiple-functions-method (with-multiple-functions method)
;  ()
;  (:metaclass base-class)
;  (:documentation  "The class of methods with multiple compiled functions."))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass generic-function (object)
  ((name :initarg name 
         :reader generic-function-name)
   (lambda-list :initarg lambda-list 
                :reader generic-function-lambda-list)
   (methods :initform nil 
            :accessor generic-function-methods)
   (discriminating-function :initarg discriminating-function
                            :accessor generic-function-discriminating-function)
   (method-class :initarg method-class 
                 :accessor generic-function-method-class)
   (method-combination :initarg method-combination 
                       :reader generic-function-method-combination)
   (combined-methods :initform nil 
                     :accessor generic-function-combined-methods)
   (signature :reader generic-function-signature))
  (:metaclass base-class)
  (:documentation "The class Generic-Function."))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass reader (generic-function) () (:metaclass base-class))

(defclass writer (generic-function) () (:metaclass base-class))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defclass method-combination (object)
  ((name :initarg name 
         :reader method-combination-name)
   (options :initarg options)
   (order :initarg order)
   (operator :initarg operator)
   (identity-with-one-argument :initarg identity-with-one-argument)
   (lookup-function :initarg lookup-function))
  (:documentation "The class Method-Combination.")
  (:metaclass base-class))



;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

;;; -----------------------------------------------------------------------------------
;;; The built-in classes (instances of Built-in-class or Abstract-built-in-class)
;;; -----------------------------------------------------------------------------------

(defclass character (t) () (:metaclass built-in-class))

(defclass symbol (t) () (:metaclass built-in-class))

(defclass sequence (t) () (:metaclass abstract-built-in-class))

(defclass array (t) () (:metaclass built-in-class))

(defclass vector (array sequence) () (:metaclass built-in-class))

(defclass bit-vector (vector) () (:metaclass built-in-class))

(defclass string (vector) () (:metaclass built-in-class))

(defclass list (sequence) () (:metaclass abstract-built-in-class))

(defclass cons (list) () (:metaclass built-in-class))

(defclass null (symbol list) () (:metaclass built-in-class))

(defclass number (t) () (:metaclass abstract-built-in-class))

(defclass rational (number) () (:metaclass built-in-class))

(defclass complex (number) () (:metaclass built-in-class))

(defclass float (number) () (:metaclass built-in-class))

(defclass integer (rational) () (:metaclass built-in-class))

(defclass ratio (rational) () (:metaclass built-in-class))

(defclass stream (t) () (:metaclass built-in-class))

(defclass hash-table (t) () (:metaclass built-in-class))

(defclass function (t) () (:metaclass built-in-class))


;;; -----------------------------------------------------------------------------------
;;; Fix up the pointer of each class and each slot-definition to its class
;;; -----------------------------------------------------------------------------------

;;; Create and Insert class wrappers for instantiable classes:

(progn
  (mapc #'(lambda (cl-name)
            (let ((class (find-class cl-name)))
              (setf (%%class-wrapper class) (list class))))
        '(base-class abstract-base-class mixin-class 
          abstract-built-in-class built-in-class 
          slot-definition
          method reader-method writer-method
          generic-function reader writer
          method-combination)))

(progn
  (maphash #'(lambda (cl-name cl)
               (declare (ignore cl-name))
               ;; fix up the pointers to classes of direct-slots of a defined class
               (mapc #'(lambda (slot) 
                         (setf (mcs%-isit slot) 
                               (%%class-wrapper (find-class (mcs%-isit slot)))))
                     (%%class-slots cl))
               ;; fix up the pointer to the class of cl
               (setf (mcs%-isit cl) (%%class-wrapper (find-class (mcs%-isit cl)))))
           *named-classes*))

;;; -----------------------------------------------------------------------------------
;;; Finalize inheritance for built-in classes
;;; -----------------------------------------------------------------------------------

(progn
  (mapc #'(lambda (cl-name)
            (let ((class (find-class cl-name)))
              (let ((direct-superclasses (%%class-precedence-list class)))
                (setf (%%class-precedence-list class)
                      (compute-class-precedence-list class 
                                                     direct-superclasses)))))
        '(t sequence list number
          symbol character array stream hash-table function cons 
          null vector rational float complex
          bit-vector string ratio integer)))

;;; -----------------------------------------------------------------------------------
;;; Finalize inheritance for defined classes
;;; -----------------------------------------------------------------------------------

(progn
  (mapc #'(lambda (class-name)
            (let ((class (find-class class-name)))
              (let ((direct-superclasses (%%class-precedence-list class)))
                (setf (%%class-precedence-list class)
                      (compute-class-precedence-list class direct-superclasses))
              (setf (%%class-slots class)
                    (compute-slots class direct-superclasses))
              (setf (%%class-initargs class)
                    (compute-initargs class direct-superclasses)))))
        '(object
          abstract instantiable single-inherited multiple-inherited
          defined built-in class 
          base-class abstract-base-class mixin-class abstract-built-in-class built-in-class 
          slot-definition
          method reader-method writer-method
          generic-function reader writer
          method-combination)))

;;; -----------------------------------------------------------------------------------
;;; Finalize inheritance for instantiable single-inherited defined classes
;;; -----------------------------------------------------------------------------------

;(complete-instantiable-classes
; base-class abstract-base-class mixin-class abstract-built-in-class built-in-class 
; slot-definition
; method reader-method writer-method
; generic-function reader writer
; method-combination)

(dolist (class-name '(base-class abstract-base-class mixin-class abstract-built-in-class built-in-class 
                      slot-definition
                      method reader-method writer-method
                      generic-function reader writer
                      method-combination))
  (let ((class (find-class class-name)))
    (let ((slots-number (list-length (%%class-slots class))))
      (setf (%%class-slots-number class) slots-number))))

(progn
  (setf (%%class-slot-accessor (find-class 'base-class))
        #'instantiable-class-accessor)
  (setf (%%class-slot-accessor (find-class 'abstract-base-class))
        #'class-accessor)
  (setf (%%class-slot-accessor (find-class 'mixin-class))
        #'class-accessor)
  (setf (%%class-slot-accessor (find-class 'abstract-built-in-class))
        #'class-accessor)
  (setf (%%class-slot-accessor (find-class 'built-in-class))
        #'class-accessor)
  (setf (%%class-slot-accessor (find-class 'slot-definition))
        #'slot-definition-accessor)
  (setf (%%class-slot-accessor (find-class 'method))
        #'method-accessor)
  (setf (%%class-slot-accessor (find-class 'reader-method))
        #'method-accessor)
  (setf (%%class-slot-accessor (find-class 'writer-method))
        #'method-accessor)
  (setf (%%class-slot-accessor (find-class 'generic-function))
        #'generic-function-accessor)
  (setf (%%class-slot-accessor (find-class 'reader))
        #'generic-function-accessor)
  (setf (%%class-slot-accessor (find-class 'writer))
        #'generic-function-accessor)
  (setf (%%class-slot-accessor (find-class 'method-combination))
        #'method-combination-accessor))

;;; -----------------------------------------------------------------------------------
;;; Insert Standard Objects 
;;; -----------------------------------------------------------------------------------

(setf (find-class 'standard-object) (find-class 'object))

(setf (find-class 'standard-base-class) (find-class 'base-class))
(setf (find-class 'standard-abstract-base-class)
      (find-class 'abstract-base-class))
(setf (find-class 'standard-mixin-class) (find-class 'mixin-class))

(setf (find-class 'standard-slot-definition) (find-class 'slot-definition))
(setf (find-class 'standard-generic-function) (find-class 'generic-function))
(setf (find-class 'standard-method) (find-class 'method))

;;; Historical nick names:

(setf (find-class 'standard-class) (find-class 'base-class))
(setf (find-class 'standard-mixin) (find-class 'mixin-class))
(setf (find-class 'mixin) (find-class 'mixin-class))


;;; -----------------------------------------------------------------------------------
;;; Built-in-class-of                                                         Function
;;; -----------------------------------------------------------------------------------

(setf *cons-cl* (find-class 'cons))
(setf *null-cl* (find-class 'null))
(setf *symbol-cl* (find-class 'symbol))

(setf *string-cl* (find-class 'string))
(setf *bit-vector-cl* (find-class 'bit-vector))
(setf *vector-cl* (find-class 'vector))
(setf *array-cl* (find-class 'array))

;; numbers
(setf *integer-cl* (find-class 'integer))
(setf *ratio-cl* (find-class 'ratio))
(setf *rational-cl* (find-class 'rational))
(setf *float-cl* (find-class 'float))
(setf *complex-cl* (find-class 'complex))
(setf *number-cl* (find-class 'number))

(setf *character-cl* (find-class 'character))
(setf *stream-cl* (find-class 'stream))
(setf *hash-table-cl* (find-class 'hash-table))
(setf *function-cl* (find-class 'function))
(setf *t-cl* (find-class 't))

(defun built-in-class-of (object)
  (declare (optimize (speed 3) (safety 0)))
  (typecase object
    (cons *cons-cl*)
    (null *null-cl*)
    (symbol *symbol-cl*)
    
    (string *string-cl*)
    (bit-vector *bit-vector-cl*)
    (vector *vector-cl*)
    (array *array-cl*)
    
    ;; numbers
    (integer *integer-cl*)
    (ratio *ratio-cl*)
    (rational *rational-cl*)
    (float *float-cl*)
    (complex *complex-cl*)
    (number *number-cl*)
    
    (character *character-cl*)
    (stream *stream-cl*)
    (hash-table *hash-table-cl*)
    (function *function-cl*)
    (t *t-cl*)))

;;; -----------------------------------------------------------------------------------
;;; Built-in-key-of                                                           Function
;;; -----------------------------------------------------------------------------------

(defun built-in-key (object)
  (declare (optimize (speed 3) (safety 0)))
  (typecase object
    (cons *cons-cl*)
    (null *null-cl*)
    (symbol *symbol-cl*)
    
    (string *string-cl*)
    (bit-vector *bit-vector-cl*)
    (vector *vector-cl*)
    (array *array-cl*)
    
    ;; numbers
    (integer *integer-cl*)
    (ratio *ratio-cl*)
    (rational *rational-cl*)
    (float *float-cl*)
    (complex *complex-cl*)
    (number *number-cl*)
    
    (character *character-cl*)
    (stream *stream-cl*)
    (hash-table *hash-table-cl*)
    (function *function-cl*)
    (t *t-cl*)))

;(defun built-in-key (object)
;  (declare (optimize (speed 3) (safety 0)))
;  (typecase object
;    (cons 'cons)
;    (null 'null)
;    (symbol 'symbol)
;
;    (string 'string)
;    (bit-vector 'bit-vector)
;    (vector 'vector)
;    (array 'array)
;
;    ;; numbers
;    (integer 'integer)
;    (ratio 'ratio)
;    (rational 'rational)
;    (float 'float)
;    (complex 'complex)
;    (number 'number)
;
;    (character 'character)
;    (stream 'stream)
;    (hash-table 'hash-table)
;    (function 'function)
;    (t 't)))




;;; eof

