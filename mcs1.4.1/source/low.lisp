;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the low operations file.
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

;;; -----------------------------------------------------------------------------------
;;; The Protocol
;;; -----------------------------------------------------------------------------------

;;; (%class-of  obj)  -->  class                                               macro
;;; (class-of   obj)  -->  class                                            function

;;; (%type-of   obj)  -->  symbol                                              macro

;;; (%discr-key         obj)     -->  symbol                                   macro
;;; (mcs%r/w-discr-key  mcsobj)  -->  symbol                                   macro


;;; (mcs%typep  mcsobj type)  -->  bool                                        macro
;;; (%typep     obj type)     -->  bool                                        macro
;;; (%subtypep  type1 type2)  -->  bool                                        macro


;;; -----------------------------------------------------------------------------------
;;; The Implementation
;;; -----------------------------------------------------------------------------------

;;; -----------------------------------------------------------------------------------
;;; Class Accessor                                                 Bootstrap  Function
;;; -----------------------------------------------------------------------------------

(defun class-accessor (object slot-name)
  (declare (optimize (speed 3) (safety 0))
           (ignore object))
  (case slot-name
    (name 0) (superclasses 1) (slots 2) (initargs 3)
    (t nil)))

;;; -----------------------------------------------------------------------------------
;;; Instantiable Class Accessor                                        Bootstrap  Function
;;; -----------------------------------------------------------------------------------

(defun instantiable-class-accessor (object slot-name)
  (declare (optimize (speed 3) (safety 0))
           (ignore object))
  (case slot-name
    (name 0) (superclasses 1) (slots 2) (initargs 3)
    (slot-accessor 4) (prototype 5) (slots-number 6) (wrapper 7)
    (t nil)))

;;; -----------------------------------------------------------------------------------
;;; Class Accessors                                                             Macros
;;; -----------------------------------------------------------------------------------

(defmacro %%class-name (class) 				`(mcs%obj-ref ,class 0))
(defmacro %%class-precedence-list (class) 		`(mcs%obj-ref ,class 1))
(defmacro %%class-slots (class) 			`(mcs%obj-ref ,class 2))
(defmacro %%class-initargs (class) 			`(mcs%obj-ref ,class 3))

(defmacro %%class-slot-accessor (class)			`(mcs%obj-ref ,class 4))
(defmacro %%class-prototype (class) 			`(mcs%obj-ref ,class 5))
(defmacro %%class-slots-number (class)			`(mcs%obj-ref ,class 6))
(defmacro %%class-wrapper (class) 			`(mcs%obj-ref ,class 7))

;(defmacro %%class-direct-superclasses (class)	 	`(mcs%obj-ref ,class 1))
;(defmacro %%class-direct-methods (class)		`(mcs%obj-ref ,class 2))
;(defmacro %%class-direct-slots (class) 		`(mcs%obj-ref ,class 3))
;(defmacro %%class-direct-initargs (class) 		`(mcs%obj-ref ,class 4))
;(defmacro %%class-effective-methods (class) 		`(mcs%obj-ref ,class 7))

;(defmacro %%buil-in-class-precedence-list (class) 	`(mcs%obj-ref ,class 5)) ;; ** hack
;(defmacro %%built-in-class-precedence-list (class) 	`(mcs%obj-ref ,class 5)) ;; ** hack

;;; -----------------------------------------------------------------------------------
;;; Standard Method Accessor                              Bootstrap Function
;;; -----------------------------------------------------------------------------------

(defun method-accessor (object slot-name)
  (declare (optimize (speed 3) (safety 0))
           (ignore object))
  (case slot-name
    (name 0) (lambda-list 1) (specializers 2) (qualifiers 3) (function 4)
    (t nil)))

;;; -----------------------------------------------------------------------------------
;;; Method Accessors                                                            Macros
;;; -----------------------------------------------------------------------------------

(defmacro %%method-name (method) 		`(mcs%obj-ref ,method 0))
(defmacro %%method-lambda-list (method) 	`(mcs%obj-ref ,method 1))
(defmacro %%method-specializers (method)	`(mcs%obj-ref ,method 2))
(defmacro %%method-qualifiers (method)		`(mcs%obj-ref ,method 3))
(defmacro %%method-function (method)		`(mcs%obj-ref ,method 4))

;;; -----------------------------------------------------------------------------------
;;; Standard Generic Function Accessor                              Bootstrap Function
;;; -----------------------------------------------------------------------------------

(defun generic-function-accessor (object slot-name)
  (declare (optimize (speed 3) (safety 0))
           (ignore object))
  (case slot-name
    (name 0) (lambda-list 1) (methods 2) (discriminating-function 3) (method-class 4)
    (method-combination 5) (combined-methods 6) (signature 7)
    (t nil)))

;;; -----------------------------------------------------------------------------------
;;; Generic Function Accessors                                                  Macros
;;; -----------------------------------------------------------------------------------

(defmacro %%gfn-name (gfn)			`(mcs%obj-ref ,gfn 0))
(defmacro %%gfn-lambda-list (gfn)		`(mcs%obj-ref ,gfn 1))
(defmacro %%gfn-methods (gfn) 			`(mcs%obj-ref ,gfn 2))
(defmacro %%gfn-discriminating-function (gfn) 	`(mcs%obj-ref ,gfn 3))
;(defmacro %%gfn-discriminating-function (gfn) 	`(symbol-function (%%gfn-name ,gfn)))
(defmacro %%gfn-method-class (gfn) 		`(mcs%obj-ref ,gfn 4))
(defmacro %%gfn-method-combination (gfn)	`(mcs%obj-ref ,gfn 5))
(defmacro %%gfn-combined-methods (gfn)		`(mcs%obj-ref ,gfn 6))
(defmacro %%gfn-signature (gfn) 		`(mcs%obj-ref ,gfn 7))

;;; -----------------------------------------------------------------------------------
;;; Slot Description Accessor                                       Bootstrap Function
;;; -----------------------------------------------------------------------------------

(defun slot-definition-accessor (object slot-name)
  (declare (optimize (speed 3) (safety 0))
           (ignore object))
  (case slot-name
    (name 0) (initform 1) (initfunction 2) (type 3)
    (t nil)))

;;; -----------------------------------------------------------------------------------
;;; Slot Description Accessors                                                  Macros
;;; -----------------------------------------------------------------------------------

(defmacro %%slot-name (slot)		`(mcs%obj-ref ,slot 0))
(defmacro %%slot-initform (slot)	`(mcs%obj-ref ,slot 1))
(defmacro %%slot-initfunction (slot)	`(mcs%obj-ref ,slot 2))
(defmacro %%slot-type (slot)		`(mcs%obj-ref ,slot 3))

;;; -----------------------------------------------------------------------------------
;;; Standard Method Combination Accessor                            Bootstrap Function
;;; -----------------------------------------------------------------------------------

(defun method-combination-accessor (object slot-name)
  (declare (optimize (speed 3) (safety 0))
           (ignore object))
  (case slot-name
    (name 0) (options 1) (order 2) (operator 3) (identity-with-one-argument 4)
    (lookup-function 5)
    (t nil)))

;;; -----------------------------------------------------------------------------------
;;; Method Combination Accessors                                                Macros
;;; -----------------------------------------------------------------------------------

(defmacro %%method-combination-name (meth-c)		`(mcs%obj-ref ,meth-c 0))
(defmacro %%method-combination-operator (meth-c)	`(mcs%obj-ref ,meth-c 3))


;;; ***********************************************************************************
;;; Type Operations
;;; ***********************************************************************************

(defmacro %class-of (obj)
  `(if (%object-p ,obj)
     ;; user-definded class 
     (mcs%class-of ,obj)
     ;; built-in class
     (built-in-class-of ,obj)  ;; this function is defined in cl-boot.lisp
     ))

(defun class-of (obj)
  (declare (optimize (speed 3) (safety 1)))
  (%class-of obj))

(defmacro %type-of (obj)
  `(if (%object-p ,obj)
     ;; user-definded class
     (mcs%type-of ,obj)
     ;; built-in class
     (type-of ,obj)  ;; ** hack?
     ))

(defmacro %discr-key (obj)
  `(if (%object-p ,obj)
     (mcs%class-of ,obj)
     (built-in-class-of ,obj)))

(defmacro mcs%r/w-discr-key (obj)
  `(mcs%class-of ,obj))

(defmacro mcs%typep (obj type)
  `(and (mcs-memq ,type (%%class-precedence-list (mcs%class-of ,obj)))
        t))

(defmacro %typep (obj type)
  `(if (%object-p ,obj)
     (mcs%typep ,obj (find-class ,type))
     (typep ,obj ,type)))

(defmacro %subtypep (name1 name2)
  (declare (optimize (speed 3) (safety 0)))
  `(let ((class1 (find-class ,name1))
         (class2 (find-class ,name2)))
     (declare (optimize (speed 3) (safety 0)))
    (if class1
      (if (mcs-memq class2 (%%class-precedence-list class1)) t)
      (if class2
        ()
        (subtypep ,name1 ,name2)))))

(defmacro %subclassp (class1 class2)
  `(if (mcs-memq ,class2 (%%class-precedence-list ,class1))
     t
     ()))

;(defun mixin-method-p (method)
;  (declare (optimize (speed 3) (safety 0)))
;  (%typep method 'multiple-functions-method))
;
;(defun mixin-p (object)
;  (declare (optimize (speed 3) (safety 0)))
;  (%typep object 'mixin))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defun obj-type-of (object)
  (declare (optimize (speed 3) (safety 0)))
  (%type-of object))

(defun obj-typep (object type)
  (declare (optimize (speed 3) (safety 1)))
  (%typep object type))

(defun obj-subtypep (type1 type2)
  (declare (optimize (speed 3) (safety 1)))
  (%subtypep type1 type2))

(defun subclassp (class1 class2)
  (declare (optimize (speed 3) (safety 0)))
  (if (mcs-memq class2 (%%class-precedence-list class1))
    t))

(defmacro %subinstance-p (object class)
  `(if (%object-p ,object)
     (mcs%typep ,object ,class)
     (typep ,object (%%class-name ,class))))


(defun method-p (object)
  (declare (optimize (speed 3) (safety 0)))
  (and (%object-p object)
       (mcs%typep object 'method)))
 
(defun generic-p (object)
  (declare (optimize (speed 3) (safety 0)))
  (if (symbolp object)
    (if (find-gfn object) t)
    (mcs%typep object 'generic-function)))

(defun obj-copy (obj)
  (declare (optimize (speed 3) (safety 0)))
  (if (%object-p obj)
    (mcs%copy obj)
    (error "Can't make a copy of ~S." obj)))

;;; -----------------------------------------------------------------------------------
;;; CLASS-P                                                                   Function
;;; -----------------------------------------------------------------------------------
;;;
;;; Purpose:   User Interface: returns t if object is a class object.
;;; Arguments: object is any object.
;;; Values:    The result is true or false. 
;;; CLOS:      no such function.

(defvar *class-cl*) 

;; (setf *class-cl* (find-class 'class))  ;; has to be done later

(defun class-p (object)
  (declare (optimize (speed 3) (safety 0)))
  (and (%object-p object)
       (mcs%typep object *class-cl*)))

;;; -----------------------------------------------------------------------------------
;;; METACLASS-P                                                               Function
;;; -----------------------------------------------------------------------------------
;;;
;;; Purpose:   User Interface: returns t if object is a metaclass object.
;;; Arguments: object is any object.
;;; Values:    The result is true or false. 
;;; CLOS:      no such function.

(defun metaclass-p (object)
  (declare (optimize (speed 3) (safety 0)))
  (and (%object-p object)
       (mcs%typep object *class-cl*)
       (mcs-memq *class-cl* (%%class-precedence-list object))
       t))

;;; -----------------------------------------------------------------------------------
;;; Computing Reader and Writer
;;; -----------------------------------------------------------------------------------

(defun gen-reader-fn (index)
  (declare (optimize (speed 3) (safety 0)))
  `(lambda (%next-fns object)
     (declare (optimize (speed 3) (safety 0))
              (ignore %next-fns))
     (let ((result (mcs%obj-ref object ,index)))
       (if (eq result '<unbound>) 
         (slot-unbound (mcs%class-of object) object 
                       (%find-slot-name object ,index))
         result))) )

(defun gen-writer-fn (index)
  (declare (optimize (speed 3) (safety 0)))
  `(lambda (%next-fns value object)
     (declare (optimize (speed 3) (safety 0))
              (ignore %next-fns))
     (setf (mcs%obj-ref object ,index) value)) )

;;; -----------------------------------------------------------------------------------
;;; Computing Reader and Writer Closures at run time
;;; -----------------------------------------------------------------------------------

(defun gen-reader-closure (index)
  (declare (optimize (speed 3) (safety 0)))
  #'(lambda (%next-fns object)
     (declare (optimize (speed 3) (safety 0))
              (ignore %next-fns))
     (let ((result (mcs%obj-ref object index)))
       (if (eq result '<unbound>) 
         (slot-unbound (mcs%class-of object) object 
                       (%find-slot-name object index))
         result))) )

(defun gen-writer-closure (index)
  (declare (optimize (speed 3) (safety 0)))
  #'(lambda (%next-fns value object)
     (declare (optimize (speed 3) (safety 0))
              (ignore %next-fns))
     (setf (mcs%obj-ref object index) value)) )


;;; eof

