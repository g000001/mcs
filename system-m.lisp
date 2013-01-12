;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the System Methods and Functions File.
;;;
;;; notes:          
;;;
;;; contact:        Juergen Kopp, Harry Bretthauer
;;;
;;; history:
;;;          date:      author:             comments:
;;;          23.02.90   Harry Bretthauer    initial version
;;;          09.07.91   Juergen Kopp        check-for-existing-gfn modified 
;;; -----------------------------------------------------------------------------------

(in-package "MCS")

;(export '(find-method remove-method get-local-protocol get-protocol which-operations 
;          apropos-protocol next-method-p slot-exists-p slot-makunbound slot-boundp
;          class-p metaclass-p))

;;; -----------------------------------------------------------------------------------
;;; The Protocol
;;; -----------------------------------------------------------------------------------

;;; (%find-slot  class symbol)  -->  slot                                    function

(defgeneric print-object (obj stream))
(defgeneric obj-describe (obj))
(defgeneric find-method (generic-fn qualifier specializers &rest options))
(defgeneric remove-method (generic-fn method))
(defgeneric remove-direct-method (class method))
(defgeneric find-slot (class slot-name))
(defgeneric slot-missing (class obj slot-name operation &rest options))
(defgeneric slot-unbound (class obj slot-name))
(defgeneric no-applicable-methods (generic-fn &rest arguments))
(defgeneric no-next-method (generic-fn method &rest arguments))

;;; -----------------------------------------------------------------------------------
;;; PRINT-OBJECT                                             Standard-Generic-Function
;;; -----------------------------------------------------------------------------------
;;;
;;; Purpose:   Meta Object Protocol: controls the external representation.
;;;            called in PRINT-MCS, which is called in PRINT.
;;; Arguments: object    is an mcs-object (structure MCSOBJECT).
;;;            stream    is a stream.
;;; Values:    The result is the printed representation of object. 
;;; CLOS:      compatible.

(defmethod PRINT-OBJECT ((object object) stream)
  (if (and (functionp (mcs%slot-fn-of object))
           (slot-exists-p object 'name)
           (slot-boundp object 'name))
    (format stream "#<~A ~S>" 
            (string-capitalize (%%class-name (class-of object)))
            (slot-value object 'name))
    (format stream "#<~A>" 
            (string-capitalize (%%class-name (class-of object))))))

(defmethod PRINT-OBJECT ((object method) stream)
  (let ((class-name (string-capitalize (%%class-name (class-of object))))
        (name (%%method-name object))
        (qualifiers (%%method-qualifiers object))
        (specializers (%%method-specializers object)))
    (if (mcs-memq '<unbound> (list name qualifiers specializers))
      (call-next-method)
      (let ((signature (mapcar #'(lambda (class)
                                   (%%class-name class))
                               specializers)))
      (if qualifiers
        (format stream "#<~A ~S ~S ~S>"
                class-name
                name qualifiers signature )
        (format stream "#<~A ~S ~S>"
                class-name 
                name signature))))))

;;; -----------------------------------------------------------------------------------
;;; PRINT-MCS                                                                 Function
;;; -----------------------------------------------------------------------------------
;;;
;;; PRINT-MCS was previosly defined in file core.lisp. Now it can call PRINT-OBJECT.

#|
(defun mcs%print (object stream depth)
  (declare (ignore depth))                    
  (format stream "<obj ~S>" (%%class-name object)))
|#

(setf (symbol-function 'mcs%print)
      #'(lambda (object stream depth)
          (declare (ignore depth))                    
          (print-object object stream)))

;;; -----------------------------------------------------------------------------------
;;; OBJ-DESCRIBE                                             Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;;
;;; Purpose:   User Interface: prints information about a given object on std. output.
;;;            calls DESCRIBE-MCS or DESCRIBE 
;;; Arguments: object    is any Lisp object.
;;; Values:    The result is nil. 
;;; CLOS:      Standard Generic Function DESCRIBE 
;;;
;;; Remarks:   DESCRIBE is defined in the Lisp System. 
;;;            Redefining a system function is implementation dependent.


(defmethod OBJ-DESCRIBE (object) (describe object))

(defmethod OBJ-DESCRIBE ((object object)) 
  (let ((class (mcs%class-of object)))
      (format t "~&~S, an instance of ~S, has slot values:~%" 
              object class)
      (dolist (ivar (class-slots class))
        (let ((slot-name (slot-definition-name ivar)))
          (format t "~%    ~S:~27T~S" 
                  slot-name
                  (slot-value-low object slot-name))))))

;;; Generic Functions for OBJECTS:

(defgeneric reinitialize-instance (obj &rest initlist))

(defmethod reinitialize-instance ((obj object) &rest initargs)
  (let ((class (mcs%class-of obj))
        (collected-initargs nil))
    ;; initialize from initargs:
    (doplist ((sl-name sl-value) initargs
              ((valid-initargs (%%class-initargs class))))
      (if (mcs-memq sl-name collected-initargs)
        (error "Initarg ~S supplied twice: ~S." sl-name initargs)
        (push sl-name collected-initargs))
      (if (mcs-memq sl-name valid-initargs)
        (setf (slot-value obj sl-name) sl-value)
        (when *check-if-wrong-initargs*
          (warn "Illegal initarg ~S for ~S." sl-name obj))))
    obj))

;;; -----------------------------------------------------------------------------------
;;; FIND-METHOD                                              Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   User Interface: 
;;; Arguments: gfn              is a generic function object.
;;;            method-qualifiers is a list of qualifiers for the method.
;;;            specializers      is a list of class objects
;;; Values:    The result is the method object with the given method qualifiers and 
;;;            parameter specializers or nil.
;;; CLOS:      + &optional (errorp t)
;;;              if no method is found and errorp is t an error is signaled.

(defmethod find-method ((gfn generic-function)
                        method-qualifiers specializers &rest options)
  (let ((errorp (if options (first options) 't))
        (method (dolist (method (%%gfn-methods gfn))
                  (when (and (equal specializers (%%method-specializers method))
                             (equal method-qualifiers (%%method-qualifiers method)))
                    (return method)))))
    (if method 
      method
      ;; check list length of specializers
      (progn
        (if (not (= (length specializers) 
                    (length (%%gfn-signature gfn))))
          (error "Wrong list length of specializers: ~S for ~S:"
                 specializers gfn)
          (if errorp (error "No method in ~S with qualifiers ~S and specializers ~S."
                            gfn method-qualifiers specializers))) ))))

;;; -----------------------------------------------------------------------------------
;;; REMOVE-METHOD                                            Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   User Interface: 
;;; Arguments: gfn              is a generic function object.
;;;            method           is a method object.
;;; Values:    The result is the modified generic function, which is eq to gfn.
;;; CLOS:      compatible.


(defmethod remove-method ((gfn generic-function)
                          (method method))
  "Removes method-object from gfn-object and from associated
           class-object;
           removes invalid combined methods."
  (let ((specializers (method-specializers method)))
    (setf (%%gfn-methods gfn)
          (mcs-remq method (%%gfn-methods gfn)))
                
    
    ;; remove method from the list of backpointers to corresponding methods
    ;; of the specializers
    (dolist (class specializers)
      (remove-direct-method class method))
    (remove-invalid-combined-methods gfn (relevant-method-specializers
                                          (%%gfn-signature gfn)
                                          specializers)))
  gfn)

 
;;; -----------------------------------------------------------------------------------
;;; REMOVE-DIRECT-METHOD                                     Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   Meta Object Protocol: removes method from list of backpointers of class,
;;;            which appears as specializer in methods specializers list.
;;;            called in remove-method for standard generic functions.
;;; Arguments: class    is a class object.
;;;            method is a method object.
;;; Values:    The result is unspecified. 
;;; CLOS:      compatible.

(defmethod  remove-direct-method ((class class) method)
  (declare (ignore class method))
  ())

;;; -----------------------------------------------------------------------------------
;;; FIND-SLOT                                                Standard Generic Function
;;; -----------------------------------------------------------------------------------

(defmethod find-slot ((class class) slot-name)
  (dolist (slot (%%class-slots class))
    (if (eq slot-name (%%slot-name slot)) (return slot))))

;;; -----------------------------------------------------------------------------------
;;; COMPUTE-APPLICABLE-METHODS                                                Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   Meta Object Protocol: computes the applicable methods of
;;;            a generic function for supplied arguments.
;;; Arguments: gfn    is a generic function object.
;;;            arguments is a list of arguments to that generic function.
;;; Values:    sorted list of applicable methods. 
;;; CLOS:      compatible.

(defun compute-applicable-methods (gfn arguments)
  (declare (optimize (speed 3) (safety 1)))
  (select-applicable-methods 
   gfn 
   (mapcar #'(lambda (arg sign)
                       (if (eq sign (find-class 't)) sign (%discr-key arg)))
           arguments (%%gfn-signature gfn))))


;;; -----------------------------------------------------------------------------------
;;; SLOT-MISSING                                             Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   Meta Object Protocol: signals an error, if no slot has been found.
;;;            called in slot-value (setf slot-value), ...
;;; Arguments: class    is a class object.
;;;            object   is an instance of class, on which slot-value,... was applied.
;;;            slot-name is the name of the slot.
;;;            operation is either slot-value or (setf slot-value) or ...
;;; Values:    The result is an error. 
;;; CLOS:      one optional parameter more: &optional new-value

(defmethod slot-missing ((a_class instantiable) (object object) 
                         slot-name operation &rest options)
  (declare (ignore a_class options))
  (error "No slot ~S in ~S while executing ~S." slot-name object operation))

;;; -----------------------------------------------------------------------------------
;;; SLOT-UNBOUND                                             Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   Meta Object Protocol: signals an error, if slot is unbound.
;;;            called in slot-value, a reader...
;;; Arguments: class    is a class object.
;;;            object   is an instance of class, on which slot-value,... was applied.
;;;            slot-name is the name of the slot.
;;; Values:    The result is an error. 
;;; CLOS:      compatible

(defmethod slot-unbound ((a_class defined) (object object) slot-name)
  (declare (ignore a_class))
  (error "Slot ~S in ~S is unbound." slot-name object))

;;; -----------------------------------------------------------------------------------
;;; NO-APPLICABLE-METHODS                                    Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   Meta Object Protocol: signals an error, if there are no applicable
;;;            methods in generic function for supplied arguments.
;;; Arguments: gfn    is a generic function object.
;;;            arguments is a list of arguments to that generic function.
;;; Values:    The result is an error. 
;;; CLOS:      compatible.

(defmethod no-applicable-methods ((gfn generic-function) 
                                   &rest arguments)
    (error "No applicable methods in ~S for ~S." gfn arguments))

;;; -----------------------------------------------------------------------------------
;;; NEXT-METHOD-P                                                                Macro
;;; -----------------------------------------------------------------------------------
;;; Purpose:   User Interface: localy defined macro can be used within the body of
;;;            a method to determine whether a next method exists.
;;;            called in slot-value (setf slot-value), ...
;;; Arguments: no arguments
;;; Values:    The result is true or false. 
;;; CLOS:      is a function like call-next-method.

(defmacro next-method-p ()
  `%next-fns)

;;; -----------------------------------------------------------------------------------
;;; NO-NEXT-METHOD                                           Standard Generic Function
;;; -----------------------------------------------------------------------------------

(defmethod no-next-method ((gfn generic-function) (method method)
                           &rest args)
  (error "No next method in ~S for arguments: ~S while executing method ~S."
         gfn args method))


;;; -----------------------------------------------------------------------------------
;;; SLOT-EXISTS-P                                                             Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   User Interface: tests whether the object has a slot of the given name.
;;; Arguments: object   is any object.
;;;            slot-name is a symbol.
;;; Values:    The result is true or false. 
;;; CLOS:      compatible.

;;; history: date       name    comment
;;;          24.02.90   *hb*    doesn't call has-slot

(defun slot-exists-p (object slot-name)
  (declare #|(optimize (speed 3) (safety 1))|#)
  (and (%object-p object)
    (%slot-location-of object slot-name)
    t))

;;; -----------------------------------------------------------------------------------
;;; SLOT-MAKUNBOUND                                                           Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   User Interface: restores a slot in an object to the unbound state.
;;; Arguments: object   is an instance of a class.
;;;            slot-name is a symbol.
;;; Values:    The result is the object. 
;;; CLOS:      compatible.

(defun slot-makunbound (object slot-name)
  (declare #|(optimize (speed 3) (safety 1))|#)
  (if (%object-p object)
    (let ((slot-position (%slot-location-of object slot-name)))
    (declare (optimize (speed 3) (safety 0)))
    (if slot-position  
      (setf (mcs%obj-ref object slot-position) '<unbound>)
      (slot-missing (mcs%class-of object) object slot-name 'slot-makunbound)))
    (slot-missing (built-in-class-of object) object slot-name 'slot-makunbound)))

;;; -----------------------------------------------------------------------------------
;;; SLOT-BOUNDP                                                               Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   User Interface: tests whether a slot in an object is bound.
;;; Arguments: object   is an instance of a class.
;;;            slot-name is a symbol.
;;; Values:    The result is true or false. 
;;; CLOS:      compatible.

(defun slot-boundp (object slot-name)
  (declare #|(optimize (speed 3) (safety 1))|#)
  (if (%object-p object)
    (let ((slot-position (%slot-location-of object slot-name)))
      (declare (optimize (speed 3) (safety 0)))
      (if slot-position
        (not (eq (mcs%obj-ref object slot-position) '<unbound>))
        (slot-missing (mcs%class-of object) object slot-name 'slot-boundp)))
    (slot-missing (built-in-class-of object) object slot-name 'slot-boundp)))


;;; -----------------------------------------------------------------------------------
;;; CHECK-FOR-EXISTING-GFN                                          Bootstrap Function
;;; -----------------------------------------------------------------------------------

;; *jak* 09.07.91

(setf (symbol-function 'check-for-existing-gfn)
      #'(lambda (fn-specifier lambda-list plist)
          (declare #|(optimize (speed 3) (safety 0))|#
                   (ignore plist))
          ;; check for already existing generic function
          (let ((gfn (find-gfn fn-specifier)))
            (if (%typep gfn 'generic-function)
              ;; validate lambda-list
              (let ((old-lambda-list (%%gfn-lambda-list gfn)))
                (if (not 
                     ;; eq --> eql : KCL error
                     (and (eql (length old-lambda-list) (length lambda-list))
                          (let ((rest-par (mcs-memq '&rest old-lambda-list)))
                            (eql (length rest-par) 
                                 (length (mcs-memq '&rest lambda-list))))))
                  (error "Can't change old lambda-list ~S to ~S for ~S." 
                         old-lambda-list lambda-list gfn))
                ;; change method-combination if different
                ;      (if (not (eq (%%gfn-method-combination gfn) method-combination))
                ;        (setf (%%gfn-method-combination gfn) method-combination))
                ;; validate gfn-class
                ;; validate method-class
                gfn)
              (progn
                (error "Can't make a non generic function generic: ~S." fn-specifier)
                ())))))



;;; eof

