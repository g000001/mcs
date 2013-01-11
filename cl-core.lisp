;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the bootstrap constructors file.
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
;;; DEFREADER/ DEFWRITER                                              Bootstrap Macros
;;; -----------------------------------------------------------------------------------

(defvar *during-bootstrap*)
(setf *during-bootstrap* t)

;(defmacro defreader (&rest args) args ())
;(defmacro defwriter (&rest args) args ())

;;; -----------------------------------------------------------------------------------
;;; DEFCLASS                                                                     Macro
;;; -----------------------------------------------------------------------------------
;;;
;;; Purpose:   User interface
;;; Arguments: class-name  a name of a class
;;;            supers       a list of class names
;;;            slots       a list that contains
;;;                            - slot names or
;;;                            - lists of a slot name with slot options
;;;                        slot-options are reader, writer, accessor, initarg,
;;;                                         initform, type and documentation
;;;            options     metaclass, documentation
;;; Values:    The result is a class object 
;;; CLOS:      compatible subset

(defmacro defclass (class-name direct-superclasses slots &rest options)
  (let ((metaclass-name   (or (second (assoc ':metaclass options))
                              'standard-base-class))
        (nonslot-initargs (second (assoc ':initargs options)))
        (documentation    (second (assoc ':documentation options)))
        (slot-specifications 
         (compute-canonical-slot-specifications slots class-name))
        (nonstandard-class-options ()) )
    (dolist (option options)
      (when (and *warn-if-unknown-class-option*
                 (not (mcs-memq (first option) *implemented-class-options*)))
        (warn "Unknown class option ~S in ~S." (first option) class-name))
      (unless (mcs-memq (first option) '(:metaclass :documentation :initargs))
        (setf nonstandard-class-options
              (nconc nonstandard-class-options
                     `(',(first option) ',(second option))))))
    (let ((canonical-slot-specifications (fourth slot-specifications))
          (slot-initargs (first slot-specifications))
          (defreader-forms (second slot-specifications))
          (defwriter-forms (third slot-specifications)))
      (let ((direct-slots (if slots `(list ,@canonical-slot-specifications) ())))
        `(progn
           ;; Functional Interface:
           (ensure-class ',class-name
                         ',metaclass-name
                         (list 'name ',class-name
                               'superclasses 
                               (list ,@(if direct-superclasses
                                         (mapcar #'(lambda (name)
                                                     `(find-class ',name 'signal-error))
                                                 direct-superclasses)
                                         '((find-class 'standard-object))))
                               'slots ,direct-slots
                               'initargs ',(append slot-initargs nonslot-initargs)
                               ,@nonstandard-class-options))
               ,(unless (mcs-memq metaclass-name 
                                  '(built-in-class abstract-built-in-class))
                  (%compute-deftype-form class-name))
               ,@defreader-forms
               ,@defwriter-forms
               ;; Programming Environment Stuff:
               (record-source-file ',class-name 'class)
               ,(when documentation
                  `(setf (documentation ',class-name 'type) ,documentation))
               ;; Result:
               (find-class ',class-name))))))

;;; -----------------------------------------------------------------------------------
;;; COMPUTE-CANONICAL-SLOT-SPECIFICATIONS                                     Function
;;; -----------------------------------------------------------------------------------

(defun compute-canonical-slot-specifications (slot-specs class-name)
  (declare (optimize (speed 3) (safety 0)))
  (let ((slot-names nil))
    (do ((rest-slot-specs slot-specs (rest rest-slot-specs))
         (name nil nil) (initform '<unbound> '<unbound>) (initfunction nil nil)
         (type nil nil) (documentation nil nil)
         (initarg nil nil) (defreader-forms nil) (defwriter-forms nil)
         (slot-class nil nil) (unknown-options nil nil) 
         (initargs nil) (result nil) (temp-result nil nil)) 
        ((null rest-slot-specs) 
         (list initargs defreader-forms defwriter-forms (reverse result)))
      (cond 
       ((consp (first rest-slot-specs))
        (setq name (caar rest-slot-specs))
        (doplist ((key value) (cdar rest-slot-specs))
          (case key
            (:reader (unless *during-bootstrap*
                       (push `(defreader ,value ,class-name ,name) defreader-forms)))
            (:writer (unless *during-bootstrap*
                       (push `(defwriter ,value ,class-name ,name) defwriter-forms)))
            (:accessor (unless *during-bootstrap*
                         (push `(defreader ,value ,class-name ,name) defreader-forms)
                         (push `(defwriter (setf ,value) ,class-name ,name) defwriter-forms)))
            (:initarg (if initarg
                        (error "For slot ~S of ~S :initarg option used twice."
                               name class-name)
                        (if (eq name value)
                          (setq initarg t
                                initargs (cons value initargs))
                          (error "Initarg ~S for slot ~S isn't eq to slot name."
                                 value name) )))
            (:allocation 
             (error "Don't use slot option :allocation for slot ~S any more." name))
            (:initform (if (eq initform '<unbound>)
                         (setq initform value
                               initfunction `#'(lambda () ,value))
                         (error "For slot ~S of ~S :initform option used twice."
                                name class-name)))
            (:type (if type 
                     (error "For slot ~S of ~S :type option used twice." name class-name)
                     (setq type `(and ,value))))
            (:slot-class (if slot-class
                           (error "For slot ~S of ~S :slot-class option used twice."
                                  name class-name)
                           (progn
                             (setq slot-class value)
                             (setf temp-result `('slot-class ',slot-class ,@temp-result)))))
            (:documentation (if documentation
                              (error "For slot ~S of ~S :documentation option used twice."
                                     name class-name)
                              (pe-store 'slot-documentation
                                        (list class-name name) value)))
            (t  (if (mcs-memq key *implemented-slot-options*)
                  (setq unknown-options
                          `(',key ,value ,@unknown-options))
                  (progn
                    (when *warn-if-unknown-slot-option*
                      (warn "Undefined slot option ~S for slot ~S in class ~S."
                            key name class-name))
                    (setq unknown-options
                          `(',key ,value ,@unknown-options))))) )))
       (t (setq name (first rest-slot-specs))))
      (if (mcs-memq name slot-names)
        (error "Slot ~S specified twice for class ~S." name class-name)
        (push name slot-names))
      
      (push
       `(list 'name ',name 'initform ',initform 'initfunction ,initfunction
              'type ',type 	;; 'readers ',readers 'writers ',writers 
              ,@unknown-options
              ,@temp-result)
       result))))

;;; -----------------------------------------------------------------------------------
;;; ENSURE-CLASS                                                    Bootstrap Function
;;; -----------------------------------------------------------------------------------

(setf (symbol-function 'ensure-class)
      #'(lambda (class-name metaclass-name initarguments)
          (declare (optimize (speed 3) (safety 0)))
          (let ((direct-superclasses (getf initarguments 'superclasses))
                (direct-slots (mapcar #'make-slot-definition (getf initarguments 'slots)))
                (direct-initargs (getf initarguments 'initargs)))
            (let ((class 
                   (case metaclass-name
                     ((base-class)
                      (%make-obj metaclass-name 
                                 #'instantiable-class-accessor
                                 class-name direct-superclasses
                                 direct-slots direct-initargs
                                 () () () ()))
                     ((built-in-class abstract-built-in-class)
                      (%make-obj metaclass-name 
                                 #'class-accessor
                                 class-name direct-superclasses 
                                 direct-slots direct-initargs))
                     ((abstract-base-class)
                      (%make-obj metaclass-name 
                                 #'class-accessor
                                 class-name direct-superclasses 
                                 direct-slots direct-initargs))
                     ((mixin-class)
                      (%make-obj metaclass-name 
                                 #'class-accessor
                                 class-name direct-superclasses 
                                 direct-slots direct-initargs))
                     (t (error "Wrong metaclass-name: ~S." metaclass-name)))))
              (insert-class class-name class)
              ;    (insert-type class)
              class))))

;(setf (symbol-function 'ensure-class)
;      #'(lambda (class-name metaclass-name initarguments)
;          (declare (optimize (speed 3) (safety 0)))
;          (let ((direct-superclasses (getf initarguments 'direct-superclasses))
;                (direct-slots (mapcar #'make-slot-definition (getf initarguments 'direct-slots)))
;                (direct-initargs (getf initarguments 'direct-initargs))
;                (direct-methods ()))
;            (let ((class 
;                   (case metaclass-name
;                     ((base-class)
;                      (make-mcs% metaclass-name 
;                                 ;(function std-class-accessor)
;                                 (vector
;                                 class-name direct-superclasses direct-methods
;                                 direct-slots direct-initargs
;                                 () () () () () () ())))
;                     ((built-in-class abstract-built-in-class)
;                      (make-mcs% metaclass-name 
;                                 ;(function built-in-accessor)
;                                 (vector
;                                 class-name direct-superclasses direct-methods 
;                                 direct-slots direct-initargs ())))
;                     ((abstract-base-class)
;                      (make-mcs% metaclass-name 
;                                 ;(function abstract-class-accessor)
;                                 (vector
;                                 class-name direct-superclasses direct-methods 
;                                 direct-slots direct-initargs () ())))
;                     ((mixin-class)
;                      (make-mcs% metaclass-name 
;                                 ;(function mixin-accessor)
;                                 (vector
;                                 class-name direct-superclasses direct-methods 
;                                 direct-slots direct-initargs ())))
;                     (t (error "Wrong metaclass-name: ~S." metaclass-name)))))
;              (insert-class class-name class)
;              ;    (insert-type class)
;              class))))

;;; -----------------------------------------------------------------------------------
;;; COMPUTE-CLASS-PRECEDENCE-LIST                                   Bootstrap Function
;;; -----------------------------------------------------------------------------------

(setf (symbol-function 'compute-class-precedence-list)
      #'(lambda (class direct-superclasses)
          (declare (optimize (speed 3) (safety 0)))
          (let (;(direct-superclasses (%%class-precedence-list class))
                (result (list class)))
            (dolist (superclass direct-superclasses)
              (setf result 
                    (append result (%%class-precedence-list superclass))))
            (remove-duplicates result :test #'eq :from-end nil))))
            
;(setf (symbol-function 'compute-class-precedence-list)
;      #'(lambda (class)
;          (declare (optimize (speed 3) (safety 0)))
;          (labels ((traverse-node (node result succ-fn)
;                                  (declare (optimize (speed 3) (safety 0)))
;                                  (if (mcs-memq node result) 
;                                    result
;                                    (if (%typep node 'single-inherited)
;                                      (append (%%class-precedence-list node) result)
;                                      (cons node (traverse-list (reverse (funcall succ-fn node))
;                                                                result 
;                                                                succ-fn)))))
;                   (traverse-list (nodes result succ-fn)
;                                  (declare (optimize (speed 3) (safety 0)))
;                                  (if (null nodes) 
;                                    result
;                                    (traverse-list (rest nodes) 
;                                                   (traverse-node (first nodes) result succ-fn)
;                                                   succ-fn))))
;            (cons class 
;                  (traverse-list (reverse (%%class-direct-superclasses class))
;                                 () 
;                                 #'(lambda (class) 
;                                     (%%class-direct-superclasses class)))))))

;;; -----------------------------------------------------------------------------------
;;; COMPUTE-SLOTS                                                   Bootstrap Function
;;; -----------------------------------------------------------------------------------

;;; Important assumption: each slot is specified only once in the class hierarchie!!!!!

(setf (symbol-function 'compute-slots) 
      #'(lambda (class direct-superclasses)
          (declare (optimize (speed 3) (safety 0)))
          (let ((result (%%class-slots class)))
            (dolist (superclass direct-superclasses)
              (setf result (append (%%class-slots superclass)
                                      result)))
            (remove-duplicates result :test #'eq :from-end t))))

;(setf (symbol-function 'compute-slots) 
;      #'(lambda (class)
;          (declare (optimize (speed 3) (safety 0)))
;          (let ((all-slots (%%class-direct-slots class)))
;            (dolist (superclass (rest (%%class-precedence-list class)) all-slots)
;              (when (%typep superclass 'single-inherited)
;                (return (append (%%class-slots superclass) all-slots)))
;              (when (or (eq superclass (find-class 'object))
;                        (eq superclass (find-class 't)))
;                (return all-slots))
;              (setf all-slots (append (%%class-direct-slots superclass)
;                                      all-slots))))))
     
;;; -----------------------------------------------------------------------------------
;;; COMPUTE-SLOT-ACCESSOR-LAMBDA                                    Bootstrap Function
;;; -----------------------------------------------------------------------------------
;;; Slot Accessor Computation

;(setf (symbol-function 'compute-slot-accessor-lambda)
;      #'(lambda (class)
;          (declare (optimize (speed 3) (safety 0)))
;          (let ((pos (list-length (%%class-slots class)))
;                (case-entries (list (list 't nil))))
;            (dolist (slot (reverse (%%class-slots class)))
;              (push (list (%%slot-name slot) (decf pos)) case-entries))
;            `(lambda (object slot-name)
;               (declare (optimize (speed 3) (safety 0))
;                        (ignore object))
;               (case slot-name
;                 ,@case-entries)))))

;;; -----------------------------------------------------------------------------------
;;; COMPUTE-INITARGS                                                Bootstrap Function
;;; -----------------------------------------------------------------------------------

(setf (symbol-function 'compute-initargs)
      #'(lambda (class direct-superclasses)
          (let ((result (%%class-initargs class)))
            (dolist (superclass direct-superclasses)
              (setf result (append (%%class-initargs superclass) result)))
            (remove-duplicates result :test #'eq :from-end t))))

;;; -----------------------------------------------------------------------------------
;;; MAKE-INSTANCE                                                   Bootstrap Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   Meta Object Protocol: 
;;; Arguments: class     is a class or a symbol
;;;            initargs  is a list of initialization arguments
;;; Values:    The result is a new instance of class. 

(setf (symbol-function 'make-instance)
      #'(lambda (class &rest initargs)
          (cond
           ((eq class 'slot-definition)
            (make-slot-definition class initargs))
           ((or (eq class (find-class 'method)) 
                (eq class (find-class 'multiple-functions-method)))
            (make-method class initargs))
           ((eq class (find-class 'generic-function))
            (make-generic class initargs))
           ((eq class (find-class 'method-combination))
            (make-method-combination class initargs))
           (t (error "Can't make an instance of ~S now." class)))))

;;; -----------------------------------------------------------------------------------
;;; MAKE-SLOT-DEFINITION                                            Bootstrap Function
;;; -----------------------------------------------------------------------------------
;;; The Standard-slot-definition Constructor
(defun make-slot-definition (plist)
  (declare (optimize (speed 3) (safety 0))
           (ignore class-name))
  (let ((name ())
        (initform '<unbound>)
        (initfunction nil)
        (type '(and t))
        (documentation ""))
    (doplist ((ia value) plist)
             (case ia
               (name (setq name value))
               (initform (setq initform value))
               (initfunction (setq initfunction value))
               (type (setq type  value))
               (documentation (setq documentation value))
               (t (when *check-if-wrong-initargs*
                    (warn "No such initarg during bootstrap:~S." ia)))))
    (make-mcs% 'slot-definition
               ;(function std-s-d-accessor)
               (vector name initform initfunction type) )))

;(defun make-slot-definition (class name initform initfunction type &rest args)
;  (declare (optimize (speed 3) (safety 0))
;           (ignore args))
;  (%make-obj class
;             (function std-s-d-accessor)
;             name initform initfunction type))

;;; -----------------------------------------------------------------------------------
;;; MAKE-METHOD-COMBINATION                                         Bootstrap Function
;;; -----------------------------------------------------------------------------------
;;; Method Combination Constructor 
(defun make-method-combination (class plist)
  (let (name options order operator identity-with-one-argument lookup-fn documentation)
    (doplist ((key val) plist)
      (case key
        (name (setf name val))
        (options (setf options val))
        (order (setf order val))
        (operator (setf operator val))
        (identity-with-one-argument (setf identity-with-one-argument val))
        (lookup-function (setf lookup-fn val))
        (documentation (setf documentation val))
        (t (error "Illegal initarg ~S for class ~S." key class))))
    (%make-obj (%%class-wrapper class)
               (%%class-slot-accessor class)
               name options order operator identity-with-one-argument 
               lookup-fn)))

;(defun make-method-combination (class name options order operator 
;                                identity-with-one-argument lookup-fn)
;  (%make-obj class
;             (%%class-slot-accessor class)
;             name options order operator identity-with-one-argument lookup-fn))

;;; -----------------------------------------------------------------------------------
;;; MAKE-GENERIC                                                    Bootstrap Function
;;; -----------------------------------------------------------------------------------
;;; Bootstrap Generic Function Constructor
(defun make-generic (class plist)
  (declare (optimize (speed 3) (safety 0)))
  (let (gfn-name lambda-list method-class method-combination documentation discr-name)
    (doplist ((key val) plist)
      (case key
        (name (setf gfn-name val))
        (lambda-list (setf lambda-list val))
        (method-class (setf method-class val))
        (method-combination (setf method-combination val))
        (documentation (setf documentation val))
        (discriminating-function (setf discr-name val))
        (t (error "Wrong initarg."))))
    (let ((signature (make-list (if (mcs-memq '&rest lambda-list)
                                  (- (length lambda-list) 2)
                                  (length lambda-list))
                                :initial-element nil))
          methods combined-methods)
      (let ((gfn 
             (%make-obj (%%class-wrapper class)
                        (%%class-slot-accessor class)
                        gfn-name lambda-list methods discr-name method-class
                        method-combination combined-methods signature)))
        (compute-discriminating-fn gfn)
        gfn))))

;(defun make-generic (class name lambda-list method-class method-combination methods)
;  (declare (optimize (speed 3) (safety 0)))
;  (let ((discriminating-function ()) (combined-methods ()) (signature ()))
;    (setf signature (make-list (if (mcs-memq '&rest lambda-list)
;                                 (- (length lambda-list) 2)
;                                 (length lambda-list))
;                               :initial-element (find-class 't)))
;    (let ((gfn 
;           (%make-obj class
;                      (%%class-slot-accessor class)
;                      name lambda-list methods discriminating-function
;                      method-class method-combination combined-methods signature)))
;      (setf (%%gfn-discriminating-function gfn)
;            (compute-discriminating-function gfn))
;      gfn)))

;;; -----------------------------------------------------------------------------------
;;; MAKE-METHOD                                                     Bootstrap Function
;;; -----------------------------------------------------------------------------------
(defun make-method (class plist)
  (declare (optimize (speed 3) (safety 0)))
  (let (gfn-name lambda-list specializers qualifiers function documentation)
    (doplist ((key val) plist)
      (case key
        (name (setf gfn-name val))
        (lambda-list (setf lambda-list val))
        (specializers (setf specializers val))
        (qualifiers (setf qualifiers val))
        (function (setf function val))
        (documentation (setf documentation val))
        (t (error "Wrong initarg."))))
    (if (eq class (find-class 'multiple-functions-method))
      (%make-obj (%%class-wrapper class)
                 (%%class-slot-accessor class)
                 gfn-name lambda-list  specializers qualifiers function ())
      (%make-obj (%%class-wrapper class)
                 (%%class-slot-accessor class)
                 gfn-name lambda-list  specializers qualifiers function ))))

;(defun make-method (class name lambda-list specializers qualifiers function)
;  (declare (optimize (speed 3) (safety 0)))
;  (%make-obj class
;             (%%class-slot-accessor class)
;             name lambda-list  specializers qualifiers function))

;;; -----------------------------------------------------------------------------------
;;; Macro to finalize inheritance for instantiable single-inherited derived classes
;;; -----------------------------------------------------------------------------------

;(defmacro complete-instantiable-classes (&rest class-names)
;  `(progn
;     ,@(let ((result ())) 
;         (dolist (class-name class-names)
;           (let ((class (find-class class-name)))
;             (let (;(initargs (compute-initargs class))
;                   (fn (compute-slot-accessor-lambda class))
;                   (slots-number (list-length (%%class-slots class))))
;               
;               (unless (%%class-slot-accessor class)
;                 (setf (%%class-slot-accessor class) fn)
;                 (push  `(setf (%%class-slot-accessor (find-class ',class-name))
;                               (function ,fn))
;                        result))
;;               (setf (%%class-initargs class) initargs)
;;               (push `(setf (%%class-initargs (find-class ',class-name))
;;                            ',initargs)
;;                     result)
;;               (setf (class-wrapper class) (list class))
;;               (push `(setf (class-wrapper (find-class ',class-name))
;;                            (list (find-class ',class-name)))
;;                     result)
;               (setf (%%class-slots-number class) slots-number)
;               (push `(setf (%%class-slots-number (find-class ',class-name))
;                            ',slots-number)
;                     result)
;               )))
;         result)
;     ',class-names))



;;; eof

