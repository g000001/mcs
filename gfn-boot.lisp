;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the Methods and Generic Functions Boot File.
;;;                 After loading this file the bootstrap methods are created:
;;;                  - for standard objects 
;;;                  - for standard generic functions
;;;                  - for classes
;;;                  that allow make-instance to follow the protocol
;;;                 Then, you can create methods and generic functions using the 
;;;                 external protocol:
;;;                  - defmethod, defgeneric, make-instance, initialize-instance
;;;
;;; exports:
;;;  (initialize-instance obj list)  -->  obj                                  generic
;;;  (allocate-instance class list)  -->  obj                                  generic
;;;  (make-instance {class | symbol} . list  --> obj                          function
;;;
;;; notes:          
;;;
;;; contact:        Juergen Kopp, Harry Bretthauer
;;;
;;; history:
;;;          date:      author:             comments:
;;;          23.02.90   Harry Bretthauer    initial version
;;;          06.12.90   jak                 initialize-instance modified!!
;;; -----------------------------------------------------------------------------------

(in-package "MCS")

;(export '(initialize-instance shared-initialize allocate-instance make-instance))

(defgeneric initialize-instance (obj &rest initlist))
(defgeneric allocate-instance (class initlist))

;;; -----------------------------------------------------------------------------------
;;; INITIALIZE-INSTANCE                                      Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;;
;;; Purpose:   Meta Object Protocol: 
;;; Arguments: obj    is a standard object
;;;            initargs  is a plist
;;; Values:    The result is the object. 
;;; CLOS:      initargs is a &rest parameter in CLOS

;;; history: date       name    comment
;;;          06.12.90   jak     initialization of slots corrected

(defmethod initialize-instance ((obj object) &rest initargs)
  (let ((class (mcs%class-of obj)))
    
    ;; check initargs:
    (when *check-if-wrong-initargs*
      (doplist ((ia-name ia-value) initargs
                ((valid-initargs (%%class-initargs class))
                 (collected-initargs nil)))
        (if (mcs-memq ia-name collected-initargs)
          (error "Initarg ~S supplied twice: ~S." ia-name initargs)
          (push ia-name collected-initargs))
        (unless (mcs-memq ia-name valid-initargs)
          (error "Illegal initarg ~S for ~S." ia-name obj))))
    
    ;; initialize slots:
    (fill-slots obj (%%class-slots class) initargs)
;    (let ((position 0))
;      (dolist (slot (%%class-slots class))
;        (let ((val (getf initargs (%%slot-name slot) '%%initarg-not-found)))
;          (setf (mcs%obj-ref obj position)
;                (if (eq val '%%initarg-not-found)
;                  (let ((init-fn (%%slot-initfunction slot)))
;                    (if init-fn (funcall init-fn) '<unbound>))
;                  val)))
;        (incf position)))
    obj))

(defun fill-slots (obj slotds plist)
  (let ((position 0))
    (dolist (slot slotds)
      (let ((slot-name (%%slot-name slot)))
        (let ((val (getf plist slot-name '<unbound>)))
          (if (eq val '<unbound>)
            (let ((init-fn (%%slot-initfunction slot)))
              (if init-fn 
                (setf (mcs%obj-ref obj position)
                      (funcall init-fn))))
            (setf (mcs%obj-ref obj position) val)))
        (incf position)))))

;(defmethod initialize-instance ((obj object) initargs)
;  (let ((class (mcs%class-of obj))
;        (collected-initargs nil))
;    
;    ;; initialize from initargs:
;    (doplist ((sl-name sl-value) initargs
;              ((valid-initargs (%%class-initargs class))))
;      (if (mcs-memq sl-name collected-initargs)
;        (error "Initarg ~S supplied twice: ~S." sl-name initargs)
;        (push sl-name collected-initargs))
;      (if (mcs-memq sl-name valid-initargs)
;        (setf (slot-value obj sl-name) sl-value)
;        (when *warn-if-unknown-initarg*
;          (warn "Illegal initarg ~S for ~S." sl-name obj))))
;    
;    ;; initialize from initforms:
;    (let ((position 0))
;      (dolist (slot (%%class-slots class))
;        (let ((init-fn (%%slot-initfunction slot)))
;          (when (and init-fn (not (mcs-memq (%%slot-name slot)
;                                            collected-initargs)))
;            (setf (mcs%obj-ref obj position)
;                  (funcall init-fn))))
;        (incf position)))
;    obj))

(defmethod initialize-instance ((obj generic-function) &rest initargs)
  (declare (ignore initargs))
  (call-next-method)
  ;; initialize signature:
  (setf (%%gfn-signature obj)
        (let ((lambda-list (%%gfn-lambda-list obj)))
          (make-list (if (mcs-memq '&rest lambda-list)
                       (- (length lambda-list) 2)
                       (length lambda-list))
                     ':initial-element nil)))
  ;; initialize discriminating-function:
  (setf (%%gfn-discriminating-function obj)
        #'no-applicable-methods)
  obj)

;;; -----------------------------------------------------------------------------------
;;; ALLOCATE-INSTANCE                                        Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   Meta Object Protocol: 
;;; Arguments: class     is a standard class
;;;            initargs  is a plist
;;; Values:    The result is a new "uninitialized" instance of class. 
;;; CLOS:      initargs is a &rest parameter

;(defmethod allocate-instance ((class class) initargs)
;  (declare (ignore initargs))
;  (error "Can't allocate an instance of ~S." class))

(defmethod allocate-instance ((class instantiable) initargs)
  (declare (ignore initargs))
  (%make-empty-obj (%%class-wrapper class)
                   'error ;(%%class-slot-accessor class)
                   (%%class-slots-number class)))

;;; -----------------------------------------------------------------------------------
;;; MAKE-INSTANCE                                                   Bootstrap Function
;;; -----------------------------------------------------------------------------------
;;; Purpose:   Meta Object Protocol: 
;;; Arguments: class     is a class or a symbol
;;;            initargs  is a plist
;;; Values:    The result is a new initialized instance of class. 
;;; CLOS:      generic function

(setf (symbol-function 'make-instance)
      #'(lambda (class &rest initargs)
          (declare (optimize (speed 3) (safety 1)))
          (apply #'initialize-instance 
                 (allocate-instance
                        (if (symbolp class)
                          (find-class class 'signal-error)
                          class)
                        initargs)
                 initargs)))

;;; -----------------------------------------------------------------------------------
;;; MAKE-INSTANCE-FAST                                                           Macro
;;; -----------------------------------------------------------------------------------
;;; Purpose:   User interface
;;; Arguments: class-name or a form that evaluates to a class
;;;            initarg-name value
;;;            initarg-names have to be keywords
;;; Values:    The result is an instance 
;;; CLOS:      

(defmacro make-instance-fast (symbol-or-class &rest initarg-plist)
  (if (quoted-symbol-p symbol-or-class)
    (let ((class (find-class (second symbol-or-class)))) 
      (if (and class (constant-keywords-p initarg-plist))
        ;; class is known, keywords in initarg-plist are constant
        ;;  --> so you can optimize the instantiation
        (let ((legal-initargs (%%class-initargs class))
              (used-initargs nil) 
              (key-value-pairs nil)
              (local-init-exprs nil)
              (initargs-to-pass nil))
          (doplist ((quoted-ia val) initarg-plist (ia))
            (setq ia (second quoted-ia))
            (if (not (mcs-memq ia legal-initargs)) 
              (error "Illegal initarg: ~S for class ~S." ia class))
            (push ia used-initargs)
            (push `(,ia ,val) key-value-pairs)
            (setq initargs-to-pass `(,@initargs-to-pass ',ia ,ia)))
          (dolist (slot (reverse (%%class-slots class)))
            ; 'push' collects the init-exprs in the right order!
            (push (get-local-init slot used-initargs) local-init-exprs) )
          `(let ,key-value-pairs
             ,(let ((initialize-instance-calls
                     (compute-initialize-instance-calls class)))
                (if initialize-instance-calls
                  `(let ((initargs%%-list (list ,@initargs-to-pass))
                         (new%%-instance (%make-obj (%%class-wrapper (find-class ',(%%class-name class)))
                                                  ',(%%class-slot-accessor class)
                                                  ,@local-init-exprs)))
                     ,@initialize-instance-calls
                     new%%-instance)
                  `(%make-obj (%%class-wrapper (find-class ',(%%class-name class)))
                              ',(%%class-slot-accessor class)
                              ,@local-init-exprs))) ))
        ;; (second symbol-or-class) doesn't name a class, 
        ;; no class or keywords are not constant
        `(make-instance ,symbol-or-class ,@initarg-plist)))
    ;; bad case: symbol-or-class is a varibale or a form to evaluate
    `(make-instance ,symbol-or-class ,@initarg-plist) ))

(defun compute-initialize-instance-calls (class)
  ;; --> ((funcall 'after-fn-1 () new%%-instance initargs%%-list) ...)
  ;; legal if there are only :after initialize-instance methods
  (let ((methods (select-applicable-methods (find-gfn 'initialize-instance)
                                            (list class (find-class 't))))
        after-methods-calls)
    (dolist (method methods after-methods-calls)
      (cond
       ((equal (%%method-qualifiers method) '(:after))
        (push `(funcall ',(%%method-function method) new%%-instance initargs%%-list)
              after-methods-calls))
       ((equal (%%method-specializers method) (mapcar #'find-class '(object))) t)
       (t (return `((apply #'initialize-instance new%%-instance initargs%%-list))))) )))
             
(defun get-local-init (slot used-initargs)
  (declare (optimize (speed 3) (safety 0)))
  (let ((init-fn (%%slot-initfunction slot))
        (initarg (%%slot-name slot)))
    (if (mcs-memq initarg used-initargs)
      initarg
      (if init-fn
        (%%slot-initform slot)
        ''<unbound>) )))


;;; -----------------------------------------------------------------------------------
;;; %FIND-SLOT                                                                Function
;;; -----------------------------------------------------------------------------------

(defun %find-slot (class slot-name)
  (declare (optimize (speed 3) (safety 0)))
;  (cond
;   ((%typep class 'instantiable)
;    (nth (funcall (%%class-slot-accessor class) 
;                  'pseudo-obj slot-name '%slot-exists-p)
;         (%%class-slots class)))
;   ((%typep class 'single-inherited)
    (dolist (slot (%%class-slots class)
                  (error "No slot ~S in ~S." slot-name class))
      (if (eq slot-name (%%slot-name slot)) (return slot))))
;   (t ())))

;(defun %find-direct-slot (class slot-name)
;  (dolist (slot (%%class-direct-slots class)
;                (error "No slot ~S in ~S." slot-name class))
;    (if (eq slot-name (%%slot-name slot)) (return slot))))



;;; eof


