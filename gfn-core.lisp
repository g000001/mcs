;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the core of Methods and Generic Functions.
;;;
;;; notes:          
;;;
;;; contact:        Juergen Kopp, Harry Bretthauer
;;;
;;; history:
;;;          date:      author:             comments:
;;;          23.02.90   Harry Bretthauer    initial version
;;;          08.07.91   Juergen Kopp        CHECK-FOR-EXISTING-GFN modified
;;; -----------------------------------------------------------------------------------

(in-package "MCS")

;(export '(ensure-gfn add-method defmethod defgeneric))
  
;;; -----------------------------------------------------------------------------------
;;; DEFMETHOD                                                                    Macro
;;; -----------------------------------------------------------------------------------

#|
(defmacro defmethod (fn-specifier &rest arglist)
  (let ((global-gfn-name fn-specifier)
        defsetf-form qualifiers lambda-list specializer-names documentation body
        required-arguments all-arguments)
    ;; parse arglist: qualifier* spec-lambda-list form*
    (dolist (arg arglist) 
      (if (listp arg) (return nil))
      (if (legal-qualifier-p arg)
        (push (pop arglist) qualifiers)
        (error "Defmethod: illegal qualifier ~S for ~S." arg fn-specifier)))
    (setf qualifiers (reverse qualifiers))
    (setf lambda-list (extract-lambda-list (first arglist)))
    (setf specializer-names (extract-specializer-names (pop arglist)))
;    (check-specializers specializer-names)
    (when (stringp (first arglist)) (setf documentation (first arglist)))
    (setf body arglist)
    (cond
     ((mcs-memq '&rest lambda-list)
      (setf required-arguments (butlast lambda-list 2))
      (setf all-arguments (mcs-remq '&rest lambda-list)))
     (t (setf required-arguments lambda-list)
        (setf all-arguments lambda-list)))
    (when (setf-form-p fn-specifier)
      (setf global-gfn-name (make-setf-name fn-specifier))
      (setf defsetf-form (%compute-defsetf fn-specifier lambda-list)))
    ;; compute method
    `(progn
       ,defsetf-form
       (define-function ,global-gfn-name
         (let ((gfn-existed-before-p (and (find-gfn ',global-gfn-name)
                                          (fboundp ',global-gfn-name)))
               (%gfn (ensure-gfn ',global-gfn-name ',lambda-list 
                                (find-method-combination ',qualifiers))))
           
           (let ((method
                  (make-instance 
                   (or (%%gfn-method-class %gfn) (find-class 'method))
                   'name ',global-gfn-name
                   'lambda-list ',lambda-list
                   'specializers (mapcar #'(lambda (cl-name)
                                             (find-class cl-name 'signal-error))
                                         ',specializer-names)
                   'qualifiers ',qualifiers
                   'function 
                   (%set-anonymous-function-name
                    #',(make-method-lambda () 	;; method-prototype
                                                  global-gfn-name qualifiers 
                                                  lambda-list specializer-names
                                                  body)
                    ',(make-method-name fn-specifier qualifiers specializer-names)))))
             (add-method %gfn method)
             (record-source-file ',global-gfn-name 
                                 (list 'method ',qualifiers ',specializer-names))
             (if gfn-existed-before-p
               #',global-gfn-name
               (progn
                 ;(setf ,global-gfn-name %gfn)
                 (%set-anonymous-function-name
                  #'(lambda ,lambda-list
                   (declare ;(special ,global-gfn-name)
                            (optimize (speed 3) (safety 1)))
                   (let ((effective-methods
                          (funcall (%%gfn-discriminating-function %gfn) ;,global-gfn-name)
                                   %gfn    ;,global-gfn-name
                                   ,@required-arguments)))
                     (declare ;(special ,global-gfn-name)
                              (optimize (speed 3) (safety 0)))
                     (funcall (car effective-methods)
                              effective-methods
                              ,@all-arguments)))
                  ',global-gfn-name))) )))) ))


(defmacro defmethod (fn-specifier &rest arglist)
  (let ((global-gfn-name fn-specifier)
        defsetf-form qualifiers lambda-list specializer-names documentation body
        required-arguments all-arguments)
    ;; parse arglist: qualifier* spec-lambda-list form*
    (dolist (arg arglist) 
      (if (listp arg) (return nil))
      (if (legal-qualifier-p arg)
        (push (pop arglist) qualifiers)
        (error "Defmethod: illegal qualifier ~S for ~S." arg fn-specifier)))
    (setf qualifiers (reverse qualifiers))
    (setf lambda-list (extract-lambda-list (first arglist)))
    (setf specializer-names (extract-specializer-names (pop arglist)))
;    (check-specializers specializer-names)
    (when (stringp (first arglist)) (setf documentation (first arglist)))
    (setf body arglist)
    (cond
     ((mcs-memq '&rest lambda-list)
      (setf required-arguments (butlast lambda-list 2))
      (setf all-arguments (mcs-remq '&rest lambda-list)))
     (t (setf required-arguments lambda-list)
        (setf all-arguments lambda-list)))
    (when (setf-form-p fn-specifier)
      (setf global-gfn-name (make-setf-name fn-specifier))
      (setf defsetf-form (%compute-defsetf fn-specifier lambda-list)))
    ;; compute method
    `(progn
       ,defsetf-form
       (setf (symbol-function '*tmp-trampoline*)
	   #'(lambda ()
       (define-function ,global-gfn-name
         (let ((gfn-existed-before-p (and (find-gfn ',global-gfn-name)
                                          (fboundp ',global-gfn-name)))
               (%gfn (ensure-gfn ',global-gfn-name ',lambda-list 
                                (find-method-combination ',qualifiers))))
           
           (let ((method
                  (make-instance 
                   (or (%%gfn-method-class %gfn) (find-class 'method))
                   'name ',global-gfn-name
                   'lambda-list ',lambda-list
                   'specializers (mapcar #'(lambda (cl-name)
                                             (find-class cl-name 'signal-error))
                                         ',specializer-names)
                   'qualifiers ',qualifiers
                   'function 
                   (%set-anonymous-function-name
                    #',(make-method-lambda () 	;; method-prototype
                                                  global-gfn-name qualifiers 
                                                  lambda-list specializer-names
                                                  body)
                    ',(make-method-name fn-specifier qualifiers specializer-names)))))
             (add-method %gfn method)
             (record-source-file ',global-gfn-name 
                                 (list 'method ',qualifiers ',specializer-names))
             (if gfn-existed-before-p
               #',global-gfn-name
               (progn
                 ;(setf ,global-gfn-name %gfn)
                 (%set-anonymous-function-name
                  #'(lambda ,lambda-list
                   (declare ;(special ,global-gfn-name)
                            (optimize (speed 3) (safety 1)))
                   (let ((effective-methods
                          (funcall (%%gfn-discriminating-function %gfn)
                                   %gfn  
                                   ,@required-arguments)))
                     (declare ;(special ,global-gfn-name)
                              (optimize (speed 3) (safety 0)))
                     (funcall (car effective-methods)
                              effective-methods
                              ,@all-arguments)))
                  ',global-gfn-name))))))))
       (*tmp-trampoline*))))
|#

(defmacro defmethod (fn-specifier &rest arglist)
  (let ((global-gfn-name fn-specifier)
        defsetf-form qualifiers lambda-list specializer-names documentation body
        required-arguments all-arguments)
    ;; parse arglist: qualifier* spec-lambda-list form*
    (dolist (arg arglist) 
      (if (listp arg) (return nil))
      (if (legal-qualifier-p arg)
        (push (pop arglist) qualifiers)
        (error "Defmethod: illegal qualifier ~S for ~S." arg fn-specifier)))
    (setf qualifiers (reverse qualifiers))
    (setf lambda-list (extract-lambda-list (first arglist)))
    (setf specializer-names (extract-specializer-names (pop arglist)))
    ;    (check-specializers specializer-names)
    (when (stringp (first arglist)) (setf documentation (first arglist)))
    (setf body arglist)
    (cond
     ((mcs-memq '&rest lambda-list)
      (setf required-arguments (butlast lambda-list 2))
      (setf all-arguments (mcs-remq '&rest lambda-list)))
     (t (setf required-arguments lambda-list)
        (setf all-arguments lambda-list)))
    (when (setf-form-p fn-specifier)
      (setf global-gfn-name (make-setf-name fn-specifier))
      (setf defsetf-form (%compute-defsetf fn-specifier lambda-list)))
    ;; compute method
    `(progn
       ,defsetf-form
       (setf (symbol-function '*tmp-trampoline*)
	     #'(lambda ()
                 (let ((gfn-existed-before-p (and (find-gfn ',global-gfn-name)
                                                  (fboundp ',global-gfn-name)))
                       (%gfn (ensure-gfn ',global-gfn-name ',lambda-list 
                                         (find-method-combination ',qualifiers))))
                   
                   (let ((method
                          (make-instance 
                            (or (%%gfn-method-class %gfn) (find-class 'method))
                            'name ',global-gfn-name
                            'lambda-list ',lambda-list
                            'specializers (mapcar #'(lambda (cl-name)
                                                      (find-class cl-name 'signal-error))
                                                  ',specializer-names)
                            'qualifiers ',qualifiers
                            'function 
                            (%set-anonymous-function-name
                             #',(make-method-lambda () 	;; method-prototype
                                                    global-gfn-name qualifiers 
                                                    lambda-list specializer-names
                                                    body)
                             ',(make-method-name fn-specifier qualifiers specializer-names)))))
                     (add-method %gfn method)
                     (record-source-file ',global-gfn-name 
                                         (list 'method ',qualifiers ',specializer-names))
                     (unless gfn-existed-before-p
                       (defun ,global-gfn-name ,lambda-list
                         (declare (optimize (speed 3) (safety 1)))
                         (let ((effective-methods
                                (funcall (%%gfn-discriminating-function %gfn)
                                         %gfn  
                                         ,@required-arguments)))
                           (declare (optimize (speed 3) (safety 0)))
                           (funcall (car effective-methods)
                                    effective-methods
                                    ,@all-arguments)))) ))))
       (*tmp-trampoline*)
       #',global-gfn-name)))

;;; -----------------------------------------------------------------------------------
;;; DEFGENERIC                                                                   Macro
;;; -----------------------------------------------------------------------------------


#|
(defmacro defgeneric (fn-specifier lambda-list &rest options)
  (let ((gfn-name fn-specifier)
        (gfn-class-name (or (second (assoc ':class options :test #'eq))
                            'generic-function))
        (method-class-name (or (second (assoc ':method-class options :test #'eq))
                               'method))
        (documentation (second (assoc ':documentation options :test #'eq)))
        (method-combination-name
         (or (second (assoc ':method-combination options :test #'eq))
             'standard))
        (defsetf-form ()) 
        (required-arguments lambda-list)
        (all-arguments lambda-list))
    (syntax-defgeneric gfn-name lambda-list method-combination-name options)
    (when (setf-form-p fn-specifier)
      (setf gfn-name (make-setf-name fn-specifier))
      (setf defsetf-form (%compute-defsetf fn-specifier lambda-list)))
    (when (mcs-memq '&rest lambda-list)
      (setf required-arguments (butlast lambda-list 2))
      (setf all-arguments (mcs-remq '&rest lambda-list)))
    `(progn
       ;(defvar ,gfn-name)
       ,defsetf-form
       (define-function ,gfn-name
         (let ((gfn-existed-before-p (and (find-gfn ',gfn-name)
                                          (fboundp ',gfn-name)))
               (%gfn 
                ;; Functional Interface
                (ensure-gfn ',gfn-name ',lambda-list 
                            (find-method-combination ',method-combination-name)
                            'class (find-class ',gfn-class-name 'signal-error)
                            'method-class (find-class ',method-class-name 'signal-error))))
           ;; Programming Environment Stuff
;           (record-source-file ',gfn-name 'generic-function)
           ,(when documentation
              `(pe-store 'generic-function-documentation 
                         ',gfn-name
                         ',documentation))
           (if gfn-existed-before-p
             #',gfn-name
             (progn
               ;(setf ,gfn-name %gfn)
               (%set-anonymous-function-name
             #'(lambda ,lambda-list
             (declare ;(special ,gfn-name)
                      (optimize (speed 3) (safety 1)))
             (let ((effective-methods
                    (funcall (%%gfn-discriminating-function %gfn) ;,gfn-name)
                             %gfn ;,gfn-name
                             ,@required-arguments)))
               (declare ;(special ,gfn-name)
                        (optimize (speed 3) (safety 0)))
               (funcall (car effective-methods)
                        effective-methods
                        ,@all-arguments)))
             ',gfn-name)))))) ))



(defmacro defgeneric (fn-specifier lambda-list &rest options)
  (let ((gfn-name fn-specifier)
        (gfn-class-name (or (second (assoc ':class options :test #'eq))
                            'generic-function))
        (method-class-name (or (second (assoc ':method-class options :test #'eq))
                               'method))
        (documentation (second (assoc ':documentation options :test #'eq)))
        (method-combination-name
         (or (second (assoc ':method-combination options :test #'eq))
             'standard))
        (defsetf-form ()) 
        (required-arguments lambda-list)
        (all-arguments lambda-list))
    (syntax-defgeneric gfn-name lambda-list method-combination-name options)
    (when (setf-form-p fn-specifier)
      (setf gfn-name (make-setf-name fn-specifier))
      (setf defsetf-form (%compute-defsetf fn-specifier lambda-list)))
    (when (mcs-memq '&rest lambda-list)
      (setf required-arguments (butlast lambda-list 2))
      (setf all-arguments (mcs-remq '&rest lambda-list)))
    `(progn
       ;(defvar ,gfn-name)
       ,defsetf-form
       (setf (symbol-function '*tmp-trampoline*)
	   #'(lambda ()
       (define-function ,gfn-name
         (let ((gfn-existed-before-p (and (find-gfn ',gfn-name)
                                          (fboundp ',gfn-name)))
               (%gfn 
                ;; Functional Interface
                (ensure-gfn ',gfn-name ',lambda-list 
                            (find-method-combination ',method-combination-name)
                            'class (find-class ',gfn-class-name 'signal-error)
                            'method-class (find-class ',method-class-name 'signal-error))))
           ;; Programming Environment Stuff
;           (record-source-file ',gfn-name 'generic-function)
           ,(when documentation
              `(pe-store 'generic-function-documentation 
                         ',gfn-name
                         ',documentation))
           (if gfn-existed-before-p
             #',gfn-name
             (progn
               ;(setf ,gfn-name %gfn)
               (%set-anonymous-function-name
             #'(lambda ,lambda-list
             (declare ;(special ,gfn-name)
                      (optimize (speed 3) (safety 1)))
             (let ((effective-methods
                    (funcall (%%gfn-discriminating-function %gfn)
                             %gfn
                             ,@required-arguments)))
               (declare ;(special ,gfn-name)
                        (optimize (speed 3) (safety 0)))
               (funcall (car effective-methods)
                        effective-methods
                        ,@all-arguments)))
             ',gfn-name)))))))
	   (*tmp-trampoline*))))
|#

(defmacro defgeneric (fn-specifier lambda-list &rest options)
  (let ((gfn-name fn-specifier)
        (gfn-class-name (or (second (assoc ':class options :test #'eq))
                            'generic-function))
        (method-class-name (or (second (assoc ':method-class options :test #'eq))
                               'method))
        (documentation (second (assoc ':documentation options :test #'eq)))
        (method-combination-name
         (or (second (assoc ':method-combination options :test #'eq))
             'standard))
        (defsetf-form ()) 
        (required-arguments lambda-list)
        (all-arguments lambda-list))
    (syntax-defgeneric gfn-name lambda-list method-combination-name options)
    (when (setf-form-p fn-specifier)
      (setf gfn-name (make-setf-name fn-specifier))
      (setf defsetf-form (%compute-defsetf fn-specifier lambda-list)))
    (when (mcs-memq '&rest lambda-list)
      (setf required-arguments (butlast lambda-list 2))
      (setf all-arguments (mcs-remq '&rest lambda-list)))
    `(progn
       ;(defvar ,gfn-name)
       ,defsetf-form
       (setf (symbol-function '*tmp-trampoline*)
	     #'(lambda ()
                 (let ((gfn-existed-before-p (and (find-gfn ',gfn-name)
                                                  (fboundp ',gfn-name)))
                       (%gfn 
                        ;; Functional Interface
                        (ensure-gfn ',gfn-name ',lambda-list 
                                    (find-method-combination ',method-combination-name)
                                    'class (find-class ',gfn-class-name 'signal-error)
                                    'method-class (find-class ',method-class-name 'signal-error))))
                   ;; Programming Environment Stuff
                   ;           (record-source-file ',gfn-name 'generic-function)
                   ,(when documentation
                      `(pe-store 'generic-function-documentation 
                                 ',gfn-name
                                 ',documentation))
                   (unless gfn-existed-before-p
                     (defun ,gfn-name ,lambda-list
                       (declare ;(special ,gfn-name)
                        (optimize (speed 3) (safety 1)))
                       (let ((effective-methods
                              (funcall (%%gfn-discriminating-function %gfn)
                                       %gfn
                                       ,@required-arguments)))
                         (declare ;(special ,gfn-name)
                          (optimize (speed 3) (safety 0)))
                         (funcall (car effective-methods)
                                  effective-methods
                                  ,@all-arguments)))))))
       (*tmp-trampoline*)
       #',gfn-name)))

;;; -----------------------------------------------------------------------------------
;;; Help Functions
;;; -----------------------------------------------------------------------------------

(defun syntax-defgeneric (fn-specifier lambda-list method-combination-name options)
  (declare (optimize (speed 3) (safety 0))
           (ignore options method-combination-name fn-specifier))
  ;; check lambda-list
  (detect-cl-stuff lambda-list))

(defun detect-cl-stuff (lambda-list)
  (cond
   ((mcs-memq '&optional lambda-list) 
    (error "&optional is not legal in the lambda-list of a generic function or method."))
   ((mcs-memq '&key lambda-list) 
    (error "&key is not legal in the lambda-list of a generic function or method."))
   ((mcs-memq '&aux lambda-list) 
    (error "&aux is not legal in the lambda-list of a generic function or method."))
   (t t)))
     
(defun extract-lambda-list (spec-lambda-list)
  (detect-cl-stuff spec-lambda-list)
  (mapcar #'(lambda (arg) 
                      (if (consp arg) (car arg) arg))
          spec-lambda-list))

(defun extract-specializer-names (specialized-lambda-list)
  (declare (optimize (speed 3) (safety 0)))
  (mapcar #'(lambda (arg) 
                      (if (consp arg) 
                        (second arg)
                        'T))
          ;; &rest aus der specialized-lambda-list loeschen
          (if (mcs-memq '&rest specialized-lambda-list)
            (butlast specialized-lambda-list 2)
            specialized-lambda-list)))

(defun check-specializers (specializers)
  (dolist (class-name specializers t)
    (unless (find-class class-name)
      (error "Defmethod: no specializer named  ~S." class-name))))

;;; -----------------------------------------------------------------------------------
;;; COMPUTE-DISCRIMINATING-FN    
;;; -----------------------------------------------------------------------------------
;;; Computing the discriminator of a generic function 

;(defun compute-discriminating-fn (gfn)         
;  "Compute the discriminating function for a generic
;           function object and sets the associated 
;           function cell."
;  (declare (optimize (speed 3) (safety 0)))
;  (let ((lambda-list (%%gfn-lambda-list gfn))
;        (combined-methods (%%gfn-combined-methods gfn))
;        (type-checkers (get-type-checkers gfn)))
;    (let ((discriminating-fn
;           `(lambda ,(remove '&rest lambda-list)
;              (declare (optimize (speed 3) (safety 0))
;                       (inline funcall))
;              ,(if (mcs-memq '&rest lambda-list)
;                   (compute-case-expr combined-methods
;                                      (%%gfn-signature gfn)
;                                      (without-rest-parameter lambda-list)
;                                      (without-rest-parameter lambda-list)
;                                      (without-rest-keyword lambda-list)
;                                      gfn type-checkers)
;                   (compute-case-expr combined-methods
;                                      (%%gfn-signature gfn)
;                                      lambda-list lambda-list lambda-list 
;                                      gfn type-checkers) ) )))
;;      (setf (symbol-function (%%gfn-discriminating-function gfn))
;;            discriminating-fn)
;      (compile (%%gfn-discriminating-function gfn) discriminating-fn)
;      )))

(defun compute-discriminating-fn (gfn)
  (declare (optimize (speed 3) (safety 0)))
  (let ((methods (%%gfn-methods gfn))
        (signature (%%gfn-signature gfn)))
    (setf (%%gfn-discriminating-function gfn)
          (cond
           ((null methods) #'no-applicable-methods)
           ((equal signature '(t)) #'darg1-discriminator)
           ((equal signature '(() t)) #'arg1-darg2-discriminator)
           ((equal signature '(t ())) #'darg1-arg2-discriminator)
           ((equal signature '(t () ())) #'darg1-arg2-arg3-discriminator)
           ((equal signature '(t () () ())) #'darg1-arg2-arg3-arg4-discriminator)
           ; ((equal?? signature '(t . ())) #'darg1-args-discriminator)
           ((equal signature '(t t)) #'darg1-darg2-discriminator)
           ((equal signature '(t t ())) #'darg1-darg2-arg3-discriminator)
           ((equal signature '(t t () ())) #'darg1-darg2-arg3-arg4-discriminator)
           ; ((equal signature '(t t . ())) #'darg1-darg2-args-discriminator)
           ((equal signature '(())) #'arg1-discriminator)

           (t (compile () (compute-discriminating-lambda gfn methods)))))))

(setf (symbol-function 'no-applicable-methods) 
      #'(lambda (gfn &rest arguments)
          (error "No applicable methods in ~S for ~S." gfn arguments)))

(defun arg1-discriminator (gfn arg1)
  (declare (optimize (speed 3) (safety 0)))
  (or (cdr (mcs-assq () (%%gfn-combined-methods gfn)))
      (dynamic-lookup gfn (list arg1))))

(defun darg1-discriminator (gfn darg1)
  (declare (optimize (speed 3) (safety 0)))
  (or (cdr (mcs-assq (%class-of darg1)
                     (%%gfn-combined-methods gfn)))
      (dynamic-lookup gfn (list darg1))))

(defun arg1-darg2-discriminator (gfn arg1 darg2)
  (declare (optimize (speed 3) (safety 0)))
  (or (cdr (mcs-assq (%class-of darg2)
                     (%%gfn-combined-methods gfn)))
      (dynamic-lookup gfn (list arg1 darg2))))

(defun darg1-arg2-discriminator (gfn darg1 arg2)
  (declare (optimize (speed 3) (safety 0)))
  (or (cdr (mcs-assq (%class-of darg1)
                     (%%gfn-combined-methods gfn)))
      (dynamic-lookup gfn (list darg1 arg2))))

(defun darg1-arg2-arg3-discriminator (gfn darg1 arg2 arg3)
  (declare (optimize (speed 3) (safety 0)))
  (or (cdr (mcs-assq (%class-of darg1)
                     (%%gfn-combined-methods gfn)))
      (dynamic-lookup gfn (list darg1 arg2 arg3))))

(defun darg1-arg2-arg3-arg4-discriminator (gfn darg1 arg2 arg3 arg4)
  (declare (optimize (speed 3) (safety 0)))
  (or (cdr (mcs-assq (%class-of darg1)
                     (%%gfn-combined-methods gfn)))
      (dynamic-lookup gfn (list darg1 arg2 arg3 arg4))))

(defun darg1-args-discriminator (gfn darg1 &rest args)
  (declare (optimize (speed 3) (safety 0)))
  (or (cdr (mcs-assq (%class-of darg1)
                     (%%gfn-combined-methods gfn)))
      (dynamic-lookup gfn (cons darg1 args))))

(defun darg1-darg2-discriminator (gfn darg1 darg2)
  (declare (optimize (speed 3) (safety 0)))
  (or (cdr (mcs-assq (%class-of darg2)
                     (cdr (mcs-assq (%class-of darg1)
                                    (%%gfn-combined-methods gfn)))))
      (dynamic-lookup gfn (list darg1 darg2))))
  
(defun darg1-darg2-arg3-discriminator (gfn darg1 darg2 arg3)
  (declare (optimize (speed 3) (safety 0)))
  (or (cdr (mcs-assq (%class-of darg2)
                     (cdr (mcs-assq (%class-of darg1)
                                    (%%gfn-combined-methods gfn)))))
      (dynamic-lookup gfn (list darg1 darg2 arg3))))
  
(defun darg1-darg2-arg3-arg4-discriminator (gfn darg1 darg2 arg3 arg4)
  (declare (optimize (speed 3) (safety 0)))
  (or (cdr (mcs-assq (%class-of darg2)
                     (cdr (mcs-assq (%class-of darg1)
                                    (%%gfn-combined-methods gfn)))))
      (dynamic-lookup gfn (list darg1 darg2 arg3 arg4))))

(defun darg1-darg2-args-discriminator (gfn darg1 darg2 &rest args)
  (declare (optimize (speed 3) (safety 0)))
  (or (cdr (mcs-assq (%class-of darg2)
                     (cdr (mcs-assq (%class-of darg1)
                                    (%%gfn-combined-methods gfn)))))
      (dynamic-lookup gfn (cons darg1 (cons darg2 args)))))

(defun optimize-discriminating-function (gfn)
  (setf (%%gfn-discriminating-function gfn)
        (compile nil
                 (compute-discriminating-lambda gfn (%%gfn-methods gfn)))))

(defun compute-discriminating-lambda (gfn methods)
  ;(declare (ignore methods))
  (let ((lambda-list (%%gfn-lambda-list gfn))
        (signature (%%gfn-signature gfn))
        (class-checkers (get-type-checkers gfn)))
    (let ((all-arguments lambda-list)
          (required-arguments lambda-list))
      (when (mcs-memq '&rest lambda-list)
        (setf all-arguments (mcs-remq '&rest lambda-list))
        (setf required-arguments (butlast lambda-list 2)))
      (cond
;       ((null methods)
;        `(lambda (%gfn ,@lambda-list)
;           (declare (optimize (speed 3) (safety 1)))
;           (no-applicable-methods %gfn ,@required-arguments)))
       ((null (cdr methods))
        `(lambda (%gfn ,@required-arguments)
           (declare (optimize (speed 3) (safety 0)))
             (if (and 
                  ,@(let ((specializers (%%method-specializers (car methods)))
                          (expr ()))
                      (dolist (cl signature)
                        (if cl
                          (setf expr (cons `(%subinstance-p
                                             ;(,(pop class-checkers)
                                              ,(pop lambda-list)  ;)
                                             ',(pop specializers))
                                           expr))
                          (progn (pop class-checkers) 
                                 (pop lambda-list)
                                 (pop specializers))))
                      (reverse expr))
                  t)
               ',(list (%%method-function (car methods)))
               (no-applicable-methods %gfn ,@required-arguments))))
       (t `(lambda (%gfn ,@required-arguments)
             (declare (optimize (speed 3) (safety 0)))
             (or ,(let ((expr `(%%gfn-combined-methods %gfn)))
                           (dolist (cl signature)
                             (if cl
                               (setf expr `(cdr (mcs-assq (,(pop class-checkers)
                                                           ,(pop lambda-list))
                                                          ,expr)))
                               (progn (pop class-checkers) (pop lambda-list))))
                           expr)
                        (dynamic-lookup %gfn (list ,@required-arguments)))))) )))

       
(defun get-type-checkers (gfn)
  (let ((signature (%%gfn-signature gfn))
        (pos 0)
        result)
      (dolist (cl signature)
        (push (if cl
                '%discr-key
                nil)
              result)
        (incf pos))
      (reverse result)))


;;; candidate to become a macro
(defun without-rest-parameter (lambda-list-with-rest)
  (declare (optimize (speed 3) (safety 0)))
  (butlast (butlast lambda-list-with-rest)))

;;; candidate to become a macro
(defun without-rest-keyword (lambda-list-with-rest)
  (declare (optimize (speed 3) (safety 0)))
  (mcs-remq '&rest lambda-list-with-rest))

;;; -----------------------------------------------------------------------------------
;;; COMPUTE-CASE-EXPR
;;; -----------------------------------------------------------------------------------

(defun compute-case-expr (a-list signature spec-params all-spec-params all-params 
                                 gfn type-ckecker-names)
  (declare (optimize (speed 3) (safety 0)))
  (if a-list
    ;; there are combined-methods
    (if (null (rest spec-params))
      ;; no recursive call any more
      (if (eq (first signature) (find-class 'T))
        ;; without discrimination 
        `(funcall ',(first (rest (first a-list))) ',(rest (rest (first a-list)))
                  ,@all-params)
        ;; with discrimination
        `(case (,(first type-ckecker-names) ,(first spec-params))
           ,@(mapcar #'(lambda (pair)
                                 `(,(first pair) (funcall ',(first (rest pair))
                                                          ',(rest (rest pair))
                                                          ,@all-params)))
                     a-list)
           (t (dynamic-lookup ',gfn (list ,@all-spec-params) (list ,@all-params)))
           ) )
      ;; with recursive call
      (if (eq (first signature) (find-class 'T))
        ;; without discrimination 
        (compute-case-expr (rest (first a-list)) (rest signature) (rest spec-params) 
                           all-spec-params all-params gfn (rest type-ckecker-names))
        ;; with discrimination    
        `(case (,(first type-ckecker-names) ,(first spec-params))
           ,@(mapcar #'(lambda (pair)
                                 `(,(first pair)
                                   ,(compute-case-expr 
                                     (rest pair) (rest signature)  (rest spec-params)
                                     all-spec-params all-params 
                                     gfn (rest type-ckecker-names))))
                     a-list)
           (t (dynamic-lookup ',gfn  (list ,@all-spec-params) (list ,@all-params)))
           )))
    ;; no case at all
    `(dynamic-lookup ',gfn  (list ,@all-spec-params) (list ,@all-params)) ))

(defun compute-case-lambda (a-list signature spec-params all-spec-params 
                                   gfn type-ckecker-names)
  (declare (optimize (speed 3) (safety 0)))
  (if a-list
    ;; there are combined-methods
    (if (null (rest spec-params))
      ;; no recursive call any more
      (if (first signature)
        ;; with discrimination
        `(case (,(first type-ckecker-names) ,(first spec-params))
           ,@(mapcar #'(lambda (pair)
                                 `(,(first pair) ',(rest pair)))
                     a-list)
           (t (dynamic-lookup ',gfn (list ,@all-spec-params))) )
        ;; without discrimination 
        `'(,(first a-list)))
      ;; with recursive call
      (if (first signature)
        ;; with discrimination    
        `(case (,(first type-ckecker-names) ,(first spec-params))
           ,@(mapcar #'(lambda (pair)
                                 `(,(first pair)
                                   ,(compute-case-lambda 
                                     (rest pair) (rest signature)  (rest spec-params)
                                     all-spec-params 
                                     gfn (rest type-ckecker-names))))
                     a-list)
           (t (dynamic-lookup ',gfn  (list ,@all-spec-params))))
        ;; without discrimination 
        (compute-case-lambda a-list (rest signature) (rest spec-params) 
                             all-spec-params gfn (rest type-ckecker-names))))
    ;; no case at all
    `(dynamic-lookup ',gfn  (list ,@all-spec-params))))


;;; -----------------------------------------------------------------------------------
;;; ENSURE-GFN
;;; -----------------------------------------------------------------------------------
;;; External constructor of generic functions

(defun ensure-gfn (fn-specifier lambda-list method-combination &rest plist)
  (declare (optimize (speed 3) (safety 3))
           (ignore gfn-class))
  (let ((global-gfn-name (cond
                          ((symbolp fn-specifier) fn-specifier)
                          ((setf-form-p fn-specifier)
                           (%install-defsetf fn-specifier lambda-list)
                           (make-setf-name fn-specifier))
                          (t (error "Wrong function-speciafier: ~S." fn-specifier))))
        (gfn ()))
  (when (fboundp global-gfn-name)
    (setf gfn (check-for-existing-gfn global-gfn-name lambda-list plist)))
  (if gfn
    gfn
    ;; create a new generic function
    (let ((gfn-class (find-class 'generic-function))
          (method-class (find-class 'method))
          ;(discriminator-name (make-discriminator-name global-gfn-name))
          )
      (doplist ((key val) plist)
        (case key
          (method-class (setf method-class val))
          ;(discriminator-name (setf discriminator-name val))
          (class (setf gfn-class val))
          (t (error "Illegal initarg ~S for function ENSURE-GFN." key))))
      ;(bind-gfn-globaly global-gfn-name lambda-list discriminator-name)
      (insert-gfn global-gfn-name 
                  (make-instance gfn-class 
                                 'name               global-gfn-name
                                 'lambda-list        lambda-list 
                                 'method-class       method-class 
                                 'method-combination method-combination
                                 ;'discriminating-function  discriminator-name
                                 ))))))

(defun make-gfn-function (gfn-name %gfn)
  (setf (symbol-function gfn-name) 
        (%set-anonymous-function-name
         #'(lambda (&rest all-arguments)
             (declare (optimize (speed 3) (safety 1)))
             (let ((effective-methods
                    (apply (%%gfn-discriminating-function %gfn)
                           %gfn
                           all-arguments)))
               (declare (optimize (speed 3) (safety 0)))
               (apply (car effective-methods)
                      effective-methods
                      all-arguments)))
         gfn-name)))

       
;(defun bind-gfn-globaly (fn-specifier lambda-list &rest options)
;  (let ((discriminator-name (or (first options)
;                                (make-discriminator-name fn-specifier))))
;    (compile fn-specifier 
;               `(lambda ,lambda-list
;                  (declare (optimize (speed 3) (safety ,*gfn-safety*)))
;                  (,discriminator-name ,@(remove '&rest lambda-list))))))

;;; -----------------------------------------------------------------------------------
;;; CHECK-FOR-EXISTING-GFN                                          Bootstrap Function
;;; -----------------------------------------------------------------------------------

;; *jak* 08.07.91

(setf (symbol-function 'check-for-existing-gfn)
      #'(lambda (fn-specifier lambda-list plist)
          (declare (optimize (speed 3) (safety 0))
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
              ()))))

;;; -----------------------------------------------------------------------------------
;;; MAKE-METHOD-LAMBDA
;;; -----------------------------------------------------------------------------------
  ;; used in DEFMETHOD
  ;; for standard-generic-function + standard-combination.
  ;; the only allowed lambda-list keyword is  &rest.
  ;; the effective lambda-list of method-function has no &rest parameter.
  ;; If the user declared a &rest parameter, the actual values binded to it are lists,
  ;; because the effective lambda-list of discriminating-fn has a &rest
  ;; parameter.

(defun make-method-lambda (method gfn-name qualifiers lambda-list specializer-names
                                  body)
  (declare (optimize (speed 3) (safety 0))
           (ignore method))
  (let ((effective-lambda-list (mcs-remq '&rest lambda-list))
        (declarations `((declare (optimize (speed 3) (safety ,*method-safety*))))))
    (dolist (form body)
      (cond 
       ((and (consp form) (eq (car form) 'declare))
        (push form declarations)
        (setf body (remove form body :test #'eq)))
       ((not (stringp form)) (return nil))
       (t nil)))
    (if (mcs-memq (first qualifiers) '(:before :after))
      
      `(lambda ,effective-lambda-list
         ,@declarations
         ,@body)
      (let ((real-lambda-list
             (mapcar #'(lambda (arg) (%make-name '%true- arg))
                     effective-lambda-list)))
        `(lambda (%next-fns ,@real-lambda-list)
           %next-fns
           ((lambda ,effective-lambda-list
              ,@declarations
              (macrolet  ((call-next-method ()
                            ',(compute-call-next-method-substitution
                               gfn-name real-lambda-list 
                               qualifiers specializer-names)))
                ,@body))
            ,@real-lambda-list))))))

(defun compute-call-next-method-substitution (gfn-name real-lambda-list 
                                              qualifiers specializer-names)
  `(if (cdr %next-fns)
     (funcall (cadr %next-fns)
              (cdr %next-fns)
              ,@real-lambda-list)
     (let ((%gfn (find-gfn ',gfn-name)))
       (no-next-method
        %gfn
        (find-method %gfn 
                     ',qualifiers
                     (mapcar #'find-class
                             ',specializer-names))
        ,@real-lambda-list)) ))

;;; -----------------------------------------------------------------------------------
;;; ADD-METHOD
;;; -----------------------------------------------------------------------------------

(defun add-method (gfn method)
  "Adds method-object to gfn-object and to associated
           class-objects;
           removes invalid combined methods."
  (declare (optimize (speed 3) (safety 0)))
  ;  (if (%typep method (%%gfn-method-class gfn))
  (let ((specializers (%%method-specializers method))
        (qualifiers (%%method-qualifiers method))
        (old-methods (%%gfn-methods gfn)))
    (dolist (specializer specializers)
      (add-direct-method specializer method))
    (setf (%%gfn-methods gfn)
          (cons method
                (remove method 
                        old-methods
                        :test #'(lambda (x y)
                                  (declare (ignore x))
                                  (and (equal specializers (%%method-specializers y))
                                       (equal qualifiers (%%method-qualifiers y)))))))
    (update-discriminating-function gfn method old-methods))
  gfn)

(defun update-discriminating-function (gfn new-method old-methods)
  (declare (optimize (speed 3) (safety 0)))
  (let ((signature-change-p ())
        (old-signature (%%gfn-signature gfn))
        
        (method-signature (%%method-specializers new-method)))
    (let ((new-signature (mapcar #'(lambda (gfn-sign meth-sign)
                                     (if (not gfn-sign)
                                       (if (eq meth-sign (find-class 't))
                                         ;; no signature change:
                                         gfn-sign
                                         ;; signature has to be changed:
                                         (setf signature-change-p 't))
                                       gfn-sign))
                                 old-signature method-signature) ))
      (cond
       (signature-change-p
        (setf (%%gfn-signature gfn) new-signature)
        (setf (%%gfn-combined-methods gfn) ())
        (compute-discriminating-fn gfn))
       ((null old-methods)
        ;; the signature could remain unchanged:
        (compute-discriminating-fn gfn))
       ((= (length old-methods) 1)
        ;; old special one-method discriminating function is invalid:
        (compute-discriminating-fn gfn)
        (remove-invalid-combined-methods
          gfn (relevant-method-specializers old-signature method-signature)))
        (t
         ; the old discriminating function is still valid
         ; but some combined methods may become invalid:
         (remove-invalid-combined-methods
          gfn (relevant-method-specializers old-signature method-signature)))))))

;;; -----------------------------------------------------------------------------------
;;; ADD-COMBINED-METHOD
;;; -----------------------------------------------------------------------------------

(defun make-entry-in-alist2 (specializers function)
  (declare (optimize (speed 3) (safety 0)))
  (let ((rev-spec (reverse specializers)) result)
    (setq result (cons (first rev-spec)
                       function)
          rev-spec (rest rev-spec))
    (dolist (key rev-spec result)
      (setq result (cons key (list result))))))

(defun ADD-COMBINED-METHOD (gfn-object specializers function)
  (declare (optimize (speed 3) (safety 0)))
  (let ((alist-to-modify
         (let ((result (cons t (%%gfn-combined-methods gfn-object)))
               ;cons for iteration
               alist key)
           (loop
             (setq alist (rest result)
                   key   (pop specializers))
             (setq result (assoc key alist))
             (cond ((null result)
                    (push key specializers) ; rest of specializers used below!
                    (return alist))
                   ((null (second specializers))
                    ; last specializer examined and result not nil!
                    (return result))))
           )))
    (cond ((null specializers) ; modify combined method
           (setf (rest alist-to-modify)
                   function))
          (alist-to-modify     ; modify alist
           (setf (rest alist-to-modify)
                   (cons 
                    (make-entry-in-alist2 specializers function)
                    (rest alist-to-modify))))
          (t                   ; gfn has not yet any methods
           (setf (%%gfn-combined-methods gfn-object)
                 (list (make-entry-in-alist2 specializers function)))) )
    function))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------
;;; **** Sollte wohl lieber remove-entry heissen *****

(defun delete-entry (alist specializers)
  (declare (optimize (speed 3) (safety 0)))
  ;; alist contains firstly the element to compare
  ;; ((c1 (c2 ..)
  ;;      (c3 (c4 .cm)))
  ;;  ..)
  (let ((cm-alist alist)
        (remove-entry t) (result t))
    (loop 
      (if (or (null cm-alist)
              (null (first cm-alist)))
        (return))
      ;(format t "~% alist: ~S~% specializers: ~S" alist specializers)
      (cond ((or (eq (first specializers) (find-class 'T))  ; super of all classes
                 (eq (caar cm-alist) (first specializers))
                 (subclassp (caar cm-alist)
                                 (first specializers)))
             (if (null (rest specializers))
               ;; last specializer tested -> ready 
               (setq remove-entry t)
               ;; loop over rest of cm-alist
               (setq remove-entry
                     (delete-entry (cdar cm-alist) (rest specializers))))
             (if remove-entry
               ;; remove tested entry from cm-alist
               (progn
                 (setf (car cm-alist) (cadr cm-alist))
                 (setf (cdr cm-alist) (cddr cm-alist))
                 ;(format t "~% modified alist: ~S" alist)
                 )
               ;; otherwise carry on loop with next entry
               (setq cm-alist (cdr cm-alist))))
            (t ;; carry on loop with next entry
             (setq cm-alist (cdr cm-alist)
                   remove-entry nil)))
      (setq result (and result remove-entry)))
    ;(format t "~% resulting alist: ~S" alist)
    result))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defun REMOVE-INVALID-COMBINED-METHODS (gfn-object specializers)
  (declare (optimize (speed 3) (safety 0)))
  (when (%%gfn-combined-methods gfn-object)
    (progn
      (delete-entry (%%gfn-combined-methods gfn-object) specializers)
      (if (eq (first (%%gfn-combined-methods gfn-object)) 
              nil)
        ;; combined methods are empty
        (setf (%%gfn-combined-methods gfn-object) nil)
        ;; remove nil (from last position)
        (setf (%%gfn-combined-methods gfn-object)
              (remove nil (%%gfn-combined-methods gfn-object) :test #'eq))))))

;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

(defun ADD-DIRECT-METHOD (class method)
  "Adds a method to backpointers list of a class."
  (declare (optimize (speed 3) (safety 0)))
  (when (mcs%typep class (find-class 'redefinable))
    (let ((method-name (%%method-name method))
          (qualifiers (%%method-qualifiers method))
          (specializers (%%method-specializers method))
          (result nil))
      (setf (slot-value class 'direct-methods)
            (cons method 
                  (dolist (d-method (slot-value class 'direct-methods) result)
                    (if (and (eq method-name (%%method-name d-method))
                             (equal qualifiers (%%method-qualifiers d-method))
                             (equal specializers
                                    (%%method-specializers d-method)))
                      nil 
                      (setq result (cons d-method result)))))))))



;;; eof

