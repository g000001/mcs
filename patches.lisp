;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the file of patches.
;;;
;;; notes:          
;;;
;;; contact:        Juergen Kopp, Harry Bretthauer
;;;
;;; history:
;;;          date:      author:             comments:
;;;          23.02.90   Harry Bretthauer    initial version

;;; 29.05.90     compute-canonical-slot-specifications                          cl-def
;;; Bretthauer:  unknown slot-options passed to result                      

;;; 05.06.90     extend-accessor-fns                                           globals
;;; Bretthauer:  (compile ... ersetzt durch (symbol-function (compile ....

;;; 05.06.90     class-direct-subclasses                                      access-m
;;; Bretthauer:  %class-direct-superclasses ersetzt durch %class-direct-supers

;;; 05.06.90     ensure-class                                                   cl-def
;;; Bretthauer:  no initargs, slot-accessor for built-in-classes

;;; 10.06.90     method-combination-name                                      access-m
;;; Bretthauer:  new definition

;;; 10.06.90     make-method-lambda                                             m-core
;;; Bretthauer:  declarations verbessert

;;; 11.06.90     determin-method-class                                          m-core
;;; Bretthauer:  ==> mixin-method or gfn-method-class

;;; 11.06.90     ensure-gfn                                                     m-core
;;; Bretthauer:  behandelt: setf, globale bindung

;;; 11.06.90     defgeneric                                                     m-core
;;; Bretthauer:  ruft ensure-gfn

;;; 11.06.90     defmethod                                                      m-core
;;; Bretthauer:  ruft ensure-gfn

;;; 20.07.90     generic-function-methods                                     access-m
;;; Bretthauer:  renamed => generic-function-methods-table

;;; 20.07.90     generic-function-methods                                     access-m
;;; Bretthauer:  added

;;; 20.07.90     ADD-COMBINED-METHOD                                            m-core
;;; Bretthauer:  compute-discriminating-fn call removed

;;; 20.07.90     dynamic-lookup                                               m-lookup
;;; Bretthauer:  compute-discriminating-fn call inserted

;;; 24.1.91        compute-canonical-slot-specifications                   cl-def.lisp                                    file
;;; Bretthauer:    evaluate unknown slot options now

;;; 25.1.91        undefmethod, undefgeneric                                 util.lisp                                    file
;;; Bretthauer:    extended syntax provided

;;; 25.1.91        make-reader-name, make-writer-name                      macros.lisp                                    file
;;; Bretthauer:    return strings

;;; 25.1.91        make-method-id,...                                      macros.lisp                                    file
;;; Bretthauer:    removed

;;; 25.1.91        defmethod                                               m-core.lisp                                    file
;;; Bretthauer:    calls make-method-name (new in macros.lisp)

;;; 25.1.91        1. generate-accessor-fns, 2. extend-accessor-fns       globals.lisp                                    file
;;; Bretthauer:    removed (compile ...) from 1., 
;;;                1. and 2. call make-reader-name, make-writer-name

;;; 15.2.91        standard objects inserted                           classes.lisp, ...                                    file
;;; Bretthauer:    

;;; 09.07.91       check-for-existing-gfn modified        gfn-core.lisp, system-m.lisp
;;; Kopp:

;;; 21.11.91   allocate-instance                                       redefine.lisp
;;; Bretthauer serious bug fixed!!!



;;; 23.01.92   initialize-instance                                       class-m.lisp
;;; Bretthauer serious bug fixed!!!



;;; 23.01.92   reinitialize-instance                                       redefine.lisp
;;; Bretthauer serious bug fixed!!!



;;; 23.01.92   class                                                  cl-boot.lisp
;;; NOT A PATCH: still to do in the next version!
;;;              (defclass class (object)
;;;                ((name :initform '|Anonymous|

;;; -----------------------------------------------------------------------------------

(in-package "MCS")
 
;;; date        function                                                          file
;;; author      comment

;;; -----------------------------------------------------------------------------------


;;; Unknown slot options in (DEFCLASS ...) forms are evaluated in newer versions of MCS.
;;; The frames in babylon require to quote them. This is a corresponding version
;;; of canonicalizing slot options:

#|
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
                          `(',key ',value ,@unknown-options))
                  (progn
                    (when *warn-if-unknown-slot-option*
                      (warn "Undefined slot option ~S for slot ~S in class ~S."
                            key name class-name))
                    (setq unknown-options
                          `(',key ',value ,@unknown-options))))) )))
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
|#


;;; eof

