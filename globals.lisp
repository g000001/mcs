;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the file of global data.
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
;;; Parameters
;;; -----------------------------------------------------------------------------------

;;; history:  
;;;         date        name        comment
;;;         13.03.90    Kopp        *implemented-slot-options* added               
;;;         12.06.90    Bretthauer  *warn-if-...* added               

(defvar *implemented-class-options* '(:documentation :metaclass))
(defvar *implemented-slot-options* '(:reader :writer :accessor 
                                     :initarg :initform
                                     :type :documentation))

(defvar *check-if-wrong-initargs* t)
(defvar *warn-if-unknown-class-option* nil)
(defvar *warn-if-unknown-slot-option* nil)
(defvar *warn-if-unknown-generic-option* nil)
(defvar *warn-if-redefine-class* t)
(defvar *programming-environment* t)

;;; -----------------------------------------------------------------------------------
;;; Optimization Variables
;;; -----------------------------------------------------------------------------------

;;; Global variables used when computing the effective method for optimizations:
;;; the macros SLOT-VALUE and CALL-NEXT-METHOD access them during the macroexpansion.

(defvar *temp-name*)      
(defvar *temp-lambda-list*)
(defvar *temp-specializer-names*)
(defvar *temp-qualifiers*)

(defvar *gfn-safety* 1)
(defvar *method-safety* 0)

(defvar *optimize-slot-access* ())
(defvar *runtime-mode* nil)

(defvar *no-type-check-in-r/w* ())

;;; Global variables which bind system classes:

(defvar *cons-cl* )
(defvar *null-cl* )
(defvar *symbol-cl* )

(defvar *string-cl* )
(defvar *bit-vector-cl* )
(defvar *vector-cl* )
(defvar *array-cl* )

;; numbers
(defvar *integer-cl* )
(defvar *ratio-cl* )
(defvar *rational-cl* )
(defvar *float-cl* )
(defvar *complex-cl* )
(defvar *number-cl* )

(defvar *character-cl* )
(defvar *stream-cl* )
(defvar *hash-table-cl* )
(defvar *function-cl* )
(defvar *t-cl* )

;;; -----------------------------------------------------------------------------------
;;; Class table
;;; -----------------------------------------------------------------------------------

;;; hisoty:  
;;;         date        name        comment

(defvar *named-classes* (mcs-make-hash-table)) 

(defun find-class (class-name &rest errorp)
  (declare (optimize (speed 3) (safety 1)))
  (if errorp
    (let ((class (gethash class-name *named-classes*)))
      (if class
        class
        (error "No class named ~S." class-name)))
    (gethash class-name *named-classes*)))

(defun insert-class (class-name class)
  (declare (optimize (speed 3) (safety 0)))
  (setf (gethash class-name *named-classes*) class))

(defsetf find-class insert-class)

(defun remove-class (class-name)
  (declare (optimize (speed 3) (safety 0)))
  (remhash class-name *named-classes*))

;;; Class wrappers

;(defvar *class-wrappers* (mcs-make-hash-table)) 
;
;(defun class-wrapper (class)
;  (declare (optimize (speed 3) (safety 1)))
;  (let ((wrapper (gethash class *class-wrappers*)))
;      (if wrapper
;        wrapper
;        (error "No wrapper for ~S." class))))
;
;(defun insert-wrapper (class wrapper)
;  (declare (optimize (speed 3) (safety 0)))
;  (setf (gethash class *class-wrappers*) wrapper))
;
;(defsetf class-wrapper insert-wrapper)
;
;(defun remove-wrapper (class)
;  (declare (optimize (speed 3) (safety 0)))
;  (remhash class *class-wrappers*))

;;; -----------------------------------------------------------------------------------
;;; Generic functions table
;;; -----------------------------------------------------------------------------------

;;; hisoty:  
;;;         date        name        comment
;;;         dd.mm.yy                

(defvar *named-gfns* (mcs-make-hash-table)) 

(defun find-gfn (gfn-name &rest errorp)
  (declare (optimize (speed 3) (safety 1)))
  (if errorp
    (let ((gfn (gethash gfn-name *named-gfns*)))
      (if gfn
        gfn
        (error "No gfn named ~S." gfn-name)))
    (gethash gfn-name *named-gfns*)))

;(defun find-gfn (gfn-name)
;  (declare (optimize (speed 3) (safety 0)))
;  (gethash gfn-name *named-gfns*))

(defun insert-gfn (gfn-name gfn)
  (declare (optimize (speed 3) (safety 0)))
  (setf (gethash gfn-name *named-gfns*) gfn))

(defsetf find-gfn insert-gfn)

(defun remove-gfn (gfn-name)
  (declare (optimize (speed 3) (safety 0)))
  (fmakunbound gfn-name)
  (remhash gfn-name *named-gfns*))

;;; -----------------------------------------------------------------------------------
;;; Legal method qualifiers list
;;; -----------------------------------------------------------------------------------

;;; hisoty:  
;;;         date        name        comment

(defvar *legal-qualifiers* nil)

(defun insert-qualifiers (qualifiers)
  (declare (optimize (speed 3) (safety 0)))
  (setf *legal-qualifiers* (append qualifiers *legal-qualifiers*)))

(defun legal-qualifier-p (qualifiers)
  (declare (optimize (speed 3) (safety 0)))
  (if (mcs-memq qualifiers *legal-qualifiers*) t))

;;; -----------------------------------------------------------------------------------
;;; Method combinations table
;;; -----------------------------------------------------------------------------------

;;; hisoty:  
;;;         date        name        comment

(defvar *implemented-method-combinations* nil)

(defun find-method-combination (qualifiers)
  (declare (optimize (speed 3) (safety 0)))
  (let ((result (assoc qualifiers *implemented-method-combinations*
                     :test #'equal)))
    (if result
      (cdr result)
      (error "Can't find method combination associated with: ~S." qualifiers))))

(defun insert-method-combination (qualifiers method-combination)
  (declare (optimize (speed 3) (safety 0)))
  (setf *implemented-method-combinations*
        (acons qualifiers method-combination *implemented-method-combinations*)))

;;; -----------------------------------------------------------------------------------
;;;     Predefined reader and writer functions for local slots  
;;; -----------------------------------------------------------------------------------

;;; hisoty:  
;;;         date        name        comment

(defvar *local-reader-fns*)
(defvar *local-writer-fns*)
(defvar *local-fns-fill-pointer*)

(defmacro copy-and-extend-array (array no-of-entries &rest default)
  (if default (setf default (first default)))
  `(let ((new-array (mcs-make-vector (+ (array-total-size ,array) ,no-of-entries)
                                     ,default)))
     (dotimes (i (array-total-size ,array))
       (setf (svref new-array i) (svref ,array i)))
     new-array))

(defun extend-accessor-fns (no-of-entries)
  (declare #|(optimize (speed 3) (safety 0))|#)
  ; adjust the arrays with the accessor-fns and generates the apropriate accessors
  (let (array-r array-w  ; fuer schnelleren Zugriff in der loop
                old-fill-pointer new-fill-pointer)
    (setf *local-reader-fns* 
          (copy-and-extend-array *local-reader-fns* no-of-entries))
    (setf *local-writer-fns* 
          (copy-and-extend-array *local-writer-fns* no-of-entries))
    (setq array-r *local-reader-fns*
          array-w *local-writer-fns*
          old-fill-pointer *local-fns-fill-pointer*
          new-fill-pointer (+ old-fill-pointer no-of-entries))
    (setf *local-fns-fill-pointer* new-fill-pointer)
    
    (loop                            ; from 'old-fill-pointer' to 'new-fill-pointer'-1
      (if (= old-fill-pointer new-fill-pointer) (return nil))
      (setf (svref array-r old-fill-pointer) 
            (%set-anonymous-function-name
             (gen-reader-closure old-fill-pointer)
             (make-reader-name old-fill-pointer)))
      (setf (svref array-w old-fill-pointer) 
            (%set-anonymous-function-name
             (gen-writer-closure old-fill-pointer)
             (make-writer-name old-fill-pointer)))
      (setq old-fill-pointer (1+ old-fill-pointer)))))


(defmacro generate-accessor-fns (no-of-entries)
  (let (array-r array-w fill-pointer)
    (setq array-r '*local-reader-fns*
          array-w '*local-writer-fns*
          fill-pointer '*local-fns-fill-pointer*)
    `(progn
       (setf ,array-r (mcs-make-vector ,no-of-entries nil))
       (setf ,array-w (mcs-make-vector ,no-of-entries nil))
       ,@(let ((result nil) (i 0))
           (loop ; form "i" to "no-of-entries" -1
             (if (= i no-of-entries) (return result))
             (setf result
                   (cons `(setf (svref ,array-r ,i)
                                (%set-anonymous-function-name
                                 #',(gen-reader-fn i)
                                 ',(make-reader-name i)))
                         (cons `(setf (svref ,array-w ,i)
                                      (%set-anonymous-function-name
                                       #',(gen-writer-fn i)
                                       ',(make-writer-name i)))
                               result)))
             (setq i (1+ i)))
           result)
       (setf ,fill-pointer ,no-of-entries))))

(defun get-accessor-fn (accessor-type location)
  (declare #|(optimize (speed 3) (safety 0))|#)
  (let ((fill-pointer *local-fns-fill-pointer*))
    (if (>= location fill-pointer)
      (extend-accessor-fns (max 8 (- location (1- fill-pointer))))))
  (let ((access-fns (if (eq accessor-type 'reader)
                        *local-reader-fns*
                        *local-writer-fns*)
                      ))
    (svref access-fns location)))


;;; Generate 32 standard local slot access methods

(generate-accessor-fns 32)


;;; -----------------------------------------------------------------------------------
;;; Documentation Table for the Programming Environment
;;; -----------------------------------------------------------------------------------

(if *programming-environment*

  (let ((*docu-table* (make-hash-table :test #'equal)))
    
    (defun pe-store (kind-of-information key value)
      (setf (gethash (cons kind-of-information key) *docu-table*)
            value))
    
    (defun pe-find (kind-of-information key)
      (gethash (cons kind-of-information key) *docu-table*)))

  (progn
    (defun pe-store (kind-of-information key value)
      (declare (ignore kind-of-information key value))
      ())

    (defun pe-find (kind-of-information key)
      (declare (ignore kind-of-information key))
      ())))



;;; eof

