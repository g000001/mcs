;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: "MCS" -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the macros and help functions file of MCS.
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


;(export '(doplist))

;;; -----------------------------------------------------------------------------------
;;; Macros for Common Lisp
;;; -----------------------------------------------------------------------------------
                                   
(defmacro with-global (bindings &rest body)
  `(progn
     ,@(mapcar #'(lambda (binding)
                   `(setf (symbol-value ',(car binding)) ,(cadr binding)))
               bindings)
     ,@body))

;       ,@(mapcar #'(lambda (binding)
;                   `(makunbound ',(car binding)))
;               bindings)
;       result)))

(defmacro mcs-posq (el a_list) `(position ,el ,a_list :test #'eq))

#|
(defun my-position (el a_list)
  (declare (optimize (speed 3) (safety 0)))
  (if (not (and (atom el) (consp a_list))) (error "Wrong arguments"))
   (let ((position 0))
     (loop
       (if (null a_list) (return nil)
           (if (eq (car a_list) el) (return position)))
       (psetq position (1+ position)
              a_list (cdr a_list))
       )))
|#

;(defmacro mcs-memq (el a_list) `(member ,el ,a_list :test #'eq))

(defun mcs-memq (obj a-list)
  (declare (optimize (speed 3) (safety 0)))
  (loop
    (if a-list
      (if (eq (car a-list) obj)
        (return a-list)
        (setq a-list (cdr a-list)))
      (return ()))))

(defmacro mcs-remq (el a_list) `(remove ,el ,a_list :test #'eq))

(defmacro mcs-removec (el a_list) `(remove ,el ,a_list :test #'equal :key #'car))

(defmacro mcs-assq (key a-list) `(assoc ,key ,a-list :test #'eq))

(defmacro mcs-assoc (key a-list) `(assoc ,key ,a-list :test #'equal))

(defmacro mcs-make-vector (size initial-element) 
  `(make-array ,size :initial-element ,initial-element))

(defmacro mcs-vlength (vect) `(array-total-size ,vect))

(defmacro mcs-make-hash-table () '(make-hash-table :test #'eq))




;;; -----------------------------------------------------------------------------------

;;; iteration macro to deal with plists

(defmacro doplist (((key val) plist &rest args) &rest body)
  "Iteration over a plist
Arguments: ((key val) plist additional-variable-list result-form)
           body"
  ;; Checks for malformed plists!
  (let ((vars (first args))
        (result (second args)))
    (let ((tail (gensym)))
      `(let ((,tail ,plist) ,key ,val ,@vars)
         (loop 
           (when (null ,tail) (return ,result))
           (setq ,key (pop ,tail))
           ;(when (null ,tail)
           ;  (error "Malformed plist in doplist, odd number of elements."))
           (setq ,val (pop ,tail))
           ,@body)))))


;;; indentation for CCL

#+:CCL (progn 
         (push '(doplist . 1) user::*fred-special-indent-alist*)
         ;;(push '(defreader . 2) user::*fred-special-indent-alist*)

         nil)


;;; -----------------------------------------------------------------------------------
;;; Scheme like definition macros
;;; -----------------------------------------------------------------------------------

(defmacro define (variable-name value)
  `(setf (symbol-value ',variable-name) ,value))

(defmacro define-class (class-name form)
  `(setf (symbol-value ',class-name) ,form))

(defmacro define-function (function-name fn)
  `(setf (symbol-function ',function-name) ,fn))


;;; -----------------------------------------------------------------------------------
;;; Strings and Symbols Constructors
;;; -----------------------------------------------------------------------------------

;;; called by: make-reader-name, make-writer-name, make-method-symbol, ...

(defun %make-name (&rest args)
  (intern (apply #'concatenate
                      'string
                      (if (symbolp (first args))
                        (string (first args))
                        (first args))
                      (mapcar #'(lambda (arg)
                                          (if (symbolp arg)
                                            (concatenate 'string " " 
                                                         (string arg))
                                            arg))
                              (rest args)))
          *package*))

(defun make-function-name (generic-name specializers extension)
  (declare (optimize (speed 3) (safety 0)))
  (concatenate 'string
               (string generic-name)
               " (" (string (first specializers))
               (apply #'concatenate
                      'string
                      (mapcar #'(lambda (class-name)
                                          (concatenate 'string " "
                                                       (string class-name)))
                              (rest specializers)))
               ") " extension))

(defun make-discriminator-name (generic-name)
  (declare (optimize (speed 3) (safety 0)))
  (intern (concatenate 'string (if (keywordp generic-name)
                                 (concatenate 'string ":" (string generic-name))
                                 (string generic-name))
                       "-discriminator") *package*))


(defun make-reader-name (index)
  (declare (optimize (speed 3) (safety 0)))
  (format nil "Slot-reader (obj) ~S" index))

(defun make-writer-name (index)
  (declare (optimize (speed 3) (safety 0)))
  (format nil "Slot-writer (val obj) ~S" index))

;;; Generate symbols for methods

(defun make-method-name (gfn-name qualifiers specializer-names)
  (declare (optimize (speed 3) (safety 0)))
  (if qualifiers
    (format nil "Method ~S ~S ~S" gfn-name qualifiers specializer-names)
    (format nil "Method ~S ~S" gfn-name specializer-names)))

;(defun make-method-symbol (gfn-name specializers)
;  (declare (optimize (speed 3) (safety 0)))
;  (intern (make-function-name gfn-name specializers "method") 
;          *package*))
;
;(defun make-eff-method-id (gfn-name specializers)
;  (declare (optimize (speed 3) (safety 0)))
;  (intern (make-function-name gfn-name specializers "eff method")
;          *package*))
;
;;;; Find symbols which identifies methods
;;;; Should be reimplemented!!!
;
;(defun find-method-id (gfn-name specializers)
;  (declare (optimize (speed 3) (safety 0)))
;  (intern (make-function-name gfn-name specializers "method") 
;          *package*))
;
;(defun find-eff-method-id (gfn-name specializers)
;  (declare (optimize (speed 3) (safety 0)))
;  (intern (make-function-name gfn-name specializers "eff method") 
;          *package*))
;
;(defun make-eff-demons-id (gfn-name specializers)
;  (declare (optimize (speed 3) (safety 0)))
;  (intern (make-function-name gfn-name specializers "eff demons") 
;          *package*))

(defun make-predicate-name (class-name)
  (declare (optimize (speed 3) (safety 0)))
  (intern (concatenate 'string (string class-name) "%-P") *package*))

(defun make-setf-name (setf-form)
  (declare (optimize (speed 3) (safety 0)))
  (let ((package (symbol-package (cadr setf-form))))
    (intern (concatenate 'string "(SETF " (string (cadr setf-form))  ")") package)))

;;; -----------------------------------------------------------------------------------
;;; DEFTYPE Functions
;;; -----------------------------------------------------------------------------------

(defun %compute-deftype-form (class-name)
  (let ((predicate-fn-name (make-predicate-name class-name)))
    `(progn
       (deftype ,class-name () '(satisfies ,predicate-fn-name))
       (defun ,predicate-fn-name (obj)
         (and (%object-p obj)
              (%typep obj ',class-name))))))

(defun %install-deftype (class)
  (eval (%compute-deftype-form (%%class-name class))))

;;; -----------------------------------------------------------------------------------
;;; Defsetf Functions
;;; -----------------------------------------------------------------------------------

(defun %compute-defsetf (setf-form lambda-list)
  (let ((value (first lambda-list))
        (reader-arguments (rest lambda-list))
        (setf-name (make-setf-name setf-form))
        (required-reader-arguments (if (mcs-memq '&rest lambda-list)
                                     (butlast (butlast (rest lambda-list)))
                                     (rest lambda-list)))
        (rest-reader-argument (if (mcs-memq '&rest lambda-list)
                                (first (last lambda-list)))))
    (if rest-reader-argument
      `(defsetf ,(second setf-form)
                (,@reader-arguments) (,value)
         (list 'apply 
               '#',setf-name 
               ,value 
               ,@required-reader-arguments
               ,rest-reader-argument))
      `(defsetf ,(second setf-form)
                (,@reader-arguments) (,value)
         (cons ',setf-name (cons ,value 
                                 (list ,@required-reader-arguments)))))))

(defun %install-defsetf (setf-form lambda-list)
  (eval (%compute-defsetf setf-form lambda-list)))

;;; -----------------------------------------------------------------------------------
;;; List Manipulation Functions
;;; -----------------------------------------------------------------------------------

(defun setf-form-p (form)
  (declare (optimize (speed 3) (safety 0)))
  (and (consp form) 
       (= (list-length form) 2) 
       (eq (first form) 'setf)
       (if (symbolp (second form))
         t
         (error "Illegal form: ~S." form))))

(defun constant-keywords-p (initarg-plist)
  (declare (optimize (speed 3) (safety 0)))
  (doplist ((key val) initarg-plist ((result t)) result)
    (if (not (quoted-symbol-p key)) (return nil))))

(defun quoted-symbol-p (form)
  (declare (optimize (speed 3) (safety 0)))
  (and (consp form)
       (eq (car form) 'quote)
       (symbolp (cadr form))))


;;; -----------------------------------------------------------------------------------
;;; Association List Manipulation Functions
;;; -----------------------------------------------------------------------------------

(defun rest-append (lists)
  (apply #'append (mapcar #'cdr lists)))

(defun nested-rest-append (lists depth)
  (cond ((= depth 0) lists)
        ((= depth 1) (rest-append lists))
        (t (rest-append (nested-rest-append lists (1- depth))))))

;;; -----------------------------------------------------------------------------------
;;; Effective Methods Slot in Classes Manipulation Functions
;;; -----------------------------------------------------------------------------------

(defmacro entry-name (effective-methods-entry) `(first ,effective-methods-entry))
(defmacro entry-specializers (effective-methods-entry) `(rest ,effective-methods-entry))


;;; -----------------------------------------------------------------------------------
;;; Method Table Manipulation Functions
;;; -----------------------------------------------------------------------------------

;;; candidate to become a macro
(defun GET-METHOD-ENTRY (key method-table)
  (declare (optimize (speed 3) (safety 0)))
  (cdr (assoc key method-table :test #'eq)))

(defun GET-METHOD-ENTRY-PAIR (key a-list)
  (declare (optimize (speed 3) (safety 0)))
  (assoc key a-list :test #'eq))

(defun GET-METHOD-ENTRY-PAIR-N (a-list first-keys rest-keys result)
  (declare (optimize (speed 3) (safety 0)))
  (cond
   ((null a-list) result)
   ((null first-keys) result)
   ((null rest-keys) (get-method-entry-pair-N a-list (rest first-keys) rest-keys
                                       (append result
                                               (mcs-assq (first first-keys) 
                                                              a-list)) ))
   (t (get-method-entry-pair-N a-list (rest first-keys) rest-keys
                        (get-method-entry-pair-N  (cdr (mcs-assq (first first-keys) 
                                                          a-list))
                                           (first rest-keys) (rest rest-keys) result)
                        ))
   ))

(defun FIND-METHOD-ENTRY (method-table specializers)
  (declare #|(optimize (speed 3) (safety 0))|#)
  (case (length specializers)
           (1 (get-method-entry-pair (first specializers) method-table))
           (2 (get-method-entry-pair (second specializers)
                                (get-method-entry (first specializers)
                                                  method-table)))
           (3 (get-method-entry-pair 
               (third specializers)
               (get-method-entry (second specializers)
                                 (get-method-entry (first specializers)
                                                   method-table))))
           (t (get-method-entry-pair-N method-table
                                (list (first specializers))
                                (mapcar #'(lambda (spec)
                                                    (list spec))
                                        (rest specializers))
                                nil)) ))

(defun remove-empty-entries (a-list) a-list)

(defun relevant-method-specializers (signature-pattern specializers)
  (declare (optimize (speed 3) (safety 0)))
  (let ((result ()))
    (dolist (sign signature-pattern)
      (if sign
        (push (pop specializers) result)
        (pop specializers)))
    (reverse result)))


;;; eof

