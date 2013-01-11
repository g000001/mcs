;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the User Utilities File.
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

;(export '(undefmethod undefgeneric 
;          class-direct-protocol class-protocol object-protocol apropos-protocol)

;;; -----------------------------------------------------------------------------------
;;; Protocol
;;; -----------------------------------------------------------------------------------

(defgeneric class-direct-protocol (class))
(defgeneric class-protocol (class))
(defgeneric object-protocol (obj))
(defgeneric apropos-protocol (obj substring))

(defgeneric slotinit-describe (slot-definition))
(defgeneric which-slots (obj))
(defgeneric which-slotinits (obj))
(defgeneric class-describe (obj))
(defgeneric method-short-describe (obj))
(defgeneric gfn-describe (obj))
(defgeneric find-all-methods (obj))
(defgeneric method-select (obj &rest some-specializers))
(defgeneric method-describe (obj))
(defgeneric which-cplist (obj))

;;; -----------------------------------------------------------------------------------
;;; undefmethod                                                                  Macro
;;; -----------------------------------------------------------------------------------
;;;
;;; Purpose:   User Interface: utility to undo a defmethod.
;;; Arguments: fn-specifier              is a generic function name.
;;;            method-qualifier is the qualifier for the method (can be omitted).
;;;            specializer      is a class object
;;; Values:    The result is the modified generic function bind to fn-specifier.
;;; CLOS:      no such macro.

(defmacro undefmethod (fn-specifier &rest qualifier+specializers+body)
  (let ((r-qualifiers '())
        (specializer-names '()))
    (dolist (form qualifier+specializers+body 
                  (error "Wrong arguments: ~S" qualifier+specializers+body))
      (if (consp form)
        (progn
          (setf specializer-names (extract-specializer-names form))
          (return nil))
        (push form r-qualifiers)))
  `(let ((method (find-method (find-gfn ',fn-specifier)
                               ',(reverse r-qualifiers)
                               (list ,@(mapcar #'(lambda (name)
                                             `(find-class ',name 'signal-error))
                                         specializer-names))
                               nil)))
     (if method 
       (remove-method (find-gfn ',fn-specifier) method)
       (warn "No such method: ~S." 
             ',(cons fn-specifier qualifier+specializers+body))))))

;;; -----------------------------------------------------------------------------------
;;; UNDEFGENERIC                                                                  Macro
;;; -----------------------------------------------------------------------------------
;;;
;;; Purpose:   User Interface: utility to undo a defgeneric.
;;; Arguments: fn-specifier              is a generic function name.
;;; Values:    The result is fn-specifier.
;;; CLOS:      no such macro.

(defmacro undefgeneric (fn-specifier &rest args)
  (declare (ignore args))
  (let ((gfn-name (cond
                    ((symbolp fn-specifier) fn-specifier)
                    ((setf-form-p fn-specifier) (make-setf-name fn-specifier))
                    (t (error "Illegeal function specifier ~S." fn-specifier)))))
     `(let ((gfn (find-gfn ',gfn-name)))
        (dolist (method (generic-function-methods gfn))
          (remove-method gfn method))
        (remove-gfn ',gfn-name)
        ',fn-specifier)))

;;; -----------------------------------------------------------------------------------
;;; CLASS-DIRECT-PROTOCOL                                     Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;;
;;; Purpose:   User Interface: utility.
;;; Arguments: class    is a class object.
;;; Values:    The result is a list of generic function names, which have methods
;;;            with class as specializer.
;;; CLOS:      no such function

(defmethod class-direct-protocol ((class class))
  (declare (ignore class))
  ())

;(defmethod class-direct-protocol ((class class))
;  (if (eq (%%class-name class) 't)
;    (delete nil 
;            (mapcar (function 
;                     (lambda (method)
;                       (let ((gfn-signature 
;                              (%%gfn-signature (find-gfn (%%method-name method))))
;                             (specializers (%%method-specializers method)))
;                         (if (eq (nth (position class specializers)
;                                      gfn-signature) class)
;                           nil
;                           (%%method-name method)))))
;                    (%%class-direct-methods class)) :test #'eq)
;    (mapcar #'method-name (%%class-direct-methods class))))
       
;;; -----------------------------------------------------------------------------------
;;; CLASS-PROTOCOL                                           Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;;
;;; Purpose:   User Interface: utility.
;;; Arguments: class    is a class object.
;;; Values:    The result is a list of generic function names, which have methods
;;;            applicable to instances of class.
;;; CLOS:      no such function

(defmethod class-protocol ((class class))
  (let ((result nil))
    (dolist (superclass (%%class-precedence-list class)
                        (remove-duplicates result :test #'eq))
      (setf result (append (class-direct-protocol superclass) result)) )))

;;; -----------------------------------------------------------------------------------
;;; OBJECT-PROTOCOL                                          Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;;
;;; Purpose:   User Interface: utility.
;;; Arguments: object    is an object.
;;; Values:    The result is a list of generic function names, which are
;;;            applicable to object.
;;; CLOS:      no such function

(defmethod object-protocol ((object object))  
  (class-protocol (mcs%class-of object)))

;;; -----------------------------------------------------------------------------------
;;; APROPOS-PROTOCOL                                         Standard Generic Function
;;; -----------------------------------------------------------------------------------
;;;
;;; Purpose:   User Interface: utility to get similar generic function names.
;;; Arguments: object    is an object.
;;;            substring is a string
;;; Values:    The result is a list of generic function names containing substring, 
;;;            which are applicable to object.
;;; CLOS:      no such function

(defmethod apropos-protocol ((object object)
                             substring)
  (remove nil 
          (mapcar #'(lambda (gfn-name)
                      (if (search substring (string gfn-name) 
                                  :test #'char-equal) gfn-name))
                  (class-protocol (mcs%class-of object)))
          :test #'eq))

;;; -----------------------------------------------------------------------------------
;;; General functions:
;;; -----------------------------------------------------------------------------------

(defun average-size-of (entity-fn map-fn)
  (let ((counter 0)
        (sum 0))
    (funcall map-fn #'(lambda (object)
                        (let ((size (length (funcall entity-fn object))))
                          (setf sum (+ sum size)))
                        (incf counter)))
    (coerce (/ sum counter) 'float)))

(defun maximal-size-of (entity-fn map-fn)
  (let ((max 0))
    (funcall map-fn #'(lambda (object)
                     (let ((entity (length (funcall entity-fn object))))
                       (if (> entity max) (setf max entity)))))
    max))

(defun show (list)
  (pprint (cons "This is a list of entities, which satisfies a condition:" 
                list)))

;;; -----------------------------------------------------------------------------------
;;; Class information:
;;; -----------------------------------------------------------------------------------

(defun map-classes (fn)
  (maphash #'(lambda (key val)
               (declare (ignore key))
               (funcall fn val))
           *named-classes*))

(defun select-classes (predicate-fn)
  (let ((result nil))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (when (funcall predicate-fn val)
                   (push val result)))
             *named-classes*)
     result))

;;; -----------------------------------------------------------------------------------
;;; Generic functions / methods information:
;;; -----------------------------------------------------------------------------------

(defun map-gfns (fn)
  (maphash #'(lambda (key val)
               (declare (ignore key))
               (funcall fn val))
           *named-gfns*))

(defun select-gfns (predicate-fn)
  (let ((result nil))
    (maphash #'(lambda (key val)
                 (declare (ignore key))
                 (when (funcall predicate-fn val)
                   (push val result)))
             *named-gfns*)
     result))

;;; -----------------------------------------------------------------------------------
;;; Applications:
;;; -----------------------------------------------------------------------------------

(defun all-methods ()
  (apply #'append (mapcar #'generic-function-methods (select-gfns #'identity))))

(defun select-mixins () (select-classes #'mixin-p))
(defun select-abstracts () (select-classes #'abstract-p))



;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------
;;; von Eckehard
;;; -----------------------------------------------------------------------------------
;;; -----------------------------------------------------------------------------------

#|
;(export '(                   
;          apropos-class
;          which-slots 
;          which-slotinits 
;          class-describe
;          apropos-gfn 
;          gfn-describe 
;          find-all-methods 
;          method-select 
;          method-describe
;          which-cplist
;          ))
(export '(obj-name obj<= gfn-list class-list))

          
;;-------------------------------------------------------------------------------------

(defun apropos-class (&rest args) 
  "displays the names of classes containing substring belonging to pkg."
  (let ((substring (string (or (first args) "")))
        (pkg (or (find-package (second args)) *package*)))
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (if (and (eq key (find-symbol (symbol-name key) pkg))
                          (search substring (symbol-name key)))
                   (print key)))
             *NAMED-CLASSES*)))

;;-------------------------------------------------------------------------------------



;  (do ((slot-name (slot-value slot 'name) nil)
;       (readers (slot-value slot 'readers) (rest readers))
;       (writers (slot-value slot 'writers) (rest writers)))
;      ((and  (null slot-name) (null readers) (null writers)))
;    (format t "~%~5T~(~@[~S~] ~25,5T~@[~S~] ~55,5T~@[~S~]~)"
;            slot-name  (first readers) (first writers))))

(defmethod slotinit-describe ((slot slot-definition))
  (let ((initargs (slot-definition-initargs slot))
        (initform (if (slot-boundp slot 'initform)
                    (slot-definition-initform slot))))
    (if (or initargs initform)
      (format t "~%~5T~(~@[~S~] ~25,5T~{~S ~} ~55,5T~@[~S~]~)"
          (slot-definition-name slot) initargs initform))))

(defmethod which-slots ((object class))
  (let ((slots (class-slots object)))
    (cond (slots    
           (format t "~%Slots, Readers and Writers provided by Class ~S are:"
                   (class-name object))
           (dolist (slot slots)
             (slotinit-describe slot)))
          (t (format t "~%No slots")))))

(defmethod which-slots ((object symbol))
  (let ((class (find-class object)))
    (if class
      (which-slots class)
      (format t "~%unknown class ~S" object))))

(defmethod which-slotinits ((object class))
  (let ((slots (class-slots object)))
    (cond (slots
           (format t "~%Slots with Initargs or Initform provided by Class ~S are:"
                   (class-name object))
           (dolist (slot slots)
             (slotinit-describe slot)))
          (t (format t "~%No slots")))))

(defmethod which-slotinits ((object symbol))
  (let ((class (find-class object)))
    (if class
      (which-slotinits class)
      (format t "~%unknown class ~S" object))))

;;-------------------------------------------------------------------------------------
        
(defmethod class-describe ((object class))
  (format t "~%Class ~S is an instance of metaclass ~S"
          (class-name object)(class-name (class-of object)))
  (unless (string= (obj-documentation object) "")
    (format t "~%~A" (obj-documentation object)))
  (format t "~%Class has cplist: ~S" (%class-precedence-list object))
  (which-slots object))

(defmethod class-describe ((object symbol))
  (let ((class (find-class object)))
    (if class
      (class-describe class)
      (format t "~%~S unknown class" object))))

;;-------------------------------------------------------------------------------------

(defun apropos-gfn (&rest args)
  "displays the names of generic functions containing substring belonging to pkg."
  (let ((substring (string (or (first args) "")))
        (pkg (or (find-package (second args)) *package*)))
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (if (and (find-symbol (symbol-name key) pkg)
                          (search substring (symbol-name key)))
                   (print key)))
             *named-gfns*)))

(defmethod method-short-describe ((method method))
  (format t "~%   ~S~10T~S"
          (method-qualifiers method)
          (method-specializers method))
  (unless (string= (obj-documentation method) "")
    (format t "~%~A" (obj-documentation method))))

(defmethod gfn-describe ((gfn generic-function))
  (let ((documentation (obj-documentation gfn)))
    (format t "~%~S ~S~%"
            (generic-function-name gfn) (generic-function-lambda-list gfn))
    (unless (string= documentation "")
      (format t "~A~%" documentation))
    (dolist (method (find-all-methods gfn))
      (method-short-describe method))))

(defmethod gfn-describe ((gfn-name symbol))
  (let ((gfn (find-gfn gfn-name)))
    (if gfn 
      (gfn-describe gfn)
      (format t "~% ~S unknown gfn" gfn-name))))

;;-------------------------------------------------------------------------------------

(defmethod find-all-methods ((gfn generic-function))
  (generic-function-methods gfn))

(defmethod find-all-methods ((name symbol))
  (let ((gfn (find-gfn name)))
    (if gfn
      (find-all-methods gfn)
      (format t "~% ~S unknown generic function." name))))

(defmethod method-select ((gfn generic-function) &rest some-specializers)
  (let ((methods (generic-function-methods gfn)))
    (if methods
      (dolist (method methods)
        (let ((specializers (method-specializers method)))
          (when (search (mapcar #'find-class some-specializers) specializers) 
            (format t "~%Enter :yes, :no or :quit to select a method of ~S"
                    (generic-function-name gfn))
            (format t "~%  ~S ~S " (method-qualifiers method) specializers)
            (case (read)
              (:yes  (return method))
              (:quit (return nil))))))
      (format t "~% ~S has no methods." (generic-function-name gfn) ))))
 
(defmethod method-select ((name symbol) &rest some-specializers)
  (let ((gfn (find-gfn name)))
    (if gfn
      (apply #'method-select gfn some-specializers)
      (format t "~% ~S unknown generic function." name))))

;;-------------------------------------------------------------------------------------

(defmethod method-describe ((method-name symbol))
  (let ((gfn (find-gfn method-name)))
    (if gfn
      (let ((method (method-select gfn)))
        (when method
          (terpri)
          (method-describe method)))
      (format t "~%~S unknown generic funtion" method-name))))
     

;;-------------------------------------------------------------------------------------

(defmethod which-cplist (object)
  (format t "~%Instance belongs to the classes: ~{~S ~}" 
          (class-precedence-list (class-of object))))
|#

;;; still used

(defun specialize-lambda-list (lambda-list specializers)
  (do ((rlist lambda-list (rest rlist))
       (rspecializers specializers (rest rspecializers))
       (result nil))
      ((null rlist) (nreverse result))
    (if (or (eq (first rspecializers) t)
            (null (first rspecializers)))
      (setf result (cons (first rlist) result))
      (setf result (cons (list (first rlist) (first rspecializers))  result)))))

(defmethod method-describe ((method method))
  (format t "~%~S ~@[~S ~]~S~%"
          (method-name method)
          (method-qualifiers method)
          (specialize-lambda-list (method-lambda-list method)
                                  (mapcar #'class-name (method-specializers method))))
  (unless (string= (obj-documentation method) "")
    (format t "~A~%" (obj-documentation method)))
  (pprint (method-function method)))


;;; -------------------------------------------------------------------------------------
;;; von Juergen
;;; -------------------------------------------------------------------------------------

(defun obj-name (obj)
  (if (slot-exists-p obj 'name)
    (string (slot-value obj 'name))
    "Anonymous"))

(defun obj<= (obj1 obj2)
  (string<= (obj-name obj1) (obj-name obj2)))

(defun gfn-list (&rest args)
  "Returns a list of generic functions containing substring belonging to pkg."
  (let ((substring (if args (string-upcase (first args)) ""))
        (pkg (or (second args) *package*))
        (sorted (third args))
        (result nil))
    (maphash #'(lambda (key value)
                 (if (and (or (keywordp key)         ; keywords as gfnames, horrible!!!
                              (eq key (find-symbol (symbol-name key) pkg)))
                          (search substring (symbol-name key)))
                   (push value result)))
             *NAMED-GFNS*)
    (if sorted (sort result 'obj<=) result)))

(defun class-list (&rest args)
  "Returns a list of classes containing substring belonging to pkg."
  (let ((substring (if args (string-upcase (first args)) ""))
        (pkg (or (second args) *package*))
        (sorted (third args))
        (result nil))
    (maphash #'(lambda (key value)
                 (if (and (eq key (find-symbol (symbol-name key) pkg))
                          (search substring (symbol-name key)))
                   (push value result)))
             *NAMED-CLASSES*)
    (if sorted (sort result 'obj<=) result)))




;;; eof

