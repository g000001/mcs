;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the slot-value implementation.
;;;
;;; notes:          
;;;
;;; contact:        Juergen Kopp, Harry Bretthauer
;;;
;;; history:
;;;          date:      author:             comments:
;;;          01.03.90   Harry Bretthauer    initial version
;;; -----------------------------------------------------------------------------------

(in-package "MCS")

;;; -----------------------------------------------------------------------------------
;;; The Protocol
;;; -----------------------------------------------------------------------------------

;;; (slot-value     obj symbol)  -->  obj                                       macro
;;; (slot-value-low obj symbol)  -->  obj | <unbound>                           macro

;;; (%find-slot-position  class symbol)  -->  n | nil                        function
;;; (%find-slot-named obj n)  -->  symbol                                    function


;;; -----------------------------------------------------------------------------------
;;; The Implementation
;;; -----------------------------------------------------------------------------------

(defmacro %slot-location-of (object slot-name)
  "io: <object> <slot-name> --> <n> | nil
       Computes the logical position of slot <slot-name> in <object>."
  `(funcall (mcs%slot-fn-of ,object) ,object ,slot-name))

(defun mcs%slot-value-low (object slot-name)
  (declare #|(optimize (speed 3) (safety 0))|#)
  "No check if slot is unbound, e. g. has the value '<unbound>"
  (let ((slot-position (%slot-location-of object slot-name)))
    (declare (optimize (speed 3) (safety 0)))
    (if slot-position
      (mcs%obj-ref object slot-position)
      (slot-missing (mcs%class-of object) object slot-name 'slot-value))))
        
(defun mcs%set-slot-value-low (object slot-name value)
  (declare #|(optimize (speed 3) (safety 0))|#)
  (let ((slot-position (%slot-location-of object slot-name)))
    (declare (optimize (speed 3) (safety 0)))
    (if slot-position  
      (setf (mcs%obj-ref object slot-position) value)
      (slot-missing (mcs%class-of object) object slot-name 'setf value))))

(defsetf mcs%slot-value-low mcs%set-slot-value-low)


(defun mcs%slot-value (object slot-name)
  (declare (optimize (speed 3) (safety 0)))
  (if (%object-p object)
    (let ((result 
           (let ((slot-position (%slot-location-of object slot-name)))
             (declare #|(optimize (speed 3) (safety 0))|#)
             (if slot-position
               (mcs%obj-ref object slot-position)
               (slot-missing (mcs%class-of object) object slot-name 'slot-value))) ))
      (if (eq result '<unbound>) 
        (slot-unbound (mcs%class-of object) object slot-name)
       result))
    (slot-missing (built-in-class-of object) object slot-name 'slot-value)))

(defun mcs%set-slot-value (object slot-name value)
  (declare #|(optimize (speed 3) (safety 0))|#)
  (if (%object-p object)
    (let ((slot-position (%slot-location-of object slot-name)))
      (declare #|(optimize (speed 3) (safety 0))|#)
      (if slot-position  
        (setf (mcs%obj-ref object slot-position) value)
        (slot-missing (mcs%class-of object) object slot-name 'setf value)))
    (slot-missing (built-in-class-of object) object slot-name 'setf value)))

(defsetf mcs%slot-value mcs%set-slot-value)


;;; Low slot access primitives for cases where the slot position is known and
;;; with no check for slot unbound. setf can be used to set a slot.

(defmacro mcs%local-slot-indexed-low (object index)
  `(mcs%obj-ref ,object ,index))


;;; If the slot is unbound, e. g. has the value '<unbound> 
;;; the slot-unbound generic function is called, which signals an error
;;; for metainstances of standard-class

(defmacro mcs%local-slot-indexed (object slot-name index)
  `(let ((result (mcs%local-slot-indexed-low ,object ,index)))
     (if (eq result '<unbound>) 
       (slot-unbound (mcs%class-of ,object) ,object ,slot-name)
       result)))

(defmacro mcs%set-local-slot-indexed (object slot-name index value)
  (declare (ignore slot-name)) ; (optimize (speed 3) (safety 0)))
  `(setf (mcs%local-slot-indexed-low ,object ,index) ,value))

(defsetf mcs%local-slot-indexed mcs%set-local-slot-indexed)

;;; -----------------------------------------------------------------------------------
;;; SLOT-VALUE                                                                   Macro
;;; -----------------------------------------------------------------------------------
;;; hisoty:  
;;;         date        name        comment
;;;         dd.mm.yy                

;;; Because it is a macro it must appear in the program here,
;;; before using it in function definitions.

;;; In methods slot-value expands into a structure/vector ref, if object is
;;; a specialized parameter in the lambda-list.

(defmacro slot-value (object slot-name)
  (if (and *optimize-slot-access*
           (quoted-symbol-p slot-name)
           (print slot-name)
           (boundp '*temp-name*) (print *temp-name*))
    ;; called in method body, trie to optimize
    (let ((class-position 
           (mcs-posq object 
                     (if (eq (first *temp-lambda-list*) '%next-fns)
                       (rest *temp-lambda-list*)
                       *temp-lambda-list*) )))
      (if class-position
        ;; object is a lambda variable 
        (let ((location (%find-slot-position 
                         (find-class (nth class-position *temp-specializer-names*))
                         (second slot-name))))
          (print location)
          (if location
            ;; proved, that object has slot named slot-name -->> optimized access!
            `(mcs%local-slot-indexed ,object ,slot-name ,location)
            ;; can't prove, that object has slot named slot-name
          `(mcs%slot-value ,object ,slot-name)))
        ;; object is not a lambda variable
        `(mcs%slot-value ,object ,slot-name)))
    ;; don't try to optimize
    `(mcs%slot-value ,object ,slot-name)))

;;; -----------------------------------------------------------------------------------
;;; SLOT-VALUE-LOW                                                              Macro
;;; -----------------------------------------------------------------------------------
;;; hisoty:  
;;;         date        name        comment
;;;         dd.mm.yy                

(defmacro slot-value-low (object slot-name)
  "No check if slot is unbound, e. g. has the value '<unbound>"
  (if (and *optimize-slot-access*
           (quoted-symbol-p slot-name)
           (boundp '*temp-name*))
    ;; called in method body, trie to optimize
    (let ((class-position 
           (mcs-posq object 
                     (if (eq (first *temp-lambda-list*) '%next-fns)
                       (rest *temp-lambda-list*)
                       *temp-lambda-list*) )))
      (if class-position
        ;; object is a lambda variable 
        (let ((location (%find-slot-position 
                         (find-class (nth class-position *temp-specializer-names*))
                         (second slot-name))))
          (if location
            ;; proved, that object has slot named slot-name -->> optimized access!
            `(mcs%local-slot-indexed-low ,object ,location)
            ;; can't prove, that object has slot named slot-name
          `(mcs%slot-value-low ,object ,slot-name)))
        ;; object is not a lambda variable
        `(mcs%slot-value-low ,object ,slot-name)))
    ;; don't try to optimize
    `(mcs%slot-value-low ,object ,slot-name)))

;;; -----------------------------------------------------------------------------------
;;; %FIND-SLOT-POSITION                                                       Function
;;; -----------------------------------------------------------------------------------

(defun %find-slot-position (class slot-name)
  (cond
;   ((%typep class 'instantiable)
;    (funcall (%%class-slot-accessor class) 'pseudo-obj slot-name '%slot-exists-p))
   ((%typep class 'single-inherited)
    (let ((slots (%%class-slots class))
          (pos 0))
      (loop
        (if (null slots) (return ()))
        (if (eq slot-name (%%slot-name (car slots))) (return pos))
        (setf slots (cdr slots))
        (incf pos))))
    (t (error "Slot ~S of ~S can change its position in a subclass."
              slot-name class))))

;;; -----------------------------------------------------------------------------------
;;; %FIND-SLOT-NAME                                                           Function
;;; -----------------------------------------------------------------------------------

(defun %find-slot-name (object index)
  (declare (optimize (speed 3) (safety 0)))
  ; called within the standard-readers, when slot is unbound
  (%%slot-name (nth index (%%class-slots (mcs%class-of object)))))

;;; -----------------------------------------------------------------------------------
;;; GENERAL-SLOT-POSITION                                                     Function
;;; -----------------------------------------------------------------------------------

(defun general-slot-position (object slot-name)
  (declare #|(optimize (speed 3) (safety 0))|#)
  (let ((slots (%%class-slots (mcs%class-of object)))
        (pos 0))
    (loop
      (if (null slots) (return nil))
      (if (eq slot-name (%%slot-name (car slots))) (return pos))
      (setf slots (cdr slots))
      (incf pos))))



;;; eof

