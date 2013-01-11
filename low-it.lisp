;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the portable low level objects representation.
;;;
;;; notes:          For efficiency reasons there are Lisp System dependent
;;;                 implementations of this file: low-mac.lisp, low-sun.lisp
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

;;; (%object-p  obj)  -->  boolean                                            macro
;;; (%make-obj  class fn . args)  -->  obj                                    macro
;;; (%make-empty-obj class fn n)  -->  obj                                    macro
;;; (mcs%copy  mcsobj)  -->  mcsobj                                           macro

;;; (mcs%obj-ref  mcsobj n)  -->  obj                                         macro

;;; (mcs%type-of  mcsobj)  -->  symbol                                        macro
;;; (mcs%class-of  mcsobj)  -->  class                                        macro
;;; (mcs%slot-fn-of  mcsobj)  -->  fn                                         macro

;;; -----------------------------------------------------------------------------------
;;; The Implementation
;;; -----------------------------------------------------------------------------------

;;; Objects Representation: 
;;; <object> == <structure>
;;; <structure> == <mcsobject>
;;; <mcsobject> == <isit> <slots>
;;; <isit> == (class) 
;;; <slot-access-function> == function: 
;;;                            (object slot-name operation) --> n | nil
;;; <slots> == vector


(defstruct (mcs% (:print-function mcs%print)
                 (:constructor make-mcs% (isit slots)))
  isit
;  slot-fn
  slots)

#+:EXCL (defmacro %object-p (x)
          `(and (excl::structurep ,x)
                (locally
                 (declare (optimize (speed 3) (safety 0)))
                 (eq (svref ,x 0) 'mcs%))))

#-:EXCL (defmacro %object-p (obj)
          `(typep ,obj 'mcs%))

(defmacro %make-obj (class-wrapper slot-fn &rest slot-values)
  (declare (ignore slot-fn))
  `(make-mcs% ,class-wrapper
              (vector ,@slot-values)))

(defmacro %make-empty-obj (class-wrapper slot-fn number-of-slots)
  (declare (ignore slot-fn))
  `(make-mcs% ,class-wrapper
              (mcs-make-vector ,number-of-slots '<unbound>)))

(defmacro mcs%obj-ref (obj index)
  `(svref (mcs%-slots ,obj) ,index))

(setf (symbol-function 'mcs%print)
      #'(lambda (obj stream depth)
          (declare (ignore depth))
          (format stream "#<Boot-Object @ ~S>" (mcs%obj-ref obj 0))))

(defmacro mcs%copy (obj)
  `(make-mcs% (mcs%-isit ,obj)
              (copy-seq (mcs%-slots ,obj))))

(defmacro mcs%type-of (obj)
  `(%%class-name (car (mcs%-isit ,obj))))

(defmacro mcs%class-of (obj)
  `(car (mcs%-isit ,obj)))

(defmacro mcs%slot-fn-of (obj)
  `(mcs%obj-ref (car (mcs%-isit ,obj)) 4))	;; Fixed position of slot-accessor function

;;; Environment Stuff:

#+:ccl (defun %set-anonymous-function-name (function new-name)
         (cond ((ccl::lfunp function)
                (ccl::lfun-name function new-name))
               (t nil))
         function)

#+:excl (defun %set-anonymous-function-name (function new-name)
          (cond ((excl::function-object-p function)
                 (setf (excl::fn_symdef function) new-name))
                (t nil))
          function)

 #+CLISP (defun %set-anonymous-function-name (function new-name)
           (when (sys::closurep function)
             (sys::%record-store function 0 new-name))
           function)
 
 #-(or :ccl :excl CLISP)
   (defun %set-anonymous-function-name (function new-name)
      (declare (ignore new-name))
      function)


;;; Making classes, generic-functions, methods available for edit-definition:

#+:ccl(defun record-source-file (name type)
        (when ccl::*record-source-file*
          (unless (member (append (list type) 
                                  (list ccl::*loading-file-source-file*))
                          (get (find-symbol (symbol-name name) 
                                            (symbol-package name))
                               'ccl::%source-files)
                          :test #'equal)
            (ccl::record-source-file  name type 
                                      ccl::*loading-file-source-file*))))

;#+:ccl (defun record-source-file (name type)
;         (when ccl::*record-source-file*
;         (ccl::record-source-file  name type ccl::*loading-file-source-file*)))

#+:TI (defun record-source-file (function-spec type)
       (sys:record-source-file-name function-spec type))

#-(or :ccl :TI) 
  (defun record-source-file (name type)
    (declare (ignore name type)) nil)



;;; eof

