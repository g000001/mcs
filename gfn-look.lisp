;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    This is the Method Lookup File.
;;;
;;; notes:          
;;;
;;; contact:        Juergen Kopp, Harry Bretthauer
;;;
;;; history:
;;;          date:      author:             comments:
;;;          23.02.90   Harry Bretthauer    initial version
;;;          16.11.92   Harry Bretthauer    bug in sorting multimethods fixed
;;; -----------------------------------------------------------------------------------

(in-package "MCS")

;(export '())

;;; -----------------------------------------------------------------------------------
;;; DYNAMIC-LOOKUP                                                            Function
;;; -----------------------------------------------------------------------------------
;;; Method Lookup at runtime for given arguments (General message handler)

(defun dynamic-lookup (gfn spec-params)
  (declare (optimize (speed 3) (safety 0)))
  (let ((specializers (mapcar #'(lambda (arg sign) 
                                          (if sign
                                            (%discr-key arg)
                                            (find-class 't)))
                              spec-params (%%gfn-signature gfn))))
    (let ((sorted-applicable-methods (select-applicable-methods gfn  specializers)))
      (if sorted-applicable-methods
        (save-effective-method gfn 
                               specializers
                               (compute-effective-method gfn 
                                                         specializers 
                                                         (%%gfn-method-combination gfn) 
                                                         sorted-applicable-methods))
        (apply #'no-applicable-methods gfn spec-params)))))

;;; -----------------------------------------------------------------------------------
;;; COMPUTE-EFFECTIVE-METHOD 
;;; -----------------------------------------------------------------------------------
 
(defun compute-effective-method (gfn specializers method-combination methods)
  (declare (optimize (speed 3) (safety 0)))
  (or
  ;; lookup for a subsuming combined method
  (lookup-for-subsuming-eff-method gfn specializers (first methods))
  ;; the combined-method function has no &rest parameter (as method functions too)
  (let ((lambda-list (mcs-remq '&rest (%%gfn-lambda-list gfn))))
    (funcall (%%method-combination-operator method-combination)
             specializers methods lambda-list) )))

(defun lookup-for-subsuming-eff-method (gfn specializers most-specific-method)
  (declare (optimize (speed 3) (safety 0)))
  (let ((combined-methods (%%gfn-combined-methods gfn)))
    (when combined-methods
      (let ((method-signature (%%method-specializers most-specific-method))
            (gfn-signature (%%gfn-signature gfn))
            (relevant-signature ())
            (relevant-specializers ()))
        (dolist (cl gfn-signature)
          (if cl
            (progn 
              (push (pop method-signature) relevant-signature)
              (push (pop specializers) relevant-specializers))
            (progn
              (pop method-signature)
              (pop specializers))))
        (setf relevant-signature (reverse relevant-signature))
        (setf relevant-specializers (reverse relevant-specializers))
        (lookup-for-matching-entry1 (first relevant-specializers)
                                    (rest relevant-specializers)
                                    (first relevant-signature)
                                    (rest relevant-signature)
                                    combined-methods) ))))

(defun lookup-for-matching-entry1 
       (first-spec rest-spec first-sign rest-sign combined-methods)
  (declare (optimize (speed 3) (safety 0)))
  ;; nested a-list
  (if (null rest-sign) 
    (dolist (entry combined-methods)
        (if (and (subclassp first-spec (car entry))
                 (subclassp (car entry) first-sign))
          (return (cdr entry))))
   (dolist (entry combined-methods)
        (let ((result nil))
          (if (and (subclassp first-spec (car entry))
                   (subclassp (car entry) first-sign)
                   (setq result (lookup-for-matching-entry1 (first rest-spec) 
                                                            (rest rest-spec)
                                                            (first rest-sign) 
                                                            (rest rest-sign)
                                                            (cdr entry))))
            (return result))))))

(defun save-effective-method (gfn specializers effective-method)
  (declare (optimize (speed 3) (safety 0)))
  (add-combined-method-to-classes specializers (%%gfn-name gfn))
  (let ((signature (%%gfn-signature gfn))
        (relevant-specializers ()))
    (dolist (cl signature)
      (if cl
        (push (pop specializers) relevant-specializers)
        (pop specializers)))
    (add-combined-method gfn (reverse relevant-specializers) effective-method))
  effective-method)

(defun add-combined-method-to-classes (specializers gfn-name)
  (declare (optimize (speed 3) (safety 0)))
  (dolist (specializer specializers)
    (if (mcs%typep specializer (find-class 'instantiable-redefinable))
      (setf (slot-value specializer 'effective-methods)
            (acons gfn-name specializers
                   (remove (cons gfn-name specializers)
                           (slot-value specializer 'effective-methods)
                           :test #'equal))))))

;;; -----------------------------------------------------------------------------------
;;; SELECT-APPLICABLE-METHODS                           General Method Lookup Function
;;; -----------------------------------------------------------------------------------

(defun select-applicable-methods (gfn key-list)
  (let ((applicable-methods ()))
    (dolist (method (%%gfn-methods gfn))
      (when (applicable-p method key-list)
        (push method applicable-methods)))
    (sort-methods applicable-methods key-list)))

(defun applicable-p (method key-list)
  (dolist (specializer (%%method-specializers method) t)
    (unless (subclassp (pop key-list) specializer)
      (return ()))))

(defun sort-methods (methods key-list)
  (let ((m-length (length methods)))
    (if (= m-length 1)
      methods
      (let ((cplists (mapcar #'(lambda (cl)
                                 (%%class-precedence-list cl))
                             key-list))
            (s-length (length key-list)))
        (sort methods
              #'(lambda (m1 m2)
                  (let ((pos 0)
                        (specializers1 (%%method-specializers m1))
                        (specializers2 (%%method-specializers m2)))
                    (loop
                      (if (= pos s-length) (return nil))
                      (let ((cplist (nth pos cplists)))
                        (cond
                         ((< (position (nth pos specializers1) cplist)
                             (position (nth pos specializers2) cplist))
                          (return t))
                         ((< (position (nth pos specializers2) cplist)
                             (position (nth pos specializers1) cplist))
                          (return '()))
                         (t (incf pos))) ))))) ))))

;;; -----------------------------------------------------------------------------------
;;; STANDARD-OPERATOR                                                         Function
;;; -----------------------------------------------------------------------------------

;;; Computes the effective method, which is a list, specified as follows:

;;; <effective-method> ::=
;;;  (<function>+ [#((<primary-function>+) (<after-function>*) (before-function>*))])

;;;  <function> ::= <primary-function> | <around-function> | <operator-function>

;;;  <operator-function> ::= #'with-demons-operator |
;;;                          #'with-before-operator |
;;;                          #'with-after-operator

(defun standard-operator (specializers applicable-methods args) 
  (declare (optimize (speed 3) (safety 0))
           (ignore args))
  (let ((r-arounds nil) (r-befores nil) (r-primaries nil) (r-afters nil))
    (dolist (method applicable-methods)
      (case (first (%%method-qualifiers method))
        ((:around) (setq r-arounds (cons (%%method-function method) r-arounds)))
        ((:before) (setq r-befores (cons (%%method-function method) r-befores)))
        ((nil) (setq r-primaries (cons (%%method-function method) r-primaries)))
        ((:after) (setq r-afters (cons  (%%method-function method) r-afters)))
        (t (error "Method Combination: illegal qualifier ~S in ~S."
                  (first (%%method-qualifiers method)) method))))
    (unless r-primaries
      (error "Method Combination: no primary method for ~S in ~S." 
             specializers (find-gfn (%%method-name (first applicable-methods)))))
    (let ((demons 
           (cond
            ((and r-befores r-afters)
             (list #'with-demons-operator 
                   (vector (reverse r-primaries) r-afters (reverse r-befores))))
            (r-afters 
             (list #'with-after-operator 
                   (vector (reverse r-primaries) r-afters)))
            (r-befores 
             (list #'with-before-operator 
                   (vector (reverse r-primaries) () (reverse r-befores))))
            (t (reverse r-primaries)))))
      (if r-arounds 
        (append (reverse  r-arounds) demons)
         demons))))

(defmacro primary-of (applicable-methods) `(svref ,applicable-methods 0))
(defmacro after-of (applicable-methods) `(svref ,applicable-methods 1))
(defmacro before-of (applicable-methods) `(svref ,applicable-methods 2))

(defun with-demons-operator (operator-and-applicable-methods &rest args)
  (declare (optimize (speed 3) (safety 0)))
  (let ((applicable-methods (cadr operator-and-applicable-methods)))
    (let ((primary-methods (primary-of applicable-methods)))
      (dolist (method (before-of applicable-methods)) (apply method args))
      (let ((result (apply (car primary-methods) primary-methods args)))
        (dolist (method (after-of applicable-methods)) (apply method args))
        result))))

(defun with-before-operator (operator-and-applicable-methods &rest args)
  (declare (optimize (speed 3) (safety 0)))
  (let ((applicable-methods (cadr operator-and-applicable-methods)))
    (let ((primary-methods (primary-of applicable-methods)))
      (dolist (method (before-of applicable-methods)) (apply method args))
      (apply (car primary-methods) primary-methods args))))

(defun with-after-operator (operator-and-applicable-methods &rest args)
  (declare (optimize (speed 3) (safety 0)))
  (let ((applicable-methods (cadr operator-and-applicable-methods)))
    (let ((primary-methods (primary-of applicable-methods)))
      (let ((result (apply (car primary-methods) primary-methods args)))
        (dolist (method (after-of applicable-methods)) (apply method args))
        result))))

;;; -----------------------------------------------------------------------------------
;;; The standard method combination object                                    Instance
;;; -----------------------------------------------------------------------------------
 
(let ((standard-combination
       (make-instance (find-class 'method-combination)
                      'name 'standard
                      'order 'most-specific-first
                      'operator #'standard-operator
                      'identity-with-one-argument t
                      'documentation "Standard method combination object.")))

  (insert-method-combination nil standard-combination)
  (insert-method-combination '(:before) standard-combination)
  (insert-method-combination '(:after) standard-combination)
  (insert-method-combination '(:around) standard-combination)
  (insert-method-combination 'standard standard-combination)

  (insert-qualifiers '(:before :after :around)))



;;; eof

