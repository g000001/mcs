;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: mcs -*-

;;           Copyright  1990    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;; -----------------------------------------------------------------------------------
;;;
;;; description:    The Effective Methods Computation at Load or Compile-File Time.
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

;(export '())

(defgeneric optimize-class-space (class))

;;; -----------------------------------------------------------------------------------
;;; Optimization of classes
;;; -----------------------------------------------------------------------------------

(defmacro optimize-classes (&rest class-names)
  `(progn
     ,@(mapcar 
        #'(lambda (class-name)
            (let ((class (find-class class-name 'signal-error)))
              `(setf (%%class-slot-accessor (find-class ',class-name))
                     (function ,(compute-slot-accessor-lambda 
                                 class (%%class-slots class))))))
        class-names)))

;;; -----------------------------------------------------------------------------------
;;; Optimization of generic functions
;;; -----------------------------------------------------------------------------------

(defmacro optimize-generic-functions (&rest gfn-names)
  `(progn
     ,@(mapcan 
        #'(lambda (gfn-name)
            (let ((gfn (find-gfn gfn-name 'signal-error)))
              (precompute-effective-methods-for gfn)
              `((setf (%%gfn-discriminating-function (find-gfn ',gfn-name))
                     ',(%%gfn-discriminating-function gfn))
                (setf (%%gfn-combined-methods (find-gfn ',gfn-name))
                      ,(make-load-form (%%gfn-combined-methods gfn))))))
        gfn-names)
     ',gfn-names))

(defmacro optimize-all-generic-functions ()
  `(progn
     ,@(let ((result ()))
         (map-gfns 
        #'(lambda (gfn)
            (let ((gfn-name (%%gfn-name gfn)))
              (precompute-effective-methods-for gfn)
              (push
                `(setf (%%gfn-combined-methods (find-gfn ',gfn-name))
                      ,(make-load-form (%%gfn-combined-methods gfn)))
                result))))
       result)))

(defun make-load-form (a-list)
  (labels ((traverse-a-list 
            (list-of-entries)
            (cond
             ((null list-of-entries) ())
             (t `(list ,@(mapcar #'traverse-entry list-of-entries)))))
           (traverse-entry (entry)
                           (cond
                            ((consp entry)
                             
                             `(cons ,(if (%object-p (car entry))
                                      `(find-class ',(%%class-name (car entry)))
                                      ())
                                    ,(traverse-a-list (cdr entry))))
                            (t `(identity ',entry)))))
    (traverse-a-list a-list)))

(defun compute-1-method-gfn-lambda (gfn method)
  ;(declare (ignore methods))
  (let ((lambda-list (%%gfn-lambda-list gfn))
        (signature (%%gfn-signature gfn))
        (class-checkers (get-type-checkers gfn)))
    (let ((all-arguments lambda-list)
          (required-arguments lambda-list))
      (when (mcs-memq '&rest lambda-list)
        (setf all-arguments (mcs-remq '&rest lambda-list))
        (setf required-arguments (butlast lambda-list 2)))
      `(lambda  ,lambda-list
           (declare (optimize (speed 3) (safety 1)))
           (let ()
             (declare (optimize (speed 3) (safety 0)))
             (if (and 
                  ,@(let ((specializers (%%method-specializers method))
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
               (funcall ',(%%method-function method) () ,@all-arguments)
               (no-applicable-methods ',(%%gfn-name gfn) ,@required-arguments)))))))

(defun optimize-1-method-gfn (gfn method)
  (compile (%%gfn-name gfn)
           (compute-1-method-gfn-lambda gfn method)))

(defun optimize-1-method-gfns ()
  (map-gfns #'(lambda (gfn)
                (when (= (length (%%gfn-methods gfn)) 1)
                  (print (%%gfn-name gfn))
                  (optimize-1-method-gfn gfn (car (%%gfn-methods gfn)))
                  ;(remhash (%%gfn-name gfn) *named-gfns*)
                  ))))
(defmacro optimize-class-space-except (&rest class-names)
  `(map-classes #'(lambda (cl)
                   (unless (mcs-memq (%%class-name cl) ',class-names)
                     (optimize-class-space cl)))))


(defun optimize-discriminating-fn (gfn)
  (let ((table (%%gfn-combined-methods gfn))
        (lambda-list (%%gfn-lambda-list gfn))
        (signature (%%gfn-signature gfn))
        (class-checkers (get-type-checkers gfn)))
    (let ((all-arguments lambda-list)
          (required-arguments lambda-list))
      (when (mcs-memq '&rest lambda-list)
        (setf all-arguments (mcs-remq '&rest lambda-list))
        (setf required-arguments (butlast lambda-list 2)))
      (setf (%%gfn-discriminating-function gfn)
            (compile ()
                     `(lambda ,(cons '%gfn lambda-list)
                        (declare (optimize (speed 3) (safety 0)))
                        ,(compute-case-lambda table signature required-arguments 
                                              required-arguments
                                              gfn class-checkers)) )
                     ))))

(defun optimize-generic-functions-with-many-methods ()
  (map-gfns #'(lambda (gfn)
                (when (> (length (%%gfn-methods gfn)) 1)
                  (print (%%gfn-name gfn))
                  (optimize-discriminating-fn gfn)))))


;;; -----------------------------------------------------------------------------------
;;; Library Functions
;;; -----------------------------------------------------------------------------------

(defun collect-chain (element next-fn)
  " (<element> <next-function>) ==> (<element>*)
    <element> ::= LISP object
    <next-function> ::= function : (<element>) ==> {<element> | nil}
    Example: (collect-chain 1 #'(lambda (x) (if (= x 11) nil (1+ x))))"
  (let ((next-element (funcall next-fn element)))
    (if (not next-element)
      nil
      (cons next-element (collect-chain next-element next-fn)))))

(defun collect-tree (element direct-successors-fn)
  " (<element> <next-function>) ==> (<element>*)
    <element> ::= LISP object
    <next-function> ::= function : (<element>) ==> (<element>*)
    Collects all successors of one element in the tree.
    Example: (collect-tree 1 #'(lambda (x)
                                 (if (> x 11) 
                                    nil 
                                    (list (+ x 2) (+ x 3)))))"
  (let ((next-elements (funcall direct-successors-fn element)))
    (if (not next-elements)
      nil
      (apply #'append next-elements 
             (mapcar #'(lambda (element)
                         (collect-tree element direct-successors-fn))
                     next-elements)))))

(defun collect-acyclic-directed-graph (element direct-successors-fn)
  " (<element> <next-function>) ==> (<element>*)
    <element> ::= LISP object
    <next-function> ::= function : (<element>) ==> (<element>*)
    Collects all successors of one element in an acyclic directed graph.
    Example: (collect-acyclic-directed-graph
               1 
               #'(lambda (x) (if (> x 11) nil (list (+ x 3) (+ x 1)))))"
  (remove-duplicates (collect-tree element direct-successors-fn) :test #'eq))

(defun compute-combinations (&rest lists)
  (labels ((comb-help (r-lists result)
                      (cond 
                       ((null r-lists) result)
                       (t (comb-help (rest r-lists)
                                     (apply #'append
                                            (mapcar #'(lambda (el)
                                                        (mapcar #'(lambda (l)
                                                                    (cons el l))
                                                                result))
                                                    (first r-lists))))))))
    (let ((r-lists (reverse lists)))
      (comb-help (rest r-lists) (mapcar #'list (first r-lists))))))

;;; -----------------------------------------------------------------------------------
;;; Precomputing the Effective Method at any time 
;;; -----------------------------------------------------------------------------------

(defun precompute-effective-method (gfn specializers)
  (declare (optimize (speed 3) (safety 0)))
  (let ((sorted-applicable-methods
         (select-applicable-methods gfn  specializers)))
    (when sorted-applicable-methods
      (save-effective-method 
       gfn specializers
       (compute-effective-method 
        gfn specializers (%%gfn-method-combination gfn) 
        sorted-applicable-methods))) ))

(defun class-subclasses (class)
  (declare (optimize (speed 3) (safety 0)))
  ;  (collect-acyclic-directed-graph class #'class-direct-subclasses)
  (let ((result nil))
    (maphash #'(lambda (key value)
                 (if (mcs-memq class (rest (%%class-precedence-list value)))
                   (push value result)))
             *named-classes*)
    (remove-duplicates result :test #'eq)))


(defun compute-effective-specializers (gfn)
  (declare (optimize (speed 3) (safety 0)))
  (let ((methods (generic-function-methods gfn))
        (signature (%%gfn-signature gfn)))
    (let ((result (make-list (length signature))))
      (dolist (method methods)
        (let ((specializers (method-specializers method))
              (pos 0))
          (mapcar #'(lambda (cl sign)
                      (when sign
                        (setf (nth pos result) (cons cl (nth pos result))))
                      (incf pos))
                  specializers signature)))
      (let ((pos 0))
        (dolist (domain result)
          (if domain
            ;; there is need for discrimination over the argument in question:
            (progn
              (setf domain (delete-duplicates domain :test #'eq))
              (setf (nth pos result) domain)
              (dolist (cl domain)
                (nconc domain (class-subclasses cl)))
              (setf domain (delete-duplicates domain :test #'eq))
              (setf (nth pos result)
                    (delete-if #'(lambda (cl)
                                   (%typep cl 'abstract))
                               domain)))
             ;; there is no need for discrimination over the argument in question:
            (setf (nth pos result) (list (find-class 't))))
          (incf pos)))
       ;(print result)
      (apply #'compute-combinations result))))

(defun precompute-effective-methods-for (gfn)
  (declare (optimize (speed 3) (safety 0)))
  ;(print gfn)
  
  (dolist (eff-specializers (compute-effective-specializers gfn))
    (unless (entry-exists-p gfn eff-specializers)
    (precompute-effective-method gfn eff-specializers))))

(defun precompute-all-effective-methods ()
  (declare (optimize (speed 3) (safety 0)))
  (maphash #'(lambda (key val)
               (precompute-effective-methods-for val)
               ;(compute-discriminating-fn val)
               )
           *named-gfns*))

(defun entry-exists-p (gfn eff-specializers)
  (let ((table (%%gfn-combined-methods gfn))
        (relevant-specializers 
           (mcs-remq nil
                     (mapcar #'(lambda (sign cl)
                                 (if sign cl sign))
                             (%%gfn-signature gfn) eff-specializers))))
    (labels ((help-fn (rest-table rest-specializers)
                      (cond
                       ((null rest-table) ())
                       ((null (rest rest-specializers))
                        (cdr (mcs-assq (first rest-specializers) rest-table)))
                        (t (help-fn (cdr (mcs-assq (first rest-specializers)
                                                   rest-table))
                                    (rest rest-specializers))))))
      (help-fn table relevant-specializers))))




;;; eof

