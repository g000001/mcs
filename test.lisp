(cl:defpackage :mcs-test
  (:use :mcs))

(cl:in-package :mcs-test)

;; 3.4 A short Example
;;; define an abstract class graphical-object
(defabstract graphical-Object () ())

;;; define a base class point
(defclass Point (graphical-Object)
  ((x :initform 0
      :initarg x
      :accessor point-x
      :type integer)
   (y :initform 0
      :initarg y
      :accessor point-y
      :type integer)))

;;; the protocol for colored objects
(defgeneric color-of (obj))
(defgeneric change-color (obj color))

;;; define a mixin-class colored
(defmixin colored ()
  ((color :initform 'black :initarg color)))

;;; default methods for uncolored objects
(defmethod color-of (obj)
  'gray)

(defmethod change-color (obj color)
  (warn "Can't change the color of an uncolored object."))

;;; methods for colored objects
(defmethod color-of ((obj colored))
  (slot-value obj 'color))

(defmethod change-color ((obj colored) color)
  (setf (slot-value obj 'color) color))

;;; the protocol for moveable objects
(defgeneric move-object (obj x y))
;;; define a mixin-class moveable
(defmixin moveable ()
  ((x :accessor object-x :type integer)
   (y :accessor object-y :type integer)))

;;; default methods for unmoveable object
(defmethod move-object (obj x y)
  (warn "Can't move an unmoveable object."))

;;; methods for moveable object
(defmethod move-object ((obj moveable) x y)
  (setf (object-x obj) x)
  (setf (object-y obj) y)
  obj)

;;; define special point classes
(defclass colored-Point (colored Point) ())
(defclass moveable-colored-Point (moveable colored Point) ())

;;; create some instances of point classes
(let ((p1 (make-instance 'Point 'x 25 'y 10))
      (p2 (make-instance 'colored-Point
                         'x 1 'y 1 'color 'red))
      (p3 (make-instance 'moveable-colored-Point
                         'color 'blue)))
  (list p1 p2 p3))


;;; ================================================================

(defclass foo () 
  ()
  (:metaclass redefinable-base-class))


(defclass bar (foo) 
  ()
  (:metaclass redefinable-base-class))


(defmethod foo ((o foo))
  "Hello foo!")

(defmethod foo ((o bar))
  "Hello bar!")

(typep (find-class 'foo) 
       'redefinable-base-class)

(obj-describe (find-class 'foo))


(foo (change-class (make-instance 'bar) (find-class 'foo)))

(obj-describe (make-instance 'bar))


;;; eof
