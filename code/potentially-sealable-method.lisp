(in-package #:sealable-metaobjects)

;;; There is no portable way to add options to a method.  So instead, we
;;; allow programmers to declare METHOD-PROPERTIES.
;;;
;;; Example:
;;;
;;; (defmethod foo (x y)
;;;   (declare (method-properties inline))
;;;   (+ x y))

(declaim (declaration method-properties))

(defclass potentially-sealable-method (sealable-metaobject-mixin method)
  ((%method-properties
    :initarg .method-properties.
    :accessor method-properties
    :initform '())))

(defmethod metaobject-sealable-p ((psm potentially-sealable-method))
  (every #'specializer-sealed-p (method-specializers psm)))

(defmethod seal-metaobject :before ((psm potentially-sealable-method))
  (mapcar #'seal-specializer (method-specializers psm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived Classes

(defclass potentially-sealable-standard-method
    (standard-method potentially-sealable-method)
  ())
