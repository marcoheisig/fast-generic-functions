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

(defclass potentially-inlineable-method (potentially-sealable-method)
  ((%inline-lambda
    :initarg .inline-lambda.
    :reader method-inline-lambda
    :initform nil)))

(defmethod make-method-lambda :around
    ((gf generic-function)
     (pim potentially-inlineable-method)
     lambda
     environment)
  (multiple-value-bind (method-lambda initargs)
      (call-next-method)
    (values
     method-lambda
     (list* '.inline-lambda.
            (compute-method-inline-lambda gf pim lambda environment)
            initargs))))

(defclass potentially-inlineable-standard-method
    (potentially-inlineable-method standard-method)
  ())
