(in-package #:sealable-metaobjects)

(defclass potentially-sealable-method (sealable-metaobject-mixin method)
  ((%specializer-profile
    :initarg .specializer-profile.
    :accessor method-specializer-profile
    :initform (required-argument '.specializer-profile.))))

(defmethod metaobject-sealable-p ((psm potentially-sealable-method))
  (every
   (lambda (specializer specializing-p)
     (or (not specializing-p)
         (specializer-sealed-p specializer)))
   (method-specializers psm)
   (method-specializer-profile psm)))

(defmethod seal-metaobject :before ((psm potentially-sealable-method))
  (loop for specializer in (method-specializers psm)
        for specializing-p in (method-specializer-profile psm) do
          (when specializing-p
            (seal-specializer specializer))))

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
