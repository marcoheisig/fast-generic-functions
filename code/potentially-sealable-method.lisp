(in-package #:sealable-metaobjects)

(defclass potentially-sealable-method (sealable-metaobject-mixin method)
  ((%inline-lambda
    :initform nil
    :initarg inline-lambda
    :reader method-inline-lambda)
   (%specializer-profile
    :initform (error "No specializer profile supplied.")
    :initarg specializer-profile
    :accessor method-specializer-profile)))

(defmethod metaobject-sealable-p ((psm potentially-sealable-method))
  (every
   (lambda (specializer specializing-p)
     (or (not specializing-p)
         (specializer-sealed-p specializer)))
   (method-specializers psm)
   (method-specializer-profile psm)))

(defmethod seal-method ((psm potentially-sealable-method))
  (seal-metaobject psm))

(defmethod seal-metaobject :before ((psm potentially-sealable-method))
  (loop for specializer in (method-specializers psm)
        for specializing-p in (method-specializer-profile psm) do
          (when specializing-p
            (seal-class specializer))))
