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

(defmethod metaobject-sealable-p ((sm potentially-sealable-method))
  (every
   (lambda (specializer specializing-p)
     (or (not specializing-p)
         (and (specializer-sealed-p specializer)
              (not (eq specializer (find-class 't))))))
   (method-specializers sm)
   (method-specializer-profile sm)))

(defmethod seal-metaobject :before ((sm potentially-sealable-method))
  (mapc #'seal-class (method-specializers sm)))
