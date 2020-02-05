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
