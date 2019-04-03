(in-package #:sealable-metaobjects)

(defvar *seal-methods-eagerly* t)

(defclass potentially-sealable-method (sealable-metaobject-mixin method)
  ((%inline-lambda :initarg %inline-lambda :reader method-inline-lambda)))

(defmethod add-method :after ((gf generic-function) (sm potentially-sealable-method))
  (when (and *seal-methods-eagerly*
             (metaobject-sealable-p sm))
    (seal-method sm)))

(defmethod metaobject-sealable-p ((sm potentially-sealable-method))
  (every
   (lambda (specializer specializing-p)
     (or (specializer-sealed-p specializer)
         (and (not specializing-p)
              (eq specializer (find-class 't)))))
   (method-specializers sm)
   (generic-function-specializer-profile (method-generic-function sm))))

(defmethod seal-metaobject ((sm potentially-sealable-method))
  (if (metaobject-sealable-p sm)
      (call-next-method)
      (error "Attempt to seal a method with non-sealed specializers.")))
