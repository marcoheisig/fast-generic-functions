(in-package #:sealable-metaobjects)

(defvar *seal-methods-eagerly* t)

(defclass potentially-sealable-method (sealable-metaobject-mixin method)
  ())

(defmethod initialize-instance :after ((sm potentially-sealable-method) &key &allow-other-keys)
  (when (and *seal-methods-eagerly*
             (metaobject-sealable-p sm))
    (seal-method sm)))

(defmethod metaobject-sealable-p ((sm potentially-sealable-method))
  (every
   (lambda (specializer)
     (or (specializer-sealed-p specializer)
         (eq specializer (find-class 't))))
   (method-specializers sm)))

(defmethod seal-metaobject ((sm potentially-sealable-method))
  (if (metaobject-sealable-p sm)
      (call-next-method)
      (error "Attempt to seal a method with non-sealed specializers.")))
