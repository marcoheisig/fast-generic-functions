(in-package #:sealable-metaobjects)

(defvar *seal-methods-eagerly* t)

(defclass sealable-method (sealable-metaobject-mixin method)
  ())

(defmethod initialize-instance :after ((sm sealable-method) &key &allow-other-keys)
  (when *seal-methods-eagerly*
    (seal-method sm)))

;;; Surprise!  Sealable methods are not always sealable.
(defmethod metaobject-sealable-p ((sm sealable-method))
  (every
   (lambda (specializer)
     (or (specializer-sealed-p specializer)
         (eq specializer (find-class 't))))
   (method-specializers sm)))

(defmethod seal-metaobject ((sm sealable-method))
  (when (metaobject-sealable-p sm)
    (call-next-method)))
