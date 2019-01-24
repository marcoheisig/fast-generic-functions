(in-package #:sealable-metaobjects)

(defvar *seal-methods-eagerly* t)

(defclass sealable-method (sealable-metaobject-mixin method)
  ())

(defmethod seal-metaobject :before ((sm sealable-method))
  (dolist (specializer (method-specializers sm))
    (unless (or (specializer-sealed-p specializer)
                (eq specializer (find-class 't)))
      (error "~@<Invalid sealed method specializer ~S.~:@>" specializer))))

(defmethod initialize-instance :after ((sm sealable-method) &key &allow-other-keys)
  (when *seal-methods-eagerly*
    (seal-method sm)))
