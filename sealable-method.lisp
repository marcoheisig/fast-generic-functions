(in-package #:sealable-metaobjects)

(defclass sealable-method (sealable-metaobject-mixin method)
  ())

(defmacro define-sealed-method (name &body body)
  `(seal-method (defmethod ,name ,@body)))

(defmethod seal-metaobject :before ((sm sealable-method))
  (dolist (specializer (method-specializers sm))
    (unless (or (specializer-sealed-p specializer)
                (eq specializer (find-class 't)))
      (error "Invalid sealed method specializer ~S." specializer))))
