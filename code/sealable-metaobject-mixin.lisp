(in-package #:sealable-metaobjects)

(defclass sealable-metaobject-mixin ()
  ((%sealed-p :initform nil :reader metaobject-sealed-p))
  (:documentation ""))

(defmethod metaobject-sealable-p ((metaobject sealable-metaobject-mixin))
  t)

(defmethod seal-metaobject ((metaobject sealable-metaobject-mixin))
  (setf (slot-value metaobject '%sealed-p) t))

(defmethod seal-metaobject :around ((metaobject sealable-metaobject-mixin))
  (call-next-method)
  metaobject)
