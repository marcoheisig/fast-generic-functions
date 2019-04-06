(in-package #:sealable-metaobjects)

(defclass sealable-metaobject-mixin ()
  ((%sealed-p :initform nil :reader metaobject-sealed-p))
  (:documentation ""))

(defmethod metaobject-sealable-p ((metaobject sealable-metaobject-mixin))
  t)

(defmethod seal-metaobject ((metaobject sealable-metaobject-mixin))
  (debug-format "Sealing the metaobject ~S~%" metaobject)
  (setf (slot-value metaobject '%sealed-p) t))

(defmethod seal-metaobject :around ((metaobject sealable-metaobject-mixin))
  (unless (metaobject-sealed-p metaobject)
    (call-next-method))
  metaobject)

(defmethod change-class :around
    ((metaobject sealable-metaobject-mixin) new-class-name &key &allow-other-keys)
  (cond ((metaobject-sealed-p metaobject)
         (warn "Attempt to change the class of the sealed metaobject ~S" metaobject)
         metaobject)
        (t (call-next-method))))

(defmethod reinitialize-instance :around
    ((metaobject sealable-metaobject-mixin) &key &allow-other-keys)
  (cond ((metaobject-sealed-p metaobject)
         (warn "Attempt to reinitialize the sealed metaobject ~S" metaobject)
         metaobject)
        (t (call-next-method))))
