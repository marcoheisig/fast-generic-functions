(in-package #:sealable-metaobjects)

(defclass sealable-metaobject-mixin ()
  ((%sealed-p :initform nil :reader metaobject-sealed-p))
  (:documentation ""))

(defmethod metaobject-sealable-p ((metaobject sealable-metaobject-mixin))
  t)

(defmethod seal-metaobject ((metaobject sealable-metaobject-mixin))
  (debug-format "~&Sealing ~S~%"
                (etypecase metaobject
                  (class (class-name metaobject))
                  (generic-function (generic-function-name metaobject))
                  (method (list* (if (method-generic-function metaobject)
                                     (generic-function-name (method-generic-function metaobject))
                                     metaobject)
                                 (mapcar #'specializer-type (method-specializers metaobject))))
                  (t metaobject)))
  (setf (slot-value metaobject '%sealed-p) t))

(defmethod seal-metaobject :around ((metaobject sealable-metaobject-mixin))
  (unless (metaobject-sealed-p metaobject)
    (call-next-method)))

(defmethod change-class :before
    ((metaobject sealable-metaobject-mixin) new-class-name &key &allow-other-keys)
  (when (metaobject-sealed-p metaobject)
    (error "Attempt to change the class of the sealed metaobject ~S."
           metaobject)))
