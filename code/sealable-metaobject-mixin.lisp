(in-package #:sealable-metaobjects)

(defclass sealable-metaobject-mixin ()
  ((%sealed-p :initform nil :reader metaobject-sealed-p))
  (:documentation ""))

(defmethod metaobject-sealable-p ((metaobject sealable-metaobject-mixin))
  t)

(defmethod seal-metaobject ((metaobject sealable-metaobject-mixin))
  (setf (slot-value metaobject '%sealed-p) t))

;; Invoke the primary methods on SEAL-METAOBJECT exactly once.
(defmethod seal-metaobject :around ((metaobject sealable-metaobject-mixin))
  (unless (metaobject-sealed-p metaobject)
    (call-next-method)))

;;; It is an error to change the class of a sealed metaobject.
(defmethod change-class :around
    ((metaobject sealable-metaobject-mixin) new-class &key &allow-other-keys)
  (if (metaobject-sealed-p metaobject)
      (error "Attempt to change the class of the sealed metaobject ~S."
             metaobject)
      (call-next-method)))

;;; It is an error to change any object's class to a sealed metaobject.
(defmethod update-instance-for-different-class :around
    (previous (current sealable-metaobject-mixin) &key &allow-other-keys)
  (error "Attempt to change the class of ~S to the sealable metaobject ~S."
         previous (class-of current)))

;;; Attempts to reinitialize a sealed metaobject are silently ignored.
(defmethod reinitialize-initialize :around
    ((metaobject sealable-metaobject-mixin) &key &allow-other-keys)
  (unless (metaobject-sealed-p metaobject)
    (call-next-method)))
