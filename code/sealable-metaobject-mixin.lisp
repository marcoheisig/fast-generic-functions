(in-package #:sealable-metaobjects)

(defclass sealable-metaobject-mixin ()
  ((%sealed-p :initform nil :reader metaobject-sealed-p))
  (:documentation ""))

(defmethod metaobject-sealable-p ((metaobject sealable-metaobject-mixin))
  t)

(defmethod seal-metaobject ((metaobject sealable-metaobject-mixin))
  (setf (slot-value metaobject '%sealed-p) t))

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

;;; It is an error to change the class of an instance of a sealable
;;; metaobject.

(defclass sealable-metaobject-instance (t)
  ())

(defmethod change-class :around
    ((instance sealable-metaobject-instance) new-class &key &allow-other-keys)
  (error "Attempt to change the class of the sealable metaobject instance ~S."
         instance))

(defmethod shared-initialize
    ((instance sealable-metaobject-mixin)
     (slot-names (eql t))
     &rest initargs
     &key direct-superclasses)
  (unless (every #'class-sealable-p direct-superclasses)
    (error "~@<The superclasses of a sealable metaobject must be sealable. ~
               The superclass ~S violates this restriction.~:@>"
           (find-if-not #'class-sealable-p direct-superclasses)))
  (apply #'call-next-method instance slot-names
         :direct-superclasses
         (adjoin (find-class 'sealable-metaobject-instance) direct-superclasses)
         initargs))
