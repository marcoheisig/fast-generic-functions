(in-package #:sealable-metaobjects)

(defclass sealable-class (sealable-metaobject-mixin class)
  ())

;;; Ensure that each instance of a sealable class is a sealed instance,
;;; i.e., an object with a fixed class.

(defclass sealed-instance (t)
  ())

(defmethod change-class :around
    ((object sealed-instance) new-class &key &allow-other-keys)
  (error "Attempt to change the class of the sealed instance ~S."
         object))

(defmethod shared-initialize
    ((instance sealable-class)
     (slot-names (eql t))
     &rest initargs
     &key direct-superclasses)
  (unless (every #'class-sealable-p direct-superclasses)
    (error "~@<The superclasses of a sealable class must be sealable. ~
               The superclass ~S violates this restriction.~:@>"
           (find-if-not #'class-sealable-p direct-superclasses)))
  (apply #'call-next-method instance slot-names
         :direct-superclasses
         (adjoin (find-class 'sealed-instance) direct-superclasses)
         initargs))
