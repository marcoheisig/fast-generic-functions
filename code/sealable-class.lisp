(in-package #:sealable-metaobjects)

(defvar *seal-classes-eagerly* t)

(defclass sealable-class (sealable-metaobject-mixin class)
  ())

(defclass sealable-class-instance (t)
  ())

;;; Ensure that each instance of a sealable class is a sealable instance.

(defmethod initialize-instance :after ((instance sealable-class) &key &allow-other-keys)
  (unless (typep instance 'sealable-class-instance)
    (error "Sealable classes must inherit the class SEALABLE-CLASS-INSTANCE.")))

;;; Ensure that a finalized sealable class is never mutated.

(defmethod change-class :before
    ((sealable-class sealable-class) new-class-name &key &allow-other-keys)
  (when (class-sealed-p sealable-class)
    (error "Attempt to change the class of a sealed class.")))

(defmethod reinitialize-instance :before
    ((sealable-class sealable-class) &key &allow-other-keys)
  (when (class-sealed-p sealable-class)
    (error "Attempt to redefine a sealed class.")))

;;; Ensure that instances of sealed classes never have their class changed.

(defmethod change-class :before
    ((instance sealable-class-instance) new-class-name &key &allow-other-keys)
  (when (class-sealed-p (class-of instance))
    (error "Attempt to change the class of an instance of a sealed class.")))

(defmethod specializer-sealed-p ((sealable-class sealable-class))
  (class-sealed-p sealable-class))

(defmethod finalize-inheritance :before ((class sealable-class))
  (when *seal-classes-eagerly*
    (seal-class class)))
