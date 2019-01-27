(in-package #:sealable-metaobjects)

(defvar *seal-classes-eagerly* t)

(defclass sealable-class (sealable-metaobject-mixin class)
  ((%seal-eagerly-p :initform *seal-classes-eagerly* :reader seal-eagerly-p)))

(defclass sealable-class-instance (t)
  ())

;;; Ensure that each instance of a sealable class is a sealable instance.

(defmethod initialize-instance :after ((instance sealable-class) &key &allow-other-keys)
  ;; We cannot use typep here, because the inheritance of INSTANCE is not
  ;; yet finalized.  So we use a custom replacement instead.
  (unless (inherits instance (find-class 'sealable-class-instance))
    (error "Sealable classes must inherit the class SEALABLE-CLASS-INSTANCE.")))

(defun inherits (class other-class)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((scan (class)
               (unless (gethash class table)
                 (setf (gethash class table) t)
                 (when (eq class other-class)
                   (return-from inherits t))
                 (mapc #'scan (class-direct-superclasses class)))))
      (scan class))))

;;; Ensure that a finalized sealable class is never mutated.

(defmethod change-class :before
    ((sealable-class sealable-class) new-class-name &key &allow-other-keys)
  (when (class-sealed-p sealable-class)
    (error "Attempt to change the class of a sealed class.")))

(defmethod reinitialize-instance :before
    ((sealable-class sealable-class) &key &allow-other-keys)
  (when (class-sealed-p sealable-class)
    (warn "Redefining a sealed class.")))

;;; Ensure that instances of sealed classes never have their class changed.

(defmethod change-class :before
    ((instance sealable-class-instance) new-class-name &key &allow-other-keys)
  (when (class-sealed-p (class-of instance))
    (error "Attempt to change the class of an instance of a sealed class.")))

(defmethod specializer-sealed-p ((sealable-class sealable-class))
  (class-sealed-p sealable-class))

(defmethod finalize-inheritance :before ((class sealable-class))
  (when (seal-eagerly-p class)
    (seal-class class)))
