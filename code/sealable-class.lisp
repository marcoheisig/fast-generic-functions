(in-package #:sealable-metaobjects)

(defclass sealable-class (sealable-metaobject-mixin class)
  ())

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

(defmethod seal-metaobject :before ((class sealable-class-instance))
  (seal-class (class-of class)))

(defmethod metaobject-sealed-p ((class sealable-class-instance))
  (class-sealed-p (class-of class)))

(defmethod specializer-sealed-p ((sealable-class sealable-class))
  (class-sealed-p sealable-class))

(defmethod seal-metaobject :before ((class sealable-class))
  ;; Class sealing implies finalization.
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  ;; A sealed class must have sealed superclasses.
  (mapc #'seal-class (rest (class-precedence-list class))))

