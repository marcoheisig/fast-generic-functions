(in-package #:sealable-metaobjects)

(defclass sealable-class (sealable-metaobject-mixin class)
  ())

(defclass sealable-standard-class
    (standard-class sealable-class)
  ())

(defmethod validate-superclass
    ((sealable-standard-class sealable-standard-class)
     (standard-class standard-class))
  t)

(defclass sealable-class-instance (t)
  ()
  (:metaclass sealable-standard-class))

(defclass sealable-standard-class-instance
    (sealable-class-instance)
  ()
  (:metaclass sealable-standard-class))

;;; Ensure that each instance of a sealable class is a sealable instance.

(defmethod initialize-instance :after ((instance sealable-class) &key &allow-other-keys)
  ;; We cannot use typep here, because the inheritance of INSTANCE is not
  ;; yet finalized.
  (unless (inherits instance (find-class 'sealable-class-instance))
    (error "Sealable classes must inherit the class SEALABLE-CLASS-INSTANCE.")))

(defmethod initialize-instance :after ((instance sealable-standard-class) &key &allow-other-keys)
  ;; We cannot use typep here, because the inheritance of INSTANCE is not
  ;; yet finalized.
  (unless (inherits instance (find-class 'sealable-standard-class-instance))
    (error "Sealable standard classes must inherit the class SEALABLE-STANDARD-CLASS-INSTANCE.")))

(defun inherits (class other-class)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((scan (class)
               (unless (gethash class table)
                 (setf (gethash class table) t)
                 (when (eq class other-class)
                   (return-from inherits t))
                 (mapc #'scan (class-direct-superclasses class)))))
      (scan class))))

