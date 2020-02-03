(in-package #:sealable-metaobjects)

;;; Ensure that system classes are treated as sealed classes.

(defmethod metaobject-sealable-p ((system-class sb-pcl:system-class))
  (declare (ignore system-class))
  t)

(defmethod metaobject-sealed-p ((system-class sb-pcl:system-class))
  (every #'class-sealed-p (class-direct-subclasses system-class)))

;;; Cache sealed-p values of system-classes.
(defmethod metaobject-sealed-p :around ((system-class sb-pcl:system-class))
  (let ((table (load-time-value (make-hash-table :test #'eq))))
    (multiple-value-bind (value present-p)
        (gethash system-class table)
      (if present-p
          value
          (setf (gethash system-class table)
                (call-next-method))))))

(defmethod seal-class ((system-class sb-pcl:system-class))
  (declare (ignore system-class))
  (values))

(defmethod specializer-sealed-p ((system-class sb-pcl:system-class))
  (declare (ignore system-class))
  t)
