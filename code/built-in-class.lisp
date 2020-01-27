(in-package #:sealable-metaobjects)

;;; Ensure that built-in classes are treated like sealed classes.

(defmethod metaobject-sealable-p ((built-in-class built-in-class))
  (declare (ignore built-in-class))
  t)

(defmethod metaobject-sealed-p ((built-in-class built-in-class))
  (every #'class-sealed-p (class-direct-subclasses built-in-class)))

;;; Cache sealed-p values of built-in-classes.
(defmethod metaobject-sealed-p :around ((built-in-class built-in-class))
  (let ((table (load-time-value (make-hash-table :test #'eq))))
    (multiple-value-bind (value present-p)
        (gethash built-in-class table)
      (if present-p
          value
          (setf (gethash built-in-class table)
                (call-next-method))))))

(defmethod seal-class ((built-in-class built-in-class))
  (declare (ignore built-in-class))
  (values))

(defmethod specializer-sealed-p ((built-in-class built-in-class))
  (declare (ignore built-in-class))
  t)
