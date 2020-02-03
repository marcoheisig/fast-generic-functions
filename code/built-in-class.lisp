(in-package #:sealable-metaobjects)

;;; Ensure that built-in classes are treated as sealed classes.

(defmethod metaobject-sealable-p ((built-in-class built-in-class))
  (declare (ignore built-in-class))
  t)

(defmethod metaobject-sealed-p ((built-in-class built-in-class))
  t)

(defmethod seal-class ((built-in-class built-in-class))
  (declare (ignore built-in-class))
  (values))

(defmethod specializer-sealed-p ((built-in-class built-in-class))
  (declare (ignore built-in-class))
  t)
