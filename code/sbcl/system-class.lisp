(in-package #:sealable-metaobjects)

;;; Ensure that system classes are treated as sealed classes.

(defmethod metaobject-sealable-p ((system-class sb-pcl:system-class))
  (declare (ignore system-class))
  t)

(defmethod metaobject-sealed-p ((system-class sb-pcl:system-class))
  t)

(defmethod seal-class ((system-class sb-pcl:system-class))
  (declare (ignore system-class))
  (values))

(defmethod specializer-sealed-p ((system-class sb-pcl:system-class))
  (declare (ignore system-class))
  t)
