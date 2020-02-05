(in-package #:sealable-metaobjects)

;;; Ensure that system classes are treated as sealed classes.

(defmethod metaobject-sealable-p ((system-class sb-pcl:system-class))
  (declare (ignore system-class))
  t)

(defmethod metaobject-sealed-p ((system-class sb-pcl:system-class))
  t)

(defmethod seal-metaobject ((system-class sb-pcl:system-class))
  (values))
