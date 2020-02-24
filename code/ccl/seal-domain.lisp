(in-package #:sealable-metaobjects)

(defmethod seal-domain :after
    ((fast-generic-function fast-generic-function)
     (domain list))
  (values))
