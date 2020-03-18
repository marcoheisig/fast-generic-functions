(in-package #:fast-generic-functions)

(defmethod seal-domain :after
    ((fast-generic-function fast-generic-function)
     (domain list))
  (values))
