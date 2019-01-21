(in-package #:sealable-metaobjects)

(defclass sealable-standard-generic-function
    (standard-generic-function sealable-generic-function)
  ()
  (:metaclass funcallable-standard-class))
