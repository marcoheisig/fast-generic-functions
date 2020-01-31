(in-package #:sealable-metaobjects)

(defclass inlineable-standard-generic-function
    (inlineable-generic-function standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))
