(in-package #:sealable-metaobjects)

(defclass inlineable-generic-function (sealable-generic-function)
  ()
  (:metaclass funcallable-standard-class))
