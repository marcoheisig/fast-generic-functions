(in-package #:sealable-metaobjects)

(defclass inlineable-generic-function (sealable-generic-function)
  ()
  (:default-initargs :method-class (find-class 'potentially-inlineable-method))
  (:metaclass funcallable-standard-class))
