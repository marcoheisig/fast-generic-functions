(in-package #:sealable-metaobjects)

(defclass inlineable-standard-generic-function
    (inlineable-generic-function sealable-standard-generic-function)
  ()
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-class (find-class 'potentially-inlineable-standard-method)))
