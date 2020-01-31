(in-package #:sealable-metaobjects)

(defclass sealable-standard-generic-function
    (standard-generic-function sealable-generic-function)
  ()
  (:default-initargs :method-class (find-class 'potentially-sealable-standard-method))
  (:metaclass funcallable-standard-class))
