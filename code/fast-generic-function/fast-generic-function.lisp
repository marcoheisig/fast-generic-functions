(in-package #:sealable-metaobjects)

(defclass fast-generic-function (sealable-standard-generic-function)
  ((%full-effective-method-cache :initform '() :accessor full-effective-method-cache)
   (%flat-effective-method-cache :initform '() :accessor flat-effective-method-cache))
  (:default-initargs
   :method-class (find-class 'fast-method))
  (:metaclass funcallable-standard-class))
