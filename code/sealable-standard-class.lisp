(in-package #:sealable-metaobjects)

(defclass sealable-standard-class
    (standard-class sealable-class)
  ())

(defclass sealable-standard-class-instance
    (standard-object sealable-class-instance)
  ())

(defmethod validate-superclass
    ((sealable-standard-class sealable-standard-class)
     (standard-class standard-class))
  t)
