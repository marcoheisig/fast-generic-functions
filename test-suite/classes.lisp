(in-package #:sealable-metaobjects-test-suite)

(defclass color-mixin (sealable-standard-class-instance)
  ((%color :initarg :color :reader color))
  (:metaclass sealable-standard-class))

(defclass position-mixin (sealable-standard-class-instance)
  ((%x :initarg :x :reader x)
   (%y :initarg :y :reader y))
  (:metaclass sealable-standard-class))

(defclass point (color-mixin position-mixin)
  ()
  (:metaclass sealable-standard-class))

(seal-class (find-class 'point))

