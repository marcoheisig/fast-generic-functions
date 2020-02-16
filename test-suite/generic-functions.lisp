(in-package #:sealable-metaobjects-test-suite)

(defgeneric generic-find (item sequence &key test)
  (:argument-precedence-order sequence)
  (:generic-function-class fast-generic-function))

(defgeneric generic-binary-+ (a b)
  (:generic-function-class fast-generic-function))

(defgeneric generic-binary-* (a b)
  (:generic-function-class fast-generic-function))

(defgeneric rest-args (a1 a2 &rest rest)
  (:argument-precedence-order a2)
  (:generic-function-class fast-generic-function))

