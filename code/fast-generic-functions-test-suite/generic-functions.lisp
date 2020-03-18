(in-package #:fast-generic-functions-test-suite)

(defgeneric generic-find (item sequence &key test)
  (:argument-precedence-order sequence item)
  (:generic-function-class fast-generic-function))

(defgeneric generic-binary-+ (a b)
  (:generic-function-class fast-generic-function))

(defgeneric generic-binary-* (a b)
  (:generic-function-class fast-generic-function))

(defgeneric rest-args (a1 a2 &rest rest)
  (:argument-precedence-order a2 a1)
  (:generic-function-class fast-generic-function))

(defgeneric crazy-next-method-caller (a b)
  (:generic-function-class fast-generic-function))

(defgeneric keyword-function (x &key y z)
  (:generic-function-class fast-generic-function))
