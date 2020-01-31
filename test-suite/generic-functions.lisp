(in-package #:sealable-metaobjects-test-suite)

(define-sealable-generic-function generic-find (item sequence &key test)
  (:argument-precedence-order sequence)
  (:generic-function-class inlineable-standard-generic-function))

(define-sealable-generic-function generic-binary-+ (a b)
  (:generic-function-class inlineable-standard-generic-function))

(define-sealable-generic-function generic-binary-* (a b)
  (:generic-function-class inlineable-standard-generic-function))

(define-sealable-generic-function rest-args (a1 a2 &rest rest)
  (:argument-precedence-order a2)
  (:generic-function-class inlineable-standard-generic-function))

