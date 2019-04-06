(in-package #:sealable-metaobjects-test-suite)

(define-sealable-generic-function generic-find (item sequence &key test)
  (:argument-precedence-order sequence))

(define-sealable-generic-function generic-binary-+ (a b))

(define-sealable-generic-function generic-binary-* (a b))

(define-sealable-generic-function rest-args (a1 a2 &rest rest)
  (:argument-precedence-order a2))

