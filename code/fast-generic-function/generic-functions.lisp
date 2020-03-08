(in-package #:sealable-metaobjects)

(defgeneric fast-method-inline-lambda (fast-method))

(defgeneric optimize-function-call (generic-function static-call-signature))

(defgeneric compute-fast-lambda (generic-function static-call-signature))

