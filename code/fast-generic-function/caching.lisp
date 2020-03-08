(in-package #:sealable-metaobjects)

(defvar *fast-lambda-cache* (make-hash-table))

(defmethod optimize-function-call
    ((generic-function generic-function)
     (static-call-signature static-call-signature))
  )
