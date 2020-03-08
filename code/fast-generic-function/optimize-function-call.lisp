(in-package #:sealable-metaobjects)

(defmethod optimize-function-call
    ((fast-generic-function fast-generic-function)
     (static-call-signature static-call-signature))
  (compute-fast-lambda
   fast-generic-function
   static-call-signature
   (compute-applicable-methods
    fast-generic-function
    (static-call-signature-prototypes static-call-signature))))
