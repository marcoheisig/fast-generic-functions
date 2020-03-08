(in-package #:sealable-metaobjects)

(defmethod optimize-function-call
    ((fgf fast-generic-function)
     (scs static-call-signature))
  (compute-fast-lambda fgf scs))
