(in-package #:sealable-metaobjects)

(defmethod compute-fast-lambda :around
    ((generic-function generic-function)
     (static-call-signature static-call-signature))
  (let ((form (call-next-method)))
    (debug-format "~&Creating a fast effective method for ~S:~%~S~%"
                  (generic-function-name generic-function)
                  form)
    form))
