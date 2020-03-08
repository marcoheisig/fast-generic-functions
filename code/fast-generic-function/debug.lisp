(in-package #:sealable-metaobjects)

(defmethod optimize-function-call :around
    ((generic-function generic-function)
     (static-call-signature static-call-signature))
  (let ((form (call-next-method)))
    (debug-format "~&Optimizing #'~S:~%~S~%"
                  (generic-function-name generic-function)
                  form)
    form))
