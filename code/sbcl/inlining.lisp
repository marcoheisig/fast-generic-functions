(in-package #:sealable-metaobjects)

(defmethod seal-domain :after
    ((fast-generic-function fast-generic-function)
     (domain list))
  (let ((name (generic-function-name fast-generic-function)))
    ;; Ensure that the function is known.
    (eval `(sb-c:defknown ,name * * () :overwrite-fndb-silently t))
    ;; Create an IR1-transform for each static call signature.
    (mapc
     (lambda (call-signature)
       (eval
        (make-deftransform fast-generic-function call-signature)))
     (compute-static-call-signatures fast-generic-function domain))))

(defun make-deftransform (generic-function static-call-signature)
  (with-accessors ((name generic-function-name)) generic-function
    (with-accessors ((types static-call-signature-types)
                     (prototypes static-call-signature-prototypes)) static-call-signature
      (debug-format "~&Creating deftransform for ~S~{ ~S~}~%" name types)
      `(sb-c:deftransform ,name ((&rest args) (,@types &rest *))
         (fast-generic-function-lambda #',name ',static-call-signature)))))
