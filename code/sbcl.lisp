(in-package #:fast-generic-functions)

(defmethod seal-domain :after
    ((fast-generic-function fast-generic-function)
     (domain domain))
  (let ((name (generic-function-name fast-generic-function)))
    ;; Ensure that the function is known.
    (unless (sb-c::info :function :info name)
      (eval `(sb-c:defknown ,name * * ())))
    ;; Create an IR1-transform for each static call signature.
    (dolist (static-call-signature (compute-static-call-signatures fast-generic-function domain))
      (with-accessors ((types static-call-signature-types)
                       (prototypes static-call-signature-prototypes))
          static-call-signature
        (eval
         `(sb-c:deftransform ,name ((&rest args) (,@types &rest *))
            (or (optimize-function-call #',name ',static-call-signature)
                (sb-c::give-up-ir1-transform))))))))
