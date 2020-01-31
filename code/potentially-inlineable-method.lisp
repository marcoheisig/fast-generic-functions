(in-package #:sealable-metaobjects)

(defclass potentially-inlineable-method (potentially-sealable-method)
  ((%inline-lambda
    :initarg .inline-lambda.
    :reader method-inline-lambda
    :initform nil)))

(defmethod make-method-lambda :around
    ((gf generic-function)
     (pim potentially-inlineable-method)
     lambda
     environment)
  (multiple-value-bind (method-lambda initargs)
      (call-next-method)
    (values
     method-lambda
     (list* '.inline-lambda.
            (compute-method-inline-lambda gf pim lambda environment)
            initargs))))
