(in-package #:sealable-metaobjects)

(defgeneric fast-method-inline-lambda (fast-method))

(defgeneric optimize-function-call (generic-function static-call-signature))

(defgeneric compute-fast-lambda
    (generic-function static-call-signature applicable-methods))

(defgeneric externalizable-object-p (object)
  (:method ((structure-object structure-object)) t)
  (:method ((object t))
    (typep (class-of object) 'built-in-class))
  (:method ((standard-object standard-object))
    (and (make-load-form standard-object) t)))
