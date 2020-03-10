(in-package #:sealable-metaobjects)

(defgeneric fast-method-inline-lambda (fast-method))

(defgeneric optimize-function-call (generic-function static-call-signature))

(defgeneric externalizable-object-p (object)
  (:method ((object t))
    (typep (class-of object) 'built-in-class))
  (:method ((structure-object structure-object)) t)
  (:method ((standard-object standard-object))
    (and (make-load-form standard-object) t))
  (:method ((static-call-signature static-call-signature))
    (and
     (every #'externalizable-object-p
            (static-call-signature-types static-call-signature))
     (every #'externalizable-object-p
            (static-call-signature-prototypes static-call-signature)))))
