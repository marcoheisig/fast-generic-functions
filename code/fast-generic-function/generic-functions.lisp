(in-package #:sealable-metaobjects)

(defgeneric optimize-function-call (generic-function static-call-signature))

(defgeneric compute-static-call-signatures (generic-function domain))

(defgeneric no-primary-method (generic-function &rest arguments)
  (:method ((generic-function generic-function) &rest arguments)
    (error "~@<No primary method for call to the generic function ~S with ~
             arguments ~S.~:@>"
           generic-function arguments)))

(defgeneric externalizable-object-p (object)
  (:method ((object t))
    (typep (class-of object) 'built-in-class))
  (:method ((structure-object structure-object)) t)
  (:method ((standard-object standard-object))
    (and (make-load-form standard-object) t)))
