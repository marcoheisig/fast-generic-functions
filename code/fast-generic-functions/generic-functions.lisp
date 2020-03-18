(in-package #:fast-generic-functions)

(defgeneric optimize-function-call (generic-function static-call-signature))

(defgeneric no-primary-method (generic-function &rest arguments)
  (:method ((generic-function generic-function) &rest arguments)
    (error "~@<No primary method for call to the generic function ~S with ~
             arguments ~S.~:@>"
           generic-function arguments)))

