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

(defgeneric specializer-prototype (specializer &optional excluded-specializers)
  (:documentation
   "Returns an object that is of the type indicated by SPECIALIZER, but not
of any of the types indicated the optionally supplied
EXCLUDED-SPECIALIZERS.  Returns a secondary value of T if such an object
could be determined, and NIL if no such object was found.

Examples:
 (specializer-prototype
   (find-class 'double-float))
 => 5.0d0, T

 (specializer-prototype
   (find-class 'double-float)
   (list (intern-eql-specializer 5.0d0)))
 => 6.0d0, T

 (specializer-prototype
   (find-class 'real)
   (list (find-class 'rational) (find-class 'float)))
 => NIL, NIL
"))

(defgeneric specializer-direct-superspecializers (specializer)
  (:method ((class class))
    (class-direct-superclasses class))
  (:method ((eql-specializer eql-specializer))
    (list
     (class-of
      (eql-specializer-object eql-specializer)))))
