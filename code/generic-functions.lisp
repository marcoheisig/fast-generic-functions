(in-package #:sealable-metaobjects)

;;; Checking for sealability.

(defgeneric metaobject-sealable-p (metaobject)
  (:method ((class class)) nil)
  (:method ((generic-function generic-function)) nil)
  (:method ((method method)) nil))

(defgeneric class-sealable-p (class)
  (:method ((class class))
    (metaobject-sealable-p class)))

(defgeneric generic-function-sealable-p (generic-function)
  (:method ((generic-function generic-function))
    (metaobject-sealable-p generic-function)))

(defgeneric method-sealable-p (method)
  (:method ((method method))
    (metaobject-sealable-p method)))

;;; Checking for sealed-ness

(defgeneric metaobject-sealed-p (metaobject)
  (:method ((class class)) nil)
  (:method ((generic-function generic-function)) nil)
  (:method ((method method)) nil))

(defgeneric class-sealed-p (class)
  (:method ((class class))
    (metaobject-sealed-p class)))

(defgeneric generic-function-sealed-p (generic-function)
  (:method ((generic-function generic-function))
    (metaobject-sealed-p generic-function)))

(defgeneric method-sealed-p (method)
  (:method ((method method))
    (metaobject-sealed-p method)))

(defgeneric specializer-sealed-p (specializer)
  (:method ((class class)) nil)
  (:method ((eql-specializer eql-specializer))
    (specializer-sealed-p
     (class-of
      (eql-specializer-object eql-specializer)))))

;;; Sealing of metaobjects

(defgeneric seal-metaobject (metaobject)
  (:method ((object t))
    (declare (ignore object))
    (values)))

(defgeneric seal-class (class))

(defgeneric seal-generic-function (generic-function))

(defgeneric seal-method (method))

;;; Miscellaneous

(defgeneric generic-function-specializer-profile (generic-function))

(defgeneric method-body (method))

(defgeneric specializer-type (specializer)
  (:method ((class class))
    (class-name class))
  (:method ((eql-specializer eql-specializer))
    `(eql ,(eql-specializer-object eql-specializer))))

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
    (class-direct-superclasses
     (class-of
      (eql-specializer-object eql-specializer)))))

(defgeneric compute-method-inline-lambda (method lambda environment))

(defgeneric compute-generic-function-inline-lambda (generic-function applicable-methods))
