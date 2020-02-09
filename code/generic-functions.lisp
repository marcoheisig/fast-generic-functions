(in-package #:sealable-metaobjects)

;;; Checking for sealability.

(defgeneric metaobject-sealable-p (metaobject)
  (:method ((class class)) nil)
  (:method ((generic-function generic-function)) nil)
  (:method ((method method)) nil)
  (:method ((built-in-class built-in-class)) t))

(defgeneric class-sealable-p (class)
  (:method ((class class))
    (metaobject-sealable-p class)))

(defgeneric generic-function-sealable-p (generic-function)
  (:method ((generic-function generic-function))
    (metaobject-sealable-p generic-function)))

(defgeneric method-sealable-p (method)
  (:method ((method method))
    (metaobject-sealable-p method)))

(defgeneric specializer-sealable-p (specializer)
  (:method ((class class))
    (class-sealable-p class))
  (:method ((eql-specializer eql-specializer))
    (class-sealable-p
     (class-of
      (eql-specializer-object eql-specializer)))))

;;; Checking for sealed-ness

(defgeneric metaobject-sealed-p (metaobject)
  (:method ((class class)) nil)
  (:method ((generic-function generic-function)) nil)
  (:method ((method method)) nil)
  (:method ((built-in-class built-in-class)) t))

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
  (:method ((class class))
    (class-sealed-p class))
  (:method ((eql-specializer eql-specializer))
    (specializer-sealed-p
     (class-of
      (eql-specializer-object eql-specializer)))))

;;; Sealing of metaobjects

(defgeneric seal-metaobject (metaobject)
  (:method :before ((metaobject t))
    (assert (metaobject-sealable-p metaobject)))
  (:method :before ((class class))
    ;; Class sealing implies finalization.
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    ;; A sealed class must have sealed superclasses.
    (mapc #'seal-class (rest (class-precedence-list class)))))

(defgeneric seal-class (class)
  (:method ((symbol symbol))
    (seal-metaobject (find-class symbol)))
  (:method ((class class))
    (seal-metaobject class)))

(defgeneric seal-generic-function (generic-function)
  (:method ((generic-function generic-function))
    (seal-metaobject generic-function)))

(defgeneric seal-method (method)
  (:method ((method method))
    (seal-metaobject method)))

(defgeneric seal-specializer (specializer)
  (:method ((class class))
    (seal-class class))
  (:method ((eql-specializer eql-specializer))
    (seal-class
     (class-of
      (eql-specializer-object eql-specializer)))))

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

(defgeneric compute-method-inline-lambda (generic-function method lambda environment))

(defgeneric compute-generic-function-inline-lambda (generic-function applicable-methods))
