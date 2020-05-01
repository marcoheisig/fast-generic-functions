(in-package #:fast-generic-functions)

(defclass fast-generic-function (sealable-standard-generic-function)
  ((%full-effective-method-cache :initform '() :accessor full-effective-method-cache)
   (%flat-effective-method-cache :initform '() :accessor flat-effective-method-cache))
  (:default-initargs
   :method-class (find-class 'fast-method))
  (:metaclass funcallable-standard-class))

(defmethod compute-effective-method-function
    ((fgf fast-generic-function) effective-method options)
  (let ((lambda-list (generic-function-lambda-list fgf)))
    (compile
     nil
     `(lambda ,lambda-list
        (expand-effective-method-body effective-method generic-function lambda-list)))))
