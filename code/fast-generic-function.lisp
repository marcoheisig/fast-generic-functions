(in-package #:fast-generic-functions)

(defclass fast-generic-function (sealable-standard-generic-function)
  ((%full-effective-method-cache :initform '() :accessor full-effective-method-cache)
   (%flat-effective-method-cache :initform '() :accessor flat-effective-method-cache))
  (:default-initargs
   :method-class (find-class 'fast-method))
  (:metaclass funcallable-standard-class))

(defmethod compute-effective-method-function
    ((fgf fast-generic-function) effective-method options)
  (let ((lambda-list
          (anonymize-ordinary-lambda-list
           ;; Unfortunately, we don't know the list of applicable methods
           ;; anymore at this stage.  So instead, we consider all methods
           ;; applicable.
           (compute-effective-method-lambda-list fgf (generic-function-methods fgf)))))
    (compile
     nil
     `(lambda ,lambda-list
        ,(expand-effective-method-body effective-method fgf lambda-list)))))
