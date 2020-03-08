(in-package #:sealable-metaobjects)

(defmethod optimize-function-call
    ((fast-generic-function fast-generic-function)
     (static-call-signature static-call-signature))
  (let ((name (generic-function-name fast-generic-function))
        (applicable-methods
          (compute-applicable-methods
           fast-generic-function
           (static-call-signature-prototypes static-call-signature))))
    (cond
      ;; Inlining of the entire effective method.
      ((every #'inlineable-method-p applicable-methods)
       (compute-fast-lambda
        fast-generic-function
        static-call-signature
        applicable-methods))
      ;; Inlining of the keyword parsing function.
      #+(or)
      ((member '&key (generic-function-lambda-list fast-generic-function)))
      ;; Elimination of the generic function dispatch.
      ((every #'externalizable-object-p (static-call-signature-prototypes static-call-signature))
       `(lambda (&rest args)
          (apply
           (load-time-value
            (lookup-effective-method #',name ',static-call-signature))
           args)))
      (t
       `(lambda (&rest args)
          (locally (declare (notinline ,name))
            (apply #',name args)))))))

(defun inlineable-method-p (method)
  (member 'inlineable (method-properties method)))

(defvar *effective-method-cache* (make-hash-table :test #'equal))

(declaim (ftype (function (t t) function) lookup-effective-method))
(defun lookup-effective-method (generic-function static-call-signature)
  (with-accessors ((prototypes static-call-signature-prototypes)) static-call-signature
    (multiple-value-bind (value present-p)
        (gethash (cons generic-function prototypes) *effective-method-cache*)
      (if present-p
          value
          (setf (gethash (cons generic-function prototypes) *effective-method-cache*)
                (compile nil (compute-fast-lambda
                              generic-function
                              static-call-signature
                              (compute-applicable-methods generic-function prototypes))))))))
