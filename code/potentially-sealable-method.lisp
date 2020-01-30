(in-package #:sealable-metaobjects)

(defclass potentially-sealable-method (sealable-metaobject-mixin method)
  ((%method-body
    :initform nil
    :initarg .method-body.
    :reader method-body)
   (%specializer-profile
    :initform (error "No specializer profile supplied.")
    :initarg .specializer-profile.
    :accessor method-specializer-profile)))

(defmethod metaobject-sealable-p ((psm potentially-sealable-method))
  (every
   (lambda (specializer specializing-p)
     (or (not specializing-p)
         (specializer-sealed-p specializer)))
   (method-specializers psm)
   (method-specializer-profile psm)))

(defmethod seal-method ((psm potentially-sealable-method))
  (seal-metaobject psm))

(defmethod seal-metaobject :before ((psm potentially-sealable-method))
  (loop for specializer in (method-specializers psm)
        for specializing-p in (method-specializer-profile psm) do
          (when specializing-p
            (seal-class specializer))))

(defmethod make-method-lambda :around
    ((gf generic-function)
     (psm potentially-sealable-method)
     lambda
     environment)
  (multiple-value-bind (method-lambda initargs)
      (call-next-method)
    (values
     method-lambda
     (list* '.method-body. (extract-method-body lambda environment)
            initargs))))

(defun extract-method-body (lambda environment)
  (assert (eql (first lambda) 'lambda))
  (assert (listp (second lambda)))
  (let* ((body (cddr lambda))
         (end-of-declarations (position-if-not #'declare-form-p body)))
    (append
     (subseq body 0 end-of-declarations)
     (mapcar
      (lambda (form)
        (trivial-macroexpand-all:macroexpand-all form environment))
      (subseq body end-of-declarations)))))

(defun declare-form-p (form)
  (and (consp form)
       (eql (car form) 'declare)))
