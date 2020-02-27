(in-package #:sealable-metaobjects)

(defclass fast-method (potentially-sealable-standard-method)
  ((%inline-lambda
    :initarg .inline-lambda.
    :reader fast-method-inline-lambda
    :initform nil)))

(defclass fast-generic-function (sealable-standard-generic-function)
  ()
  (:default-initargs
   :method-class (find-class 'fast-method))
  (:metaclass funcallable-standard-class))

(defmethod validate-method-property
    ((method fast-method) (property (eql 'inlineable)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expanding Effective Methods

(defmethod make-method-lambda :around
    ((fast-generic-function generic-function)
     (fast-method fast-method)
     lambda
     environment)
  (multiple-value-bind (method-lambda initargs)
      (call-next-method)
    (values
     method-lambda
     (list*
      '.inline-lambda.
      (compute-fast-method-inline-lambda fast-generic-function fast-method lambda environment)
      initargs))))

(defun compute-fast-method-inline-lambda
    (fast-generic-function fast-method lambda environment)
  (declare (ignore fast-method))
  (destructuring-bind (lambda-symbol lambda-list &rest body) lambda
    (assert (eql lambda-symbol 'lambda))
    (multiple-value-bind (required optional rest-var keyword allow-other-keys-p auxiliary)
        (parse-ordinary-lambda-list lambda-list)
      (let ((n-declarations (position-if-not (starts-with 'declare) body))
            (partially-flattened-lambda-list
              `(,@(lambda-list-variables
                   (unparse-ordinary-lambda-list
                    required optional rest-var keyword allow-other-keys-p '()))
                ,@(unparse-ordinary-lambda-list '() '() nil '() nil auxiliary))))
        (trivial-macroexpand-all:macroexpand-all
         `(lambda ,partially-flattened-lambda-list
            (declare (ignorable ,@(mapcar #'required-info-variable required)))
            ,@(subseq body 0 n-declarations)
            (block ,(block-name (generic-function-name fast-generic-function))
              ,@(subseq body n-declarations)))
         environment)))))
