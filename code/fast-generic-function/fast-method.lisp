(in-package #:sealable-metaobjects)

(defclass fast-method (potentially-sealable-standard-method)
  ((%lambda
    :initarg .lambda.
    :reader fast-method-lambda
    :initform nil)))

(defmethod validate-method-property
    ((method fast-method) (property (eql 'inlineable)))
  t)

(defmethod make-method-lambda :around
    ((generic-function generic-function)
     (fast-method fast-method)
     lambda
     environment)
  (multiple-value-bind (method-lambda initargs)
      (call-next-method)
    (values
     method-lambda
     (list*
      '.lambda.
      (make-fast-method-lambda generic-function fast-method lambda environment)
      initargs))))

(defun make-fast-method-lambda
    (generic-function method lambda environment)
  (declare (ignore method))
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
            (block ,(block-name (generic-function-name generic-function))
              ,@(subseq body n-declarations)))
         environment)))))
