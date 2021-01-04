(in-package #:fast-generic-functions)

(defclass fast-method (potentially-sealable-standard-method)
  ((%lambda
    :initarg .lambda.
    :reader fast-method-lambda
    :initform (required-argument '.lambda.))))

(defmethod validate-method-property
    ((method fast-method) (property (eql 'inlineable)))
  t)

(defmethod make-method-lambda :around
    ((gf sealable-standard-generic-function)
     (fast-method fast-method)
     lambda
     environment)
  (multiple-value-bind (method-lambda initargs)
      (call-next-method)
    (values
     method-lambda
     (list*
      '.lambda.
      (make-fast-method-lambda gf fast-method lambda environment)
      initargs))))

(defun make-fast-method-lambda
    (generic-function method lambda environment)
  (declare (ignore method))
  (destructuring-bind (lambda-symbol lambda-list &rest body) lambda
    (assert (eql lambda-symbol 'lambda))
    (multiple-value-bind (required optional rest-var keyword allow-other-keys-p auxiliary)
        (parse-ordinary-lambda-list lambda-list)
      (multiple-value-bind (forms declarations)
          (parse-body body)
        (let ((partially-flattened-lambda-list
                `(,@(lambda-list-variables
                     (unparse-ordinary-lambda-list
                      required optional rest-var keyword allow-other-keys-p '()))
                  ,@(unparse-ordinary-lambda-list '() '() nil '() nil auxiliary))))
          (trivial-macroexpand-all:macroexpand-all
           `(lambda ,partially-flattened-lambda-list
              (declare (ignorable ,@(mapcar #'required-info-variable required)))
              ,@declarations
              (block ,(block-name (generic-function-name generic-function))
                ,@forms))
           environment))))))
