(in-package #:sealable-metaobjects)

(defclass sealable-generic-function (sealable-metaobject-mixin generic-function)
  ((%specializer-profile
    :accessor generic-function-specializer-profile))
  (:metaclass funcallable-standard-class))

(defmethod seal-metaobject :before ((sgf sealable-generic-function))
  (mapc #'seal-method (generic-function-methods sgf)))

(defmethod add-method :before ((sgf sealable-generic-function) (sm potentially-sealable-method))
  (when (and (generic-function-sealed-p sgf)
             (method-sealed-p sm))
    (error "~@<Must not add further sealed methods to the already sealed ~
               generic function ~S~:@>" sgf))
  (loop for boolean in (generic-function-specializer-profile sgf)
        for specializer in (method-specializers sm)
        for argument-number from 0 do
          (unless (or boolean
                      (eq specializer (find-class 't)))
            (error "~@<The argument ~D of the method ~S has a specializer ~
                      (~S) in position where its specializer profile ~
                      forbids specialization.~:@>"
                   argument-number sm specializer))))

(defmethod make-method-lambda :around
    ((sgf sealable-generic-function)
     (psm potentially-sealable-method)
     lambda
     environment)
  (multiple-value-bind (method-lambda initargs)
      (call-next-method)
    (flet ((extend-initargs (key value)
             (push value initargs)
             (push key initargs)))
      (extend-initargs 'specializer-profile (generic-function-specializer-profile sgf))
      (cond ((inlineable-method-lambda-p lambda environment)
             (extend-initargs 'inline-lambda lambda))
            (t
             (debug-format "~&The method body~% ~S~%is too hairy for method inlining." lambda)))
      (values method-lambda initargs))))
