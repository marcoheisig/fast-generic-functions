(in-package #:sealable-metaobjects)

(defclass sealable-generic-function (sealable-metaobject-mixin generic-function)
  ((%specializer-profile
    :accessor generic-function-specializer-profile))
  (:default-initargs :method-class (find-class 'potentially-sealable-method))
  (:metaclass funcallable-standard-class))

(defmethod seal-generic-function ((sealable-generic-function sealable-generic-function))
  (seal-metaobject sealable-generic-function))

(defmethod seal-metaobject :before ((sgf sealable-generic-function))
  (mapc #'seal-method (generic-function-methods sgf)))

(defmethod add-method :before ((sgf sealable-generic-function) (sm potentially-sealable-method))
  (loop for boolean in (generic-function-specializer-profile sgf)
        for specializer in (method-specializers sm)
        for argument-number from 0 do
          (unless (or boolean
                      (eq specializer (find-class 't)))
            (error "~@<The ~:R argument of the method ~S has the specializer ~
                      ~S, but the generic function's specializer profile ~
                      forbids specialization in the ~:R argument.~:@>"
                   argument-number sm specializer argument-number))))

(defmethod make-method-lambda :around
    ((sgf sealable-generic-function)
     (psm potentially-sealable-method)
     lambda
     environment)
  (multiple-value-bind (method-lambda initargs)
      (call-next-method)
    (values
     method-lambda
     (list* '.specializer-profile. (generic-function-specializer-profile sgf)
            initargs))))
