(in-package #:sealable-metaobjects)

(defclass sealable-generic-function (sealable-metaobject-mixin generic-function)
  ((%specializer-profile
    :accessor generic-function-specializer-profile))
  (:default-initargs :method-class (find-class 'potentially-sealable-method))
  (:metaclass funcallable-standard-class))

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
            '.method-properties
            (let* ((declare-forms (remove-if-not (starts-with 'declare) lambda))
                   (declarations (apply #'append (mapcar #'rest declare-forms))))
              (reduce #'union (remove-if-not (starts-with 'method-properties) declarations)
                      :key #'rest
                      :initial-value '()))
            initargs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived Classes

(defclass sealable-standard-generic-function
    (standard-generic-function sealable-generic-function)
  ()
  (:default-initargs :method-class (find-class 'potentially-sealable-standard-method))
  (:metaclass funcallable-standard-class))

(defclass inlineable-generic-function (sealable-generic-function)
  ()
  (:default-initargs :method-class (find-class 'potentially-inlineable-method))
  (:metaclass funcallable-standard-class))

(defclass inlineable-standard-generic-function
    (inlineable-generic-function sealable-standard-generic-function)
  ()
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-class (find-class 'potentially-inlineable-standard-method)))
