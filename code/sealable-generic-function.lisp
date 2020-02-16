(in-package #:sealable-metaobjects)

(defclass sealable-generic-function (sealable-metaobject-mixin generic-function)
  ()
  (:default-initargs :method-class (find-class 'potentially-sealable-method))
  (:metaclass funcallable-standard-class))

(defmethod seal-metaobject :before ((sgf sealable-generic-function))
  (mapc #'seal-method (generic-function-methods sgf)))

(defmethod add-method :before ((sgf sealable-generic-function) (sm potentially-sealable-method))
  ;; TODO
  )

(defmethod make-method-lambda :around
    ((sgf sealable-generic-function)
     (psm potentially-sealable-method)
     lambda
     environment)
  (multiple-value-bind (method-lambda initargs)
      (call-next-method)
    (values
     method-lambda
     (list* '.method-properties.
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
