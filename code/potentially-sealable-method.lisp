(in-package #:sealable-metaobjects)

;;; There is no portable way to add options to a method.  So instead, we
;;; allow programmers to declare METHOD-PROPERTIES.
;;;
;;; Example:
;;;
;;; (defmethod foo (x y)
;;;   (declare (method-properties inline))
;;;   (+ x y))

(declaim (declaration method-properties))

(defclass potentially-sealable-method (sealable-metaobject-mixin method)
  ((%method-properties
    :initarg .method-properties.
    :accessor method-properties
    :initform '())))

(defmethod shared-initialize :after
    ((psm potentially-sealable-method)
     slot-names &key ((.method-properties. method-properties) '()) &allow-other-keys)
  (dolist (method-property method-properties)
    (unless (validate-method-property psm method-property)
      (error "~@<~S is not a valid method property for the method ~S.~@:>"
             method-property psm))))

;;; Track all properties that have been declared in the body of the method
;;; lambda, and make them accessible as METHOD-PROPERTIES of that method.
(defmethod make-method-lambda :around
    ((gf generic-function)
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

(defmethod metaobject-sealable-p ((psm potentially-sealable-method))
  (every #'specializer-sealed-p (method-specializers psm)))

(defmethod seal-metaobject :before ((psm potentially-sealable-method))
  (mapcar #'seal-specializer (method-specializers psm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived Classes

(defclass potentially-sealable-standard-method
    (standard-method potentially-sealable-method)
  ())
