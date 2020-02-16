(in-package #:sealable-metaobjects)

(defclass sealable-generic-function (sealable-metaobject-mixin generic-function)
  ((%sealed-domains :initform '() :accessor sealed-domains))
  (:default-initargs :method-class (find-class 'potentially-sealable-method))
  (:metaclass funcallable-standard-class))

;;; Check that the supplied domain is sane.
(defmethod seal-domain :around
    ((sgf sealable-generic-function)
     (domain list))
  ;; Ensure that all elements of the domain are specializers.
  (let ((domain (mapcar #'ensure-specializer domain)))
    ;; Ensure that the length of the domain matches the number of mandatory
    ;; arguments of the generic function.
    (unless (= (length domain)
               (length
                (generic-function-argument-precedence-order sgf)))
      (error "~@<Cannot seal the domain ~S of the generic function ~S, ~
                 because the latter requires ~R specializer~:P, while the ~
                 former provides ~R specializer~:P.~@:>"
             (mapcar #'specializer-type domain)
             sgf
             (length (generic-function-argument-precedence-order sgf))
             (length domain)))
    ;; Ensure that we don't call any next methods if the supplied domain
    ;; has already been sealed.
    (unless (find domain (sealed-domains sgf))
      (call-next-method sgf domain))))

;;; Ensure that the generic function is sealed, and that the newly sealed
;;; domain is disjoint from other domains.
(defmethod seal-domain :before
    ((sgf sealable-generic-function)
     (domain list))
  (seal-generic-function sgf)
  (dolist (existing-domain (sealed-domains sgf))
    (when (domain-intersectionp domain existing-domain)
      (error "~@<Cannot seal the domain ~S of the generic function ~S, ~
               because it intersects with the existing domain ~S.~@:>"
             (mapcar #'specializer-type domain)
             sgf
             (mapcar #'specializer-type existing-domain)))))

;;; Add a new sealed domain.
(defmethod seal-domain
    ((sgf sealable-generic-function)
     (domain list))
  (dolist (method (generic-function-methods sgf))
    (when (domain-intersectionp (method-specializers method) domain)
      (unless (domain-subtypep (method-specializers method) domain)
        (error "~@<The method ~S with specializers ~S is only partially ~
                   within the sealed domain ~S.~:@>"
               method
               (mapcar #'specializer-type (method-specializers method))
               (mapcar #'specializer-type domain)))
      (seal-method method)))
  (push domain (sealed-domains sgf)))

;;; Ensure that the method to be added is disjoint from all sealed domains.
(defmethod add-method :before
    ((sgf sealable-generic-function)
     (psm potentially-sealable-method))
  (dolist (domain (sealed-domains sgf))
    (when (domain-intersectionp domain (method-specializers psm))
      (error "~@<Cannot add the method ~S with specializers ~S to ~
                 the sealed generic function ~S, because it intersects ~
                 with the existing sealed domain ~S.~:@>"
             psm (method-specializers psm) sgf (mapcar #'specializer-type domain)))))

;;; Ensure that the method to be removed is disjoint from all sealed domains.
(defmethod remove-method :before
    ((sgf sealable-generic-function)
     (psm potentially-sealable-method))
  (dolist (domain (sealed-domains sgf))
    (when (domain-intersectionp domain (method-specializers psm))
      (error "~@<Cannot remove the method ~S with specializers ~S from ~
                 the sealed generic function ~S, because it intersects ~
                 with the existing sealed domain ~S.~:@>"
             psm (method-specializers psm) sgf (mapcar #'specializer-type domain)))))

;;; Track all properties that have been declared in the body of the method
;;; lambda, and make them accessible as METHOD-PROPERTIES of that method.
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
