(in-package #:sealable-metaobjects)

;;; Checking for sealability.

(defgeneric metaobject-sealable-p (metaobject)
  (:method ((class class)) (eql class (find-class t)))
  (:method ((generic-function generic-function)) nil)
  (:method ((method method)) nil)
  (:method ((built-in-class built-in-class)) t)
  (:method ((structure-class structure-class)) t)
  #+sbcl (:method ((system-class sb-pcl:system-class)) t))

(defgeneric class-sealable-p (class)
  (:method ((class class))
    (metaobject-sealable-p class)))

(defgeneric generic-function-sealable-p (generic-function)
  (:method ((generic-function generic-function))
    (metaobject-sealable-p generic-function)))

(defgeneric method-sealable-p (method)
  (:method ((method method))
    (metaobject-sealable-p method)))

;;; Checking for sealed-ness.

(defgeneric metaobject-sealed-p (metaobject)
  (:method ((class class)) (eql class (find-class t)))
  (:method ((generic-function generic-function)) nil)
  (:method ((method method)) nil)
  (:method ((built-in-class built-in-class)) t)
  (:method ((structure-class structure-class)) t)
  #+sbcl (:method ((system-class sb-pcl:system-class)) t))

(defgeneric class-sealed-p (class)
  (:method ((class class))
    (metaobject-sealed-p class)))

(defgeneric generic-function-sealed-p (generic-function)
  (:method ((generic-function generic-function))
    (metaobject-sealed-p generic-function)))

(defgeneric method-sealed-p (method)
  (:method ((method method))
    (metaobject-sealed-p method)))

;;; Sealing of metaobjects.

(defgeneric seal-metaobject (metaobject)
  ;; Invoke primary methods on SEAL-METAOBJECT at most once.
  (:method :around ((metaobject t))
    (unless (metaobject-sealed-p metaobject)
      (call-next-method)))
  ;; Signal an error if the default primary method is reached.
  (:method ((metaobject t))
    (error "Cannot seal the metaobject ~S." metaobject))
  (:method :before ((class class))
    ;; Class sealing implies finalization.
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    ;; A sealed class must have sealed superclasses.
    (mapc #'seal-class (rest (class-precedence-list class)))))

(defgeneric seal-class (class)
  ;; Invoke primary methods on SEAL-CLASS at most once.
  (:method :around ((class class))
    (unless (class-sealed-p class)
      (call-next-method)))
  (:method ((symbol symbol))
    (seal-metaobject (find-class symbol)))
  (:method ((class class))
    (seal-metaobject class)))

(defgeneric seal-generic-function (generic-function)
  ;; Invoke primary methods on SEAL-GENERIC-FUNCTION at most once.
  (:method :around ((generic-function generic-function))
    (unless (generic-function-sealed-p generic-function)
      (call-next-method)))
  (:method ((generic-function generic-function))
    (seal-metaobject generic-function)))

(defgeneric seal-method (method)
  ;; Invoke primary methods on SEAL-METHOD at most once.
  (:method :around ((method method))
    (unless (method-sealed-p method)
      (call-next-method)))
  (:method ((method method))
    (seal-metaobject method)))

(defgeneric seal-domain (generic-function domain))

;;; Working with specializers.

(defgeneric specializer-sealable-p (specializer)
  (:method ((class class))
    (class-sealable-p class))
  (:method ((eql-specializer eql-specializer))
    (class-sealable-p
     (class-of
      (eql-specializer-object eql-specializer)))))

(defgeneric specializer-sealed-p (specializer)
  (:method ((class class))
    (class-sealed-p class))
  (:method ((eql-specializer eql-specializer))
    (specializer-sealed-p
     (class-of
      (eql-specializer-object eql-specializer)))))

(defgeneric seal-specializer (specializer)
  (:method ((class class))
    (seal-class class))
  (:method ((eql-specializer eql-specializer))
    (seal-class
     (class-of
      (eql-specializer-object eql-specializer)))))

(defgeneric specializer-type (specializer)
  (:method ((class class))
    (class-name class))
  (:method ((eql-specializer eql-specializer))
    `(eql ,(eql-specializer-object eql-specializer))))

(defgeneric specializer-intersectionp (specializer-1 specializer-2)
  (:method ((class-1 class) (class-2 class))
    (multiple-value-bind (disjointp success)
        (subtypep `(and ,class-1 ,class-2) nil)
      (assert success)
      (not disjointp)))
  (:method ((class class) (eql-specializer eql-specializer))
    (typep (eql-specializer-object eql-specializer) class))
  (:method ((eql-specializer eql-specializer) (class class))
    (typep (eql-specializer-object eql-specializer) class))
  (:method ((eql-specializer-1 eql-specializer) (eql-specializer-2 eql-specializer))
    (eql (eql-specializer-object eql-specializer-1)
         (eql-specializer-object eql-specializer-2))))

(defgeneric specializer-subtypep (specializer-1 specializer-2)
  (:method ((class-1 class) (class-2 class))
    (values (subtypep class-1 class-2)))
  (:method ((class class) (eql-specializer eql-specializer))
    (subtypep class (specializer-type eql-specializer)))
  (:method ((eql-specializer eql-specializer) (class class))
    (typep (eql-specializer-object eql-specializer) class))
  (:method ((eql-specializer-1 eql-specializer) (eql-specializer-2 eql-specializer))
    (eql (eql-specializer-object eql-specializer-1)
         (eql-specializer-object eql-specializer-2))))

(defun domain-intersectionp (domain-1 domain-2)
  (assert (= (length domain-1)
             (length domain-2)))
  (every #'specializer-intersectionp domain-1 domain-2))

(defun domain-subtypep (domain-1 domain-2)
  (assert (= (length domain-1)
             (length domain-2)))
  (every #'specializer-subtypep domain-1 domain-2))

;;; Method properties

(defgeneric method-properties (method)
  (:method ((method method))
    '()))

(defgeneric validate-method-property (method method-property)
  (:method ((method method) (method-property t))
    nil))

(defgeneric sealed-domains (generic-function))

(defgeneric (setf sealed-domains) (value generic-function))
