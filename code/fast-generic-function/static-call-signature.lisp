(in-package #:sealable-metaobjects)

;;; In this file, we compute the static call signatures of a given, sealed
;;; generic function. A static call signature consists of a list of types,
;;; and a list of prototypes.  The list of types is guaranteed to be
;;; non-overlapping with the types of any other call signature.  The list
;;; of prototypes is chosen such that the list of applicable methods of
;;; these prototypes is representative for all arguments of the types of
;;; the call signature.

(defclass static-call-signature ()
  ((%types
    :initarg :types
    :reader static-call-signature-types)
   (%prototypes
    :initarg :prototypes
    :reader static-call-signature-prototypes)))

(defmethod print-object ((scs static-call-signature) stream)
  (print-unreadable-object (scs stream :type t :identity t)
    (format stream "~S ~S"
            (static-call-signature-types scs)
            (static-call-signature-prototypes scs))))

(defmethod make-load-form
    ((static-call-signature static-call-signature) &optional environment)
  (make-load-form-saving-slots
   static-call-signature
   :slot-names '(%types %prototypes)
   :environment environment))

(defmethod externalizable-object-p
    ((static-call-signature static-call-signature))
  (and
   (every #'externalizable-object-p
          (static-call-signature-types static-call-signature))
   (every #'externalizable-object-p
          (static-call-signature-prototypes static-call-signature))))

(defmethod compute-static-call-signatures
    ((sgf sealable-generic-function)
     (domain list))
  (let* ((sealed-methods
           (remove-if-not
            (lambda (method)
              (domain-intersectionp (method-specializers method) domain))
            (generic-function-methods sgf)))
         (list-of-specializers
           (mapcar #'method-specializers sealed-methods))
         (static-call-signatures '()))
    (unless (null list-of-specializers)
      (map-types-and-prototypes
       (lambda (types prototypes)
         (push (make-instance 'static-call-signature
                 :types types
                 :prototypes prototypes)
               static-call-signatures))
       ;; Transpose the list of specializers so that we operate on each
       ;; argument instead of on each method.
       (apply #'mapcar #'list list-of-specializers)
       domain))
    static-call-signatures))

(defun map-types-and-prototypes (fn specializers-list domain)
  (assert (= (length specializers-list) (length domain)))
  (labels ((rec (sl domain types prototypes)
             (if (null sl)
                 (funcall fn (reverse types) (reverse prototypes))
                 (loop for (type prototype)
                         in (type-prototype-pairs
                             (first sl)
                             (first domain))
                       do (rec (rest sl)
                               (rest domain)
                               (cons type types)
                               (cons prototype prototypes))))))
    (rec specializers-list domain '() '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning About Specializer Specificity

(defclass snode ()
  (;; The specializer of an snode.
   (%specializer :initarg :specializer :accessor snode-specializer)
   ;; A (possibly empty) list of snodes for each child class or eql specializer.
   (%children :initform '() :accessor snode-children)
   ;; A list of snodes with one entry for each parent class.
   (%parents :initform '() :accessor snode-parents)
   ;; Whether the snode has already been visited.
   (%visitedp :initform nil :accessor snode-visitedp)
   ;; Whether the snode corresponds to a specializer of an existing method
   ;; or the domain.
   (%relevantp :initform nil :accessor snode-relevantp)))

(defun snode-type (snode)
  (type-specifier-and
   (specializer-type (snode-specializer snode))
   (type-specifier-not
    (apply #'type-specifier-or
           (loop for subspecializer in (snode-children snode)
                 collect
                 (specializer-type
                  (snode-specializer subspecializer)))))))

(defun snode-prototype (snode)
  (specializer-prototype
   (snode-specializer snode)
   (mapcar #'snode-specializer (snode-children snode))))

(defvar *snode-table*)

(defun specializer-snode (specializer)
  (multiple-value-bind (snode present-p)
      (gethash specializer *snode-table*)
    (if present-p
        snode
        (let ((snode (make-instance 'snode :specializer specializer)))
          (setf (gethash specializer *snode-table*) snode)
          snode))))

(defun snode-add-edge (super-snode sub-snode)
  (pushnew super-snode (snode-parents sub-snode))
  (pushnew sub-snode (snode-children super-snode))
  (values))

(defun type-prototype-pairs (specializers domain)
  (let* ((*snode-table* (make-hash-table))
         (specializer-snodes (mapcar #'specializer-snode specializers))
         (domain-snode (specializer-snode domain)))
    ;; Initialize domain and specializer snodes.
    (dolist (snode specializer-snodes)
      (setf (snode-relevantp snode) t))
    (setf (snode-relevantp domain-snode) t)
    ;; Now connect all snodes.
    (labels ((visit (current relevant)
               (unless (snode-visitedp current)
                 (setf (snode-visitedp current) t)
                 (unless (eql current domain)
                   (dolist (specializer
                            (specializer-direct-superspecializers
                             (snode-specializer current)))
                     (let ((super (specializer-snode specializer)))
                       (cond ((snode-relevantp super)
                              (snode-add-edge super relevant)
                              (visit super super))
                             (t
                              (visit super relevant)))))))))
      (mapc #'visit specializer-snodes specializer-snodes))
    ;; Finally, build all pairs.
    (let ((pairs '()))
      (loop for snode being the hash-values of *snode-table* do
        (when (snode-relevantp snode)
          (multiple-value-bind (prototype prototype-p)
              (snode-prototype snode)
            (when prototype-p
              (push (list (snode-type snode) prototype)
                    pairs)))))
      pairs)))

