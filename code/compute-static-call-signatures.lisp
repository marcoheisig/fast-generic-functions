(in-package #:sealable-metaobjects)

;;; In this file, we compute the static call signatures of a given, sealed
;;; generic function. A static call signature consists of a list of types,
;;; and a list of prototypes.  The list of types is guaranteed to be
;;; non-overlapping with the types of any other call signature.  The list
;;; of prototypes is chosen such that the list of applicable methods of
;;; these prototypes is representative for all arguments of the types of
;;; the call signature.

(defgeneric compute-static-call-signatures (generic-function))

(defclass static-call-signature ()
  ((%type
    :initarg :types
    :reader static-call-signature-types)
   (%prototype
    :initarg :prototypes
    :reader static-call-signature-prototypes)))

(defmethod compute-static-call-signatures ((sgf sealable-generic-function))
  (let* ((sealed-methods (remove-if-not #'method-sealed-p (generic-function-methods sgf)))
         (list-of-specializers (mapcar #'method-specializers sealed-methods))
         (static-call-signatures '()))
    (map-types-and-prototypes
     (lambda (types prototypes)
       (push (make-instance 'static-call-signature
               :types types
               :prototypes prototypes)
             static-call-signatures))
     (apply #'mapcar #'list list-of-specializers)
     (generic-function-specializer-profile sgf))
    static-call-signatures))

(defun map-types-and-prototypes (fn specializers-list specializer-profile)
  (assert (= (length specializers-list)
             (length specializer-profile)))
  (labels ((rec (sl sp types prototypes)
             (if (null sl)
                 (funcall fn (reverse types) (reverse prototypes))
                 (if (not (first sp))
                     (rec (rest sl)
                          (rest sp)
                          (cons 't types)
                          (cons 42 prototypes))
                     (loop for (type prototype) in (type-prototype-pairs (first sl))
                           do (rec (rest sl)
                                   (rest sp)
                                   (cons type types)
                                   (cons prototype prototypes)))))))
    (rec specializers-list specializer-profile '() '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reasoning About Specializer Specificity

(defclass snode ()
  ((%specializer
    :initarg :specializer
    :accessor snode-specializer)
   (%direct-subspecializers
    :initform '()
    :accessor snode-direct-subspecializers)
   (%direct-superspecializers
    :initform '()
    :accessor snode-direct-superspecializers)))

(defvar *snode-table*)

(defun specializer-snode (specializer)
  (multiple-value-bind (snode present-p)
      (gethash specializer *snode-table*)
    (if present-p
        snode
        (let ((snode (make-instance 'snode :specializer specializer)))
          (setf (gethash specializer *snode-table*) snode)
          snode))))

(defun specializer-snode-p (specializer)
  (nth-value 1 (gethash specializer *snode-table*)))

(defun snode-add-edge (super-snode sub-snode)
  (pushnew super-snode (snode-direct-superspecializers sub-snode))
  (pushnew sub-snode (snode-direct-subspecializers super-snode))
  (values))

(defun snode-type (snode)
  (let ((stype (specializer-type (snode-specializer snode)))
        (subspecializers (snode-direct-subspecializers snode)))
    (if (null subspecializers)
        stype
        `(and ,stype
              (not (or ,@(loop for subspecializer in subspecializers
                               collect (specializer-type
                                        (snode-specializer subspecializer)))))))))

(defun snode-prototype (snode)
  (specializer-prototype
   (snode-specializer snode)))

(defun type-prototype-pairs (specializers)
  (let ((*snode-table* (make-hash-table)))
    ;; Ensure that supplied specializers have a corresponding snode.
    (mapc #'specializer-snode specializers)
    ;; Now connect all snodes.
    (labels ((walk (current origin)
               (if (and (not (eq current origin))
                        (specializer-snode-p current))
                   (snode-add-edge
                    (specializer-snode current)
                    (specializer-snode origin))
                   (mapc
                    (lambda (super) (walk super origin))
                    (specializer-direct-superspecializers current)))))
      (mapc #'walk specializers specializers))
    ;; Finally, build all pairs.
    (loop for snode being the hash-values of *snode-table*
          collect
          (list (snode-type snode)
                (snode-prototype snode)))))
