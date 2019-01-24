(in-package #:sealable-metaobjects)

(defclass sealable-generic-function (sealable-metaobject-mixin generic-function)
  ()
  (:metaclass funcallable-standard-class))

(defmethod seal-metaobject :after ((sgf sealable-generic-function))
  (let* ((all-methods (generic-function-methods sgf))
         (sealed-methods (remove-if-not #'method-sealed-p all-methods))
         (unsealed-methods (remove-if #'method-sealed-p all-methods)))
    (if (null sealed-methods)
        (warn "Sealing a generic function with zero sealed methods.")
        (let ((specializer-mask (method-specializer-mask (first sealed-methods))))
          (dolist (sealed-method (rest sealed-methods))
            (unless (equal (method-specializer-mask sealed-method) specializer-mask)
              (error "The sealed methods ~S and ~S have incompatible specializer masks."
                     (first sealed-methods) sealed-method)))
          (dolist (unsealed-method unsealed-methods)
            ;; TODO
            )
          (mapc
           (lambda (call-signature)
             (make-inlineable sgf call-signature))
           (compute-static-call-signatures specializer-mask sealed-methods))))))

(defun method-specializer-mask (method)
  (assert (method-sealed-p method))
  (mapcar
   (lambda (s)
     (not (eq s (find-class 't))))
   (method-specializers method)))

(defun compute-static-call-signatures (specializer-mask sealed-methods)
  (let* ((list-of-specializers (mapcar #'method-specializers sealed-methods))
         (specializer-lists
           (mapcar
            (lambda (specializer-list mask-bit)
              (if mask-bit
                  (remove-duplicates specializer-list :test #'equal)
                  (list (find-class 't))))
            (apply #'mapcar #'list list-of-specializers)
            specializer-mask)))
    (if (null specializer-lists)
        '()
        (apply #'alexandria:map-product #'list specializer-lists))))
