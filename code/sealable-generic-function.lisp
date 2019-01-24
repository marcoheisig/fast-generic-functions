(in-package #:sealable-metaobjects)

(defvar *seal-generic-functions-eagerly* t)

(defclass sealable-generic-function (sealable-metaobject-mixin generic-function)
  ((%specializer-profile :accessor generic-function-specializer-profile))
  (:metaclass funcallable-standard-class))

(defmethod seal-metaobject :after ((sgf sealable-generic-function))
  (mapc
   (lambda (call-signature)
     (make-inlineable sgf call-signature))
   (compute-static-call-signatures
    (generic-function-specializer-profile sgf)
    (remove-if-not #'method-sealed-p (generic-function-methods sgf)))))

(defun compute-static-call-signatures (specializer-profile sealed-methods)
  (let* ((list-of-specializers (mapcar #'method-specializers sealed-methods))
         (specializer-lists
           (mapcar
            (lambda (specializer-list mask-bit)
              (if mask-bit
                  (remove-duplicates specializer-list :test #'equal)
                  (list (find-class 't))))
            (apply #'mapcar #'list list-of-specializers)
            specializer-profile)))
    (if (null specializer-lists)
        '()
        (apply #'alexandria:map-product #'list specializer-lists))))

(defmethod add-method :before ((sgf sealable-generic-function) (sm sealable-method))
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
