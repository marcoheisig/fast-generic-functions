(in-package #:sealable-metaobjects)

#+(or)
(defmethod seal-metaobject :after ((sealable-class sealable-class))
  (proclaim `(sb-ext:freeze-type ,(class-name sealable-class))))

(defmethod seal-metaobject :after
    ((fast-generic-function fast-generic-function))
  (update-sealed-deftransforms fast-generic-function))

(defmethod add-method :after
    ((fast-generic-function fast-generic-function)
     (fast-method fast-method))
  (when (and (generic-function-sealed-p fast-generic-function)
             (method-sealable-p fast-method))
    (seal-method fast-method)
    (update-sealed-deftransforms fast-generic-function)))

(defun update-sealed-deftransforms (fast-generic-function)
  (let ((name (generic-function-name fast-generic-function)))
    ;; Ensure that the function is known.
    (eval `(sb-c:defknown ,name * * () :overwrite-fndb-silently t))
    ;; Remove all existing IR1-transforms.
    (let ((fun-info (sb-c::fun-info-or-lose name)))
      (setf (sb-c::fun-info-transforms fun-info) '()))
    ;; Create an IR1-transform for each static call signature.
    (mapc
     (lambda (call-signature)
       (eval (make-deftransform fast-generic-function call-signature)))
     (compute-static-call-signatures fast-generic-function))))

(defun make-deftransform (generic-function static-call-signature)
  (with-accessors ((name generic-function-name)) generic-function
    (with-accessors ((types static-call-signature-types)
                     (prototypes static-call-signature-prototypes)) static-call-signature
      (debug-format "~&Creating deftransform for ~S~{ ~S~}~%" name types)
      `(sb-c:deftransform ,name ((&rest rest) (,@types &rest *))
         (let ((inline-lambda
                 (generic-function-inline-lambda ',generic-function (length rest) ',prototypes)))
           (debug-format "~&Creating inline lambda:~% ~S~%" inline-lambda)
           inline-lambda)))))

(defun make-effective-method-using-prototypes (gf prototypes arity)
  (let ((emf (sb-pcl::get-effective-method-function gf (compute-applicable-methods gf prototypes))))
    (typecase emf
      (sb-pcl::fast-method-call
       (let ((fn (sb-pcl::fast-method-call-function emf))
             (pv (sb-pcl::fast-method-call-pv emf))
             (nm (sb-pcl::fast-method-call-next-method-call emf)))
         (declare (function fn))
         (case arity
           (0 (lambda  () (funcall fn pv nm)))
           (1 (lambda  (a1) (funcall fn pv nm a1)))
           (2 (lambda  (a1 a2) (funcall fn pv nm a1 a2)))
           (3 (lambda  (a1 a2 a3) (funcall fn pv nm a1 a2 a3)))
           (4 (lambda  (a1 a2 a3 a4) (funcall fn pv nm a1 a2 a3 a4)))
           (5 (lambda  (a1 a2 a3 a4 a5) (funcall fn pv nm a1 a2 a3 a4 a5)))
           (6 (lambda  (a1 a2 a3 a4 a5 a6) (funcall fn pv nm a1 a2 a3 a4 a5 a6)))
           (7 (lambda  (a1 a2 a3 a4 a5 a6 a7) (funcall fn pv nm a1 a2 a3 a4 a5 a6 a7)))
           (t (lambda (&rest args) (apply fn pv nm args))))))
      (t
       (lambda (&rest args)
         (sb-pcl::invoke-emf emf args))))))

(defun sb-pcl-symbol-p (symbol)
  (not
   (null
    (nth-value 1 (find-symbol (symbol-name symbol)
                              (find-package "SB-PCL"))))))

(defun effective-method-inline-lambda (effective-method)
  (cond ((atom effective-method) nil)
        ((and (eq (first effective-method) 'let)
              (let ((variables (mapcar #'first (second effective-method))))
                (every #'sb-pcl-symbol-p variables))
              (sb-pcl-symbol-p (first (third effective-method))))
         (effective-method-inline-lambda (fourth effective-method)))
        ((and (eq (first effective-method) 'call-method)
              (typep (second effective-method) 'potentially-sealable-method))
         (method-inline-lambda (second effective-method)))
        (t nil)))

(defmethod generic-function-inline-lambda
    ((generic-function sealable-generic-function) arity prototypes)
  (let* ((gensyms (loop repeat arity collect (gensym))))
    `(lambda (,@gensyms)
       (funcall
        (load-time-value
         (make-effective-method-using-prototypes
          #',(generic-function-name generic-function)
          ',prototypes
          ',arity))
        ,@gensyms))))

(defmethod generic-function-inline-lambda
    ((generic-function fast-generic-function) arity prototypes)
  (compute-generic-function-inline-lambda
   generic-function
   (compute-applicable-methods generic-function prototypes)))
