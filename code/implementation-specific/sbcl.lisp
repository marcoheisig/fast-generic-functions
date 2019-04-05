(in-package #:sealable-metaobjects)

(defvar *fast-methods* (make-array 0 :adjustable t :fill-pointer 0))
(declaim (type (vector t) *fast-methods*))

(defmethod seal-metaobject :after
    ((sgf sealable-generic-function))
  (eval `(sb-c:defknown ,(generic-function-name sgf) * * () :overwrite-fndb-silently t))
  (mapc
   (lambda (call-signature)
     (eval (make-deftransform sgf call-signature)))
   (compute-static-call-signatures sgf)))

(defun compute-static-call-signatures (sgf)
  (let* ((sealed-methods (remove-if-not #'method-sealed-p (generic-function-methods sgf)))
         (list-of-specializers (mapcar #'method-specializers sealed-methods))
         (specializer-lists
           (mapcar
            (lambda (specializer-list mask-bit)
              (if mask-bit
                  (remove-duplicates specializer-list :test #'eq)
                  (list (find-class 't))))
            (apply #'mapcar #'list list-of-specializers)
            (generic-function-specializer-profile sgf))))
    (if (null specializer-lists)
        '()
        (apply #'alexandria:map-product #'list specializer-lists))))

(defun make-deftransform (gf specializers)
  (with-accessors ((name generic-function-name)
                   (lambda-list generic-function-lambda-list)) gf
    (let* ((applicable-methods
             (compute-applicable-methods
              gf
              (mapcar #'specializer-prototype specializers)))
           (types (mapcar #'specializer-type specializers))
           (index (vector-push-extend
                   (let ((emf (sb-pcl::get-effective-method-function gf applicable-methods)))
                     (lambda (&rest args)
                       (sb-pcl::invoke-emf emf args)))
                   *fast-methods*)))
      (assert (every #'method-sealed-p applicable-methods))
      `(sb-c:deftransform ,name ((&rest rest) (,@types &rest *))
         (let ((gensyms (loop for r in rest collect (gensym))))
           `(lambda (,@gensyms)
              (funcall
               ,',(if (and (= 1 (length applicable-methods))
                           (null (method-qualifiers (first applicable-methods)))
                           (method-inline-lambda (first applicable-methods)))
                      (method-inline-lambda (first applicable-methods))
                      `(aref *fast-methods* ,index))
               ,@gensyms)))))))

(defun specializer-type (specializer)
  (etypecase specializer
    (eql-specializer
     `(eql ,(eql-specializer-object specializer)))
    (class
     (class-name specializer))))

(defun specializer-prototype (specializer)
  (etypecase specializer
    (eql-specializer (eql-specializer-object specializer))
    (class (class-prototype specializer))))
