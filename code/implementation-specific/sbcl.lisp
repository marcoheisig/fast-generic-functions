(in-package #:sealable-metaobjects-implementation-specific)

(defmethod seal-metaobject :after
    ((sgf sealable-generic-function))
  (eval `(sb-c:defknown ,(generic-function-name sgf) * * () :overwrite-fndb-silently t))
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

(defun sealable-metaobjects::null-lexical-environement-p (environment)
  (sb-c::null-lexenv-p environment))

(defun make-inlineable (gf specializers)
  (with-accessors ((name generic-function-name)
                   (lambda-list generic-function-lambda-list)) gf
    (eval
     (multiple-value-bind (bitmask required optional rest key)
         (sb-int:parse-lambda-list (generic-function-lambda-list gf) :context 'defmethod)
       (declare (ignore bitmask))
       (let ((applicable-methods
               (sb-pcl::compute-applicable-methods
                gf
                (mapcar #'specializer-prototype specializers)))
             (types (mapcar #'specializer-type specializers))
             (restp (not (not (or optional rest key))))
             (rest-arg (gensym "REST")))
         (print types)
         (cond
           ((null applicable-methods)
            `(progn))
           ((null restp)
            `(sb-c:deftransform ,name ((,@required) (,@types))
               (print
                (if ,(= 1 (length applicable-methods))
                    `(funcall ,',(sealable-metaobjects::method-inline-lambda
                                  (first applicable-methods))
                              ,@',required)
                    `(sb-pcl::invoke-effective-method-function
                      ,',(effective-method-function gf applicable-methods)
                      ,',restp
                      :required-args ,',required)))))
           ((not (null restp))
            `(sb-c:deftransform ,name ((,@required &rest ,rest-arg) (,@types &rest t))
               (print
                (if ,(= 1 (length applicable-methods))
                    `(apply ,',(sealable-metaobjects::method-inline-lambda
                                (first applicable-methods))
                            ,@',required ,',rest-arg)
                    `(sb-pcl::invoke-effective-method-function
                      ,',(effective-method-function gf applicable-methods)
                      ,',restp
                      :required-args ,',required
                      :rest-arg ,'(,rest-arg))))))))))))

;;; A hash table, mapping from (generic-function applicable-methods)
;;; entries to effective method functions.

(defparameter *effective-method-functions* (make-hash-table :test #'equal))

(defun effective-method-function (gf applicable-methods)
  (let ((key (list gf applicable-methods)))
    (or (gethash key *effective-method-functions*)
        (setf (gethash key *effective-method-functions*)
              (sb-pcl::get-effective-method-function gf applicable-methods)))))

(defun specializer-type (specializer)
  (etypecase specializer
    (eql-specializer `(eql ,(eql-specializer-object specializer)))
    (class (class-name specializer))))

(defun specializer-prototype (specializer)
  (etypecase specializer
    (eql-specializer (eql-specializer-object specializer))
    (class (class-prototype specializer))))
