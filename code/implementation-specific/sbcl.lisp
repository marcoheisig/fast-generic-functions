(in-package #:sealable-metaobjects)

(defmethod make-inlineable ((gf generic-function) specializers)
  (declare (optimize (debug 3)))
  (multiple-value-bind (bitmask required optional rest key)
      (sb-int:parse-lambda-list (generic-function-lambda-list gf) :context 'defmethod)
    (declare (ignore bitmask))
    (let* ((name (generic-function-name gf))
           (types (mapcar #'type-from-specializer specializers))
           (applicable-methods
             ;; TODO Probably doesn't work for EQL specializers.
             (sb-pcl::compute-applicable-methods-using-classes gf specializers))
           (restp (not (not (or optional rest key))))
           (rest-arg (gensym "REST")))
      (eval
       `(progn
          (sb-c:defknown ,name * * () :overwrite-fndb-silently t)
          ,(if restp
               `(sb-c:deftransform ,name ((,@required &rest ,rest-arg)
                                          (,@types &rest t))
                  `(sb-pcl::invoke-effective-method-function
                    ,',(effective-method-function gf applicable-methods)
                    ,',restp
                    :required-args ,',required
                    :rest-arg ,'(,rest-arg)))
               `(sb-c:deftransform ,name (,required ,types)
                  `(sb-pcl::invoke-effective-method-function
                    ,',(effective-method-function gf applicable-methods)
                    ,',restp
                    :required-args ,',required))))))))

;;; A hash table, mapping from (generic-function applicable-methods)
;;; entries to effective method functions.

(defparameter *effective-method-functions* (make-hash-table :test #'equal))

(defun effective-method-function (gf applicable-methods)
  (let ((key (list gf applicable-methods)))
    (or (gethash key *effective-method-functions*)
        (setf (gethash key *effective-method-functions*)
              (sb-pcl::get-effective-method-function gf applicable-methods)))))

(defun type-from-specializer (specializer)
  (etypecase specializer
    (eql-specializer `(eql ,(eql-specializer-object specializer)))
    (class (class-name specializer))))
