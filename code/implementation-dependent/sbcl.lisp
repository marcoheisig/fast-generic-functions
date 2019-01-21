(in-package #:sealable-metaobjects)

;;; A hash table, mapping from (generic-function . applicable-methods)
;;; entries to effective method functions.

(defparameter *inlineable-functions* (make-hash-table :test #'equal))

(defun specializer-type (specializer)
  (etypecase specializer
    (eql-specializer `(eql ,(eql-specializer-object specializer)))
    (class (class-name specializer))))

(defmethod make-inlineable ((gf generic-function) specializers)
  ;; TODO
  #+nil
  (print `(sb-c:deftransform ,(generic-function-name gf)
              (,(generic-function-lambda-list gf)
               ,(mapcar #'specializer-type specializers))
            ,(sb-pcl::generate-discrimination-net
              gf
              (sb-pcl::compute-applicable-methods-using-types
               gf
               (mapcar #'specializer-type specializers))
              (mapcar #'specializer-type specializers)
              nil))))
