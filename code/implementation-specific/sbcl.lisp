(in-package #:sealable-metaobjects)

(defmethod seal-metaobject :after
    ((sgf sealable-generic-function))
  (eval `(sb-c:defknown ,(generic-function-name sgf) * * () :overwrite-fndb-silently t))
  (mapc
   (lambda (call-signature)
     (eval (make-deftransform sgf call-signature)))
   (compute-static-call-signatures sgf)))

(defun make-effective-method-using-prototypes (gf prototypes)
  (let ((applicable-methods (compute-applicable-methods gf prototypes)))
    (let ((emf (sb-pcl::get-effective-method-function gf applicable-methods)))
      (lambda (&rest args)
        (sb-pcl::invoke-emf emf args)))))

(defun make-deftransform (gf static-call-signature)
  (with-accessors ((name generic-function-name)
                   (lambda-list generic-function-lambda-list)) gf
    (let* ((types (static-call-signature-types static-call-signature))
           (prototypes (static-call-signature-prototypes static-call-signature))
           (applicable-methods (compute-applicable-methods gf prototypes)))
      (unless (null applicable-methods)
        (assert (every #'method-sealed-p applicable-methods))
        (debug-format "~&Creating deftransform for ~S~{ ~S~}~%"
                      (generic-function-name gf) types)
        `(sb-c:deftransform ,name ((&rest rest) (,@types &rest *))
           (let* ((gensyms (loop for r in rest collect (gensym)))
                  (inline-lambda
                    `(lambda (,@gensyms)
                       (block ,',(generic-function-name gf)
                         (funcall
                          ,',(if (and (every #'method-inline-lambda applicable-methods)
                                      (notany #'method-qualifiers applicable-methods))
                                 (method-inline-lambda (first applicable-methods))
                                 `(load-time-value
                                   (make-effective-method-using-prototypes
                                    #',(generic-function-name gf)
                                    ',prototypes)))
                          ,@gensyms)))))
             (debug-format "~&Creating inline lambda:~% ~S~%" inline-lambda)
             inline-lambda))))))
