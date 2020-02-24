(in-package #:sealable-metaobjects)

(defmethod seal-domain :after
    ((fast-generic-function fast-generic-function)
     (domain list))
  (setf (compiler-macro-function (generic-function-name fast-generic-function))
        (fast-generic-function-compiler-macro fast-generic-function)))

(defun fast-generic-function-compiler-macro (fast-generic-function)
  (lambda (form environment)
    (block compiler-macro
      (dolist (sealed-domain (sealed-domains fast-generic-function))
        (dolist (scs (compute-static-call-signatures fast-generic-function sealed-domain))
          (when (loop for argument in (rest form)
                      for type in (static-call-signature-types scs)
                      always (compiler-typep argument type environment))
            (return-from compiler-macro
              `(funcall
                ,(fast-generic-function-lambda fast-generic-function scs)
                ,@(rest form))))))
      form)))

(defun compiler-typep (form type environment)
  (constantp
   `(unless (typep ,form ',type)
      (tagbody label (go label)))
   environment))
