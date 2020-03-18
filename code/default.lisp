(in-package #:fast-generic-functions)

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
  "Try to statically determine whether FORM is provably of TYPE in the
supplied ENVIRONMENT.  A value of T means that the result of evaluating
FORM is provably the supplied TYPE.  A value of NIL means that the result
of evaluating FORM may or may not be of the supplied TYPE."
  (or
   ;; In a first step, we try to abuse CONSTANTP to perform a portable
   ;; compile-time type query.  To do so, we generate a form that is
   ;; constant if the type relation holds, and that diverges when the type
   ;; relation does not hold.  Unfortunately, most implementations of
   ;; CONSTANTP are not sophisticated for our trick to work.  But one day,
   ;; this might suddenly start working really well.
   (constantp
    `(unless (typep ,form ',type)
       (tagbody label (go label)))
    environment)
   ;; Our fallback solution.  We may not be able to check the type relation
   ;; for arbitrary forms, but we certainly can check the type relation if
   ;; FORM is a constant.
   (and (constantp form)
        (typep (eval form) type environment))))
