(in-package #:sealable-metaobjects)

(defmethod compute-method-inline-lambda (method lambda environment)
  )

(defun extract-method-body (lambda environment)
  (assert (eql (first lambda) 'lambda))
  (assert (listp (second lambda)))
  (let* ((body (cddr lambda))
         (end-of-declarations (position-if-not #'declare-form-p body)))
    (append
     (subseq body 0 end-of-declarations)
     (mapcar
      (lambda (form)
        (trivial-macroexpand-all:macroexpand-all form environment))
      (subseq body end-of-declarations)))))

(defun declare-form-p (form)
  (and (consp form)
       (eql (car form) 'declare)))

(defmethod compute-generic-function-inline-lambda
    ((igf inlineable-generic-function) applicable-methods)
  (multiple-value-bind (required optional rest-var keyword allow-other-keys-p)
      (parse-ordinary-lambda-list (generic-function-lambda-list igf))
    ;; The keywords of the effective method are the union of the keywords
    ;; of the generic function and the keywords of each applicable method.
    (dolist (method applicable-methods)
      (multiple-value-bind (req opt rst m-keyword m-allow-other-keys-p)
          (parse-ordinary-lambda-list (method-lambda-list method))
        (declare (ignore req opt rst))
        (setf allow-other-keys-p (and allow-other-keys-p m-allow-other-keys-p))
        (dolist (keyword-info m-keyword)
          (pushnew keyword-info keyword :key #'keyword-info-keyword))))
    ;; Now replace all bindings of the extended generic function lambda
    ;; list with gensyms.
    (multiple-value-setq (required optional rest-var keyword allow-other-keys-p)
      (anonymize-ordinary-lambda-list
       (unparse-ordinary-lambda-list required optional rest-var keyword allow-other-keys-p '())))
    (break)
    ))
