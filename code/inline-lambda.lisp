(in-package #:sealable-metaobjects)

(defmethod compute-method-inline-lambda
    ((method potentially-inlineable-method) lambda environment)
  (destructuring-bind (lambda-symbol lambda-list &rest body) lambda
    (assert (eql lambda-symbol 'lambda))
    (multiple-value-bind (required optional rest-var keyword allow-other-keys-p auxiliary)
        (parse-ordinary-lambda-list lambda-list)
      (declare (ignore allow-other-keys-p))
      (let ((variables '()))
        (dolist (info required)
          (push (required-info-variable info) variables))
        (dolist (info optional)
          (push (optional-info-variable info) variables)
          (when (optional-info-suppliedp info)
            (push (optional-info-suppliedp info) variables)))
        (unless (null rest-var)
          (push rest-var variables))
        (dolist (info keyword)
          (push (keyword-info-variable info) variables)
          (when (keyword-info-suppliedp info)
            (push (keyword-info-suppliedp info) variables)))
        `(lambda ,(append
                   (reverse variables)
                   (unparse-ordinary-lambda-list '() '() nil '() nil auxiliary))
           ,@body)))))

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
