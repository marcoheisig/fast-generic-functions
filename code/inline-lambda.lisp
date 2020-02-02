(in-package #:sealable-metaobjects)

(defmethod compute-method-inline-lambda
    ((generic-function inlineable-generic-function)
     (method potentially-inlineable-method)
     lambda
     environment)
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
        (let ((augmented-lambda-list
                (append
                 (reverse variables)
                 (unparse-ordinary-lambda-list '() '() nil '() nil auxiliary))))
          (trivial-macroexpand-all:macroexpand-all
           `(lambda ,augmented-lambda-list
              (declare (ignorable ,@(lambda-list-variables augmented-lambda-list)))
              ,@(subseq body 0 (position-if-not (starts-with 'declare) body))
              (block ,(block-name (generic-function-name generic-function))
                ,@(subseq body (position-if-not (starts-with 'declare) body))))
           environment))))))

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
    ;; We anonymize all bindings of the inline lambda, to prevent
    ;; accidental variable capture by any of the method lambdas.
    (let ((anonymized-lambda-list
            (anonymize-ordinary-lambda-list
             (unparse-ordinary-lambda-list
              required optional rest-var keyword allow-other-keys-p '()))))
      ;; Create the inline lambda.
      (trivial-macroexpand-all:macroexpand-all
       `(lambda ,anonymized-lambda-list
          (declare (ignorable ,@(lambda-list-variables anonymized-lambda-list)))
          (let ((.gf. #',(generic-function-name igf)))
            (declare (ignorable .gf.))
            #+sbcl(declare (sb-ext:disable-package-locks common-lisp:call-method))
            #+sbcl(declare (sb-ext:disable-package-locks common-lisp:make-method))
            #+sbcl(declare (sb-ext:disable-package-locks sb-pcl::check-applicable-keywords))
            (macrolet
                ;; SBCL introduces explicit keyword argument checking into
                ;; the effective method.  Since we do our own checking, we
                ;; can safely disable it.  However, we touch the relevant
                ;; variables to prevent unused variable warnings.
                (#+sbcl
                 (sb-pcl::check-applicable-keywords (&rest args)
                   (declare (ignore args))
                   `(progn sb-pcl::.valid-keys. sb-pcl::.keyargs-start. (values))))
              ,(wrap-in-call-method-macrolet
                (compute-effective-method
                 igf
                 (generic-function-method-combination igf)
                 applicable-methods)
                anonymized-lambda-list))))))))

(defun wrap-in-call-method-macrolet (form lambda-list)
  `(macrolet ((call-method (method &optional next-methods)
                (wrap-in-next-methods
                 (call-inlineable-method method ',lambda-list)
                 next-methods)))
     ,form))

(defun wrap-in-next-methods (form next-methods)
  (if (null next-methods)
      `(flet ((next-method-p () nil)
              (call-next-method ()
                (no-next-method .gf. (sb-pcl:class-prototype 'standard-method))))
         (declare (ignorable #'next-method-p #'call-next-method))
         ,form)
      (wrap-in-next-methods
       `(flet ((next-method-p () t)
               (call-next-method ()
                 (call-method ,(first next-methods) ,(rest next-methods))))
          (declare (ignorable #'next-method-p #'call-next-method))
          ,form)
       (rest next-methods))))

(defun call-inlineable-method (method lambda-list)
  (cond ((and (consp method)
              (eql (first method) 'make-method))
         (assert (null (rest (rest method))))
         (second method))
        ((and (typep method 'potentially-inlineable-method))
         (multiple-value-bind (g-required g-optional g-rest-var g-keyword)
             (parse-ordinary-lambda-list lambda-list)
           (multiple-value-bind (m-required m-optional m-rest-var m-keyword)
               (parse-ordinary-lambda-list (method-lambda-list method))
             (assert (= (length g-required)
                        (length m-required)))
             (assert (= (length g-optional)
                        (length m-optional)))
             (when (null g-rest-var)
               (assert (null m-rest-var)))
             `(funcall
               ,(method-inline-lambda method)
               ,@(mapcar #'required-info-variable g-required)
               ,@(loop for g-info in g-optional
                       for m-info in m-optional
                       append
                       (if (null (optional-info-suppliedp m-info))
                           `(,(optional-info-variable g-info))
                           `(,(optional-info-variable g-info)
                             ,(optional-info-suppliedp g-info))))
               ,@(if (null m-rest-var)
                     `()
                     `(,g-rest-var))
               ,@(loop for m-info in m-keyword
                       for g-info = (find (keyword-info-keyword m-info) g-keyword
                                          :key #'keyword-info-keyword)
                       append
                       (if (null (keyword-info-suppliedp m-info))
                           `(,(keyword-info-variable g-info))
                           `(,(keyword-info-variable g-info)
                             ,(keyword-info-suppliedp g-info))))))))
        (t
         (error "Cannot turn ~S into an inline method call." method))))
