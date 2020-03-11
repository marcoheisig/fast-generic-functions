(in-package #:sealable-metaobjects)

(defun expand-effective-method-body
    (effective-method generic-function lambda-list)
  (trivial-macroexpand-all:macroexpand-all
   `(let ((.gf. #',(generic-function-name generic-function)))
      (declare (ignorable .gf.))
      #+sbcl(declare (sb-ext:disable-package-locks common-lisp:call-method))
      #+sbcl(declare (sb-ext:disable-package-locks common-lisp:make-method))
      #+sbcl(declare (sb-ext:disable-package-locks sb-pcl::check-applicable-keywords))
      #+sbcl(declare (sb-ext:disable-package-locks sb-pcl::%no-primary-method))
      (macrolet
          (;; SBCL introduces explicit keyword argument checking into
           ;; the effective method.  Since we do our own checking, we
           ;; can safely disable it.  However, we touch the relevant
           ;; variables to prevent unused variable warnings.
           #+sbcl
           (sb-pcl::check-applicable-keywords (&rest args)
             (declare (ignore args))
             `(progn sb-pcl::.valid-keys. sb-pcl::.keyargs-start. (values)))
           ;; SBCL introduces a magic form to report when there are no
           ;; primary methods.  The problem is that this form contains a
           ;; reference to the literal generic function, which is not an
           ;; externalizable object.  Our solution is to replace it with
           ;; something portable.
           #+sbcl
           (sb-pcl::%no-primary-method (&rest args)
             (declare (ignore args))
             `(error "No primary method for the generic function ~S." .gf.)))
        ,(wrap-in-call-method-macrolet
          effective-method
          generic-function
          lambda-list)))))

(defun wrap-in-call-method-macrolet (form generic-function lambda-list)
  `(macrolet ((call-method (method &optional next-methods)
                (expand-call-method
                 method
                 next-methods
                 ',lambda-list
                 ',(class-name
                    (generic-function-method-class generic-function)))))
     ,(wrap-in-reinitialize-arguments form lambda-list)))

(defun wrap-in-reinitialize-arguments (form lambda-list)
  (let ((anonymized-lambda-list
          (anonymize-ordinary-lambda-list lambda-list)))
    `(flet ((reinitialize-arguments ,anonymized-lambda-list
              ,@(mapcar
                 (lambda (place value)
                   `(setf ,place ,value))
                 (lambda-list-variables lambda-list)
                 (lambda-list-variables anonymized-lambda-list))))
       (declare (ignorable #'reinitialize-arguments))
       (declare (inline reinitialize-arguments))
       ,form)))

(defun expand-call-method (method next-methods lambda-list method-class)
  (wrap-in-next-methods
   (call-fast-method-lambda
    (coerce-to-fast-method method lambda-list method-class)
    lambda-list)
   next-methods
   lambda-list
   method-class))

(defun coerce-to-fast-method (method lambda-list method-class)
  (cond ((typep method 'fast-method)
         method)
        ((and (consp method)
              (eql (car method) 'make-method)
              (null (cddr method)))
         (make-instance method-class
           :lambda-list lambda-list
           :specializers (make-list (length (parse-ordinary-lambda-list lambda-list))
                                    :initial-element (find-class 't))
           :qualifiers '()
           :function #'values
           '.lambda.
           `(lambda ,lambda-list
              (declare (ignorable ,@(lambda-list-variables lambda-list)))
              ,(second method))))
        (t
         (error "Cannot turn ~S into an inlineable method."
                method))))

(defun wrap-in-next-methods (form next-methods lambda-list method-class)
  (if (null next-methods)
      `(flet ((next-method-p () nil)
              (call-next-method ()
                (apply
                 #'no-next-method
                 .gf.
                 (class-prototype (find-class ',method-class))
                 ,@(lambda-list-apply-arguments lambda-list))))
         (declare (ignorable #'next-method-p #'call-next-method))
         ,form)
      (wrap-in-next-methods
       `(flet ((next-method-p () t)
               (call-next-method (&rest args)
                 (unless (null args)
                   (apply #'reinitialize-arguments args))
                 (call-method ,(first next-methods) ,(rest next-methods))))
          (declare (ignorable #'next-method-p #'call-next-method))
          ,form)
       (rest next-methods)
       lambda-list
       method-class)))

(defun call-fast-method-lambda (method lambda-list)
  (multiple-value-bind (g-required g-optional g-rest-var g-keyword)
      (parse-ordinary-lambda-list lambda-list)
    (multiple-value-bind (m-required m-optional m-rest-var m-keyword)
        (parse-ordinary-lambda-list (method-lambda-list method))
      ;; Assert that the method has arguments that are congruent to those
      ;; of the corresponding generic function.
      (assert (or (= (length g-required)
                     (length m-required))))
      (assert (= (length g-optional)
                 (length m-optional)))
      (when (null g-rest-var)
        (assert (null m-rest-var)))
      `(funcall
        ,(fast-method-lambda method)
        ;; Required arguments.
        ,@(mapcar #'required-info-variable g-required)
        ;; Optional arguments.
        ,@(loop for g-info in g-optional
                for m-info in m-optional
                append
                (let ((initform
                        `(if ,(optional-info-suppliedp g-info)
                             ,(optional-info-variable g-info)
                             ,(optional-info-initform m-info))))
                  (if (null (optional-info-suppliedp m-info))
                      `(,initform)
                      `(,initform ,(optional-info-suppliedp g-info)))))
        ;; The rest argument.
        ,@(if (null m-rest-var)
              `()
              `(,g-rest-var))
        ;; Keyword arguments.
        ,@(loop for m-info in m-keyword
                for g-info = (find (keyword-info-keyword m-info) g-keyword
                                   :key #'keyword-info-keyword)
                append
                (let ((initform
                        `(if ,(keyword-info-suppliedp g-info)
                             ,(keyword-info-variable g-info)
                             ,(keyword-info-initform m-info))))
                  (if (null (keyword-info-suppliedp m-info))
                      `(,initform)
                      `(,initform ,(keyword-info-suppliedp g-info)))))))))

