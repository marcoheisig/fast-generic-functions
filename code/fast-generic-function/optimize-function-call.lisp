(in-package #:sealable-metaobjects)

(defmethod optimize-function-call
    ((fast-generic-function fast-generic-function)
     (static-call-signature static-call-signature))
  (let* ((applicable-methods
           (compute-applicable-methods
            fast-generic-function
            (static-call-signature-prototypes static-call-signature)))
         (effective-method-lambda-list
           (compute-effective-method-lambda-list
            fast-generic-function applicable-methods)))
    (cond (;; Inline the entire effective method.
           (every #'inlineable-method-p applicable-methods)
           (effective-method-lambda fast-generic-function static-call-signature nil))
          ;; Inline only the optional/keyword parsing step.
          ((and (keyword-generic-function-p fast-generic-function)
                (externalizable-object-p static-call-signature))
           (let ((anonymized-lambda-list
                   (anonymize-ordinary-lambda-list effective-method-lambda-list)))
             `(lambda ,anonymized-lambda-list
                (funcall
                 (load-time-value
                  (lookup-effective-method
                   #',(generic-function-name fast-generic-function)
                   ',static-call-signature
                   t))
                 ,@(lambda-list-variables anonymized-lambda-list)))))
          ;; Eliminate the dispatch function.
          ((externalizable-object-p static-call-signature)
           `(lambda (&rest args)
              (apply
               (load-time-value
                (lookup-effective-method
                 #',(generic-function-name fast-generic-function)
                 ',static-call-signature
                 nil))
               args)))
          ;; Give up.
          (t nil))))

(defun inlineable-method-p (method)
  (member 'inlineable (method-properties method)))

(defun keyword-generic-function-p (generic-function)
  (member '&key (generic-function-lambda-list generic-function)))

(defun compute-effective-method-lambda-list (generic-function applicable-methods)
  (multiple-value-bind (required optional rest-var keyword allow-other-keys-p)
      (parse-ordinary-lambda-list (generic-function-lambda-list generic-function))
    ;; The keywords of the effective method are the union of the keywords
    ;; of the generic function and the keywords of each applicable method.
    (dolist (method applicable-methods)
      (multiple-value-bind (req opt rst m-keyword m-allow-other-keys-p)
          (parse-ordinary-lambda-list (method-lambda-list method))
        (declare (ignore req opt rst))
        (setf allow-other-keys-p (and allow-other-keys-p m-allow-other-keys-p))
        (dolist (keyword-info m-keyword)
          (pushnew keyword-info keyword :key #'keyword-info-keyword))))
    (unparse-ordinary-lambda-list
     required optional rest-var keyword allow-other-keys-p '())))

(defun effective-method-lambda
    (generic-function static-call-signature flatten-arguments)
  (let* ((applicable-methods
           (compute-applicable-methods
            generic-function
            (static-call-signature-prototypes static-call-signature)))
         (effective-method-lambda-list
           (compute-effective-method-lambda-list
            generic-function applicable-methods))
         (anonymized-lambda-list
           (anonymize-ordinary-lambda-list effective-method-lambda-list)))
    `(lambda ,(if flatten-arguments
                  (lambda-list-variables anonymized-lambda-list)
                  anonymized-lambda-list)
       ,@(loop for type in (static-call-signature-types static-call-signature)
               for argument in anonymized-lambda-list
               collect `(declare (ignorable ,argument))
               collect `(declare (type ,type ,argument)))
       ,(expand-effective-method-body
         (compute-effective-method
          generic-function
          (generic-function-method-combination generic-function)
          applicable-methods)
         generic-function
         anonymized-lambda-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Effective Method Caching

(defvar *effective-method-cache* (make-hash-table :test #'equal))

(declaim (ftype (function (t t t) function) lookup-effective-method))
(defun lookup-effective-method
    (generic-function static-call-signature flatten-arguments)
  (let ((key (list* generic-function
                    flatten-arguments
                    (static-call-signature-prototypes static-call-signature))))
    (multiple-value-bind (value present-p)
        (gethash key *effective-method-cache*)
      (if present-p
          value
          (setf (gethash key *effective-method-cache*)
                (compile nil (effective-method-lambda
                              generic-function
                              static-call-signature
                              flatten-arguments)))))))
