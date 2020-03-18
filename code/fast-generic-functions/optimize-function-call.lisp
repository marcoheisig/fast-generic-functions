(in-package #:fast-generic-functions)

(defmethod optimize-function-call :around
  ((fast-generic-function fast-generic-function)
   (static-call-signature static-call-signature))
  (call-next-method)
  #+(or)(print (call-next-method)))

(defmethod optimize-function-call
    ((fast-generic-function fast-generic-function)
     (static-call-signature static-call-signature))
  (let ((applicable-methods
          (compute-applicable-methods
           fast-generic-function
           (static-call-signature-prototypes static-call-signature))))
    (cond (;; Inline the entire effective method.
           (every #'inlineable-method-p applicable-methods)
           (effective-method-lambda fast-generic-function static-call-signature nil))
          ;; Inline only the optional/keyword parsing step.
          ((and (externalizable-object-p static-call-signature)
                (intersection (generic-function-lambda-list fast-generic-function)
                              '(&optional &key &rest)))
           (let ((lambda-list
                   (anonymize-ordinary-lambda-list
                    (compute-effective-method-lambda-list
                     fast-generic-function applicable-methods))))
             `(lambda ,lambda-list
                (funcall
                 (load-time-value
                  (the function
                       (lookup-flat-effective-method
                        #',(generic-function-name fast-generic-function)
                        ',static-call-signature)))
                 ,@(lambda-list-variables lambda-list)))))
          ;; Eliminate the dispatch function.
          ((externalizable-object-p static-call-signature)
           `(lambda (&rest args)
              (apply
               (load-time-value
                (the function
                     (lookup-full-effective-method
                      #',(generic-function-name fast-generic-function)
                      ',static-call-signature)))
               args)))
          ;; Give up.
          (t nil))))

(defun inlineable-method-p (method)
  (member 'inlineable (method-properties method)))

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
       (declare (optimize (safety 0)))
       ,@(loop for type in (static-call-signature-types static-call-signature)
               for argument in anonymized-lambda-list
               collect `(declare (ignorable ,argument))
               collect `(declare (type ,type ,argument)))
       (locally (declare (optimize (safety 1)))
         ,(expand-effective-method-body
           (compute-effective-method
            generic-function
            (generic-function-method-combination generic-function)
            applicable-methods)
           generic-function
           anonymized-lambda-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the Effective Method Lambda List

(defun compute-effective-method-lambda-list (generic-function applicable-methods)
  (multiple-value-bind (required optional rest-var keyword allow-other-keys)
      (parse-ordinary-lambda-list (generic-function-lambda-list generic-function))
    (let ((method-parses
            (mapcar
             (lambda (method)
               (multiple-value-list
                (parse-ordinary-lambda-list
                 (method-lambda-list method))))
             applicable-methods)))
      (unparse-ordinary-lambda-list
       (merge-required-infos required (mapcar #'first method-parses))
       (merge-optional-infos optional (mapcar #'second method-parses))
       rest-var
       (merge-keyword-infos keyword (mapcar #'fourth method-parses))
       (merge-allow-other-keys allow-other-keys (mapcar #'fifth method-parses))
       '()))))

(defun merge-required-infos (g-required m-requireds)
  (dolist (m-required m-requireds g-required)
    (assert (= (length m-required)
               (length g-required)))))

(defun merge-optional-infos (g-optional m-optionals)
  (let ((n (length g-optional)))
    (dolist (m-optional m-optionals)
      (assert (= (length m-optional) n)))
    (unless (zerop n)
      (loop for g-info in g-optional
            for m-infos in (apply #'mapcar #'list m-optionals)
            collect
            ;; Now we have two cases - the one is that at least one method
            ;; cares about the suppliedp flag, the other one is that no
            ;; method cares.  Even if a method doesn't reference the
            ;; suppliedp flag itself, it may still need it to decide whether
            ;; to supply its initform or not.  Because of this, the suppliedp
            ;; parameter can only be discarded globally when the initforms of
            ;; all methods are constant and equal.
            (let ((global-initform (optional-info-initform (first m-infos)))
                  (no-one-cares (not (optional-info-suppliedp (first m-infos)))))
              (dolist (m-info m-infos)
                (with-accessors ((variable optional-info-variable)
                                 (initform optional-info-initform)
                                 (suppliedp optional-info-suppliedp))
                    m-info
                  (unless (and (constantp initform)
                               (equal initform global-initform)
                               (not suppliedp))
                    (setf no-one-cares nil))))
              (if no-one-cares
                  (make-instance 'optional-info
                    :variable (optional-info-variable g-info)
                    :initform global-initform)
                  (make-instance 'optional-info
                    :variable (optional-info-variable g-info)
                    :initform nil
                    :suppliedp (optional-info-suppliedp g-info))))))))

(defun merge-keyword-infos (g-keyword m-keywords)
  ;; First we assemble an alist whose keys are keywords and whose values
  ;; are all method keyword info objects that read this keyword.
  (let ((alist '()))
    (dolist (g-info g-keyword)
      (pushnew (list (keyword-info-keyword g-info)) alist))
    (dolist (m-keyword m-keywords)
      (dolist (m-info m-keyword)
        (let* ((key (keyword-info-keyword m-info))
               (entry (assoc key alist)))
          (if (consp entry)
              (push m-info (cdr entry))
              (push (list key m-info) alist)))))
    (loop for (key . m-infos) in alist
          collect
          ;; Merging keyword info objects is handled just like in the case
          ;; of optional info objects above.
          (let ((global-initform (keyword-info-initform (first m-infos)))
                (no-one-cares (not (keyword-info-suppliedp (first m-infos))))
                ;; Not actually g-info, but we need some place to grab a
                ;; variable name form.
                (g-info (or (find key g-keyword :key #'keyword-info-keyword)
                            (first m-infos))))
            (dolist (m-info m-infos)
              (with-accessors ((initform keyword-info-initform)
                               (suppliedp keyword-info-suppliedp))
                  m-info
                (unless (and (constantp initform)
                             (equal initform global-initform)
                             (not suppliedp))
                  (setf no-one-cares nil))))
            (if no-one-cares
                (make-instance 'keyword-info
                  :keyword key
                  :variable (keyword-info-variable g-info)
                  :initform global-initform)
                (make-instance 'keyword-info
                  :keyword key
                  :variable (keyword-info-variable g-info)
                  :initform nil
                  :suppliedp (or (keyword-info-suppliedp g-info)
                                 (gensymify "SUPPLIEDP"))))))))

(defun merge-allow-other-keys (g-allow-other-keys m-allow-other-keys-list)
  (reduce
   (lambda (a b) (or a b))
   m-allow-other-keys-list
   :initial-value g-allow-other-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Effective Method Lookup

(declaim (ftype (function * function) lookup-full-effective-method))
(declaim (ftype (function * function) lookup-flat-effective-method))

(defun lookup-full-effective-method
    (generic-function static-call-signature)
  (with-accessors ((alist full-effective-method-cache)) generic-function
    (let* ((key (static-call-signature-types static-call-signature))
           (entry (assoc key alist :test #'equal)))
      (if (consp entry)
          (cdr entry)
          (let ((fn (compile nil (effective-method-lambda
                                  generic-function
                                  static-call-signature
                                  nil))))
            (push (cons key fn) alist)
            fn)))))

(defun lookup-flat-effective-method
    (generic-function static-call-signature)
  (with-accessors ((alist flat-effective-method-cache)) generic-function
    (let* ((key (static-call-signature-types static-call-signature))
           (entry (assoc key alist :test #'equal)))
      (if (consp entry)
          (cdr entry)
          (let ((fn (compile nil (effective-method-lambda
                                  generic-function
                                  static-call-signature
                                  t))))
            (push (cons key fn) alist)
            fn)))))
