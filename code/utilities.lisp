(in-package #:sealable-metaobjects)

(defun starts-with (item)
  (lambda (sequence)
    (typecase sequence
      (list (eql (first sequence) item))
      (sequence (eql (elt sequence 0) item))
      (otherwise nil))))

(defun block-name (function-name)
  (etypecase function-name
    ((and symbol (not null)) function-name)
    ((cons (eql setf) (cons symbol null)) (second function-name))))

(defun required-argument (name)
  (error "Required argument: ~S" name))

(defgeneric gensymify (object)
  (:method ((string string))
    (gensym (string-upcase (concatenate 'string string "-"))))
  (:method ((symbol symbol))
    (if (null (symbol-package symbol))
        ;; If we are dealing with uninterned symbols, we strip any
        ;; non-alphanumeric characters.  This has the effect that
        ;; gensymification of gensyms doesn't just add more and more
        ;; digits.
        (let ((name (symbol-name symbol)))
          (gensymify (subseq name 0 (1+ (position-if #'alpha-char-p name :from-end t)))))
        (gensymify (symbol-name symbol))))
  (:method ((object t))
    (gensymify (princ-to-string object))))

(defun null-lexical-environement-p (environment)
  (declare (ignorable environment))
  (or (null environment)
      #+sbcl (sb-c::null-lexenv-p environment)))

(defgeneric ensure-specializer (object)
  (:method ((class class))
    class)
  (:method ((symbol symbol))
    (or (find-class symbol nil)
        (call-next-method)))
  (:method ((cons cons))
    (if (typep cons '(cons (eql eql) (cons t null)))
        (intern-eql-specializer (second cons))
        (call-next-method)))
  (:method ((object t))
    (error "~@<~S is not a specializer, or a type designator that ~
                can be converted to a specializer.~:@>"
           object)))

(defun type-specifier-and (&rest type-specifiers)
  (let ((relevant (remove t type-specifiers)))
    (cond ((null relevant) t)
          ((null (cdr relevant)) (first relevant))
          (t `(and ,@relevant)))))

(defun type-specifier-or (&rest type-specifiers)
  (let ((relevant (remove nil type-specifiers)))
    (cond ((null relevant) nil)
          ((null (cdr relevant)) (first relevant))
          (t `(or ,@relevant)))))

(defun type-specifier-not (type-specifier)
  (cond ((eql type-specifier t) nil)
        ((eql type-specifier nil) t)
        (t `(not ,type-specifier))))
