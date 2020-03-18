(in-package #:sealable-metaobjects)

(defun starts-with (item)
  (lambda (sequence)
    (typecase sequence
      (list (eql (first sequence) item))
      (sequence (eql (elt sequence 0) item))
      (otherwise nil))))

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
