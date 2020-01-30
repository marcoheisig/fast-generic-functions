(in-package #:sealable-metaobjects)

(defun block-name (function-name)
  (etypecase function-name
    ((and symbol (not null)) function-name)
    ((cons (eql setf) (cons symbol null)) (second function-name))))

(defun declare-form-p (form)
  (and (consp form)
       (eql (car form) 'declare)))
