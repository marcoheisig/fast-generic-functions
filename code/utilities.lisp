(in-package #:sealable-metaobjects)

(defun block-name (function-name)
  (etypecase function-name
    ((and symbol (not null)) function-name)
    ((cons (eql setf) (cons symbol null)) (second function-name))))

(defun required-argument (name)
  (error "Required argument: ~S" name))
