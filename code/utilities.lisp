(in-package #:sealable-metaobjects)

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
        ;; If we are dealing with uninterned symbols, we strip any trailing
        ;; digits.  This has the effect that gensymification of gensyms
        ;; doesn't just add more and more digits.
        (let ((name (symbol-name symbol)))
          (gensymify (subseq name 0 (1+ (position-if-not #'digit-char-p name :from-end t)))))
        (gensymify (symbol-name symbol))))
  (:method ((object t))
    (gensymify (princ-to-string object))))
