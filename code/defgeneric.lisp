(in-package #:sealable-metaobjects)

(cl:defgeneric generic-function-specializer-profile (generic-function)
  (:method ((generic-function generic-function))
    (make-list (length (generic-function-argument-precedence-order generic-function))
               :initial-element t)))

(cl:defgeneric (setf generic-function-specializer-profile) (value generic-function)
  (:method (value (generic-function generic-function))
    (assert (= (length value)
               (length (generic-function-argument-precedence-order generic-function))))
    (values)))

(defmacro defgeneric (function-name lambda-list &body options-and-methods)
  "A macro that behaves exactly like CL:DEFGENERIC, except that "
  (let* ((options (remove-if (starts-with :method) options-and-methods))
         (methods (remove-if-not (starts-with :method) options-and-methods))
         (required-args
           (loop for elt in lambda-list
                 until (member elt lambda-list-keywords)
                 collect elt))
         (argument-precedence-order
           (or (find-if (starts-with :argument-precedence-order) options)
               `(:argument-precedence-order ,@required-args)))
         (specializer-profile
           (mapcar
            (lambda (arg)
              (and (member arg (rest argument-precedence-order)) t))
            required-args)))
    `(progn
       (cl:defgeneric ,function-name ,lambda-list
         (:argument-precedence-order
          ,@(rest argument-precedence-order)
          ,@(set-difference required-args (rest argument-precedence-order)))
         ,@(remove argument-precedence-order options))
       (setf (generic-function-specializer-profile #',function-name)
             ',specializer-profile)
       ,@(loop for (nil . defmethod-body) in methods
               collect `(defmethod ,function-name ,@defmethod-body)))))
