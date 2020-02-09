(in-package #:sealable-metaobjects-test-suite)

;;; GENERIC-FIND

(defmethod generic-find (elt (list list) &key (test #'eql))
  (loop for item in list
        when (funcall test item elt) do (return item)))

(defmethod generic-find (elt (vector vector) &key (test #'eql))
  (cl:find elt vector :test test))

;;; GENERIC-BINARY-+

(defmethod generic-binary-+ :around ((a number) (b number))
  (print '(:around number number))
  (print (call-next-method)))

(defmethod generic-binary-+ ((a point) (b point))
  (print '(point point))
  (make-point
    :x (+ (point-x a) (point-x b))
    :y (+ (point-y a) (point-y b))))

(defmethod generic-binary-+ ((a number) (b number))
  (print '(number number))
  (+ a b))

(defmethod generic-binary-+ ((a integer) (b integer))
  (print '(integer integer))
  (+ a b))

(defmethod generic-binary-+ ((a double-float) (b double-float))
  (print '(double-float double-float))
  (+ a b))

(defmethod generic-binary-+ ((a character) (b character))
  (print '(character character))
  (+ (char-code a)
     (char-code b)))

(defmethod generic-binary-+ ((a double-float) (b t))
  (print '(double-float t))
  (+ a b))

;;; GENERIC-BINARY-*

(defmethod generic-binary-* ((a double-float) (b double-float))
  (* a b))

(defmethod generic-binary-* ((a single-float) (b single-float))
  (* a b))

;;; REST-ARGS

(defmethod rest-args (a1 (a2 number) &rest rest)
  (+ a1 a2 (length rest)))

(seal-generic-function #'generic-find)
(seal-generic-function #'generic-binary-+)
(seal-generic-function #'generic-binary-*)
(seal-generic-function #'rest-args)
