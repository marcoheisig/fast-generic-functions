(in-package #:sealable-metaobjects-test-suite)

;;; GENERIC-BINARY-+

(defmethod generic-binary-+ :around ((a number) (b number))
  (print '(:around number number))
  (print (call-next-method)))

(defmethod generic-binary-+ ((a point) (b point))
  (print '(point point))
  (make-instance 'point
    :x (+ (x a) (x b))
    :y (+ (y a) (y b))))

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

(seal-generic-function #'generic-binary-+)
(seal-generic-function #'generic-binary-*)
(seal-generic-function #'rest-args)
