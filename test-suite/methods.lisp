(in-package #:fast-generic-functions-test-suite)

;;; GENERIC-FIND

(defmethod generic-find (elt (list list) &key (test #'eql))
  (loop for item in list
        when (funcall test item elt) do (return item)))

(defmethod generic-find (elt (vector vector) &key (test #'eql))
  (cl:find elt vector :test test))

(seal-domain #'generic-find '(t sequence))

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

(defmethod generic-binary-+ ((a double-float) (b number))
  (print '(double-float t))
  (+ a b))

(seal-domain #'generic-binary-+ '(number number))

;;; GENERIC-BINARY-*

(defmethod generic-binary-* ((a double-float) (b double-float))
  (declare (method-properties inlineable))
  (* a b))

(defmethod generic-binary-* ((a single-float) (b single-float))
  (declare (method-properties inlineable))
  (* a b))

(seal-domain #'generic-binary-* '(number number))

;;; REST-ARGS

(defmethod rest-args (a1 (a2 number) &rest rest)
  (+ a1 a2 (length rest)))

(seal-domain #'rest-args '(t number))

;;; CRAZY-NEXT-METHOD-CALLER

(defmethod crazy-next-method-caller ((a number) (b number))
  (declare (method-properties inlineable))
  (+ a b))

(defmethod crazy-next-method-caller ((a real) (b real))
  (declare (method-properties inlineable))
  (call-next-method (* b 5) (* a 7)))

(defmethod crazy-next-method-caller ((a integer) (b integer))
  (declare (method-properties inlineable))
  (call-next-method (+ b 2) (+ a 7)))

(seal-domain #'crazy-next-method-caller '(number number))

;;; KEYWORD-FUNCTION

(defmethod keyword-function ((x integer) &key y z)
  (list x y z (call-next-method x :y y)))

(defmethod keyword-function ((x real) &key (y 3) (z 4))
  (list x y z))

(seal-domain #'keyword-function '(real))
