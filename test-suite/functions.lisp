(in-package #:sealable-metaobjects-test-suite)

(defun generic-+ (&rest things)
  (cond ((null things) 0)
        ((null (rest things)) (first things))
        (t (reduce #'generic-binary-+ things))))

(define-compiler-macro generic-+ (&rest things)
  (cond ((null things) 0)
        ((null (rest things)) (first things))
        (t
         (flet ((symbolic-generic-binary-+ (a b)
                  `(generic-binary-+ ,a ,b)))
           (reduce #'symbolic-generic-binary-+ things)))))

(defun generic-* (&rest things)
  (cond ((null things) 0)
        ((null (rest things)) (first things))
        (t (reduce #'generic-binary-* things))))

(define-compiler-macro generic-* (&rest things)
  (cond ((null things) 1)
        ((null (rest things)) (first things))
        (t
         (flet ((symbolic-generic-binary-* (a b)
                  `(generic-binary-* ,a ,b)))
           (reduce #'symbolic-generic-binary-* things)))))

(defun generic-find-user (item list)
  (declare (list list))
  (generic-find item list))

(assert (= 42 (generic-find-user 42 '(1 3 17 42 9))))

(defun generic-+-user-1 (x y z)
  (declare (double-float x y z))
  (generic-+ x y z))

(defun generic-+-user-2 (p)
  (declare (point p))
  (generic-+ p p))

(defun generic-*-user (x y z)
  (declare (single-float x y z))
  (generic-* x y z))

(assert (= (generic-*-user 5.0 6.0 7.0) 210.0))

(defun rest-args-user (x y z)
  (declare (single-float x y z))
  (rest-args x y z z z))

(assert (= (rest-args-user 5.0 6.0 7.0) 14))

(defun crazy-next-method-caller-user (a b)
  (declare (integer a) (integer b))
  (crazy-next-method-caller a b))

(assert (= (crazy-next-method-caller-user 5 6) 116))
(assert (= (crazy-next-method-caller-user 1 2) 68))

(defun keyword-function-user (x y)
  (keyword-function x :y y))

(assert (equal (keyword-function-user 8 3) '(8 3 nil (8 3 4))))
(assert (equal (keyword-function-user 1 2) '(1 2 nil (1 2 4))))
