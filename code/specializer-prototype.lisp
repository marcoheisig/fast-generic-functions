(in-package #:sealable-metaobjects)

;;; Finding a suitable prototype for eql specializers is easy.
(defmethod specializer-prototype ((eql-specializer eql-specializer)
                                  &optional excluded-specializers)
  (if (member eql-specializer excluded-specializers)
      (values nil nil)
      (values (eql-specializer-object eql-specializer) t)))

(defun eql-specializer-p (object)
  (typep object 'eql-specializer))

(defmethod specializer-prototype ((class class) &optional excluded-specializers)
  (let* ((excluded-non-eql-specializers (remove-if #'eql-specializer-p excluded-specializers))
         (excluded-eql-specializers (remove-if-not #'eql-specializer-p excluded-specializers))
         (excluded-objects (mapcar #'eql-specializer-object excluded-eql-specializers))
         (excluded-types (mapcar #'specializer-type excluded-non-eql-specializers)))
    (map-class-prototypes
     (lambda (prototype)
       (when (and (not (member prototype excluded-objects))
                  (notany
                   (lambda (excluded-type)
                     (typep prototype excluded-type))
                   excluded-types))
         (return-from specializer-prototype (values prototype t))))
     class)
    (values nil nil)))

;;; The difficult part is to find suitable prototypes for specializers that
;;; are classes.  Ideally, we want several prototypes for each class, such
;;; that we can avoid collisions with excluded specializers.  Our technique
;;; is to find prototypes from two sources - the value returned by the MOP
;;; function CLASS-PROTOTYPE, and manually curated lists of prototypes for
;;; each class, which we store in the hash table *CLASS-PROTOTYPES*.

(defvar *class-prototypes* (make-hash-table :test #'eq))

(defun map-class-prototypes (function class)
  (let ((visited-classes (make-hash-table :test #'eq)))
    (labels ((visit-class (class)
               (unless (gethash class visited-classes)
                 (setf (gethash class visited-classes) t)
                 ;; Surprisingly, some implementations return a
                 ;; CLASS-PROTOTYPE that is not an instance of the given
                 ;; class.
                 (let ((prototype (class-prototype class)))
                   (when (typep prototype class)
                     (funcall function prototype)))
                 (loop for prototype in (gethash class *class-prototypes* '()) do
                   (funcall function prototype))
                 (mapc #'visit-class (class-direct-subclasses class)))))
      (visit-class class))))

(defun register-class-prototype (prototype)
  (pushnew prototype (gethash (class-of prototype) *class-prototypes* '())
           :test #'equalp))

;; Register list prototypes.
(register-class-prototype '(.prototype.))
(register-class-prototype nil)

;; Register vector and array prototypes.
(loop for dimensions in '(() (2) (2 2)) do
  (loop for bits from 1 to 64 do
    (register-class-prototype (make-array dimensions :element-type `(signed-byte ,bits)))
    (register-class-prototype (make-array dimensions :element-type `(unsigned-byte ,bits))))
  (loop for type in '(short-float single-float double-float long-float base-char character t) do
    (register-class-prototype (make-array dimensions :element-type type))))

;; Register integer prototypes.
(loop for integer in '(0 3 19 1337 1338 91676) do
  (register-class-prototype (+ integer))
  (register-class-prototype (- integer)))
(loop for bits = 1 then (* bits 2) until (>= bits 64) do
  (register-class-prototype (1+ (+ (expt 2 bits))))
  (register-class-prototype (1- (+ (expt 2 bits))))
  (register-class-prototype (+ (expt 2 bits)))
  (register-class-prototype (- (expt 2 bits)))
  (register-class-prototype (1- (- (expt 2 bits))))
  (register-class-prototype (1+ (- (expt 2 bits)))))

;; Register float and complex float prototypes.
(register-class-prototype pi)
(loop for base in '(-0.7L0 -0.1L0 -0.0L0 +0.0L0 +0.1L0 +0.7L0) do
  (loop for fp-type in '(short-float single-float double-float long-float) do
    (loop for exponent in '(1 2 3 5) do
      (let ((float (scale-float (coerce base fp-type) exponent)))
        (register-class-prototype float)
        (register-class-prototype (complex (float 0 float) float))))))

;; Register character prototypes.
(loop for char across "The quick brown fox jumps over the lazy dog." do
  (register-class-prototype (char-downcase char))
  (register-class-prototype (char-upcase char)))
(loop for char across "0123456789!$\"'(),_-./:;?+<=>#%&*@[\\]{\|}`^~" do
  (register-class-prototype char))
(loop for char in '(#\backspace #\tab #\newline #\linefeed #\page #\return #\space #\rubout) do
  (register-class-prototype char))
