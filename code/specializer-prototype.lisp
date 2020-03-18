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
       ;; The prototype must not be a member of the excluded objects.
       (when (not (member prototype excluded-objects))
         ;; The prototype must not be of one of the excluded types.
         (when (notany
                (lambda (excluded-type)
                  (typep prototype excluded-type))
                excluded-types)
           (return-from specializer-prototype (values prototype t)))))
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
                 (loop for prototype in (gethash class *class-prototypes* '()) do
                   (funcall function prototype))
                 (mapc #'visit-class (class-direct-subclasses class))
                 ;; CLASS-PROTOTYPE is difficult to handle...
                 (when (class-finalized-p class)
                   (let ((prototype (class-prototype class)))
                     ;; Surprisingly, some implementations don't always
                     ;; return a CLASS-PROTOTYPE that is an instance of the
                     ;; given class.  So we only scan the prototype if it is
                     ;; actually valid.
                     (when (typep prototype class)
                       (funcall function prototype)))))))
      (visit-class class))))

(defun register-class-prototype (prototype)
  (pushnew prototype (gethash (class-of prototype) *class-prototypes* '())
           :test #'equalp))

;; Register list prototypes.
(register-class-prototype '(.prototype.))
(register-class-prototype nil)

(defparameter *array-element-types*
  (remove-duplicates
   (mapcar #'upgraded-array-element-type
           (append '(short-float single-float double-float long-float base-char character t)
                   (loop for bits from 1 to 64
                         collect `(unsigned-byte ,bits)
                         collect `(signed-byte ,bits))))
   :test #'equal))

;; Register vector and array prototypes.
(loop for adjustable in '(nil t) do
  (loop for fill-pointer in '(nil t) do
    (loop for dimensions in '(() (2) (2 2)) do
      (loop for element-type in *array-element-types* do
        (let ((storage-vector (make-array (reduce #'* dimensions) :element-type element-type)))
          (loop for displaced-to in (list nil storage-vector) do
            (register-class-prototype
             (make-array dimensions
                         :adjustable adjustable
                         :fill-pointer (and (= 1 (length dimensions)) fill-pointer)
                         :element-type element-type
                         :displaced-to displaced-to))))))))

;; Register integer and rational prototypes.
(loop for integer in '(19 1337 1338 91676) do
  (register-class-prototype (+ integer))
  (register-class-prototype (- integer)))
(loop for bits = 1 then (* bits 2) until (>= bits 512)
      for value = (expt 2 bits) do
        (loop for value in (list (1+ value) value (1- value)) do
          (register-class-prototype value)
          (register-class-prototype (- value))
          (register-class-prototype (/ value 17))))

;; Register float and complex float prototypes.
(register-class-prototype pi)
(register-class-prototype (- pi))
(register-class-prototype (exp 1S0))
(register-class-prototype (exp 1F0))
(register-class-prototype (exp 1D0))
(register-class-prototype (exp 1L0))
(mapcar #'register-class-prototype
        (list most-positive-short-float
              most-positive-single-float
              most-positive-double-float
              most-positive-long-float
              most-negative-short-float
              most-negative-single-float
              most-negative-double-float
              most-positive-long-float
              short-float-epsilon
              single-float-epsilon
              double-float-epsilon
              long-float-epsilon
              short-float-negative-epsilon
              single-float-negative-epsilon
              double-float-negative-epsilon
              long-float-negative-epsilon))
(loop for base in '(-0.7L0 -0.1L0 -0.0L0 +0.0L0 +0.1L0 +0.7L0) do
  (loop for fp-type in '(short-float single-float double-float long-float) do
    (loop for exponent in '(1 2 3 5 7 23 99) do
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
