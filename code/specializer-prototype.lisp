(in-package #:sealable-metaobjects)

;;; Finding a suitable prototype for eql specializers is easy.
(defmethod specializer-prototype ((eql-specializer eql-specializer)
                                  &optional excluded-specializers)
  (if (member eql-specializer excluded-specializers)
      (values nil nil)
      (values (eql-specializer-object eql-specializer) t)))

;;; The difficult part is to find suitable prototypes for specializers that
;;; are classes.  Ideally, we want several prototypes for each class, such
;;; that we can avoid collisions with excluded specializers.  Our technique
;;; is to find prototypes from two sources - the value returned by the MOP
;;; function CLASS-PROTOTYPE, and manually curated lists of prototypes for
;;; each class, which we store in the hash table *CLASS-PROTOTYPES*.

(defvar *class-prototypes* (make-hash-table :test #'eq))

(defun class-prototypes (class)
  ;; Surprisingly, some implementations return a CLASS-PROTOTYPE that is
  ;; not an instance of the given class.
  (let ((prototype (class-prototype class)))
    (if (typep prototype class)
        (list* prototype (gethash class *class-prototypes*))
        (values (gethash class *class-prototypes*)))))

(defun eql-specializer-p (object)
  (typep object 'eql-specializer))

(defmethod specializer-prototype ((class class)
                                  &optional excluded-specializers)
  (let* ((excluded-non-eql-specializers (remove-if #'eql-specializer-p excluded-specializers))
         (excluded-eql-specializers (remove-if-not #'eql-specializer-p excluded-specializers))
         (excluded-objects (mapcar #'eql-specializer-object excluded-eql-specializers)))
    (dolist (prototype (class-prototypes class) (values nil nil))
      (when (and (not (member prototype excluded-objects))
                 (notany
                  (lambda (excluded-specializer)
                    (typep prototype (specializer-type excluded-specializer)))
                  excluded-non-eql-specializers))
        (return (values prototype t))))))

(defun register-class-prototype (prototype &optional (class (class-of prototype)))
  (when (symbolp class)
    (setf class (find-class class)))
  (assert (typep prototype class))
  (pushnew prototype (gethash class *class-prototypes* '())))

(register-class-prototype '(.prototype.) 'list)

(register-class-prototype #(.prototype.) 'vector)
