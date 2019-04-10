(in-package #:sealable-metaobjects)

;;; The following code conservatively detects whether a given form can be
;;; inlined when occurring in the body of a sealable method.  We are quite
;;; picky here, but, as a result, we can do powerful optimizations later
;;; on.
;;;
;;; The following conditions prevent inlining:
;;;
;;; - An occurrence of CALL-NEXT-METHOD.
;;;
;;; - An occurrence of NEXT-METHOD-P.
;;;
;;; - An occurrence of the special form LOAD-TIME-VALUE.
;;;
;;; - Any occurrence of an unknown special form.

(defun keyword-lambda-list-p (lambda-list)
  (let ((key-pos (position '&key lambda-list))
        (aok-pos (position '&allow-other-keys lambda-list))
        (rest-pos (position '&rest lambda-list)))
    (and key-pos
         (or (not aok-pos)
             (> aok-pos (1+ key-pos)))
         (or (not rest-pos)
             (> rest-pos (1+ key-pos))))))

(defun inlineable-method-lambda-p (lambda-expression environment)
  (and
   ;; We cannot inline a potential closure.
   (null-lexical-environement-p environment)
   ;; We can only inline forms starting with LAMBDA
   (consp lambda-expression)
   (eq (first lambda-expression) 'lambda)
   ;; If the body contains crazy things like LOAD-TIME-VALUE or non-trivial
   ;; calls to CALL-NEXT-METHOD, we give up.
   (inlineable-form-p (sb-cltl2:macroexpand-all lambda-expression))))

;;; This following code is essentially a very poor code walker.

(defun inlineable-form-p (form)
  (cond
    ((atom form) t)
    ((not (symbolp (first form)))
     (every #'inlineable-form-p form))
    (t
     (case (first form)
       ;; References to call-next-method.
       ((call-next-method next-method-p)
        nil)
       ((function)
        (if (member (second form) '(call-next-method next-method-p))
            nil
            t))
       ;; Binding forms.
       ((lambda)
        (and (second form)
             (inlineable-lambda-list-p (second form))
             (inlineable-body-p (rest (rest form)))))
       ((let let*)
        (and (every
              (lambda (binding)
                (or (symbolp binding)
                    (and (typep binding '(cons symbol (cons t null)))
                         (inlineable-form-p (second binding)))))
              (second form))
             (inlineable-body-p (rest (rest form)))))
       ((flet labels)
        (and (every
              (lambda (binding)
                (inlineable-form-p `(lambda ,@(rest binding))))
              (second form))
             (inlineable-body-p (rest (rest form)))))
       ;; Other special forms.
       ((quote) t)
       ((load-time-value) nil)
       ((progn tagbody multiple-value-call go multiple-value-prog1 throw progv unwind-protect)
        (every #'inlineable-form-p (rest form)))
       ((setq eval-when return-from catch block the)
        (every #'inlineable-form-p (rest (rest form))))
       ((locally)
        (inlineable-body-p (rest form)))
       ((if)
        (and (inlineable-form-p (second form))
             (inlineable-form-p (third form))
             (inlineable-form-p (fourth form))))
       ;; Function calls
       (otherwise
        (if (special-operator-p (first form))
            nil
            (every #'inlineable-form-p (rest form))))))))

(defun inlineable-lambda-list-p (lambda-list)
  (flet ((inlineable-lambda-list-item-p (item)
           (or (symbolp item)
               (and (consp item)
                    (< 0 (length item) 4)
                    (inlineable-form-p (second item))))))
    (every #'inlineable-lambda-list-item-p lambda-list)))

(defun parse-body (body)
  (let ((documentation nil)
        (declarations '())
        (forms '()))
    (labels ((process-body (body)
               (let ((item (first body)))
                 (cond ((stringp item)
                        (if (null documentation)
                            (setf documentation item)
                            (error "Duplicate documentation string ~S." item))
                        (process-body (rest body)))
                       ((and (consp item)
                             (eq (first item) 'declare))
                        (push item declarations)
                        (process-body (rest body)))
                       (t
                        (values body (nreverse declarations) documentation))))))
      (process-body body))))

(defun inlineable-body-p (forms)
  (every #'inlineable-form-p (parse-body forms)))
