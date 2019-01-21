(cl:in-package #:cl-user)

(defpackage #:sealable-metaobjects
  (:use #:closer-common-lisp)
  (:export
   #:class-sealable-p
   #:class-sealed-p
   #:seal-class
   #:generic-function-sealable-p
   #:generic-function-sealed-p
   #:seal-generic-function
   #:method-sealable-p
   #:method-sealed-p
   #:seal-method
   #:sealable-class
   #:sealable-class-instance
   #:sealable-generic-function
   #:sealable-method
   #:sealable-standard-class
   #:sealable-standard-class-instance
   #:sealable-standard-generic-function
   #:sealable-standard-method))

