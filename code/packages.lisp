(cl:in-package #:cl-user)

(defpackage #:sealable-metaobjects
  (:use #:closer-common-lisp)
  (:export
   #:*seal-methods-eagerly*
   #:*seal-classes-eagerly*
   #:*seal-generic-functions-eagerly*
   #:define-sealable-generic-function
   #:generic-function-specializer-profile

   #:metaobject-sealable-p
   #:class-sealable-p
   #:generic-function-sealable-p
   #:method-sealable-p

   #:metaobject-sealed-p
   #:class-sealed-p
   #:generic-function-sealed-p
   #:method-sealed-p

   #:seal-metaobject
   #:seal-class
   #:seal-generic-function
   #:seal-method

   #:sealable-class
   #:sealable-class-instance
   #:sealable-generic-function
   #:sealable-method
   #:sealable-standard-class
   #:sealable-standard-class-instance
   #:sealable-standard-generic-function
   #:sealable-standard-method))

