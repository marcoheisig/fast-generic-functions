(cl:in-package #:cl-user)

(defpackage #:sealable-metaobjects
  (:use #:closer-common-lisp)
  (:export
   #:metaobject-sealable-p
   #:class-sealable-p
   #:generic-function-sealable-p
   #:method-sealable-p

   #:metaobject-sealed-p
   #:class-sealed-p
   #:generic-function-sealed-p
   #:method-sealed-p

   #:seal-class
   #:seal-generic-function
   #:seal-method

   #:generic-function-specializer-profile
   #:specializer-type
   #:specializer-prototype
   #:specializer-direct-superspecializers

   #:define-sealable-generic-function

   #:sealable-class
   #:sealable-class-instance
   #:sealable-generic-function
   #:inlineable-generic-function
   #:inlineable-standard-generic-function
   #:sealable-method
   #:sealable-standard-class
   #:sealable-standard-class-instance
   #:sealable-standard-generic-function
   #:sealable-standard-method))

