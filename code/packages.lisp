(cl:in-package #:cl-user)

(defpackage #:sealable-metaobjects
  (:use #:closer-common-lisp)
  (:shadow #:defgeneric)
  (:export
   #:defgeneric

   #:metaobject-sealable-p
   #:class-sealable-p
   #:generic-function-sealable-p
   #:method-sealable-p
   #:specializer-sealable-p

   #:metaobject-sealed-p
   #:class-sealed-p
   #:generic-function-sealed-p
   #:method-sealed-p
   #:specializer-sealed-p

   #:seal-class
   #:seal-generic-function
   #:seal-method
   #:specializer-specializer

   #:generic-function-specializer-profile
   #:specializer-type
   #:specializer-prototype
   #:specializer-direct-superspecializers

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

