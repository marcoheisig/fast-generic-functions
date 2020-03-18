(cl:in-package #:cl-user)

(defpackage #:sealable-metaobjects
  (:use #:closer-common-lisp)
  (:export

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
   #:seal-domain
   #:seal-specializer

   #:specializer-type
   #:specializer-prototype
   #:specializer-direct-superspecializers
   #:specializer-intersectionp
   #:specializer-subtypep
   #:domain-intersectionp
   #:domain-subtypep

   #:method-properties
   #:validate-method-property

   #:static-call-signature
   #:static-call-signature-types
   #:static-call-signature-prototypes

   #:sealed-domains
   #:compute-static-call-signatures
   #:externalizable-object-p
   #:sealable-class
   #:sealable-generic-function
   #:sealable-standard-generic-function
   #:potentially-sealable-method
   #:potentially-sealable-standard-method))

