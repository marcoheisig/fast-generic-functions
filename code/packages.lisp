(cl:in-package #:cl-user)

(defpackage #:fast-generic-functions
  (:use
   #:closer-common-lisp)

  (:import-from
   #:sealable-metaobjects
   #:method-properties
   #:validate-method-property
   #:seal-domain
   #:sealed-domains
   #:compute-static-call-signatures
   #:static-call-signature
   #:static-call-signature-types
   #:static-call-signature-prototypes
   #:externalizable-object-p
   #:sealable-class
   #:sealable-generic-function
   #:sealable-standard-generic-function
   #:potentially-sealable-method
   #:potentially-sealable-standard-method)

  (:export
   #:method-properties
   #:seal-domain
   #:validate-method-property
   #:fast-generic-function
   #:fast-method
   #:inlineable
   #:no-primary-method))
