(defsystem "sealable-metaobjects"
  :depends-on ("closer-mop" "alexandria")

  :serial t
  :components
  ((:file "packages")
   (:file "null-lexical-environment-p")
   (:file "inlineable-method-lambda-p")
   (:file "expand-effective-method")
   (:file "generic-functions")

   (:file "sealable-metaobject-mixin")
   (:file "sealable-class")
   (:file "potentially-sealable-method")
   (:file "sealable-generic-function")
   (:file "sealable-standard-class")
   (:file "potentially-sealable-standard-method")
   (:file "sealable-standard-generic-function")

   (:module "implementation-specific"
    :components
    ((:file "sbcl" :if-feature :sbcl)
     (:file "default" :if-feature (:not (:or :sbcl)))))))
