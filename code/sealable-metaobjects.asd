(defsystem "sealable-metaobjects"
  :depends-on ("closer-mop" "alexandria")

  :serial t
  :components
  ((:file "packages")
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
    ((:file "packages")
     (:file "sbcl" :if-feature :sbcl)
     (:file "default" :if-feature (:not (:or :sbcl)))))))
