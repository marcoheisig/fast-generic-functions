(defsystem "sealable-metaobjects"
  :depends-on ("closer-mop")

  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "sealable-metaobject-mixin")
   (:file "sealable-class")
   (:file "sealable-method")
   (:file "sealable-generic-function")
   (:file "sealable-standard-class")
   (:file "sealable-standard-method")
   (:file "sealable-standard-generic-function")
   (:module "implementation-dependent"
    :components
    ((:file "sbcl" :if-feature :sbcl)))))
