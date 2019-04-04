(defsystem "sealable-metaobjects-test-suite"
  :depends-on ("closer-mop" "sealable-metaobjects")

  :serial t
  :components
  ((:file "packages")
   (:file "classes")
   (:file "generic-functions")
   (:file "methods")
   (:file "functions")))
