(defsystem "fast-generic-functions-test-suite"
  :depends-on ("closer-mop" "fast-generic-functions")

  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "classes")
   (:file "methods")
   (:file "functions")))
