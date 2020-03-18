(defsystem "sealable-metaobjects"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :description "A CLOSsy way to trade genericity for performance."
  :license "MIT"
  :depends-on ("closer-mop" "trivial-macroexpand-all")

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "generic-functions")

   ;; Sealable Metaobjects.
   (:file "sealable-metaobject-mixin")
   (:file "sealable-class")
   (:file "potentially-sealable-method")
   (:file "sealable-generic-function")

   ;; Analysis.
   (:file "specializer-prototype")
   (:file "static-call-signature")))
