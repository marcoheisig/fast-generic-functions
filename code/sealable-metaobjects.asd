(defsystem "sealable-metaobjects"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :description "A CLOSsy way to trade genericity for performance."
  :license "MIT"
  :depends-on ("closer-mop" "trivial-macroexpand-all")

  :in-order-to ((test-op (load-op "sealable-metaobjects-test-suite")))

  :serial t
  :components
  ((:file "packages")
   (:file "debug")
   (:file "utilities")
   (:file "lambda-lists")
   (:file "generic-functions")
   (:file "built-in-class")
   (:file "null-lexical-environment-p")
   (:file "inlineable-method-lambda-p")
   (:file "specializer-prototype")
   (:file "sealable-metaobject-mixin")
   (:file "sealable-class")
   (:file "potentially-sealable-method")
   (:file "sealable-generic-function")
   (:file "sealable-standard-class")
   (:file "potentially-sealable-standard-method")
   (:file "sealable-standard-generic-function")
   (:file "compute-static-call-signatures")
   (:module "implementation-specific"
    :components
    ((:file "sbcl" :if-feature :sbcl)
     (:file "default" :if-feature (:not (:or :sbcl)))))))
