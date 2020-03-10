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
   (:file "specializer-prototype")

   ;; Sealable Metaobjects.
   (:file "sealable-metaobject-mixin")
   (:file "sealable-class")
   (:file "potentially-sealable-method")
   (:file "sealable-generic-function")
   (:file "static-call-signature")

   ;; Fast Generic Functions.
   (:module "fast-generic-function"
    :components
    ((:file "generic-functions")
     (:file "debug")
     (:file "fast-generic-function")
     (:file "expand-effective-method")
     (:file "optimize-function-call")
     ;(:file "caching")
     (:file "default" :if-feature (:not (:or :sbcl :ccl)))
     (:file "sbcl" :if-feature :sbcl)
     (:file "ccl" :if-feature :ccl)))))
