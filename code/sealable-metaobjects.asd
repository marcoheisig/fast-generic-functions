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
   (:file "defgeneric")
   (:file "lambda-lists")
   (:file "generic-functions")
   (:file "specializer-prototype")

   ;; Sealable Metaobjects.
   (:file "sealable-metaobject-mixin")
   (:file "sealable-class")
   (:file "potentially-sealable-method")
   (:file "sealable-generic-function")
   (:file "compute-static-call-signatures")
   (:file "inline-lambda")

   ;; The Magic.
   (:module "sbcl" :if-feature :sbcl
    :components
    ((:file "system-class")
     (:file "inlining")))
   (:module "default" :if-feature (:not (:or :sbcl))
    :components
    ())))
