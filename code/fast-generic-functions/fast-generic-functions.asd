(defsystem "fast-generic-functions"
  :author "Marco Heisig <marco.heisig@fau.de>"
  :description "Seal your generic functions for an extra boost in performance."
  :license "MIT"
  :depends-on
  ("closer-mop"
   "trivial-macroexpand-all"
   "sealable-metaobjects")

  :in-order-to ((test-op (load-op "fast-generic-functions-test-suite")))

  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "lambda-lists")
   (:file "generic-functions")
   (:file "fast-method")
   (:file "fast-generic-function")
   (:file "expand-effective-method-body")
   (:file "optimize-function-call")
   (:file "default" :if-feature (:not (:or :sbcl :ccl)))
   (:file "sbcl" :if-feature :sbcl)
   (:file "ccl" :if-feature :ccl)))
