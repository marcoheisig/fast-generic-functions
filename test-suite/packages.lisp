(cl:in-package #:cl-user)

(defpackage #:sealable-metaobjects-test-suite
  (:use #:closer-common-lisp #:sealable-metaobjects)
  (:shadowing-import-from #:sealable-metaobjects #:defgeneric))
