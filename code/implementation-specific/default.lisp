(in-package #:sealable-metaobjects)

(defmethod make-inlineable ((gf generic-function) specializers)
  (warn "~@<This implementation has no implementation of MAKE-INLINEABLE (yet). ~
            That means that generic function sealing will not speed up any of ~
            your code.  Sorry about that.~:@>"))
