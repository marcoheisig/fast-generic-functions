(in-package #:sealable-metaobjects)

(defun null-lexical-environement-p (environment)
  (declare (ignorable environment))
  nil
  #+sbcl(sb-c::null-lexenv-p environment))
