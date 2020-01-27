(in-package #:sealable-metaobjects)

(defun null-lexical-environement-p (environment)
  (declare (ignorable environment))
  (or (null environment)
      #+sbcl (sb-c::null-lexenv-p environment)))
