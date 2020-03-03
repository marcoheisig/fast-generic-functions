(in-package #:sealable-metaobjects)

(defparameter *debug* nil)

(defmacro debug-format (format-string &rest arguments)
  `(when *debug*
     (format *trace-output* ,format-string ,@arguments)))
