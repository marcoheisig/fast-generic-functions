(in-package #:sealable-metaobjects)

(defvar *debug* nil)

(defmacro debug-format (format-string &rest arguments)
  `(when *debug*
     (format *trace-output* ,format-string ,@arguments)))
