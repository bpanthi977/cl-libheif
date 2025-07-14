;;;; package.lisp

(defpackage :heif/ffi
  (:use :cffi))

(defpackage #:heif
  (:use #:cl)
  (:local-nicknames (#:ffi #:heif/ffi))
  (:export #:read-image
	   #:register-opticl-handler))
