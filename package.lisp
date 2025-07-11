;;;; package.lisp

(defpackage :cl-libheif/bindings
  (:use :cl :cffi))

(defpackage #:cl-libheif
  (:use #:cl)
  (:local-nicknames (#:b #:cl-libheif/bindings))
  (:export #:read-image
	   #:register-opticl-handler))
