(in-package #:heif)

(cffi:define-foreign-library libheif
  (:darwin (:default "libheif"))
  (:unix (:or "libheif.so"))
  (t (:default "libheif")))

(cffi:load-foreign-library 'libheif)
