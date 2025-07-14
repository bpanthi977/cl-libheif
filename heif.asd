;;;; cl-libheif.asd

(asdf:defsystem #:heif
  :description "Binding to the libheif library"
  :author "Bibek Panthi"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi-libffi)
  :components ((:file "package")
	       (:file "library")
	       (:file "bindings")
               (:file "heif")))
