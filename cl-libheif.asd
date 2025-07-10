;;;; cl-libheif.asd

(asdf:defsystem #:cl-libheif
  :description "Binding to the libheif library"
  :author "Bibek Panthi"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:cffi-libffi)
  :components ((:file "package")
	       (:file "library")
	       (:file "bindings")
               (:file "cl-libheif")))
