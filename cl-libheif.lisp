;;;; cl-libheif.lisp

(in-package #:cl-libheif)

(defun version ()
  (values (b:heif-get-version-number-major)
	  (b:heif-get-version-number-minor)
	  (b:heif-get-version-number-maintenance)))

(defun read-image (path)
  (let (ctx
	(handle* (cffi:foreign-alloc :pointer))
	handle
	(img* (cffi:foreign-alloc :pointer))
	img
	image)

    (unwind-protect
	 (progn
	   ;; Create context
	   (setf ctx (b:heif-context-alloc))
	   (b:check-ret (b:heif-context-read-from-file ctx (namestring (truename path)) (cffi:null-pointer)))
	   ;; Get primary image handle
	   (b:check-ret (b:heif-context-get-primary-image-handle ctx handle*))
	   (setf handle (cffi:mem-ref handle* :pointer 0))
	   ;; Decode image
	   (b:check-ret (b:heif-decode-image handle img* :rgb :interleaved-rgb (cffi:null-pointer)))
	   (setf img (cffi:mem-ref img* :pointer 0))
	   ;; Read image data into an array
	   (let ((width (b:heif-image-get-width img :interleaved))
		 (height (b:heif-image-get-height img :interleaved)))
	     (setf image (make-array (list height width 3) :element-type '(unsigned-byte 8)))
	     (cffi:with-foreign-object (stride* :int)
	       (let ((data (b:heif-image-get-plane-readonly img :interleaved stride*))
		     (stride (cffi:mem-ref stride* :int)))
		 (loop for y from 0 below height do
		       (loop for x from 0 below width
			     for offset = (+ (* y stride) (* x 3)) do
			       (setf (aref image  y x 0) (cffi:mem-ref data :uint8 (+ offset 0)))
			       (setf (aref image  y x 1) (cffi:mem-ref data :uint8 (+ offset 1)))
			       (setf (aref image  y x 2) (cffi:mem-ref data :uint8 (+ offset 2)))))
		 image))))

      (when ctx
	(b:heif-context-free ctx))
      (when handle
	(b:heif-image-handle-release handle))
      (when img
	(b:heif-image-release img)))))

(defun register-opticl-handler ()
  (let ((opticl-package (find-package "OPTICL")))
    (unless opticl-package
      (error "opticl package is not loaded."))
    (let ((table (find-symbol "*IMAGE-FILE-READER-HASH-TABLE*" opticl-package)))
      (loop for ext in '(:heif :heic :avif :hif :heics) do
	(setf (gethash ext (symbol-value table)) #'read-image)))))
