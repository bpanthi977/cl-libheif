(in-package :heif/ffi)

(cffi:defctype size-t :ulong)

;;; heif_error.h

;;; Enums
(cffi:defcenum error-code
  (:ok 0)
  (:input-does-not-exist 1)
  (:invalid-input 2)
  (:unsupported-filetype 3)
  (:unsupported-feature 4)
  (:usage-error 5)
  (:memory-allocation-error 6)
  (:decoder-plugin-error 7)
  (:encoder-plugin-error 8)
  (:encoding-error 9)
  (:color-profile-does-not-exist 10)
  (:plugin-loading-error 11)
  (:canceled 12)
  (:end-of-sequence 13))

(cffi:defcenum suberror-code
  (:unspecified 0)
  (:end-of-data 100)
  (:invalid-box-size 101)
  (:no-ftyp-box 102)
  (:no-idat-box 103)
  (:no-meta-box 104)
  (:no-hdlr-box 105)
  (:no-hvcc-box 106)
  (:no-pitm-box 107)
  (:no-ipco-box 108)
  (:no-ipma-box 109)
  (:no-iloc-box 110)
  (:no-iinf-box 111)
  (:no-iprp-box 112)
  (:no-iref-box 113)
  (:no-pict-handler 114)
  (:ipma-references-nonexisting-property 115)
  (:no-properties-assigned-to-item 116)
  (:no-item-data 117)
  (:invalid-grid-data 118)
  (:missing-grid-images 119)
  (:invalid-clean-aperture 120)
  (:invalid-overlay-data 121)
  (:overlay-outside-canvas 122)
  (:auxiliary-image-type-unspecified 123)
  (:no-or-invalid-primary-item 124)
  (:no-infe-box 125)
  (:unknown-color-profile-type 126)
  (:wrong-tile-chroma-format 127)
  (:invalid-fractional-number 128)
  (:invalid-image-size 129)
  (:invalid-pixi-box 130)
  (:no-av1c-box 131)
  (:wrong-tile-pixel-depth 132)
  (:unknown-nclx-color-primaries 133)
  (:unknown-nclx-transfer-characteristics 134)
  (:unknown-nclx-matrix-coefficients 135)
  (:invalid-region-data 136)
  (:no-ispe-property 137)
  (:camera-intrinsic-matrix-undefined 138)
  (:camera-extrinsic-matrix-undefined 139)
  (:invalid-j2k-codestream 140)
  (:no-vvcc-box 141)
  (:no-icbr-box 142)
  (:no-avcc-box 143)
  (:invalid-mini-box 149)
  (:decompression-invalid-data 150)
  (:no-moov-box 151)

  ;; Memory allocation
  (:security-limit-exceeded 1000)
  (:compression-initialisation-error 1001)

  ;; Usage
  (:nonexisting-item-referenced 2000)
  (:null-pointer-argument 2001)
  (:nonexisting-image-channel-referenced 2002)
  (:unsupported-plugin-version 2003)
  (:unsupported-writer-version 2004)
  (:unsupported-parameter 2005)
  (:invalid-parameter-value 2006)
  (:invalid-property 2007)
  (:item-reference-cycle 2008)

  ;; Unsupported feature
  (:unsupported-codec 3000)
  (:unsupported-image-type 3001)
  (:unsupported-data-version 3002)
  (:unsupported-color-conversion 3003)
  (:unsupported-item-construction-method 3004)
  (:unsupported-header-compression-method 3005)
  (:unsupported-generic-compression-method 3006)
  (:unsupported-essential-property 3007)

  ;; Encoder plugin
  (:unsupported-bit-depth 4000)

  ;; Encoding error
  (:cannot-write-output-data 5000)
  (:encoder-initialization 5001)
  (:encoder-encoding 5002)
  (:encoder-cleanup 5003)
  (:too-many-regions 5004)

  ;; Plugin loading
  (:plugin-loading-error 6000)
  (:plugin-is-not-loaded 6001)
  (:cannot-read-plugin-directory 6002)
  (:no-matching-decoder-installed 6003))

;;; Struct
(cffi:defcstruct error
  (code error-code)
  (subcode suberror-code)
  (message :string))

;;; Global success value (extern const)
(cffi:defcvar ("heif_error_success" error-success)
    (:pointer (:struct error)))

(cl:defun check-ret (err)
  (cl:assert (cl:eql (cl:getf err 'code) :ok)))
(cl:export 'check-ret)



;;; library.h

(cffi:defcstruct init-params
  (version :int))

;; ------------------------------------------------------------
;; Version Functions

(cffi:defcfun ("heif_get_version" get-version) :string)

(cffi:defcfun ("heif_get_version_number" get-version-number) :uint32)

(cffi:defcfun ("heif_get_version_number_major" get-version-number-major) :int)

(cffi:defcfun ("heif_get_version_number_minor" get-version-number-minor) :int)

(cffi:defcfun ("heif_get_version_number_maintenance" get-version-number-maintenance) :int)

;; ------------------------------------------------------------
;; Init / Deinit Functions

(cffi:defcfun ("heif_init" init) :void
  (params :pointer))  ;; can be NULL

(cffi:defcfun ("heif_deinit" deinit) :void)

;; ------------------------------------------------------------
;; String utilities

(cffi:defcfun ("heif_string_release" string-release) :void
  (str :string))

;;;;;;;;;;;;;; heif_image.h

(cffi:defcenum chroma
  (:undefined 99)
  (:monochrome 0)
  (:420 1)
  (:422 2)
  (:444 3)
  (:interleaved-rgb 10)
  (:interleaved-rgba 11)
  (:interleaved-rrggbb-be 12)
  (:interleaved-rrggbbaa-be 13)
  (:interleaved-rrggbb-le 14)
  (:interleaved-rrggbbaa-le 15))

(cffi:defcenum colorspace
  (:undefined 99)
  (:ycbcr 0)
  (:rgb 1)
  (:monochrome 2)
  (:nonvisual 3))

(cffi:defcenum channel
  (:y 0)
  (:cb 1)
  (:cr 2)
  (:r 3)
  (:g 4)
  (:b 5)
  (:alpha 6)
  (:interleaved 10)
  (:filter-array 11)
  (:depth 12)
  (:disparity 13))


(cffi:defctype image :pointer)
(cffi:defctype image-handle :pointer)
(cffi:defctype security-limits :pointer)
(cffi:defctype scaling-options :pointer)

;; Image info
(cffi:defcfun ("heif_image_get_colorspace" image-get-colorspace) colorspace
  (image image))

(cffi:defcfun ("heif_image_get_chroma_format" image-get-chroma-format) chroma
  (image image))

(cffi:defcfun ("heif_image_get_width" image-get-width) :int
  (image image) (channel channel))

(cffi:defcfun ("heif_image_get_height" image-get-height) :int
  (image image) (channel channel))

(cffi:defcfun ("heif_image_get_primary_width" image-get-primary-width) :int
  (image image))

(cffi:defcfun ("heif_image_get_primary_height" image-get-primary-height) :int
  (image image))

(cffi:defcfun ("heif_image_get_bits_per_pixel" image-get-bpp) :int
  (image image) (channel channel))

(cffi:defcfun ("heif_image_get_bits_per_pixel_range" image-get-bpp-range) :int
  (image image) (channel channel))

(cffi:defcfun ("heif_image_has_channel" image-has-channel) :int
  (image image) (channel channel))

;; Plane Access
(cffi:defcfun ("heif_image_get_plane_readonly" image-get-plane-readonly) :pointer
  (image image) (channel channel) (out-stride :pointer))

(cffi:defcfun ("heif_image_get_plane" image-get-plane) :pointer
  (image image) (channel channel) (out-stride :pointer))

(cffi:defcfun ("heif_image_get_plane_readonly2" image-get-plane-readonly2) :pointer
  (image image) (channel channel) (out-stride (:pointer :size)))

(cffi:defcfun ("heif_image_get_plane2" image-get-plane2) :pointer
  (image image) (channel channel) (out-stride (:pointer :size)))

;; Image management
(cffi:defcfun ("heif_image_crop" image-crop) :void
  (image image) (left :int) (right :int) (top :int) (bottom :int))

(cffi:defcfun ("heif_image_extract_area" image-extract-area) :void
  (src image) (x0 :uint32) (y0 :uint32) (w :uint32) (h :uint32)
  (limits security-limits) (out-image (:pointer image)))

(cffi:defcfun ("heif_image_scale_image" image-scale) :void
  (input image) (output (:pointer image)) (w :int) (h :int)
  (options scaling-options))

(cffi:defcfun ("heif_image_extend_to_size_fill_with_zero" image-extend-zero) :void
  (img image) (w :uint32) (h :uint32))

(cffi:defcfun ("heif_image_get_decoding_warnings" image-get-warnings) :int
  (img image) (start-idx :int)
  (out-warnings :pointer) (max :int))

(cffi:defcfun ("heif_image_add_decoding_warning" image-add-warning) :void
  (img image) (err :pointer))

(cffi:defcfun ("heif_image_release" image-release) :void
  (img image))

(cffi:defcfun ("heif_image_get_pixel_aspect_ratio" image-get-aspect) :void
  (img image) (h (:pointer :uint32)) (v (:pointer :uint32)))

(cffi:defcfun ("heif_image_set_pixel_aspect_ratio" image-set-aspect) :void
  (img image) (h :uint32) (v :uint32))

(cffi:defcfun ("heif_image_create" image-create) :void
  (w :int) (h :int) (colorspace colorspace) (chroma chroma)
  (out (:pointer image)))

(cffi:defcfun ("heif_image_add_plane" image-add-plane) :void
  (img image) (channel channel) (w :int) (h :int) (bpp :int))

(cffi:defcfun ("heif_image_add_plane_safe" image-add-plane-safe) :void
  (img image) (channel channel) (w :int) (h :int) (bpp :int)
  (limits security-limits))

(cffi:defcfun ("heif_image_set_premultiplied_alpha" image-set-premul-alpha) :void
  (img image) (flag :int))

(cffi:defcfun ("heif_image_is_premultiplied_alpha" image-is-premul-alpha) :int
  (img image))

(cffi:defcfun ("heif_image_extend_padding_to_size" image-extend-padding) :void
  (img image) (min-w :int) (min-h :int))


;;;;;;;;;;;; heif_color.h

;;; Enums
(cffi:defcenum chroma-downsampling-algorithm
  (:nearest-neighbor 1)
  (:average 2)
  (:sharp-yuv 3))

(cffi:defcenum chroma-upsampling-algorithm
  (:nearest-neighbor 1)
  (:bilinear 2))

(cffi:defcenum alpha-composition-mode
  (:none 0)
  (:solid-color 1)
  (:checkerboard 2))

(cffi:defcenum color-profile-type
  (:not-present 0)
  (:nclx #x6E636C78) ; 'nclx'
  (:ricc #x72494343) ; 'rICC'
  (:prof #x70726F66)) ; 'prof'

(cffi:defcenum color-primaries
  (:bt-709-5 1)
  (:unspecified 2)
  (:bt-470m 4)
  (:bt-470bg 5)
  (:bt-601-6 6)
  (:smpte-240m 7)
  (:generic-film 8)
  (:bt-2020 9)
  (:smpte-st-428-1 10)
  (:smpte-rp-431-2 11)
  (:smpte-eg-432-1 12)
  (:ebu-tech-3213-e 22))

(cffi:defcenum transfer-characteristics
  (:bt-709-5 1)
  (:unspecified 2)
  (:bt-470m 4)
  (:bt-470bg 5)
  (:bt-601-6 6)
  (:smpte-240m 7)
  (:linear 8)
  (:log-100 9)
  (:log-100-sqrt10 10)
  (:iec-61966-2-4 11)
  (:bt-1361 12)
  (:iec-61966-2-1 13)
  (:bt-2020-10bit 14)
  (:bt-2020-12bit 15)
  (:pq 16)
  (:smpte-st-428-1 17)
  (:hlg 18))

(cffi:defcenum matrix-coefficients
  (:rgb-gbr 0)
  (:bt-709-5 1)
  (:unspecified 2)
  (:us-fcc-t47 4)
  (:bt-470bg 5)
  (:bt-601-6 6)
  (:smpte-240m 7)
  (:ycgco 8)
  (:bt-2020-ncl 9)
  (:bt-2020-cl 10)
  (:smpte-st-2085 11)
  (:cd-ncl 12)
  (:cd-cl 13)
  (:ictcp 14))

;;; Structs
(cffi:defcstruct color-conversion-options
  (version :uint8)
  (preferred-chroma-downsampling-algorithm chroma-downsampling-algorithm)
  (preferred-chroma-upsampling-algorithm chroma-upsampling-algorithm)
  (only-use-preferred-chroma-algorithm :uint8))

(cffi:defcstruct color-conversion-options-ext
  (version :uint8)
  (alpha-composition-mode alpha-composition-mode)
  (background-red :uint16)
  (background-green :uint16)
  (background-blue :uint16)
  (secondary-background-red :uint16)
  (secondary-background-green :uint16)
  (secondary-background-blue :uint16)
  (checkerboard-square-size :uint16))

(cffi:defcstruct color-profile-nclx
  (version :uint8)
  (color-primaries color-primaries)
  (transfer-characteristics transfer-characteristics)
  (matrix-coefficients matrix-coefficients)
  (full-range-flag :uint8)
  ;; decoded values
  (color-primary-red-x :float)
  (color-primary-red-y :float)
  (color-primary-green-x :float)
  (color-primary-green-y :float)
  (color-primary-blue-x :float)
  (color-primary-blue-y :float)
  (color-primary-white-x :float)
  (color-primary-white-y :float))

(cffi:defcstruct content-light-level
  (max-content-light-level :uint16)
  (max-pic-average-light-level :uint16))

(cffi:defcstruct mastering-display-colour-volume
  (display-primaries-x (:array :uint16 3))
  (display-primaries-y (:array :uint16 3))
  (white-point-x :uint16)
  (white-point-y :uint16)
  (max-display-mastering-luminance :uint32)
  (min-display-mastering-luminance :uint32))

(cffi:defcstruct decoded-mastering-display-colour-volume
  (display-primaries-x (:array :float 3))
  (display-primaries-y (:array :float 3))
  (white-point-x :float)
  (white-point-y :float)
  (max-display-mastering-luminance :double)
  (min-display-mastering-luminance :double))

(cffi:defcstruct ambient-viewing-environment
  (ambient-illumination :uint32)
  (ambient-light-x :uint16)
  (ambient-light-y :uint16))

;;; Function declarations

(cffi:defcfun ("heif_color_conversion_options_set_defaults" color-conversion-options-set-defaults)
  :void (opts :pointer))

(cffi:defcfun ("heif_color_conversion_options_ext_alloc" color-conversion-options-ext-alloc)
  :pointer)

(cffi:defcfun ("heif_color_conversion_options_ext_copy" color-conversion-options-ext-copy)
  :void (dst :pointer) (src :pointer))

(cffi:defcfun ("heif_color_conversion_options_ext_free" color-conversion-options-ext-free)
  :void (ptr :pointer))

(cffi:defcfun ("heif_image_handle_get_color_profile_type" image-handle-get-color-profile-type)
  color-profile-type (handle :pointer))

(cffi:defcfun ("heif_image_handle_get_raw_color_profile_size" image-handle-get-raw-color-profile-size)
  size-t (handle :pointer))

(cffi:defcfun ("heif_image_handle_get_raw_color_profile" image-handle-get-raw-color-profile)
  :pointer (handle :pointer) (out-data :pointer))

(cffi:defcfun ("heif_nclx_color_profile_set_color_primaries" nclx-color-profile-set-color-primaries)
  :pointer (nclx :pointer) (cp :uint16))

(cffi:defcfun ("heif_nclx_color_profile_set_transfer_characteristics" nclx-color-profile-set-transfer-characteristics)
  :pointer (nclx :pointer) (tc :uint16))

(cffi:defcfun ("heif_nclx_color_profile_set_matrix_coefficients" nclx-color-profile-set-matrix-coefficients)
  :pointer (nclx :pointer) (mc :uint16))

(cffi:defcfun ("heif_image_handle_get_nclx_color_profile" image-handle-get-nclx-color-profile)
  :pointer (handle :pointer) (out-data :pointer))

(cffi:defcfun ("heif_nclx_color_profile_alloc" nclx-color-profile-alloc)
  :pointer)

(cffi:defcfun ("heif_nclx_color_profile_free" nclx-color-profile-free)
  :void (profile :pointer))

(cffi:defcfun ("heif_image_get_color_profile_type" image-get-color-profile-type)
  color-profile-type (image :pointer))

(cffi:defcfun ("heif_image_get_raw_color_profile_size" image-get-raw-color-profile-size)
  size-t (image :pointer))

(cffi:defcfun ("heif_image_get_raw_color_profile" image-get-raw-color-profile)
  :pointer (image :pointer) (out-data :pointer))

(cffi:defcfun ("heif_image_get_nclx_color_profile" image-get-nclx-color-profile)
  :pointer (image :pointer) (out-data :pointer))

(cffi:defcfun ("heif_image_set_raw_color_profile" image-set-raw-color-profile)
  :pointer (image :pointer)
	   (fourcc :string)
	   (data :pointer)
	   (size size-t))

(cffi:defcfun ("heif_image_set_nclx_color_profile" image-set-nclx-color-profile)
  :pointer (image :pointer) (profile :pointer))

(cffi:defcfun ("heif_image_has_content_light_level" image-has-content-light-level)
  :int (image :pointer))

(cffi:defcfun ("heif_image_get_content_light_level" image-get-content-light-level)
  :void (image :pointer) (out :pointer))

(cffi:defcfun ("heif_image_handle_get_content_light_level" image-handle-get-content-light-level)
  :int (handle :pointer) (out :pointer))

(cffi:defcfun ("heif_image_set_content_light_level" image-set-content-light-level)
  :void (image :pointer) (in :pointer))

(cffi:defcfun ("heif_image_has_mastering_display_colour_volume" image-has-mastering-display-colour-volume)
  :int (image :pointer))

(cffi:defcfun ("heif_image_get_mastering_display_colour_volume" image-get-mastering-display-colour-volume)
  :void (image :pointer) (out :pointer))

(cffi:defcfun ("heif_image_handle_get_mastering_display_colour_volume" image-handle-get-mastering-display-colour-volume)
  :int (handle :pointer) (out :pointer))

(cffi:defcfun ("heif_image_set_mastering_display_colour_volume" image-set-mastering-display-colour-volume)
  :void (image :pointer) (in :pointer))

(cffi:defcfun ("heif_mastering_display_colour_volume_decode" mastering-display-colour-volume-decode)
  :pointer (in :pointer) (out :pointer))


;;;;;;; heif_brands.h

(cffi:defctype brand2 :uint32)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defun fourcc (a b c d)
    "Convert four ASCII characters into a 32_bit unsigned int (heif_brand2)."
    (cl:logior (cl:ash (cl:char-code a) 24)
	       (cl:ash (cl:char-code b) 16)
	       (cl:ash (cl:char-code c) 8)
	       (cl:char-code d))))

(cl:defconstant +brand2-heic+ (fourcc #\h #\e #\i #\c))
(cl:defconstant +brand2-heix+ (fourcc #\h #\e #\i #\x))
(cl:defconstant +brand2-hevc+ (fourcc #\h #\e #\v #\c))
(cl:defconstant +brand2-hevx+ (fourcc #\h #\e #\v #\x))
(cl:defconstant +brand2-heim+ (fourcc #\h #\e #\i #\m))
(cl:defconstant +brand2-heis+ (fourcc #\h #\e #\i #\s))
(cl:defconstant +brand2-hevm+ (fourcc #\h #\e #\v #\m))
(cl:defconstant +brand2-hevs+ (fourcc #\h #\e #\v #\s))
(cl:defconstant +brand2-avif+ (fourcc #\a #\v #\i #\f))
(cl:defconstant +brand2-avis+ (fourcc #\a #\v #\i #\s))
(cl:defconstant +brand2-mif1+ (fourcc #\m #\i #\f #\1))
(cl:defconstant +brand2-mif2+ (fourcc #\m #\i #\f #\2))
(cl:defconstant +brand2-mif3+ (fourcc #\m #\i #\f #\3))
(cl:defconstant +brand2-msf1+ (fourcc #\m #\s #\f #\1))
(cl:defconstant +brand2-vvic+ (fourcc #\v #\v #\i #\c))
(cl:defconstant +brand2-vvis+ (fourcc #\v #\v #\i #\s))
(cl:defconstant +brand2-evbi+ (fourcc #\e #\v #\b #\i))
(cl:defconstant +brand2-evmi+ (fourcc #\e #\v #\m #\i))
(cl:defconstant +brand2-evbs+ (fourcc #\e #\v #\b #\s))
(cl:defconstant +brand2-evms+ (fourcc #\e #\v #\m #\s))
(cl:defconstant +brand2-jpeg+ (fourcc #\j #\p #\e #\g))
(cl:defconstant +brand2-jpgs+ (fourcc #\j #\p #\g #\s))
(cl:defconstant +brand2-j2ki+ (fourcc #\j #\2 #\k #\i))
(cl:defconstant +brand2-j2is+ (fourcc #\j #\2 #\i #\s))
(cl:defconstant +brand2-miaf+ (fourcc #\m #\i #\a #\f))
(cl:defconstant +brand2-1pic+ (fourcc #\1 #\p #\i #\c))
(cl:defconstant +brand2-avci+ (fourcc #\a #\v #\c #\i))
(cl:defconstant +brand2-avcs+ (fourcc #\a #\v #\c #\s))
(cl:defconstant +brand2-iso8+ (fourcc #\i #\s #\o #\8))


(cffi:defcenum filetype-result
  (:filetype-no 0)
  (:filetype-yes-supported 1)
  (:filetype-yes-unsupported 2)
  (:filetype-maybe 3))

(cffi:defcfun ("heif_read_main_brand" read-main-brand)
  brand2
  (data :pointer)
  (len :int))

(cffi:defcfun ("heif_read_minor_version_brand" read-minor-version-brand)
  brand2
  (data :pointer)
  (len :int))

(cffi:defcfun ("heif_fourcc_to_brand" fourcc-to-brand)
  brand2
  (brand-fourcc :pointer))

(cffi:defcfun ("heif_brand_to_fourcc" brand-to-fourcc)
  :void
  (brand brand2)
  (out-fourcc :pointer))

(cffi:defcfun ("heif_has_compatible_brand" has-compatible-brand)
  :int
  (data :pointer)
  (len :int)
  (brand-fourcc :pointer))

(cffi:defcfun ("heif_list_compatible_brands" list-compatible-brands)
  (:struct error)
  (data :pointer)
  (len :int)
  (out-brands :pointer)
  (out-size :pointer))

(cffi:defcfun ("heif_free_list_of_compatible_brands" free-list-of-compatible-brands)
  :void
  (brands-list :pointer))

(cffi:defcfun ("heif_get_file_mime_type" get-file-mime-type)
  :string
  (data :pointer)
  (len :int))

(cffi:defcfun ("heif_check_filetype" check-filetype)
  filetype-result
  (data :pointer)
  (len :int))

(cffi:defcfun ("heif_has_compatible_filetype" has-compatible-filetype)
  (:struct error)
  (data :pointer)
  (len :int))

(cffi:defcfun ("heif_check_jpeg_filetype" check-jpeg-filetype)
  :int
  (data :pointer)
  (len :int))


;;;; heif_metadata.h


(cffi:defcenum metadata-compression
  (:off 0)
  (:auto 1)
  (:unknown 2)
  (:deflate 3)
  (:zlib 4)
  (:brotli 5))

;; Querying metadata
(cffi:defcfun ("heif_image_handle_get_number_of_metadata_blocks" image-handle-get-number-of-metadata-blocks)
    :int
  (handle :pointer)
  (type-filter :string))

(cffi:defcfun ("heif_image_handle_get_list_of_metadata_block_IDs" image-handle-get-list-of-metadata-block-ids)
    :int
  (handle :pointer)
  (type-filter :string)
  (ids :pointer) ; should be (item-id*) = uint32-t*
  (count :int))

(cffi:defcfun ("heif_image_handle_get_metadata_type" image-handle-get-metadata-type)
    :string
  (handle :pointer)
  (metadata-id :uint32))

(cffi:defcfun ("heif_image_handle_get_metadata_content_type" image-handle-get-metadata-content-type)
    :string
  (handle :pointer)
  (metadata-id :uint32))

(cffi:defcfun ("heif_image_handle_get_metadata_size" image-handle-get-metadata-size)
    size-t
  (handle :pointer)
  (metadata-id :uint32))

(cffi:defcfun ("heif_image_handle_get_metadata" image-handle-get-metadata)
    (:struct error)
  (handle :pointer)
  (metadata-id :uint32)
  (out-data :pointer))

(cffi:defcfun ("heif_image_handle_get_metadata_item_uri_type" image-handle-get-metadata-item-uri-type)
    :string
  (handle :pointer)
  (metadata-id :uint32))

;; Writing metadata
(cffi:defcfun ("heif_context_add_exif_metadata" context-add-exif-metadata)
    (:struct error)
  (ctx :pointer)
  (handle :pointer)
  (data :pointer)
  (size :int))

(cffi:defcfun ("heif_context_add_XMP_metadata" context-add-xmp-metadata)
    (:struct error)
  (ctx :pointer)
  (handle :pointer)
  (data :pointer)
  (size :int))

(cffi:defcfun ("heif_context_add_XMP_metadata2" context-add-xmp-metadata2)
    (:struct error)
  (ctx :pointer)
  (handle :pointer)
  (data :pointer)
  (size :int)
  (compression metadata-compression))

(cffi:defcfun ("heif_context_add_generic_metadata" context-add-generic-metadata)
    (:struct error)
  (ctx :pointer)
  (handle :pointer)
  (data :pointer)
  (size :int)
  (item-type :string)
  (content-type :string))

(cffi:defcfun ("heif_context_add_generic_uri_metadata" context-add-generic-uri-metadata)
    (:struct error)
  (ctx :pointer)
  (handle :pointer)
  (data :pointer)
  (size :int)
  (item-uri-type :string)
  (out-item-id :pointer)) ; (item-id*) = (uint32-t*)


;;; aux-images


;; --------------------------------------------------
;; Enums
;; --------------------------------------------------

(cffi:defcenum depth-representation-type
  (:uniform-inverse-z 0)
  (:uniform-disparity 1)
  (:uniform-z 2)
  (:nonuniform-disparity 3))

;; --------------------------------------------------
;; Structs
;; --------------------------------------------------

(cffi:defcstruct depth-representation-info
  (version :uint8)
  (has-z-near :uint8)
  (has-z-far :uint8)
  (has-d-min :uint8)
  (has-d-max :uint8)
  (z-near :double)
  (z-far :double)
  (d-min :double)
  (d-max :double)
  (depth-representation-type depth-representation-type)
  (disparity-reference-view :uint32)
  (depth-nonlinear-representation-model-size :uint32)
  (depth-nonlinear-representation-model :pointer)) ; uint8-t*

;; --------------------------------------------------
;; Constants
;; --------------------------------------------------

(cl:defconstant +libaux-image-filter-omit-alpha+ (cl:ash 1 1))
(cl:defconstant +libaux-image-filter-omit-depth+ (cl:ash 2 1))

;; --------------------------------------------------
;; Depth image APIs
;; --------------------------------------------------

(cffi:defcfun ("heif_image_handle_has_depth_image" image-handle-has-depth-image)
    :int
  (handle :pointer))

(cffi:defcfun ("heif_image_handle_get_number_of_depth_images" image-handle-get-number-of-depth-images)
    :int
  (handle :pointer))

(cffi:defcfun ("heif_image_handle_get_list_of_depth_image_IDs" image-handle-get-list-of-depth-image-ids)
    :int
  (handle :pointer)
  (ids :pointer) ; (item-id*)
  (count :int))

(cffi:defcfun ("heif_image_handle_get_depth_image_handle" image-handle-get-depth-image-handle)
    (:struct error)
  (handle :pointer)
  (depth-image-id :uint32)
  (out-depth-handle :pointer)) ; (image-handle**)

(cffi:defcfun ("heif_image_handle_get_depth_image_representation_info" image-handle-get-depth-image-representation-info)
    :int
  (handle :pointer)
  (depth-image-id :uint32)
  (out-info :pointer)) ; (const depth-representation-info**)

(cffi:defcfun ("heif_depth_representation_info_free" depth-representation-info-free)
    :void
  (info :pointer))

;; --------------------------------------------------
;; Thumbnail APIs
;; --------------------------------------------------

(cffi:defcfun ("heif_image_handle_get_number_of_thumbnails" image-handle-get-number-of-thumbnails)
    :int
  (handle :pointer))

(cffi:defcfun ("heif_image_handle_get_list_of_thumbnail_IDs" image-handle-get-list-of-thumbnail-ids)
    :int
  (handle :pointer)
  (ids :pointer)
  (count :int))

(cffi:defcfun ("heif_image_handle_get_thumbnail" image-handle-get-thumbnail)
    (:struct error)
  (main-image-handle :pointer)
  (thumbnail-id :uint32)
  (out-thumbnail-handle :pointer))

(cffi:defcfun ("heif_context_encode_thumbnail" context-encode-thumbnail)
    (:struct error)
  (ctx :pointer)
  (image :pointer)
  (master-image-handle :pointer)
  (encoder :pointer)
  (options :pointer)
  (bbox-size :int)
  (out-thumb-image-handle :pointer))

(cffi:defcfun ("heif_context_assign_thumbnail" context-assign-thumbnail)
    (:struct error)
  (ctx :pointer)
  (master-image :pointer)
  (thumbnail-image :pointer))

;; --------------------------------------------------
;; Auxiliary image APIs
;; --------------------------------------------------

(cffi:defcfun ("heif_image_handle_get_number_of_auxiliary_images" image-handle-get-number-of-auxiliary-images)
    :int
  (handle :pointer)
  (aux-filter :int))

(cffi:defcfun ("heif_image_handle_get_list_of_auxiliary_image_IDs" image-handle-get-list-of-auxiliary-image-ids)
    :int
  (handle :pointer)
  (aux-filter :int)
  (ids :pointer)
  (count :int))

(cffi:defcfun ("heif_image_handle_get_auxiliary_type" image-handle-get-auxiliary-type)
    (:struct error)
  (handle :pointer)
  (out-type :pointer)) ; (const char**)

(cffi:defcfun ("heif_image_handle_release_auxiliary_type" image-handle-release-auxiliary-type)
    :void
  (handle :pointer)
  (out-type :pointer))

(cffi:defcfun ("heif_image_handle_get_auxiliary_image_handle" image-handle-get-auxiliary-image-handle)
    (:struct error)
  (main-image-handle :pointer)
  (auxiliary-id :uint32)
  (out-auxiliary-handle :pointer))

;;; heif_entity_groups.h

;; --------------------------------------------------
;; Typedefs
;; --------------------------------------------------

(cffi:defctype entity-group-id :uint32)

;; --------------------------------------------------
;; Structs
;; --------------------------------------------------

(cffi:defcstruct entity-group
  (entity-group-id entity-group-id)
  (entity-group-type :uint32) ; FourCC
  (entities :pointer)         ; item-id*
  (num-entities :uint32))

;; --------------------------------------------------
;; Functions
;; --------------------------------------------------

(cffi:defcfun ("heif_context_get_entity_groups" context-get-entity-groups)
    :pointer ; returns (entity-group*)
  (ctx :pointer)
  (type-filter :uint32)      ; 0 disables the filter
  (item-filter :uint32)      ; 0 disables the filter
  (out-num-groups :pointer)) ; int*

(cffi:defcfun ("heif_entity_groups_release" entity-groups-release)
    :void
  (groups :pointer)          ; (entity-group*)
  (num-groups :int))


;;; heif_security.h

;; -------------------------
;; Structs
;; -------------------------

(cffi:defcstruct security-limits
  (version :uint8)
  ;; Padding 3 bytes after uint8-t version for alignment (optional)
  (max-image-size-pixels :uint64)
  (max-number-of-tiles :uint64)
  (max-bayer-pattern-pixels :uint32)
  (max-items :uint32)
  (max-color-profile-size :uint32)
  (max-memory-block-size :uint64)
  (max-components :uint32)
  (max-iloc-extents-per-item :uint32)
  (max-size-entity-group :uint32)
  (max-children-per-box :uint32)
  (max-total-memory :uint64)
  (max-sample-description-box-entries :uint32)
  (max-sample-group-description-box-entries :uint32))

;; -------------------------
;; Functions
;; -------------------------

(cffi:defcfun ("heif_get_global_security_limits" get-global-security-limits)
    (:pointer)) ;; returns const security-limits*

(cffi:defcfun ("heif_get_disabled_security_limits" get-disabled-security-limits)
    (:pointer)) ;; returns const security-limits*

(cffi:defcfun ("heif_context_get_security_limits" context-get-security-limits)
    (:pointer) ;; returns security-limits*
  (ctx :pointer)) ;; const context*

(cffi:defcfun ("heif_context_set_security_limits" context-set-security-limits)
    :int ;; error
  (ctx :pointer)
  (limits :pointer)) ;; const security-limits*

(cffi:defcfun ("heif_context_set_maximum_image_size_limit" context-set-maximum-image-size-limit)
    :void
  (ctx :pointer)
  (maximum-width :int))


;;;; heif_encoding.h

;; ---- enums ----

(cl:defconstant +orientation-normal+ 1)
(cl:defconstant +orientation-flip-horizontally+ 2)
(cl:defconstant +orientation-rotate-180+ 3)
(cl:defconstant +orientation-flip-vertically+ 4)
(cl:defconstant +orientation-rotate-90-cw-then-flip-horizontally+ 5)
(cl:defconstant +orientation-rotate-90-cw+ 6)
(cl:defconstant +orientation-rotate-90-cw-then-flip-vertically+ 7)
(cl:defconstant +orientation-rotate-270-cw+ 8)

;; ---- opaque structs ----

(cffi:defcstruct encoder)
(cffi:defcstruct encoder-descriptor)
(cffi:defcstruct encoder-parameter)
(cffi:defcstruct encoding-options)

;; ---- encoding-options struct ----
;; This is a large struct, but let's define main fields relevant for usage:

(cffi:defcstruct encoding-options
  (version :uint8)
  (save-alpha-channel :uint8)
  (macOS-compatibility-workaround :uint8)
  (save-two-colr-boxes-when-ICC-and-nclx-available :uint8)
  (output-nclx-profile :pointer) ;; color-profile-nclx*
  (macOS-compatibility-workaround-no-nclx-profile :uint8)
  (image-orientation :int) ;; enum orientation
  ;; skipping color-conversion-options (nested struct) for brevity; use :pointer if needed
  (color-conversion-options :pointer)
  (prefer-uncC-short-form :uint8)
  ;; padding/alignment may be needed here depending on your FFI
)

;; ---- functions ----

(cffi:defcfun ("heif_have_encoder_for_format" have-encoder-for-format)
    :int
  (format :int)) ;; enum compression-format

(cffi:defcfun ("heif_get_encoder_descriptors" get-encoder-descriptors)
    :int
  (context :pointer)
  (format-filter :int)
  (name-filter :string)
  (out-encoders (:pointer)) ;; pointer to const encoder-descriptor**
  (count :int))

(cffi:defcfun ("heif_encoder_descriptor_get_name" encoder-descriptor-get-name)
    :string
  (desc :pointer))

(cffi:defcfun ("heif_encoder_descriptor_get_id_name" encoder-descriptor-get-id-name)
    :string
  (desc :pointer))

(cffi:defcfun ("heif_encoder_descriptor_get_compression_format" encoder-descriptor-get-compression-format)
    :int
  (desc :pointer))

(cffi:defcfun ("heif_encoder_descriptor_supports_lossy_compression" encoder-descriptor-supports-lossy-compression)
    :int
  (desc :pointer))

(cffi:defcfun ("heif_encoder_descriptor_supports_lossless_compression" encoder-descriptor-supports-lossless-compression)
    :int
  (desc :pointer))

(cffi:defcfun ("heif_context_get_encoder" context-get-encoder)
    :int ;; error
  (context :pointer)
  (desc :pointer)
  (out-encoder (:pointer))) ;; encoder**

(cffi:defcfun ("heif_context_get_encoder_for_format" context-get-encoder-for-format)
    :int
  (context :pointer)
  (format :int)
  (out-encoder (:pointer)))

(cffi:defcfun ("heif_encoder_release" encoder-release)
    :void
  (encoder :pointer))

(cffi:defcfun ("heif_encoder_get_name" encoder-get-name)
    :string
  (encoder :pointer))

;; --- Encoder parameter functions ---

(cffi:defcfun ("heif_encoder_set_lossy_quality" encoder-set-lossy-quality)
    :int
  (encoder :pointer)
  (quality :int))

(cffi:defcfun ("heif_encoder_set_lossless" encoder-set-lossless)
    :int
  (encoder :pointer)
  (enable :int))

(cffi:defcfun ("heif_encoder_set_logging_level" encoder-set-logging-level)
    :int
  (encoder :pointer)
  (level :int))

(cffi:defcfun ("heif_encoder_list_parameters" encoder-list-parameters)
    (:pointer) ;; const encoder-parameter* const*
  (encoder :pointer))

(cffi:defcfun ("heif_encoder_parameter_get_name" encoder-parameter-get-name)
    :string
  (param :pointer))

(cffi:defcfun ("heif_encoder_parameter_get_type" encoder-parameter-get-type)
    :int ;; enum encoder-parameter-type
  (param :pointer))

;; ... add other parameter related functions similarly if needed ...

;; --- Encoding images ---

(cffi:defcfun ("heif_encoding_options_alloc" encoding-options-alloc)
    (:pointer)) ;; encoding-options*

(cffi:defcfun ("heif_encoding_options_copy" encoding-options-copy)
    :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("heif_encoding_options_free" encoding-options-free)
    :void
  (options :pointer))

(cffi:defcfun ("heif_context_encode_image" context-encode-image)
    :int ;; error
  (context :pointer)
  (image :pointer)
  (encoder :pointer)
  (options :pointer)
  (out-image-handle (:pointer)))

(cffi:defcfun ("heif_context_add_overlay_image" context-add-overlay-image)
    :int
  (context :pointer)
  (image-width :uint32)
  (image-height :uint32)
  (nImages :uint16)
  (image-ids (:pointer)) ;; item-id*
  (offsets (:pointer)) ;; int32-t* (or NULL)
  (background-rgba (:pointer)) ;; uint16-t[4] (or NULL)
  (out-iovl-image-handle (:pointer)))

(cffi:defcfun ("heif_context_set_primary_image" context-set-primary-image)
    :int
  (context :pointer)
  (image-handle :pointer))

(cffi:defcfun ("heif_context_set_major_brand" context-set-major-brand)
    :void
  (context :pointer)
  (major-brand :uint32)) ;; brand2

(cffi:defcfun ("heif_context_add_compatible_brand" context-add-compatible-brand)
    :void
  (context :pointer)
  (compatible-brand :uint32))


;;; heif_decoding.h

;; --- enums ---

(cl:defconstant +progress-step-total+ 0)
(cl:defconstant +progress-step-load-tile+ 1)

;; --- opaque structs ---

(cffi:defcstruct decoding-options
  (version :uint8)
  (ignore-transformations :uint8)
  ;; function pointers, will use :pointer here
  (start-progress :pointer)
  (on-progress :pointer)
  (end-progress :pointer)
  (progress-user-data :pointer)
  (convert-hdr-to-8bit :uint8)
  (strict-decoding :uint8)
  (decoder-id :pointer) ;; const char*
  (color-conversion-options :pointer) ;; color-conversion-options*
  (cancel-decoding :pointer)
  (color-conversion-options-ext :pointer) ;; color-conversion-options-ext*
)

(cffi:defcstruct decoder-descriptor)

;; --- functions ---

(cffi:defcfun ("heif_context_set_max_decoding_threads" context-set-max-decoding-threads)
    :void
  (ctx :pointer)
  (max-threads :int))

(cffi:defcfun ("heif_have_decoder_for_format" have-decoder-for-format)
    :int
  (format :int)) ;; enum compression-format

(cffi:defcfun ("heif_decoding_options_alloc" decoding-options-alloc)
    (:pointer)) ;; decoding-options*

(cffi:defcfun ("heif_decoding_options_copy" decoding-options-copy)
    :void
  (dst :pointer)
  (src :pointer))

(cffi:defcfun ("heif_decoding_options_free" decoding-options-free)
    :void
  (opts :pointer))

(cffi:defcfun ("heif_get_decoder_descriptors" get-decoder-descriptors)
    :int
  (format-filter :int)
  (out-decoders (:pointer)) ;; const decoder-descriptor**
  (count :int))

(cffi:defcfun ("heif_decoder_descriptor_get_name" decoder-descriptor-get-name)
    :string
  (desc :pointer))

(cffi:defcfun ("heif_decoder_descriptor_get_id_name" decoder-descriptor-get-id-name)
    :string
  (desc :pointer))

(cffi:defcfun ("heif_decode_image" decode-image)
    (:struct error) ;; error
  (in-handle :pointer) ;; const image-handle*
  (out-img (:pointer)) ;; image**
  (colorspace colorspace) ;; enum colorspace
  (chroma chroma) ;; enum chroma
  (options :pointer)) ;; const decoding-options*

;;;;; heif_context.h

;; --- enum compression-format ---

(cl:defconstant +compression-undefined+ 0)
(cl:defconstant +compression-hevc+ 1)
(cl:defconstant +compression-avc+ 2)
(cl:defconstant +compression-jpeg+ 3)
(cl:defconstant +compression-av1+ 4)
(cl:defconstant +compression-vvc+ 5)
(cl:defconstant +compression-evc+ 6)
(cl:defconstant +compression-jpeg2000+ 7)
(cl:defconstant +compression-uncompressed+ 8)
(cl:defconstant +compression-mask+ 9)
(cl:defconstant +compression-htj2k+ 10)

;; --- opaque structs ---

(cffi:defcstruct context) ;; Opaque context for HEIF file handling
(cffi:defcstruct image-handle) ;; Opaque handle to an image inside context
(cffi:defcstruct reader)

;; item-id is uint32-t
(cffi:defctype item-id :uint32)

;; reading-options is opaque pointer (not defined here)
(cffi:defcstruct reading-options)

;; writer struct with function pointer
(cffi:defcstruct writer
  (writer-api-version :int)
  (write (:pointer)) ;; function pointer: error (*write)(context*, const void*, size-t, void*)
)

;; --- functions ---

(cffi:defcfun ("heif_context_alloc" context-alloc)
  (:pointer)) ;; returns context*

(cffi:defcfun ("heif_context_free" context-free)
  :void
  (ctx :pointer))

(cffi:defcfun ("heif_context_read_from_file" context-read-from-file)
  (:struct error)
  (ctx :pointer)
  (filename :string)
  (reading-options :pointer)) ;; can be NULL

(cffi:defcfun ("heif_context_read_from_memory" context-read-from-memory)
  (:struct error)
  (ctx :pointer)
  (mem (:pointer))
  (size size-t)
  (reading-options :pointer)) ;; deprecated, use without copy

(cffi:defcfun ("heif_context_read_from_memory_without_copy" context-read-from-memory-without-copy)
  (:struct error)
  (ctx :pointer)
  (mem (:pointer))
  (size size-t)
  (reading-options :pointer))

(cffi:defcfun ("heif_context_read_from_reader" context-read-from-reader)
  (:struct error)
  (ctx :pointer)
  (reader (:pointer)) ;; reader*
  (userdata :pointer)
  (reading-options :pointer))

(cffi:defcfun ("heif_context_get_number_of_top_level_images" context-get-number-of-top-level-images)
  :int
  (ctx :pointer))

(cffi:defcfun ("heif_context_is_top_level_image_ID" context-is-top-level-image-ID)
  :int ;; bool as int
  (ctx :pointer)
  (id item-id))

(cffi:defcfun ("heif_context_get_list_of_top_level_image_IDs" context-get-list-of-top-level-image-IDs)
  :int ;; returns number of IDs filled
  (ctx :pointer)
  (ID-array (:pointer)) ;; item-id* array preallocated
  (count :int))

(cffi:defcfun ("heif_context_get_primary_image_ID" context-get-primary-image-ID)
  (:struct error)
  (ctx :pointer)
  (id-ptr (:pointer))) ;; item-id*

(cffi:defcfun ("heif_context_get_primary_image_handle" context-get-primary-image-handle)
  (:struct error)
  (ctx :pointer)
  (out-handle (:pointer))) ;; image-handle**

(cffi:defcfun ("heif_context_get_image_handle" context-get-image-handle)
  (:struct error)
  (ctx :pointer)
  (id item-id)
  (out-handle (:pointer))) ;; image-handle**

(cffi:defcfun ("heif_context_debug_dump_boxes_to_file" context-debug-dump-boxes-to-file)
  :void
  (ctx :pointer)
  (fd :int))

(cffi:defcfun ("heif_context_write_to_file" context-write-to-file)
  (:struct error)
  (ctx :pointer)
  (filename :string))

(cffi:defcfun ("heif_context_write" context-write)
  (:struct error)
  (ctx :pointer)
  (writer (:pointer)) ;; writer*
  (userdata :pointer))

;;; heif_image_handle.h

;; --- opaque struct ---

(cffi:defcstruct image-handle) ;; opaque pointer

;; item-id is uint32
(cffi:defctype item-id :uint32)

;; context opaque pointer
(cffi:defcstruct context)

;; --- functions ---

;; void image-handle-release(const image-handle*);
(cffi:defcfun ("heif_image_handle_release" image-handle-release)
  :void
  (handle (:pointer)))

;; int image-handle-is-primary-image(const image-handle*);
(cffi:defcfun ("heif_image_handle_is_primary_image" image-handle-is-primary-image)
  :int
  (handle (:pointer)))

;; item-id image-handle-get-item-id(const image-handle*);
(cffi:defcfun ("heif_image_handle_get_item_id" image-handle-get-item-id)
  item-id
  (handle (:pointer)))

;; int image-handle-get-width(const image-handle*);
(cffi:defcfun ("heif_image_handle_get_width" image-handle-get-width)
  :int
  (handle (:pointer)))

;; int image-handle-get-height(const image-handle*);
(cffi:defcfun ("heif_image_handle_get_height" image-handle-get-height)
  :int
  (handle (:pointer)))

;; int image-handle-has-alpha-channel(const image-handle*);
(cffi:defcfun ("heif_image_handle_has_alpha_channel" image-handle-has-alpha-channel)
  :int
  (handle (:pointer)))

;; int image-handle-is-premultiplied-alpha(const image-handle*);
(cffi:defcfun ("heif_image_handle_is_premultiplied_alpha" image-handle-is-premultiplied-alpha)
  :int
  (handle (:pointer)))

;; int image-handle-get-luma-bits-per-pixel(const image-handle*);
(cffi:defcfun ("heif_image_handle_get_luma_bits_per_pixel" image-handle-get-luma-bits-per-pixel)
  :int
  (handle (:pointer)))

;; int image-handle-get-chroma-bits-per-pixel(const image-handle*);
(cffi:defcfun ("heif_image_handle_get_chroma_bits_per_pixel" image-handle-get-chroma-bits-per-pixel)
  :int
  (handle (:pointer)))

;; (:struct error) image-handle-get-preferred-decoding-colorspace(const image-handle*,
;;                                                                enum colorspace*,
;;                                                                enum chroma*);
;; You need to define enums colorspace and chroma similarly, here assumed as int.
(cffi:defcfun ("heif_image_handle_get_preferred_decoding_colorspace" image-handle-get-preferred-decoding-colorspace)
  (:struct error)
  (handle (:pointer))
  (out-colorspace (:pointer)) ;; enum colorspace*
  (out-chroma (:pointer)))    ;; enum chroma*

;; int image-handle-get-ispe-width(const image-handle*);
(cffi:defcfun ("heif_image_handle_get_ispe_width" image-handle-get-ispe-width)
  :int
  (handle (:pointer)))

;; int image-handle-get-ispe-height(const image-handle*);
(cffi:defcfun ("heif_image_handle_get_ispe_height" image-handle-get-ispe-height)
  :int
  (handle (:pointer)))

;; int image-handle-get-pixel-aspect-ratio(const image-handle*, uint32-t* aspect-h, uint32-t* aspect-v);
(cffi:defcfun ("heif_image_handle_get_pixel_aspect_ratio" image-handle-get-pixel-aspect-ratio)
  :int
  (handle (:pointer))
  (aspect-h (:pointer)) ;; uint32-t*
  (aspect-v (:pointer))) ;; uint32-t*

;; context* image-handle-get-context(const image-handle*);
(cffi:defcfun ("heif_image_handle_get_context" image-handle-get-context)
  (:pointer) ;; context*
  (handle (:pointer)))


;;;; heif_tiling.h


;; Forward declarations (opaque pointers)
(cffi:defcstruct encoder)
(cffi:defcstruct encoding-options)
(cffi:defcstruct image-handle)
(cffi:defcstruct image)
(cffi:defcstruct context)

;; item-id is uint32
(cffi:defctype item-id :uint32)

;; colorspace and chroma enums - define as :int for now
(cffi:defctype colorspace :int)
(cffi:defctype chroma :int)

;; Struct image-tiling
(cffi:defcstruct image-tiling
  (version :int)
  (num-columns :uint32)
  (num-rows :uint32)
  (tile-width :uint32)
  (tile-height :uint32)
  (image-width :uint32)
  (image-height :uint32)
  (top-offset :uint32)
  (left-offset :uint32)
  (number-of-extra-dimensions :uint8)
  (extra-dimension-size (:array :uint32 8))) ; array of 8 uint32

;; --- functions ---

;; (:struct error) image-handle-get-image-tiling(const image-handle* handle,
;;                                              int process-image-transformations,
;;                                              struct image-tiling* out-tiling);
(cffi:defcfun ("heif_image_handle_get_image_tiling" image-handle-get-image-tiling)
  (:struct error)
  (handle (:pointer))
  (process-image-transformations :int)
  (out-tiling (:pointer))) ;; pointer to image-tiling struct

;; (:struct error) image-handle-get-grid-image-tile-id(const image-handle* handle,
;;                                                     int process-image-transformations,
;;                                                     uint32-t tile-x, uint32-t tile-y,
;;                                                     item-id* out-tile-item-id);
(cffi:defcfun ("heif_image_handle_get_grid_image_tile_id" image-handle-get-grid-image-tile-id)
  (:struct error)
  (handle (:pointer))
  (process-image-transformations :int)
  (tile-x :uint32)
  (tile-y :uint32)
  (out-tile-item-id (:pointer))) ;; pointer to uint32

;; (:struct error) image-handle-decode-image-tile(const image-handle* in-handle,
;;                                                image** out-img,
;;                                                enum colorspace colorspace,
;;                                                enum chroma chroma,
;;                                                const decoding-options* options,
;;                                                uint32-t tile-x, uint32-t tile-y);
(cffi:defcfun ("heif_image_handle_decode_image_tile" image-handle-decode-image-tile)
  (:struct error)
  (in-handle (:pointer))
  (out-img (:pointer)) ;; pointer to image* (pointer to pointer)
  (colorspace colorspace)
  (chroma chroma)
  (options (:pointer)) ;; decoding-options*
  (tile-x :uint32)
  (tile-y :uint32))

;; (:struct error) context-encode-grid(context* ctx,
;;                                     image** tiles,
;;                                     uint16-t rows,
;;                                     uint16-t columns,
;;                                     encoder* encoder,
;;                                     const encoding-options* input-options,
;;                                     image-handle** out-image-handle);
(cffi:defcfun ("heif_context_encode_grid" context-encode-grid)
  (:struct error)
  (ctx (:pointer))
  (tiles (:pointer)) ;; pointer to array of image*
  (rows :uint16)
  (columns :uint16)
  (encoder (:pointer))
  (input-options (:pointer))
  (out-image-handle (:pointer))) ;; pointer to image-handle*

;; (:struct error) context-add-grid-image(context* ctx,
;;                                        uint32-t image-width,
;;                                        uint32-t image-height,
;;                                        uint32-t tile-columns,
;;                                        uint32-t tile-rows,
;;                                        const encoding-options* encoding-options,
;;                                        image-handle** out-grid-image-handle);
(cffi:defcfun ("heif_context_add_grid_image" context-add-grid-image)
  (:struct error)
  (ctx (:pointer))
  (image-width :uint32)
  (image-height :uint32)
  (tile-columns :uint32)
  (tile-rows :uint32)
  (encoding-options (:pointer))
  (out-grid-image-handle (:pointer))) ;; pointer to image-handle*

;; (:struct error) context-add-image-tile(context* ctx,
;;                                        image-handle* tiled-image,
;;                                        uint32-t tile-x, uint32-t tile-y,
;;                                        const image* image,
;;                                        encoder* encoder);
(cffi:defcfun ("heif_context_add_image_tile" context-add-image-tile)
  (:struct error)
  (ctx (:pointer))
  (tiled-image (:pointer))
  (tile-x :uint32)
  (tile-y :uint32)
  (image (:pointer))
  (encoder (:pointer)))


;; Export all symbols
(cl:eval-when (:load-toplevel :compile-toplevel :execute)
  (cl:let ((pkg (cl:find-package "HEIF/FFI")))
    (cl:do-symbols (sym pkg)
      (cl:export sym pkg))))
