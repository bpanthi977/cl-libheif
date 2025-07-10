(in-package :cl-libheif/bindings)

;;; library.h

(cffi:defcstruct heif_init_params
  (version :int))

(cffi:defcstruct heif_error
  (code :int)
  (subcode :int)
  (message :string))

;; ------------------------------------------------------------
;; Version Functions

(cffi:defcfun ("heif_get_version" heif-get-version) :string)

(cffi:defcfun ("heif_get_version_number" heif-get-version-number) :uint32)

(cffi:defcfun ("heif_get_version_number_major" heif-get-version-number-major) :int)

(cffi:defcfun ("heif_get_version_number_minor" heif-get-version-number-minor) :int)

(cffi:defcfun ("heif_get_version_number_maintenance" heif-get-version-number-maintenance) :int)

;; ------------------------------------------------------------
;; Init / Deinit Functions

(cffi:defcfun ("heif_init" heif-init) :void
  (params :pointer))  ;; can be NULL

(cffi:defcfun ("heif_deinit" heif-deinit) :void)

;; ------------------------------------------------------------
;; String utilities

(cffi:defcfun ("heif_string_release" heif-string-release) :void
  (str :string))

;;;;;;;;;;;;;; heif_image.h

(defcenum heif_chroma
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

(defcenum heif_colorspace
  (:undefined 99)
  (:ycbcr 0)
  (:rgb 1)
  (:monochrome 2)
  (:nonvisual 3))

(defcenum heif_channel
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


(defctype heif_image :pointer)
(defctype heif_image_handle :pointer)
(defctype heif_security_limits :pointer)
(defctype heif_scaling_options :pointer)

;; Image info
(defcfun ("heif_image_get_colorspace" heif-image-get-colorspace) heif_colorspace
  (image heif_image))

(defcfun ("heif_image_get_chroma_format" heif-image-get-chroma-format) heif_chroma
  (image heif_image))

(defcfun ("heif_image_get_width" heif-image-get-width) :int
  (image heif_image) (channel heif_channel))

(defcfun ("heif_image_get_height" heif-image-get-height) :int
  (image heif_image) (channel heif_channel))

(defcfun ("heif_image_get_primary_width" heif-image-get-primary-width) :int
  (image heif_image))

(defcfun ("heif_image_get_primary_height" heif-image-get-primary-height) :int
  (image heif_image))

(defcfun ("heif_image_get_bits_per_pixel" heif-image-get-bpp) :int
  (image heif_image) (channel heif_channel))

(defcfun ("heif_image_get_bits_per_pixel_range" heif-image-get-bpp-range) :int
  (image heif_image) (channel heif_channel))

(defcfun ("heif_image_has_channel" heif-image-has-channel) :int
  (image heif_image) (channel heif_channel))

;; Plane Access
(defcfun ("heif_image_get_plane_readonly" heif-image-get-plane-readonly) :pointer
  (image heif_image) (channel heif_channel) (out-stride :pointer))

(defcfun ("heif_image_get_plane" heif-image-get-plane) :pointer
  (image heif_image) (channel heif_channel) (out-stride :pointer))

(defcfun ("heif_image_get_plane_readonly2" heif-image-get-plane-readonly2) :pointer
  (image heif_image) (channel heif_channel) (out-stride (:pointer :size)))

(defcfun ("heif_image_get_plane2" heif-image-get-plane2) :pointer
  (image heif_image) (channel heif_channel) (out-stride (:pointer :size)))

;; Image management
(defcfun ("heif_image_crop" heif-image-crop) :void
  (image heif_image) (left :int) (right :int) (top :int) (bottom :int))

(defcfun ("heif_image_extract_area" heif-image-extract-area) :void
  (src heif_image) (x0 :uint32) (y0 :uint32) (w :uint32) (h :uint32)
  (limits heif_security_limits) (out-image (:pointer heif_image)))

(defcfun ("heif_image_scale_image" heif-image-scale) :void
  (input heif_image) (output (:pointer heif_image)) (w :int) (h :int)
  (options heif_scaling_options))

(defcfun ("heif_image_extend_to_size_fill_with_zero" heif-image-extend-zero) :void
  (img heif_image) (w :uint32) (h :uint32))

(defcfun ("heif_image_get_decoding_warnings" heif-image-get-warnings) :int
  (img heif_image) (start-idx :int)
  (out-warnings :pointer) (max :int))

(defcfun ("heif_image_add_decoding_warning" heif-image-add-warning) :void
  (img heif_image) (err :pointer))

(defcfun ("heif_image_release" heif-image-release) :void
  (img heif_image))

(defcfun ("heif_image_get_pixel_aspect_ratio" heif-image-get-aspect) :void
  (img heif_image) (h (:pointer :uint32)) (v (:pointer :uint32)))

(defcfun ("heif_image_set_pixel_aspect_ratio" heif-image-set-aspect) :void
  (img heif_image) (h :uint32) (v :uint32))

(defcfun ("heif_image_create" heif-image-create) :void
  (w :int) (h :int) (colorspace heif_colorspace) (chroma heif_chroma)
  (out (:pointer heif_image)))

(defcfun ("heif_image_add_plane" heif-image-add-plane) :void
  (img heif_image) (channel heif_channel) (w :int) (h :int) (bpp :int))

(defcfun ("heif_image_add_plane_safe" heif-image-add-plane-safe) :void
  (img heif_image) (channel heif_channel) (w :int) (h :int) (bpp :int)
  (limits heif_security_limits))

(defcfun ("heif_image_set_premultiplied_alpha" heif-image-set-premul-alpha) :void
  (img heif_image) (flag :int))

(defcfun ("heif_image_is_premultiplied_alpha" heif-image-is-premul-alpha) :int
  (img heif_image))

(defcfun ("heif_image_extend_padding_to_size" heif-image-extend-padding) :void
  (img heif_image) (min-w :int) (min-h :int))


;;;;;;;;;;;; heif_color.h

;;; Enums
(cffi:defcenum heif_chroma_downsampling_algorithm
  (:nearest-neighbor 1)
  (:average 2)
  (:sharp-yuv 3))

(cffi:defcenum heif_chroma_upsampling_algorithm
  (:nearest-neighbor 1)
  (:bilinear 2))

(cffi:defcenum heif_alpha_composition_mode
  (:none 0)
  (:solid-color 1)
  (:checkerboard 2))

(cffi:defcenum heif_color_profile_type
  (:not-present 0)
  (:nclx #x6E636C78) ; 'nclx'
  (:ricc #x72494343) ; 'rICC'
  (:prof #x70726F66)) ; 'prof'

(cffi:defcenum heif_color_primaries
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

(cffi:defcenum heif_transfer_characteristics
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

(cffi:defcenum heif_matrix_coefficients
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
(cffi:defcstruct heif_color_conversion_options
  (version :uint8)
  (preferred_chroma_downsampling_algorithm heif_chroma_downsampling_algorithm)
  (preferred_chroma_upsampling_algorithm heif_chroma_upsampling_algorithm)
  (only_use_preferred_chroma_algorithm :uint8))

(cffi:defcstruct heif_color_conversion_options_ext
  (version :uint8)
  (alpha_composition_mode heif_alpha_composition_mode)
  (background_red :uint16)
  (background_green :uint16)
  (background_blue :uint16)
  (secondary_background_red :uint16)
  (secondary_background_green :uint16)
  (secondary_background_blue :uint16)
  (checkerboard_square_size :uint16))

(cffi:defcstruct heif_color_profile_nclx
  (version :uint8)
  (color_primaries heif_color_primaries)
  (transfer_characteristics heif_transfer_characteristics)
  (matrix_coefficients heif_matrix_coefficients)
  (full_range_flag :uint8)
  ;; decoded values
  (color_primary_red_x :float)
  (color_primary_red_y :float)
  (color_primary_green_x :float)
  (color_primary_green_y :float)
  (color_primary_blue_x :float)
  (color_primary_blue_y :float)
  (color_primary_white_x :float)
  (color_primary_white_y :float))

(cffi:defcstruct heif_content_light_level
  (max_content_light_level :uint16)
  (max_pic_average_light_level :uint16))

(cffi:defcstruct heif_mastering_display_colour_volume
  (display_primaries_x (:array :uint16 3))
  (display_primaries_y (:array :uint16 3))
  (white_point_x :uint16)
  (white_point_y :uint16)
  (max_display_mastering_luminance :uint32)
  (min_display_mastering_luminance :uint32))

(cffi:defcstruct heif_decoded_mastering_display_colour_volume
  (display_primaries_x (:array :float 3))
  (display_primaries_y (:array :float 3))
  (white_point_x :float)
  (white_point_y :float)
  (max_display_mastering_luminance :double)
  (min_display_mastering_luminance :double))

(cffi:defcstruct heif_ambient_viewing_environment
  (ambient_illumination :uint32)
  (ambient_light_x :uint16)
  (ambient_light_y :uint16))

;;; Function declarations

(cffi:defcfun ("heif_color_conversion_options_set_defaults" heif-color-conversion-options-set-defaults)
  :void (opts :pointer))

(cffi:defcfun ("heif_color_conversion_options_ext_alloc" heif-color-conversion-options-ext-alloc)
  :pointer)

(cffi:defcfun ("heif_color_conversion_options_ext_copy" heif-color-conversion-options-ext-copy)
  :void (dst :pointer) (src :pointer))

(cffi:defcfun ("heif_color_conversion_options_ext_free" heif-color-conversion-options-ext-free)
  :void (ptr :pointer))

(cffi:defcfun ("heif_image_handle_get_color_profile_type" heif-image-handle-get-color-profile-type)
  heif_color_profile_type (handle :pointer))

(cffi:defcfun ("heif_image_handle_get_raw_color_profile_size" heif-image-handle-get-raw-color-profile-size)
  :size_t (handle :pointer))

(cffi:defcfun ("heif_image_handle_get_raw_color_profile" heif-image-handle-get-raw-color-profile)
  :pointer (handle :pointer) (out-data :pointer))

(cffi:defcfun ("heif_nclx_color_profile_set_color_primaries" heif-nclx-color-profile-set-color-primaries)
  :pointer (nclx :pointer) (cp :uint16))

(cffi:defcfun ("heif_nclx_color_profile_set_transfer_characteristics" heif-nclx-color-profile-set-transfer-characteristics)
  :pointer (nclx :pointer) (tc :uint16))

(cffi:defcfun ("heif_nclx_color_profile_set_matrix_coefficients" heif-nclx-color-profile-set-matrix-coefficients)
  :pointer (nclx :pointer) (mc :uint16))

(cffi:defcfun ("heif_image_handle_get_nclx_color_profile" heif-image-handle-get-nclx-color-profile)
  :pointer (handle :pointer) (out-data :pointer))

(cffi:defcfun ("heif_nclx_color_profile_alloc" heif-nclx-color-profile-alloc)
  :pointer)

(cffi:defcfun ("heif_nclx_color_profile_free" heif-nclx-color-profile-free)
  :void (profile :pointer))

(cffi:defcfun ("heif_image_get_color_profile_type" heif-image-get-color-profile-type)
  heif_color_profile_type (image :pointer))

(cffi:defcfun ("heif_image_get_raw_color_profile_size" heif-image-get-raw-color-profile-size)
  :size_t (image :pointer))

(cffi:defcfun ("heif_image_get_raw_color_profile" heif-image-get-raw-color-profile)
  :pointer (image :pointer) (out-data :pointer))

(cffi:defcfun ("heif_image_get_nclx_color_profile" heif-image-get-nclx-color-profile)
  :pointer (image :pointer) (out-data :pointer))

(cffi:defcfun ("heif_image_set_raw_color_profile" heif-image-set-raw-color-profile)
  :pointer (image :pointer)
	   (fourcc :string)
	   (data :pointer)
	   (size :size_t))

(cffi:defcfun ("heif_image_set_nclx_color_profile" heif-image-set-nclx-color-profile)
  :pointer (image :pointer) (profile :pointer))

(cffi:defcfun ("heif_image_has_content_light_level" heif-image-has-content-light-level)
  :int (image :pointer))

(cffi:defcfun ("heif_image_get_content_light_level" heif-image-get-content-light-level)
  :void (image :pointer) (out :pointer))

(cffi:defcfun ("heif_image_handle_get_content_light_level" heif-image-handle-get-content-light-level)
  :int (handle :pointer) (out :pointer))

(cffi:defcfun ("heif_image_set_content_light_level" heif-image-set-content-light-level)
  :void (image :pointer) (in :pointer))

(cffi:defcfun ("heif_image_has_mastering_display_colour_volume" heif-image-has-mastering-display-colour-volume)
  :int (image :pointer))

(cffi:defcfun ("heif_image_get_mastering_display_colour_volume" heif-image-get-mastering-display-colour-volume)
  :void (image :pointer) (out :pointer))

(cffi:defcfun ("heif_image_handle_get_mastering_display_colour_volume" heif-image-handle-get-mastering-display-colour-volume)
  :int (handle :pointer) (out :pointer))

(cffi:defcfun ("heif_image_set_mastering_display_colour_volume" heif-image-set-mastering-display-colour-volume)
  :void (image :pointer) (in :pointer))

(cffi:defcfun ("heif_mastering_display_colour_volume_decode" heif-mastering-display-colour-volume-decode)
  :pointer (in :pointer) (out :pointer))

;;; heif_error.h

(in-package :your-libheif-package)

;;; Enums
(cffi:defcenum heif_error_code
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

(cffi:defcenum heif_suberror_code
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
(cffi:defcstruct heif_error
  (code heif_error_code)
  (subcode heif_suberror_code)
  (message :string))

;;; Global success value (extern const)
(cffi:defcvar ("heif_error_success" heif-error-success)
  (:pointer heif_error))


;;;;;;; heif_brands.h

(defctype heif-brand2 :uint32)

(defun heif-fourcc (a b c d)
  "Convert four ASCII characters into a 32-bit unsigned int (heif_brand2)."
  (logior (ash (char-code a) 24)
	  (ash (char-code b) 16)
	  (ash (char-code c) 8)
	  (char-code d)))

(defconstant +heif-brand2-heic+ (heif-fourcc #\h #\e #\i #\c))
(defconstant +heif-brand2-heix+ (heif-fourcc #\h #\e #\i #\x))
(defconstant +heif-brand2-hevc+ (heif-fourcc #\h #\e #\v #\c))
(defconstant +heif-brand2-hevx+ (heif-fourcc #\h #\e #\v #\x))
(defconstant +heif-brand2-heim+ (heif-fourcc #\h #\e #\i #\m))
(defconstant +heif-brand2-heis+ (heif-fourcc #\h #\e #\i #\s))
(defconstant +heif-brand2-hevm+ (heif-fourcc #\h #\e #\v #\m))
(defconstant +heif-brand2-hevs+ (heif-fourcc #\h #\e #\v #\s))
(defconstant +heif-brand2-avif+ (heif-fourcc #\a #\v #\i #\f))
(defconstant +heif-brand2-avis+ (heif-fourcc #\a #\v #\i #\s))
(defconstant +heif-brand2-mif1+ (heif-fourcc #\m #\i #\f #\1))
(defconstant +heif-brand2-mif2+ (heif-fourcc #\m #\i #\f #\2))
(defconstant +heif-brand2-mif3+ (heif-fourcc #\m #\i #\f #\3))
(defconstant +heif-brand2-msf1+ (heif-fourcc #\m #\s #\f #\1))
(defconstant +heif-brand2-vvic+ (heif-fourcc #\v #\v #\i #\c))
(defconstant +heif-brand2-vvis+ (heif-fourcc #\v #\v #\i #\s))
(defconstant +heif-brand2-evbi+ (heif-fourcc #\e #\v #\b #\i))
(defconstant +heif-brand2-evmi+ (heif-fourcc #\e #\v #\m #\i))
(defconstant +heif-brand2-evbs+ (heif-fourcc #\e #\v #\b #\s))
(defconstant +heif-brand2-evms+ (heif-fourcc #\e #\v #\m #\s))
(defconstant +heif-brand2-jpeg+ (heif-fourcc #\j #\p #\e #\g))
(defconstant +heif-brand2-jpgs+ (heif-fourcc #\j #\p #\g #\s))
(defconstant +heif-brand2-j2ki+ (heif-fourcc #\j #\2 #\k #\i))
(defconstant +heif-brand2-j2is+ (heif-fourcc #\j #\2 #\i #\s))
(defconstant +heif-brand2-miaf+ (heif-fourcc #\m #\i #\a #\f))
(defconstant +heif-brand2-1pic+ (heif-fourcc #\1 #\p #\i #\c))
(defconstant +heif-brand2-avci+ (heif-fourcc #\a #\v #\c #\i))
(defconstant +heif-brand2-avcs+ (heif-fourcc #\a #\v #\c #\s))
(defconstant +heif-brand2-iso8+ (heif-fourcc #\i #\s #\o #\8))


(defcenum heif_filetype_result
  (:heif_filetype_no 0)
  (:heif_filetype_yes_supported 1)
  (:heif_filetype_yes_unsupported 2)
  (:heif_filetype_maybe 3))

(defcstruct heif_error
  (code :int)
  (subcode :int)
  (message :string))

(defcfun ("heif_read_main_brand" heif_read_main_brand)
  heif-brand2
  (data :pointer)
  (len :int))

(defcfun ("heif_read_minor_version_brand" heif_read_minor_version_brand)
  heif-brand2
  (data :pointer)
  (len :int))

(defcfun ("heif_fourcc_to_brand" heif_fourcc_to_brand)
  heif-brand2
  (brand-fourcc :pointer))

(defcfun ("heif_brand_to_fourcc" heif_brand_to_fourcc)
  :void
  (brand heif-brand2)
  (out-fourcc :pointer))

(defcfun ("heif_has_compatible_brand" heif_has_compatible_brand)
  :int
  (data :pointer)
  (len :int)
  (brand-fourcc :pointer))

(defcfun ("heif_list_compatible_brands" heif_list_compatible_brands)
  (:struct heif_error)
  (data :pointer)
  (len :int)
  (out-brands :pointer)
  (out-size :pointer))

(defcfun ("heif_free_list_of_compatible_brands" heif_free_list_of_compatible_brands)
  :void
  (brands-list :pointer))

(defcfun ("heif_get_file_mime_type" heif_get_file_mime_type)
  :string
  (data :pointer)
  (len :int))

(defcfun ("heif_check_filetype" heif_check_filetype)
  heif_filetype_result
  (data :pointer)
  (len :int))

(defcfun ("heif_has_compatible_filetype" heif_has_compatible_filetype)
  (:struct heif_error)
  (data :pointer)
  (len :int))

(defcfun ("heif_check_jpeg_filetype" heif_check_jpeg_filetype)
  :int
  (data :pointer)
  (len :int))


;;;; heif_metadata.h


(in-package :libheif)

(defcenum heif_metadata_compression
  (:off 0)
  (:auto 1)
  (:unknown 2)
  (:deflate 3)
  (:zlib 4)
  (:brotli 5))

;; Querying metadata
(defcfun ("heif_image_handle_get_number_of_metadata_blocks" heif-image-handle-get-number-of-metadata-blocks)
    :int
  (handle :pointer)
  (type-filter :string))

(defcfun ("heif_image_handle_get_list_of_metadata_block_IDs" heif-image-handle-get-list-of-metadata-block-ids)
    :int
  (handle :pointer)
  (type-filter :string)
  (ids :pointer) ; should be (heif_item_id*) = uint32_t*
  (count :int))

(defcfun ("heif_image_handle_get_metadata_type" heif-image-handle-get-metadata-type)
    :string
  (handle :pointer)
  (metadata-id :uint32))

(defcfun ("heif_image_handle_get_metadata_content_type" heif-image-handle-get-metadata-content-type)
    :string
  (handle :pointer)
  (metadata-id :uint32))

(defcfun ("heif_image_handle_get_metadata_size" heif-image-handle-get-metadata-size)
    :size_t
  (handle :pointer)
  (metadata-id :uint32))

(defcfun ("heif_image_handle_get_metadata" heif-image-handle-get-metadata)
    (:struct heif_error)
  (handle :pointer)
  (metadata-id :uint32)
  (out-data :pointer))

(defcfun ("heif_image_handle_get_metadata_item_uri_type" heif-image-handle-get-metadata-item-uri-type)
    :string
  (handle :pointer)
  (metadata-id :uint32))

;; Writing metadata
(defcfun ("heif_context_add_exif_metadata" heif-context-add-exif-metadata)
    (:struct heif_error)
  (ctx :pointer)
  (handle :pointer)
  (data :pointer)
  (size :int))

(defcfun ("heif_context_add_XMP_metadata" heif-context-add-xmp-metadata)
    (:struct heif_error)
  (ctx :pointer)
  (handle :pointer)
  (data :pointer)
  (size :int))

(defcfun ("heif_context_add_XMP_metadata2" heif-context-add-xmp-metadata2)
    (:struct heif_error)
  (ctx :pointer)
  (handle :pointer)
  (data :pointer)
  (size :int)
  (compression heif_metadata_compression))

(defcfun ("heif_context_add_generic_metadata" heif-context-add-generic-metadata)
    (:struct heif_error)
  (ctx :pointer)
  (handle :pointer)
  (data :pointer)
  (size :int)
  (item-type :string)
  (content-type :string))

(defcfun ("heif_context_add_generic_uri_metadata" heif-context-add-generic-uri-metadata)
    (:struct heif_error)
  (ctx :pointer)
  (handle :pointer)
  (data :pointer)
  (size :int)
  (item-uri-type :string)
  (out-item-id :pointer)) ; (heif_item_id*) = (uint32_t*)


;;; heif_aux_images


;; --------------------------------------------------
;; Enums
;; --------------------------------------------------

(defcenum heif_depth_representation_type
  (:uniform-inverse-z 0)
  (:uniform-disparity 1)
  (:uniform-z 2)
  (:nonuniform-disparity 3))

;; --------------------------------------------------
;; Structs
;; --------------------------------------------------

(defcstruct heif_depth_representation_info
  (version :uint8)
  (has-z-near :uint8)
  (has-z-far :uint8)
  (has-d-min :uint8)
  (has-d-max :uint8)
  (z-near :double)
  (z-far :double)
  (d-min :double)
  (d-max :double)
  (depth-representation-type heif_depth_representation_type)
  (disparity-reference-view :uint32)
  (depth-nonlinear-representation-model-size :uint32)
  (depth-nonlinear-representation-model :pointer)) ; uint8_t*

;; --------------------------------------------------
;; Constants
;; --------------------------------------------------

(defconstant +libheif-aux-image-filter-omit-alpha+ (ash 1 1))
(defconstant +libheif-aux-image-filter-omit-depth+ (ash 2 1))

;; --------------------------------------------------
;; Depth image APIs
;; --------------------------------------------------

(defcfun ("heif_image_handle_has_depth_image" heif-image-handle-has-depth-image)
    :int
  (handle :pointer))

(defcfun ("heif_image_handle_get_number_of_depth_images" heif-image-handle-get-number-of-depth-images)
    :int
  (handle :pointer))

(defcfun ("heif_image_handle_get_list_of_depth_image_IDs" heif-image-handle-get-list-of-depth-image-ids)
    :int
  (handle :pointer)
  (ids :pointer) ; (heif_item_id*)
  (count :int))

(defcfun ("heif_image_handle_get_depth_image_handle" heif-image-handle-get-depth-image-handle)
    (:struct heif_error)
  (handle :pointer)
  (depth-image-id :uint32)
  (out-depth-handle :pointer)) ; (heif_image_handle**)

(defcfun ("heif_image_handle_get_depth_image_representation_info" heif-image-handle-get-depth-image-representation-info)
    :int
  (handle :pointer)
  (depth-image-id :uint32)
  (out-info :pointer)) ; (const heif_depth_representation_info**)

(defcfun ("heif_depth_representation_info_free" heif-depth-representation-info-free)
    :void
  (info :pointer))

;; --------------------------------------------------
;; Thumbnail APIs
;; --------------------------------------------------

(defcfun ("heif_image_handle_get_number_of_thumbnails" heif-image-handle-get-number-of-thumbnails)
    :int
  (handle :pointer))

(defcfun ("heif_image_handle_get_list_of_thumbnail_IDs" heif-image-handle-get-list-of-thumbnail-ids)
    :int
  (handle :pointer)
  (ids :pointer)
  (count :int))

(defcfun ("heif_image_handle_get_thumbnail" heif-image-handle-get-thumbnail)
    (:struct heif_error)
  (main-image-handle :pointer)
  (thumbnail-id :uint32)
  (out-thumbnail-handle :pointer))

(defcfun ("heif_context_encode_thumbnail" heif-context-encode-thumbnail)
    (:struct heif_error)
  (ctx :pointer)
  (image :pointer)
  (master-image-handle :pointer)
  (encoder :pointer)
  (options :pointer)
  (bbox-size :int)
  (out-thumb-image-handle :pointer))

(defcfun ("heif_context_assign_thumbnail" heif-context-assign-thumbnail)
    (:struct heif_error)
  (ctx :pointer)
  (master-image :pointer)
  (thumbnail-image :pointer))

;; --------------------------------------------------
;; Auxiliary image APIs
;; --------------------------------------------------

(defcfun ("heif_image_handle_get_number_of_auxiliary_images" heif-image-handle-get-number-of-auxiliary-images)
    :int
  (handle :pointer)
  (aux-filter :int))

(defcfun ("heif_image_handle_get_list_of_auxiliary_image_IDs" heif-image-handle-get-list-of-auxiliary-image-ids)
    :int
  (handle :pointer)
  (aux-filter :int)
  (ids :pointer)
  (count :int))

(defcfun ("heif_image_handle_get_auxiliary_type" heif-image-handle-get-auxiliary-type)
    (:struct heif_error)
  (handle :pointer)
  (out-type :pointer)) ; (const char**)

(defcfun ("heif_image_handle_release_auxiliary_type" heif-image-handle-release-auxiliary-type)
    :void
  (handle :pointer)
  (out-type :pointer))

(defcfun ("heif_image_handle_get_auxiliary_image_handle" heif-image-handle-get-auxiliary-image-handle)
    (:struct heif_error)
  (main-image-handle :pointer)
  (auxiliary-id :uint32)
  (out-auxiliary-handle :pointer))

;;; heif_entity_groups.h

;; --------------------------------------------------
;; Typedefs
;; --------------------------------------------------

(defctype heif_entity_group_id :uint32)

;; --------------------------------------------------
;; Structs
;; --------------------------------------------------

(defcstruct heif_entity_group
  (entity-group-id heif_entity_group_id)
  (entity-group-type :uint32) ; FourCC
  (entities :pointer)         ; heif_item_id*
  (num-entities :uint32))

;; --------------------------------------------------
;; Functions
;; --------------------------------------------------

(defcfun ("heif_context_get_entity_groups" heif-context-get-entity-groups)
    :pointer ; returns (heif_entity_group*)
  (ctx :pointer)
  (type-filter :uint32)      ; 0 disables the filter
  (item-filter :uint32)      ; 0 disables the filter
  (out-num-groups :pointer)) ; int*

(defcfun ("heif_entity_groups_release" heif-entity-groups-release)
    :void
  (groups :pointer)          ; (heif_entity_group*)
  (num-groups :int))


;;; heif_security.h

;; -------------------------
;; Structs
;; -------------------------

(defcstruct heif_security_limits
  (version :uint8)
  ;; Padding 3 bytes after uint8_t version for alignment (optional)
  (max_image_size_pixels :uint64)
  (max_number_of_tiles :uint64)
  (max_bayer_pattern_pixels :uint32)
  (max_items :uint32)
  (max_color_profile_size :uint32)
  (max_memory_block_size :uint64)
  (max_components :uint32)
  (max_iloc_extents_per_item :uint32)
  (max_size_entity_group :uint32)
  (max_children_per_box :uint32)
  (max_total_memory :uint64)
  (max_sample_description_box_entries :uint32)
  (max_sample_group_description_box_entries :uint32))

;; -------------------------
;; Functions
;; -------------------------

(defcfun ("heif_get_global_security_limits" heif-get-global-security-limits)
    (:pointer)) ;; returns const heif_security_limits*

(defcfun ("heif_get_disabled_security_limits" heif-get-disabled-security-limits)
    (:pointer)) ;; returns const heif_security_limits*

(defcfun ("heif_context_get_security_limits" heif-context-get-security-limits)
    (:pointer) ;; returns heif_security_limits*
  (ctx :pointer)) ;; const heif_context*

(defcfun ("heif_context_set_security_limits" heif-context-set-security-limits)
    :int ;; heif_error
  (ctx :pointer)
  (limits :pointer)) ;; const heif_security_limits*

(defcfun ("heif_context_set_maximum_image_size_limit" heif-context-set-maximum-image-size-limit)
    :void
  (ctx :pointer)
  (maximum_width :int))


;;;; heif_encoding.h

(in-package :libheif)

;; ---- enums ----

(defconstant +heif-orientation-normal+ 1)
(defconstant +heif-orientation-flip-horizontally+ 2)
(defconstant +heif-orientation-rotate-180+ 3)
(defconstant +heif-orientation-flip-vertically+ 4)
(defconstant +heif-orientation-rotate-90-cw-then-flip-horizontally+ 5)
(defconstant +heif-orientation-rotate-90-cw+ 6)
(defconstant +heif-orientation-rotate-90-cw-then-flip-vertically+ 7)
(defconstant +heif-orientation-rotate-270-cw+ 8)

;; ---- opaque structs ----

(defcstruct heif_encoder)
(defcstruct heif_encoder_descriptor)
(defcstruct heif_encoder_parameter)
(defcstruct heif_encoding_options)

;; ---- heif_encoding_options struct ----
;; This is a large struct, but let's define main fields relevant for usage:

(defcstruct heif_encoding_options
  (version :uint8)
  (save_alpha_channel :uint8)
  (macOS_compatibility_workaround :uint8)
  (save_two_colr_boxes_when_ICC_and_nclx_available :uint8)
  (output_nclx_profile :pointer) ;; heif_color_profile_nclx*
  (macOS_compatibility_workaround_no_nclx_profile :uint8)
  (image_orientation :int) ;; enum heif_orientation
  ;; skipping heif_color_conversion_options (nested struct) for brevity; use :pointer if needed
  (color_conversion_options :pointer)
  (prefer_uncC_short_form :uint8)
  ;; padding/alignment may be needed here depending on your FFI
)

;; ---- functions ----

(defcfun ("heif_have_encoder_for_format" heif-have-encoder-for-format)
    :int
  (format :int)) ;; enum heif_compression_format

(defcfun ("heif_get_encoder_descriptors" heif-get-encoder-descriptors)
    :int
  (context :pointer)
  (format_filter :int)
  (name_filter :string)
  (out_encoders (:pointer)) ;; pointer to const heif_encoder_descriptor**
  (count :int))

(defcfun ("heif_encoder_descriptor_get_name" heif-encoder-descriptor-get-name)
    :string
  (desc :pointer))

(defcfun ("heif_encoder_descriptor_get_id_name" heif-encoder-descriptor-get-id-name)
    :string
  (desc :pointer))

(defcfun ("heif_encoder_descriptor_get_compression_format" heif-encoder-descriptor-get-compression-format)
    :int
  (desc :pointer))

(defcfun ("heif_encoder_descriptor_supports_lossy_compression" heif-encoder-descriptor-supports-lossy-compression)
    :int
  (desc :pointer))

(defcfun ("heif_encoder_descriptor_supports_lossless_compression" heif-encoder-descriptor-supports-lossless-compression)
    :int
  (desc :pointer))

(defcfun ("heif_context_get_encoder" heif-context-get-encoder)
    :int ;; heif_error
  (context :pointer)
  (desc :pointer)
  (out_encoder (:pointer))) ;; heif_encoder**

(defcfun ("heif_context_get_encoder_for_format" heif-context-get-encoder-for-format)
    :int
  (context :pointer)
  (format :int)
  (out_encoder (:pointer)))

(defcfun ("heif_encoder_release" heif-encoder-release)
    :void
  (encoder :pointer))

(defcfun ("heif_encoder_get_name" heif-encoder-get-name)
    :string
  (encoder :pointer))

;; --- Encoder parameter functions ---

(defcfun ("heif_encoder_set_lossy_quality" heif-encoder-set-lossy-quality)
    :int
  (encoder :pointer)
  (quality :int))

(defcfun ("heif_encoder_set_lossless" heif-encoder-set-lossless)
    :int
  (encoder :pointer)
  (enable :int))

(defcfun ("heif_encoder_set_logging_level" heif-encoder-set-logging-level)
    :int
  (encoder :pointer)
  (level :int))

(defcfun ("heif_encoder_list_parameters" heif-encoder-list-parameters)
    (:pointer) ;; const heif_encoder_parameter* const*
  (encoder :pointer))

(defcfun ("heif_encoder_parameter_get_name" heif-encoder-parameter-get-name)
    :string
  (param :pointer))

(defcfun ("heif_encoder_parameter_get_type" heif-encoder-parameter-get-type)
    :int ;; enum heif_encoder_parameter_type
  (param :pointer))

;; ... add other parameter related functions similarly if needed ...

;; --- Encoding images ---

(defcfun ("heif_encoding_options_alloc" heif-encoding-options-alloc)
    (:pointer)) ;; heif_encoding_options*

(defcfun ("heif_encoding_options_copy" heif-encoding-options-copy)
    :void
  (dst :pointer)
  (src :pointer))

(defcfun ("heif_encoding_options_free" heif-encoding-options-free)
    :void
  (options :pointer))

(defcfun ("heif_context_encode_image" heif-context-encode-image)
    :int ;; heif_error
  (context :pointer)
  (image :pointer)
  (encoder :pointer)
  (options :pointer)
  (out_image_handle (:pointer)))

(defcfun ("heif_context_add_overlay_image" heif-context-add-overlay-image)
    :int
  (context :pointer)
  (image_width :uint32)
  (image_height :uint32)
  (nImages :uint16)
  (image_ids (:pointer)) ;; heif_item_id*
  (offsets (:pointer)) ;; int32_t* (or NULL)
  (background_rgba (:pointer)) ;; uint16_t[4] (or NULL)
  (out_iovl_image_handle (:pointer)))

(defcfun ("heif_context_set_primary_image" heif-context-set-primary-image)
    :int
  (context :pointer)
  (image_handle :pointer))

(defcfun ("heif_context_set_major_brand" heif-context-set-major-brand)
    :void
  (context :pointer)
  (major_brand :uint32)) ;; heif_brand2

(defcfun ("heif_context_add_compatible_brand" heif-context-add-compatible-brand)
    :void
  (context :pointer)
  (compatible_brand :uint32))


;;; heif_decoding.h

(in-package :libheif)

;; --- enums ---

(defconstant +heif-progress-step-total+ 0)
(defconstant +heif-progress-step-load-tile+ 1)

;; --- opaque structs ---

(defcstruct heif_decoding_options
  (version :uint8)
  (ignore_transformations :uint8)
  ;; function pointers, will use :pointer here
  (start_progress :pointer)
  (on_progress :pointer)
  (end_progress :pointer)
  (progress_user_data :pointer)
  (convert_hdr_to_8bit :uint8)
  (strict_decoding :uint8)
  (decoder_id :pointer) ;; const char*
  (color_conversion_options :pointer) ;; heif_color_conversion_options*
  (cancel_decoding :pointer)
  (color_conversion_options_ext :pointer) ;; heif_color_conversion_options_ext*
)

(defcstruct heif_decoder_descriptor)

;; --- functions ---

(defcfun ("heif_context_set_max_decoding_threads" heif-context-set-max-decoding-threads)
    :void
  (ctx :pointer)
  (max_threads :int))

(defcfun ("heif_have_decoder_for_format" heif-have-decoder-for-format)
    :int
  (format :int)) ;; enum heif_compression_format

(defcfun ("heif_decoding_options_alloc" heif-decoding-options-alloc)
    (:pointer)) ;; heif_decoding_options*

(defcfun ("heif_decoding_options_copy" heif-decoding-options-copy)
    :void
  (dst :pointer)
  (src :pointer))

(defcfun ("heif_decoding_options_free" heif-decoding-options-free)
    :void
  (opts :pointer))

(defcfun ("heif_get_decoder_descriptors" heif-get-decoder-descriptors)
    :int
  (format_filter :int)
  (out_decoders (:pointer)) ;; const heif_decoder_descriptor**
  (count :int))

(defcfun ("heif_decoder_descriptor_get_name" heif-decoder-descriptor-get-name)
    :string
  (desc :pointer))

(defcfun ("heif_decoder_descriptor_get_id_name" heif-decoder-descriptor-get-id-name)
    :string
  (desc :pointer))

(defcfun ("heif_decode_image" heif-decode-image)
    :int ;; heif_error
  (in_handle :pointer) ;; const heif_image_handle*
  (out_img (:pointer)) ;; heif_image**
  (colorspace :int) ;; enum heif_colorspace
  (chroma :int) ;; enum heif_chroma
  (options :pointer)) ;; const heif_decoding_options*

;;;;; heif_context.h

;; --- enum heif_compression_format ---

(defconstant +heif-compression-undefined+ 0)
(defconstant +heif-compression-hevc+ 1)
(defconstant +heif-compression-avc+ 2)
(defconstant +heif-compression-jpeg+ 3)
(defconstant +heif-compression-av1+ 4)
(defconstant +heif-compression-vvc+ 5)
(defconstant +heif-compression-evc+ 6)
(defconstant +heif-compression-jpeg2000+ 7)
(defconstant +heif-compression-uncompressed+ 8)
(defconstant +heif-compression-mask+ 9)
(defconstant +heif-compression-htj2k+ 10)

;; --- opaque structs ---

(defcstruct heif_context) ;; Opaque context for HEIF file handling
(defcstruct heif_image_handle) ;; Opaque handle to an image inside context
(defcstruct heif_reader)

;; heif_item_id is uint32_t
(define-foreign-type heif-item-id :uint32)

;; heif_error is usually int or struct; define as int here for simplicity
(define-foreign-type heif-error :int)

;; heif_reading_options is opaque pointer (not defined here)
(defcstruct heif_reading_options)

;; heif_writer struct with function pointer
(defcstruct heif_writer
  (writer_api_version :int)
  (write (:pointer)) ;; function pointer: heif_error (*write)(heif_context*, const void*, size_t, void*)
)

;; --- functions ---

(defcfun ("heif_context_alloc" heif-context-alloc)
  (:pointer)) ;; returns heif_context*

(defcfun ("heif_context_free" heif-context-free)
  :void
  (ctx :pointer))

(defcfun ("heif_context_read_from_file" heif-context-read-from-file)
  heif-error
  (ctx :pointer)
  (filename :string)
  (reading_options :pointer)) ;; can be NULL

(defcfun ("heif_context_read_from_memory" heif-context-read-from-memory)
  heif-error
  (ctx :pointer)
  (mem (:pointer))
  (size :size-t)
  (reading_options :pointer)) ;; deprecated, use without copy

(defcfun ("heif_context_read_from_memory_without_copy" heif-context-read-from-memory-without-copy)
  heif-error
  (ctx :pointer)
  (mem (:pointer))
  (size :size-t)
  (reading_options :pointer))

(defcfun ("heif_context_read_from_reader" heif-context-read-from-reader)
  heif-error
  (ctx :pointer)
  (reader (:pointer)) ;; heif_reader*
  (userdata :pointer)
  (reading_options :pointer))

(defcfun ("heif_context_get_number_of_top_level_images" heif-context-get-number-of-top-level-images)
  :int
  (ctx :pointer))

(defcfun ("heif_context_is_top_level_image_ID" heif-context-is-top-level-image-ID)
  :int ;; bool as int
  (ctx :pointer)
  (id heif-item-id))

(defcfun ("heif_context_get_list_of_top_level_image_IDs" heif-context-get-list-of-top-level-image-IDs)
  :int ;; returns number of IDs filled
  (ctx :pointer)
  (ID_array (:pointer)) ;; heif_item_id* array preallocated
  (count :int))

(defcfun ("heif_context_get_primary_image_ID" heif-context-get-primary-image-ID)
  heif-error
  (ctx :pointer)
  (id_ptr (:pointer))) ;; heif_item_id*

(defcfun ("heif_context_get_primary_image_handle" heif-context-get-primary-image-handle)
  heif-error
  (ctx :pointer)
  (out_handle (:pointer))) ;; heif_image_handle**

(defcfun ("heif_context_get_image_handle" heif-context-get-image-handle)
  heif-error
  (ctx :pointer)
  (id heif-item-id)
  (out_handle (:pointer))) ;; heif_image_handle**

(defcfun ("heif_context_debug_dump_boxes_to_file" heif-context-debug-dump-boxes-to-file)
  :void
  (ctx :pointer)
  (fd :int))

(defcfun ("heif_context_write_to_file" heif-context-write-to-file)
  heif-error
  (ctx :pointer)
  (filename :string))

(defcfun ("heif_context_write" heif-context-write)
  heif-error
  (ctx :pointer)
  (writer (:pointer)) ;; heif_writer*
  (userdata :pointer))

;;; heif_image_handle.h

;; --- opaque struct ---

(defcstruct heif_image_handle) ;; opaque pointer

;; heif_item_id is uint32
(define-foreign-type heif-item-id :uint32)

;; heif_error is usually int or struct; use int for now
(define-foreign-type heif-error :int)

;; heif_context opaque pointer
(defcstruct heif_context)

;; --- functions ---

;; void heif_image_handle_release(const heif_image_handle*);
(defcfun ("heif_image_handle_release" heif-image-handle-release)
  :void
  (handle (:pointer)))

;; int heif_image_handle_is_primary_image(const heif_image_handle*);
(defcfun ("heif_image_handle_is_primary_image" heif-image-handle-is-primary-image)
  :int
  (handle (:pointer)))

;; heif_item_id heif_image_handle_get_item_id(const heif_image_handle*);
(defcfun ("heif_image_handle_get_item_id" heif-image-handle-get-item-id)
  heif-item-id
  (handle (:pointer)))

;; int heif_image_handle_get_width(const heif_image_handle*);
(defcfun ("heif_image_handle_get_width" heif-image-handle-get-width)
  :int
  (handle (:pointer)))

;; int heif_image_handle_get_height(const heif_image_handle*);
(defcfun ("heif_image_handle_get_height" heif-image-handle-get-height)
  :int
  (handle (:pointer)))

;; int heif_image_handle_has_alpha_channel(const heif_image_handle*);
(defcfun ("heif_image_handle_has_alpha_channel" heif-image-handle-has-alpha-channel)
  :int
  (handle (:pointer)))

;; int heif_image_handle_is_premultiplied_alpha(const heif_image_handle*);
(defcfun ("heif_image_handle_is_premultiplied_alpha" heif-image-handle-is-premultiplied-alpha)
  :int
  (handle (:pointer)))

;; int heif_image_handle_get_luma_bits_per_pixel(const heif_image_handle*);
(defcfun ("heif_image_handle_get_luma_bits_per_pixel" heif-image-handle-get-luma-bits-per-pixel)
  :int
  (handle (:pointer)))

;; int heif_image_handle_get_chroma_bits_per_pixel(const heif_image_handle*);
(defcfun ("heif_image_handle_get_chroma_bits_per_pixel" heif-image-handle-get-chroma-bits-per-pixel)
  :int
  (handle (:pointer)))

;; heif_error heif_image_handle_get_preferred_decoding_colorspace(const heif_image_handle*,
;;                                                                enum heif_colorspace*,
;;                                                                enum heif_chroma*);
;; You need to define enums heif_colorspace and heif_chroma similarly, here assumed as int.
(defcfun ("heif_image_handle_get_preferred_decoding_colorspace" heif-image-handle-get-preferred-decoding-colorspace)
  heif-error
  (handle (:pointer))
  (out_colorspace (:pointer)) ;; enum heif_colorspace*
  (out_chroma (:pointer)))    ;; enum heif_chroma*

;; int heif_image_handle_get_ispe_width(const heif_image_handle*);
(defcfun ("heif_image_handle_get_ispe_width" heif-image-handle-get-ispe-width)
  :int
  (handle (:pointer)))

;; int heif_image_handle_get_ispe_height(const heif_image_handle*);
(defcfun ("heif_image_handle_get_ispe_height" heif-image-handle-get-ispe-height)
  :int
  (handle (:pointer)))

;; int heif_image_handle_get_pixel_aspect_ratio(const heif_image_handle*, uint32_t* aspect_h, uint32_t* aspect_v);
(defcfun ("heif_image_handle_get_pixel_aspect_ratio" heif-image-handle-get-pixel-aspect-ratio)
  :int
  (handle (:pointer))
  (aspect_h (:pointer)) ;; uint32_t*
  (aspect_v (:pointer))) ;; uint32_t*

;; heif_context* heif_image_handle_get_context(const heif_image_handle*);
(defcfun ("heif_image_handle_get_context" heif-image-handle-get-context)
  (:pointer) ;; heif_context*
  (handle (:pointer)))


;;;; heif_tiling.h


;; Forward declarations (opaque pointers)
(defcstruct heif_encoder)
(defcstruct heif_encoding_options)
(defcstruct heif_image_handle)
(defcstruct heif_image)
(defcstruct heif_context)

;; heif_item_id is uint32
(define-foreign-type heif-item-id :uint32)

;; heif_error is int
(define-foreign-type heif-error :int)

;; heif_colorspace and heif_chroma enums - define as :int for now
(define-foreign-type heif-colorspace :int)
(define-foreign-type heif-chroma :int)

;; Struct heif_image_tiling
(defcstruct heif_image_tiling
  (version :int)
  (num_columns :uint32)
  (num_rows :uint32)
  (tile_width :uint32)
  (tile_height :uint32)
  (image_width :uint32)
  (image_height :uint32)
  (top_offset :uint32)
  (left_offset :uint32)
  (number_of_extra_dimensions :uint8)
  (extra_dimension_size (* 8 :uint32))) ; array of 8 uint32

;; --- functions ---

;; heif_error heif_image_handle_get_image_tiling(const heif_image_handle* handle,
;;                                              int process_image_transformations,
;;                                              struct heif_image_tiling* out_tiling);
(defcfun ("heif_image_handle_get_image_tiling" heif-image-handle-get-image-tiling)
  heif-error
  (handle (:pointer))
  (process_image_transformations :int)
  (out_tiling (:pointer))) ;; pointer to heif_image_tiling struct

;; heif_error heif_image_handle_get_grid_image_tile_id(const heif_image_handle* handle,
;;                                                     int process_image_transformations,
;;                                                     uint32_t tile_x, uint32_t tile_y,
;;                                                     heif_item_id* out_tile_item_id);
(defcfun ("heif_image_handle_get_grid_image_tile_id" heif-image-handle-get-grid-image-tile-id)
  heif-error
  (handle (:pointer))
  (process_image_transformations :int)
  (tile_x :uint32)
  (tile_y :uint32)
  (out_tile_item_id (:pointer))) ;; pointer to uint32

;; heif_error heif_image_handle_decode_image_tile(const heif_image_handle* in_handle,
;;                                                heif_image** out_img,
;;                                                enum heif_colorspace colorspace,
;;                                                enum heif_chroma chroma,
;;                                                const heif_decoding_options* options,
;;                                                uint32_t tile_x, uint32_t tile_y);
(defcfun ("heif_image_handle_decode_image_tile" heif-image-handle-decode-image-tile)
  heif-error
  (in_handle (:pointer))
  (out_img (:pointer)) ;; pointer to heif_image* (pointer to pointer)
  (colorspace heif-colorspace)
  (chroma heif-chroma)
  (options (:pointer)) ;; heif_decoding_options*
  (tile_x :uint32)
  (tile_y :uint32))

;; heif_error heif_context_encode_grid(heif_context* ctx,
;;                                     heif_image** tiles,
;;                                     uint16_t rows,
;;                                     uint16_t columns,
;;                                     heif_encoder* encoder,
;;                                     const heif_encoding_options* input_options,
;;                                     heif_image_handle** out_image_handle);
(defcfun ("heif_context_encode_grid" heif-context-encode-grid)
  heif-error
  (ctx (:pointer))
  (tiles (:pointer)) ;; pointer to array of heif_image*
  (rows :uint16)
  (columns :uint16)
  (encoder (:pointer))
  (input_options (:pointer))
  (out_image_handle (:pointer))) ;; pointer to heif_image_handle*

;; heif_error heif_context_add_grid_image(heif_context* ctx,
;;                                        uint32_t image_width,
;;                                        uint32_t image_height,
;;                                        uint32_t tile_columns,
;;                                        uint32_t tile_rows,
;;                                        const heif_encoding_options* encoding_options,
;;                                        heif_image_handle** out_grid_image_handle);
(defcfun ("heif_context_add_grid_image" heif-context-add-grid-image)
  heif-error
  (ctx (:pointer))
  (image_width :uint32)
  (image_height :uint32)
  (tile_columns :uint32)
  (tile_rows :uint32)
  (encoding_options (:pointer))
  (out_grid_image_handle (:pointer))) ;; pointer to heif_image_handle*

;; heif_error heif_context_add_image_tile(heif_context* ctx,
;;                                        heif_image_handle* tiled_image,
;;                                        uint32_t tile_x, uint32_t tile_y,
;;                                        const heif_image* image,
;;                                        heif_encoder* encoder);
(defcfun ("heif_context_add_image_tile" heif-context-add-image-tile)
  heif-error
  (ctx (:pointer))
  (tiled_image (:pointer))
  (tile_x :uint32)
  (tile_y :uint32)
  (image (:pointer))
  (encoder (:pointer))
)
