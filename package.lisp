(in-package :cl-user)

(defpackage dummy-gl2.assets
  (:use :cl
   :cl-fad
   #:dummy-gl2.config
   #:rtmp-utils
   #:rtg-math
   :obj-parser)
  (:export :list-assets
   :load-asset))

(defpackage dummy-gl2.bits
  (:use :cl
   :cl-fad
   #:dummy-gl2.config
   #:rtmp-utils
   #:rtg-math
   :obj-parser)
  (:export #:c-sizeof
	   #:c-ptr-offset
	   #:null-pointer
	   #:continuable
	   #:update-swank
	   #:make-simple-array
	   #:set-array
	   #:triangulize
	   #:face-vert
	   #:face-normal
	   #:vert-normal
	   #:calculate-normals
	   #:pi/2
	   #:spi
	   #:spi/2
	   #:push-when-thing))

(defpackage dummy-gl2.attrib
  (:use :cl)
  (:export #:attrib-size
	   #:attrib-offset
	   #:attrib-position
	   #:layout-size))

(defpackage dummy-gl2.base-gl
  (:use #:cl
	#:dummy-gl2.bits
	#:dummy-gl2.attrib)
  (:export
   ;; Base OpenGL object
   #:gl-object
   #:make-gl-object
   #:gl-name
   #:gl-object-validp
   #:gl-object-destructor
   ;; Buffer objects
   #:gl-buffer-object
   #:make-gl-buffer-object
   #:xbo-object
   #:xbo-validp
   #:xbo-bind
   #:xbo-unbind
   #:xbo-data
   #:xbo-free
   #:xbo-alloc
   #:xbo-pointer
   #:xbo-fill
   ;; Vertex arrays
   #:gl-array-object
   #:make-gl-array-object
   #:vao-bind
   #:vao-unbind
   #:vao-validp
   #:vao-vbo
   #:vao-vbo-offset
   #:vao-vbo-length
   #:vao-ebo
   #:vao-ebo-offset
   #:vao-ebo-length
   #:vao-layout))

(defpackage dummy-gl2.shader
  (:use #:dummy-gl2.bits
	#:cl
	#:rtmp-utils
	#:varjo
	#:varjo-lang
	#:rtg-math
	#:dummy-gl2.base-gl
	#:dummy-gl2.attrib)
  (:import-from :alexandria
		:ensure-gethash)
  (:export shader-update-current
	   shader-set-active
	   shader-set-uniform))

(defpackage :dummy-gl2
  (:use #:cl
	#:rtg-math
	#:dummy-gl2.bits
	#:dummy-gl2.base-gl
	#:dummy-gl2.shader
	#:dummy-gl2.assets
	#:dummy-gl2.attrib)
  (:export run))


