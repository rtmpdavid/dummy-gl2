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

(defpackage dummy-gl2.base-gl
  (:use :cl)
  (:export #:gl-object
	   #:make-gl-object
	   #:gl-name
	   #:gl-object-validp
	   #:gl-object-destructor))

(defpackage dummy-gl2.attrib
  (:use :cl)
  (:export #:attrib-size
	   #:attrib-offset
	   #:attrib-position
	   #:layout-size))

(defpackage dummy-gl2.shader
  (:use
   :cl
   #:alexandria
   #:rtmp-utils
   :varjo
   #:rtg-math
   #:dummy-gl2.base-gl
   #:dummy-gl2.attrib)
  (:export use-shader
	   shader-set-uniform))

(defpackage :dummy-gl2
  (:use #:cl
	#:rtg-math
	#:dummy-gl2.shader
	#:dummy-gl2.assets
	#:dummy-gl2.attrib)
  (:export start-main-loop))


