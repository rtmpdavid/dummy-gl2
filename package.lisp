(in-package :cl-user)

(defpackage dummy-gl2.assets
  (:use :cl
   :cl-fad
   #:dummy-gl2.config
   :rtmp-utils
   #:rtg-math
   :obj-parser)
  (:export :list-assets
	   :load-asset))

(defpackage dummy-gl2.shader
  (:use
   :cl
   #:alexandria
   :varjo
   #:rtg-math)
  (:export use-gl-shader
	   shader-set-uniform))

(defpackage :dummy-gl2
  (:use #:cl
	#:rtg-math
	#:dummy-gl2.shader
	#:dummy-gl2.assets)
  (:export start-main-loop))


