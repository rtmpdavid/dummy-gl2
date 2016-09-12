(defpackage :dummy-gl2.shader
  (:use #:cl
	#:rtg-math
	#:varjo)
  (:export :use-gl-shader
	   :shader-set-uniform
	   :attrib-position))

(defpackage :dummy-gl2
  (:use #:cl
	#:rtg-math
	#:dummy-gl2.shader))


