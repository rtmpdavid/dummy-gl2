
(asdf:defsystem #:dummy-gl2
  :version "0.0.0"
  :author "David Selivanov"
  :licence "wtfpl"
  :serial t
  :depends-on (:iterate
		:alexandria
		:bordeaux-threads
		:cl-opengl
		:sdl2
		;;:cl-freetype2
		:png
		:obj-parser
		:cl-fad
		:rtmp-utils
		:varjo
		:rtg-math)
  :components ((:file "config")
	       (:file "package")
	       (:module "src"
		:components ((:file "bits")
			     (:file "rect")
			     (:file "math")
			     (:file "gl")
			     (:file "assets")
			     (:file "attrib")
			     (:file "mesh")
			     (:file "mesh-utils")
			     (:file "vertices")
			     (:file "mesh-pack")
			     (:file "texture")
			     (:file "shader-compiler")
			     (:file "shader")
			     (:file "gpu-programs")
			     (:file "framebuffer")
			     (:file "renderer")
			     (:file "window")
			     (:file "dummy-gl2")))
	       (:module "tools"
		:components (;; (:file "sprite")
			     ;; (:file "mesh")
			     ))))
