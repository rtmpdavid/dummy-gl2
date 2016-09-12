
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
			     (:file "assets")
			     (:file "mesh")
			     (:file "gl")
			     (:file "texture")
			     (:file "shader-compiler")
			     (:file "shader")
			     (:file "renderer")
			     (:file "dummy-gl2")))
	       (:module "tools"
		:components (;; (:file "sprite")
			     ;; (:file "mesh")
			     ))))
