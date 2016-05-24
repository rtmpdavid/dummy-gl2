
(asdf:defsystem dummy-gl2
  ;; :version "0.0.0"
  ;; :author "rtmpdavid"
  ;; :licence "wtfpl"
  :serial t
  :depends-on (:iterate
		:alexandria		
		:cl-opengl
		:sdl2
		;;:cl-freetype2
		:png
		:obj-parser
		:cl-fad
		:rtmp-utils)
  :components ((:file "config")
	       (:module "src"
			:components
			((:file "dummy-gl2")
			 (:module "engines"
			  :components ((:file "engine-base")
				       (:file "engine-sdl2-gl"
					:depends-on ("engine-base")))
			  :depends-on ("scene" "renderer"))
			 (:module "scene"
			  :components ((:file "scene")))
			 (:module "renderer"
			  :components ((:file "renderer-base")))
			 (:file "assets")))))
