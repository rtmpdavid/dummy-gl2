(in-package :cl-user)
(defpackage dummy-gl2.shader
  (:use :varjo
   :rtg-math))
(in-package :dummy-gl2.shader)

(add-gpu-program :trivial
		 :force-reload t    
		 :uniforms '()
		 :vertex '(((pos3 :vec3))
			   (v! pos3 1.0))
		 :fragment '(()
			     (v! 1.0 1.0 1.0 1.0)))

(add-gpu-program :trivial-model-color-uniform
		 :force-reload t    
		 :uniforms '((model :mat4)
			     (col3 :vec3))
		 :vertex '(((pos3 :vec3))
			   (* model (v! pos3 1.0)))
		 :fragment '(()
			     (v! col3 1.0)))

(add-gpu-program :texture-proj-model
		 :force-reload t    
		 :uniforms '((texture-1 :sampler-2d)
			     (projection :mat4)
			     (model :mat4))
		 :vertex '(((pos3 :vec3)
			    (tex2 :vec2))
			   (let ((out pos3))
			     (values (* projection
					model
					(v! pos3 1.0))
				     tex2)))
		 :fragment '(((tex2 :vec2))
			     (varjo::texture texture-1 tex2)))

(add-gpu-program :diffuse
		 :force-reload t    
		 :uniforms '((projection :mat4)
			     (model :mat4)
			     (normal-matrix :mat4)
			     (light-posistion :vec3)
			     (ambient :float))
		 :vertex '(((pos3 :vec3)
			    (nor3 :vec3))
			   (let ((nor (* normal-matrix (v! nor3 1.0)))
				 (pos (* model (v! pos3 1.0))))
		0	     (values (* projection pos)
				     (v! (x pos) (y pos) (z pos))
				     (v! (x nor) (y nor) (z nor)))))
		 :fragment '(((pos :vec3)
			      (normal :vec3))
			     (let* ((light-direction (v:normalize (- light-posistion
								     pos)))
				    (diff (+ ambient
					     (* 0.5
					      (max (v:dot (v:normalize normal)
							  (v:normalize light-direction))
						   0.0)))))
			       (v! diff diff diff 1.0)
			       )))