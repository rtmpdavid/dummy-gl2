(in-package :dummy-gl2.shader)

(add-shader :trivial
	    :force-reload t    
	    :uniforms '()
	    :vertex '(((pos3 :vec3))
		      (v! 1.0 1.0 1.0 1.0))
	    :fragment '(()
			(v! 1.0 1.0 1.0 1.0)))

(add-shader :trivial-model-color-uniform
	    :force-reload t    
	    :uniforms '((model :mat4)
			(col3 :vec3))
	    :vertex '(((pos3 :vec3))
		      (* model (v! pos3 1.0)))
	    :fragment '(()
			(v! col3 1.0)))

(add-shader :texture-proj-model
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

(add-shader :diffuse
	    :force-reload t    
	    :uniforms '((projection :mat4)
			(model :mat4)
			(view :mat4)
			(color :vec3)
			(normal-matrix :mat4)
			(light-position :vec3)
			(light-color :vec3)
			(ambient :float)
			(spec-strength :float)
			(spec-shiny :float))
	    :vertex '(((pos3 :vec3)
		       (nor3 :vec3))
		      (let ((nor (* normal-matrix (v! nor3 1.0)))
			    (pos (* model (v! pos3 1.0))))
			(values (* projection view pos)
				(v! (x pos) (y pos) (z pos))
				(v! (x nor) (y nor) (z nor)))))
	    :fragment '(((pos :vec3)
			 (normal :vec3))
			(let* ((n-normal (v:normalize normal))
			       (light-direction (v:normalize (- light-position pos)))
			       (diffuse (max (v:dot n-normal light-direction) 0.0))
			       (specular (varjo::pow
					  (max (v:dot (v! 0.0 0.0 -1.0)
					       	      (varjo-lang:reflect
						       light-direction 
						       n-normal))
					       0.0)
					  spec-shiny))
			       (light (+ (* light-color (+ ambient
							   diffuse (* spec-strength
								      specular))))))
			  (v! (* light color) 1.0)
			  )))
