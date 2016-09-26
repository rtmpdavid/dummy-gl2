(in-package :dummy-gl2.shader)

(defvar shaders (make-hash-table))

(defstruct uniform
  (type nil)
  (location nil))

(defstruct (gl-shader (:conc-name shader-)
		      (:include gl-object))
  (stage-vert nil)
  (stage-frag nil)
  (uniforms (make-hash-table))
  (attribs (make-hash-table)))

(defun add-shader (name &key (uniforms nil) (version :330) (vertex nil) (fragment nil) (force-reload nil))
  (restart-case
      (progn
	(when (and (not force-reload) (get-shader name))
	  (cerror "Set anyway"
		  (format nil "GLSL program ~a is already defined, do you wish to continue?" name)))
	(let ((compile-result (translate-shader uniforms version :vertex vertex :fragment fragment))
	      (shader (ensure-gethash name shaders (make-gl-shader))))
	  (setf (shader-stage-frag shader) (result-stage compile-result :fragment)
		(shader-stage-vert shader) (result-stage compile-result :vertex)
		(shader-validp shader) nil
		(shader-attribs shader)
		(loop
		  for arg in (in-args (result-stage compile-result :vertex))
		  collect (intern (car (last arg)) "KEYWORD")))
	  (loop for u in uniforms
		do (setf (gethash (intern (symbol-name (first u)) "KEYWORD")
				  (shader-uniforms (gethash name shaders)))
			 (make-uniform :type (second u))))))
    (cancel () :report "Get current value" (get-shader name))))

(defun get-shader (name)
  (gethash name shaders))

(defun compile-shader (target source)
  (let ((shader (gl:create-shader target)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (if (gl:get-shader shader :compile-status) (list shader nil)
	(let ((compilation-status (gl:get-shader-info-log shader)))
	  (gl:delete-shader shader)
	  (list nil compilation-status)))))

(defun shader-bind-attrib-locations (shader-program attribs)
  (loop for attrib in attribs
	do (gl:bind-attrib-location shader-program
				    (attrib-position attrib)
				    (symbol-name attrib))))

(defun compile-shader-program (shader)
  (when (shader-object shader) (gl:delete-program (shader-object shader)))
  (let ((vertex-shader (compile-shader :vertex-shader (glsl-code (shader-stage-vert shader))))
	(fragment-shader (compile-shader :fragment-shader (glsl-code (shader-stage-frag shader)))))
    (when (or (not (first vertex-shader))
	      (not (first fragment-shader)))
      (error (format nil "Failed to compile shader!~%~@[fragment:~%~a~]~%~@[vertex:~%~a~]"
		     (second fragment-shader) (second vertex-shader))))
    (let ((shader-program (gl:create-program)))
      (print shader-program)
      (gl:attach-shader shader-program (first vertex-shader))
      (gl:attach-shader shader-program (first fragment-shader))
      ;;vertex shader input attrib locations
      (shader-bind-attrib-locations shader-program (shader-attribs shader)) 
      ;;fragment shader output color numbers
      ;;transform feedback output capturing
      ;;program separation
      (gl:link-program shader-program)
      (gl:detach-shader shader-program (first vertex-shader))
      (gl:detach-shader shader-program (first fragment-shader))
      (gl:delete-shader (first vertex-shader))
      (gl:delete-shader (first fragment-shader))
      (when (not (gl:get-program shader-program :link-status))
	(let ((link-log (gl:get-program-info-log shader-program)))
	  (gl:delete-program shader-program)
	  (error (format nil "Failed to link shader program!~%~a"
			 link-log))))
      (setf (shader-object shader) shader-program
	    (shader-validp shader) t))))

(defun shader-attrib-location (shader attrib)
  (when (not (shader-object shader))
    (compile-shader-program shader))
  (gl:get-attrib-location (shader-object shader) (symbol-name attrib)))

(defun use-shader (name)
  (let ((shader (get-shader name)))
    (when (not (shader-validp shader))
      (when (shader-object shader)
	(gl:delete-program (shader-object shader))
	(setf (shader-object shader) nil))
      (compile-shader-program shader))
    (gl:use-program (shader-object shader))))

(defun set-uniform-values (uniform value)  
  (let ((location (uniform-location uniform)))
    (case (uniform-type uniform)
      (:sampler-2d (%gl:uniform-1i location value))
      (:sampler-2d-shadow (%gl:uniform-1i location value))
      (:mat4 (gl:uniform-matrix-4fv location value nil))
      (:vec3 (gl:uniformf location (x value) (y value) (z value)))
      (:float (gl:uniformf location value))
      (t (warn (format nil "Do not yet know how to set ~a" (uniform-type uniform))))))
  )

(defun shader-set-uniform (shader-name uniform-name values)
  (let* ((shader (get-shader shader-name))
	 (uniform (gethash uniform-name (shader-uniforms shader))))
    (if uniform
	(progn
	  (when (not (uniform-location uniform))
	    (setf (uniform-location uniform)
	    	  (gl:get-uniform-location (shader-object shader)
	    				   (varjo::safe-glsl-name-string uniform-name))))
	  (set-uniform-values uniform values))
	(warn (format nil "Shader ~a has no uniform named ~a"
		      shader-name uniform-name)))))

(defun shader-set-texture (progname name tex-num)
  (%gl:uniform-1i (gl:get-uniform-location (shader-object (get-shader progname)) name) tex-num))

(defun shader-set-float (progname name val)
  (%gl:uniform-1f (gl:get-uniform-location (shader-object (get-shader progname)) name) val))


