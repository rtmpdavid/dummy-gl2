(in-package :dummy-gl2.shader)

(defvar shaders (make-hash-table))

(defvar active-shader nil)
(defvar current-shader nil)

(defstruct uniform
  (type nil)
  (location nil)
  (current-value nil)
  (desired-value nil))

(defun uniform-default-value (type)
  (case type
    (:vec2 (v! 0.0 0.0))
    (:vec3 (v! 0.0 0.0 0.0))
    (:vec4 (v! 0.0 0.0 0.0 0.0))
    (:mat3 (m3:0!))
    (:mat4 (m4:0!))
    (:sampler-2d 0)
    (:float 0.0)))

(defstruct (gl-shader (:conc-name shader-)
		      (:include gl-object))
  (stage-vert nil)
  (stage-frag nil)
  (uniforms (make-hash-table))
  (uniforms-updated t)
  (attribs (make-hash-table)))

(defun add-shader (name &key (uniforms nil) (version :330) (vertex nil) (fragment nil) (force-reload nil))
  (restart-case
      (progn
	(when (and (not force-reload) (get-shader name))
	  (cerror "Set anyway"
		  (format nil "GLSL program ~a is already defined, do you wish to continue?" name)))
	(let ((compile-result (translate-shader uniforms version :vertex vertex :fragment fragment))
	      (shader (ensure-gethash name shaders (make-gl-shader))))
	  (if (eq name (first current-shader))
	      (setf current-shader nil))
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
			 (make-uniform :type (second u)
				       :desired-value (uniform-default-value
						       (second u)))))))
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

(defun reset-shader (shader)
  (when (shader-object shader)
    (gl:delete-program (shader-object shader))
    (setf (shader-object shader) nil))
  (compile-shader-program shader))

(defun shader-set-active (name)
  "Sets shader to use in a next draw operation"
  (when (not (eq name (first active-shader)))
    (setf active-shader (list name (get-shader name)))))

(defun update-uniform-value (uniform)
  (let ((location (uniform-location uniform))
	(value (uniform-desired-value uniform)))
    (case (uniform-type uniform)
      (:sampler-2d (%gl:uniform-1i location value))
      (:sampler-2d-shadow (%gl:uniform-1i location value))
      (:mat4 (gl:uniform-matrix-4fv location value nil))
      (:vec3 (gl:uniformf location (x value) (y value) (z value)))
      (:float (gl:uniformf location value))
      (t (warn (format nil "Do not yet know how to set ~a" (uniform-type uniform)))))))

(defun shader-update-uniforms ()
  (when (shader-uniforms-updated (second current-shader))
    (maphash #'(lambda (name uniform)
		 (when (not (eq (uniform-desired-value uniform)
				(uniform-current-value uniform)))
		   ;; (format t "Updating value for uniform ~a in shader ~a~%" name (first current-shader))
		   (when (not (uniform-location uniform))
		     (setf (uniform-location uniform)
			   (gl:get-uniform-location (shader-object (second current-shader))
						    (varjo::safe-glsl-name-string name))))		   
		   (update-uniform-value uniform)
		   (setf (uniform-current-value uniform)
			 (uniform-desired-value uniform))))
	     (shader-uniforms (second current-shader)))))

(defun shader-set-current ()
  (let ((use nil))
    (when (not (eq (first active-shader)
		   (first current-shader)))
      (setf current-shader active-shader
	    use t))
    (when (not (shader-validp (second current-shader)))
      (reset-shader (second current-shader))
      (setf use t))
    (when use
      (gl:use-program (shader-object (second current-shader))))))

(defun shader-update-current ()
  (shader-set-current)
  (shader-update-uniforms))

(defun shader-set-uniform (shader-name uniform-name values)
  (let* ((shader (get-shader shader-name))
	 (uniform (gethash uniform-name (shader-uniforms shader) nil)))   
    (if (not uniform) (warn (format nil "Shader ~a has no uniform named ~a" shader-name uniform-name))
	(when (not (equalp (uniform-desired-value uniform)
			   values))
	  (setf (shader-uniforms-updated shader) t
		(uniform-desired-value uniform) values)))))

(defun clear-shaders ()
  (maphash #'(lambda (name shader)
	       (when (shader-object shader)
		 (gl:delete-program (shader-object shader))
		 (setf (shader-object shader) nil)))
	   shaders)
  (setf current-shader nil))

(defun shader-glsl-source (progname stage)
  (varjo:glsl-code (slot-value
		    (get-shader progname)
		    stage)))



