(in-package :cl-user)
(defpackage dummy-gl2.shader
  (:use #:cl
	#:rtmp-utils
	#:alexandria
	#:varjo
	#:rtg-math))
(in-package :dummy-gl2.shader)

(defparameter attrib-positions
  (list :pos2 0
	:pos3 0
	:col3 1
	:col4 1
	:tex2 2
	:nor2 3
	:nor3 3))

(defun attrib-position (attrib)
  (loop for key in attrib-positions by #'cddr
	and value in (cdr attrib-positions) by #'cddr
	if (string= (symbol-name attrib) (symbol-name key))
	  do (return value)))

(defvar gl-shaders (make-hash-table))

(defstruct uniform
  (type nil)
  (location nil))

(defclass gl-shader ()
  ((vertex
    :initform nil
    :initarg  :stage-vert
    :accessor shader-stage-vert)
   (fragment
    :initform nil
    :initarg  :stage-frag
    :accessor shader-stage-frag)
   (gpu-object-valid
    :initform nil
    :accessor shader-object-valid-p)
   (gpu-object
    :initform nil
    :accessor shader-object)
   (uniforms
    :initform (make-hash-table)
    :accessor shader-uniforms)))

(defun add-gpu-program (name &key (uniforms nil) (version :330) (vertex nil) (fragment nil) (force-reload nil))
  (restart-case
      (progn
	(when (and (not force-reload) (get-gl-shader name))
	  (cerror "Set anyway"
		  (format nil "GLSL program ~a is already defined, do you wish to continue?" name)))
	(let ((compile-result (translate-shader uniforms version :vertex vertex
								 :fragment fragment))
	      (gl-program (gethash name gl-shaders)))
	  (flet ((result-stage (stage)
		   (find stage compile-result :key #'(lambda (s) (slot-value s 'stage-type)))))			   
	    (if gl-program
		(setf (shader-stage-frag gl-program) (result-stage :fragment)
		      (shader-stage-vert gl-program) (result-stage :vertex)
		      (shader-object-valid-p gl-program) nil)
		(setf (gethash name gl-shaders)
		      (make-instance 'gl-shader
				     :stage-vert (result-stage :vertex)
				     :stage-frag (result-stage :fragment))))
	    (loop for u in uniforms
		  do (setf (gethash (intern (symbol-name (first u)) "KEYWORD") (shader-uniforms (gethash name gl-shaders)))
			   (make-uniform :type (second u)))))))
    (cancel () :report "Get current value"
      (get-gl-shader name))))

(defun get-gl-shader (name)
  (gethash name gl-shaders))

(defun compile-shader (target source)
  (let ((shader (gl:create-shader target)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (if (gl:get-shader shader :compile-status) (list shader nil)
	(let ((compilation-status (gl:get-shader-info-log shader)))
	  (gl:delete-shader shader)
	  (list nil compilation-status)))))

(defun shader-bind-attrib-locations (shader-program stage-vert)
  (let ((in-args (in-args stage-vert)))
    (loop for arg in in-args
	  do (gl:bind-attrib-location shader-program (attrib-position (car arg)) (car (last arg))))))

(defun compile-gl-shader-program (gl-shader)
  (when (shader-object gl-shader) (gl:delete-program (shader-object gl-shader)))
  (let ((vertex-shader (compile-shader :vertex-shader (glsl-code (shader-stage-vert gl-shader))))
	(fragment-shader (compile-shader :fragment-shader (glsl-code (shader-stage-frag gl-shader)))))
    (when (or (not (first vertex-shader))
	      (not (first fragment-shader)))
      (error (format nil "Failed to compile shader!~%~@[fragment:~%~a~]~%~@[vertex:~%~a~]"
		     (second fragment-shader) (second vertex-shader))))
    (let ((shader-program (gl:create-program)))
      (gl:attach-shader shader-program (first vertex-shader))
      (gl:attach-shader shader-program (first fragment-shader))
      ;;vertex shader input attrib locations
      (shader-bind-attrib-locations shader-program (shader-stage-vert gl-shader)) 
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
      (setf (shader-object gl-shader) shader-program
	    (shader-object-valid-p gl-shader) t))))

(defun gl-shader-attrib-location (gl-shader attrib)
  (when (not (shader-object gl-shader))
    (compile-gl-shader-program gl-shader))
  (gl:get-attrib-location (shader-object gl-shader) (symbol-name attrib)))

(defun use-gl-shader (name)
  (let ((gl-shader (get-gl-shader name)))
    (when (not (shader-object-valid-p gl-shader))
      (when (shader-object gl-shader)
	(gl:delete-program (shader-object gl-shader)))
      (compile-gl-shader-program gl-shader))
    (gl:use-program (shader-object gl-shader))))

(defun set-uniform-values (uniform value)  
  (let ((location (uniform-location uniform)))
   (case (uniform-type uniform)
     (:sampler-2d (%gl:uniform-1i location value))
     (:mat4 (gl:uniform-matrix-4fv location value nil)
      )
     (t (warn (format nil "Do not yet know how to set ~a" (uniform-type uniform))))))
  )

(defun shader-set-uniform (shader-name uniform-name values)
  (let* ((shader (get-gl-shader shader-name))
	 (uniform (gethash uniform-name (shader-uniforms shader))))
    (if uniform
	(progn
	  (when (not (uniform-location uniform))
	    (setf (uniform-location uniform)
	    	  (gl:get-uniform-location (shader-object shader)
	    				   (varjo::safe-glsl-name-string uniform-name))))
	  (set-uniform-values uniform values))
	;; (warn (format nil "Shader ~a has no uniform named ~a"
	;; 	      shader-name uniform-name))
	)))

(defun shader-set-texture (progname name tex-num)
  (%gl:uniform-1i (gl:get-uniform-location (shader-object (get-gl-shader progname)) name) tex-num))

(defun shader-set-float (progname name val)
  (%gl:uniform-1f (gl:get-uniform-location (shader-object (get-gl-shader progname)) name) val))

(add-gpu-program :trivial-texture :force-reload t
				  :uniforms '((texture-1 :sampler-2d))
				  :vertex '(((pos3 :vec3)
					     (tex2 :vec2))
					    (values
					     (v! pos3 1.0)
					     tex2))
				  :fragment '(((tex2 :vec2))
					      (+ (varjo::texture texture-1 tex2))))

(add-gpu-program :trivial-texture-model
		 :force-reload t    
		 :uniforms '((texture-1 :sampler-2d)
			     (model :mat4))
		 :vertex '(((pos3 :vec3)
			    (tex2 :vec2))
			   (values (* model
				    (v! pos3 1.0))
			    tex2))
		 :fragment '(((tex2 :vec2))
			     (+ (varjo::texture texture-1 tex2))))





