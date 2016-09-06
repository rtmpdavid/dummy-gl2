(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils
	#:alexandria
	#:varjo
	#:rtg-math))
(in-package :dummy-gl2)

;; (v-compile '((a :float)) :330
;; 	   :vertex '(((pos :vec3))
;; 		     (values (v! pos 1.0) a))
;; 	   :fragment '(((hmm :float))
;; 		       (labels ((fun ((x :float))
;; 				  (* x x)))
;; 			 (v! 1.0 1.0 hmm (fun

(defvar gl-shaders (make-hash-table))

(defclass gl-shader ()
  ((vertex
    :initform nil
    :initarg  :stage-vert
    :accessor stage-vert)
   (fragment
    :initform nil
    :initarg  :stage-frag
    :accessor stage-frag)
   (gpu-object-valid
    :initform nil
    :accessor shader-object-valid-p)
   (gpu-object
    :initform nil
    :accessor shader-object)))

(defun add-gpu-program (name &key (uniforms nil) (version :330) (vertex nil) (fragment nil) (force-reload nil))
  (restart-case (progn (when (and (not force-reload) (get-gl-shader name))
			 (cerror "Set anyway"
				 (format nil "GLSL program ~a is already defined, do you wish to continue?" name)))
		       (let ((compile-result (v-compile uniforms version :vertex vertex
									 :fragment fragment))
			     (gl-program (gethash name gl-shaders)))
			 (flet ((result-stage (stage)
				  (find stage compile-result :key #'(lambda (s) (slot-value s 'stage-type)))))
			   (if gl-program
			       (setf (stage-frag gl-program) (result-stage :fragment)
				     (stage-vert gl-program) (result-stage :vertex)
				     (shader-object-valid-p gl-program) nil)
			       (setf (gethash name gl-shaders)
				     (make-instance 'gl-shader
						    :stage-vert (result-stage :vertex)
						    :stage-frag (result-stage :fragment)))))))
    (cancel () :report "Get current value"
      (get-gl-shader name))))

(defun get-gl-shader (name)
  (gethash name gl-shaders))

;; (defun set-gpu-program (name uniforms version vertex fragment)
;;   (setf (gethash name gpu-programs)
;; 	(v-compile uniforms version :vertex vertex
;; 				    :fragment fragment)))

(add-gpu-program :trivial :force-reload t
			  :vertex '(((pos3 :vec3))
				    (v! pos3 1.0))
			  :fragment '(()
				      (v! 1.0 1.0 1.0 1.0)))

(add-gpu-program :trivial-color :force-reload t
				:vertex '(((pos3 :vec3)
					   (col3 :vec3))
					  (v! pos3 1.0))
				:fragment '(()
					    (v! 1.0 1.0 1.0 1.0)))

(add-gpu-program :trivial-color-uniform :force-reload t
					:uniforms '((col3 :vec3))
					:vertex '(((pos3 :vec3))
						  (values
						   (v! (- pos3 (v! 0.5  0.5 0.0)) 0.5)
						   (v2! (x (* pos3 20.00)) (y (* pos3 20.0)))))
					:fragment '(((pos :vec2))
						    (let ((red (sin (x pos)))
							  (green (cos (y pos)))
							  (blue (atan (x pos) (y pos))))
						      (v! red green blue 1.0))))

(defun compile-shader (target source)
  (let ((shader (gl:create-shader target)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (if (gl:get-shader shader :compile-status) (list shader nil)
	(let ((compilation-status (gl:get-shader-info-log shader)))
	  (gl:delete-shader shader)
	  (list nil compilation-status)))))

(defun compile-gl-shader-program (gl-shader)
  (when (shader-object gl-shader) (gl:delete-program (shader-object gl-shader)))
  (let ((vertex-shader (compile-shader :vertex-shader (glsl-code (stage-vert gl-shader))))
	(fragment-shader (compile-shader :fragment-shader (glsl-code (stage-frag gl-shader)))))
    (when (or (not (first vertex-shader))
	      (not (first fragment-shader)))
      (error (format nil "Failed to compile shader!~%~@[fragment:~%~a~]~%~@[vertex:~%~a~]"
		     (second fragment-shader) (second vertex-shader))))
    (let ((shader-program (gl:create-program)))
      (gl:attach-shader shader-program (first vertex-shader))
      (gl:attach-shader shader-program (first fragment-shader))
      ;;vertex shader input attrib locations
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
      (compile-gl-shader-program gl-shader))
    (gl:use-program (shader-object gl-shader))))
