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
;; 			 (v! 1.0 1.0 hmm (fun a)))))

(defvar gpu-programs (make-hash-table))

(defun program-stage (program stage)
  (loop for p in program
	if (eq (slot-value p 'stage-type) stage)
	  do (return p)
	finally (return nil)))

(defun get-gpu-program (name)
  (gethash name gpu-programs))

(defun set-gpu-program (name uniforms version vertex fragment)
  (setf (gethash name gpu-programs)
	(v-compile uniforms version :vertex vertex
				    :fragment fragment)))

(defun make-gpu-program (name &key (uniforms nil) (version :330) (vertex nil) (fragment nil) (force-reload nil))
  (restart-case (progn 
		  (when (and (gethash name gpu-programs) (not force-reload))
		    (cerror "Set anyway"
			    (format nil "GLSL program ~a is already defined, do you wish to continue?" name)))
		  (invalidate-shader name)
		  (set-gpu-program name uniforms version vertex fragment))
    (cancel () :report "Get current value"
      (get-gpu-program name))))

(make-gpu-program :trivial :force-reload t
			   :vertex '(((pos :vec3))
				     (v! pos 1.0))
			   :fragment '(()
				       (v! 1.0 1.0 1.0 1.0)))

(make-gpu-program :trivial-color :force-reload t
				 :vertex '(((pos :vec3)
					    (col :vec3))
					   (v! pos 1.0))
				 :fragment '(()
					     (v! 1.0 1.0 1.0 1.0)))

;; (make-gpu-program :trivial :force-reload t
;; 		  :vertex '(((pos :vec3))
;; 			    (v! pos 1.0))
;; 		  :fragment '(()
;; 			      (v! 1.0 1.0 1.0 1.0)))
