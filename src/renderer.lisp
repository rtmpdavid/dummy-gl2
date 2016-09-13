(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils))
(in-package :dummy-gl2)

(defvar static-meshes (make-instance 'gl-vertices))

(defmacro define-and-add-mesh (name &body body)
  `(progn
     (let ((new-mesh ,@body))
       (if (boundp ',name)
	   (remove-mesh static-meshes ,name)
	   (defvar ,name))
       (setf ,name new-mesh)
       (add-meshes static-meshes ,name))))

(define-and-add-mesh square-3d-tex (make-square t nil t))
(define-and-add-mesh circle (make-n-gon 50))

(defun init-renderer ()
  )

(defun clear-buffers (&optional &key (color '(0.15 0.1 0.1 1.0)) (buffers (list :depth-buffer :color-buffer)))
  (apply #'gl:clear-color color)
  (apply #'gl:clear buffers))

(defstruct opengl-state
  (mesh nil :type 'mesh)
  (shader nil :type 'gl-shader)
  )

(defvar current-mesh nil)

(defvar polygon-count 0)
(defvar polygon-count-last 0)

(defun draw-mesh (mesh)
  (declare (inline))
  (let ((gl-array (mesh-gl-array mesh)))
    (if (not gl-array) (warn  "Mesh does not have gl array set")
	(progn 
	  (when (not (gl-array-valid-p gl-array)) (alloc-vertices gl-array))
	  (when (not (mesh-vao-valid-p mesh))
	    (bind-vao mesh)
	    (setf current-mesh nil))
	  (when (not (eq current-mesh mesh))
	    (gl:bind-vertex-array (mesh-vao mesh))
	    (setf current-mesh mesh))
	  (incf polygon-count (length (mesh-elts mesh)))
	  ;; (%gl:draw-range-elements  :triangles
	  ;; 			      (mesh-offset-elts mesh)
	  ;; 			      (verts-length-elts gl-array)
	  ;; 			      (length (mesh-elts mesh))
	  ;; 			      :unsigned-int 0)
	  (%gl:draw-elements :triangles (length (mesh-elts mesh))
			     :unsigned-int
			     (* (mesh-offset-elts mesh) 4))))))

(defun flush-renderer ()
  (setf polygon-count-last polygon-count
	polygon-count 0)
  (sdl2:gl-swap-window *window*))
