(in-package :dummy-gl2)

(defvar static-meshes (make-instance 'gl-mesh-pack))

(defmacro define-and-add-mesh (name &body body)
  `(progn
     (let ((new-mesh ,@body))
       (if (boundp ',name)
	   (mesh-pack-remove static-meshes ,name)
	   (defvar ,name))
       (setf ,name new-mesh)
       (mesh-pack-add static-meshes ,name))))

;; (define-and-add-mesh cube-3d (mesh-make-from-asset (dummy-gl2.assets:load-asset "meshes/cube.obj")))
(define-and-add-mesh cube-2 (mesh-make-from-asset (dummy-gl2.assets:load-asset "meshes/cube2.obj")))
(define-and-add-mesh square-3d-tex (mesh-make-square t nil t))
(define-and-add-mesh circle (mesh-make-n-gon 100))
(define-and-add-mesh teapot (mesh-make-from-asset (dummy-gl2.assets:load-asset "meshes/teapot.obj") t))
(define-and-add-mesh urth (mesh-make-from-asset (dummy-gl2.assets:load-asset "meshes/urth.obj")))

(defun init-renderer ()
  )

(defun clear-buffers (&optional &key (color '(0.15 0.1 0.1 1.0)) (buffers (list :depth-buffer :color-buffer)))
  (apply #'gl:clear-color color)
  (apply #'gl:clear buffers))

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
	    (vao-setup mesh)
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
			     (* (mesh-offset-elts mesh)
				(c-sizeof :float)))))))

(defun draw-mesh-2 (mesh)
  (let ((pack (mesh-pack mesh)))
    (when (not (mesh-pack-validp pack))
      (mesh-pack-fill-buffers pack))
    (vao-bind (mesh-vao mesh))
    (incf polygon-count (/ (length (mesh-elts mesh)) 3))
    (%gl:draw-elements :triangles (length (mesh-elts mesh))
    		       :unsigned-int
    		       (* (mesh-offset-elts mesh)
    			  (c-sizeof :float)))
    (vao-unbind)))

(defvar frame-count 0)
(defvar old-time 0)

(defun flush-renderer ()
  (incf frame-count)
  (let ((time (get-internal-real-time)))
    (when (>= (/ (- time old-time) cl::internal-time-units-per-second) 1)
      (format t "fc: ~a polycount: ~a average per poly: ~,2f ms~%"
	      frame-count polygon-count
	      (if (zerop polygon-count) "N/A"
		  (* 1000 (/ (float (- time old-time)) polygon-count))))
      (setf frame-count 0
  	    old-time time)))
  (setf polygon-count 0)
  (window-swap *window*)
  (finish-output))
