(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils))
(in-package :dummy-gl2)

(defvar static-meshes (make-instance 'gl-vertices))

(defmacro define-and-add-mesh (name &body body)
  `(progn
     (defvar ,name ,@body)
     (add-meshes static-meshes ,name)))

(define-and-add-mesh square-2d (make-square nil nil nil))
(define-and-add-mesh square-3d (make-square t nil nil))
(define-and-add-mesh square-2d-tex (make-square nil nil t))
(define-and-add-mesh square-3d-tex (make-square t nil t))
(define-and-add-mesh square-2d-cols-tex (make-square nil t t))
(define-and-add-mesh square-3d-cols-tex (make-square t t t))

(define-and-add-mesh random-verts-0 (make-random-mesh 100))
(define-and-add-mesh random-verts-1 (make-random-mesh 1000))
(define-and-add-mesh random-verts-2 (make-random-mesh 10000))
(define-and-add-mesh random-verts-3 (make-random-mesh 50000))
(define-and-add-mesh random-verts-4 (make-random-mesh 100000))
(define-and-add-mesh random-verts-5 (make-random-mesh 500000))

(defun init-renderer ()
  )

(defun clear-buffers (&optional &key (color '(0.15 0.1 0.1 1.0)) (buffers (list :depth-buffer :color-buffer)))
  (apply #'gl:clear-color color)
  (apply #'gl:clear buffers))

(defvar current-mesh nil)
(defvar current-vbo nil)

(defvar polygon-count 0)
(defvar polygon-count-last 0)

(defun draw-mesh (mesh)
  (declare (inline))
  (let ((gl-array (mesh-gl-array mesh)))
    (when (not gl-array) (error "Mesh does not have gl array set"))
    (when (not (gl-array-valid-p gl-array)) (alloc-vertices gl-array))
    (when (not (mesh-vao mesh))
      (bind-vao mesh))
    (gl:bind-vertex-array (mesh-vao mesh))
    (incf polygon-count (/ (length (mesh-elts mesh)) 3))
    (%gl:draw-range-elements  :points
    			      (mesh-offset-elts mesh)
    			      (verts-length-elts gl-array)
    			      (length (mesh-elts mesh))
    			      :unsigned-int
    			      (cffi:null-pointer)
    			      ;; (slot-value (verts-array-elts gl-array) 'gl::pointer)
    			      )
    ;; (%gl:draw-elements :triangles
    ;; 		       (length (mesh-elts mesh))
    ;; 		       :unsigned-int
    ;; 		       (cffi:incf-pointer
    ;; 			(slot-value (verts-array-elts gl-array) 'gl::pointer)
    ;; 			(* 4 (mesh-offset-elts mesh))
    ;; 			)
		       
    ;; 		       ;; (* 4 (mesh-offset-elts mesh))
    ;; 		       )
    ;; (%gl:draw-elements :triangles
    ;; 		       (length (mesh-elts mesh))
    ;; 		       :unsigned-int
    ;; 		       (* 4 (mesh-offset-elts mesh))
    ;; 		       )

    ))

(defun flush-renderer ()
  (setf polygon-count-last polygon-count
	polygon-count 0)
  (sdl2:gl-swap-window *window*))
