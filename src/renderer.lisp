(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils))
(in-package :dummy-gl2)

(defvar static-meshes (make-instance 'gl-vertices))

(defmacro define-and-add-mesh (name &body body)
  `(progn (defvar ,name ,@body)
	  (add-meshes static-meshes ,name)))

(define-and-add-mesh square-2d (make-square nil nil nil))
(define-and-add-mesh square-3d (make-square t nil nil))
(define-and-add-mesh square-2d-tex (make-square nil nil t))
(define-and-add-mesh square-3d-tex (make-square t nil t))
(define-and-add-mesh square-2d-cols-tex (make-square nil t t))
(define-and-add-mesh square-3d-cols-tex (make-square t t t))

(defun init-renderer ()
  (alloc-vertices static-meshes))

(defun clear-buffers (&optional &key (color '(0.15 0.1 0.1 1.0)) (buffers (list :depth-buffer :color-buffer)))
  (apply #'gl:clear-color color)
  (apply #'gl:clear buffers))

(defun bind-vbo-buffer (vertices)
  (when (not (verts-vbo vertices))
    (alloc-buffers vertices))
  (gl:bind-buffer :array-buffer (verts-vbo vertices)))

(defun bind-ebo-buffer (vertices)
  (when (not (verts-vbo vertices))
    (alloc-buffers vertices))
  (gl:bind-buffer :element-array-buffer (verts-vbo vertices)))

(defun bind-vbo-data (vertices &optional (usage :static-draw))
  (bind-vbo-buffer vertices)
  (gl:buffer-data :array-buffer usage (verts-array vertices)))

(defun bind-ebo-data (vertices &optional (usage :static-draw))
  (bind-ebo-buffer vertices)
  (gl:buffer-data :element-array-buffer usage (verts-array-elts vertices)))


(defun bind-vao (mesh)
  (let ((vao ;; (gl:gen-vertex-array)
	     )
	(vec-array ;; (gl:alloc-gl-array :float (length (mesh-verts)))
		   )
	(elt-array ;; (gl:alloc-gl-array :uint32 (length (mesh--elts)))
	  ))
    (loop with layout = (mesh-layout mesh)
	  for attrib in layout
	  do (print (attrib-pointer-args attrib layout)))
    ))

(defun draw-mesh (mesh)
  (let ((gl-array (mesh-gl-array mesh)))
    (when (not gl-array) (error "Mesh does not have gl array set"))
    (when (not (gl-array-valid-p gl-array)) (alloc-vertices gl-array))
    (apply #'gl:vertex-attrib-pointer (append (list 0) (attrib-pointer-args :pos3 square-3d)))
    (%gl:draw-elements :triangles
		       (length (mesh-elts mesh))
		       :unsigned-int
		       (mesh-offset-elts mesh))))
