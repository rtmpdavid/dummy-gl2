(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils))
(in-package :dummy-gl2)

(defconstant square-2d (make-square nil nil nil t))
(defconstant square-3d (make-square t nil nil t))
(defconstant square-2d-tex (make-square nil nil t t))
(defconstant square-3d-tex (make-square t nil t t))
(defconstant square-2d-cols-tex (make-square nil t t t))
(defconstant square-3d-cols-tex (make-square t t t t))

(defun clear-buffers (&optional &key (color '(0.15 0.1 0.1 1.0)) (buffers (list :depth-buffer :color-buffer)))
  (apply #'gl:clear-color color)
  (apply #'gl:clear buffers))

(defun attrib-pointer-args (attrib layout &optional (normalized nil))
  (list (attrib-size attrib)
	:float
	(if normalized :true :false)
	(layout-size layout)
	(attrib-offset layout attrib)))

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
