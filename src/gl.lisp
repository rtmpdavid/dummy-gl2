(in-package :cl-user)
(defpackage dummy-gl2
  (:use :cl
   :rtmp-utils))
(in-package :dummy-gl2)

(defvar gl-vaos (make-hash-table))
;; (defvar gl-vertices (make-hash-table))
(defvar gl-elements (make-hash-table))
(defvar gl-textures (make-hash-table))


(defun invalidate-shader (name)
  ;; (when (gethash name gl-shaders)
  ;;   (setf (gethash name gl-shaders) nil))
  )

(defclass gl-vertices ()
  ((gl-array
    :initform nil
    :accessor verts-array)
   (gl-array-valid
    :initform nil
    :accessor gl-array-valid-p)
   (length
    :initform 0
    :accessor verts-length)
   (meshes
    :initform nil
    :accessor verts-meshes)))

(defun remove-mesh (vertices mesh)
  (when (find mesh (verts-meshes vertices))
    (decf (verts-length vertices) (length (mesh-verts mesh)))
    (setf (verts-meshes vertices)
	  (delete mesh (verts-meshes vertices))
	  (mesh-gl-array mesh) nil
	  (mesh-offset mesh) 0
	  (gl-array-valid-p vertices) nil)))

(defun add-mesh (vertices mesh)
  (unless (eq (mesh-gl-array mesh) vertices)
    (when (mesh-gl-array mesh) (remove-mesh (mesh-gl-array mesh) mesh))
    (setf (mesh-gl-array mesh) vertices
	  (gl-array-valid-p vertices) nil)
    (incf (verts-length vertices) (length (mesh-verts mesh)))
    (push mesh (verts-meshes vertices))))

(defun add-meshes (vertices &rest meshes)
  (dolist (mesh meshes)
    (add-mesh vertices mesh)))

(defun alloc-vertices (vertices)
  (when (verts-array vertices)
    (gl:free-gl-array (verts-array vertices)))
  (setf (verts-array vertices) (gl:alloc-gl-array :float (verts-length vertices)))
  (loop for mesh in (verts-meshes vertices)
	with index = 0
	do (setf (mesh-offset mesh) index)
	   (loop for element across (mesh-verts mesh)
		 for i from 0 
		 do (setf (gl:glaref (verts-array vertices) index)
			  (aref (mesh-verts mesh) i))
		    (incf index)))
  (setf (gl-array-valid-p vertices) t))

(defun free-vertices (vertices)
  (gl:free-gl-array (verts-array vertices)))

(defclass gl-shader ()
  ((gpu-program
    :initform nil
    :accessor shader-progs)
   (gpu-object
    :initform nil
    :accessor shader-objects)))
 

