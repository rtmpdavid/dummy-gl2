(in-package :cl-user)
(defpackage dummy-gl2
  (:use :cl
   :rtmp-utils))
(in-package :dummy-gl2)

(defclass gl-vertices ()
  ((gl-array
    :initform nil
    :accessor verts-array)
   (gl-array-elts
    :initform nil
    :accessor verts-array-elts)
   (gl-array-valid
    :initform nil
    :accessor gl-array-valid-p)
   (length-verts
    :initform 0
    :accessor verts-length)
   (length-elts
    :initform 0
    :accessor verts-length-elts)
   (meshes
    :initform nil
    :accessor verts-meshes)
   (vertex-buffer-object
    :initform nil
    :accessor verts-vbo)
   (element-array-object
    :initform nil
    :accessor verts-ebo)))

(defun remove-mesh (vertices mesh)
  (when (find mesh (verts-meshes vertices))
    (decf (verts-length vertices) (length (mesh-verts mesh)))
    (decf (verts-length-elts vertices) (length (mesh-elts mesh)))
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
    (incf (verts-length-elts vertices) (length (mesh-elts mesh)))
    (push mesh (verts-meshes vertices))))

(defun add-meshes (vertices &rest meshes)
  (dolist (mesh meshes)
    (add-mesh vertices mesh)))

(defun push-gl-array (glarray seq count offset)
  (loop for i from 0
	do (setf (gl:glaref glarray (+ offset i)) (elt seq i))
	repeat count))

(defun alloc-vertices (vertices)
  (format *standard-output* "Allocating vertices.~%")
  (when (verts-array vertices) (gl:free-gl-array (verts-array vertices)))
  (when (verts-array-elts vertices) (gl:free-gl-array (verts-array-elts vertices)))
  (setf (verts-array vertices) (gl:alloc-gl-array :float (verts-length vertices))
	(verts-array-elts vertices) (gl:alloc-gl-array :unsigned-int (verts-length-elts vertices)))
  (loop for mesh in (verts-meshes vertices)
	with index = 0
	with index-elts = 0
	;; copy vertex data
	do (setf (mesh-offset mesh) index)
	   (let ((vert-count (length (mesh-verts mesh))))
	     (push-gl-array (verts-array vertices) (mesh-verts mesh) vert-count index)
	     (incf index vert-count))
	   ;; copy element data
	   (setf (mesh-offset-elts mesh) index-elts)
	   (let ((elt-count (length (mesh-elts mesh))))
	     (push-gl-array (verts-array-elts vertices) (mesh-elts mesh) elt-count index-elts)
	     (incf index-elts elt-count)))
  (setf (gl-array-valid-p vertices) t))

(defun free-vertices (vertices)
  (gl:free-gl-array (verts-array vertices)))

(defun alloc-buffers (vertices)
  (let ((buffers (gl:gen-buffers 2)))
    (setf (verts-vbo vertices) (first buffers)
	  (verts-ebo vertices) (second buffers))))

(defun free-buffers (vertices)
  (gl:delete-buffers
   (loop for buff in (list (verts-vbo vertices) (verts-ebo vertices))
	 if buff
	   collect buff))
  (setf (verts-vbo vertices) nil
	(verts-ebo vertices) nil))
