(in-package :dummy-gl2)

(defgeneric mesh-pack-add (mesh-pack mesh))
(defgeneric mesh-pack-remove (mesh-pack mesh))
(defgeneric mesh-pack-modify (mesh-pack offset vertices))
(defgeneric mesh-pack-alloc-vertices (mesh-pack))

(defstruct  (gl-mesh-pack (:conc-name mesh-pack-))
  (validp nil)
  (vbo (make-gl-buffer-object :target :array-buffer :usage :static-draw)
   :type gl-buffer-object)
  (ebo (make-gl-buffer-object :target :element-array-buffer :usage :static-draw)
   :type gl-buffer-object)
  (length-verts 0 :type fixnum)
  (length-elts 0 :type fixnum)
  (meshes (make-array 0 :element-type 'mesh :adjustable t :fill-pointer t)
   :type (vector mesh)))

(defmethod mesh-pack-add (mesh-pack mesh)
  (when (mesh-pack mesh)
    (mesh-pack-remove (mesh-pack mesh) mesh))
  (vector-push-extend mesh (mesh-pack-meshes mesh-pack))
  (incf (mesh-pack-length-verts mesh-pack) (length (mesh-verts mesh)))
  (incf (mesh-pack-length-elts mesh-pack) (length (mesh-elts mesh)))
  (setf (mesh-pack-validp mesh-pack) nil
	(mesh-pack mesh) mesh-pack
	(xbo-validp (mesh-pack-vbo mesh-pack)) nil
	(xbo-validp (mesh-pack-ebo mesh-pack)) nil
	(vao-vbo (mesh-vao mesh)) (mesh-pack-vbo mesh-pack)
	(vao-ebo (mesh-vao mesh)) (mesh-pack-ebo mesh-pack)
	(vao-validp (mesh-vao mesh)) nil))

(defmethod mesh-pack-remove (mesh-pack mesh)
  (when (find mesh (mesh-pack-meshes mesh-pack))
    (let ((vao (mesh-vao mesh)))
      (decf (mesh-pack-length-verts mesh-pack) (length (mesh-verts mesh)))
      (decf (mesh-pack-length-elts mesh-pack) (length (mesh-elts mesh)))
      (setf (mesh-pack-validp mesh-pack) nil
	    (mesh-pack-meshes mesh-pack) (delete mesh (mesh-pack-meshes mesh-pack))
	    (mesh-pack mesh) nil
	    (xbo-validp (mesh-pack-vbo mesh-pack)) nil 
	    (xbo-validp (mesh-pack-ebo mesh-pack)) nil
	    (vao-vbo vao) nil 
	    (vao-ebo vao) nil	    
	    (vao-validp vao) nil))))

(defun mesh-pack-fill-vbo (pack)
  (let ((vbo (mesh-pack-vbo pack)))
    (xbo-alloc vbo :float (mesh-pack-length-verts pack))
    (loop for mesh across (mesh-pack-meshes pack)
	  with offset = 0
	  do (xbo-fill vbo (mesh-verts mesh) :offset offset)
	     (setf (vao-vbo-offset (mesh-vao mesh)) offset)
	     (incf offset (length (mesh-verts mesh))))
    (setf (xbo-validp vbo) nil)))

(defun mesh-pack-fill-ebo (pack)
  (let ((ebo (mesh-pack-ebo pack)))
    (xbo-alloc ebo :unsigned-int (mesh-pack-length-elts pack))
    (loop for mesh across (mesh-pack-meshes pack)
	  with offset = 0
	  do (xbo-fill ebo (mesh-elts mesh) :offset offset)
	     (setf (mesh-offset-elts mesh) offset)
	     (incf offset (length (mesh-elts mesh))))
    (setf (xbo-validp ebo) nil)))

(defun mesh-pack-fill-buffers (pack)
  (mesh-pack-fill-vbo pack)
  (mesh-pack-fill-ebo pack)
  (setf (mesh-pack-validp pack) t))
