(in-package :dummy-gl2)

(defstruct (mesh (:conc-name mesh-)
		 (:constructor make-mesh
		     (n-verts n-elts layout)))
  (verts (make-simple-array (* (layout-size layout) n-verts) 'single-float) :type (simple-array single-float))
  (elts (make-simple-array n-elts 'fixnum) :type (simple-array fixnum))
  (layout nil :type list)
  (mode :triangles)
  (vao nil)
  (vao-valid-p nil)
  (gl-array nil)
  (offset 0 :type fixnum)
  (offset-elts 0 :type fixnum))

(defun mesh-attrib (mesh attrib index)
  (let  ((offset (attrib-offset (mesh-layout mesh) attrib index)))
    (list (aref (mesh-verts mesh) offset)
	  (aref (mesh-verts mesh) (1+ offset))
	  (aref (mesh-verts mesh) (+ offset 2)))))

(defun (setf mesh-attrib) (value mesh attrib index)
  (when (< (length value) (attrib-size attrib))
    (error (format nil "Too few elements to set ~a" attrib)))
  (loop for i from (attrib-offset (mesh-layout mesh) attrib index)
	for n in (coerce value 'list)
	repeat (attrib-size attrib)
	do (setf (aref (mesh-verts mesh) i) n)))

(defun mesh-set-attrib-pointers (mesh &optional (normalized :false))
  (dolist (attrib (mesh-layout mesh))    
    (%gl:enable-vertex-attrib-array (attrib-position attrib))
    (%gl:vertex-attrib-pointer (attrib-position attrib)
			       (attrib-size attrib)
			       :float
			       normalized
			       (* (c-sizeof :float)
				  (layout-size (mesh-layout mesh)))
			       (cffi:inc-pointer
				(cffi:null-pointer)
				(* (c-sizeof :float)
				   (+ (mesh-offset mesh)
				      (attrib-offset (mesh-layout mesh) attrib)))))))

(defun mesh-set-vert (mesh index &key (pos2 nil pos2p) (pos3 nil pos3p)
				   (col3 nil col3p) (col4 nil col4p)
				   (nor2 nil nor2p) (nor3 nil nor3p)
				   (tex2 nil tex2p))
  (when pos2p (setf (mesh-attrib mesh :pos2 index) pos2))
  (when pos3p (setf (mesh-attrib mesh :pos3 index) pos3))
  (when col3p (setf (mesh-attrib mesh :col3 index) col3))
  (when col4p (setf (mesh-attrib mesh :col4 index) col4))
  (when nor2p (setf (mesh-attrib mesh :nor2 index) nor2))
  (when nor3p (setf (mesh-attrib mesh :nor3 index) nor3))
  (when tex2p (setf (mesh-attrib mesh :tex2 index) tex2)))


