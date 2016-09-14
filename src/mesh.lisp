(in-package :cl-user)
(defpackage dummy-gl2
  (:use :cl
   :rtmp-utils))
(in-package :dummy-gl2)

(defparameter attrib-sizes
  (list :pos2 2
	:pos3 3
	:col3 3
	:col4 4
	:tex2 2
	:nor2 2
	:nor3 3))

(defun make-simple-array (size type)
  (make-array size :element-type type :adjustable nil))

(defun attrib-size (attrib)
  (getf attrib-sizes attrib))

(defun layout-size (layout)
  (reduce #'+ (mapcar #'attrib-size layout)))

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

(defun attrib-offset (layout attrib &optional (index 0))
  (when (not (find attrib layout)) (error "Attrib not found"))
  (+ (* index (layout-size layout))
     (loop for a in layout
	   until (eq a attrib)
	   summing (attrib-size a))))

(defun mesh-attrib (mesh attrib index)
  (let  ((offset (attrib-offset (mesh-layout mesh) attrib index)))
    (list (aref (mesh-verts mesh) offset)
	  (aref (mesh-verts mesh) (1+ offset))
	  (aref (mesh-verts mesh) (+ offset 2)))))

(defun (setf mesh-attrib) (value mesh attrib index)
  (when (< (length value) (attrib-size attrib))
    (error (format nil "Too few elements to set ~a" attrib)))
  (loop for i from (attrib-offset (mesh-layout mesh) attrib index)
	for n in value
	repeat (attrib-size attrib)
	do (setf (aref (mesh-verts mesh) i) n)))

(defun attrib-pointer-args (attrib mesh &optional (normalized :false))
  (list (attrib-position attrib)
	(attrib-size attrib)
	:float
	normalized
	(* 4 (layout-size (mesh-layout mesh)))
	(cffi:inc-pointer
	 (cffi:null-pointer)
	 (* 4 (+ (mesh-offset mesh)
		 (attrib-offset (mesh-layout mesh) attrib))))))

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

(defun make-square (&optional 3d colors tex)
  (let ((layout (list (if 3d :pos3 :pos2))))
    (when colors (push :col3 layout))
    (when tex (push :tex2 layout))
    (let ((mesh (make-mesh 4 6
			   (reverse layout)))
	  (verts (list (list 0.0 1.0 0.0)
		       (list 1.0 1.0 0.0)
		       (list 1.0 0.0 0.0)
		       (list 0.0 0.0 0.0))))
      (loop for i from 0
	    for v in verts
	    for val = (list mesh i (if 3d :pos3 :pos2) v)
	    do (when colors (nconc val (list :col3 '(1.0 1.0 1.0))))
	       (when tex (nconc val (list :tex2 (subseq v 0 2))))
	       (apply #'mesh-set-vert val))
      (set-array (mesh-elts mesh) '(0 1 2
				    2 3 0))
      mesh)))

(defun make-random-mesh (n-triangles)
  (let ((mesh (make-mesh (* n-triangles 3)
			 (* n-triangles 3)
			 (list :pos3))))
    (loop for i from 0 to (1- (* n-triangles 3))
	  do (mesh-set-vert mesh i :pos3 (list (random 0.5)
					       (random 0.5)
					       0.0)))
    (loop for i from 0 to (1- (* n-triangles 3))
	  do (setf (aref (mesh-elts mesh) i) i))
    mesh))

(defun make-n-gon (n-vertices &optional (offset-phi 0.0d0))
  (declare (type (integer 2)))
  (let* ((layout (list :pos3 :tex2))
	 (elts (triangulize n-vertices :zigzag1))
	 (mesh (make-mesh n-vertices 0 layout)))
    (setf (mesh-elts mesh) (make-array (length elts) :element-type 'fixnum :initial-contents elts))
    (loop for i from 0
	  for phi = (+ (* i (/ (* 2 pi) n-vertices)) offset-phi)
	  for pos-x = (coerce (/ (1+ (sin phi)) 2) 'single-float)
	  for pos-y = (coerce (/ (1+ (cos phi)) 2) 'single-float)
	  do (mesh-set-vert mesh i :pos3 (list pos-x pos-y 0.0)
				   :tex2 (list pos-x pos-y))
	  repeat n-vertices)
    mesh))

(defun mesh-from-asset (asset)
  (let ((layout '(:pos3)))
    (when (getf asset :texture)
      (push :tex2 layout))
    (when (getf asset :normals)
      (push :nor3 layout))
    (let ((mesh (make-mesh (length (getf asset :verts))
			   (* 3 (length (getf asset :faces))) (reverse layout))))
      (loop for i from 0
	    for v across (getf asset :verts)
	    do (mesh-set-vert mesh i :pos3 v))
      (when (getf asset :texture)
	(loop for i from 0
	      for tex across (getf asset :texture)
	      do (mesh-set-vert mesh i :tex2 tex)))
      (when (getf asset :normals)
	(loop for i from 0
	      for tex across (getf asset :normals)
	      do (mesh-set-vert mesh i :nor3 tex)))
      (loop for i from 0
	    for elt across (getf asset :faces)
	    for v = (getf elt :v)
	    if v
	      do (loop for j from 0
		       for e in v
		       do (setf (aref (mesh-elts mesh) (+ (* i 3) j))
				e)))
      mesh)))
