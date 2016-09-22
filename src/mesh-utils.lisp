(in-package :dummy-gl2)

(defun mesh-make-n-gon (n-vertices &optional (offset-phi 0.0d0))
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

(defun mesh-make-random (n-triangles)
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

(defun mesh-make-square (&optional 3d colors tex)
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

(defun mesh-make-from-asset (asset &optional (flip nil))
  (let ((layout '(:pos3)))
    (when (getf asset :texture)
      (push :tex2 layout))
    (push :nor3 layout)
    (let ((normals (if (getf asset :normals) (getf asset :normals)
		       (calculate-normals asset))))
      (let ((mesh (make-mesh (length (getf asset :verts))
			     (* 3 (length (getf asset :faces))) (reverse layout))))
	(loop for i from 0
	      for v across (getf asset :verts)
	      do (mesh-set-vert mesh i :pos3 v))
	(loop for i from 0
	      for nor3 across normals
	      do (mesh-set-vert mesh i :nor3 nor3))
	(when (getf asset :texture)
	  (loop for i from 0
		for tex across (getf asset :texture)
		do (mesh-set-vert mesh i :tex2 tex)))
	(loop for i from 0
	      for elt across (getf asset :faces)
	      for v = (getf elt :v)
	      if v
		do (loop for j from 0
			 for e in (if flip (reverse v)
				      v)
			 do (setf (aref (mesh-elts mesh) (+ (* i 3) j))
				  e)))
	mesh))))
