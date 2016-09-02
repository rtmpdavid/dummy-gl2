(in-package :cl-user)
(defpackage dummy-gl2
  (:use :cl
   :rtmp-utils))
(in-package :dummy-gl2)

(defparameter attrib-sizes
  (list :pos2 2
	:pos3 3
	:col3  3
	:col4  3
	:tex  2
	:nor2 2
	:nor3 3))

(defun make-simple-array (size type)
  (make-array size :element-type type :adjustable nil))

(defun attrib-size (attrib)
  (getf attrib-sizes attrib))

(defun layout-size (layout)
  (reduce #'+ (mapcar #'attrib-size layout)))

(defvar mesh-counter 0)

(defstruct (mesh (:conc-name mesh-)
		 (:constructor make-mesh
		     (n-verts n-elts layout)))
  (length 0 :type fixnum)
  (verts (make-simple-array (* (layout-size layout) n-verts) 'single-float) :type (simple-array single-float))
  (elts (make-simple-array n-elts 'fixnum) :type (simple-array fixnum))
  (layout nil :type list)
  (id (incf mesh-counter) :read-only t))

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

(defun mesh-set-vert (mesh index &key (pos2 nil pos2p) (pos3 nil pos3p)
				   (col3 nil col3p) (col4 nil col4p)
				   (nor2 nil nor2p) (nor3 nil nor3p)
				   (tex nil texp))
  (when pos2p (setf (mesh-attrib mesh :pos2 index) pos2))
  (when pos3p (setf (mesh-attrib mesh :pos3 index) pos3))
  (when col3p (setf (mesh-attrib mesh :col3 index) col3))
  (when col4p (setf (mesh-attrib mesh :col4 index) col4))
  (when nor2p (setf (mesh-attrib mesh :nor2 index) nor2))
  (when nor3p (setf (mesh-attrib mesh :nor3 index) nor3))
  (when texp (setf (mesh-attrib mesh :tex index) tex)))

(defun make-square (&optional 3d colors tex quad)
  (let ((layout (list (if 3d :pos3 :pos2))))
    (when colors (push :col3 layout))
    (when tex (push :tex layout))
    (let ((mesh (make-mesh 4 (if quad 4
				 6)
			   (reverse layout)))
	  (verts (list (list 0.0 1.0 0.0)
		       (list 1.0 1.0 0.0)
		       (list 1.0 0.0 0.0)
		       (list 0.0 0.0 0.0))))
      (loop for i from 0
	    for v in verts
	    for val = (list mesh i (if 3d :pos3 :pos2) v)
	    do (when colors (nconc val (list :col3 '(1.0 1.0 1.0))))
	       (when tex (nconc val (list :tex (subseq v 0 2))))
	       (apply #'mesh-set-vert val))
      (set-array (mesh-elts mesh) (if quad
				      '(0 1 2 3)
				      '(0 1 2
					2 3 0)))
      mesh)))

