(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtg-math))
(in-package :dummy-gl2)

(defun c-sizeof (c-type)
  (declare (inline))
  (cffi:foreign-type-size c-type))

(defun c-ptr-offset (c-type ))

(when (not (boundp 'null-pointer))
  (defconstant null-pointer (cffi:null-pointer)))

(defmacro continuable (&body body)
  `(restart-case (progn ,@body)
     (continue () :report "Continuable: Continue")))

(defun update-swank ()
  (continuable
    (let ((connection (or swank::*emacs-connection*
			  (swank::default-connection))))
      (when connection
	(swank::handle-requests connection t)))))

(defun make-simple-array (size type)
  (make-array size :element-type type :adjustable nil))

(defun set-array (array values)
  (let ((type (array-element-type array)))
    (loop for i from 0
	  repeat (1- (length array))
	  do (setf (aref array i) (coerce (elt values i) type)))))

(defun triangulize (n-points &optional (method :zigzag1))
  (if (= 3 n-points) '(0 1 2)
      (case method
	(:sun1 (loop
		 for i from 1
		 for j from 2 to (1- n-points)
		 appending (list 0 i j)))
	(:zigzag1 (loop
		    for flip = t then (not flip)
		    for orig = 0 then (if flip c cc)
		    for c = 1 then (if flip (1+ c) c)
		    for cc = (1- n-points) then (if flip cc (1- cc))
		    while (< c cc)
		    appending (list orig c cc))))))

(defun mult-mat4 (&rest matrices)
  (reduce #'rtg-math.matrix4:* matrices
	  :initial-value (rtg-math.matrix4:identity)))

(defun face-vert (index face verts)
  (elt verts (elt face index)))

(defun face-normal (face verts)
  (let ((a (coerce (face-vert 0 face verts) '(vector single-float)))
	(b (coerce (face-vert 1 face verts) '(vector single-float)))
	(c (coerce (face-vert 2 face verts) '(vector single-float))))
    (rtg-math.vector3:cross (rtg-math.vector3:- b a)
			    (rtg-math.vector3:- c a))))

(defun vert-normal (verts faces)
  (loop for face in faces
	collecting (face-normal face verts)))

(defun calculate-normals (mesh)
  (let* ((verts (getf mesh :verts))
	 (normals (make-array (length verts))))
    (prog1 normals
     (loop for index from 0 to (1- (length verts))
	   for faces = (map 'list #'(lambda (e) (getf e :v))
			    (remove-if-not #'(lambda (face) (find index face))
					   (getf mesh :faces)
					   :key #'(lambda (face) (getf face :v))))
	   do (setf (aref normals index)
		    (v:normalize (reduce #'v:+
				       (vert-normal verts faces))))))))
