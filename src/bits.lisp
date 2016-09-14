(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl))
(in-package :dummy-gl2)

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

(defun set-array (array values)
  (let ((type (array-element-type array)))
    (loop for i from 0
	  repeat (1- (length array))
	  do (setf (aref array i) (coerce (elt values i) type)))))

(defun triangulize (n-points &optional (method :zigzag1))
  (if (= 3 n-points) '(0 1 2)
      (case method
	(:sun1
	 (loop
	   for i from 1
	   for j from 2 to (1- n-points)
	   appending (list 0 i j)))
	(:zigzag1
	 (loop
	   for flip = t then (not flip)
	   for orig = 0 then (if flip c 
				 cc)
	   for c = 1 then (if flip (1+ c) 
			      c)
	   for cc = (1- n-points) then (if flip cc 
					   (1- cc))
	   while (< c cc)
	   appending (list orig c cc))))))

(defun mult-mat4 (&rest matrices)
  (reduce #'rtg-math.matrix4:* matrices
	  :initial-value (rtg-math.matrix4:identity)))
