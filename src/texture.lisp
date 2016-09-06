(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils))
(in-package :dummy-gl2)

(defun make-checker-pattern (size &optional (color-b '(255 255 255)) (color-f '(0 0 0)))
  (let ((pattern (make-array (list (* 2 size) (* 2 size 3)))))
    (loop for j from 0 to (1- (* 2 size))
	  do (loop for i from 0 to (1- (* 2 size))
		   with color
		   if (zerop (logxor (truncate i size) (truncate j size)))
		     do (setf color color-b)
		   else
		     do (setf color color-f)
		   do (loop for k from 0 to 2
		     	      do (setf (aref pattern j (+ (* i 3) k)) (elt color k)))))
    pattern))



