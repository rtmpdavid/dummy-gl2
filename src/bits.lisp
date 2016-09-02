(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl))
(in-package :dummy-gl2)

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
