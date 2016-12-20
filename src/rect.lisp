;;; Some convinience rectangle manipulation functions

(defun rx0 (rect)
  (aref rect 0))

(defun (setf rx0) (value rect)
  (setf (aref rect 0) value))

(defun ry0 (rect)
  (aref rect 1))

(defun (setf ry0) (value rect)
  (setf (aref rect 1) value))

(defun rx1 (rect)
  (aref rect 2))

(defun (setf rx1) (value rect)
  (setf (aref rect 2) value))

(defun ry1 (rect)
  (aref rect 3))

(defun (setf ry1) (value rect)
  (setf (aref rect 3) value))

(defun make-rect (x0 y0 x1 y1)
  (let ((arr (make-array 4)))
    (setf (aref arr 0) x0
	  (aref arr 1) y0
	  (aref arr 2) x1
	  (aref arr 3) y1)
    arr))

