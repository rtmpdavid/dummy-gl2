(in-package :dummy-gl2)

(defun attrib-size (attrib)
  (case attrib
    (:pos2 2)
    (:pos3 3)
    (:col3 3)
    (:col4 4)
    (:tex2 2)
    (:nor2 2)
    (:nor3 3)))

(defun attrib-offset (layout attrib &optional (index 0))
  (when (not (find attrib layout)) (error "Attrib not found"))
  (+ (* index (layout-size layout))
     (loop for a in layout
	   until (eq a attrib)
	   summing (attrib-size a))))

(defparameter attrib-positions
  (list :pos2 0
	:pos3 0
	:col3 1
	:col4 1
	:tex2 2
	:nor2 3
	:nor3 3))

(defun attrib-position (attrib)
  (loop for key in attrib-positions by #'cddr
	and value in (cdr attrib-positions) by #'cddr
	if (string= (symbol-name attrib) (symbol-name key))
	  do (return value)))
