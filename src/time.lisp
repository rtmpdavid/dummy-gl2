(in-package :dummy-gl2)

(defparameter *counter-frequency* 1)
(defparameter *last-runtime-counter* 0)
(defparameter *runtime-delta* 0)
(defparameter *runtime-start* 0)
(defparameter *runtime-fun* nil)
(defparameter *delay-fun* nil)

(defun %call-runtime-fun ()
  (funcall *runtime-fun*))

(defun init-runtime-counter (&optional (get-counter-value #'get-internal-real-time)
			       (counter-units-per-second internal-time-units-per-second)
			       (delay-function #'(lambda (count)
						   (sleep (/ count *counter-frequency*)))))
  (setf *runtime-fun* get-counter-value
	*runtime-start* (%call-runtime-fun)
	*counter-frequency* counter-units-per-second
	*last-runtime-counter* *runtime-start*
	*runtime-delta* 0
	*delay-fun* delay-function))

(defun update-runtime-counter ()
  (declare (optimize speed)
	   (type (integer 0) *last-runtime-counter*))
  (let* ((new-time (%call-runtime-fun))
	 (new-delta (- new-time *last-runtime-counter*)))
    (when (not (zerop new-delta))
      (psetf *runtime-delta* new-delta
	     *last-runtime-counter* new-time))))

(defun get-run-time ()
  (declare (optimize speed)
	   (type (integer 0) *last-runtime-counter*))
  (- (%call-runtime-fun) *runtime-start*))

(defun delay (time)
  (funcall *delay-fun* time))

(defmacro ticks-s (count)
  `(* ,count *counter-frequency*))

(defmacro ticks-ms (count)
  `(* (/ ,count 1000) *counter-frequency*))

(defmacro ticks-us (count)
  `(* (/ ,count 1000000) *counter-frequency*))

(defmacro ticks-ns (count)
  `(* (/ ,count 1000000000) *counter-frequency*))

