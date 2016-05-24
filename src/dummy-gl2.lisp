(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils))
(in-package :dummy-gl2)

(defun print-gl-info ()
  )

(defgeneric start-main-loop (engine &key w h title out-stream))

(defmethod start-main-loop (engine &key (w 320) (h 640) (title "foobar") (out-stream :repl))
  (let ((stdout (case out-stream
		  (:repl *standard-output*)
		  (t out-stream))))
    (format t "Initializing engine~%")
    (init engine :w w :h h :title title)
    (unwind-protect
	 (let ((*standard-output* stdout))
	   (print-info engine)
	   (main-loop engine))
      (progn
	(format t "Quitting engine")
	(quit engine)))))
