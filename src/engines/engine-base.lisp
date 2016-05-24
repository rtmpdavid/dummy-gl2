(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils))
(in-package :dummy-gl2)

(defgeneric init-engine (engine &key title w h flags))
(defgeneric print-info (engine))
(defgeneric quit (engine))
(defgeneric main-loop (engine))

(defclass engine-base ()
  ())

(defmethod init-engine ((engine engine-base) &key title w h flags)
  (declare (ignore title w h flags))
  (format t "Nothing to init.~%"))

(defmethod print-info ((engine engine-base))
  (format t "The dummiest of all engines.~%"))

(defmethod quit ((engine engine-base))
  (format t "Bye!"))

(defmethod main-loop ((engine engine-base))
  (let ((loop-start (get-universal-time)))
    (loop
      (sleep 1)
      (format t "Nopping for ~r seconds and counting.~%"
	      (- (get-universal-time) loop-start)))))

