(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils))
(in-package :dummy-gl2)

(defgeneric draw-dot (point))
(defgeneric draw-line (point-a point-b))
(defgeneric draw-)

(defclass renderer-base ()
  ())


