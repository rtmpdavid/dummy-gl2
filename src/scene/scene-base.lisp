(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils))
(in-package :dummy-gl2)

(defgeneric scene-init (scene))
(defgeneric scene-step (scene time-passed))
(defgeneric scene-draw (scene))

(defgeneric scene-done (scene))

(defclass scene-base ()
  ((name
    :initarg name
    :initform "Scene base"
    :accessor scene-name)))
