(in-package :dummy-gl2)

(defclass window ()
  ((handle :initform nil
	   :initarg :handle
	   :accessor window-handle)
   (context :initform nil
	    :initarg :context
	    :accessor window-context)
   (title :initform ""
	  :initarg :title
	  :accessor window-title)
   (width :initform nil
	  :initarg :w
	  :accessor window-w)
   (height :initform nil
	   :initarg :h
	   :accessor window-h)))

(defun make-window (width height title flags)
  (format t "Creating sdl2 window~%")
  (let ((window (make-instance 'window
			       :w width :h height
			       :title title
			       :handle (sdl2:create-window :title title
							   :w width
							   :h height
							   :flags (append flags (list :opengl))))))
    (prog1 window
      (sdl2:gl-set-attrs  :context-major-version 4
			  :context-minor-version 1
			  :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+)
      (format t "Creating OpenGL context~%")
      (setf (window-context window) (window-create-gl-context (window-handle window)))
      (format t "Making OpenGL context current~%")
      (sdl2:gl-make-current (window-handle window) (window-context window)))))

(defun window-size (window)
  (list (window-w window)
	(window-h window)))

(defun (setf window-size) (dimensions window)
  (setf (window-w window) (first dimensions)
	(window-h window) (second dimensions)))

(defun window-aspect-ratio (window)
  (/ (window-w window)
     (window-h window)))

(defun window-create-gl-context (window)
  (sdl2:gl-create-context window))

(defun window-swap (window)
  (sdl2:gl-swap-window (window-handle window)))
