(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils))
(in-package :dummy-gl2)

(defclass engine-gl-sdl (engine-base)
  ((window
    :initform nil)
   (context
    :initform nil)))

(defmethod init-engine ((engine engine-gl-sdl) &key (title "engine-base") (w 320) (h 640) (flags '(:shown :opengl)))
  (format t "Initializing sdl2~%")
  (sdl2:init :everything)
  (format t "Creating sdl2 window~%")
  (let ((window (sdl2:create-window :title title :w w :h h :flags flags)))
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3)
    (sdl2:gl-set-attr :context-profile-mask sdl2-ffi::+SDL-GL-CONTEXT-PROFILE-CORE+)
    (format t "Creating OpenGL context~%")
    (let ((context (sdl2:gl-create-context window)))
      (setf (slot-value engine 'window) window
	    (slot-value engine 'context) context)
      (format t "Making OpenGL context current~%")
      (sdl2:gl-make-current window context))))

(defmethod quit ((engine engine-gl-sdl))
  (when (slot-value engine 'context)
    (format t "Destroying OpenGL context~%")
    (sdl2:gl-delete-context (slot-value engine 'context)))
  (when (slot-value engine 'window)
    (format t "Destroying sdl2 window~%")
    (sdl2:destroy-window (slot-value engine 'window)))
  (format t "Quitting sdl2~%")
  (sdl2:quit))

(defmethod print-info ((engine engine-gl-sdl))
  (multiple-value-bind (sdl2-major sdl2-minor sdl2-patch)
      (sdl2:version-wrapped)
    (format t "SDL2 OpenGL engine:
~tSDL version: ~a.~a.~a
~tDriver vendor: ~a
~tRunning on ~a.
~tOpenGL Version ~a.~a
~tGlSl version ~a.~%"
	    sdl2-major
	    sdl2-minor
	    sdl2-patch
	    (gl:get-string :vendor)
	    (gl:get-string :renderer)
	    (gl:get-integer :major-version) (gl:get-integer :minor-version)
	    (gl:get-string :shading-language-version))))

(defmethod main-loop ((engine engine-gl-sdl))
  (sdl2:with-event-loop ()
    (:quit () t)))
