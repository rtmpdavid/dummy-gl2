(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils)
  (:export start-main-loop))
(in-package :dummy-gl2)

(defparameter *sdl2-thread* nil)
(defparameter *gl-context* nil)
(defparameter *window* nil)

(defun print-info ()
  (multiple-value-bind (sdl2-major sdl2-minor sdl2-patch)
      (sdl2:version-wrapped)
    (format t "SDL2 OpenGL engine:
~tSDL version: ~a.~a.~a
~tDriver vendor: ~a
~tRunning on ~%~a. 
~tOpenGL Version ~a.~a
~tGlSl version ~a.~%"
	    sdl2-major
	    sdl2-minor
	    sdl2-patch
	    (gl:get-string :vendor)
	    (gl:get-string :renderer)
	    (gl:get-integer :major-version) (gl:get-integer :minor-version)
	    (gl:get-string :shading-language-version))))

(defun init-window (&key (title "engine-base") (w 320) (h 640) (flags '(:shown :opengl)))
  (format t "Creating sdl2 window~%")
  (setf *window* (sdl2:create-window :title title :w w :h h :flags flags))
  (sdl2:gl-set-attr :context-major-version 4)
  (sdl2:gl-set-attr :context-minor-version 1)
  (sdl2:gl-set-attr :context-profile-mask sdl2-ffi::+SDL-GL-CONTEXT-PROFILE-CORE+)
  (format t "Creating OpenGL context~%")
  (setf *gl-context* (sdl2:gl-create-context *window*))
  (format t "Making OpenGL context current~%")
  (sdl2:gl-make-current *window* *gl-context*)
  (sdl2:gl-set-swap-interval 0))

(defun start-main-loop (&key (w 320) (h 640) (title "foobar"))
  (setf *sdl2-thread*
	(bordeaux-threads:make-thread
	 #'(lambda ()
	     (format t "Initializing sdl2~%")
	     (sdl2:init :everything)
	     (unwind-protect		      
		  (sdl2:in-main-thread ()
		    (init-window :w w :h h :title title)
		    (init-renderer)
		    (print-info)
		    (main-loop))
	       (progn
		 (format t "Quitting engine~%")
		 (quit))))
	 :name (format nil "DummyGL2 - ~a" title))))

(defun stop-main-loop (&optional (force nil))
  (when *sdl2-thread*
    (sdl2:push-quit-event)
    (if force
	(progn (bordeaux-threads:with-timeout (10)
		 (bordeaux-threads:join-thread *sdl2-thread*)
		 (when *sdl2-thread* (bordeaux-threads:destroy-thread *sdl2-thread*))))
	(bordeaux-threads:join-thread *sdl2-thread*))))

(defun quit ()
  (when *gl-context*
    (free-buffers static-meshes)
    (format t "Destroying OpenGL context~%")
    (sdl2:gl-delete-context *gl-context*))
  (when *window*
    (format t "Destroying sdl2 window~%")
    (sdl2:destroy-window *window*))
  (format t "Quitting sdl2~%")
  (sdl2:quit)
  (setf *window* nil
	*gl-context* nil))

(defvar vao nil)

(defun main-loop ()
  (setf vao (gl:gen-vertex-array))
  (gl:bind-vertex-array vao)
    (gl:enable-vertex-attrib-array (attrib-position :pos3))
  
  (bind-vbo-buffer static-meshes)
  (bind-vbo-data static-meshes)
  ;; (bind-ebo-buffer static-meshes)
  ;; (bind-ebo-data static-meshes)

  (sdl2:with-event-loop (:method :poll)
    (:idle ()
	   (update-swank)
	   (continuable (idle-fun)))
    (:quit () t))
  (gl:delete-vertex-arrays (list vao)))

(defvar frame-count 0)
(defvar old-time 0)

(defun idle-fun ()
  (incf frame-count)
  (let ((time (get-internal-real-time)))
    (when (> (- time old-time) 1000)
      (print frame-count)
      (setf frame-count 0
  	    old-time time)))
  (clear-buffers :color '(0.0 0.1 0.1 1.0))

  (use-gl-shader :trivial)

  (draw-mesh random-verts-1 )
  (draw-mesh random-verts-1 )
  (draw-mesh random-verts-1 )
  (draw-mesh random-verts-1 )
  (draw-mesh random-verts-1 )
  (draw-mesh random-verts-1 )
  (draw-mesh random-verts-1 )
  (draw-mesh random-verts-1 )
  (draw-mesh random-verts-1 )
  (draw-mesh random-verts-1 )

  (sdl2:gl-swap-window *window*))
