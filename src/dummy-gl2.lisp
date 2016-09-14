(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils)
  (:export start-main-loop))
(in-package :dummy-gl2)

(defparameter *sdl2-thread* nil)
(defparameter *gl-context* nil)
(defparameter *window* nil)
(defparameter *window-size* nil)

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

(defun init-window (&key (title "engine-base") (w 480) (h 640) (flags '(:shown :opengl :resizable)))
  (format t "Creating sdl2 window~%")
  (setf *window* (sdl2:create-window :title title :w w :h h :flags flags))
  (setf *window-size* (list w h))
  (sdl2:gl-set-attr :context-major-version 4)
  (sdl2:gl-set-attr :context-minor-version 1)
  (sdl2:gl-set-attr :context-profile-mask sdl2-ffi::+SDL-GL-CONTEXT-PROFILE-CORE+)
  (format t "Creating OpenGL context~%")
  (setf *gl-context* (sdl2:gl-create-context *window*))
  (format t "Making OpenGL context current~%")
  (sdl2:gl-make-current *window* *gl-context*)
  (sdl2:gl-set-swap-interval 1))

(defun start-main-loop (&key (w 1000) (h 1000) (title "foobar"))
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

(defvar fb nil)
(defvar ms-fb nil)

(defun main-loop ()
  (setf fb (make-framebuffer :color (make-texture :size '(500 500)
  						  :internal-format :rgba
  						  :mag-filter :linear)))
  (setf ms-fb (make-framebuffer :color-size *window-size* :samples 8))
  (sdl2:with-event-loop (:method :poll)
    (:idle ()
	   (update-swank)
	   (continuable (idle-fun)))
    (:windowevent (:event e :data1 d1 :data2 d2)
		  (process-event-window e d1 d2))
    (:quit () t)))

(defun process-event-window (event data1 data2)
  ;; (if (eq :resized (decode-window-event event))
  ;;     ;; (progn (setf *window-size* (list data1 data2))
  ;;     ;; 	     (gl:viewport 0.0 0.0 data1 data2))
  ;;     )
  )

(defvar frame-count 0)
(defvar old-time 0)
(defvar meh 0)
(defvar foo nil)
(defvar bar 0.0)

;; (decf ncount)
;; (defvar ndir 1)

(defun idle-fun ()
  (incf bar 0.001)
  (incf frame-count)
  (incf meh)
  (let ((time (get-internal-real-time)))
    (when (> (- time old-time) 1000)
      (setf foo (not foo))
      (format t "~a ~a ~a~%" frame-count polygon-count-last (/ (float (- time old-time))
      							       polygon-count-last))
      (setf frame-count 0
  	    old-time time)))

  ;; (set-framebuffer-size fb 20 20)
  ;; (bind-framebuffer fb :framebuffer)
  (bind-framebuffer ms-fb)
  (clear-buffers)

  (use-gl-shader :trivial-texture-model)
  (use-texture texture-1 :texture0)

  ;; (shader-set-uniform :trivial-texture-model :texture-1 0)
  ;; (shader-set-uniform :trivial-texture-model :model
  ;; 		      (rtg-math.matrix4:*
  ;; 		       (rtg-math.matrix4:*
  ;; 			(rtg-math.matrix4:rotation-z bar)
  ;; 			(rtg-math.matrix4:translation (v! -1.0 -1.0 1.0)))
  ;; 		       (rtg-math.matrix4:scale (v! 2.0 2.0 2.0))))
  (gl:cull-face :back)
  (gl:enable :cull-face)
  (gl:polygon-mode :front-and-back :line)
  (gl:polygon-mode :front-and-back :fill)
  (use-gl-shader :trivial-wat)
  ;; (shader-set-uniform :trivial-model-color-uniform :col3 (v! 1.0 0.0 0.0))
  (shader-set-uniform :trivial-wat :model
  		      (mult-mat4
		       (rtg-math.projection:perspective (first *window-size*)
							(second *window-size*)
							 1.0 -10.0 45)
  		       (rtg-math.matrix4:translation (v! 1.0 1.0 -2.0))
  		       (rtg-math.matrix4:rotation-x (* bar 0.3))		       
  		       (rtg-math.matrix4:rotation-z bar)
  		       (rtg-math.matrix4:scale (v! 0.01 0.01 0.01))))
  (draw-mesh teapot)

  (shader-set-uniform :trivial-wat :model
  		      (mult-mat4
		       (rtg-math.projection:perspective (first *window-size*)
							(second *window-size*)
							 1.0 10.0 45)
  		       (rtg-math.matrix4:translation (v! -0.5 -0.5 -2.0))
  		       (rtg-math.matrix4:rotation-x (* bar 0.3))		       
  		       (rtg-math.matrix4:rotation-z bar)
  		       (rtg-math.matrix4:scale (v! 1.0 1.0 1.0))))
  (draw-mesh cube-3d)

  ;; (use-gl-shader :trivial-texture-model)
  ;; (use-texture (framebuffer-color-attachment fb) :texture0)
  ;; (shader-set-uniform :trivial-texture-model :texture-1 0)
  ;; (shader-set-uniform :trivial-texture-model :model
  ;; 		      (mult-mat4
  ;; 		       (rtg-math.matrix4:rotation-z bar)
  ;; 		       (rtg-math.matrix4:translation (v! -1.0 -1.0 1.0))
  ;; 		       (rtg-math.matrix4:scale (v! 2.0 2.0 2.0))))

  ;; (gl:polygon-mode :front-and-back :fill)
  ;; (clear-buffers :color '(0.30 0.2 0.2 1.0))
  ;; (draw-mesh circle)
  
  ;; (unbind-framebuffer)

  (blit-framebuffer ms-fb :filter :nearest)

  (gl:bind-framebuffer :framebuffer 0)
  (flush-renderer))


