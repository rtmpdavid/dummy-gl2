(in-package :dummy-gl2)

(defparameter *sdl2-thread* nil)
(defparameter *gl-context* nil)
(defparameter *window* nil)
(defparameter *limit-fps* nil)

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

(defun init-window (&key (title "engine-base") (w 480) (h 640) (flags '(:shown :resizable)))
  (setf *window* (make-window w h title flags))
  (sdl2:gl-set-swap-interval 1))

(defun start-main-loop (&key (w 500) (h 500) (title "foobar"))
  (format t "Initializing sdl2~%")
  (sdl2:make-this-thread-main
   #'(lambda ()
       (unwind-protect 
	    (sdl2:init :everything)
	 (init-window :w w :h h :title title)
	 (init-renderer)
	 (print-info)
	 (main-loop)
	 (progn
	   (format t "Quitting engine~%")
	   (quit)))))
  :name (format nil "DummyGL2 - ~a" title))

(defun stop-main-loop (&optional (force nil))
  (when *sdl2-thread*
    (sdl2:push-quit-event)
    (if force
	(progn (bordeaux-threads:with-timeout (10)
		 (bordeaux-threads:join-thread *sdl2-thread*)
		 (when *sdl2-thread* (bordeaux-threads:destroy-thread *sdl2-thread*))))
	(bordeaux-threads:join-thread *sdl2-thread*))))

(defun run (&key (w 500) (h 500) (title "foobar") (dedicated-thread t))
  (if dedicated-thread (setf *sdl2-thread*
			     (bt:make-thread 
			      #'(lambda () (start-main-loop :w w :h h :title title))
			      :name "DummyGL2 Main Thread"))
      (progn (setf *sdl2-thread* (bt:current-thread))
	     (start-main-loop :w w :h h :title title))))

(defun quit ()
  (when *gl-context*
    (free-buffers static-meshes)
    (format t "Destroying OpenGL context~%")
    (sdl2:gl-delete-context *gl-context*))
  (when *window*
    (format t "Destroying sdl2 window~%")
    (sdl2:destroy-window (window-handle *window*)))
  (format t "Quitting sdl2~%")
  (sdl2:quit)
  (setf *window* nil
	*gl-context* nil))

(defvar fb nil)
(defvar ms-fb nil)

(defun main-loop ()
  (setf texture-1 (make-texture :image (make-checker-pattern 250 :color-b (mapcar #'floor (list (* 255 0.30) (* 255 0.2) (* 255 0.2)))
								 :color-f '(255 255 255))))
  texture-2 (make-texture :image (make-checker-pattern 10 :color-b '(255 0 0)
							  :color-f '(0 255 0))
			  :min-filter :nearest
			  :mag-filter :nearest
			  :wrap-s :clamp-to-edge
			  :wrap-t :clamp-to-edge)
  ;; (setf fb (make-framebuffer :color (make-texture :size '(500 500)
  ;; 						  :internal-format :rgba
  ;; 						  :mag-filter :nearest)))
  ;; (setf ms-fb (make-framebuffer :color-size (window-size *window*)
  ;; 				:samples 8))
  (setf fb (make-framebuffer
  	    :color-attachments  (loop for i from 0 to 5
				      collecting (make-texture :size '(500 500)
							       :internal-format :rgba
							       :mag-filter :nearest))
	    :depth-stencil
	    (make-texture :size '(500 500)
			  :internal-format :depth24-stencil8
			  :format :depth-stencil
			  :mag-filter :nearest)))
  (sdl2:with-event-loop (:method :poll)
    (:idle ()
	   (update-swank)
	   (continuable (idle-fun)))
    (:windowevent (:event e :data1 d1 :data2 d2)
		  (process-event-window e d1 d2))
    (:quit ()
	   (free-framebuffer fb))))

(defun process-event-window (event data1 data2)
  (when (or (= event sdl2-ffi:+sdl-windowevent-resized+)
	    (= event sdl2-ffi:+sdl-windowevent-size-changed+))
    (setf (window-size *window*) (list data1 data2))
    (gl:viewport 0.0 0.0 data1 data2)))

(defvar bar 0.0)

(defun idle-fun ()
  (bind-framebuffer fb)
  ;; (draw-full-checkers)
  (gl:draw-buffers (list
		    :color-attachment0
		    :color-attachment1
		    :color-attachment2))

  (gl-state-enable :cull-face)
  (gl:cull-face :back)
  (gl-state-enable :depth-test)
  (gl:polygon-mode :front-and-back :fill)

  (clear-buffers)

  (let ((shader :deff-step1))
    (use-shader shader)
    (shader-set-uniform shader :color (v! 1.0 1.0 1.0))
    ;; (shader-set-uniform shader :light-direction (v! 0.0 0.0 -1.0))
    ;; (shader-set-uniform shader :light-position (v! 0.0 0.0 0.0))
    ;; (shader-set-uniform shader :light-color (v! 1.0 1.0 1.0))
    ;; (shader-set-uniform shader :diffuse 0.5)
    ;; (shader-set-uniform shader :ambient 0.0)
    ;; (shader-set-uniform shader :spec-shiny 256.0)
    ;; (shader-set-uniform shader :spec-strength 1.0)

    (shader-set-uniform shader :view
			(look-vec
			 (v! 0.0 0.0 0.0)
			 (v! 0.0 0.0 1.0)))

    (shader-set-uniform shader :projection
			(rtg-math.projection:perspective (window-w *window*)
							 (window-h *window*)
							 1.0 100.0
							 (* 45.0 (/ spi 180.0))))
    
    (let* ((model-mat (m4*
		       (m4:translation (v! 0.0 0.0 120.0))
		       (m4:rotation-y (* 2.0 bar))
		       (m4:scale (v3:*s (v! 1 1 1) 1.0)))))
      (shader-set-uniform shader :model model-mat)
      (shader-set-uniform shader :normal-matrix (m4:transpose (m4:inverse model-mat)))))
  (draw-mesh-2 teapot)

  (incf bar 0.005)

  (unbind-framebuffer)
  (clear-buffers)
  (gl-state-disable :cull-face)
  (use-shader :texture-depth)  
  (use-texture (find-attachment fb :depth-attachment) :texture0)
  
  (shader-set-uniform :texture-proj-model :texture-1 0)
  (shader-set-uniform :texture-proj-model :projection
		      (rtg-math.projection::orthographic (window-aspect-ratio *window*)
							 1.0
							 ;; (window-w *window*)
							 ;; (window-h *window*)
							 0.0 1.0))
  (let ((s-val (if (apply #'< (window-size *window*))
  		   (window-w *window*)
  		   (window-h *window*))))
    (incf s-val s-val)
    (shader-set-uniform :texture-proj-model :model
  			(m4*
			 (m4:identity)
			 ;; (m4:translation
  			 ;;  (v! 0.4 0.0 0.0))
			 
  			 (m4:translation
  			  (v! (/ (window-aspect-ratio *window*) -2.0)
  			      -0.5
  			      0.0))
  			 (m4:scale
  			  (v! (window-aspect-ratio *window*)
  			      1.0
  			      0.0))
			 )))
  (draw-mesh-2 square-3d-tex)
  ;; (blit-framebuffer fb :read-buffer 0)
  (blit-framebuffer fb :read-buffer 0 :dst-rect (make-rect 0 0 150 150))
  (blit-framebuffer fb :read-buffer 1 :dst-rect (make-rect 150 0 300 150))
  (blit-framebuffer fb :read-buffer 2 :dst-rect (make-rect 300 0 450 150))
  ;; (find-attachment )
  (flush-renderer))

(defun draw-full-checkers ()
  (gl:polygon-mode :front-and-back :fill)
  (gl-state-disable :cull-face)
  (use-shader :texture-proj-model)  
  (use-texture texture-1 :texture0)
  
  (shader-set-uniform :texture-proj-model :texture-1 0)
  (shader-set-uniform :texture-proj-model :projection
		      (rtg-math.projection::orthographic (window-aspect-ratio *window*)
							 1.0
							 ;; (window-w *window*)
							 ;; (window-h *window*)
							 0.0 1.0))
  (let ((s-val (if (apply #'< (window-size *window*))
  		   (window-w *window*)
  		   (window-h *window*))))
    (incf s-val s-val)
    (shader-set-uniform :texture-proj-model :model
  			(m4*
			 (m4:identity)
			 ;; (m4:translation
  			 ;;  (v! 0.4 0.0 0.0))
			 
  			 (m4:translation
  			  (v! (/ (window-aspect-ratio *window*) -2.0)
  			      -0.5
  			      0.0))
  			 (m4:scale
  			  (v! (window-aspect-ratio *window*)
  			      1.0
  			      0.0))
			 )))
  (draw-mesh-2 square-3d-tex))
