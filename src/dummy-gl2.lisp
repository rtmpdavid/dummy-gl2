(in-package :dummy-gl2)

(defparameter *sdl2-thread* nil)
(defparameter *gl-context* nil)
(defparameter *window* nil)
(defparameter *limit-fps* nil)

(defparameter counter-frequency 1)
(defparameter *last-runtime-counter* 0)
(defparameter *runtime-delta* 0)

(defun update-runtime-counter ()
  (declare (optimize speed)
	   (type (integer 0) *last-runtime-counter*))
  (let ((new-time (sdl2:get-performance-counter)))
    (psetf *runtime-delta* (truncate (- new-time *last-runtime-counter*)
				     counter-frequency)
	   *last-runtime-counter* new-time)))

(defun get-run-time ()
  (declare (optimize speed)
	   (type (integer 0) *last-runtime-counter*))
  (truncate (sdl2:get-performance-counter) counter-frequency))

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
	 (setf counter-frequency (truncate (sdl2:get-performance-frequency) 1000000))
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
  (free-framebuffer fb)
  (clear-shaders)
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
  	    :color-attachments  (loop for i from 0 to 3
  				      collecting (make-texture :size '(500 500)
  							       :internal-format :rgba
  							       :mag-filter :nearest))
  	    :depth-stencil
  	    (make-texture :size '(500 500)
  	    		  :internal-format :depth24-stencil8
  	    		  :format :depth-stencil)))
  (sdl2:with-event-loop (:method :poll)
    (:idle ()	   
	   (update-swank)
	   (update-runtime-counter)
	   (scheduler-step)
	   (continuable (idle-fun)))
    (:windowevent (:event e :data1 d1 :data2 d2)
		  (process-event-window e d1 d2))
    (:quit ()
	   (quit)
	   t)))

(defun process-event-window (event data1 data2)
  (when (or (= event sdl2-ffi:+sdl-windowevent-resized+)
	    (= event sdl2-ffi:+sdl-windowevent-size-changed+))
    (setf (window-size *window*) (list data1 data2))
    (gl:viewport 0.0 0.0 data1 data2)))

(defvar bar 0.0)

(defun idle-fun ()

  ;; (setf fb (make-framebuffer
  ;; 	    :color-attachments  (loop for i from 1 to 3
  ;; 				      collecting (make-texture :size '(500 500)
  ;; 							       :internal-format :rgba
  ;; 							       :mag-filter :nearest))
  ;; 	    :depth-stencil
  ;; 	    (make-texture :size '(500 500)
  ;; 	    		  :internal-format :depth24-stencil8
  ;; 	    		  :format :depth-stencil
  ;; 	    		  :mag-filter :nearest
  ;; 	    		  :min-filter :nearest)
  ;; 	    :stencil t
  ;; 	    ))

  (bind-framebuffer fb)

  (gl:cull-face :back)
  (gl-state-enable :depth-test)
  (gl-state-enable :cull-face)
  (gl:depth-func :less)
  (gl:depth-mask :true)
  
  (gl:draw-buffers (list
  		    :color-attachment0
  		    :color-attachment1
  		    :color-attachment2))
  (gl:polygon-mode :front-and-back :fill)

  (clear-buffers)
  
  (let ((shader :deff-step1))
    (shader-set-active shader)
    (shader-set-uniform shader :color (v! 1.0 1.0 1.0))
    ;; (shader-set-uniform shader :light-direction (v! 0.0 0.0 -1.0))
    ;; (shader-set-uniform shader :light-position (v! 0.0 0.0 -10.0))
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
    			(rtg-math.projection:perspective ;; (window-w *window*)
			 ;; (window-h *window*)
			 1
			 1
			 1.0 100.0
			 (* 45.0 (/ spi 180.0))))
    (let* ((model-mat (m4*
    		       (m4:translation (v! 0.0 0.0 100.0))
    		       (m4:rotation-y (* 2.0 bar))
    		       (m4:scale (v3:*s (v! 1 1 1) 1.0)))))
      (shader-set-uniform shader :model model-mat)
      (shader-set-uniform shader :normal-matrix (m4:transpose (m4:inverse model-mat))))
    (draw-mesh-2 teapot))

  (incf bar 0.005)

  (unbind-framebuffer)
  (gl:polygon-mode :front-and-back :fill)
  (clear-buffers :color '(0.0 0.0 0.0 0.0))
  (draw-texture-overlay (find-attachment fb 0) (make-rect 0 0 300 300))
  (draw-texture-overlay (find-attachment fb 1) (make-rect 0 300 300 600))
  (draw-texture-overlay (find-attachment fb 2) (make-rect 0 600 300 900))
  (draw-texture-overlay (find-attachment fb :depth-attachment) (make-rect 300 0 600 300))
  ;; (free-framebuffer fb)
  (flush-renderer))

(defun draw-texture-overlay (tex rect)
  (shader-set-active :texture-proj-model)
  (use-texture tex :texture0)
  (shader-set-uniform :texture-proj-model :texture-1 0)
  (shader-set-uniform :texture-proj-model :projection
  		      (rtg-math.projection::orthographic
		       (window-w *window*)
		       (window-h *window*)
		       0.0 1.0))
  (shader-set-uniform :texture-proj-model :model
  		      (m4*
		       (m4:translation (v! (- (rx0 rect) (/ (window-w *window*) 2))
					   (- (ry0 rect) (/ (window-h *window*) 2))
					   0.0))
		       (m4:scale (v! (- (rx1 rect) (rx0 rect))
				     (- (ry1 rect) (ry0 rect))
				     1.0))))
  (draw-mesh-2 square-3d-tex))
