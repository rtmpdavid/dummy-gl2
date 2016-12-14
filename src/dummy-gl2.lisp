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
			      #'(lambda () (start-main-loop :w w :h h :title title))))
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
  (setf fb (make-framebuffer :color (make-texture :size '(500 500)
  						  :internal-format :rgba
  						  :mag-filter :nearest)))
  (setf ms-fb (make-framebuffer :color-size (window-size *window*)
				:samples 8))
  (sdl2:with-event-loop (:method :poll)
    (:idle ()
	   (update-swank)
	   (continuable (idle-fun)))
    (:windowevent (:event e :data1 d1 :data2 d2)
		  (process-event-window e d1 d2))
    (:quit () t)))

(defun process-event-window (event data1 data2)
  (when (or (= event sdl2-ffi:+sdl-windowevent-resized+)
	    (= event sdl2-ffi:+sdl-windowevent-size-changed+))
    (setf (window-size *window*) (list data1 data2))
    (gl:viewport -1.0 -1.0 (/ data1 data2) 1.0)))

(defvar bar 0.0)

(defun idle-fun ()
  (let ((res (/ (window-h *window*) 0.5)))
    (set-framebuffer-size fb (window-w *window*) (window-h *window*))
    (set-framebuffer-size ms-fb (window-w *window*) (window-h *window*))
    (bind-framebuffer ms-fb)
    (clear-buffers)

    (gl-state-enable :cull-face)
    (gl:cull-face :back)
    (gl-state-enable :depth-test)
    (gl:polygon-mode :front-and-back :fill)

    (use-shader :diffuse)
    (shader-set-uniform :diffuse :light-position (v! 0.0 0.0 -10.0))
    (shader-set-uniform :diffuse :light-color (v! 1.0 1.0 1.0))
    (shader-set-uniform :diffuse :diffuse 0.7)
    (shader-set-uniform :diffuse :ambient 0.3)
    (shader-set-uniform :diffuse :spec-shiny 16.0)
    (shader-set-uniform :diffuse :spec-strength 0.3)

   (shader-set-uniform :diffuse :projection
   			(m4*
   			 (rtg-math.projection:perspective (window-aspect-ratio *window*)
   							  1.0
   							  1.0 -1.0 90)))   
   (let ((n 1))
      (shader-set-uniform :diffuse :view (look-vec
      					  (v! 0.0 0.0 -20.0)
      					  (v! 0.0 0.0 1.0)))
      (dotimes (i n)
   	(let ((phi (* 2 spi (/ (1+ i) n))))
   	  (shader-set-uniform :diffuse :color (v! (/ (1+ (sin (+ bar phi))) 2)
						  (/ (1+ (sin (* 2 (+ bar phi)))) 2)
						  (/ (1+ (sin (* 4 (+ bar phi)))) 2)
   						  1.0))
   	  (let* ((model-mat (m4*
   			     (m4:translation (v! (* 20.0 (sin (+ phi bar)))
						 (* 20.0 (cos (+ phi bar)))
						 60.0
						 ))
   			     ;; (m4:rotation-y (sin (+ bar (* 2 (coerce pi 'single-float)
   			     ;; 				   (/ (1+ i) n)))))
			     ;; (m4:rotation-x (/ (+ (* 3 bar) (* i 3.0)) 2.0))
			     
   			     ;; (m4:rotation-y (+ bar (* i 4.0)))
   			     ;; if (oddp i)
			     (m4:scale (v3:*s (v! 1 1 1) 50.0))
			     ;; (m4:scale (v3:*s (v! 1 1 1) 0.0005))
			     )))

	    (shader-set-uniform :diffuse :model model-mat)
   	    (shader-set-uniform :diffuse :normal-matrix (m4:transpose (m4:inverse model-mat)))
   	    (draw-mesh-2 ;; if (oddp i)
			 urth
	     ;; teapot

			 ))))))

  (shader-set-uniform :diffuse :color (v! 0.3 0.3 0.3))
  
    (shader-set-uniform :diffuse :light-position (v! 0.0 0.0 0.0))
    (shader-set-uniform :diffuse :light-color (v! 1.0 1.0 1.0))
    (shader-set-uniform :diffuse :diffuse 0.1)
    (shader-set-uniform :diffuse :ambient 0.1)
    (shader-set-uniform :diffuse :spec-shiny 1024.0)
    (shader-set-uniform :diffuse :spec-strength 0.0)
  
  (let* ((model-mat (m4*
  		     (m4:translation (v! 0.0 0.0 4.0))
  		     (m4:rotation-y (* 0.1 bar))
  		     (m4:scale (v3:*s (v! 1 1 1) 1.0)))))

  	  (shader-set-uniform :diffuse :model model-mat)
  	  (shader-set-uniform :diffuse :normal-matrix (m4:transpose (m4:inverse model-mat)))
  	  (draw-mesh-2 cube-2))

  (blit-framebuffer ms-fb :fb-dest 0)
  
  (unbind-framebuffer)
  (gl:polygon-mode :front-and-back :fill)

  ;; (clear-buffers :color '(0.30 0.2 0.2 1.0))
  (use-shader :texture-proj-model)  
  (use-texture (framebuffer-color-attachment fb) :texture0)
  (shader-set-uniform :texture-proj-model :texture-1 0)
  (shader-set-uniform :texture-proj-model :projection
  		      (rtg-math.projection::orthographic (window-w *window*)
  							 (window-h *window*)
  							 0.0 -1.0))
  (gl-state-disable :cull-face)
  (let ((s-val (if (apply #'< (window-size *window*))
  		   (window-w *window*)
  		   (window-h *window*))))
    (incf s-val s-val)
    (shader-set-uniform :texture-proj-model :model
  			(m4*
  			 ;; (m4:rotation-z (- (/ bar 3.0)))
  			 (m4:translation
  			  (v! (/ (window-w *window*) -2)
			      (/ (window-h *window*) -2)
  			      0.0))
  			 (m4:scale
  			  (v! (window-w *window*)
			      (window-h *window*)
  			      0.0)))))
  ;; (draw-mesh-2 square-3d-tex)
  
  (incf bar 0.005)
  (flush-renderer))

