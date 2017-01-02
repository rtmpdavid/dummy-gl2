(in-package :dummy-gl2)

(defun make-checker-pattern (size &key (color-b '(255 255 255)) (color-f '(0 0 0)))
  (let ((pattern (make-array (list (* 2 size) (* 2 size) 3)))
	(color))
    (dotimes (j (* 2 size))
      (dotimes (i (* 2 size))
	(if (zerop (logxor (truncate i size) (truncate j size)))
	    (setf color color-b)
	    (setf color color-f))
	(dotimes (k 3)
	  (setf (aref pattern j i k) (elt color k)))))
    (list
     :data pattern
     :format :rgb
     :width (* size 2)
     :height (* size 2))))

(defstruct (gl-texture (:conc-name tex-)
		       (:constructor make-tex))
  (gl-object nil)
  (gl-object-valid nil)
  (pixels nil)
  (pixel-format nil)
  (internal-format :rgb)
  (width 0)
  (height 0)
  (wrap-s :clamp-to-edge)
  (wrap-t :clamp-to-edge)
  (min-filter :linear)
  (mag-filter :linear))

(defun texel-size (format)
  (case format
    (:rgb 3)
    (:rgba 4)
    (:depth-stencil 2)
    (t (error (format nil "Format ~a not supported" format)))))

(defun create-texture-data (texture)
  (when (tex-pixels texture) (gl:free-gl-array (tex-pixels texture)))
  (setf (tex-gl-object-valid texture) nil
	(tex-pixels texture) (gl:alloc-gl-array :unsigned-char (* (texel-size (tex-pixel-format texture))
								  (tex-width texture)
								  (tex-height texture)))))

(defun set-texture-image (texture image)
  (let ((width (getf image :width))
	(height (getf image :height))
	(format (texel-size (tex-pixel-format texture))))
    (setf (tex-gl-object-valid texture) nil)
    (when (or (/= (tex-width texture) width)
	      (/= (tex-height texture) height))
      (setf (tex-width texture) width
	    (tex-height texture) height)
      (create-texture-data texture))
    (dotimes (y height)
      (dotimes (x width)
	(loop for i from 0
	      while (< i (texel-size (getf image :format)))
	      do (setf (gl:glaref (tex-pixels texture) (+ i (* x format) (* y (* width format))))
		       (if (>= i format) 1.0
			   (aref (getf image :data) y x i))))))))

(defun make-texture (&key (image) (size)
		       (format :rgb)
		       (internal-format :rgb)
		       (wrap-s :clamp-to-edge) (wrap-t :clamp-to-edge)
		       (min-filter :linear) (mag-filter :linear))
  (when (or (and image size) (not (or image size)))
    (error "Texture should be created with either image or size, not both"))
  (let ((texture (make-tex :pixel-format format :internal-format internal-format
			   :wrap-s wrap-s :wrap-t wrap-t
			   :min-filter min-filter :mag-filter mag-filter)))
    (prog1 texture
      (if image (set-texture-image texture image)
	  (setf (tex-width texture) (elt size 0)
		(tex-height texture) (elt size 1)
		(tex-pixels texture) (gl:make-null-gl-array :char))))))

(defun alloc-texture-data (texture)
  )

(defun use-texture (texture texture-unit &optional (target :texture-2d) (n-samples 1))
  (if (not (tex-gl-object texture))
      (progn (setf (tex-gl-object texture) (gl:gen-texture))
	     (bind-gl-texture texture texture-unit target)
	     (when (not (eq target :texture-2d-multisample))
	       (gl:tex-parameter target :texture-wrap-s (tex-wrap-s texture))
	       (gl:tex-parameter target :texture-wrap-t (tex-wrap-t texture))
	       (gl:tex-parameter target :texture-min-filter (tex-min-filter texture))
	       (gl:tex-parameter target :texture-mag-filter (tex-mag-filter texture))))
      (bind-gl-texture texture texture-unit target))
  (when (not (tex-gl-object-valid texture))
    (if (eq target :texture-2d-multisample)
	(set-texture-multisample texture target n-samples)
	(set-texture-data texture target))))

(defun bind-gl-texture (texture texture-unit target)
  (gl:active-texture texture-unit)
  (gl:bind-texture target (tex-gl-object texture)))

(defun set-texture-data (texture target)
  (%gl:tex-image-2d target 0
		    (gl::internal-format->int (tex-internal-format texture))
		    (tex-width texture) (tex-height texture)
		    0
		    (tex-pixel-format texture)
		    (if (eq (tex-internal-format texture) :depth24-stencil8)
			:unsigned-int-24-8
			:unsigned-int)
		    (slot-value (tex-pixels texture) 'gl::pointer))
  (setf (tex-gl-object-valid texture) t))

(defun set-texture-multisample (texture target n-samples)
  (%gl:tex-image-2d-multisample
   target n-samples
   (gl::internal-format->int (tex-internal-format texture))
   (tex-width texture) (tex-height texture)
   :false)
  (setf (tex-gl-object-valid texture) t))

(defun free-texture-data (texture)
  (when (tex-pixels texture) (gl:free-gl-array (tex-pixels texture)))
  (when (tex-gl-object texture)
    (gl:delete-textures (list (tex-gl-object texture)))
    (setf (tex-gl-object texture) nil
	  (tex-gl-object-valid texture) nil)))

(defun set-texture-size (tex width height)
  (free-texture-data tex)
  (setf (tex-width tex) width
	(tex-height tex) height))

(defvar texture-1 (make-texture :image (make-checker-pattern 250 :color-b (mapcar #'floor (list (* 255 0.30) (* 255 0.2) (* 255 0.2)))
								 :color-f '(255 255 255))))
(defvar texture-2 (make-texture :image (make-checker-pattern 10 :color-b '(255 0 0)
								:color-f '(0 255 0))
				:min-filter :linear
				:mag-filter :linear
				:wrap-s :clamp-to-edge
				:wrap-t :clamp-to-edge))

