(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils))
(in-package :dummy-gl2)

(defun make-checker-pattern (size &key (color-b '(255 255 255)) (color-f '(0 0 0)))
  (let ((pattern (make-array (list (* 2 size) (* 2 size) 3))))
    (loop for j from 0 to (1- (* 2 size))
	  do (loop for i from 0 to (1- (* 2 size))
		   with color
		   if (zerop (logxor (truncate i size) (truncate j size)))
		     do (setf color color-b)
		   else
		     do (setf color color-f)
		   do (loop for k from 0 to 2
			    do (setf (aref pattern j i k) (elt color k)))))
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
    (t (error (format nil "Format ~a not supported" format)))))

(defun set-texture-image (texture image)
  (when (tex-pixels texture) (gl:free-gl-array (tex-pixels texture)))
  (setf (tex-width texture) (getf image :width)
	(tex-height texture) (getf image :height)
	(tex-gl-object-valid texture) nil)
  (let ((pixels  (gl:alloc-gl-array :unsigned-char (* (texel-size (tex-pixel-format texture))
						      (getf image :width)
						      (getf image :height)))))
    (loop for y from 0 to (1- (tex-height texture))
	  do (loop for x from 0 to (1- (tex-width texture))
		   do (loop for i from 0
			    while (< i (texel-size (getf image :format)))
			    do (setf (gl:glaref pixels (+ i
							  (* x (texel-size (tex-pixel-format texture)))
							  (* y (* (tex-width texture)
								  (texel-size (tex-pixel-format texture))))))
				     (if (< i (texel-size (tex-pixel-format texture)))
					 (aref (getf image :data) y x i)
					 1.0)))))
    (setf (tex-pixels texture) pixels)))

(defun make-texture (image &key (format :rgb)
			     (wrap-s :clamp-to-edge)
			     (wrap-t :clamp-to-edge)
			     (min-filter :linear)
			     (mag-filter :linear))
  (let ((texture (make-tex :pixel-format format
			   :wrap-s wrap-s :wrap-t wrap-t
			   :min-filter min-filter :mag-filter mag-filter)))
    (set-texture-image texture image)
    texture))

(defun use-texture (texture texture-unit)
  (if (not (tex-gl-object texture))
      (progn (setf (tex-gl-object texture) (gl:gen-texture))
	     (bind-gl-texture texture texture-unit)
	     (gl:tex-parameter :texture-2d :texture-wrap-s (tex-wrap-s texture))
	     (gl:tex-parameter :texture-2d :texture-wrap-t (tex-wrap-t texture))
	     (gl:tex-parameter :texture-2d :texture-min-filter (tex-min-filter texture))
	     (gl:tex-parameter :texture-2d :texture-mag-filter (tex-mag-filter texture)))
      (bind-gl-texture texture texture-unit))
  (when (not (tex-gl-object-valid texture))
    (set-texture-data texture)))

(defun bind-gl-texture (texture texture-unit)
  (gl:active-texture texture-unit)
  (gl:bind-texture :texture-2d (tex-gl-object texture)))

(defun set-texture-data (texture)
  (%gl:tex-image-2d :texture-2d 0
		    (gl::internal-format->int :rgb)
		    (tex-width texture) (tex-height texture)
		    0 (tex-pixel-format texture)
		    :unsigned-byte
		    (slot-value (tex-pixels texture) 'gl::pointer))
  (setf (tex-gl-object-valid texture) t))

(defvar texture-1 (make-texture (make-checker-pattern 500 :color-b '(0 255 0)
							  :color-f '(255 0 0))))
(defvar texture-2 (make-texture (make-checker-pattern 10 :color-b '(255 0 0)
							  :color-f '(0 255 0))
				:min-filter :linear
				:mag-filter :linear
				:wrap-s :clamp-to-edge
				:wrap-t :clamp-to-edge))


;; (defvar textures (make-hash-table))

;; (defun get-texture (name)
;;   (gethash name textures))

;; (defun add-texture (name data)
;;   (if (gethash name textures) (get-texture name))
;; )
