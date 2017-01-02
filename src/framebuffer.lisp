(in-package :dummy-gl2)

(defun color-attachment-n (n)
  (when (> n
	   (cffi::foreign-enum-value '%gl:enum :max-color-attachments))
    (error (format nil "Your opengl implementation does not support this many color attachments (~a)" n) ))
  (+ (cffi::foreign-enum-value '%gl:enum :color-attachment0)
     n))

(defstruct (gl-renderbuffer (:conc-name renderbuffer-))
  (gl-object nil)
  (gl-object-valid-p nil)
  (samples nil)
  (format nil)
  (width 0)
  (height 0))

(defun bind-renderbuffer (rb &optional (realloc-storage nil))
  (when (not (renderbuffer-gl-object rb))
    (setf (renderbuffer-gl-object rb) (gl:gen-renderbuffer)))
  (gl:bind-renderbuffer :renderbuffer (renderbuffer-gl-object rb))
  (when (or realloc-storage
	    (not (renderbuffer-gl-object-valid-p rb)))
    (if (renderbuffer-samples rb)
	(%gl:renderbuffer-storage-multisample
	 :renderbuffer
	 (renderbuffer-samples rb)
	 (renderbuffer-format rb)
	 (renderbuffer-width rb)
	 (renderbuffer-height rb))
	(gl:renderbuffer-storage
	 :renderbuffer
	 (renderbuffer-format rb)
	 (renderbuffer-width rb)
	 (renderbuffer-height rb)))
    (setf (renderbuffer-gl-object-valid-p rb) t)))

(defun set-renderbuffer-size (rb width height)
  (setf (renderbuffer-width rb) width
	(renderbuffer-height rb) height
	(renderbuffer-gl-object-valid-p rb) nil))

(defstruct (gl-framebuffer (:conc-name framebuffer-))
  (gl-object nil)
  (gl-object-valid-p nil)
  (size nil)
  (multisample)
  (color-attachments nil)
  (depth-stencil-attachment nil)
  (stencil-p nil))

(defun make-framebuffer (&key
			   (color-attachments nil color-p)
			   (color-n 1)
			   (color-size nil)
			   (samples nil)
			   (depth-stencil nil depth-stencil-p)
			   (stencil nil)
			   (depth-stencil-size nil))
  (when (and (not color-p) (not color-size))
    (error "Can't make framebuffer, no color attachments or color attachment size provided"))
  (let ((color (if (listp color-attachments) color-attachments
		   (list color-attachments))))    
    (when color-attachments
      (setf color-size (attachment-size (first color))))
    (when depth-stencil-p
      (setf depth-stencil-size (attachment-size depth-stencil)))
    (when (not depth-stencil-size)
      (setf depth-stencil-size color-size))
    (make-gl-framebuffer
     :size color-size
     :multisample samples
     :color-attachments
     (append color
	     (loop for i from 0 to (- color-n (length color))
		   collect (make-gl-renderbuffer :width (elt color-size 0)
						 :height (elt color-size 1)
						 :format :rgb
						 :samples samples)))
     :depth-stencil-attachment
     (if depth-stencil-p depth-stencil
	 (make-gl-renderbuffer :format (if stencil :depth24-stencil8
					   :depth-component24)
			       :samples samples
			       :width (elt depth-stencil-size 0)
			       :height (elt depth-stencil-size 1)))
     :stencil-p stencil)))

(defun attach-texture (fb tex attachment)
  (use-texture tex :texture0 (if (framebuffer-multisample fb)
				 :texture-2d-multisample
				 :texture-2d)
	       (framebuffer-multisample fb))
  (gl:framebuffer-texture-2d :framebuffer
			     attachment
			     (if (framebuffer-multisample fb)
				 :texture-2d-multisample
				 :texture-2d)
			     (tex-gl-object tex)
			     0))

(defun attach-renderbuffer (rb attachment)
  (bind-renderbuffer rb)
  (gl:framebuffer-renderbuffer :framebuffer
			       attachment
			       :renderbuffer
			       (renderbuffer-gl-object rb)))

(defun attach-to-framebuffer (fb attachment target)
  (if (gl-texture-p target) (attach-texture fb target attachment)
      (attach-renderbuffer target attachment)))

(defun bind-framebuffer (fb &optional (target :framebuffer))
  (when (not (framebuffer-gl-object fb))
    (setf (framebuffer-gl-object fb) (gl:gen-framebuffer)))
  (gl::bind-framebuffer target (framebuffer-gl-object fb))
  (apply #'gl:viewport (append (list 0 0) (framebuffer-size fb)))
  (when (not (framebuffer-gl-object-valid-p fb))
    (loop for attachment in (framebuffer-color-attachments fb)
	  for n from 0 
	  do (attach-to-framebuffer fb (color-attachment-n n)
				    attachment))
    (attach-to-framebuffer fb (if (framebuffer-stencil-p fb)
			      	  :depth-stencil-attachment
			   	  :depth-attachment)
			   (framebuffer-depth-stencil-attachment fb))
    (case (gl:check-framebuffer-status :framebuffer)
      (:framebuffer-unsupported (warn "Framebuffer unsopported"))
      (:framebuffer-incomplete-attachment (warn "Framebuffer attachment incomplete"))
      (:framebuffer-incomplete-dimensions (warn "Framebuffer dimensions incomplete"))
      (:framebuffer-incomplete-missing-attachment (warn "No images attached to framebuffer"))
      (:framebuffer-complete t)
      (:framebuffer-complete-oes t)
      (t (warn (format nil "Framebuffer error ~a" (gl:check-framebuffer-status :framebuffer)))))
    (setf (framebuffer-gl-object-valid-p fb) t)))

(defun unbind-framebuffer ()
  (gl:bind-framebuffer :framebuffer 0)
  (apply #'gl:viewport (append (list 0 0) (window-size *window*))))

(defun do-blit (src-rect dst-rect mask filter)
  (%gl:blit-framebuffer (rx0 src-rect) (ry0 src-rect) (rx1 src-rect) (ry1 src-rect)
			(rx0 dst-rect) (ry0 dst-rect) (rx1 dst-rect) (ry1 dst-rect)
			mask filter))

(defun find-color-attachment (fb attachment)
  "Okay, so we got something not matched by obvious things. Lets see what we can do"
  (when (or (not (keywordp attachment)))
    (error (format nil "Don't know what kind of attachment ~a is" attachment)))
  (let ((n (- (cffi::foreign-enum-value '%gl:enum attachment)
	      (cffi::foreign-enum-value '%gl:enum :color-attachment0))))
    (when (or (< n 0)
	      (> n (cffi::foreign-enum-value '%gl:enum :max-color-attachments)))
      (error (format nil "Wrong cenum: ~a" attachment)))
    (values (elt (framebuffer-color-attachments fb) n)
	    (color-attachment-n n))))

(defun find-attachment (fb attachment)
  (cond
    ((numberp attachment) (values (elt (framebuffer-color-attachments fb) attachment)
				  (color-attachment-n attachment)))
    ((eq :depth-stencil-attachment attachment) (values
						(framebuffer-depth-stencil-attachment fb)
						:depth-stencil-attachment))
    ((eq :depth-attachment attachment) (values
					(framebuffer-depth-stencil-attachment fb)
					:depth-attachment))
    ((eq :stencil-attachment attachment) (error "stencil attachments not supported yet"))
    (t (multiple-value-prog1 (find-color-attachment fb attachment)))))

(defun prepare-blit-get-info (fb attachment rectangle target)
  "Figure out and does things necessary for the framebuffer blitting operations."
  (if (gl-framebuffer-p fb)
      (progn
	(bind-framebuffer fb target)
	(multiple-value-bind (a sa) (find-attachment fb attachment)
	  (setf attachment sa)
	  (when (not rectangle)
	    (let ((size (attachment-size a)))
	      (setf rectangle
		    (make-rect 0 0 (elt size 0) (elt size 1)))))))
      (progn
	(gl:bind-framebuffer target fb)
	(when (not rectangle)
	  (setf rectangle (make-rect 0 0 (window-w *window*) (window-h *window*))))))
  (values attachment rectangle))

(defun blit-framebuffer (fb-src &key
				  (fb-dest 0)
				  (read-buffer :color-attachment0) 
				  (filter :nearest)
				  (mask :color-buffer-bit)
				  (src-rect nil)
				  (dst-rect nil))
  (multiple-value-bind (src-attachment src-rectangle)
      (prepare-blit-get-info fb-src read-buffer src-rect :read-framebuffer)
    (gl:read-buffer src-attachment)
    (multiple-value-bind (dst-attachment dst-rectangle)
	(prepare-blit-get-info fb-dest nil dst-rect :draw-framebuffer)
      (do-blit src-rectangle dst-rectangle mask filter)))
  (gl:bind-framebuffer :draw-framebuffer 0)
  (gl:bind-framebuffer :read-framebuffer 0))

(defun attachment-size (attachment)
  (if (gl-texture-p attachment)
      (list (tex-width attachment)
	    (tex-height attachment))
      (list (renderbuffer-width attachment)
	    (renderbuffer-height attachment))))

(defun set-attachment-size (attachment width height)
  (when attachment
    (if (gl-texture-p attachment) (set-texture-size attachment width height)
	(set-renderbuffer-size attachment width height))))

(defun set-color-attachment-size (fb attachment-n width height)
  (set-attachment-size (elt (framebuffer-color-attachments fb) attachment-n) width height))

(defun set-framebuffer-size (fb width height)
  (set-attachment-size (elt (framebuffer-color-attachments fb) 0) width height)
  (set-attachment-size (framebuffer-depth-stencil-attachment fb) width height)
  (setf (framebuffer-size fb) (list width height)
	(framebuffer-gl-object-valid-p fb) nil))

(defun free-framebuffer (fb &optional (clear-textures t))
  (let ((renderbuffers nil)
	(textures nil))
    (flet ((push-relevant (thing)
	     (if (gl-texture-p thing)
		 (push thing textures)
		 (push (renderbuffer-gl-object thing) renderbuffers))))
     (let ((da (framebuffer-depth-stencil-attachment fb)))
       (when da (push-relevant da)))
      (loop for ca in (framebuffer-color-attachments fb)
	    do (push-relevant ca)))
    (gl:delete-renderbuffers renderbuffers)
    (when clear-textures
      (map nil #'free-texture-data textures)))
  (when (framebuffer-gl-object fb)
    (gl:delete-framebuffers (list (framebuffer-gl-object fb)))))
