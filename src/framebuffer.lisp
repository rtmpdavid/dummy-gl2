(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:rtmp-utils)
  (:export start-main-loop))
(in-package :dummy-gl2)

;; (gl:gen-texture)
;; (gl:bind-texture :texture-2d-multisample tex)
;; (%gl:tex-image-2d-multisample :texture-2d-multisample )
;; (gl:gen-framebuffer)
;; (gl:bind-framebuffer :framebuffer framebuffer)
;; (gl:gen-renderbuffers )
(defstruct (gl-renderbuffer (:conc-name renderbuffer-))
  (gl-object nil)
  (storage-bound-p nil)
  (samples nil)
  (format nil)
  (width 0)
  (height 0))

(defun bind-renderbuffer (rb &optional (realloc-storage nil))
  (when (not (renderbuffer-gl-object rb))
    (setf (renderbuffer-gl-object rb) (gl:gen-renderbuffer)))
  (gl:bind-renderbuffer :renderbuffer (renderbuffer-gl-object rb))
  (when (or realloc-storage
	    (not (renderbuffer-storage-bound-p rb)))
    (if (renderbuffer-samples rb)
	(%gl:renderbuffer-storage-multisample
	 :renderbuffer (renderbuffer-samples rb)
	 (renderbuffer-format rb)
	 (renderbuffer-width rb)
	 (renderbuffer-height rb))
	(gl:renderbuffer-storage
	 :renderbuffer
	 (renderbuffer-format rb)
	 (renderbuffer-width rb)
	 (renderbuffer-height rb)))))

(defstruct (gl-framebuffer (:conc-name framebuffer-))
  (gl-object nil)
  (gl-object-valid-p nil)
  (multisample)
  (color-attachment nil)
  (depth-attachment nil)
  (stencil-attachment nil))

(defun make-framebuffer (color &key (samples nil) (depth nil depth-p) (stencil nil stencil-p))
  (let* ((width (tex-width color))
	 (height (tex-height color)))
    (make-gl-framebuffer
     :multisample samples
     :color-attachment color
     :depth-attachment
     (if depth-p depth
	 (make-gl-renderbuffer :format :depth-component24
			       :samples samples
			       :width width :height height))
     :stencil-attachment
     (if stencil-p stencil
	 (make-gl-renderbuffer :format :stencil-index8
			       :samples samples
			       :width width :height height)))))

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
  (when (not (framebuffer-gl-object-valid-p fb))
    (attach-to-framebuffer fb :color-attachment0 (framebuffer-color-attachment fb))
    (attach-to-framebuffer fb :depth-attachment (framebuffer-depth-attachment fb))
    (attach-to-framebuffer fb :stencil-attachment (framebuffer-stencil-attachment fb))
    (case (gl:check-framebuffer-status :framebuffer)
      (:framebuffer-unsupported (warn "Framebuffer unsopported"))
      (:framebuffer-incomplete-attachment (warn "Framebuffer attachment incomplete"))
      (:framebuffer-incomplete-dimensions (warn "Framebuffer dimensions incomplete"))
      (:framebuffer-incomplete-missing-attachment (warn "No images attached to framebuffer"))
      (:framebuffer-complete (print "Framebuffer complete"))
      (:framebuffer-complete-oes (print "Framebuffer complete"))
      (t (warn (format nil "Framebuffer error ~a" (gl:check-framebuffer-status :framebuffer)))))
    (setf (framebuffer-gl-object-valid-p fb) t)))

(defun unbind-framebuffer ()
  (gl:bind-framebuffer :framebuffer 0))

