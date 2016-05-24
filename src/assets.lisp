(in-package :cl-user)
(defpackage dummy-gl2.assets
  (:use :cl
	:cl-fad
	#:dummy-gl2.config
	:rtmp-utils)
  (:export :list-assets))
(in-package :dummy-gl2.assets)

(defparameter *assets* (make-hash-table :test #'equal))

(defun list-dir (root-dir path)
  (let ((lst nil))
    (walk-directory
     (merge-pathnames path
		      (pathname root-dir))
     #'(lambda (dir)
	 (push (reduce
		#'(lambda (a b)
		    (concatenate 'string a (if a "/" "") b))
		(append (subseq (pathname-directory dir)
				(mismatch (pathname-directory root-dir)
					  (pathname-directory dir) :from-end t))
			(list (concatenate 'string (pathname-name dir) "." (pathname-type dir)))))
	       lst)))
    lst))

(defun list-assets ()
  (list-dir *assets-dir* *assets-dir*))

(defun load-asset (filename)
  (hash-memoize filename *assets*
    (%load-asset-switch filename)))

(defun absolute-asset-name (filename)
  (merge-pathnames filename *assets-dir*))

(defun load-text (filename)
  (with-output-to-string (out)
    (with-open-file (text (absolute-asset-name filename))
      (loop for c = (read-char text nil nil)
	 while c
	 do (write-char c out)))))

(defun load-png (filename)
  (with-open-file (img (absolute-asset-name filename) :element-type '(unsigned-byte 8))
    (let ((img (png:decode img)))
      (list :width (array-dimension img 1)
	    :height (array-dimension img 0)
	    :data (make-array (reduce #'* (array-dimensions img) :initial-value 1)
			      :element-type (array-element-type img)
			      :displaced-to img)))))

(defun %load-asset-switch (filename)
  (if (null (pathname-type filename))
      (load-text filename)
      (string-case (pathname-type filename)
	("png" (load-png filename))
	("txt" (load-text filename))
	(t (error (format nil "Don't know how to load a ~a" (pathname-type filename)))))))
