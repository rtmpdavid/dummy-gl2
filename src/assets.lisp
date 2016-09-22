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
    (let ((img (png:decode img :preserve-alpha t)))
      (list :width (array-dimension img 1)
      	    :height (array-dimension img 0)
      	    :data img))))

(defun load-obj (filename)
  (let* ((obj (with-open-file (img (absolute-asset-name filename))
		(obj-parser:parse-obj img t))))
    obj))

(defun %load-asset-switch (filename)
  (if (null (pathname-type filename))
      (load-text filename)
      (string-case (pathname-type filename)
	("png" (load-png filename))
	("txt" (load-text filename))
	("obj" (load-obj filename))
	(t (error (format nil "Don't know how to load a ~a" (pathname-type filename)))))))


