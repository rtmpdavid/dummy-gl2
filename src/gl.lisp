(in-package :dummy-gl2.base-gl)

(deftype gl-name ()
  `(or null (integer 0 ,most-positive-fixnum )))

(defstruct (gl-object (:conc-name gl-object-))
  (object nil :type gl-name)
  (validp nil :type boolean)
  (destructor nil :type (or symbol function)))

(defvar gl-state (make-hash-table))

(defun gl-state-enable (target)
  (if (not (gethash target gl-state))
      (progn
	(gl:enable target)
	(setf (gethash target gl-state) t))))

(defun gl-state-disable (target)
  (if (gethash target gl-state)
      (progn
	(gl:disable target)
	(setf (gethash target gl-state) nil))))

(defstruct (gl-buffer-object (:conc-name xbo-)
			     (:include gl-object))
  (target nil)
  (usage nil)
  (pointer nil))

(defun xbo-bind (buffer)
  (when (not (xbo-object buffer))
    (setf (xbo-object buffer) (gl:gen-buffer)
	  (xbo-validp buffer) nil))
  (gl:bind-buffer (xbo-target buffer) (xbo-object buffer))
  (when (not (xbo-validp buffer))
    (xbo-data buffer)
    (setf (xbo-validp buffer) nil)))

(defun xbo-unbind (buffer)
  (gl:bind-buffer (xbo-target buffer) 0))

(defun xbo-data (buffer)
  (if (not (xbo-pointer buffer))
      (error (format nil "Buffer (~a ~a) has no data"
		     (xbo-target buffer)
		     (xbo-usage buffer)))
      (let ((ptr (xbo-pointer buffer)))
	(%gl:buffer-data (xbo-target buffer) (gl:gl-array-byte-size ptr)
			 (gl::gl-array-pointer ptr) (xbo-usage buffer)))))

(defun xbo-free (buffer)
  (when (xbo-pointer (gl:free-gl-array (xbo-pointer buffer)))))

(defun xbo-alloc (buffer type count &key (free-if-allocated nil) (free-if-mismatched t))
  (when (xbo-pointer buffer)
   (when free-if-allocated
     (xbo-free buffer))
   (when (and free-if-mismatched
	      (or (not (eq type (gl::gl-array-type (xbo-pointer buffer))))
		  (/= (* (c-sizeof type) count) (gl:gl-array-byte-size (xbo-pointer buffer)))))
     (xbo-free buffer)))
  (setf (xbo-pointer buffer) (gl:alloc-gl-array type count)))
 
(defun xbo-fill (buffer data &key (offset 0) (offset-data 0) (count (length data)))
  (loop for i-p from offset
	for i-d from offset-data
	do (setf (gl:glaref (xbo-pointer buffer) i-p) (aref data i-d))
	repeat count))

(defstruct (gl-array-object (:conc-name vao-)
			    (:include gl-object))
  (vbo)
  (vbo-offset 0 :type fixnum)
  (vbo-length 0 :type fixnum)
  (ebo)
  (layout))

(defun vao-set-attrib-pointers (vao)
  (let ((layout (vao-layout vao)))
   (dolist (attrib layout)
     (%gl:enable-vertex-attrib-array (attrib-position attrib))
     (%gl:vertex-attrib-pointer (attrib-position attrib)
				(attrib-size attrib)
				:float
				:false
				(* (c-sizeof :float)
				   (layout-size layout))
				(cffi:inc-pointer
				 (cffi:null-pointer)
				 (* (c-sizeof :float)
				    (+ (vao-vbo-offset vao)
				       (attrib-offset layout attrib))))))))

(defun vao-bind (vao)
  (when (not (vao-object vao))
    (setf (vao-object vao) (gl:gen-vertex-array)))
  (gl:bind-vertex-array (vao-object vao))
  (when (not (vao-validp vao))
    (xbo-bind (vao-vbo vao))
    (xbo-bind (vao-ebo vao))
    (vao-set-attrib-pointers vao)
    ;; (setf (vao-validp vao) t)
    (xbo-unbind (vao-vbo vao))
    (xbo-unbind (vao-ebo vao))))

(defun vao-unbind ()
  (gl:bind-vertex-array 0))
