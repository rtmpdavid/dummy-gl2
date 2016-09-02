(in-package :cl-user)
(defpackage dummy-gl2
  (:use :cl
   :rtmp-utils))
(in-package :dummy-gl2)

(defvar gl-vaos (make-hash-table))
(defvar gl-arrays (make-hash-table))
(defvar gl-elements (make-hash-table))
(defvar gl-textures (make-hash-table))
(defvar gl-shaders (make-hash-table))

(defun invalidate-shader (name)
  (when (gethash name gl-shaders)
    (setf (gethash name gl-shaders) nil)))
