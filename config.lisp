(in-package :cl-user)
(defpackage dummy-gl2.config
  (:use :cl)
  (:export #:*root-dir*
	   #:*assets-dir*))
(in-package :dummy-gl2.config)

(defvar *root-dir* (asdf:system-source-directory :dummy-gl2))
(defvar *assets-dir* (merge-pathnames #P"assets/" *root-dir*))
