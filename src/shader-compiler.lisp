(in-package :cl-user)
(defpackage dummy-gl2
  (:use #:cl
	#:varjo))
(in-package :dummy-gl2)

(defun list-attrib-positions (in-args)
  (loop for arg in in-args
	collect (attrib-position (car arg))))

(defun gen-in-arg-strings (post-proc-obj)
  (with-slots (varjo::env) post-proc-obj
    (let* ((types (mapcar #'second (varjo::v-in-args varjo::env)))
	   (type-objs (mapcar #'type-spec->type types))
	   (locations (if (member :vertex (varjo::v-context varjo::env))
			  (list-attrib-positions (varjo::v-in-args varjo::env))
			  (loop for i below (length type-objs) collect nil))))
      (setf (in-args post-proc-obj)
	    (loop :for (name type-spec qualifiers glsl-name) :in (varjo::v-in-args varjo::env)
		  :for location :in locations :for type :in type-objs
		  :do (identity type-spec)
		  :collect
		  `(,name ,type ,@qualifiers ,@(list glsl-name)
			  ,(varjo::gen-in-var-string (or glsl-name name) type
						     qualifiers location))))))
  post-proc-obj)

(defun translate-dgl (in-args uniforms context body
		      &optional (third-party-metadata (make-hash-table)))
  (varjo::flow-id-scope
    (let ((env (varjo::%make-base-environment third-party-metadata)))
      (pipe-> (in-args uniforms context body env)
	#'varjo::split-input-into-env
	#'varjo::process-context
	#'varjo::add-context-glsl-vars
	#'varjo::process-in-args
	#'varjo::process-uniforms
	(equalp #'varjo::symbol-macroexpand-pass
		#'varjo::macroexpand-pass
		#'varjo::compiler-macroexpand-pass)
	#'varjo::compile-pass
	#'varjo::make-post-process-obj
	#'varjo::check-stemcells
	#'varjo::post-process-ast
	#'varjo::filter-used-items
	#'gen-in-arg-strings
	#'varjo::gen-out-var-strings
	#'varjo::final-uniform-strings
	#'varjo::dedup-strings
	#'varjo::final-string-compose
	#'varjo::code-obj->result-object))))

(defun translate-shader (uniforms version
			 &key vertex fragment)
  (let ((stages (list (when vertex
                        (list (first vertex)
                              uniforms
                              (list  :vertex version)
                              `(progn ,@(rest vertex))))
                      (when fragment
                        (list (first fragment)
                              uniforms
                              (list :fragment version)
                              `(progn ,@(rest fragment)))))))
    (rolling-translate (remove nil stages) #'translate-dgl)))
