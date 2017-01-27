(in-package :dummy-gl2)

;; (defparameter *constant-time*)
;; (defparameter *model-delta*)

;;; Scheduler is a the system that decides what should be done and when.
;;;
;;;
;;; some definitions:
;;; model - A thing that should be stepped
;;; delta - time passed since model was last stepped 
;;; 
;;; There are several strategies scheduler can use:
;;; :simple
;;; Steps model by real delta, each scheduler iteration
;;; input is delivered as soon as possible, in one chunk
;;; possible uses: Either processes which aren't
;;; sensitive to delta fluctuations, and are not expensive to redraw
;;; 
;;; :constant
;;; Steps process by constant time then waits until at least constant time has passed;
;;; input is delivered as soon as possible, in one chunk
;;; possible uses: Models which are not expensive to redraw, but are
;;; delta sensitive, drawing process
;;; 
;;; :multi-constan
;;; Process is stepped n times, then redrawn
;;; where n is (/ delta step-delta)
;;; possible uses: Real-time physics-based simulations;
;;; in general, models which are very sensitive to delta fluctuations
;;; and are 
;;;
;;; :input-bound
;;; Waits for input, advances model by one delta-less step, redraws
;;; possible uses: rogue-likes and such
;;;
;;; :processes-bound
;;; This process only needs to be stepped when some other process is ready
;;; possible uses: redraw
;;; 
;;; Multiple processes
;;; Sometimes it may be desirable to have multiple models (or rather
;;; processes in this case) to get effects such as running animations
;;; while waiting for input, this can of course be accomplished with
;;; having user to do all the management themselves,
;;;
;;; Multi threading
;;; 

(defvar *processes* nil
  "The processes to be stepped")

(defvar *active-process* nil
  "Currently stepped process")

(defvar *stepped-precesses* nil
  "What was stepped this iteration")

(defparameter +scheduler-strategies+
  '(:simple :constant :multi-constant :process-bound))

(defun scheduler-strategy-p (thing)
  (when (find thing +scheduler-strategies+) t))

(deftype scheduler-strategy ()
  `(satisfies scheduler-strategy-p))

;;; Process 
;;; Slots:
;;; 
;;; name - user-friendly name of the process (ex. 'Physics simulation', 'Display')
;;; strategy - strategy, scheduler will use when stepping this process
;;; delta - time passed since last step
;;; step-delta - time between steps for this process
;;; paused - This process is paused and should not be stepped
;;; mt-safe - this process can run in a separate thread, only to be paused when dependent process is stepped
;;; state - status of the process:
;;; Can be one of '(:running :paused :ready :waiting :finished :error)
;;;
;;;;; :running - The process is running, and should be stepped according to its strategy
;;;;; :paused - The process is paused, and should not be stepped
;;;;; :ready - The process has finished doing something, all the dependent processes should be stepped,
;;;;;          process state should be set to :running. 
;;;;; :finished - The process has finished, all the dependent process should be stepped.
;;;;; :error - The process has finished abnormally, all the dependent processes will also be terminated,
;;;;;          a condition describing what has happened to the process will be raised 
;;;;;          If any dependent process themselves have processes depending on them, they will be included
;;;;;          in this condition, all dependent processes and their dependent processes are to be terminated
;;; 
;;; depends-on - A list of processes this process depends on, each element should either
;;; be an instance of process struct or a list of an instance, state and state-extra it is waiting for.
;;; step-fn - A function to be called when the process is to be stepped, function signature being:
;;; (fn delta)


(defparameter +scheduler-process-states+
  '(:running :paused :ready :finished :error))

(defun scheduler-process-state-p (thing)
  (when (find thing +scheduler-process-states+) t))

(deftype scheduler-process-state ()
  '(satisfies scheduler-process-state-p))

(defstruct (process (:conc-name process-)
		    (:constructor make-process))
  "A structure describing a process being scheduled"  
  (name "" :type string)
  (strategy :simple :type scheduler-strategy)
  (delta 0 :type fixnum)
  (step-delta 0 :type fixnum)
  (paused t :type boolean)
  (mt-safe nil :type boolean)
  (state :running :type scheduler-process-state)
  (depends-on nil :type sequence)  
  (step-fn nil :type function))

(defmethod print-object ((obj process) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (process-name obj))))

(defun process-running-p (process)
  (declare (inline)
	   (type process process))
  (eq (process-state process) :running))

(defun process-paused-p (process)
  (declare (inline)
	   (type process process))
  (eq (process-state process) :paused))

(defun process-ready-p (process)
  (declare (inline)
	   (type process process))
  (eq (process-state process) :ready))

(defun process-finished-p (process)
  (declare (inline)
	   (type process process))
  (eq (process-state process) :finished))

(defun process-error-p (process)
  (declare (inline)
	   (type process process))
  (eq (process-state process) :error))

(defun scheduler-step-simple (process dt)
  (declare (inline)
	   (optimize speed))
  (push process *stepped-precesses*)
  (funcall (process-step-fn process) dt))

(defun scheduler-step-constant (process dt)
  (declare (inline)
	   (fixnum dt)
	   (optimize speed))
  (incf (process-delta process) dt)
  (when (>= (process-delta process) (process-step-delta process))
    (scheduler-step-simple process (process-step-delta process))
    (decf (process-delta process) (process-step-delta process))))

(defun scheduler-step-multi-constant (process dt)
  (declare (inline)
	   (fixnum dt)
	   (optimize speed))
  (incf (process-delta process) dt)
  (multiple-value-bind (n-iterations remainder)
      (truncate (process-delta process) (process-step-delta process))
    (when (not (zerop n-iterations))
      (loop for i from 1 to n-iterations
	    do (scheduler-step-simple process (process-step-delta process)))
      (setf (process-delta process) remainder))))

(defun scheduler-step-process-bound (process dt)
  (declare (inline)
	   (fixnum dt))
  (incf (process-delta process) dt)
  (when (loop for depp in (process-depends-on process)
	      always (find depp *stepped-precesses*))
    (scheduler-step-simple process dt)))

(defun scheduler-step-process (process)
  (case (process-state process)
    (:running (case (process-strategy process)
		(:simple (scheduler-step-simple process *runtime-delta*))
		(:constant (scheduler-step-constant process *runtime-delta*))
		(:multi-constant (scheduler-step-multi-constant process *runtime-delta*))
		(:process-bound (scheduler-step-process-bound process *runtime-delta*))))
    (:finished (progn (format nil "Process ~a has finished" (process-name process))
		      (remove-process process)))
    (:error (progn (remove-process process)
		   (warn (format nil "Process ~a is in a state of error" (process-name process)))))))

(defun scheduler-step ()
  (setf *stepped-precesses* nil)
  (loop for process in *processes*
	do (setf *active-process* process)
	   (scheduler-step-process process))
  (setf *active-process* nil))

(defun walk-dep-tree (prev next)
  (let ((last-proc nil))
    (loop for proc in (process-depends-on next)
	  do (setf last-proc proc)
	  if (find proc prev)
	    do (error (format nil "Process dependency loop detected ~{\"~a\"~^->~}"
    		     (mapcar #'process-name (append prev (list last-proc)))))
	  do (walk-dep-tree (append prev (list next)) proc))))

(defun collect-deps (procs)
  (loop for proc in procs
	if (process-depends-on proc)
	  append (collect-deps (process-depends-on proc))
	collect proc))

(defun find-deps (proc)
  (loop for dep in (process-depends-on proc)
	always (find dep *processes*)))

(defun sort-processes ()
  (loop for proc in *processes*
	if (not (find-deps proc))
	  do (error (format nil "Process ~a has dependencies not met" proc)))
  (let ((trees (collect-deps *processes*))
	(sorted))
    (loop for proc in trees
	  if (not (find proc sorted))
	    do (push proc sorted))
    (setf *processes* (reverse sorted))))

(defun add-process (proc)
  (walk-dep-tree nil proc)
  (if (find proc *processes*)
      (error (format nil "Process \"~a\" added twice" (process-name proc)))
      (push proc *processes*)))

(defun add-processes (&rest processes)
  (map nil #'add-process processes)
  (setf *processes*
	(sort *processes*
	      #'(lambda (proc-a proc-b)
		  (find proc-a (process-depends-on proc-b)))))
  (sort-processes))

(defun remove-process (proc)
  (setf *processes* (remove proc *processes*)))
