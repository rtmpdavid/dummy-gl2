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
;;; :one-off
;;; This process will be ran once after a specified delta has passed
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

(defvar *processes-to-remove* nil
  "These should be removed from scheduler at the end of the current iteration")

(defvar *active-process* nil
  "Currently stepped process")

(defvar *stepped-processes* nil
  "What was stepped this iteration")

(defvar *to-sleep* nil)

(defparameter +scheduler-strategies+
  '(:simple :constant :multi-constant :process-bound))

(defun scheduler-strategy-p (thing)
  (when (find thing +scheduler-strategies+) t))

(deftype scheduler-strategy ()
  `(satisfies scheduler-strategy-p))

(defvar *lambda-proc-counter* 0)

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
;;;;; :waiting - The process is waiting for something
;;;;; :ready - The process has finished doing something, all the dependent processes should be stepped,
;;;;;          process state should be set to :running. 
;;;;; :finished - The process has finished, all the dependent process should be stepped.
;;;;; :error - The process has finished abnormally, all the dependent processes will also be terminated,
;;;;;          a condition describing what has happened to the process will be raised 
;;;;;          If any dependent process themselves have processes depending on them, they will be included
;;;;;          in this condition, all dependent processes and their dependent processes are to be terminated
;;;
;;; depends-on - A list of processes this process should be stepped after
;;;
;;; conditions - A list of conditions for this process to run
;;;
;;; condition: (:type  &optional arg1 
;;; 
;;; depends on, each element should either
;;; be an instance of process struct or a pair of process instance and state it is waiting for.
;;; 
;;; step-fn - A function to be called when the process is to be stepped, function signature being:
;;; (fn delta)
;;;
;;; finish-fn - A function to be called when the process is finished (Optional)
;;; 
;;; run-after - A list of processes after which this process will run
;;;
;;; repeat - If and how the process should be repeated
;;;;; nil - process will removed after first iteration
;;;;; t - process will be repeated indefinitely
;;;;; integer - process will be  repeated fixed number of times
;;; end-time - How long a process should be ran for
;;;;; nil - process has no time constraints
;;;;; integer - process will be ran until end-time

(defmacro unrorred (thing list)
  `(or ,@(loop for elt in list
	 collect `(eq ,thing ,elt))))

(defun scheduler-process-state-p (thing)
  (declare (optimize speed))
  (when (find thing '(:running :paused :ready :finished :error) :test #'eq) t))

(deftype scheduler-process-state ()
  '(satisfies scheduler-process-state-p))

(defstruct (process (:conc-name process-) (:constructor %make-process))
  "A structure describing a process being scheduled"  
  (name "" :type string)
  (strategy :simple :type scheduler-strategy)
  (delta 0 :type fixnum)
  (step-delta 1000 :type fixnum)
  (repeat nil :type (or integer boolean))
  (end-time nil :type (or integer null))
  (mt-safe nil :type boolean)
  (state :running :type scheduler-process-state)
  (depends-on nil :type (or cons null))
  (conditions nil :type (or cons null))
  (step-fn nil :type function)
  (finish-fn nil :type (or function null)))

(defun make-process (step-fn &key (name "") (strategy :simple) (step-delta 1000) (repeat t) (end-time nil) (depends-on nil)  (finish-fn nil))
  (%make-process :name name		 
		 :strategy strategy
		 :delta 0
		 :step-delta step-delta
		 :repeat repeat		 
		 :end-time (when end-time (+ (get-run-time) end-time))
		 :mt-safe nil
		 :depends-on depends-on
		 :conditions nil
		 :state :running
		 :step-fn step-fn
		 :finish-fn finish-fn))

(defmethod print-object ((obj process) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (process-name obj))))

(defun (setf process-state) (value process)
  (setf (slot-value process 'state) value)
  (if (eq value :running)
      (update-to-sleep (- (process-step-delta process) (process-delta process)))))

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

(defun update-to-sleep (delta)
  (when (or (not *to-sleep*)
	    (and (< delta *to-sleep*)
		 (>= delta (ticks-ms 1))))
    (setf *to-sleep* delta)))

(defun scheduler-step-simple (process dt)
  (declare (inline)
	   (optimize speed))
  (push process *stepped-processes*)
  (funcall (process-step-fn process) dt))

(defun scheduler-step-constant (process dt)
  (declare (inline)
	   (fixnum dt)
	   (optimize speed))
  (incf (process-delta process) dt)
  (when (>= (process-delta process) (process-step-delta process))
    (scheduler-step-simple process (process-step-delta process))
    (decf (process-delta process) (process-step-delta process))
    (update-to-sleep (- (process-step-delta process) (process-delta process)))))

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
      (setf (process-delta process) remainder)
      (update-to-sleep (+ (process-step-delta process) (process-delta process))))))

(defun dep-satisfies-p (dep)
  (declare (inline)
	   (optimize speed))
  (if (process-p dep) (find dep *stepped-processes*)
      (eq (process-state (car dep)) (cdr dep))))

(defun scheduler-step-process-bound (process dt)
  (declare (inline)
	   (fixnum dt))
  (incf (process-delta process) dt)
  (when (loop for dep in (process-depends-on process)
	      always (dep-satisfies-p dep))
    (scheduler-step-simple process dt)))

;; (defun process-remove-finished-deps
;;     (loop for dep in (process-wa)))

;; (defun process-check-wait-conditions (process)
;;   (case (process-wa)))

(defun process-do-step (process)
  (progn (case (process-strategy process)
	   (:simple (scheduler-step-simple process *runtime-delta*))
	   (:constant (scheduler-step-constant process *runtime-delta*))
	   (:multi-constant (scheduler-step-multi-constant process *runtime-delta*))
	   (:process-bound (scheduler-step-process-bound process *runtime-delta*)))
	 (with-slots (state repeat end-time finish-fn) process
	   (if repeat
	       (when (and (integerp repeat) (zerop (decf repeat))) (setf state :finished))
	       (setf state :finished))
	   (when (and end-time (<= end-time (get-run-time))) (setf state :finished))
	   (when (and (eq state :finished) finish-fn) (funcall finish-fn)))))

(defun scheduler-step-process (process)
  (case (process-state process)
    (:running (process-do-step process))
    (:waiting (when (process-check-wait-conditions process)) (process-do-step process))
    (:finished (progn (push process *processes-to-remove*)
		      (format nil "Process ~a has finished" (process-name process))))
    (:error (progn (push process *processes-to-remove*)
		   (warn (format nil "Process ~a is in a state of error" (process-name process)))))))

(defun scheduler-step ()
  (when *to-sleep*
    (decf *to-sleep* *runtime-delta*)
    (when (<= *to-sleep* 0) (setf *to-sleep* nil)))
  (when (not *to-sleep*)
    (setf *stepped-processes* nil
	  *to-sleep* nil)
    (loop for process in *processes*
	  do (setf *active-process* process)
	     (scheduler-step-process process))
    (if *processes-to-remove*
	(setf *processes* (loop for proc in *processes*
				if (find proc *processes-to-remove*)
				  do (setf *processes-to-remove*
					   (delete proc *processes-to-remove*))
				else
				  collect proc)
	      *processes-to-remove* nil))
    (setf *active-process* nil)
    (when *to-sleep* (delay *to-sleep*))))

(defun walk-dep-tree (prev next)
  (let ((last-proc nil))
    (loop for dep in (process-depends-on next)
	  for proc = (dependency-process dep)
	  do (setf last-proc proc)
	  if (find proc prev)
	    do (error (format nil "Process dependency loop detected ~{\"~a\"~^->~}"
			      (mapcar #'process-name (append prev (list last-proc)))))
	  do (walk-dep-tree (append prev (list next)) proc))))

(defun dependency-process (dep)
  (if (process-p dep) dep
      (car dep)))

(defun collect-deps (procs)
  (loop for proc in procs
	if (process-depends-on proc)
	  append (collect-deps (mapcar #'dependency-process (process-depends-on proc)))
	collect proc))

(defun find-deps (proc)
  (loop for dep in (process-depends-on proc)
	always (find (dependency-process dep) *processes*)))

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

(defun schedule-process (proc)
  (walk-dep-tree nil proc)
  (if (find proc *processes*)
      (error (format nil "Process \"~a\" added twice" (process-name proc)))
      (push proc *processes*)))

(defun schedule-processes (&rest processes)
  (map nil #'schedule-process processes)
  (setf *processes*
	(sort *processes*
	      #'(lambda (proc-a proc-b)
		  (find proc-a (process-depends-on proc-b)))))
  (sort-processes))

(defun remove-process (proc)
  (setf *processes* (remove proc *processes*)))

(defmacro defprocess ((name delta &key (friendly-name
					(concatenate 'string "proc-"
						     (symbol-name name)))
				    (repeat t)
				    (strategy :simple)
				    (step-delta 1000000)
				    (depends-on nil))
		      &rest body)  
  `(progn
     (defun ,name (,delta) ,@body)
     (when (not (boundp ',name))
       (defvar ,name nil))     
     (if (process-p ,name)
	 (setf (process-strategy ,name) ,strategy
	       (process-step-delta ,name) ,step-delta
	       (process-depends-on ,name) ,depends-on)
	 (setf ,name  (make-process #'(lambda (dt) (,name dt))
				    :name ,friendly-name
				    :strategy ,strategy
				    :step-delta ,step-delta
				    :depends-on ,depends-on
				    :repeat ,repeat)))
     (when (not (find ,name *processes*))
       (schedule-processes ,name))))


(defmacro defmain ((delta) &rest body)
  `(defprocess (,(intern "MAIN") ,delta :friendly-name "Main"
					:step-delta (/ (ticks-s 1) 60)
					:strategy :constant
					:repeat t)
     ,body))

(defmacro lambda-process ((delta &key (repeat nil) (step-delta 0) (strategy :simple) (depends-on nil)) &rest body)
  (with-gensyms (proc)
    `(let ((,proc (make-process #'(lambda (,delta) ,@body)
				:name (format nil "Lambda Process ~d" (1- (incf *lambda-proc-counter*)))
				:strategy ,strategy
				:depends-on ,depends-on
				:step-delta ,step-delta
				:repeat ,repeat)))
       (schedule-processes ,proc)
       ,proc)))

(defun append-finish-fn (process finish-fn &optional (before t))
  (setf (process-finish-fn process)
	(let ((old-fn (process-finish-fn process)))
	  (if old-fn
	      (if before #'(lambda ()
			     (funcall finish-fn)
			     (funcall old-fn))
		  #'(lambda ()
		      (funcall old-fn)
		      (funcall finish-fn)))
	      finish-fn))))

(defun schedule-sequence (&rest processes)
  (apply
   #'schedule-processes
   (loop with procs = processes
	 while procs
	 for proc = (car procs)
	 for next = (cadr procs)
	 do (setf procs (cdr procs))
	 collect proc
	 if next
	   do (let ((next-proc next))
		(append-finish-fn proc #'(lambda () (setf (process-state next-proc) :running)) nil))
	      (setf (process-state next) :paused))))



(defun schedule-while (process &rest processes)
  (apply #'schedule-processes processes)
  (append-finish-fn process #'(lambda () (map nil #'(lambda (proc) (setf (process-state proc) :finished))
					      processes))))
