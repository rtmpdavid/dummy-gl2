(in-package :dummy-gl2)

(defun mult-mat4 (&rest matrices)
  (reduce #'m4:* matrices
	  :initial-value (rtg-math.matrix4:identity)))

(defun look-at (e l)
  (let* ((f (v:normalize (v:- l e)))
	 (s (v:cross f (v! 0.0 1.0 0.0)))
	 (u (v:cross s f)))
    (m4:*
     (m4:make (x s)     (y s)     (z s)     0.0
	      (x u)     (y u)     (z u)     0.0
	      (- (x f)) (- (y f)) (- (z f)) 0.0
	      0.0       0.0       0.0       1.0)
     (m4:make 1.0 0.0 0.0 (- (x e))
	      0.0 1.0 0.0 (- (y e))
	      0.0 0.0 1.0 (- (z e))
	      0.0 0.0 0.0 1.0))))

(defun look-vec (e view)
  
  (let* ((l (v:+ e (v:normalize view)))
	 (f (v:normalize (v:- l e)))
	 (s (v:cross f (v! 0.0 1.0 0.0)))
	 (u (v:cross s f)))
    (m4:*
     (m4:make (x s)     (y s)     (z s)     0.0
	      (x u)     (y u)     (z u)     0.0
	      (- (x f)) (- (y f)) (- (z f)) 0.0
	      0.0       0.0       0.0       1.0)
     (m4:make 1.0 0.0 0.0 (- (x e))
	      0.0 1.0 0.0 (- (y e))
	      0.0 0.0 1.0 (- (z e))
	      0.0 0.0 0.0 1.0))))

