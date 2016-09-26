(in-package :dummy-gl2.base-gl)

(deftype gl-name ()
  `(or null (integer 0 ,most-positive-fixnum )))

(defstruct (gl-object (:conc-name gl-object-))
  (object nil :type gl-name)
  (validp nil :type boolean)
  (destructor nil :type (or symbol function)))

