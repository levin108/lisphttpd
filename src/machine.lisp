(in-package :lisphttpd)

(defclass machine ()
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port)
   (home :initarg :home :accessor home)))