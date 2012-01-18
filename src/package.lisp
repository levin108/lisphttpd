(in-package :cl-user)

(defpackage :lisphttpd
  (:use :cl :iolib :bordeaux-threads)
  (:export :webstart
           :cleanup))

(in-package :lisphttpd)
