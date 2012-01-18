(defpackage :lisphttpd-system
  (:use :cl :asdf))

(in-package :lisphttpd-system)

(defsystem lisphttpd
  :name "lisphttpd"
  :author "levin li"
  :version "0.0.1"
  :license "MIT"
  :description "A lightweight web server."
  :depends-on (:iolib :bordeaux-threads)
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "types")
     (:file "common")
     (:file "header")
     (:file "response")
     (:file "request")
     (:file "client")
     (:file "worker")
     (:file "network")
     (:file "server")))))
