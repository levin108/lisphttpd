(in-package :lisphttpd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +http-error-400-page+
    "<html>
<head><title>400 Bad Request</title></head>
<body bgcolor=\"white\">
<h1>400 Bad Request</h1>
Your browser sent an invalid request . <br/>
<small>lisphttpd by levin li</small>
</body></html>")

  (defconstant +http-error-404-page+
    "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">
<html><head>
<title>404 Not Found</title>
</head><body>
<h1>Not Found</h1>
<p>The requested URL ~A was not found on this server.</p>
</body></html>")

)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *resp-code-table* nil)
  (defconstant +http-ok+                       200)
  (defconstant +http-created+                  201)
  (defconstant +http-accepted+                 202)
  (defconstant +http-no-content+               204)
  (defconstant +http-partial-content+          206)

  (defconstant +http-special-response+         300)
  (defconstant +http-moved-permanently+        301)
  (defconstant +http-moved-temporarily+        302)
  (defconstant +http-see-other+                303)
  (defconstant +http-not-modified+             304)

  (defconstant +http-bad-request+              400)
  (defconstant +http-unauthorized+             401)
  (defconstant +http-forbidden+                403)
  (defconstant +http-not-found+                404)
  (defconstant +http-not-allowed+              405)
  (defconstant +http-request-time-out+         408)
  (defconstant +http-conflict+                 409)
  (defconstant +http-length-required+          411)
  (defconstant +http-precondition-failed+      412)
  (defconstant +http-request-entity-too-large+ 413)
  (defconstant +http-request-uri-too-large+    414)
  (defconstant +http-unsupported-media-type+   415)
  (defconstant +http-range-not-satisfiable+    416)
  
  (defconstant +http-internal-server-error+    500)
  (defconstant +http-not-implemented+          501)
  (defconstant +http-bad-gateway+              502)
  (defconstant +http-service-unavailable+      503)
  (defconstant +http-gateway-time-out+         504)
  (defconstant +http-insufficient-storage+     507))

(defun init-resp-code ()
  (setf *resp-code-table*
        (make-array 4
         :element-type '(array string (*))
         :initial-contents
         #(("200 OK"
            "201 Created"
            "202 Accepted"
            "203 null_string"
            "204 No Content"
            "205 null_string"
            "206 Partial Content")
           
           ("300 null_string"
            "301 Moved Permanently"
            "302 Moved Temporarily"
            "303 See Other"
            "304 Not Modified")
           
           ("400 Bad Request"
            "401 Unauthorized"
            "402 Payment Required"
            "403 Forbidden"
            "404 Not Found"
            "405 Not Allowed"
            "406 not acceptable"
            "407 null_string"
            "408 Request Time-out"
            "409 Conflict"
            "410 Gone"
            "411 Length Required"
            "412 Precondition Failed"
            "413 Request Entity Too Large"
            "414 Request URI Too Large"
            "415 Unsupported Media Type"
            "416 Requested Range Not Satisfiable")
           
           ("500 Internal Server Error"
            "501 Method Not Implemented"
            "502 Bad Gateway"
            "503 Service Temporarily Unavailable"
            "504 Gateway Time-out"
            "505 null_string"
            "506 null_string"
            "507 Insufficient Storage")))))

(defclass http-response ()
  ((code          :initarg :code          :accessor code          :type 'integer)
   (phrase        :initarg :phrase        :accessor phrase        :type 'string)
   (major-version :initarg :major-version :accessor major-version :type 'integer)
   (minor-version :initarg :minor-version :accessor minor-version :type 'integer)
   (headers       :initarg :headers       :accessor headers       :type '(vector http-header (*)))
   (body          :initarg :body          :accessor body          :type 'string)
   (write-vector  :initarg :write-vector  :accessor write-vector  :type 'ub8-vector)
   (write-pos     :initarg :write-pos     :accessor write-pos     :type 'integer))
  )

(defun make-http-response (&optional code)
  (let ((obj (make-instance 'http-response
                            :code code
                            :major-version 1
                            :minor-version 1
                            :phrase ""
                            :body ""
                            :headers
                            (make-array 0 :element-type 'http-header
                                        :fill-pointer 0 :adjustable t)
                            :write-pos 0)))
    (when code
        (multiple-value-bind (class index) (floor code 100)
          (setf (phrase obj) (elt (aref *resp-code-table* (- class 2)) index)))
        (debug-info 'DEBUG-INFO "response phrase: ~A." (phrase obj)))
    obj))
        
(defmacro set-response-code (obj code)
  (with-gensyms (class index)
    `(progn
       (setf (code ,obj) ,code)
       (multiple-value-bind (,class ,index) (floor ,code 100)
         (setf (phrase ,obj) (elt (aref *resp-code-table* (- ,class 2)) ,index))))))

(defmacro set-response-body (obj body)
  `(setf (body ,obj) ,body))

(defun resp-to-vector (resp)
  (with-slots (phrase major-version minor-version headers body) resp
    (let ((vec (format nil "HTTP/~D.~D ~A~C~C~A~C~C~A"
                       major-version minor-version
                       phrase #\Return #\Newline
                       (headers-to-string headers)
                       #\Return #\Newline body)))
      (setf (write-vector resp) (map 'vector #'char-code vec))
      (write-vector resp))))
