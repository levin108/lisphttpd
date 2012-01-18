(in-package :lisphttpd)

(deftype header-value-type () '(or integer string null))

(defclass http-header ()
  ((header-name  :initarg :header-name  :accessor header-name  :type 'string)
   (header-value :initarg :header-value :accessor header-value :type 'header-value-type))
)

(defun %make-http-header (name &optional value)
  (check-type name string "a string.")
  (check-type value header-value-type "a header value.")
  (make-instance 'http-header
                 :header-name name
                 :header-value value))

(defun make-http-header (obj name &optional value)
  (let ((hdr (%make-http-header name value)))
    (vector-push-extend hdr (headers obj))
    hdr))


(defun headers-to-string (headers)
  (let ((hdr-string (make-string 0)))
    (loop for hdr across headers do
         (with-slots (header-name header-value) hdr
           (setf hdr-string (concatenate 'string hdr-string header-name))
           (setf hdr-string (concatenate 'string hdr-string ": "))
           (cond
             ((integerp header-value)
              (setf hdr-string (concatenate 'string hdr-string
                                            (write-to-string header-value))))
             ((stringp header-value)
              (setf hdr-string (concatenate 'string hdr-string
                                            header-value))))
           )
         (setf hdr-string (concatenate 'string hdr-string #(#\Return #\Newline))))
    hdr-string))
