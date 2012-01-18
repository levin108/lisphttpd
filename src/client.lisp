(in-package :lisphttpd)

(defclass web-client ()
  ((connection :initarg :connection :accessor connection)
   (request    :initarg :request    :accessor request  :type 'http-request)
   (response   :initarg :response   :accessor response :type 'http-response)
   ;; whether this client is waiting for a read event or an write event
   (event-type :initarg :event-type :accessor event-type)
   ))

(defgeneric make-web-client (net &key state)
  (:documentation "Make a web client object."))

(defmethod make-web-client ((net web-connection) &key state)
  (declare (ignore state))
  (make-instance 'web-client
                 :connection net
                 :request (make-http-request)
                 :response (make-http-response)))

(defmethod make-web-client ((net stream-socket) &key (state 'CONN_STATE_ACCEPT))
  (let ((client nil)
        (socket (make-instance 'web-connection
                               :socket net
                               :state state))
        (request (make-http-request)))
    (setf client (make-instance 'web-client
                                :connection socket
                                :request request
                                :response (make-http-response)))
    client))