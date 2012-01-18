(in-package :lisphttpd)

(defun make-http-event-loop (conn client)
  (lambda (fd event exception)
    (declare (ignore fd exception)
             (inline make-web-client))
      (let ((new-client client) (sig 0))
        (if (null new-client)
            (setf new-client (make-web-client conn)))

        (cond
          ((eq event :read)
           ;; push the task to the queue
           (debug-info 'DEBUG-INFO "pushed a read event.")
           (acquire-lock *task-read-lock*)
           (vector-push-extend new-client *task-read-queue*)
           (setf sig 10)
           (release-lock *task-read-lock*))
          ((eq event :write)
           (debug-info 'DEBUG-INFO "pushed a write event.")
           (acquire-lock *task-write-lock*)
           (vector-push-extend new-client *task-write-queue*)
           (setf sig 12)
           (release-lock *task-write-lock*)))

        ;; wakeup a worker thread to process.
        (let ((pipe (pipe-writer (elt *thread-pool* 0))))
          (write-byte sig pipe)
          (finish-output pipe))
        )))

(defun http-add-event (conn &key event client)
  (with-slots (socket) conn
    (debug-info 'DEBUG-INFO
                "set reader io handler ~A"
                *http-event-base*)
    (acquire-lock *event-base-lock*)
    (set-io-handler *http-event-base*
                    (socket-os-fd socket)
                    event
                    (make-http-event-loop conn client)
                    :one-shot t)
    (release-lock *event-base-lock*)
    ))

(defun destroy-connection (conn)
  (with-slots (socket) conn
    (close socket)))

(defun init-network ()
  "Initialize the event base object, and the listening socket, bind it
to the local address, make it listen on a specified port . "
  
  (setf *http-event-base*
        (make-instance 'iomux:event-base))
  (setf *http-connection*
        (make-instance 'web-connection :state 'CONN_STATE_ACCEPT))
  (with-slots (socket) *http-connection*
    (setf socket
          (make-socket
           :connect :passive
           :address-family :internet
           :type :stream
           :external-format '(:utf-8 :eol-style :crlf)))
    (bind-address socket
                  (ensure-address "127.0.0.1")
                  :port 8080
                  :reuse-addr t)

    (listen-on socket :backlog 10)
    
    (http-add-event *http-connection* :event :read))
  )
