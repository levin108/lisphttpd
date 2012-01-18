(in-package :lisphttpd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +wm-signal-read+  10)
  (defconstant +wm-signal-write+ 12)
  (defconstant +wm-signal-exit+  15))

(defun worker-process-accept (http-client)
  (let ((accept-conn (connection http-client))
        new-client socket)
    ;; accept a new connection.
    (setf socket (accept-connection (socket accept-conn)))
    (multiple-value-bind (who port) (remote-name socket)
      (debug-info 'DEBUG-INFO "client ~A:~D connected." who port))
    
    (setf new-client (make-web-client socket :state 'CONN_STATE_READ))

    (http-add-event (connection new-client)
                    :event :read
                    :client new-client)
    ;; add the listening socket to event base again.
    (http-add-event (connection http-client)
                    :event :read)))

(defun worker-process-read (http-client)
  (let ((buf-vector (make-array 4096 :element-type 'ub8))
        (buf-string nil) (ret nil)
        (conn (connection http-client))
        (req (request http-client)))
    (with-slots (socket) conn
      (multiple-value-bind (buf-vector rbytes)
          (receive-from socket :buffer buf-vector :start 0 :end 4096 :size 4096)
        (debug-info 'DEBUG-INFO "received ~d bytes." rbytes)
        (setf buf-vector (subseq buf-vector 0 rbytes))
        (setf buf-string (map 'string #'code-char buf-vector))
        (with-slots (data-vector data-string data-length) req
          (setf data-vector (concatenate 'vector data-vector buf-vector))
          (setf data-string (concatenate 'string data-string buf-string))
          (setf data-length (+ data-length rbytes))
          (setf ret (parse-request req))
          (cond
            ((eql ret 'REQ-ERR-BAD-REQUEST)
             (debug-info 'DEBUG-ERROR "request with error BAD REQUEST.")
             (finalize-request http-client))
            
            ((eql ret 'REQ-ERR-AGAIN)
             (debug-info 'DEBUG-ERROR "request with error AGAIN.")
             (http-add-event conn :event :read
                             :client http-client))

            ((eql ret 'REQ-OK)
             (debug-info 'DEBUG-INFO "request OK.")
             (finalize-request http-client)))
          
          (with-slots (method path headers) req
            (debug-info 'DEBUG-INFO "Method is: ~A" method)
            (debug-info 'DEBUG-INFO "Request Path is: ~A" path)
            (loop for x across headers do
                 (debug-info 'DEBUG-INFO "<#~A>: ~A" (header-name x) (header-value x))))
          (debug-info 'DEBUG-INFO "~%Recv: ~A" data-string)))
      (debug-info 'DEBUG-INFO "event base: ~A" *http-event-base*))
    ))

(defun worker-process-write (http-client)
  (with-slots (response connection) http-client
    (with-slots (write-vector write-pos) response
      (debug-info 'DEBUG-INFO "write message is: ~%~A"
                  (map 'string #'code-char write-vector))
      (send-to (socket connection) write-vector
               :start write-pos :end (length write-vector))
      (setf write-pos 0)
      (destroy-connection connection))))

(defun worker-event-loop (http-client event-type)
  (with-slots (state) (connection http-client)
    (debug-info 'DEBUG-INFO "thread state: ~A" state)
    (cond
      ((eq event-type 'WORKER-READ-EVENT)
       (cond
         ((eq 'CONN_STATE_ACCEPT state)
          (worker-process-accept http-client))
         ((eq 'CONN_STATE_READ state)
          (worker-process-read http-client))))
      ((eq event-type 'WORKER-WRITE-EVENT)
       (worker-process-write http-client)))))

(defun worker-thread-func (http-thread)
  (lambda ()
    (with-slots (worker startup-cond pipe-reader) http-thread
      (let ((name-of-thread (thread-name worker)))
        
        (debug-info 'DEBUG-INFO "hello thread: ~A" name-of-thread)
        
        (acquire-lock *startup-lock*)
        (condition-wait startup-cond *startup-lock*)
        (release-lock *startup-lock*)
        
        (debug-info 'DEBUG-INFO "thread ~A was waked up" name-of-thread)

        ;; event loop of the worker thread.
        (loop 
           (let ((client nil)
                 (sig (read-byte pipe-reader)))

             (debug-info 'DEBUG-INFO
                         "worker ~A was activated with signal<#~d>."
                         name-of-thread sig)

             (cond
               ((equalp sig +wm-signal-read+)
                (acquire-lock *task-read-lock*)
                (setf client (vector-pop *task-read-queue*))
                (release-lock *task-read-lock*)
                (debug-info 'DEBUG-INFO "poped up a read task.")
                
                (worker-event-loop client 'WORKER-READ-EVENT))

               ((equalp sig +wm-signal-write+)
                (acquire-lock *task-write-lock*)
                (setf client (vector-pop *task-write-queue*))
                (release-lock *task-write-lock*)
                (debug-info 'DEBUG-INFO "poped up a write task.")
                (worker-event-loop client 'WORKER-WRITE-EVENT))
                 
               ((equalp sig +wm-signal-exit+)
                (debug-info 'DEBUG-INFO "thread ~A exiting." name-of-thread)
                (return))

               (t
                (debug-info 'DEBUG-ERROR "FATAL, unknown ipc signal received.")))
             ))))))

(defun init-thread ()
  "Initialize the thread pool, create *workers-count* threads
with thread name <worker-thread-N>, also initialize the socketpair
for the master thread controlling the worker threads through IPC."
  (loop
     for i from 0 below *workers-count* do
       (let ((thread-name (format nil "worker-thread-~D" i))
             (http-thread nil))
         (setf http-thread (make-instance 'worker-thread))
         (with-slots (worker startup-cond pipe-reader pipe-writer) http-thread
           (setf startup-cond (make-condition-variable))
           (multiple-value-bind (fd1 fd2)
               (make-socket-pair :input-buffer-size 1024
                                 :output-buffer-size 1024)
             (setf pipe-reader fd1)
             (setf pipe-writer fd2))
           (setf (elt *thread-pool* i) http-thread)
           (setf worker (make-thread
                         (worker-thread-func http-thread)
                         :name thread-name))
           )))
  ;(sleep 1)
  ;; wakeup all the threads.
  (debug-info 'DEBUG-INFO "wakeup all the threads.")
  (loop for x across *thread-pool* do
       (acquire-lock *startup-lock*)
       (condition-notify (startup-cond x))
       (release-lock *startup-lock*)))

(defun destroy-workers ()
  (loop for thread across *thread-pool* do
       (with-slots (worker) thread
         (debug-info 'DEBUG-INFO "terminate thread ~A."
                     (thread-name worker))
         (destroy-thread worker))))
