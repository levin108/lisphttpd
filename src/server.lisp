(in-package :lisphttpd)

(defun cleanup ()
  (debug-info 'DEBUG-INFO "cleanup")
  (destroy-connection *http-connection*)
  (close (socket *http-connection*))
  (destroy-workers)
  (socket *http-connection*))

(defun webstart ()

  (init-network)
  (init-resp-code)
  (init-thread)

  (event-dispatch *http-event-base*)
  (when *http-event-base*
    (close *http-event-base*))

  (loop for x across *thread-pool* do
       (join-thread (worker x)))

  (loop for x across *thread-pool* do
       (with-slots (pipe-writer pipe-reader) x
         (close pipe-writer)
         (close pipe-reader)))
  
  (destroy-connection *http-connection*)
  )
