(in-package :lisphttpd)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; count of the threads in the thread pool.
  (defvar *workers-count* 3)
  ;; listening connection.
  (defvar *http-connection* nil)
  ;; global event base for IO multiplex.
  (defvar *http-event-base* nil)
  ;; queue for read task.
  (defvar *task-read-queue* (make-array 0 :fill-pointer 0 :adjustable t))
  ;; queue for write task.
  (defvar *task-write-queue* (make-array 0 :fill-pointer 0 :adjustable t))
  ;; lock for visiting the read task queue.
  (defvar *task-read-lock* (make-lock))
  ;; lock for visiting the write task queue.
  (defvar *task-write-lock* (make-lock))
  ;; lock for initialization of the thread pool.
  (defvar *startup-lock* (make-lock))
  ;; lock for the event base.
  (defvar *event-base-lock* (make-lock))
  ;; lock for the standard output, used in debug-info.
  (defvar *standard-output-lock* (make-lock))
  ;; virtual machines defined in the configuration file.
  (defvar *machines* (make-array 0 :element-type 'machine
                                 :fill-pointer 0 :adjustable t))
  ;; thread pool.
  (defvar *thread-pool* (make-array *workers-count*
                                    :element-type 'worker-thread)))

(defclass worker-thread ()
  ((worker :initarg :worker :accessor worker)
   (mutex  :initarg :mutex :accessor mutex)
   (startup-cond :initarg :startup-cond :accessor startup-cond)
   (pipe-reader :initarg :pipe-reader :accessor pipe-reader)
   (pipe-writer :initarg :pipe-writer :accessor pipe-writer)))

(defclass web-connection ()
  ((socket :initarg :socket :accessor socket)
   (state  :initarg :state  :accessor state :initform 0)))


(defmacro debug-info (type fmt &rest args)
  (with-gensyms (fmt-str fmt-string)
    `(let ((,fmt-string
      (with-output-to-string (,fmt-str)
        (cond
          ((eq ,type 'DEBUG-INFO)
           (format ,fmt-str "[DEBUG_INFO] ~A~%" ,fmt))
          ((eq ,type 'DEBUG-WARNING)
           (format ,fmt-str "[DEBUG_WARNING] ~A~%" ,fmt))
          ((eq ,type 'DEBUG-ERROR)
           (format ,fmt-str "[DEBUG_ERROR] ~A~%" ,fmt))
          (t
           (format ,fmt-str "[UNKNOWN_STATE] ~A~%" ,fmt))))))
       (acquire-lock *standard-output-lock*)
       (format t ,fmt-string ,@args)
       (release-lock *standard-output-lock*))))
