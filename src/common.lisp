(in-package :lisphttpd)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *workers-count* 3)
  (defvar *http-connection* nil)
  (defvar *http-event-base* nil)
  (defvar *task-read-queue* (make-array 0 :fill-pointer 0 :adjustable t))
  (defvar *task-write-queue* (make-array 0 :fill-pointer 0 :adjustable t))
  (defvar *task-read-lock* (make-lock))
  (defvar *task-write-lock* (make-lock))
  (defvar *startup-lock* (make-lock))
  (defvar *event-base-lock* (make-lock))
  (defvar *standard-output-lock* (make-lock))
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
