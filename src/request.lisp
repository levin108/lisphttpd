(in-package :lisphttpd)

(defclass http-request ()
  ((method        :initarg :method        :accessor method)
   (path          :initarg :path          :accessor path
                  :type 'string)
   (major-version :initarg :major-version :accessor major-version)
   (minor-version :initarg :minor-version :accessor minor-version)
   (headers       :initarg :headers       :accessor headers
                  :type '(vector http-header (*)))
   (data-vector   :initarg :data-vector   :accessor data-vector :initform nil)
   (data-string   :initarg :data-string   :accessor data-string :initform nil)
   (data-length   :initarg :data-length   :accessor data-length :initform 0)
   (current-pos   :initarg :current-pos   :accessor current-pos :initform 0)
   (current-state :initarg :current-state :accessor current-state
                  :initform 'PROCESS-METHOD)))

(defun make-http-request ()
  (make-instance 'http-request
                 :method 'METHOD-GET
                 :major-version 1
                 :minor-version 1
                 :path "/"
                 :headers
                 (make-array 0 :element-type 'http-header
                             :fill-pointer 0 :adjustable t)))

(defmacro set-request-method (req meth)
  `(setf (method ,req) ,meth))

(defmacro set-request-path (req path)
  `(setf (path ,req) ,path))

(defmacro set-request-minor-version (req ver)
  `(setf (minor-version ,req) ,ver))

(defmacro set-request-header-value (hdr val)
  `(setf (header-value ,hdr) ,val))

(defun finalize-request (http-client)
  (with-slots (connection request response) http-client
    (set-response-code response +http-not-found+)
    (make-http-header response "Cache-Control" "no-cache")
    (make-http-header response "Connection" "Closed")
    (make-http-header response "Content-Type" "text/html")
    (set-response-body response (format nil +http-error-404-page+ (path request)))
    (debug-info 'DEBUG-INFO "headers is : ~%~A" (resp-to-vector response))
    (http-add-event connection
                    :event :write
                    :client http-client)))

(defun parse-request (request)
  (with-slots (data-string current-pos
               current-state data-length) request
    (let (hdr)
      (flet
          ((do-parse-request ()
             (cond
               ((eq current-state 'PROCESS-METHOD)
                (let ((first-char (aref data-string current-pos)))
                  (cond
                    ((eql first-char #\G) ;; GET method
                     (unless
                         (and (eql (aref data-string (+ current-pos 1)) #\E)
                              (eql (aref data-string (+ current-pos 2)) #\T)
                              (eql (aref data-string (+ current-pos 3)) #\Space))
                       (return-from parse-request 'REQ-ERR-BAD-REQUEST))
                     (set-request-method request 'METHOD-GET)
                     (incf current-pos 4))
                    ((eql first-char #\P) ;; POST method
                     (unless
                         (and (eql (aref data-string (+ current-pos 1)) #\O)
                              (eql (aref data-string (+ current-pos 2)) #\S)
                              (eql (aref data-string (+ current-pos 3)) #\T)
                              (eql (aref data-string (+ current-pos 4)) #\Space))
                       (return-from parse-request 'REQ-ERR-BAD-REQUEST))
                     (set-request-method request 'METHOD-POST)
                     (incf current-pos 5))
                    ((eql first-char #\H) ;; HEAD method
                     (unless
                         (and (eql (aref data-string (+ current-pos 1)) #\E)
                              (eql (aref data-string (+ current-pos 2)) #\A)
                              (eql (aref data-string (+ current-pos 3)) #\D)
                              (eql (aref data-string (+ current-pos 4)) #\Space))
                       (return-from parse-request 'REQ-ERR-BAD-REQUEST))
                     (set-request-method request 'METHOD-HEAD)
                     (incf current-pos 5)))
                  (setf current-state 'PROCESS-PATH)))
               
               ((eq current-state 'PROCESS-PATH)
                (loop for x from current-pos below data-length
                   do (progn
                        (when (or
                               (eql (aref data-string x) #\Return)
                               (eql (aref data-string x) #\Newline))
                          (return-from parse-request) 'REQ-OK)
                        (when (eql (aref data-string x) #\Space)
                          (set-request-path request (subseq data-string current-pos x))
                          (setf current-pos (+ 1 x))
                          (setf current-state 'PROCESS-VERSION)
                          (return-from do-parse-request)))))
               
               ((eq current-state 'PROCESS-VERSION)
                (when (< (- data-length current-pos) 9)
                  (return-from parse-request 'REQ-ERR-AGAIN))
                
                (unless (and (eql (aref data-string current-pos) #\H)
                             (eql (aref data-string (+ current-pos 1)) #\T)
                             (eql (aref data-string (+ current-pos 2)) #\T)
                             (eql (aref data-string (+ current-pos 3)) #\P)
                             (eql (aref data-string (+ current-pos 4)) #\/)
                             (eql (aref data-string (+ current-pos 5)) #\1)
                             (eql (aref data-string (+ current-pos 6)) #\.))

                  (return-from parse-request 'REQ-ERR-BAD-REQUEST))

                (unless (eq (aref data-string (+ 7 current-pos)) #\1)
                  (debug-info 'DEBUG-ERROR "minor-version is not 1.")
                  (return-from parse-request 'REQ-ERR-BAD-REQUEST))

                (set-request-minor-version request 1)

                (unless (and (eql (aref data-string (+ current-pos 8)) #\Return)
                             (eql (aref data-string (+ current-pos 9)) #\Newline))
                  (return-from parse-request) 'REQ-ERR-BAD-REQUEST)
                
                (incf current-pos 10)
                (setf current-state 'PROCESS-HEADER-NAME))
               
               ((eq current-state 'PROCESS-HEADER-NAME)
                
                (when (< (- data-length current-pos) 2)
                  (return-from parse-request 'REQ-ERR-AGAIN))

                (when (and
                       (eql (aref data-string current-pos) #\Return)
                       (eql (aref data-string (+ 1 current-pos)) #\Newline))
                  (incf current-pos 2)
                  (debug-info 'DEBUG-INFO "header end with current-pos: ~D and data-length: ~D."
                              current-pos data-length)
                  (return-from parse-request 'REQ-OK))
                
                (loop for x from current-pos below data-length
                   do
                     (progn
                       (when (eql (aref data-string x) #\:)
                         (setf hdr
                               (make-http-header request
                                                 (subseq data-string current-pos x)))
                         (incf x)
                         (loop for i from x below data-length
                            while (eql (aref data-string i) #\Space)
                            finally (setf current-pos i))
                         
                         (if (eql (aref data-string current-pos) #\Space)
                             (return-from parse-request 'REQ-ERR-AGAIN))
                         
                         (setf current-state 'PROCESS-HEADER-VALUE)
                         (return-from do-parse-request))
                       
                       (when (or
                              (eql (aref data-string current-pos) #\Return)
                              (eql (aref data-string current-pos) #\Newline))
                         (return-from parse-request 'REQ-ERR-BAD-REQUEST))
                       (when (eql x data-length)
                         (return-from parse-request 'REQ-ERR-AGAIN))
                       )))
               ;; process header value.
               ((eq current-state 'PROCESS-HEADER-VALUE)
                (loop for x from current-pos below data-length
                   do
                     (when (and
                            (eql (aref data-string x) #\Return)
                            (eql (aref data-string (+ 1 x)) #\Newline))
                       (set-request-header-value hdr (subseq data-string current-pos x))
                       (setf current-pos (+ 2 x))
                       (setf current-state 'PROCESS-HEADER-NAME)
                       (return-from do-parse-request))
                   finally
                     (progn
                       (return-from parse-request 'REQ-ERR-AGAIN)))
                ))))
        (loop
           (do-parse-request))))))