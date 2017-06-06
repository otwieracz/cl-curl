;; Pooled CURL

(in-package #:curl)

;; Locked variables

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun locked-value-lock-name (name)
    "Used for locked-values"
    (intern (format nil "~A-LOCK" name) (find-package 'curl)))

  (defmacro defvar-locked (name &optional default docstring)
    "Replacement for defvar for thread-locked variables"
    `(progn
       (defvar ,name ,default ,@(when docstring `(,docstring)))
       (defvar ,(locked-value-lock-name name) (bt:make-lock ,(string name)))))

  (defmacro with-locked-var (place &body body)
    `(bt:with-lock-held (,(locked-value-lock-name place))
       ,@body))

  (defvar-locked *connections* nil))

(defvar *connection-pool-size* 32)

(defun init-connections (&key (pool-size *connection-pool-size*))
  (with-locked-var *connections*
    (dotimes (i (- pool-size (length *connections*)))
      (push (curl-init) *connections*))))

(defun get-connection ()
  (init-connections)
  (loop
    with connection = nil
    while (when (null connection)
            (sleep 0.01) t)
    do (setf connection
             (with-locked-var *connections*
               (pop *connections*)))
    finally (return connection)))

(defun return-connection (connection)
  (assert connection)
  (with-locked-var *connections*
    (push connection *connections*)))

(defmacro perform-in-connection ((&key (cookies nil)) &body body)
  "Establish a network connection, and return the final string."
  `(values-list
    (let ((connection (get-connection)))
      (flet ((set-option (option value) (set-option connection option value))
             (perform () (perform connection))
             (curl-prepare () (curl-prepare connection))
             (finish () (finish connection))
             (set-header (string) (set-header connection string))
             (return-string () (return-string connection))
             (set-send-string (string) (set-send-string connection string)))
        #-allegro
        (declare (ignorable (function set-option)
                            (function perform)
                            (function return-string)
                            (function set-send-string)))
        ,(when cookies
           (if (stringp cookies)
               `(set-option :cookiefile ,cookies)
               '(set-option :cookiefile "nonsense.cookies")))
        (curl-prepare)
        ,@body
        (perform)
        (prog1
            (list
             (copy-seq (return-string))
             (status connection))
          (return-connection connection))))))

(defun finish-connection (&key (connection *connection*))
  (finish connection))

(defun http-request (url &key (method :get) content content-type additional-headers basic-authorization (connection-timeout 15))
  (perform-in-connection (:cookies nil)
    (progn
      (curl:set-option :url url)
      (curl:set-option :timeout connection-timeout)
      (curl:set-option :connecttimeout connection-timeout)
      (curl:set-option :followlocation 1)
      ;; fastopen implemented in libcurl>=7.49
      #+libcurl-tcp-fastopen(curl:set-option :tcp-fastopen 1)
      (when basic-authorization
        (curl:set-option :username (car basic-authorization))
        (curl:set-option :password (cadr basic-authorization)))
      (when content-type
        (curl:set-header (format nil "Content-Type: ~A" content-type)))
      (ecase method
        (:post
         (curl:set-option :post 1)
         (curl:set-option :postfields content))
        (:delete
         (curl:set-option :customrequest "DELETE")
         (curl:set-option :postfields content))
        (:get
         (curl:set-option :httpget 1)))
      (dolist (header additional-headers)
        (curl:set-header header)))))
