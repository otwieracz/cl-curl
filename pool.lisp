;; Pooled CURL

(in-package #:curl)

(defvar *connection* nil)

(defun init-connection ()
  (curl-init))

(defmacro perform-in-connection ((&key (connection '*connection*) (cookies nil)) &body body)
  "Establish a network connection, and return the final string."
  `(values-list
    (flet ((set-option (option value) (set-option ,connection option value))
           (perform () (perform ,connection))
           (curl-prepare () (curl-prepare ,connection))
           (finish () (finish ,connection))
           (set-header (string) (set-header ,connection string))
           (return-string () (return-string ,connection))
           (set-send-string (string) (set-send-string ,connection string)))
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
           (status ,connection))))))

(defun finish-connection (&key (connection *connection*))
  (finish connection))

(defun http-request (url &key (connection *connection*) (method :get) content content-type additional-headers basic-authorization (connection-timeout 15))
  (perform-in-connection (:connection connection :cookies nil)
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
