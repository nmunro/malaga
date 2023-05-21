(defpackage malaga/utils
  (:use :cl)
  (:export #:download-files
           #:download-file
           #:with-file-lock
           #:lock-exists-error
           #:get-checksum
           #:get-data
           #:message))

(in-package malaga/utils)

(define-condition lock-exists-error (error)
  ((message :initarg :message :initform "lock exists" :reader message)))

(defmacro with-file-lock (path &body body)
  `(progn
    (when (probe-file ,path)
        (error 'lock-exists-error :message (format nil "lock file ~A exists" ,path)))
    (open ,path :direction :probe :if-does-not-exist :create)
    (unwind-protect (progn ,@body))
    (delete-file ,path)))

(defun file-refresh-p (file)
  (unless (probe-file file)
    (return-from file-refresh-p t))

  (let ((yesterday (local-time:timestamp- (local-time:now) (* 24 (* 60 60)) :sec))
        (file (local-time:unix-to-timestamp (osicat-posix:stat-ctime (osicat-posix:stat file)))))
    (local-time:timestamp< file yesterday)))

(defun download-files (urls dir &key (log t))
  (dolist (url urls)
    (download-file url dir :log log)))

(defun download-file (url dir &key (log t))
  (let ((cache-dir dir))
    (ensure-directories-exist cache-dir)

    (let ((cache-file (merge-pathnames (pathname (format nil "~A.json" (getf url :type))) cache-dir)))
      (unless (file-refresh-p cache-file)
        (when log
          (format t ">>> File ~A is up to date!~%" cache-file))
        (return-from download-file nil))

      (when log
        (format t ">>> Downloading ~A~%" (getf url :type)))

      (serapeum:write-stream-into-file (dex:get (getf url :uri) :want-stream t) cache-file :if-exists :supersede))))

(defun get-checksum (path)
  (let ((digest (ironclad:make-digest :md5)))
    (format nil "~{~A~}" (coerce (ironclad:digest-file digest path) 'list))))

(defun handle-failed-response (err)
  (declare (ignore err)) ; Don't need to use the err object
  (invoke-restart 'abort))

(defun get-data (url &key (retries 5) (wait 3))
  (let ((retry-request (dex:retry-request retries :interval wait)))
    (handler-bind
        ((dex:http-request-failed retry-request)
         (dex:http-request-service-unavailable retry-request)
         (error #'handle-failed-response))
      (dex:get url))))
