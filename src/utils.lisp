(defpackage malaga/utils
  (:use :cl)
  (:export #:download-files
           #:download-file
           #:with-file-lock
           #:lock-exists-error
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

(defun download-files (files dir &key (log t))
  (dolist (file files)
    (download-file file dir :log log)))

(defun download-file (file dir &key (log t))
  (let ((cache-dir dir))
    (ensure-directories-exist cache-dir)

    (let ((cache-file (merge-pathnames (pathname (format nil "~A.json" (getf file :type))) cache-dir)))
      (unless (file-refresh-p cache-file)
        (when log
          (format t ">>> File ~A is up to date!~%" cache-file))
        (return-from download-file nil))

      (when log
        (format t ">>> Downloading ~A~%" (getf file :type)))

      (serapeum:write-stream-into-file (dex:get (getf file :uri) :want-stream t) cache-file :if-exists :supersede))))
