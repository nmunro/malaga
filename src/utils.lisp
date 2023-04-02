(defpackage malaga/utils
  (:use :cl)
  (:export #:check-file-needs-refreshing
           #:find-card-lists
           #:get-checksum
           #:download-files
           #:process-users
           #:download-file
           #:show-progress
           #:with-file-lock))

(in-package malaga/utils)

(define-condition lock-exists-error (error)
  ((message :initarg :message :initform "lock exists" :reader message)))

(defmacro with-file-lock (path &body body)
  `(progn
    (when (probe-file ,path)
        (error 'lock-exists-error :message (format nil "Lock File ~A exists" path)))

    (open ,path :direction :probe :if-does-not-exist :create)
    (unwind-protect (progn ,@body))
    (delete-file ,path)))

(defun check-file-needs-refreshing (file)
  (unless (probe-file file)
    (return-from check-file-needs-refreshing t))

  (let ((yesterday (local-time:timestamp- (local-time:now) (* 24 (* 60 60)) :sec))
        (file (local-time:unix-to-timestamp (osicat-posix:stat-ctime (osicat-posix:stat file)))))
    (local-time:timestamp< file yesterday)))

(defun download-files (files dir)
  (dolist (file files)
    (download-file file dir)))

(defun download-file (file dir)
  (let ((cache-dir dir))
    (ensure-directories-exist cache-dir)

    (let ((cache-file (merge-pathnames (pathname (format nil "~A.json" (getf file :type))) cache-dir)))
      (unless (check-file-needs-refreshing cache-file)
        (format t ">>> File ~A is up to date!~%" cache-file)
        (return-from download-file nil))

      (format t ">>> Downloading ~A~%" (getf file :type))
      (serapeum:write-stream-into-file (dex:get (getf file :uri) :want-stream t) cache-file :if-exists :supersede))))

(defun get-checksum (path)
  (let ((digest (ironclad:make-digest :md5)))
    (format nil "~{~A~}" (coerce (ironclad:digest-file digest path) 'list))))

(defun process-users (file)
  (let ((checksum (malaga/utils:get-checksum file)))
    (mito:create-dao 'malaga/models:user :name (car (last (pathname-directory file))) :file (namestring file) :checksum checksum)))

(defun find-card-lists (dropbox-location)
  (flet ((map-csv-files (dir)
           (probe-file (pathname (format nil "~A/cards.csv" dir)))))
    (remove nil (mapcar #'map-csv-files (directory dropbox-location)))))
