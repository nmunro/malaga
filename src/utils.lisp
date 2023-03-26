(defpackage malaga/utils
  (:use :cl)
  (:export #:check-file-needs-refreshing
           #:find-card-lists
           #:get-checksum
           #:download-files
           #:process-users
           #:download-file))

(in-package malaga/utils)

(defun check-file-needs-refreshing (file)
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
        (format t "File ~A is up to date!~%" cache-file)
        (return-from download-file nil))

      (format t "Downloading ~A to ~A~%" (getf file :type) cache-file)
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
