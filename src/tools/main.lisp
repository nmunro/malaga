(defpackage malaga/tools
  (:use :cl)
  (:export #:sync-player-data
           #:sync-scryfall-data))

(in-package malaga/tools)

(defun sync-scryfall-data ()
  (let ((mito:*connection* (dbi:connect :mysql :database-name (uiop:getenv "MALAGA_DB") :username (uiop:getenv "MALAGA_MYSQL_USERNAME") :password (uiop:getenv "MALAGA_MYSQL_PASSWORD") :port 3306))
        (output nil))
      (setf output (unwind-protect (progn (handler-case (malaga/utils:with-file-lock (pathname (uiop:getenv "MALAGA_LOCK"))
        (malaga/tools/scryfall:sync-data))
        (malaga/utils:lock-exists-error (err) (format t "Can't update, ~A~%" (malaga/utils:message err)))))))
      (dbi:disconnect mito:*connection*)
      output))

(defun sync-player-data ()
  (let ((mito:*connection* (dbi:connect :mysql :database-name (uiop:getenv "MALAGA_DB") :username (uiop:getenv "MALAGA_MYSQL_USERNAME") :password (uiop:getenv "MALAGA_MYSQL_PASSWORD") :port 3306))
        (output nil))
      (setf output (unwind-protect (progn
        (handler-case (malaga/utils:with-file-lock (pathname (uiop:getenv "MALAGA_LOCK"))
          (malaga/tools/player:process-players (pathname (uiop:getenv "MALAGA_DROPBOX_LOCATION"))))
          (malaga/utils:lock-exists-error (err) (format t "Can't update, ~A~%" (malaga/utils:message err)))))))
      (dbi:disconnect mito:*connection*)
      output))
