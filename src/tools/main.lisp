(defpackage malaga/tools
  (:use :cl)
  (:export #:migrate
           #:sync-player-data
           #:sync-scryfall-data))

(in-package malaga/tools)

(defun migrate ()
  (malaga/db:with-mito-connection (malaga/models:sync-models)))

(defun sync-scryfall-data ()
  (malaga/db:with-mito-connection
    (handler-case (malaga/utils:with-file-lock (pathname (uiop:getenv "MALAGA_LOCK"))
        (malaga/tools/scryfall:sync-data))
      (malaga/utils:lock-exists-error (err) (format t "Can't update, ~A~%" (malaga/utils:message err))))))

(defun sync-player-data ()
  (malaga/db:with-mito-connection
      (handler-case (malaga/utils:with-file-lock (pathname (uiop:getenv "MALAGA_LOCK"))
          (malaga/tools/player:process-players (pathname (uiop:getenv "MALAGA_DROPBOX_LOCATION"))))
        (malaga/utils:lock-exists-error (err) (format t "Can't update, ~A~%" (malaga/utils:message err))))))
