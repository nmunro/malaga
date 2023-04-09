(defpackage malaga
  (:use :cl)
  (:export #:sync-player-data
           #:sync-scryfall-data))

(in-package malaga)

(defun sync-scryfall-data ()
  (let ((config (malaga/config:load-config)))
    (handler-case (malaga/utils:with-file-lock (malaga/config:lock-file config)
      (malaga/db:with-mito-connection (merge-pathnames (malaga/config:config-db config) (malaga/config:config config))
        (malaga/models:sync-models)
        (malaga/scryfall:sync-data config)))
      (malaga/utils:lock-exists-error (err) (format t "Can't update, ~A~%" (malaga/utils:message err))))))

(defun sync-player-data ()
  (let ((config (malaga/config:load-config)))
    (malaga/db:with-mito-connection (malaga/config:config-db config)
        (malaga/models:sync-models)
        (malaga/user:process-users config))))

;; (sync-player-data)
;; (ql:quickload :malaga)

;; (let ((config (malaga/config:load-config)))
;;   (malaga/db:with-mito-connection (malaga/config:config-db config)
;;     ))
