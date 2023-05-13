(defpackage malaga/tools
  (:use :cl)
  (:export #:sync-player-data
           #:sync-scryfall-data))

(in-package malaga/tools)

(malaga/db:with-mito-connection (malaga/config:load-config)
  (malaga/models:sync-models))

(defun sync-scryfall-data ()
  (malaga/db:with-mito-connection-and-conf (config (malaga/config:load-config))
    (handler-case (malaga/utils:with-file-lock (malaga/config:lock-file config)
        (malaga/tools/scryfall:sync-data config))
      (malaga/utils:lock-exists-error (err) (format t "Can't update, ~A~%" (malaga/utils:message err))))))

(defun sync-player-data ()
  (malaga/db:with-mito-connection-and-conf (config (malaga/config:load-config))
    (handler-case (malaga/utils:with-file-lock (malaga/config:lock-file config)
        (malaga/tools/player:process-players (malaga/config:dropbox-location config)))
      (malaga/utils:lock-exists-error (err) (format t "Can't update, ~A~%" (malaga/utils:message err))))))
