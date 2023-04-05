(defpackage malaga
  (:use :cl)
  (:export #:sync-player-data
           #:sync-scryfall-data))

(in-package malaga)

(defun sync-scryfall-data ()
  ; Put in a check for a update lock file here
  (let ((config (malaga/config:load-config)))
    (handler-case (malaga/utils:with-file-lock (malaga/config:lock-file config)
      (malaga/db:with-mito-connection (merge-pathnames (malaga/config:config-db config) (malaga/config:config config))
        (mito:ensure-table-exists 'malaga/models:scryfall-set)
        (mito:ensure-table-exists 'malaga/models:scryfall-card)
        (malaga/scryfall:sync-data config)))
      (malaga/utils:lock-exists-error (err) (format t "Can't update, ~A~%" (malaga/utils:message err))))))

(defun sync-player-data ()
  (let ((config (malaga/config:load-config)))
    (malaga/db:with-mito-connection (malaga/config:config-db config)
        (mito:ensure-table-exists 'malaga/models:user)
        (mito:ensure-table-exists 'malaga/models:card)
        (mito:ensure-table-exists 'malaga/models:collection)

      (dolist (card-list (malaga/utils:find-card-lists (malaga/config:dropbox-location config)))
        (format t "Card List: ~A~%" card-list)
        (malaga/utils:process-users card-list)))))
