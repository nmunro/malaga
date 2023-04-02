(defpackage malaga
  (:use :cl)
  (:export #:main
           #:sync-scryfall))

(in-package malaga)

(ql:quickload :ironclad)

(defun sync-scryfall ()
  ; Put in a check for a update lock file here
  (let ((config (malaga/config:load-config)))
    (malaga/db:with-mito-connection (merge-pathnames (malaga/config:config-db config) (malaga/config:config config))
      (mito:ensure-table-exists 'malaga/models:scryfall-set)
      (mito:ensure-table-exists 'malaga/models:scryfall-card)
      (malaga/scryfall:sync-data config))))

(defun main ()
  (let ((config (malaga/config:load-config)))
    (malaga/db:with-mito-connection (merge-pathnames (cdr (assoc :config-db config)) (cdr (assoc :config config)))
      (mito:ensure-table-exists 'malaga/models:user)
      (mito:ensure-table-exists 'malaga/models:card)
      (mito:ensure-table-exists 'malaga/models:collection)

      (dolist (card-list (malaga/utils:find-card-lists (cdr (assoc :dropbox-location config))))
        (malaga/utils:process-users card-list)))))
