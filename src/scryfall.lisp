(defpackage malaga/scryfall
  (:use :cl)
  (:export #:ingest-data
           #:sync-bulk-data
           #:sync-data))

(in-package malaga/scryfall)

(defun ingest-data (file)
  (let ((start (local-time:now)))
    (format t "Started: ~A~%" start)
    (with-open-file (in file :direction :input :if-does-not-exist nil)
      (loop :for line = (read-line in nil nil)
            :while line :do (process-scryfall-object line)))
    (format t "Stopped: ~A~%" (local-time:now))))

(defun process-scryfall-object (object)
  (when (str:starts-with-p "{" (str:trim object))
     (let ((card (json:decode-json-from-string (str:trim object))))
        (create-missing-set (cdr (assoc :set card)))
        (create-missing-card card))))

(defun create-missing-set (set-code)
  (unless (mito:find-dao 'malaga/models:scryfall-set :code set-code)
    (let ((set (json:decode-json-from-string (dex:get (format nil "https://api.scryfall.com/sets/~A" set-code)))))
        (mito:create-dao 'malaga/models:scryfall-set
            :id (cdr (assoc :id set))
            :code (cdr (assoc :code set))
            :name (cdr (assoc :name set))
            :set-type (cdr (assoc :set--type set))
            :card-count (cdr (assoc :card--count set))
            :scryfall-uri (cdr (assoc :scryfall--uri set))
            :uri (cdr (assoc :uri set))
            :icon-svg-uri (cdr (assoc :icon--svg--uri set))
            :search-uri (cdr (assoc :search--uri set))))))

(defun create-missing-card (card)
  (unless (mito:find-dao 'malaga/models:scryfall-card :name (cdr (assoc :id card)))
    (mito:create-dao 'malaga/models:scryfall-card
        :id (cdr (assoc :id card))
        :name (cdr (assoc :name card))
        :lang (cdr (assoc :lang card))
        :scryfall-uri (cdr (assoc :scryfall--uri card))
        :uri (cdr (assoc :uri card))
        :rarity (cdr (assoc :rarity card))
        :set (mito:find-dao 'malaga/models:scryfall-set :code (cdr (assoc :set card))))))

(defun get-latest-bulk-data (url)
  (with-input-from-string (json-stream (dexador:get url))
    (loop :for data :in (cdr (assoc :data (json:decode-json json-stream)))
          :collect `(:type ,(cdr (assoc :type data)) :uri ,(cdr (assoc :download--uri data))))))

(defun sync-bulk-data (dir url)
  (let ((files (get-latest-bulk-data url)))
    (malaga/utils:download-files files dir)))

(defun sync-data (config)
  (let ((path (merge-pathnames (cdr (assoc :config-data config)) (cdr (assoc :config config)))))
    (sync-bulk-data path (cdr (assoc :url config)))
    (format t "Data Sync'd!~%")
    (ingest-data (merge-pathnames (cdr (assoc :all-card-data config)) path))))
