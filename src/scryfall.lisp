(defpackage malaga/scryfall
  (:use :cl)
  (:export #:ingest-data
           #:sync-bulk-data
           #:sync-data))

(in-package malaga/scryfall)

(defun ingest-data (config)
  (with-open-file (in (malaga/config:all-card-data config) :direction :input :if-does-not-exist nil)
    (format t ">>> Started: ~A~%" (local-time:now))

    (let ((item-count (loop :for line = (read-line in nil nil) :for count :from 0 :unless line :return count)))
      ; Reset file pointer to 0
      (file-position in 0)
      (loop :for l = (read-line in nil nil) :for c :from 1 :while l :do (process-scryfall-object l c item-count config))))
  (format t ">>> 100.000%~%")
  (format t ">>> Stopped: ~A~%" (local-time:now)))

(defun process-scryfall-object (object count total-count config)
  (when (str:starts-with-p "{" (str:trim object))
    (format t ">>> ~,3f%" (* 100 (/ count total-count)))
    (finish-output)

    (dotimes (i 16)
        (write-char #\Backspace))

    (let ((card (json:decode-json-from-string (str:trim object))))
        (create-missing-set (cdr (assoc :set card)) config)
        (create-missing-card card config))))

(defun create-missing-set (set-code config)
  (unless (mito:find-dao 'malaga/models:scryfall-set :code set-code)
    (let ((set (json:decode-json-from-string (malaga/utils:get-data (format nil "~A/~A" (malaga/config:get-url config :sets) set-code)))))
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

(defun create-missing-card (card config)
  (unless (mito:find-dao 'malaga/models:scryfall-card :id (cdr (assoc :id card)))
    (mito:create-dao 'malaga/models:scryfall-card
        :id (cdr (assoc :id card))
        :name (cdr (assoc :name card))
        :lang (cdr (assoc :lang card))
        :scryfall-uri (cdr (assoc :scryfall--uri card))
        :uri (cdr (assoc :uri card))
        :rarity (cdr (assoc :rarity card))
        :set (mito:find-dao 'malaga/models:scryfall-set :code (cdr (assoc :set card))))))

(defun get-latest-bulk-data (config)
  (with-input-from-string (json-stream (malaga/utils:get-data (malaga/config:get-url config :bulk-data)))
    (loop :for data :in (cdr (assoc :data (json:decode-json json-stream)))
          :collect `(:type ,(cdr (assoc :type data)) :uri ,(cdr (assoc :download--uri data))))))

(defun sync-bulk-data (config)
  (let ((path (merge-pathnames (malaga/config:config-data config) (malaga/config:config config))))
    (malaga/utils:download-files (get-latest-bulk-data config) path)))

(defun sync-data (config)
  (sync-bulk-data config)
  (format t ">>> Data Sync'd!~%")
  (ingest-data config))
