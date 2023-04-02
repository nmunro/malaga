(defpackage malaga/scryfall
  (:use :cl)
  (:export #:ingest-data
           #:sync-bulk-data
           #:sync-data))

(in-package malaga/scryfall)

(defun ingest-data (config)
  (let ((file (malaga/config:all-card-data config))
        (start (local-time:now)))
    (format t ">>> Started: ~A~%" start)
    (with-open-file (in file :direction :input :if-does-not-exist nil)
      (let ((total-count (loop :for line = (read-line in nil nil) :for count :from 0 :unless line :return count)))
        ; Reset file pointer to 0
        (file-position in 0)

        (loop :for line = (read-line in nil nil)
              :for count :from 1
              :while line :do (process-scryfall-object line count total-count config))))
    (format t ">>> 100.000%~%")
    (format t ">>> Stopped: ~A~%" (local-time:now))))

(defun process-scryfall-object (object count total-count config)
  (when (str:starts-with-p "{" (str:trim object))
    (format t ">>> ~,3f%" (* 100 (/ count total-count)))
    (finish-output)
    (sleep 1)

    (dotimes (i 16)
        (write-char #\Backspace))

    (let ((card (json:decode-json-from-string (str:trim object))))
        (create-missing-set (cdr (assoc :set card)) config)
        (create-missing-card card config))))

(defun create-missing-set (set-code config)
  (unless (mito:find-dao 'malaga/models:scryfall-set :code set-code)
    (let ((set (json:decode-json-from-string (dex:get (format nil "~A/~A" (malaga/config:get-url config :sets) set-code)))))
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
  (with-input-from-string (json-stream (dexador:get (malaga/config:get-url config :bulk-data)))
    (loop :for data :in (cdr (assoc :data (json:decode-json json-stream)))
          :collect `(:type ,(cdr (assoc :type data)) :uri ,(cdr (assoc :download--uri data))))))

(defun sync-bulk-data (config)
  (let ((latest-data (get-latest-bulk-data config))
        (path (merge-pathnames (malaga/config:config-data config) (malaga/config:config config))))
    (malaga/utils:download-files latest-data path)))

(defun sync-data (config)
  (sync-bulk-data config)
  (format t ">>> Data Sync'd!~%")
  (ingest-data config))
