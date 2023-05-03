(defpackage malaga/scryfall
  (:use :cl)
  (:export #:sync-data))

(in-package malaga/scryfall)

(defun ingest-data (config)
  (with-open-file (in (malaga/config:all-card-data config) :direction :input :if-does-not-exist nil)
    (let ((item-count (loop :for line = (read-line in nil nil) :for count :from 0 :unless line :return count)))
      ; Reset file pointer to 0
      (file-position in 0)
      (loop :for l = (read-line in nil nil) :for c :from 1 :while l :do (process-scryfall-object l c item-count config)))))

(defun process-scryfall-object (object count total-count config)
  (when (str:starts-with-p "{" (str:trim object))
    (format t ">>> ~,3f%" (* 100 (/ count total-count)))
    (finish-output)

    (dotimes (i 16)
        (write-char #\Backspace))

    (let ((card (json:decode-json-from-string (str:trim object))))
      (malaga/controllers:get-or-create malaga/controllers:+card+
        :id (cdr (assoc :id card))
        :name (cdr (assoc :name card))
        :lang (cdr (assoc :lang card))
        :scryfall-uri (cdr (assoc :scryfall--uri card))
        :uri (cdr (assoc :uri card))
        :rarity (cdr (assoc :rarity card))
        :price-usd (or (cdr (assoc :usd (cdr (assoc :prices card)))) 0)
        :price-usd-foil (or (cdr (assoc :usd-foil (cdr (assoc :prices card)))) 0)
        :price-usd-etched (or (cdr (assoc :usd-etched (cdr (assoc :prices card)))) 0)
        :price-eur (or (cdr (assoc :eur (cdr (assoc :prices card)))) 0)
        :price-eur-foil (or (cdr (assoc :eur-foil (cdr (assoc :prices card)))) 0)
        :price-eur-etched (or (cdr (assoc :eur-etched (cdr (assoc :prices card)))) 0)
        :set (cdr (assoc :set card))))))

(defun get-latest-bulk-data (config)
  (with-input-from-string (json-stream (malaga/utils:get-data (malaga/config:get-url config :bulk-data)))
    (loop :for data :in (cdr (assoc :data (json:decode-json json-stream)))
          :collect `(:type ,(cdr (assoc :type data)) :uri ,(cdr (assoc :download--uri data))))))

(defun sync-data (config)
  (let ((path (merge-pathnames (malaga/config:config-data config) (malaga/config:config config))))
    (malaga/utils:download-files (get-latest-bulk-data config) path))
  (format t ">>> Data Sync'd!~%")
  (format t ">>> Started: ~A~%" (local-time:now))
  (ingest-data config)
  (format t ">>> 100.000%~%")
  (format t ">>> Stopped: ~A~%" (local-time:now)))
