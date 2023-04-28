(defpackage malaga/player
  (:use :cl)
  (:export #:process-players))

(in-package malaga/player)

(defun create-new-user (file)
  (let ((name (car (last (pathname-directory file)))))
    (unless (mito:find-dao 'malaga/models:user :name name)
        (mito:create-dao 'malaga/models:user :name name :file (namestring file) :checksum ""))))

(defun get-list-of-players-from-files (config)
  (mapcar #'(lambda (d) (car (last (pathname-directory d)))) (find-card-lists (malaga/config:dropbox-location config))))

(defun delete-old-players (config)
  (let ((db-players   (mapcar #'(lambda (user) (slot-value user 'malaga/models:name)) (mito:select-dao 'malaga/models:user)))
        (file-players (get-list-of-players-from-files config)))
    (dolist (user-name (set-difference db-players file-players :test #'equal))
      (mito:delete-by-values 'malaga/models:user :name user-name))))

(defun update-user-p (user)
  (string/= (slot-value user 'malaga/models:checksum) (malaga/utils:get-checksum (slot-value user 'malaga/models:file))))

(defun delete-stale-data ()
  (mito:delete-by-values 'malaga/models:collection :updated "N"))

(defun create-collection-record (user csv card index)
  (mito:create-dao
    'malaga/models:collection
    :user user
    :card card
    :updated "Y"
    :quantity (data-table:data-table-value csv :row-idx index :col-name "quantity")))

(defun update-collection-record (user csv collection index)
  (setf (slot-value collection 'malaga/models:quantity) (parse-integer (data-table:data-table-value csv :row-idx index :col-name "quantity")))
  (setf (slot-value collection 'malaga/models:updated) "Y")
  (mito:save-dao collection))

(defun update-user (user)
  (let* ((path (pathname (slot-value user 'malaga/models:file)))
         (headers (cl-csv:read-csv-row path))
         (csv (make-instance 'data-table:data-table :column-names headers :rows (cdr (cl-csv:read-csv path)))))
    (dotimes (index (1- (length (data-table:rows csv))))
      (let* ((card (mito:find-dao 'malaga/models:scryfall-card :id (data-table:data-table-value csv :row-idx index :col-name "scryfall_id")))
             (collection (mito:find-dao 'malaga/models:collection :user user :card card)))
        (if collection
          (update-collection-record user csv collection index)
          (create-collection-record user csv card index))))))

(defun find-card-lists (dropbox-location)
  (loop :for dir :in (directory dropbox-location)
        :for path = (probe-file (pathname (format nil "~A/cards.csv" dir)))
        :if path :collect path))

(defun mark-records-as-stale ()
  (dolist (collection (mito:select-dao 'malaga/models:collection))
      (setf (slot-value collection 'malaga/models:updated) "N")
      (mito:save-dao collection)))

(defun process-players (config)
  (mark-records-as-stale)
  (delete-old-players config)

  (dolist (user-path (find-card-lists (malaga/config:dropbox-location config)))
    (create-new-user user-path)

    (let ((user (mito:find-dao 'malaga/models:user :name (car (last (pathname-directory user-path))))))
      (format t ">>> Processing: ~A~%" (slot-value user 'malaga/models:name))

      (when (update-user-p user)
        (update-user user))

      (delete-stale-data))))