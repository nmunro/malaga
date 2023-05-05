(defpackage malaga/player
  (:use :cl)
  (:export #:process-players))

(in-package malaga/player)

;; (defun get-list-of-players-from-files (config)
;;   (mapcar #'(lambda (d) (car (last (pathname-directory d)))) (find-card-lists (malaga/config:dropbox-location config))))

;; (defun delete-old-players (config)
;;   (let ((db-players   (mapcar #'(lambda (user) (slot-value user 'malaga/models:name)) (mito:select-dao 'malaga/models:user)))
;;         (file-players (get-list-of-players-from-files config)))
;;     (dolist (user-name (set-difference db-players file-players :test #'equal))
;;       (mito:delete-by-values 'malaga/models:user :name user-name))))

;; (defun delete-stale-data ()
;;   (mito:delete-by-values 'malaga/models:collection :updated "N"))

;; (defun create-collection-record (user csv card index)
;;   (mito:create-dao
;;     'malaga/models:collection
;;     :user user
;;     :card card
;;     :updated "Y"
;;     :quantity (data-table:data-table-value csv :row-idx index :col-name "quantity")))

;; (defun update-collection-record (user csv collection index)
;;   (setf (slot-value collection 'malaga/models:quantity) (parse-integer (data-table:data-table-value csv :row-idx index :col-name "quantity")))
;;   (setf (slot-value collection 'malaga/models:updated) "Y")
;;   (mito:save-dao collection))

;; (defun update-user (user)
;;   (let* ((path (pathname (slot-value user 'malaga/models:file)))
;;          (headers (cl-csv:read-csv-row path))
;;          (csv (make-instance 'data-table:data-table :column-names headers :rows (cdr (cl-csv:read-csv path)))))
;;     (dotimes (index (1- (length (data-table:rows csv))))
;;       (let* ((card (mito:find-dao 'malaga/models:scryfall-card :id (data-table:data-table-value csv :row-idx index :col-name "scryfall_id")))
;;              (collection (mito:find-dao 'malaga/models:collection :user user :card card)))
;;         (if collection
;;           (update-collection-record user csv collection index)
;;           (create-collection-record user csv card index))))))

;; (defun mark-records-as-stale ()
;;   (dolist (collection (mito:select-dao 'malaga/models:collection))
;;       (setf (slot-value collection 'malaga/models:updated) "N")
;;       (mito:save-dao collection)))

;; (defun get-user-profile-md ()
;;   nil)

;; (defun process-players (config)
;;   (mark-records-as-stale)
;;   (delete-old-players config)

;;   (dolist (user-path (find-card-lists (malaga/config:dropbox-location config)))
;;     ; replace this with a call to 'get-or-create'
;;     (create-new-user user-path)

;;     (let ((user (mito:find-dao 'malaga/models:user :name (car (last (pathname-directory user-path))))))
;;       (format t ">>> Processing: ~A~%" (slot-value user 'malaga/models:name))

;;       (when (update-user-p user)
;;         (update-user user))

;;       (delete-stale-data))))


(defun process-players (config)
  (format t "Processing Players~%")
  (clean-up-old-data)

  (dolist (user-path (find-card-lists (malaga/config:dropbox-location config)))
    (let ((user (malaga/controllers:get-or-create malaga/controllers:+user+ :name (get-username-from-path user-path) :file (namestring user-path))))
      ; Compare checksum to user model
      (when (update-user-p user)
          (format t ">>> Processing: ~A~%" (slot-value user 'mito.dao.mixin::id))))))

(defun find-card-lists (dropbox-location)
  (loop :for dir :in (directory dropbox-location)
        :for path = (probe-file (pathname (format nil "~A/cards.csv" dir)))
        :if path :collect path))

(defun clean-up-old-data ()
  (format t "Cleaning up old data stub~%"))

(defun get-username-from-path (file)
  (car (last (pathname-directory file))))

(defun update-user-p (user)
  (string/= (slot-value user 'malaga/models:checksum) (malaga/utils:get-checksum (slot-value user 'malaga/models:file))))
