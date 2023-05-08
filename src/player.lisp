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
;;
;; (defun get-user-profile-md ()
;;   nil)

(defun process-players (dropbox-location)
  (format t "Processing Players~%")
  (clean-up-old-data)

  (dolist (user-path (find-card-lists dropbox-location))
    (let ((user (malaga/controllers:get-or-create malaga/controllers:+user+ :name (get-username-from-path user-path) :file (namestring user-path))))
      (when (update-user-p user)
          (update-user user)))))

(defun update-user (user)
  (format t ">>> Processing: ~A~%" (slot-value user 'malaga/models:name))
  (let* ((path (pathname (slot-value user 'malaga/models:file)))
         (headers (cl-csv:read-csv-row path))
         (csv (make-instance 'data-table:data-table :column-names headers :rows (cdr (cl-csv:read-csv path)))))
    (dotimes (index (1- (length (data-table:rows csv))))
      (let ((card (malaga/controllers:get malaga/controllers:+card+ :id (data-table:data-table-value csv :row-idx index :col-name "scryfall_id"))))
        (create-record user card (parse-integer (data-table:data-table-value csv :row-idx index :col-name "quantity")))))))

(defun create-record (user card quantity)
  (multiple-value-bind (obj created)
      (malaga/controllers:get-or-create malaga/controllers:+collection+ :user user :card card)
    (if created
      (setf (slot-value obj 'malaga/models:quantity) quantity)
      (setf (slot-value obj 'malaga/models:quantity) (+ quantity (slot-value obj 'malaga/models:quantity))))
    (mito:save-dao obj)))

(defun find-card-lists (dropbox-location)
  (loop :for dir :in (directory dropbox-location)
        :for path = (probe-file (pathname (format nil "~A/cards.csv" dir)))
        :if path :collect path))

(defun clean-up-old-data ()
  (format t "Cleaning up old data stub~%"))

(defun get-username-from-path (file)
  (car (last (pathname-directory file))))

(defun update-user-p (user)
  (if (slot-boundp user 'malaga/models:checksum)
      (string/= (slot-value user 'malaga/models:checksum) (malaga/utils:get-checksum (slot-value user 'malaga/models:file)))
      t))
