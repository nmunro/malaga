(defpackage malaga/user
  (:use :cl)
  (:export #:get-checksum
           #:process-users
           #:delete-old-users
           #:get-list-of-users-from-files
           #:get-list-of-users-from-db
           #:update-user-p))

(in-package malaga/user)

(defun get-checksum (path)
  (let ((digest (ironclad:make-digest :md5)))
    (format nil "~{~A~}" (coerce (ironclad:digest-file digest path) 'list))))

(defun create-new-user (file)
  (let ((name (car (last (pathname-directory file)))))
    (unless (mito:find-dao 'malaga/models:user :name name)
        (mito:create-dao 'malaga/models:user :name name :file (namestring file) :checksum ""))))

(defun get-list-of-users-from-files (config)
  (mapcar #'(lambda (d) (car (last (pathname-directory d)))) (find-card-lists (malaga/config:dropbox-location config))))

(defun get-list-of-users-from-db ()
  (mito:select-dao 'malaga/models:user))

(defun delete-old-users (config)
  (let ((db-users   (mapcar #'(lambda (user) (slot-value user 'malaga/models:name)) (get-list-of-users-from-db)))
        (file-users (get-list-of-users-from-files config)))
    (dolist (user-name (set-difference db-users file-users :test #'equal))
      (format t "Deleting: ~A~%" user-name)
      (mito:delete-by-values 'malaga/models:user :name user-name))))

(defun update-user-p (user)
  (string/= (slot-value user 'malaga/models:checksum) (get-checksum (slot-value user 'malaga/models:file))))

(defun delete-stale-data (user)
  (let ((records (mito:select-dao 'malaga/models:collection (sxql:where (:and (:= :delete "Y") (:= :user user))))))
    (when records
      (format t "Deleting stale user data: ~A~%" (slot-value user 'malaga/models:name))
      (mito:delete-by-values 'malaga/models:collection :user user :delete "Y"))))

(defun create-collection-record (user csv card index)
  (mito:create-dao
    'malaga/models:collection
    :user user
    :card card
    :delete "N"
    :quantity (data-table:data-table-value csv :row-idx index :col-name "quantity")))

(defun update-collection-record (user csv collection index)
  (let ((quantity (parse-integer (data-table:data-table-value csv :row-idx index :col-name "quantity"))))
    (setf (slot-value collection 'malaga/models:quantity) quantity)
    (setf (slot-value collection 'malaga/models:delete) "N")
    (mito:save-dao collection)))

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

(defun process-users (config)
  (dolist (user-list (find-card-lists (malaga/config:dropbox-location config)))
    ; Delete any users in the db who don't have a file anymore
    (delete-old-users config)

    ; Create any newly found users
    (create-new-user user-list)

    ; Mark all data to be deleted
    (dolist (collection (mito:select-dao 'malaga/models:collection))
      (setf (slot-value collection 'malaga/models:delete) "Y")
      (mito:save-dao collection))

    ; Update users
    (let ((user (mito:find-dao 'malaga/models:user :name (car (last (pathname-directory user-list))))))
      (if (update-user-p user)
        (update-user user)
        (format t "Not Updating: ~A~%" (slot-value user 'malaga/models:name)))

      (delete-stale-data user))))
