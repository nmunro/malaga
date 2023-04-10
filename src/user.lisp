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

(defun delete-stale-data ()
  nil)

(defun create-collection-record (csv user card index)
  (mito:create-dao
    'malaga/models:collection
    :user user
    :card card
    :quantity (data-table:data-table-value csv :row-idx index :col-name "quantity")))

(defun update-collection-record (csv collection index)
  (let ((quantity (parse-integer (data-table:data-table-value csv :row-idx index :col-name "quantity"))))
    (setf (slot-value collection 'malaga/models:quantity) quantity)
    (mito:save-dao collection)))

(defun update-user (user)
  (let* ((path (pathname (slot-value user 'malaga/models:file)))
         (headers (cl-csv:read-csv-row path))
         (csv (make-instance 'data-table:data-table :column-names headers :rows (cdr (cl-csv:read-csv path)))))
    (dotimes (index (1- (length (data-table:rows csv))))
      (let* ((card (mito:find-dao 'malaga/models:scryfall-card :id (data-table:data-table-value csv :row-idx index :col-name "scryfall_id")))
             (collection (mito:find-dao 'malaga/models:collection :user user :card card)))
        ; @NOTE
        ; mark the current date time
        ; if the collection exists, update the quantity and update_date to date time
        ; if it doesn't create it
        (if collection
          (update-collection-record csv collection index)
          (create-collection-record csv user card index))))))

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

    ; Update users
    (let ((user (mito:find-dao 'malaga/models:user :name (car (last (pathname-directory user-list))))))
      (if (update-user-p user)
        (update-user user)
        (format t "Not Updating: ~A~%" (slot-value user 'malaga/models:name)))

      ; Cleanup old user data
      ; @NOTE: Remember to delete orphaned cards as part of the above function call
      ; Get the current datetime and pass it into the update-user function above to update rows
      ; Delete anything earlier than the datetime
      (delete-stale-data))))
