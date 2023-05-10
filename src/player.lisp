(defpackage malaga/player
  (:use :cl)
  (:export #:process-players))

(in-package malaga/player)

(defun process-players (dropbox-location)
  (format t "Processing Players~%")
  (clean-up-old-data dropbox-location)

  ; Add profiles
  (dolist (profile (find-profiles dropbox-location))
    (let ((user (malaga/controllers:get-or-create malaga/controllers:+user+ :name (get-username-from-path profile))))
      (setf (slot-value user 'malaga/models:profile) (uiop:read-file-string profile))
      (mito:save-dao user)))

  ; Add cards
  (dolist (user-path (find-card-lists dropbox-location))
    (let ((user (malaga/controllers:get-or-create malaga/controllers:+user+ :name (get-username-from-path user-path) :file (namestring user-path))))
      (when (update-user-p user)
          (update-user user)))))

(defun get-list-of-players-from-files (dropbox-location)
  (mapcar #'(lambda (d) (car (last (pathname-directory d)))) (find-card-lists dropbox-location)))

(defun update-user (user)
  (format t ">>> Processing: ~A~%" (slot-value user 'malaga/models:name))
  (malaga/controllers:delete malaga/controllers:+collection+ :user user)

  (let* ((path (pathname (slot-value user 'malaga/models:file)))
         (headers (cl-csv:read-csv-row path))
         (csv (make-instance 'data-table:data-table :column-names headers :rows (cdr (cl-csv:read-csv path)))))
    (dotimes (index (1- (length (data-table:rows csv))))
      (let ((card (malaga/controllers:get malaga/controllers:+card+ :id (data-table:data-table-value csv :row-idx index :col-name "scryfall_id"))))
        (create-record user card (parse-integer (data-table:data-table-value csv :row-idx index :col-name "quantity"))))))

  (setf (slot-value user 'malaga/models:checksum) (malaga/utils:get-checksum (pathname (slot-value user 'malaga/models:file))))
  (mito:save-dao user))

(defun create-record (user card quantity)
  (multiple-value-bind (obj created)
      (malaga/controllers:get-or-create malaga/controllers:+collection+ :user user :card card)
    (if created ; things like 'foils' count as separate cards, so just add them to normal cards
      (setf (slot-value obj 'malaga/models:quantity) quantity)
      (setf (slot-value obj 'malaga/models:quantity) (+ quantity (slot-value obj 'malaga/models:quantity))))
    (mito:save-dao obj)))

(defun find-card-lists (dropbox-location)
  (loop :for dir :in (directory dropbox-location)
        :for path = (probe-file (pathname (format nil "~A/cards.csv" dir)))
        :if path :collect path))

(defun find-profiles (dropbox-location)
  (loop :for dir :in (directory dropbox-location)
        :for path = (probe-file (pathname (format nil "~A/profile.md" dir)))
        :if path :collect path))

(defun clean-up-old-data (dropbox-location)
  (dolist (user (malaga/controllers:stale-users malaga/controllers:+user+ (get-list-of-players-from-files dropbox-location)))
    (malaga/controllers:delete malaga/controllers:+collection+ :user user)
    (malaga/controllers:delete malaga/controllers:+user+ :name (slot-value user 'malaga/models:name))))

(defun get-username-from-path (file)
  (car (last (pathname-directory file))))

(defun update-user-p (user)
  (if (slot-boundp user 'malaga/models:checksum)
      (string/= (slot-value user 'malaga/models:checksum) (malaga/utils:get-checksum (slot-value user 'malaga/models:file)))
      t))
