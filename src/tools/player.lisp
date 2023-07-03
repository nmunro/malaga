(defpackage malaga/tools/player
  (:use :cl)
  (:export #:process-players))

(in-package malaga/tools/player)

(defun get-start-time ()
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (declare (ignore day))
    (format nil "~A-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))

(defun process-players (dropbox-location)
  (let ((now (get-start-time)))
    (format t "Processing Players~%")

    ; Add cards
    (dolist (user-path (find-card-lists dropbox-location))
      (multiple-value-bind (user created)
          (barghest/controllers:get-or-create malaga/admin/controllers:+user+ :name (get-username-from-path user-path))
        (when created
          (barghest/controllers:get-or-create malaga/trader/controllers:+profile+ :user user :file (namestring user-path)))
        (when (update-user-p user)
            (update-user user)
            (clean-up-old-data user dropbox-location now))))))

(defun get-list-of-players-from-files (dropbox-location)
  (mapcar #'(lambda (d) (car (last (pathname-directory d)))) (find-card-lists dropbox-location)))

(defun update-user (user)
  (format t ">>> Processing: ~A~%" (slot-value user 'malaga/admin/models:name))
  (barghest/controllers:delete malaga/trader/controllers:+collection+ :user user)

  (let* ((path (pathname (slot-value (barghest/controllers:get malaga/trader/controllers:+profile+ :user user) 'malaga/trader/models:file)))
         (headers (cl-csv:read-csv-row path))
         (csv (make-instance 'data-table:data-table :column-names headers :rows (cdr (cl-csv:read-csv path)))))
    (dotimes (row (length (data-table:rows csv)))
      (let ((card (barghest/controllers:get malaga/trader/controllers:+card+ :scryfall-id (data-table:data-table-value csv :row-idx row :col-name "scryfall_id"))))
        (create-record
         user
         card
         (parse-integer (data-table:data-table-value csv :row-idx row :col-name "quantity"))
         (data-table:data-table-value csv :row-idx row :col-name "extras")))))

  (let ((profile (barghest/controllers:get malaga/trader/controllers:+profile+ :user user)))
    (setf
      (slot-value profile 'malaga/trader/models:checksum)
      (malaga/utils:get-checksum (pathname (slot-value profile 'malaga/trader/models:file)))))
  (mito:save-dao user))

(defun create-record (user card quantity extras)
  (multiple-value-bind (obj created)
      (barghest/controllers:get-or-create malaga/trader/controllers:+collection+ :user user :card card :extras extras)
    (setf (slot-value obj 'malaga/trader/models:quantity) quantity)
    (mito:save-dao obj)))

(defun find-card-lists (dropbox-location)
  (loop :for dir :in (directory dropbox-location)
        :for path = (probe-file (pathname (format nil "~A/cards.csv" dir)))
        :if path :collect path))

(defun clean-up-old-data (user dropbox-location now)
  (malaga/trader/controllers:delete-before malaga/trader/controllers:+collection+ user now)

  (dolist (user (malaga/admin/controllers:stale-users malaga/admin/controllers:+user+ (get-list-of-players-from-files dropbox-location)))
    (barghest/controllers:delete malaga/trader/controllers:+collection+ :user user)
    (barghest/controllers:delete malaga/admin/controllers:+user+ :name (slot-value user 'malaga/admin/models:name))))

(defun get-username-from-path (file)
  (car (last (pathname-directory file))))

(defun update-user-p (user)
  (let ((profile (barghest/controllers:get malaga/trader/controllers:+profile+ :user user)))
    (if (slot-boundp profile 'malaga/trader/models:checksum)
      (string/= (slot-value profile 'malaga/trader/models:checksum) (malaga/utils:get-checksum (slot-value profile 'malaga/trader/models:file)))
      t)))
