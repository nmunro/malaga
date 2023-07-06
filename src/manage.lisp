(defpackage malaga/manage
  (:use :cl)
  (:export #:create-user
           #:set-password
           #:sync-models))

(in-package malaga/manage)

(defun create-user (username)
  (let* ((pass (barghest/crypt:make-user-password 16))
         (hash (cl-pass:hash pass :type :pbkdf2-sha256 :iterations 10000))
         (user (barghest/controllers:get-or-create malaga/admin/controllers:+user+ :name username :password hash)))
    (barghest/controllers:get-or-create malaga/trader/controllers:+profile+ :user user)
    (dolist (role (list "admin" "user"))
        (barghest/controllers:get-or-create
         malaga/admin/controllers:+permissions+
         :user user
         :role (barghest/controllers:get-or-create malaga/admin/controllers:+role+ :name role)))
    (format t "Initial password for ~A is: '~A', this message will not be displayed again.~%" username pass)))

(defun set-password (username)
  (format t "Please enter the new password for '~A': " username)
  (force-output)

  (let ((user (barghest/controllers:get malaga/admin/controllers:+user+ :name username)))
    (setf (slot-value user 'malaga/admin/models:password) (cl-pass:hash (read-line) :type :pbkdf2-sha256 :iterations 10000))
    (mito:save-dao user)))

(defun sync-models ()
  (mito:ensure-table-exists 'malaga/admin/models:user)
  (mito:ensure-table-exists 'malaga/admin/models:role)
  (mito:ensure-table-exists 'malaga/admin/models:permissions)
  (mito:ensure-table-exists 'malaga/trader/models:card)
  (mito:ensure-table-exists 'malaga/trader/models:profile)
  (mito:ensure-table-exists 'malaga/trader/models:collection))
