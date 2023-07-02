(defpackage malaga/admin
  (:use :cl)
  (:export #:create-super-user
           #:sync-models))

(in-package malaga/admin)

(defun create-super-user (pass)
  (let* ((hash (cl-pass:hash pass :type :pbkdf2-sha256 :iterations 10000))
         (user (malaga/controllers:get-or-create malaga/controllers:+user+ :name "admin" :password hash)))
    (dolist (role (list "admin" "user"))
        (malaga/controllers:get-or-create
         malaga/controllers:+permissions+
         :user user
         :role (malaga/controllers:get-or-create malaga/controllers:+role+ :name name)))))

(defun sync-models ()
  (mito:ensure-table-exists 'malaga/models:card)
  (mito:ensure-table-exists 'malaga/models:user)
  (mito:ensure-table-exists 'malaga/models:role)
  (mito:ensure-table-exists 'malaga/models:permissions)
  (mito:ensure-table-exists 'malaga/models:collection))
