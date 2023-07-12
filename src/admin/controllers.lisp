(defpackage malaga/admin/controllers
  (:use :cl)
  (:shadow #:search)
  (:export #:model
           #:search
           #:stale-users
           #:user-permissions
           #:+user+
           #:+role+
           #:+permissions+))

(in-package malaga/admin/controllers)

(defclass user (barghest/controllers:controller)
  ((model :initarg :model :initform 'malaga/admin/models:user :reader model)))

(defclass role (barghest/controllers:controller)
  ((model :initarg :model :initform 'malaga/admin/models:role :reader model)))

(defclass permissions (barghest/controllers:controller)
  ((model :initarg :model :initform 'malaga/admin/models:permissions :reader model)))

(defmethod user-permissions ((controller permissions) user)
  (mito:select-dao 'malaga/admin/models:permissions
                   (mito:includes 'malaga/admin/models:user)
                   (mito:includes 'malaga/admin/models:role)
                   (sxql:where (:= :user user))))

(defmethod stale-users ((controller user) users)
  (mito:select-dao (model controller) (sxql:where (:not-in :name users))))

(defmethod search ((controller permissions) &key (search string) (player nil) (paginate nil) (offset 0) (limit 500))
  (declare (ignore search paginate offset limit))
    (mito:select-dao (model controller)
            (mito:includes 'malaga/admin/models:user)
            (mito:includes 'malaga/admin/models:role)
            (sxql:inner-join :user :on (:= :user.id :permissions.user_id))
            (sxql:inner-join :role :on (:= :role.id :permissions.role_id))
            (sxql:where (:= :user player))))

(defvar +user+ (make-instance 'user :model 'malaga/admin/models:user))
(defvar +role+ (make-instance 'role :model 'malaga/admin/models:role))
(defvar +permissions+ (make-instance 'permissions :model 'malaga/admin/models:permissions))
