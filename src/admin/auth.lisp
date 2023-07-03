(defpackage malaga/admin/auth
  (:use :cl)
  (:export #:user-p
           #:user-pass
           #:user-roles))

(in-package malaga/admin/auth)

(defun user-p (user)
    (barghest/controllers:get malaga/admin/controllers:+user+ :name user))

(defun user-pass (user)
    (slot-value (barghest/controllers:get malaga/admin/controllers:+user+ :name user) 'malaga/admin/models:password))

(defun user-roles (user)
    (loop :for role
          :in (malaga/admin/controllers:search malaga/admin/controllers:+permissions+ :player (barghest/controllers:get malaga/admin/controllers:+user+ :name user))
          :collect (slot-value (slot-value role 'malaga/admin/models:role) 'malaga/admin/models:name)))
