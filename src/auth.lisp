(defpackage malaga/auth
  (:use :cl)
  (:export #:user-p
           #:user-pass
           #:user-roles))

(in-package malaga/auth)

(defun user-p (user)
    (malaga/controllers:get malaga/controllers:+user+ :name user))

(defun user-pass (user)
    (slot-value (malaga/controllers:get malaga/controllers:+user+ :name user) 'malaga/models:password))

(defun user-roles (user)
    (loop :for role
          :in (malaga/controllers:search malaga/controllers:+permissions+ :player (malaga/controllers:get malaga/controllers:+user+ :name user))
          :collect (slot-value (slot-value role 'malaga/models:role) 'malaga/models:name)))
