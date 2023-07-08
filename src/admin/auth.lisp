(defpackage malaga/admin/auth
  (:use :cl)
  (:import-from :malaga/admin/controllers
                :+user+
                :+permissions+)
  (:export #:user-p
           #:user-pass
           #:user-roles
           #:user-csrf-token))

(in-package malaga/admin/auth)

(defun user-p (user)
    (barghest/controllers:get +user+ :name user))

(defun user-pass (user)
    (slot-value (barghest/controllers:get +user+ :name user) 'malaga/admin/models:password))

(defun user-roles (user)
    (loop :for role
          :in (malaga/admin/controllers:search +permissions+ :player (barghest/controllers:get +user+ :name user))
          :collect (slot-value (slot-value role 'malaga/admin/models:role) 'malaga/admin/models:name)))

(defun user-csrf-token (user)
  (let ((s (format nil "~A~A" user (get-universal-time))))
    (car (last (uiop:split-string (cl-pass:hash s :type :pbkdf2-sha256 :iterations 10000) :separator '(#\$))))))
