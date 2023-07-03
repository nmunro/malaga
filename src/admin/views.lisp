(defpackage malaga/admin/views
  (:use :cl)
  (:export #:object
           #:login
           #:logout
           #:admin))

(in-package malaga/admin/views)

(defun login (params)
    (handler-case (cerberus:login :user (cdr (assoc "username" params :test #'equal)) :password (cdr (assoc "password" params :test #'equal)))
        (cerberus:invalid-user (err)
          (return-from login (barghest/http:render "login.html" :msg (cerberus:msg err))))

        (cerberus:invalid-password (err)
          (return-from login (barghest/http:render "login.html" :msg (cerberus:msg err)))))

    (barghest/http:redirect "/profile"))

(defun logout (params)
  (when (cerberus:user-name)
    (cerberus:logout)
    (return-from logout (barghest/http:redirect "/")))

  (barghest/http:redirect "/profile"))

(defun admin (params)
  (unless (cerberus:auth "admin")
    (setf (lack.response:response-status ningle:*response*) 403)
    (return-from admin (barghest/http:render "403.html")))

  (barghest/http:render "admin.html" :msg "Malaga Admin"))

(defun object (params)
  (format nil "~A: ~A" (or (cdr (assoc :action params :test #'equal)) "All") (cdr (assoc :object params :test #'equal))))
