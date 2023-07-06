(defpackage malaga/admin/views
  (:use :cl)
  (:shadow #:delete
           #:get)
  (:import-from :malaga/admin/controllers
                :+user+
                :+role+
                :+permissions+)
  (:export #:add
           #:admin
           #:get
           #:get
           #:save
           #:login
           #:logout))

(in-package malaga/admin/views)

(djula:add-template-directory (asdf:system-relative-pathname "malaga" "src/admin/templates/"))

(defun load-controller (name)
  (let ((model (find-class (read-from-string (format nil "malaga/admin/models:~A" name))))
        (controller (find-class (read-from-string (format nil "malaga/admin/controllers::~A" name)))))
    (funcall #'make-instance controller :model model)))

(defun login (params)
  (handler-case (cerberus:login :user (cdr (assoc "username" params :test #'equal)) :password (cdr (assoc "password" params :test #'equal)))
    (cerberus:invalid-user (err)
        (return-from login (barghest/http:render "admin/login.html" :msg (cerberus:msg err))))

    (cerberus:invalid-password (err)
        (return-from login (barghest/http:render "admin/login.html" :msg (cerberus:msg err)))))

  (barghest/http:redirect "/admin"))

(defun logout (params)
  (when (cerberus:user-name)
    (cerberus:logout)
    (return-from logout (barghest/http:redirect "/")))

  (barghest/http:redirect "/profile"))

(defun admin (params)
  (declare (ignore params))
  (if (cerberus:auth "admin")
    (barghest/http:render "admin/admin.html" :msg "Malaga Admin")
    (barghest/http:render "admin/login.html" :msg "Malaga Admin")))

(defun get (params)
  (let* ((object (cdr (assoc :object params :test #'equal)))
         (item (cdr (assoc :item params :test #'equal)))
         (controller (load-controller object)))
    (if item
        (barghest/http:render (format nil "admin/~A/item.html" object) :item (barghest/controllers:get controller :id item))
        (barghest/http:render (format nil "admin/~A/list.html" object) :objects (barghest/controllers:all controller)))))

(defun save (params)
    (let ((object (cdr (assoc :object params :test #'equal)))
          (item (cdr (assoc :item params :test #'equal))))
    (format t "Params: ~A~%" params)
    (if item
        (barghest/http:redirect (format nil "/admin/~A/~A" object item))
        (barghest/http:redirect (format nil "/admin/~A" object)))))

(defun add (params)
  (let ((object (cdr (assoc :object params :test #'equal))))
    (barghest/http:render (format nil "admin/~A/item.html" object))))

(defun delete (params)
  nil)
