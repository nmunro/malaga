(defpackage malaga/admin/views
  (:use :cl)
  (:shadow #:get)
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

(defun clean-form (params)
    (setf params (remove (assoc "action" params :test #'equalp) params :test #'equal))
    (setf params (remove (assoc "csrf-token" params :test #'equalp) params :test #'equal))
    (setf params (remove (assoc :object params :test #'equal) params :test #'equal)))

(defun login (params)
  (let ((username (cdr (assoc "username" params :test #'equal)))
        (password (cdr (assoc "password" params :test #'equal))))
    (handler-case (cerberus:login :user username :password password)
        (cerberus:invalid-user (err)
            (return-from login (barghest/http:render "admin/login.html" :msg (cerberus:msg err))))

        (cerberus:invalid-password (err)
            (return-from login (barghest/http:render "admin/login.html" :msg (cerberus:msg err))))))

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
  (unless (cerberus:auth "admin")
    (return-from get (barghest/http:redirect "/admin")))

  (let* ((object (cdr (assoc :object params :test #'equal)))
         (item (cdr (assoc :item params :test #'equal)))
         (controller (load-controller object)))
    (if item
        (barghest/http:render
         (format nil "admin/~A/item.html" object)
         :item (barghest/controllers:get controller :id item)
         :csrf-token (cerberus:csrf-token))
        (barghest/http:render
         (format nil "admin/~A/list.html" object)
         :objects (barghest/controllers:all controller)
         :csrf-token (cerberus:csrf-token)))))

(defun save-or-delete-object (action object fields)
  (let* ((controller (load-controller object))
         (id (cdr (assoc "id" fields :test #'equal)))
         (kws (barghest/utils:alist-to-plist (remove (assoc "id" fields :test #'equalp) fields :test #'equal))))
    (cond
        ((and (equalp action "save") (string/= id ""))
         (let ((obj (barghest/controllers:get controller :id id)))
           (dolist (kw (loop :for (k v) :on kws :by #'cddr :collect k))
             (setf (slot-value obj (read-from-string (format nil "malaga/admin/models:~A" kw))) (getf kws kw)))
           (mito:save-dao obj)))

        ((equalp action "save")
            (apply #'barghest/controllers:create (append `(,controller) kws)))

        ((and (equalp action "delete") (string/= id ""))
            (barghest/controllers:delete controller :id id)))))

(defun save (params)
    (let* ((object (cdr (assoc :object params :test #'equal)))
           (item (cdr (assoc "name" params :test #'equal)))
           (action (cdr (assoc "action" params :test #'equal)))
           (csrf-token (cdr (assoc "csrf-token" params :test #'equal))))
      (unless (string= csrf-token (cerberus:csrf-token))
        (return-from save (barghest/http:forbidden "403.html" :msg "CSRF Token missing")))

      (save-or-delete-object action object (clean-form params))

      (if item
        (barghest/http:redirect (format nil "/admin/~A/~A" object item))
        (barghest/http:redirect (format nil "/admin/~A" object)))))

(defun add (params)
  (let ((object (cdr (assoc :object params :test #'equal))))
    (barghest/http:render (format nil "admin/~A/item.html" object) :csrf-token (cerberus:csrf-token))))
