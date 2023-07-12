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

(defun load-slot (name)
  (read-from-string (format nil "malaga/admin/models:~A" name)))

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

(defgeneric create-object (object kws)
  (:documentation "Create an object"))

(defgeneric get-object (object kws)
  (:documentation "Gets an object"))

(defgeneric save-object (object obj kws)
  (:documentation "Save an object"))

(defgeneric delete-object (object obj)
  (:documentation "Delete an object"))

(defmethod create-object ((object (eql :user)) kws)
  (apply #'barghest/controllers:create (append `(,(load-controller "user")) kws)))

(defmethod create-object ((object (eql :role)) kws)
  (apply #'barghest/controllers:create (append `(,(load-controller "role")) kws)))

(defmethod get-object ((object (eql :user)) id)
  (flet ((get-role (role) `(:role ,role :selected ,(cerberus:auth (slot-value role 'malaga/admin/models:name)))))
    (let* ((user (barghest/controllers:get (load-controller "user") :id id))
           (permissions (malaga/admin/controllers:user-permissions (load-controller "permissions") user)))
        (barghest/http:render
            "admin/user/item.html"
            :item user
            :csrf-token (cerberus:csrf-token)
            :roles (loop :for role :in (barghest/controllers:all (load-controller "role")) :collect (get-role role))))))

(defmethod get-object ((object (eql :role)) id)
  (barghest/http:render
    "admin/role/item.html"
    :item (barghest/controllers:get (load-controller "role") :id id)
    :csrf-token (cerberus:csrf-token)))

(defmethod get-object ((object (eql :users)) id)
  (declare (ignore id))
  (barghest/http:render "admin/user/list.html" :items (barghest/controllers:all (load-controller "user"))))

(defmethod get-object ((object (eql :roles)) id)
  (declare (ignore id))
  (barghest/http:render "admin/role/list.html" :items (barghest/controllers:all (load-controller "role"))))

(defmethod save-object ((object (eql :user)) obj kws)
  (dolist (kw (loop :for (k v) :on kws :by #'cddr :collect k))
    (unless (eq kw :permission)
      (setf (slot-value obj (load-slot kw)) (getf kws kw))
      (setf kws (cddr kws))))
  (mito:save-dao obj)
  (setf kws (remove :permission kws))

  (dolist (perm (malaga/admin/controllers:user-permissions (load-controller "permissions") obj))
    (barghest/controllers:delete (load-controller "permissions") :id (mito:object-id perm)))

  (dolist (kw kws)
    (barghest/controllers:get-or-create
     (load-controller "permissions")
     :user obj
     :role (barghest/controllers:get (load-controller "role") :name kw))))

(defmethod save-object ((object (eql :role)) obj kws)
  (dolist (kw (loop :for (k v) :on kws :by #'cddr :collect k))
    (setf (slot-value obj (load-slot kw)) (getf kws kw)))
  (mito:save-dao obj))

(defmethod delete-object ((object (eql :user)) obj)
  (apply #'barghest/controllers:delete `(,(load-controller "user") :id ,(mito:object-id obj))))

(defmethod delete-object ((object (eql :role)) obj)
  (apply #'barghest/controllers:delete `(,(load-controller "role") :id ,(mito:object-id obj))))

(defun process-object (action object fields)
  (let ((controller (load-controller object))
        (id (cdr (assoc :id fields :test #'equal)))
        (kws (barghest/utils:alist-to-plist (remove (assoc :id fields :test #'equalp) fields :test #'equal))))
    (cond
      ((equalp action "create")
         (return-from process-object (create-object (intern (string-upcase object) "KEYWORD") kws)))

      ((equalp action "save")
         (alexandria:when-let (obj (barghest/controllers:get controller :id id))
            (return-from process-object (save-object (intern (string-upcase object) "KEYWORD") obj kws))))

      ((equalp action "delete")
         (alexandria:when-let (obj (barghest/controllers:get controller :id id))
            (return-from process-object (delete-object (intern (string-upcase object) "KEYWORD") obj)))))))

(defun get (params)
  (unless (cerberus:auth "admin")
    (return-from get (barghest/http:redirect "/admin")))

  (cond
    ((and (equalp (cdr (assoc :object params :test #'equal)) "user") (cdr (assoc :id params :test #'equal)))
      (get-object :user (cdr (assoc :id params :test #'equal))))

    ((equalp (cdr (assoc :object params :test #'equal)) "user")
      (get-object :users :none))

    ((and (equalp (cdr (assoc :object params :test #'equal)) "role") (cdr (assoc :id params :test #'equal)))
      (get-object :role (cdr (assoc :id params :test #'equal))))

    ((equalp (cdr (assoc :object params :test #'equal)) "role")
      (get-object :roles :none))))

(defun save (params)
  ; @TODO: A generic save that recovers from duplicate errors needs to be written
  (let* ((object (cdr (assoc :object params :test #'equal)))
         (item (cdr (assoc "name" params :test #'equal)))
         (action (cdr (assoc "action" params :test #'equal)))
         (csrf-token (cdr (assoc "csrf-token" params :test #'equal))))
    (unless (string= csrf-token (cerberus:csrf-token))
      (return-from save (barghest/http:forbidden "403.html" :msg "CSRF Token missing")))

    (alexandria:if-let (data (process-object action object (clean-form params)))
        (return-from save (barghest/http:redirect (format nil "/admin/~A/~A" object (mito:object-id data))))
        (return-from save (barghest/http:redirect (format nil "/admin/~A" object))))))

(defun add (params)
  (let ((object (cdr (assoc :object params :test #'equal))))
    (barghest/http:render (format nil "admin/~A/item.html" object) :csrf-token (cerberus:csrf-token))))

