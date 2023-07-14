(defpackage malaga/admin/urls
  (:use :cl)
  (:export #:patterns))

(in-package malaga/admin/urls)

(defparameter patterns (list
  (barghest/routes:path "/" #'malaga/admin/views:admin :method :GET :name :admin-index)
  (barghest/routes:path "/login" #'malaga/admin/views:login :method :POST :name :login)
  (barghest/routes:path "/logout" #'malaga/admin/views:logout :method :GET :name :logout)
  (barghest/routes:path "/:object" #'malaga/admin/views:get :method :GET :name :list-objects)
  (barghest/routes:path "/:object" #'malaga/admin/views:save :method :POST :name :save-object)
  (barghest/routes:path "/:object/add" #'malaga/admin/views:add :method :GET :name :add-object)
  (barghest/routes:path "/:object/:id" #'malaga/admin/views:get :method :GET :name :get-object)
  (barghest/routes:path "/:object/:id" #'malaga/admin/views:save :method :POST :name :update-object)))
