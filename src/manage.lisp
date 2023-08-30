(defpackage malaga/manage
  (:use :cl)
  (:export #:sync-models
           #:main
           #:start-app
           #:stop-app
           #:+app+))

(in-package malaga/manage)

(defparameter app-name "malaga")
(defparameter +app+ (make-instance 'ningle:<app>))

(barghest/settings:load-settings app-name)
(apply #'mito:connect-toplevel (getf (envy:config :malaga/settings) :database))

(defun sync-models ()
  (mito:ensure-table-exists 'barghest/admin/models:user)
  (mito:ensure-table-exists 'barghest/admin/models:role)
  (mito:ensure-table-exists 'barghest/admin/models:permissions)
  (mito:ensure-table-exists 'malaga/trader/models:card)
  (mito:ensure-table-exists 'malaga/trader/models:profile)
  (mito:ensure-table-exists 'malaga/trader/models:collection))

;; warning: hardcoded "hunchentoot".
(defun find-hunchentoot-thread (th)
  (search "hunchentoot" (bt:thread-name th)))

(defun main (&key (server :hunchentoot) (address (getf (envy:config :malaga/settings) :http-address)) (port (getf (envy:config :malaga/settings) :http-address)))
  (let ((app (start-app :server server :address address :port port)))
    ;; let the webserver run.
    (handler-case (bt:join-thread (find-if #'find-hunchentoot-thread (bt:all-threads)))
        ;; Catch a user's C-c
        (#+sbcl sb-sys:interactive-interrupt
          ()
          (progn
            (format *error-output* "Aborting.~&")
            (clack:stop app)
            (uiop:quit)))
        (error (c) (format t "Woops, an unknown error occured:~&~a~&" c)))))

(defun start-app (&key (server :hunchentoot) (address) (port))
  (cerberus:setup
    :user-p #'barghest/admin/auth:user-p
    :user-pass #'barghest/admin/auth:user-pass
    :user-roles #'barghest/admin/auth:user-roles
    :user-csrf-token #'barghest/admin/auth:user-csrf-token)
  (barghest/settings:load-settings app-name)

  (let ((template (format nil "templates~A" ppath.details.constants:+sep-string+)))
    ;; Process each app
    (dolist (installed-app (getf (envy:config :malaga/settings) :installed-apps))
      ;; Load static files
      (barghest/static:prepare-static-routes app-name installed-app (getf (envy:config :malaga/settings) :static-url))

      ;; Load template files
      (let ((name (format nil "~A~A" installed-app ppath.details.constants:+sep-string+)))
        (djula:add-template-directory (asdf:system-relative-pathname app-name (ppath:join "src" name)))
        (djula:add-template-directory (asdf:system-relative-pathname app-name (ppath:join "src" installed-app template)))))

      ;; Load global templates
      (djula:add-template-directory (asdf:system-relative-pathname "barghest" (ppath:join "src" template))))

  (barghest/routes:mount +app+ malaga/malaga/urls:patterns)
  (barghest/routes:mount +app+ barghest/static:patterns :prefix (getf (envy:config :malaga/settings) :static-url))

  (clack:clackup (lack.builder:builder :session +app+) :server server :address address :port port))

(defun stop-app (instance)
  (clack:stop instance))

(defmethod ningle:not-found ((this ningle:<app>))
  (declare (ignore this))
  (setf (lack.response:response-status ningle:*response*) 404)
  (barghest/http:render "404.html"))
