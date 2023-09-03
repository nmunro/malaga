(defpackage malaga/manage
  (:use :cl)
  (:export #:sync-models
           #:main
           #:start-app
           #:stop-app
           #:+app+))

(in-package malaga/manage)

(defparameter +project-name+ "malaga")
(defparameter +app+ (make-instance 'ningle:<app>))

(barghest/settings:load-settings +project-name+)
(apply #'mito:connect-toplevel (getf (envy:config :malaga/settings) :database))

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
    :user-p #'barghest/auth:user-p
    :user-pass #'barghest/auth:user-pass
    :user-roles #'barghest/auth:user-roles
    :user-csrf-token #'barghest/auth:user-csrf-token)
  (barghest/settings:load-settings +project-name+)

  (dolist (app (getf (envy:config :malaga/settings) :installed-apps))
    ;; find models
    (if (str:starts-with? "barghest" app)
        (process-barghest-app app)
        (process-project-app app)))

  (let ((template (format nil "templates~A" ppath.details.constants:+sep-string+)))
    (djula:add-template-directory (barghest/utils/tron:get-project-path "barghest")))

  (barghest/routes:mount +app+ malaga/malaga/urls:patterns)
  (barghest/routes:mount +app+ barghest/static:patterns :prefix (getf (envy:config :malaga/settings) :static-url))
  (barghest/admin/admin:main)

  (clack:clackup (lack.builder:builder :session +app+) :server server :address address :port port))

(defun process-barghest-app (app)
  (find-and-create-models app)
  (barghest/static:prepare-static-routes +project-name+ app (getf (envy:config :malaga/settings) :static-url))
  (find-and-load-templates app "barghest"))

(defun process-project-app (app)
  (find-and-create-models (format nil "~A/~A" +project-name+ app))
  (barghest/static:prepare-static-routes +project-name+ app (getf (envy:config :malaga/settings) :static-url))
  (find-and-load-templates app +project-name+))

(defun find-and-create-models (app)
  (do-external-symbols (s (find-package (string-upcase (format nil "~A/models" app))))
    (mito:ensure-table-exists s)))

(defun find-and-load-templates (app project)
  ; if app begins with "barghest/" strip the prefix
  (let ((template (format nil "templates~A" ppath.details.constants:+sep-string+)))
    (alexandria:if-let (barghest-project (str:starts-with-p "barghest/" app))
      (let* ((name (cadr (str:split "barghest/" app)))
             (root (namestring (barghest/utils/tron:get-project-path project :path (ppath:join "src" name)))))
        (djula:add-template-directory root)
        (djula:add-template-directory (ppath:join root "templates" name)))

      (let ((root (namestring (barghest/utils/tron:get-project-path project :path (ppath:join "src" app)))))
        (djula:add-template-directory root)
        (djula:add-template-directory (ppath:join root "templates" app))))))

(defun stop-app (instance)
  (clack:stop instance))

(defmethod ningle:not-found ((this ningle:<app>))
  (declare (ignore this))
  (setf (lack.response:response-status ningle:*response*) 404)
  (barghest/http:render "404.html"))
