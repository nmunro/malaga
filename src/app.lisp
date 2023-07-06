(defpackage malaga/web
  (:use :cl)
  (:export #:start-app
           #:main
           #:stop-app))

(in-package malaga/web)

(mito:connect-toplevel :mysql
                       :database-name (uiop:getenv "MALAGA_DB")
                       :username (uiop:getenv "MALAGA_MYSQL_USERNAME")
                       :password (uiop:getenv "MALAGA_MYSQL_PASSWORD")
                       :port 3306)

(defparameter +app+ (make-instance 'ningle:<app>))

(barghest/routes:defroute +app+ "/" #'malaga/trader/views:index :METHOD :GET)
(barghest/routes:defroute +app+ "/cards" #'malaga/trader/views:cards :METHOD :GET)
(barghest/routes:defroute +app+ "/cards/:card" #'malaga/trader/views:card :METHOD :GET)
(barghest/routes:defroute +app+ "/players" #'malaga/trader/views:players :METHOD :GET)
(barghest/routes:defroute +app+ "/players/:player/profile" #'malaga/trader/views:player :METHOD :GET)
(barghest/routes:defroute +app+ "/players/:player/cards" #'malaga/trader/views:player-cards :METHOD :GET)
(barghest/routes:defroute +app+ "/admin" #'malaga/admin/views:admin :METHOD :GET)
(barghest/routes:defroute +app+ "/admin/login" #'malaga/admin/views:login :METHOD :POST)
(barghest/routes:defroute +app+ "/admin/logout" #'malaga/admin/views:logout :METHOD :GET)
(barghest/routes:defroute +app+ "/admin/:object" #'malaga/admin/views:get :METHOD :GET)
(barghest/routes:defroute +app+ "/admin/:object" #'malaga/admin/views:save :METHOD :POST)
(barghest/routes:defroute +app+ "/admin/:object/add" #'malaga/admin/views:add :METHOD :GET)
(barghest/routes:defroute +app+ "/admin/:object/:item" #'malaga/admin/views:get :METHOD :GET)
(barghest/routes:defroute +app+ "/admin/:object/:item" #'malaga/admin/views:save :METHOD :POST)

;; This is the way to handle missing routes
(defmethod ningle:not-found ((this ningle:<app>))
  (declare (ignore this))
  (setf (lack.response:response-status ningle:*response*) 404)
  (barghest/http:render "404.html"))

;; warning: hardcoded "hunchentoot".
(defun find-hunchentoot-thread (th) (search "hunchentoot" (bt:thread-name th)))

(defun main (&key (server :hunchentoot) (address (or (uiop:getenv "MALAGA_ADDRESS") (machine-instance))) (port (parse-integer (uiop:getenv "MALAGA_PORT"))))
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

(defun start-app (&key (server :hunchentoot) (address (or (uiop:getenv "MALAGA_ADDRESS") (machine-instance))) (port (parse-integer (uiop:getenv "MALAGA_PORT"))))
  (djula:add-template-directory (asdf:system-relative-pathname "malaga" "src/templates/"))
  (cerberus:setup
    :user-p #'malaga/admin/auth:user-p
    :user-pass #'malaga/admin/auth:user-pass
    :user-roles #'malaga/admin/auth:user-roles)
  (clack:clackup (lack.builder:builder :session +app+) :server server :address address :port port))

(defun stop-app (instance)
  (clack:stop instance))
