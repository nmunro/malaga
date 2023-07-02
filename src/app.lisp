(defpackage malaga/web
  (:use :cl)
  (:import-from :ningle
                :*session*)
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

(malaga/web/routes:defroute +app+ "/" #'malaga/views:index :METHOD :GET)
(malaga/web/routes:defroute +app+ "/cards" #'malaga/views:cards :METHOD :GET)
(malaga/web/routes:defroute +app+ "/cards/:card" #'malaga/views:card :METHOD :GET)
(malaga/web/routes:defroute +app+ "/players" #'malaga/views:players :METHOD :GET)
(malaga/web/routes:defroute +app+ "/players/:player" #'malaga/views:player :METHOD :GET)
(malaga/web/routes:defroute +app+ "/players/:player/cards" #'malaga/views:player-cards :METHOD :GET)
(malaga/web/routes:defroute +app+ "/profile" #'malaga/views:profile :METHOD :GET)
(malaga/web/routes:defroute +app+ "/login" #'malaga/views:login :METHOD :POST)
(malaga/web/routes:defroute +app+ "/logout" #'malaga/views:logout :METHOD :GET)
(malaga/web/routes:defroute +app+ "/admin" #'malaga/views:admin :METHOD :GET)

;; This is the way to handle missing routes
(defmethod ningle:not-found ((this ningle:<app>))
  (declare (ignore this))
  (setf (lack.response:response-status ningle:*response*) 404)
  (malaga/views:render "404.html"))

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
    :user-p #'(lambda (user)
                (malaga/controllers:get malaga/controllers:+user+ :name user))
    :user-pass #'(lambda (user)
                   (slot-value (malaga/controllers:get malaga/controllers:+user+ :name user) 'malaga/models:password))
    :user-roles #'(lambda (user)
                    (loop :for role
                          :in (malaga/controllers:search malaga/controllers:+permissions+ :player (malaga/controllers:get malaga/controllers:+user+ :name user))
                          :collect (slot-value (slot-value role 'malaga/models:role) 'malaga/models:name))))
  (clack:clackup (lack.builder:builder :session +app+) :server server :address address :port port))

(defun stop-app (instance)
  (clack:stop instance))
