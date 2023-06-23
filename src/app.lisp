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

(defparameter *users* (make-hash-table :test #'equal))

(defun make-user (username pass roles)
  (setf (gethash username *users*)
        (list :pass (cl-pass:hash pass :type :pbkdf2-sha256 :iterations 10000) :roles roles)))

(make-user "admin" "admin" (list :user :staff :admin))
(make-user "joe.avg" "pass" (list :user))

(defmacro get-user (username)
  `(gethash ,username *users*))

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
  (clack:clackup (lack.builder:builder :session +app+) :server server :address address :port port)
  (hermetic:setup
    :user-p #'(lambda (user) (get-user user))
    :user-pass #'(lambda (user) (getf (get-user user) :pass))
    :user-roles #'(lambda (user) (getf (get-user user) :roles))
    :session ningle:*session*
    :denied #'(lambda (&optional params) (malaga/views:render "403.html" :msg "Generic auth denied page"))))

(defun stop-app (instance)
  (clack:stop instance))
