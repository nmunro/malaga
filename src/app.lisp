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

(malaga/web/routes:defroute +app+ "/" #'malaga/views:index)
(malaga/web/routes:defroute +app+ "/cards" #'malaga/views:cards)
(malaga/web/routes:defroute +app+ "/cards/:card" #'malaga/views:card)
(malaga/web/routes:defroute +app+ "/players" #'malaga/views:players)
(malaga/web/routes:defroute +app+ "/players/:player" #'malaga/views:player)
(malaga/web/routes:defroute +app+ "/players/:player/cards" #'malaga/views:player-cards)
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
  (clack:clackup +app+ :server server :address address :port port))

(defun stop-app (instance)
  (clack:stop instance))
