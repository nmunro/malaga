(defpackage malaga/web
  (:use :cl)
  (:export #:start-app
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

(defun start-app (&key (server :hunchentoot) (address (machine-instance)) (port 5000))
  (djula:add-template-directory (asdf:system-relative-pathname "malaga" "src/templates/"))
  (clack:clackup +app+
                 :server server
                 :address address
                 :port port))

(defun stop-app (instance)
  (clack:stop instance))
