(defpackage malaga/web
  (:use :cl)
  (:export #:start-app
           #:stop-app))

(in-package malaga/web)

;; (mito:connect-toplevel :mysql :database-name "myapp" :username "fukamachi" :password "c0mon-1isp")
(mito:connect-toplevel :sqlite3 :database-name (uiop:getenv "MALAGA_DB"))

(defparameter +app+ (make-instance 'ningle:<app>))

(malaga/web/routes:defroute +app+ "/" #'malaga/views:index)
(malaga/web/routes:defroute +app+ "/cards" #'malaga/views:cards)
(malaga/web/routes:defroute +app+ "/cards/:card" #'malaga/views:card)
(malaga/web/routes:defroute +app+ "/players" #'malaga/views:players)
(malaga/web/routes:defroute +app+ "/players/:player" #'malaga/views:player)
(malaga/web/routes:defroute +app+ "/players/:player/cards" #'malaga/views:player-cards)

(defun start-app ()
  (djula:add-template-directory (asdf:system-relative-pathname "malaga" "src/templates/"))
  (clack:clackup +app+))

(defun stop-app (instance)
  (clack:stop instance))
