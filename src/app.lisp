(defpackage malaga/web
  (:use :cl)
  (:export #:start-app
           #:stop-app))

(in-package malaga/web)

(defvar +app+ (make-instance 'ningle:<app>))
(defvar +instance+ nil)

;; Tell Djula to load the templates from the templates directory
(djula:add-template-directory (asdf:system-relative-pathname "malaga" "templates/"))

(malaga/web/routes:defroute +app+ "/" #'malaga/views:index)
(malaga/web/routes:defroute +app+ "/cards" #'malaga/views:cards)
(malaga/web/routes:defroute +app+ "/cards/:card" #'malaga/views:card)
(malaga/web/routes:defroute +app+ "/players" #'malaga/views:players)
(malaga/web/routes:defroute +app+ "/players/:player" #'malaga/views:player)
(malaga/web/routes:defroute +app+ "/players/:player/cards" #'malaga/views:player-cards)
(malaga/web/routes:defroute +app+ "/:missing" #'malaga/views:http404)

(defun start-app ()
  (setf +instance+ (clack:clackup +app+)))

(defun stop-app ()
  (clack:stop +instance+))
