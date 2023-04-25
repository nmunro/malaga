(defpackage malaga/web
  (:use :cl))

(in-package malaga/web)

(defvar *app* (make-instance 'ningle:<app>))

;; Tell Djula to load the templates from the templates directory
(djula:add-template-directory (asdf:system-relative-pathname "malaga" "templates/"))

(setf (ningle:route *app* "/") #'malaga/views:index)
(setf (ningle:route *app* "/cards") #'malaga/views:cards)
(setf (ningle:route *app* "/cards/:card") #'malaga/views:card)
(setf (ningle:route *app* "/players") #'malaga/views:players)
(setf (ningle:route *app* "/players/:player") #'malaga/views:player)
(setf (ningle:route *app* "/players/:player/cards") #'malaga/views:player-cards)
(setf (ningle:route *app* "/:missing") #'malaga/views:http404)

(clack:clackup *app*)

;; (ql:quickload :malaga)
